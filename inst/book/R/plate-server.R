# Section "server" (its number and title are in contents.R): open, close and change a tip from R, and hear back through the hint input.

server_first <- "New here? Start with **week 46**: the newest complete week."
server_new <- "Week 47 is in: **5,102 visits**, the busiest week since spring."

server_changes <- c("its words" = "words", "its words and the side it opens on" = "side")

server_ui_code <- function() {
  paste(
    c(
      code_sentence(
        "A hint on the",
        code_call(
          "tip_on",
          code_word("week picker"),
          code_str(server_first),
          marker = code_str("underline"),
          id = code_str("onboarding"),
          events = code_str("hint")
        ),
        "opens from the server on a reader's first visit."
      ),
      code_call("shiny::actionButton", code_str("srv_show"), code_str("Show")),
      code_call("shiny::actionButton", code_str("srv_hide"), code_str("Hide")),
      code_call("shiny::actionButton", code_str("srv_update"), code_str("Change the words"))
    ),
    collapse = "\n"
  )
}

# The server's code, one line per button, shown in the code note and run by the server as it is shown. The calls give
# `session = session` outright: inside a module it is the module's session, so the id is the module's own.
server_lines <- function(change = "words") {
  side <- identical(choice(change, server_changes, "words"), "side")
  c(
    "# A module's session goes in `session`; in the app's own server it is the default, so it can be left out",
    "shiny::observeEvent(input$srv_show, tip_show(\"onboarding\", session = session))",
    "shiny::observeEvent(input$srv_hide, tip_hide(\"onboarding\", session = session))",
    paste0(
      "shiny::observeEvent(input$srv_update, ",
      code_call(
        "tip_update",
        code_str("onboarding"),
        text = code_str(server_new),
        placement = if (side) code_str("bottom"),
        session = "session"
      ),
      ")"
    )
  )
}

server_server_code <- function(change = "words") paste(server_lines(change), collapse = "\n")


server_steps <- function() {
  list(
    step(
      "Press *Show*: `tip_show(\"onboarding\")` opens the hint, and `input$onboarding_hint` says `by = \"host\"`.",
      "nothing opens, or the input says reader.",
      "#srv_show",
      on_hint("onboarding", by = "host", via = "command")
    ),
    step(
      "The hint stays open until *Hide*, Escape or another tip closes it.",
      "it closes when the pointer moves.",
      tip_target("onboarding"),
      on_hint("onboarding", state = "hidden", reason = c("host", "escape", "replaced"), by = NULL)
    ),
    step(
      paste(
        "Show it, then press *Change the words*: the open hint takes the new words in place. Choose *its words and",
        "the side it opens on*, hide it and show it again: it opens below the tag."
      ),
      "it closes, keeps the old words, or opens on the old side.",
      "#srv_update"
    )
  )
}

plate_server_ui <- function() {
  plate(
    "server",
    lede = "Open, close and change a tip from R, and hear back through the hint input.",
    caption = "The three buttons call tip_show(), tip_hide() and tip_update() in the server.",
    steps = server_steps(),
    code = code_text(server_ui_code(), server_server_code()),
    log_empty = "Nothing yet. Press Show.",
    controls = htmltools::tagList(
      "Change the tip's",
      choice_set("server_change", "What changes", server_changes)
    )
  )
}

plate_server_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "server", server_steps())
  plate_code(
    input,
    output,
    session,
    "server",
    ui_code = shiny::reactive(server_ui_code()),
    server_code = shiny::reactive(server_server_code(input$server_change))
  )
  # Show and hide run from their shown lines. Update runs the handler of its shown line, so the choice reaches it.
  lines <- server_lines()
  run_server_code(paste(lines[2:3], collapse = "\n"), input, session)
  shiny::observeEvent(input$srv_update, {
    line <- server_lines(input$server_change)[[4]]
    env <- new.env(parent = book_exports())
    env$session <- session
    eval(parse(text = line, keep.source = FALSE)[[1]][[3]], env)
  })
  calls <- c(srv_show = "tip_show(\"onboarding\")", srv_hide = "tip_hide(\"onboarding\")", srv_update = "tip_update()")
  for (name in names(calls)) {
    local({
      name <- name
      shiny::observeEvent(input[[name]], {
        call <- calls[[name]]
        if (name == "srv_update") {
          words <- if (identical(choice(input$server_change, server_changes, "words"), "side")) ", placement" else ""
          call <- paste0("tip_update(\"onboarding\", text", words, ")")
        }
        log_line(session, "server", "server", call, "out")
      })
    })
  }
  log_inputs(input, session, "server", "onboarding_hint")
  ticks
}
