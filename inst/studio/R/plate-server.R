# Plate 9, From the server: open, close and change a tip from R, and hear back through the hint input.

server_first <- "New here? Start with **week 46**: the newest complete week."
server_new <- "Week 47 is in: **5,102 visits**, the busiest week since spring."

server_ui_code <- function() {
  paste(
    c(
      code_call(
        "tip_on",
        code_call(
          "htmltools::tags$span",
          code_str("Week picker"),
          tabindex = code_str("0"),
          class = code_str("target")
        ),
        code_str(server_first),
        id = code_str("onboarding"),
        events = code_str("hint")
      ),
      code_call("shiny::actionButton", code_str("srv_show"), code_str("Show")),
      code_call("shiny::actionButton", code_str("srv_hide"), code_str("Hide")),
      code_call("shiny::actionButton", code_str("srv_update"), code_str("Change the words"))
    ),
    collapse = "\n"
  )
}

# The server's code, shown in the code note and run by the server as it is shown.
server_server_code <- function() {
  paste(
    c(
      "shiny::observeEvent(input$srv_show, tip_show(\"onboarding\"))",
      "shiny::observeEvent(input$srv_hide, tip_hide(\"onboarding\"))",
      paste0(
        "shiny::observeEvent(input$srv_update, ",
        code_call("tip_update", code_str("onboarding"), text = code_str(server_new)),
        ")"
      )
    ),
    collapse = "\n"
  )
}

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
      "Show it, then press *Change the words*: the open hint takes the new words in place.",
      "it closes, or keeps the old words.",
      "#srv_update"
    )
  )
}

plate_server_ui <- function() {
  plate(
    "server",
    9,
    "From the server",
    lede = "Open, close and change a tip from R, and hear back through the hint input.",
    caption = "The three buttons call tip_show(), tip_hide() and tip_update() in the server.",
    steps = server_steps(),
    code = code_text(server_ui_code(), server_server_code()),
    log_empty = "Nothing yet. Press Show."
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
    server_code = shiny::reactive(server_server_code())
  )
  run_server_code(server_server_code(), input, session)
  calls <- c(srv_show = "tip_show(\"onboarding\")", srv_hide = "tip_hide(\"onboarding\")", srv_update = "tip_update()")
  for (name in names(calls)) {
    local({
      name <- name
      shiny::observeEvent(input[[name]], log_line(session, "server", "server", calls[[name]], "out"))
    })
  }
  log_inputs(input, session, "server", "onboarding_hint")
  ticks
}
