# Section "server": open, close and change a tip from R (`tip_show()`, `tip_hide()`, `tip_update()`), draw a tag and
# show its tip in one observer, and hear back through the hint input.

server_first <- "New here? Start with **week 46**: the newest complete week."
server_new <- "Week 47 is in: **5,102 visits**, the busiest week since spring."

server_changes <- c(
  "its words" = "words",
  "its words and the side it opens on" = "side",
  "its template and a record" = "template",
  "its side, taken away" = "away"
)

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
      code_call(
        "htmltools::p",
        code_str("A second tag, drawn by the server when you press Draw:"),
        code_call("shiny::uiOutput", code_str("srv_new"), inline = "TRUE"),
        class = code_str("specimen-text")
      ),
      code_call("shiny::actionButton", code_str("srv_show"), code_str("Show")),
      code_call("shiny::actionButton", code_str("srv_hide"), code_str("Hide")),
      code_call("shiny::actionButton", code_str("srv_update"), code_str("Change the tip")),
      code_call("shiny::actionButton", code_str("srv_draw"), code_str("Draw"))
    ),
    collapse = "\n"
  )
}

# The tip_update() call of a choice, as code, and the arguments it gives, as the log names them. `NULL` is left out of
# a call; `NA` takes the side away. `session` is named: it comes after `...`, never by position.
server_update_call <- function(change = "words") {
  change <- choice(change, server_changes, "words")
  template <- code_call(
    "tip_template",
    head = code_call("tip_title", code_str("Week {{ week }}")),
    body = code_call("tip_fields", Visits = code_str("{{ visits | number:0 }}")),
    width = 0L
  )
  code_call(
    "tip_update",
    code_str("onboarding"),
    text = if (change %in% c("words", "side")) code_str(server_new),
    template = if (change == "template") template,
    record = if (change == "template") "list(week = 47, visits = 5102)",
    placement = switch(change, side = code_str("bottom"), away = "NA"),
    session = "session"
  )
}

server_update_args <- function(change = "words") {
  switch(
    choice(change, server_changes, "words"),
    words = "text",
    side = "text, placement",
    template = "template, record",
    away = "placement = NA"
  )
}

# The server's code, one observer for each button, shown in the code note and run by the server as it is shown. The
# calls give `session = session` outright: inside a module it is the module's session, so the id is the module's own.
server_pieces <- function(change = "words") {
  draw <- code_call(
    "tip_on",
    code_word("week 47"),
    code_str("Drawn by the server a moment ago."),
    marker = code_str("underline"),
    id = code_str("srv_new_tip"),
    events = code_str("hint")
  )
  list(
    show = paste(
      c(
        "# `session` comes after `...`, so give it by name; in the app's own server it is the default and can go",
        "shiny::observeEvent(input$srv_show, tip_show(\"onboarding\", session = session))"
      ),
      collapse = "\n"
    ),
    hide = "shiny::observeEvent(input$srv_hide, tip_hide(\"onboarding\", session = session))",
    update = paste(
      c(
        "# NULL leaves a value alone, NA takes it away and \"\" clears the words",
        "shiny::observeEvent(input$srv_update, {",
        paste0("  ", code_indent(server_update_call(change), 2L)),
        "})"
      ),
      collapse = "\n"
    ),
    draw = paste(
      c(
        "shiny::observeEvent(input$srv_draw, {",
        "  output$srv_new <- shiny::renderUI(",
        paste0("    ", code_indent(draw, 4L)),
        "  )",
        "  # The call goes out after the flush, so it reaches the tag this observer has just drawn",
        "  tip_show(\"srv_new_tip\", session = session)",
        "})"
      ),
      collapse = "\n"
    )
  )
}

server_server_code <- function(change = "words") paste(server_pieces(change), collapse = "\n\n")


server_steps <- function() {
  list(
    step(
      "Press *Show*: `tip_show(\"onboarding\")` opens the hint, and `input$onboarding_hint` says `by = \"host\"`.",
      "nothing opens, or the input says reader.",
      "#srv_show",
      on_hint("onboarding", by = "host", via = "command")
    ),
    step(
      paste(
        "The hint stays open until *Hide*, a click elsewhere, Escape or another tip closes it, and the input then",
        "says why. *Hide* closes a hint and never a pin."
      ),
      "it closes when the pointer moves.",
      tip_target("onboarding"),
      on_hint("onboarding", state = "hidden", reason = c("host", "escape", "replaced", "outside"), by = NULL)
    ),
    step(
      paste(
        "Show the hint, then press *Change the tip*: the open hint takes the new words, or the template and its",
        "record, in place. Choose *its words and the side it opens on*, hide it and show it again: it opens below the",
        "tag; *its side, taken away* puts it back where the page puts it."
      ),
      "it closes, keeps the old words, or opens on the old side.",
      "#srv_update"
    ),
    step(
      "Press *Draw*: a tag appears after the colon and its tip opens at once, from the same observer.",
      "a tag with no tip, or a tip that opens on nothing.",
      "#srv_draw",
      on_hint("srv_new_tip", by = "host", via = "command")
    )
  )
}

plate_server_ui <- function() {
  plate(
    "server",
    lede = htmltools::tagList(
      htmltools::tags$code("tip_show(), tip_hide()"),
      "and",
      htmltools::tags$code("tip_update()"),
      "find a tip by the id given to",
      htmltools::tags$code("tip_on()"),
      "and answer through the hint input. An update changes only what you name:",
      htmltools::tags$code("NULL"),
      "leaves a value alone,",
      htmltools::tags$code("NA"),
      "takes it away and",
      htmltools::tags$code("\"\""),
      "clears the words. Name",
      htmltools::tags$code("session = session"),
      "since it comes after the dots."
    ),
    caption = paste(
      "The buttons call tip_show(), tip_hide() and tip_update() in the server. Draw renders a new tag and shows its",
      "tip in the same observer."
    ),
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
  # Show, hide and draw run from their shown text. Update runs the handler of its shown call, so the choice reaches it.
  pieces <- server_pieces()
  run_server_code(paste(pieces[c("show", "hide", "draw")], collapse = "\n"), input, session, output = output)
  shiny::observeEvent(input$srv_update, {
    piece <- server_pieces(input$server_change)$update
    env <- new.env(parent = book_exports())
    env$session <- session
    eval(parse(text = piece, keep.source = FALSE)[[1]][[3]], env)
  })
  calls <- c(
    srv_show = "tip_show(\"onboarding\")",
    srv_hide = "tip_hide(\"onboarding\")",
    srv_draw = "tip_show(\"srv_new_tip\")"
  )
  for (name in names(calls)) {
    local({
      name <- name
      shiny::observeEvent(input[[name]], log_line(session, "server", "server", calls[[name]], "out"))
    })
  }
  shiny::observeEvent(input$srv_update, {
    args <- server_update_args(input$server_change)
    log_line(session, "server", "server", paste0("tip_update(\"onboarding\", ", args, ")"), "out")
  })
  log_inputs(input, session, "server", c("onboarding_hint", "srv_new_tip_hint"))
  ticks
}
