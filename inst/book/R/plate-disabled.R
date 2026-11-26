# Section "disabled" (its number and title are in contents.R): a control that is switched off says why, at once, with a small lock.

disabled_states <- c("still loading" = "loading", "ready" = "ready")

disabled_code <- function() {
  paste(
    c(
      "# The reason shows while the control is switched off; its own tip shows when it is on",
      code_call(
        "tip_on",
        code_call("shiny::actionButton", code_str("export_csv"), code_str("Export CSV"), disabled = "TRUE"),
        code_str("Saves the visits of week 46 as a CSV file."),
        when_disabled = code_str("the data is still loading."),
        id = code_str("export_csv_tip"),
        events = code_str("hint")
      ),
      code_call(
        "tip_on",
        code_call("shiny::actionButton", code_str("send_report"), code_str("Send report"), disabled = "TRUE"),
        when_disabled = code_str("pick a week first."),
        id = code_str("send_report_tip"),
        events = code_str("hint")
      )
    ),
    collapse = "\n"
  )
}

disabled_server_code <- function(state) {
  on <- identical(choice(state, disabled_states, "loading"), "ready")
  paste0(
    "# When the data has loaded, switch the export on: its reason goes, its own tip comes back\n",
    "shiny::updateActionButton(session, \"export_csv\", disabled = ",
    if (on) "FALSE" else "TRUE",
    ")"
  )
}

# Whether the hint of Export CSV showed its reason ("Not available: ..."), read from the words the hint input carries.
reason_shown <- function(shown) {
  function(input) identical(startsWith(input$export_csv_tip_hint$text %||% "", "Not available"), shown)
}


disabled_steps <- function() {
  list(
    step(
      "Rest on *Export CSV*: \"Not available: the data is still loading.\" shows at once, with a lock.",
      "it waits like a hint, or shows the button's own tip.",
      "#export_csv",
      on_hint("export_csv_tip", when = reason_shown(TRUE))
    ),
    step(
      "Press Tab until *Send report* is reached: the keyboard reaches it although it is off, and its reason shows.",
      "Tab skips the switched-off buttons.",
      "#send_report",
      on_hint("send_report_tip", via = "focus")
    ),
    step(
      "Choose *ready*, then rest on *Export CSV*: the button works, and its own tip shows instead of the reason.",
      "the reason stays after the button is on.",
      "#disabled_state",
      on_hint("export_csv_tip", when = reason_shown(FALSE))
    )
  )
}

plate_disabled_ui <- function() {
  plate(
    "disabled",
    lede = paste(
      "A control that is switched off says why: \"Not available: ...\" with a small lock, at once and without a",
      "wait."
    ),
    caption = "Two buttons switched off, each with its reason. The server switches Export CSV on and off.",
    steps = disabled_steps(),
    code = code_text(disabled_code(), disabled_server_code("loading")),
    log_empty = "Nothing yet. Rest on a switched-off button.",
    controls = htmltools::tagList(
      "The data is",
      choice_set("disabled_state", "Data", disabled_states)
    )
  )
}

plate_disabled_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "disabled", disabled_steps())
  plate_code(
    input,
    output,
    session,
    "disabled",
    ui_code = shiny::reactive(disabled_code()),
    server_code = shiny::reactive(disabled_server_code(input$disabled_state))
  )
  shiny::observeEvent(
    input$disabled_state,
    {
      # The shown server code, run as it is shown.
      code <- disabled_server_code(input$disabled_state)
      run_server_code(code, input, session)
      log_line(session, "disabled", "server", sub("^.*\n", "", code), "out")
    },
    ignoreInit = TRUE
  )
  log_inputs(input, session, "disabled", c("export_csv_tip_hint", "send_report_tip_hint", "export_csv"))
  ticks
}
