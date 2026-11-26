# Section "inputs" (its number and title are in contents.R): the three inputs a tip can send (`events =`), the `kind` each carries, and a
# small server that reads them.

inputs_kinds <- c("metric" = "metric", "review_item" = "review_item", "none given" = "none")

# The kind the first tip should send for a choice: the word chosen, or the engine's own when none is given.
inputs_kind_of <- function(x) {
  kind <- choice(x, inputs_kinds, "metric")
  if (kind == "none") "element" else kind
}

inputs_code <- function(settings) {
  events <- events_of(settings$events)
  kind <- choice(settings$kind, inputs_kinds, "metric")
  data <- code_df("column", book_columns[book_columns$name == "wait_min", c("name", "about", "snippet")])
  define <- code_call(
    "tip_define",
    inputs_column = code_call(
      "tip_template",
      head = code_call("tip_title", code_str("{{ name }}")),
      body = code_call(
        "list",
        code_call("tip_text", code_str("{{ about }}")),
        code_call("tip_code", code_str("snippet"), language = code_str("r"), copy = "TRUE")
      )
    )
  )
  metric <- code_call(
    "tip_on",
    code_word("Median wait", class = "col-chip"),
    code_str("Median time from a ticket opening to its first reply."),
    kind = if (kind != "none") code_str(kind),
    pin = "TRUE",
    id = code_str("inputs_metric"),
    events = code_chr(events)
  )
  tagged <- code_call(
    "tip_on",
    code_word(I("column$name"), class = "col-chip"),
    template = code_str("inputs_column"),
    record = "column",
    kind = code_str("column"),
    pin = "TRUE",
    id = code_str("inputs_column"),
    events = code_chr(events)
  )
  paste(
    c(
      "# Once in the page's UI: the template of the column's tip, with a code block a reader can copy",
      define,
      "",
      data,
      "",
      "# Two tips that tell the server when they open, pin and copy",
      code_call("htmltools::div", metric, tagged, class = code_str("cols"))
    ),
    collapse = "\n"
  )
}

# The server's code: what it does with each input. `heard` stands for whatever your app would do (update a text, run a
# query); the book reads it from outside this code and adds a line to the log. Shown, and run, as it is.
inputs_server_code <- function() {
  paste(
    c(
      "# What the server heard, in a sentence; your app would update an output or run a query here",
      "heard <- shiny::reactiveVal(\"\")",
      "",
      "shiny::observeEvent(input$inputs_metric_hint, {",
      "  hint <- input$inputs_metric_hint",
      "  heard(paste(\"a\", hint$kind, \"hint\", hint$state, \"by\", hint$via))",
      "})",
      "",
      "shiny::observeEvent(input$inputs_column_tip, {",
      "  tip <- input$inputs_column_tip",
      "  heard(paste(\"the\", tip$kind, \"tip\", tip$state, \"by\", tip$by, tip$via))",
      "})",
      "",
      "shiny::observeEvent(input$inputs_column_copy, {",
      "  copy <- input$inputs_column_copy",
      "  done <- if (isTRUE(copy$ok)) \"done\" else \"refused\"",
      "  heard(paste(\"copied\", copy$field, \"block\", copy$block, done))",
      "})"
    ),
    collapse = "\n"
  )
}


inputs_steps <- function() {
  list(
    step(
      paste(
        "Rest on *Median wait*: `input$inputs_metric_hint` arrives with state `shown`, the `text` of the tip and",
        "the `kind` you chose; when the pointer leaves it sends `hidden` and a `reason`."
      ),
      "`kind` is missing, or says `element` when you gave one.",
      tip_target("inputs_metric"),
      proof("inputs_metric_hint", function(value, input) {
        fields_match(value, state = "shown", by = "reader") && identical(value$kind, inputs_kind_of(input$inputs_kind))
      })
    ),
    step(
      paste(
        "Pin *wait_min* with the button in its corner, or focus it and press P: `input$inputs_column_tip` arrives",
        "with state `pinned`, `kind` `column`, and `via` `click` or `key`."
      ),
      "no input arrives, or the `kind` is the metric's.",
      tip_target("inputs_column"),
      proof("inputs_column_tip", function(value, input) {
        fields_match(value, state = "pinned", by = "reader") && identical(value$kind, "column")
      })
    ),
    step(
      paste(
        "Press Copy in the pinned tip: `input$inputs_column_copy` arrives with `ok = TRUE`, the `field` that holds",
        "the code (`snippet`) and its `block` number."
      ),
      "`ok` is not `TRUE` after the clipboard took the text.",
      tip_target("inputs_column"),
      on_copy("inputs_column")
    ),
    step(
      paste(
        "Choose *hint*, then pin a tip: it pins, but no `input$..._tip` line reaches the log. Only the events you",
        "ask for are sent."
      ),
      "a pin line arrives that was not asked for.",
      "#inputs_events",
      NULL
    )
  )
}

plate_inputs_ui <- function() {
  plate(
    "inputs",
    lede = htmltools::tagList(
      "A tip tells the server when it opens, pins and copies, if you ask with",
      htmltools::tags$code("events ="),
      "and give it an",
      htmltools::tags$code("id ="),
      "to name the inputs after. Each input also carries the tip's",
      htmltools::tags$code("kind"),
      "."
    ),
    caption = paste(
      "Two tips on a support desk page. The margin's log shows each input as the server received it, and what the",
      "server made of it."
    ),
    steps = inputs_steps(),
    code = code_text(inputs_code(list()), inputs_server_code()),
    log_empty = "Nothing yet. Rest on a tip, then pin it.",
    log_title = "Inputs sent, and what the server heard",
    controls = htmltools::tagList(
      "Ask for",
      choice_set("inputs_events", "Events", event_choices, "all"),
      "and give the first tip the kind",
      choice_set("inputs_kind", "Kind", inputs_kinds)
    )
  )
}

plate_inputs_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "inputs", inputs_steps())
  code <- shiny::reactive(inputs_code(list(events = input$inputs_events, kind = input$inputs_kind)))
  plate_code(
    input,
    output,
    session,
    "inputs",
    code,
    server_code = shiny::reactive(inputs_server_code())
  )
  # The shown server code, run as it is shown. Outside it, the book adds what `heard` holds to the log.
  env <- run_server_code(inputs_server_code(), input, session)
  shiny::observeEvent(
    env$heard(),
    if (nzchar(env$heard())) log_line(session, "inputs", "heard()", env$heard(), "out"),
    ignoreInit = TRUE
  )
  log_inputs(
    input,
    session,
    "inputs",
    c("inputs_metric_hint", "inputs_metric_tip", "inputs_column_hint", "inputs_column_tip", "inputs_column_copy")
  )
  ticks
}
