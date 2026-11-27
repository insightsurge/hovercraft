# Section "inputs": the inputs a tip can send (`events =`: hint, pin and copy; `_pinned`), what each holds, and a small
# server that reads them.

inputs_code <- function(settings) {
  events <- events_of(settings$events)
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
    pin = "TRUE",
    id = code_str("inputs_metric"),
    events = code_chr(events)
  )
  tagged <- code_call(
    "tip_on",
    code_word(I("column$name"), class = "col-chip"),
    template = code_str("inputs_column"),
    record = "column",
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
      "  heard(paste(\"a hint\", hint$state, \"by\", hint$by, hint$via))",
      "})",
      "",
      "shiny::observeEvent(input$inputs_column_pin, {",
      "  pin <- input$inputs_column_pin",
      "  heard(paste(\"the column tip\", pin$state, \"by\", pin$by, pin$via))",
      "})",
      "",
      "# TRUE while the column tip is pinned, FALSE after; NULL until its first event",
      "shiny::observeEvent(input$inputs_column_pinned, {",
      "  heard(if (isTRUE(input$inputs_column_pinned)) \"the column tip is pinned\" else \"no column tip is pinned\")",
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
        "Rest on *Median wait*: `input$inputs_metric_hint` arrives with state `shown`, the `text` of the tip, `by`",
        "`reader` and `via` `pointer`; when the pointer leaves it sends `hidden` and a `reason`."
      ),
      "no input arrives, or `text` is empty.",
      tip_target("inputs_metric"),
      on_hint("inputs_metric")
    ),
    step(
      paste(
        "Pin *wait_min* with the button in its corner, or focus it and press P: `input$inputs_column_pin` arrives",
        "with state `pinned` and `via` `click` or `key`, and `input$inputs_column_pinned` is `TRUE`."
      ),
      "no input arrives, or `_pinned` stays `FALSE`.",
      tip_target("inputs_column"),
      on_pin("inputs_column")
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
        "Press Escape: `input$inputs_column_pin` arrives with state `closed` and a `reason`, and",
        "`input$inputs_column_pinned` is `FALSE`."
      ),
      "`_pinned` stays `TRUE` after the tip closed.",
      tip_target("inputs_column"),
      on_pin("inputs_column", state = "closed", reason = c("escape", "outside", "button"))
    ),
    step(
      paste(
        "Choose *hint*, then pin a tip: it pins, but no `input$..._pin` line reaches the log. Only the events you",
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
      "to name the inputs after. A pin also keeps a plain",
      htmltools::tags$code("_pinned"),
      "input, TRUE while the tip is pinned and FALSE after."
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
      choice_set("inputs_events", "Events", event_choices, "all")
    )
  )
}

plate_inputs_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "inputs", inputs_steps())
  code <- shiny::reactive(inputs_code(list(events = input$inputs_events)))
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
    c(
      "inputs_metric_hint",
      "inputs_metric_pin",
      "inputs_column_hint",
      "inputs_column_pin",
      "inputs_column_pinned",
      "inputs_column_copy"
    )
  )
  ticks
}
