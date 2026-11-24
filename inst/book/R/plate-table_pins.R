# Plate 6.3, Pins and inputs on tables: a row's tip pinned with its handle, and the inputs a table sends for its rows
# (`tip_table(pin =, events =, id =)`).

table_pins_pins <- c("pinnable" = "on", "hover only" = "off")

# The table's id, and its rows' tip ids: `table_pins_rows/row/<n>`.
table_pins_id <- "table_pins_rows"
table_pins_rows <- function() table_row_ids(table_pins_id, nrow(book_queues))

table_pins_code <- function(settings) {
  pin <- choice(settings$pin, table_pins_pins, "on")
  events <- events_of(settings$events)
  rows <- code_call(
    "tip_template",
    head = code_call("tip_title", code_str("{{ queue }}")),
    body = code_call(
      "list",
      code_call("tip_text", code_str("{{ open | number:0 }} open, median wait {{ wait_min | number:0 }} min.")),
      code_call("tip_code", code_str("query"), language = code_str("r"), copy = "TRUE")
    )
  )
  tipped <- code_call(
    "tip_table",
    rows = rows,
    pin = if (pin == "on") "TRUE",
    id = code_str(table_pins_id),
    events = code_chr(events)
  )
  table_code("queues", book_queues[c("queue", "open", "wait_min", "query")], table_default(), tipped)
}

table_pins_covers <- c(
  "tip_table(widget)",
  "tip_table(rows)",
  "tip_table(pin)",
  "tip_table(events)",
  "tip_table(id)",
  "tip_template(head)",
  "tip_template(body)",
  "tip_title(text)",
  "tip_text(text)",
  "tip_code(field)",
  "tip_code(language)",
  "tip_code(copy)"
)

table_pins_steps <- function() {
  rows <- table_pins_rows()
  reader <- function(value, ...) fields_match(value, by = "reader", ...)
  list(
    step(
      paste(
        "Rest on a row: its tip opens, and `input[[\"table_pins_rows/row/<n>_hint\"]]` arrives with state `shown`.",
        "With *pinnable*, a pin handle sits in the tip's corner."
      ),
      "the tip shows another row's words, or `<n>` is not the row's place in the data.",
      "#table_pins .stage",
      on_hint(rows)
    ),
    step(
      paste(
        "Press the handle in the tip's corner with the mouse (this step needs a mouse on the handle: a click on the",
        "row selects it and never pins): the tip stays, and `input[[\"table_pins_rows/row/<n>_tip\"]]` arrives with",
        "state `pinned`, `via` `click` and `kind` `table_row`."
      ),
      "a click on the row pins it, or the handle also selects the row.",
      "#table_pins .stage",
      proof(paste0(rows, "_tip"), function(value, input) reader(value, state = "pinned", via = "click"))
    ),
    step(
      paste(
        "Tab into the table, move to a row with the arrow keys and press P: the same, with `via` `key`. A cell has no",
        "pin of its own, so P on a cell pins its row."
      ),
      "P does nothing on a row, or a pin opens for another row.",
      "#table_pins .stage",
      proof(paste0(rows, "_tip"), function(value, input) reader(value, state = "pinned", via = "key"))
    ),
    step(
      paste(
        "Press Copy under the query in a pinned tip (choose *hint, pin and copy* if the log shows no copy):",
        "`input[[\"table_pins_rows/row/<n>_copy\"]]` arrives with `ok = TRUE`."
      ),
      "no Copy button, or no input.",
      "#table_pins .stage",
      on_copy(rows)
    ),
    step(
      paste(
        "Choose *hover only*: the handle is gone, P does nothing and the log stays silent for pins, even though the",
        "events name them. The tip still shows."
      ),
      "a handle or a pin that is not asked for.",
      "#table_pins_pin",
      NULL
    )
  )
}

plate_table_pins_ui <- function() {
  plate(
    "table_pins",
    lede = htmltools::tagList(
      htmltools::tags$code("tip_table(pin = TRUE)"),
      "lets a reader keep a row's tip open, and",
      htmltools::tags$code("events ="),
      "sends the row's tip to the server, under the name of the table's",
      htmltools::tags$code("id"),
      "and the row's place in the data."
    ),
    caption = paste(
      "Four queues of a support desk. Only rows pin and send inputs; cells and header cells send none. Paged and",
      "sorted tables keep each row's own id."
    ),
    steps = table_pins_steps(),
    code = code_text(table_pins_code(list())),
    log_empty = "Nothing yet. Rest on a row, then pin it.",
    controls = htmltools::tagList(
      "Make the rows",
      choice_set("table_pins_pin", "Pin", table_pins_pins),
      "and send",
      choice_set("table_pins_events", "Events", event_choices, "all")
    )
  )
}

plate_table_pins_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "table_pins", table_pins_steps())
  code <- shiny::reactive(table_pins_code(list(pin = input$table_pins_pin, events = input$table_pins_events)))
  plate_code(input, output, session, "table_pins", code, needs = table_default)
  log_inputs(input, session, "table_pins", c(outer(table_pins_rows(), c("_hint", "_tip", "_copy"), paste0)))
  ticks
}
