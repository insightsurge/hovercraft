# Section "table_pins": a row's tip pinned with its handle, the inputs a table sends for its rows, and the table's own
# pin count (`tip_spec(pin =, events =)`, `tip_table(id =, max_pins =)`).

table_pins_counts <- c("the page's count" = "page", "two of its own" = "own")

# The table's id: its inputs are `table_pins_rows_hint`, `_pin`, `_copy` and `_pinned`.
table_pins_id <- "table_pins_rows"

table_pins_code <- function(settings) {
  count <- choice(settings$count, table_pins_counts, "page")
  events <- events_of(settings$events)
  rows <- code_call(
    "tip_spec",
    code_call(
      "tip_template",
      head = code_call("tip_title", code_str("{{ queue }}")),
      body = code_call(
        "list",
        code_call("tip_text", code_str("{{ open | number:0 }} open, median wait {{ wait_min | number:0 }} min.")),
        code_call("tip_code", code_str("query"), language = code_str("r"), copy = "TRUE")
      )
    ),
    pin = "TRUE",
    events = code_chr(events)
  )
  tipped <- code_call(
    "tip_table",
    rows = rows,
    id = code_str(table_pins_id),
    max_pins = if (count == "own") "2"
  )
  table_code("queues", book_queues[c("queue", "open", "wait_min", "query")], table_default(), tipped)
}


table_pins_steps <- function() {
  id <- table_pins_id
  reader <- function(...) function(value, input) fields_match(value, by = "reader", ...)
  list(
    step(
      paste(
        "Rest on a row: its tip opens, and `input$table_pins_rows_hint` arrives with state `shown` and the `row`,",
        "counted in the data. A pin handle sits in the tip's corner."
      ),
      "the tip shows another row's words, or `row` is not the row's place in the data.",
      "#table_pins .stage",
      on_hint(id)
    ),
    step(
      paste(
        "Press the handle in the tip's corner with the mouse (a click on the row selects it and never pins): the",
        "tip stays, `input$table_pins_rows_pin` arrives with state `pinned` and `via` `click`, and",
        "`input$table_pins_rows_pinned` lists the row."
      ),
      "a click on the row pins it, or the handle also selects the row.",
      "#table_pins .stage",
      proof(paste0(id, "_pin"), reader(state = "pinned", via = "click"))
    ),
    step(
      paste(
        "Tab into the table, move to a row with the arrow keys and press P: the same, with `via` `key`. A cell has no",
        "pin of its own, so P on a cell pins its row."
      ),
      "P does nothing on a row, or a pin opens for another row.",
      "#table_pins .stage",
      proof(paste0(id, "_pin"), reader(state = "pinned", via = "key"))
    ),
    step(
      paste(
        "Press Copy under the query in a pinned tip (choose *hint, pin and copy* if the log shows no copy):",
        "`input$table_pins_rows_copy` arrives with `ok = TRUE`."
      ),
      "no Copy button, or no input.",
      "#table_pins .stage",
      on_copy(id)
    ),
    step(
      paste(
        "Choose *two of its own*, then pin three rows one after another: the third closes the oldest, with reason",
        "`replaced`, and `input$table_pins_rows_pinned` never holds more than two. With *the page's count* (one) the",
        "second pin already closes the first."
      ),
      "a third pin stays open, or a pin outside the table closes one of the table's.",
      "#table_pins_count",
      proof(paste0(id, "_pin"), function(value, input) {
        own <- identical(input$table_pins_count %||% "page", "own")
        own && reader(state = "closed", reason = "replaced")(value, input)
      })
    )
  )
}

plate_table_pins_ui <- function() {
  plate(
    "table_pins",
    lede = paste(
      "A row's tip_spec(pin = TRUE) lets a reader keep its tip open, and events = sends it to the server: one Shiny",
      "input per event, named after the table's id. tip_table(max_pins = ) gives the table a pin count of its own."
    ),
    caption = paste(
      "Four queues of a support desk. Only rows pin and send inputs; cells and header cells send none. Each input",
      "says which row it is about, counted in the data, so sorting and paging change nothing. Inside a module, give",
      "tip_table() the plain id: it adds the module's prefix itself."
    ),
    steps = table_pins_steps(),
    code = code_text(table_pins_code(list())),
    log_empty = "Nothing yet. Rest on a row, then pin it.",
    controls = htmltools::tagList(
      "Count this table's pins by",
      choice_set("table_pins_count", "Pin count", table_pins_counts),
      "and send",
      choice_set("table_pins_events", "Events", event_choices, "all")
    )
  )
}

plate_table_pins_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "table_pins", table_pins_steps())
  code <- shiny::reactive(table_pins_code(list(count = input$table_pins_count, events = input$table_pins_events)))
  plate_code(input, output, session, "table_pins", code, needs = table_default)
  log_inputs(input, session, "table_pins", paste0(table_pins_id, c("_hint", "_pin", "_copy", "_pinned")))
  ticks
}
