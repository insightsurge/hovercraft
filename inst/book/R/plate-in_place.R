# Section "in_place": a row's pinned tip drawn as a card under its row (`tip_spec(to = "in_place")`), or floating
# beside it, on DT and reactable.

in_place_opens <- c("under the row" = "in_place", "beside the row" = "float")

# The table's id: its inputs are `queue_cards_pin` and `queue_cards_pinned`.
in_place_id <- "queue_cards"

in_place_code <- function(settings) {
  open <- choice(settings$open, in_place_opens, "in_place")
  widget <- choice(settings$widget, table_widgets, table_default())
  rows <- code_call(
    "tip_spec",
    code_call(
      "tip_template",
      head = code_call("tip_title", code_str("{{ queue }}")),
      body = code_call(
        "list",
        code_call("tip_text", code_str("Looked after by {{ owner }}.")),
        code_call(
          "tip_fields",
          Open = code_str("{{ open | number:0 }}"),
          `Median wait` = code_str("{{ wait_min | number:0 }} min")
        )
      )
    ),
    pin = "TRUE",
    events = code_str("pin"),
    to = if (open == "in_place") code_str("in_place")
  )
  tipped <- code_call("tip_table", rows = rows, id = code_str(in_place_id))
  table_code("queues", book_queues[c("queue", "open", "wait_min", "owner")], widget, tipped)
}


in_place_steps <- function() {
  id <- in_place_id
  closed <- function(reason, ...) {
    on_pin(id, state = "closed", reason = reason, ...)
  }
  list(
    step(
      paste(
        "Rest on a row: its tip floats beside it. Press the handle in the tip's corner, or focus the row and press P:",
        "with *under the row* chosen, a card opens under the row and `input$queue_cards_pin` arrives with state",
        "`pinned`."
      ),
      "the card covers the next row, or opens under another row, or a click on the row selects it and pins.",
      "#in_place .stage",
      on_pin(id, when = chose("in_place_open", "in_place", "in_place"))
    ),
    step(
      "Click the page, away from the card: the card stays open. Rest on another row: its tip still floats.",
      "the card closes on a click away, or the other row's tip opens inside the first card.",
      "#in_place .stage"
    ),
    step(
      "Click a column header to sort the table: the card moves with its row.",
      "the card stays at the old place, or closes.",
      "#in_place .stage"
    ),
    step(
      "Press Escape, or the close button in the card: it closes, with reason `escape` or `button`.",
      "the card stays, or the row's tip closes with it.",
      "#in_place .stage",
      closed(c("escape", "button"), when = chose("in_place_open", "in_place", "in_place"))
    ),
    step(
      paste(
        "Choose *beside the row*, then pin a row and click the page: the pinned tip floats, and a click away closes it",
        "(reason `outside`)."
      ),
      "the pin opens as a card, or a click away leaves it open.",
      "#in_place_open",
      closed("outside", when = chose("in_place_open", "float", "in_place"))
    )
  )
}

plate_in_place_ui <- function() {
  plate(
    "in_place",
    lede = paste(
      "A pinned tip can open as a card under its row instead of floating beside it: room for fields, a long form or a",
      "live output. The hover tip still floats."
    ),
    caption = paste(
      "Four queues of a support desk. In place needs a pin, the rows' spec and the table's id, and opens by the pin",
      "handle or the P key only. DT's FixedColumns is refused with an error. In a server-mode DT, drawing the table",
      "again with renderDT() closes the card."
    ),
    steps = in_place_steps(),
    code = code_text(in_place_code(list())),
    log_empty = "Nothing yet. Pin a row.",
    controls = htmltools::tagList(
      "Draw the table with",
      choice_set("in_place_widget", "Widget", table_widgets, table_default()),
      "and open a pinned row's tip",
      choice_set("in_place_open", "Opens", in_place_opens)
    )
  )
}

plate_in_place_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "in_place", in_place_steps())
  code <- shiny::reactive(in_place_code(list(widget = input$in_place_widget, open = input$in_place_open)))
  plate_code(
    input,
    output,
    session,
    "in_place",
    code,
    needs = shiny::reactive(choice(input$in_place_widget, table_widgets, table_default()))
  )
  log_inputs(input, session, "in_place", paste0(in_place_id, c("_pin", "_pinned")))
  ticks
}
