# Section "tables": tips on the rows, cells and headers of a DT or reactable table, each filled in from its own row,
# and the Shiny inputs a pinnable row sends.

tables_parts <- c("rows" = "rows", "rows and headers" = "headers", "rows and a cell" = "cells")

tables_placements <- c("where they fit" = "auto", "to the right" = "right", "below" = "bottom")
tables_styles <- c("the page's look" = "page", "sober" = "sober", "editorial" = "editorial")
tables_pins <- c("only on rest" = "off", "pinnable, with inputs sent" = "on")

tables_code <- function(settings) {
  placement <- choice(settings$placement, tables_placements, "auto")
  style <- choice(settings$style, tables_styles, "page")
  pin <- choice(settings$pin, tables_pins, "off")
  widget <- choice(settings$widget, table_widgets, table_default())
  parts <- choice(settings$parts, tables_parts, "rows")
  words <- code_str("**{{ branch }}.** Opened {{ opened }}, {{ staff }} staff.")
  # The pin and its inputs belong to the rows' spec; the table only gives them its id.
  rows <- if (pin == "on") code_call("tip_spec", words, pin = "TRUE", events = code_chr(c("hint", "pin"))) else words
  tipped <- code_call(
    "tip_table",
    rows = rows,
    headers = if (parts == "headers") code_call("c", wait_min = code_str("Median minutes to the first reply.")),
    cells = if (parts == "cells") code_call("c", visits = code_str("{{ visits | number:0 }} visits in week 46.")),
    style = if (style != "page") code_str(style),
    placement = if (placement != "auto") code_str(placement),
    marker = if (parts != "rows") code_str("corner"),
    id = code_str("branches")
  )
  table_code("branches", book_branches, widget, tipped)
}


tables_steps <- function() {
  list(
    step(
      "Rest on a row: its tip opens beside the table, level with the row.",
      "the tip covers the row, or shows another row's words.",
      "#tables .stage"
    ),
    step(
      "Choose *rows and headers*: a corner marks the header; its tip explains the column.",
      "the corner moves the header's words.",
      "#tables_parts"
    ),
    step(
      "Choose *reactable*: the same tips, on the other widget.",
      "a tip shows the row that used to be there.",
      "#tables_widget"
    ),
    step(
      paste(
        "Choose *pinnable, with inputs sent*, then pin a row with its corner button or P: the tip stays open,",
        "`input$branches_pin` arrives with `target` `\"row\"`, the `row` (its place in the data) and `column` `NULL`,",
        "and `input$branches_pinned` lists the row."
      ),
      "a click on the row pins it, or the row's tip has no pin button, or `row` is missing.",
      "#tables_pin",
      on_pin("branches")
    ),
    step(
      "Choose *to the right*, then *sober*, and rest on a row: the tip opens on that side, in that look.",
      "the side or the look of one row differs from the others.",
      "#tables_placement"
    )
  )
}

plate_tables_ui <- function() {
  plate(
    "tables",
    lede = paste(
      "Tips on DT and reactable rows, cells and headers, each filled in from its own row. A row's pin and inputs go",
      "in its tip_spec(); the table's id names the inputs."
    ),
    caption = paste(
      "Three branches and their week. With pinnable, the rows' tip_spec() asks for inputs, and the table sends one per",
      "event, named after its id: branches_hint and branches_pin say which row (counted in the data, so a sort",
      "changes nothing), and branches_pinned lists the rows pinned now. Cells and headers send none."
    ),
    steps = tables_steps(),
    code = code_text(tables_code(list())),
    log_empty = "Nothing yet. Choose pinnable, then rest on a row.",
    controls = htmltools::tagList(
      "Draw the table with",
      choice_set("tables_widget", "Widget", table_widgets, table_default()),
      "and put tips on",
      choice_set("tables_parts", "Parts", tables_parts),
      "that open",
      choice_set("tables_placement", "Placement", tables_placements),
      "in",
      choice_set("tables_style", "Look", tables_styles),
      "and keep them",
      choice_set("tables_pin", "Pin", tables_pins)
    )
  )
}

plate_tables_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "tables", tables_steps())
  code <- shiny::reactive(
    tables_code(
      list(
        widget = input$tables_widget,
        parts = input$tables_parts,
        placement = input$tables_placement,
        style = input$tables_style,
        pin = input$tables_pin
      )
    )
  )
  plate_code(
    input,
    output,
    session,
    "tables",
    code,
    needs = shiny::reactive(choice(input$tables_widget, table_widgets, table_default()))
  )
  log_inputs(input, session, "tables", c("branches_hint", "branches_pin", "branches_pinned"))
  ticks
}
