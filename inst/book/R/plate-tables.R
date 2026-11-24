# Plate 8, Tables: tips on the rows, cells and headers of a DT or reactable table, each from its own row.

tables_widgets <- c("DT" = "DT", "reactable" = "reactable")

# The widget the plate opens on: DT when it is installed, else reactable.
# The table package a section draws with: DT when it is installed, else reactable. Sections 6.2 and 6.3 use it too.
tables_default <- function() if (requireNamespace("DT", quietly = TRUE)) "DT" else "reactable"
tables_parts <- c("rows" = "rows", "rows and headers" = "headers", "rows and a cell" = "cells")

tables_placements <- c("where they fit" = "auto", "to the right" = "right", "below" = "bottom")
tables_styles <- c("the page's look" = "page", "sober" = "sober", "editorial" = "editorial")
tables_pins <- c("only on rest" = "off", "pinnable, with inputs sent" = "on")

tables_code <- function(settings) {
  placement <- choice(settings$placement, tables_placements, "auto")
  style <- choice(settings$style, tables_styles, "page")
  pin <- choice(settings$pin, tables_pins, "off")
  widget <- choice(settings$widget, tables_widgets, tables_default())
  parts <- choice(settings$parts, tables_parts, "rows")
  data <- paste0(
    "branches <- data.frame(\n",
    paste0(
      "  ",
      names(book_branches),
      " = ",
      vapply(
        book_branches,
        function(x) {
          if (is.character(x)) code_chr(x) else paste0("c(", paste(x, collapse = ", "), ")")
        },
        ""
      ),
      collapse = ",\n"
    ),
    "\n)"
  )
  table <- if (widget == "DT") {
    code_call("DT::datatable", "branches", rownames = "FALSE", options = "list(dom = \"t\")")
  } else {
    code_call("reactable::reactable", "branches")
  }
  tipped <- code_call(
    "tip_table",
    rows = code_str("**{{ branch }}.** Opened {{ opened }}, {{ staff }} staff."),
    headers = if (parts == "headers") code_call("c", wait_min = code_str("Median minutes to the first reply.")),
    cells = if (parts == "cells") code_call("c", visits = code_str("{{ visits | number:0 }} visits in week 46.")),
    style = if (style != "page") code_str(style),
    placement = if (placement != "auto") code_str(placement),
    marker = if (parts != "rows") code_str("corner"),
    id = code_str("branches"),
    pin = if (pin == "on") "TRUE",
    events = if (pin == "on") code_chr(c("hint", "pin"))
  )
  paste0(data, "\n\n", table, " |>\n  ", gsub("\n", "\n  ", tipped, fixed = TRUE))
}

tables_covers <- c(
  "tip_table(widget)",
  "tip_table(rows)",
  "tip_table(cells)",
  "tip_table(headers)",
  "tip_table(style)",
  "tip_table(placement)",
  "tip_table(marker)",
  "tip_table(id)",
  "tip_table(pin)",
  "tip_table(events)"
)

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
      "Choose *pinnable, with inputs sent*, then pin a row with its corner button or P: the tip stays open.",
      "a click on the row pins it, or the row's tip has no pin button.",
      "#tables_pin"
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
    lede = "Tips on DT and reactable rows, cells and headers, each filled in from its own row.",
    caption = paste(
      "Three branches and their week. The row tips send their hint and pin inputs under ids the package builds, so",
      "these steps are ticked by hand."
    ),
    steps = tables_steps(),
    code = code_text(tables_code(list())),
    log_empty = "The book does not follow a table tip's inputs: the ids are the package's own.",
    controls = htmltools::tagList(
      "Draw the table with",
      choice_set("tables_widget", "Widget", tables_widgets, tables_default()),
      "and put tips on",
      choice_set("tables_parts", "Parts", tables_parts),
      "; open them",
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
  missing <- shiny::reactive({
    widget <- choice(input$tables_widget, tables_widgets, tables_default())
    if (!requireNamespace(widget, quietly = TRUE)) widget
  })
  plate_code(input, output, session, "tables", code, draw = FALSE)
  exports <- book_exports()
  result <- shiny::reactive({
    if (!is.null(missing())) {
      return(list(ui = NULL, warnings = character(), error = paste0("This plate needs the ", missing(), " package.")))
    }
    run_code(code(), exports)
  })
  output$tables_figure <- shiny::renderUI(result()$ui)
  output$tables_notes <- shiny::renderUI(notes_ui(result()))
  ticks
}
