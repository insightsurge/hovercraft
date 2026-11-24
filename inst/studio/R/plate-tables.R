# Plate 8, Tables: tips on the rows, cells and headers of a DT or reactable table, each from its own row.

tables_widgets <- c("DT" = "DT", "reactable" = "reactable")

# The widget the plate opens on: DT when it is installed, else reactable.
tables_default <- function() if (requireNamespace("DT", quietly = TRUE)) "DT" else "reactable"
tables_parts <- c("rows" = "rows", "rows and headers" = "headers", "rows and a cell" = "cells")

tables_code <- function(settings) {
  widget <- choice(settings$widget, tables_widgets, tables_default())
  parts <- choice(settings$parts, tables_parts, "rows")
  data <- paste0(
    "branches <- data.frame(\n",
    paste0(
      "  ",
      names(studio_branches),
      " = ",
      vapply(
        studio_branches,
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
    rows = code_call(
      "tip_spec",
      code_str("**{{ branch }}.** Opened {{ opened }}, {{ staff }} staff."),
      pin = "TRUE",
      events = code_str("pin")
    ),
    headers = if (parts == "headers") code_call("c", wait_min = code_str("Median minutes to the first reply.")),
    cells = if (parts == "cells") code_call("c", visits = code_str("{{ visits | number:0 }} visits in week 46.")),
    marker = if (parts != "rows") code_str("corner"),
    id = code_str("branches")
  )
  paste0(data, "\n\n", table, " |>\n  ", gsub("\n", "\n  ", tipped, fixed = TRUE))
}

# The server's code, shown in the code note and run by the server as it is shown: a table's tip by its row.
tables_server_code <- function() {
  "shiny::observeEvent(input$tables_show, tip_show(\"branches\", row = 2))"
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
      "Press *Show row 2's tip*: `tip_show(\"branches\", row = 2)` opens the tip of the second row of the data.",
      "nothing opens, or the tip of another row does.",
      "#tables_show"
    ),
    step(
      paste(
        "Pin a row with the handle in its tip's corner: `input$branches_pin` arrives with the data row,",
        "and `input$branches_pinned` lists it."
      ),
      "the row is selected, or the input has no `row`.",
      "#tables .stage",
      on_pin("branches")
    )
  )
}

plate_tables_ui <- function() {
  plate(
    "tables",
    8,
    "Tables",
    lede = "Tips on DT and reactable rows, cells and headers, each filled in from its own row.",
    caption = paste(
      "Three branches and their week. The rows can be pinned: one input for the table, `input$branches_pin`,",
      "says which row; the first steps are ticked by hand."
    ),
    steps = tables_steps(),
    code = code_text(tables_code(list()), tables_server_code()),
    log_empty = "Nothing yet. Pin a row.",
    controls = htmltools::tagList(
      "Draw the table with",
      choice_set("tables_widget", "Widget", tables_widgets, tables_default()),
      "and put tips on",
      choice_set("tables_parts", "Parts", tables_parts),
      shiny::actionButton("tables_show", "Show row 2's tip", class = "btn-sm")
    )
  )
}

plate_tables_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "tables", tables_steps())
  code <- shiny::reactive(tables_code(list(widget = input$tables_widget, parts = input$tables_parts)))
  missing <- shiny::reactive({
    widget <- choice(input$tables_widget, tables_widgets, tables_default())
    if (!requireNamespace(widget, quietly = TRUE)) widget
  })
  plate_code(
    input,
    output,
    session,
    "tables",
    ui_code = code,
    server_code = shiny::reactive(tables_server_code()),
    draw = FALSE
  )
  run_server_code(tables_server_code(), input, session)
  log_inputs(input, session, "tables", c("branches_pin", "branches_pinned"))
  exports <- studio_exports()
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
