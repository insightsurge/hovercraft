# Section "table_server": open, close and change the tip of a table's row, cell or header from the server, with `row`
# and `column` on tip_show(), tip_hide() and tip_update().

table_server_targets <- c("a row" = "row", "a cell" = "cell", "a header" = "header")
table_server_columns <- c("its name" = "name", "its position" = "position")

# The table's id, the column the cell and the header are in (the third of `book_branches`), and the new words.
table_server_id <- "branch_tips"
table_server_column <- "wait_min"
table_server_words <- "**Updated** from the server."

# The table and its three buttons, as the figure draws them.
table_server_ui_code <- function(widget = table_default()) {
  widget <- choice(widget, table_widgets, table_default())
  tipped <- code_call(
    "tip_table",
    rows = code_call(
      "tip_spec",
      code_str("**{{ branch }}.** Opened {{ opened }}, {{ staff }} staff."),
      events = code_str("hint")
    ),
    cells = code_call("c", wait_min = code_str("{{ branch }}: median wait {{ wait_min | number:0 }} min.")),
    headers = code_call("c", wait_min = code_str("Median minutes to the first reply.")),
    id = code_str(table_server_id)
  )
  buttons <- code_call(
    "htmltools::div",
    code_call("shiny::actionButton", code_str("tsrv_show"), code_str("Show")),
    code_call("shiny::actionButton", code_str("tsrv_hide"), code_str("Hide")),
    code_call("shiny::actionButton", code_str("tsrv_update"), code_str("Change the words"))
  )
  paste0(table_code("branches", book_branches, widget, tipped), "\n\n", buttons)
}

# The address of the tip: a row alone is a row, a row and a column a cell, a column alone a header.
table_server_address <- function(target = "row", by = "name") {
  target <- choice(target, table_server_targets, "row")
  column <- if (identical(choice(by, table_server_columns, "name"), "position")) "3" else code_str(table_server_column)
  list(row = if (target != "header") "2", column = if (target != "row") column)
}

# One call of the server's code, as text. `session` is left out for the log and given by name for the code shown.
table_server_call <- function(fn, target = "row", by = "name", session = TRUE) {
  address <- table_server_address(target, by)
  code_call(
    fn,
    code_str(table_server_id),
    text = if (fn == "tip_update") code_str(table_server_words),
    row = address$row,
    column = address$column,
    session = if (session) "session",
    width = 200L
  )
}

# The server's code, one line per button, shown in the code note and run by the server as it is shown.
table_server_lines <- function(target = "row", by = "name") {
  c(
    paste(
      "# `row` counts the data rows from 1; `column` is a name or a position. A name needs the table drawn first:",
      "before that, the call warns and sends nothing."
    ),
    paste0("shiny::observeEvent(input$tsrv_show, ", table_server_call("tip_show", target, by), ")"),
    paste0("shiny::observeEvent(input$tsrv_hide, ", table_server_call("tip_hide", target, by), ")"),
    paste0("shiny::observeEvent(input$tsrv_update, ", table_server_call("tip_update", target, by), ")")
  )
}

table_server_server_code <- function(target = "row", by = "name") {
  paste(table_server_lines(target, by), collapse = "\n")
}


table_server_steps <- function() {
  on_row <- chose("table_server_target", "row", "row")
  list(
    step(
      paste(
        "With *a row* chosen, press *Show*: `tip_show(\"branch_tips\", row = 2)` opens the tip of the second row of",
        "the data, and `input$branch_tips_hint` says `by = \"host\"`."
      ),
      "nothing opens, or the tip of another row does, or the input says reader.",
      "#tsrv_show",
      proof("branch_tips_hint", function(value, input) {
        fields_match(value, state = "shown", by = "host", row = 2) && isTRUE(on_row(input))
      })
    ),
    step(
      "The tip stays open until *Hide*, Escape or another tip closes it. Press *Hide*: it closes.",
      "it closes when the pointer moves, or *Hide* leaves it open.",
      "#tsrv_hide",
      on_hint("branch_tips", state = "hidden", reason = c("host", "escape", "replaced"), by = NULL, when = on_row)
    ),
    step(
      paste(
        "Choose *a cell*, then *a header*, and press *Show* each time: the tip of the *wait_min* cell in row 2 opens,",
        "then the tip of the *wait_min* header. Cells and headers send no input, so tick this step by hand."
      ),
      "the row's tip opens instead, or a header's call needs a row.",
      "#table_server_target"
    ),
    step(
      "Show a tip, then press *Change the words*: the open tip takes the new words in place.",
      "it closes, or keeps the old words.",
      "#tsrv_update"
    ),
    step(
      "Choose *its position* and press *Show* again: the same tip opens, as column 3 is *wait_min*.",
      "another column's tip opens, or the call needs the table drawn first.",
      "#table_server_by"
    )
  )
}

plate_table_server_ui <- function() {
  plate(
    "table_server",
    lede = paste(
      "tip_show(), tip_hide() and tip_update() reach a table's tips by the table's id, with row and column: a row",
      "alone is a row, a row and a column a cell, a column alone a header."
    ),
    caption = paste(
      "Three branches and their week. The buttons call the server's lines shown in the margin. A row counts its place",
      "in the data, so it is the same row after a sort. A row off the table's current page has no tip yet."
    ),
    steps = table_server_steps(),
    code = code_text(table_server_ui_code(), table_server_server_code()),
    log_empty = "Nothing yet. Press Show.",
    controls = htmltools::tagList(
      "Draw the table with",
      choice_set("table_server_widget", "Widget", table_widgets, table_default()),
      "and aim the calls at",
      choice_set("table_server_target", "Target", table_server_targets),
      "naming the column by",
      choice_set("table_server_by", "Column", table_server_columns)
    )
  )
}

plate_table_server_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "table_server", table_server_steps())
  widget <- shiny::reactive(choice(input$table_server_widget, table_widgets, table_default()))
  plate_code(
    input,
    output,
    session,
    "table_server",
    ui_code = shiny::reactive(table_server_ui_code(widget())),
    server_code = shiny::reactive(table_server_server_code(input$table_server_target, input$table_server_by)),
    needs = widget
  )
  # Each button runs the handler of its shown line, so the choices reach it. An error or warning from a call ends
  # nothing: the log says it.
  buttons <- c(tsrv_show = "tip_show", tsrv_hide = "tip_hide", tsrv_update = "tip_update")
  for (name in names(buttons)) {
    local({
      name <- name
      fn <- buttons[[name]]
      shiny::observeEvent(input[[name]], {
        target <- input$table_server_target
        by <- input$table_server_by
        line <- grep(name, table_server_lines(target, by), value = TRUE, fixed = TRUE)
        env <- new.env(parent = book_exports())
        env$session <- session
        tryCatch(
          eval(parse(text = line, keep.source = FALSE)[[1]][[3]], env),
          error = function(e) log_line(session, "table_server", "error", conditionMessage(e), "out")
        )
        log_line(session, "table_server", "server", table_server_call(fn, target, by, session = FALSE), "out")
      })
    })
  }
  log_inputs(input, session, "table_server", paste0(table_server_id, "_hint"))
  ticks
}
