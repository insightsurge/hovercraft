# Section 6.2, One column its own way: a `tip_spec()` for one column of `cells =` or `headers =` beats the table's own
# placement, look and marker for that column only.

columns_settings <- c(
  "where it opens" = "placement",
  "its look" = "style",
  "its marker" = "marker",
  "a long form" = "more"
)
columns_names <- c("open" = "open", "wait_min" = "wait_min", "owner" = "owner")
columns_targets <- c("cells" = "cells", "header" = "headers")

# What each column's tip says, on a cell and on its header.
columns_words <- list(
  cells = c(
    open = "**{{ queue }}**: {{ open | number:0 }} tickets open.",
    wait_min = "**{{ queue }}**: first reply after {{ wait_min | number:0 }} min, in business hours.",
    owner = "**{{ queue }}** is looked after by {{ owner }}."
  ),
  headers = c(
    open = "Tickets waiting for a reply or a fix.",
    wait_min = "Median minutes to the first reply.",
    owner = "Who looks after the queue."
  )
)
columns_long <- c(
  cells = "{{ queue }} over the last 7 days, business hours only. Auto-closed spam is left out.",
  headers = "Counted over the last 7 days, business hours only. Auto-closed spam is left out."
)

columns_code <- function(settings) {
  setting <- choice(settings$setting, columns_settings, "placement")
  column <- choice(settings$column, columns_names, "wait_min")
  target <- choice(settings$target, columns_targets, "cells")
  # The table's own settings are always given, so the spec has something to beat.
  spec <- code_call(
    "tip_spec",
    code_str(columns_words[[target]][[column]]),
    placement = if (setting == "placement") code_str("right"),
    style = if (setting == "style") code_str("lively"),
    marker = if (setting == "marker") code_str("none"),
    more = if (setting == "more") code_str(columns_long[[target]]),
    more_on = if (setting == "more") code_str("hover")
  )
  entries <- lapply(columns_names, function(name) {
    if (name == column) spec else code_str(columns_words[[target]][[name]])
  })
  tipped <- do.call(
    code_call,
    c(
      list("tip_table"),
      stats::setNames(list(do.call(code_call, c(list("list"), entries))), target),
      list(placement = code_str("bottom"), style = code_str("editorial"), marker = code_str("corner"))
    )
  )
  table_code("queues", book_queues[c("queue", "open", "wait_min", "owner")], table_default(), tipped)
}

columns_covers <- c(
  "tip_table(widget)",
  "tip_table(cells)",
  "tip_table(headers)",
  "tip_table(placement)",
  "tip_table(style)",
  "tip_table(marker)",
  "tip_spec(content)",
  "tip_spec(placement)",
  "tip_spec(style)",
  "tip_spec(marker)",
  "tip_spec(more)",
  "tip_spec(more_on)"
)

# Cells and headers send no inputs (only the tips of rows do), so every step here is ticked by hand.
columns_steps <- function() {
  list(
    step(
      paste(
        "Rest on a cell of *wait_min*, then on a cell of *open*: the two tips differ in the one setting you chose,",
        "and in no other. The table gives every tip the side *bottom*, the look *editorial* and a corner mark."
      ),
      "the other columns change as well, or *wait_min* changes in more than one way.",
      "#columns .stage"
    ),
    step(
      "Choose *its marker*: *wait_min* loses its corner mark and keeps its tip; the other columns keep theirs.",
      "the mark goes from every column, or the tip goes with it.",
      "#columns_setting"
    ),
    step(
      paste(
        "Choose *a long form*, then rest on the column: its tip shows the long form on hover. Another column's",
        "tip stays one short text."
      ),
      "every column grows, or the long form never shows.",
      "#columns_setting"
    ),
    step(
      "Choose *header*, then rest on the header of the chosen column: the same setting, on the header's tip.",
      "the header keeps the table's side and look.",
      "#columns_target"
    )
  )
}

plate_columns_ui <- function() {
  plate(
    "columns",
    lede = htmltools::tagList(
      "A column that needs a setting of its own takes a",
      htmltools::tags$code("tip_spec()"),
      "in place of words. It beats the table's settings for that column and leaves the rest alone."
    ),
    caption = paste(
      "Four queues of a support desk. Cells and headers send no Shiny inputs, so these steps are ticked by hand."
    ),
    steps = columns_steps(),
    code = code_text(columns_code(list())),
    log_empty = "Cell and header tips send no inputs.",
    controls = htmltools::tagList(
      "Give the column",
      choice_set("columns_column", "Column", columns_names, "wait_min"),
      "its own setting for",
      choice_set("columns_setting", "Setting", columns_settings),
      "on its",
      choice_set("columns_target", "Where", columns_targets)
    )
  )
}

plate_columns_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "columns", columns_steps())
  settings <- shiny::reactive(list(
    setting = input$columns_setting,
    column = input$columns_column,
    target = input$columns_target
  ))
  code <- shiny::reactive(columns_code(settings()))
  plate_code(input, output, session, "columns", code, needs = table_default)
  ticks
}
