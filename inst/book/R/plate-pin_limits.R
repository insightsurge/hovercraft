# Section "pin_limits": how many tips stay pinned (`max_pins`), the P, M and Escape keys (`keys`), a pin that closes
# because its item is gone, and one pin on a phone.

pin_limits_pins <- c("one pin" = "1", "up to three pins" = "3")
pin_limits_keys <- c(
  "all keys" = "all",
  "all but the pin key" = "no_pin",
  "all but the More key" = "no_more",
  "no letter keys" = "none"
)

# The columns of the visits table, one pinnable word each. The last one sits in a panel that can be collapsed.
pin_limits_columns <- c("visit_date", "region", "wait_min", "booked")
pin_limits_ids <- paste0("pin_limits_", pin_limits_columns)

# The `keys` value for a choice; the book's page passes it to use_hovercraft().
pin_limits_keys_value <- function(keys) {
  switch(keys, all = TRUE, no_pin = c(pin = FALSE), no_more = c(more = FALSE), none = FALSE)
}

# The page's two settings as code. The engine reads them once for the whole page, so the book's own page sets them:
# a choice here reloads the page with it in the address (see page.R), and every section then keeps it.
pin_limits_page_code <- function(pins, keys) {
  off <- switch(
    keys,
    all = NULL,
    no_pin = code_call("c", pin = "FALSE"),
    no_more = code_call("c", more = "FALSE"),
    none = "FALSE"
  )
  paste0(
    "# Once in the page's UI: how many tips stay pinned, and which letter keys work\n",
    code_call("use_hovercraft", max_pins = pins, keys = off)
  )
}

# The figure as code: four columns that can be pinned; the fourth is in an accordion panel.
pin_limits_figure_code <- function() {
  about <- c(
    book_columns$about[match(pin_limits_columns[1:3], book_columns$name)],
    "TRUE when the visit ended in a booking."
  )
  chip <- function(i) {
    code_call(
      "tip_on",
      code_word(pin_limits_columns[[i]], class = "col-chip"),
      code_str(about[[i]]),
      pin = "TRUE",
      id = code_str(pin_limits_ids[[i]]),
      events = code_str("pin")
    )
  }
  paste(
    c(
      "# Four columns, each with a pinned tip that sends its pin and pinned inputs",
      code_call("htmltools::div", chip(1), chip(2), chip(3), class = code_str("cols")),
      "",
      "# The fourth sits in a panel: collapse the panel and its pinned tip closes, as its item is gone",
      code_call(
        "bslib::accordion",
        code_call("bslib::accordion_panel", code_str("More columns"), chip(4)),
        open = code_str("More columns")
      )
    ),
    collapse = "\n"
  )
}

pin_limits_code <- function(settings) {
  pins <- choice(settings$pins, pin_limits_pins, "1")
  keys <- choice(settings$keys, pin_limits_keys, "all")
  paste(c(pin_limits_page_code(pins, keys), "", pin_limits_figure_code()), collapse = "\n")
}

pin_limits_steps <- function() {
  ids <- pin_limits_ids
  list(
    step(
      "Pin *visit_date*, then *region*: the first closes by itself, with reason `replaced`; its `_pinned` is `FALSE`.",
      "two tips stay open, or the first closes with another reason.",
      "#pin_limits .cols",
      on_pin(ids, state = "closed", reason = "replaced")
    ),
    step(
      "Press Tab to reach *wait_min*, then P: it pins, with `via` `key`. The key works while the focus is on the item.",
      "the key does nothing, or pins while the focus is in a text field.",
      tip_target("pin_limits_wait_min"),
      proof("pin_limits_wait_min_pin", function(value, input) {
        fields_match(value, state = "pinned", via = "key", by = "reader")
      })
    ),
    step(
      "Press Escape: the pinned tip closes, with reason `escape`. Escape always closes, whatever `keys` says.",
      "the tip stays open.",
      "#pin_limits .cols",
      on_pin(ids, state = "closed", reason = "escape")
    ),
    step(
      "Pin *booked*, then press the header *More columns* to collapse the panel: the tip closes, with reason `gone`.",
      "the tip floats over an empty place, or comes back when the panel opens again.",
      tip_target("pin_limits_booked"),
      on_pin(ids[[4]], state = "closed", reason = "gone", by = NULL)
    ),
    step(
      "Make the window narrower than 600 px and pin two tips in turn: only the second stays, whatever `max_pins` says.",
      "two tips stay open on a phone.",
      "#pin_limits .cols"
    )
  )
}

plate_pin_limits_ui <- function(query = list()) {
  plate(
    "pin_limits",
    lede = htmltools::tagList(
      "A page keeps one pinned tip at a time. Raise that with",
      htmltools::tags$code("use_hovercraft(max_pins =)"),
      "(1 to 10): at the limit the oldest pin closes, with the reason",
      htmltools::tags$code("replaced"),
      ". The letter keys are the page's too:",
      htmltools::tags$code("keys ="),
      "switches P (pin) and M (the long form) off, all or one. Escape always closes."
    ),
    caption = paste(
      "Four columns of a visits table. The engine reads the page's limit and keys once, when the page loads, so a",
      "choice here reloads this book with it, and every section keeps it until you choose again."
    ),
    steps = pin_limits_steps(),
    code = code_text(pin_limits_code(list())),
    log_empty = "Nothing yet. Pin a column.",
    controls = htmltools::tagList(
      "Keep",
      choice_set("pin_limits_pins", "Pins kept open", pin_limits_pins, query$max_pins %||% "1", reload = "max_pins"),
      "open, with",
      choice_set("pin_limits_keys", "Letter keys", pin_limits_keys, query$keys %||% "all", reload = "keys"),
      "working"
    )
  )
}

plate_pin_limits_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "pin_limits", pin_limits_steps())
  code <- shiny::reactive(pin_limits_code(list(pins = input$pin_limits_pins, keys = input$pin_limits_keys)))
  plate_code(
    input,
    output,
    session,
    "pin_limits",
    ui_code = code,
    figure_code = shiny::reactive(pin_limits_figure_code())
  )
  log_inputs(input, session, "pin_limits", c(paste0(pin_limits_ids, "_pin"), paste0(pin_limits_ids, "_pinned")))
  ticks
}
