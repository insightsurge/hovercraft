# Plate 5, Pins: a tip a reader keeps open, with the pin button in its corner, the P key, Copy on a code block, and
# the pin and copy inputs.

pins_placements <- c("where they fit" = "auto", "above" = "top", "below" = "bottom", "beside" = "right")
pins_copy <- c("a Copy button" = "on", "no Copy button" = "off")

pins_code <- function(settings) {
  placement <- choice(settings$placement, pins_placements, "auto")
  copy <- choice(settings$copy, pins_copy, "on")
  data <- paste0(
    "columns <- data.frame(\n",
    paste0(
      "  ",
      names(studio_columns),
      " = ",
      vapply(studio_columns, code_chr, ""),
      collapse = ",\n"
    ),
    "\n)"
  )
  define <- code_call(
    "tip_define",
    column = code_call(
      "tip_template",
      head = code_call("tip_title", code_str("{{ name }}")),
      body = code_call(
        "list",
        code_call("tip_text", code_str("{{ about }}")),
        code_call(
          "tip_fields",
          Type = code_str("{{ type }}"),
          Missing = code_str("{{ missing }}"),
          Range = code_str("{{ range }}")
        ),
        code_call("tip_code", code_str("snippet"), language = code_str("r"), copy = if (copy == "on") "TRUE")
      )
    )
  )
  place <- if (placement != "auto") code_str(placement)
  chips <- paste0(
    "htmltools::div(\n",
    "  class = \"cols\",\n",
    "  lapply(seq_len(nrow(columns)), function(i) {\n",
    "    ",
    gsub(
      "\n",
      "\n    ",
      code_call(
        "tip_on",
        code_call("htmltools::tags$span", "columns$name[[i]]", tabindex = code_str("0"), class = code_str("col-chip")),
        template = code_str("column"),
        record = "columns[i, ]",
        placement = place,
        pin = "TRUE",
        id = "paste0(\"col_\", columns$name[[i]])",
        events = code_chr(c("hint", "pin", "copy"))
      ),
      fixed = TRUE
    ),
    "\n  }),\n  ",
    gsub(
      "\n",
      "\n  ",
      code_call(
        "tip_on",
        code_call("htmltools::tags$span", code_str("booked"), tabindex = code_str("0"), class = code_str("col-chip")),
        code_str("TRUE when the visit ended in a booking."),
        placement = place,
        pin = "TRUE",
        id = code_str("col_booked"),
        events = code_chr(c("hint", "pin"))
      ),
      fixed = TRUE
    ),
    "\n)"
  )
  keep <- "# use_hovercraft(max_pins = 3) in the page's UI keeps up to three pins open; the default is one"
  paste(
    c(data, "", "# Once in the page's UI", keep, define, "", "# A pinnable chip per column", chips),
    collapse = "\n"
  )
}

pins_steps <- function() {
  cols <- c("col_visit_date", "col_region", "col_wait_min", "col_booked")
  list(
    step(
      "Rest on *booked*: the pin button sits in the tip's top-right corner, on the sentence's line.",
      "it wraps below or covers words.",
      tip_target("col_booked"),
      on_hint("col_booked")
    ),
    step(
      paste(
        "Pin *wait_min* with the corner button, or focus it and press P: the tip stays open, with a close button,",
        "and `input$col_wait_min_pin` arrives."
      ),
      "a click on the chip itself pins it.",
      tip_target("col_wait_min"),
      on_pin("col_wait_min")
    ),
    step(
      "Pin *region* while *wait_min* is pinned: the first closes with reason `replaced`.",
      "two tips stay open.",
      tip_target("col_region"),
      on_pin(cols, state = "closed", reason = "replaced")
    ),
    step(
      "Press Copy in a pinned tip: it says Copied, and `input$<id>_copy` arrives with `ok = TRUE`.",
      "nothing on the clipboard, or no input.",
      tip_target("col_wait_min"),
      on_copy(cols[1:3])
    ),
    step(
      "Click the page or press Escape: the pinned tip closes, with reason `outside` or `escape`.",
      "it stays open, or closes on a click inside the tip.",
      "#pins .cols",
      on_pin(cols, state = "closed", reason = c("outside", "escape"))
    )
  )
}

plate_pins_ui <- function() {
  plate(
    "pins",
    5,
    "Pins",
    lede = paste(
      "A pinned tip stays open, so a reader can select its text, copy its code and act on it. One pin per page, as",
      "here: pinning another closes the first. use_hovercraft(max_pins = 3) keeps three."
    ),
    caption = paste(
      "The columns of a visits table. Three carry a template with fields and code; booked carries one sentence, so its",
      "pin button sits on the sentence's line."
    ),
    steps = pins_steps(),
    code = code_text(pins_code(list())),
    log_empty = "Nothing yet. Pin a column.",
    controls = htmltools::tagList(
      "Open pinned tips",
      choice_set("pins_placement", "Placement", pins_placements),
      "the column, and give code blocks",
      choice_set("pins_copy", "Copy button", pins_copy),
      "."
    )
  )
}

plate_pins_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "pins", pins_steps())
  code <- shiny::reactive(pins_code(list(placement = input$pins_placement, copy = input$pins_copy)))
  plate_code(input, output, session, "pins", code)
  ids <- c("col_visit_date", "col_region", "col_wait_min", "col_booked")
  log_inputs(input, session, "pins", c(paste0(ids, "_pin"), paste0(ids[1:3], "_copy")))
  ticks
}
