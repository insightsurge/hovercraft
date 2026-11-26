# Section "start" (its number and title are in contents.R): the book's title, whose word holds a tip that shows most of what the package does, and a
# sentence with three tips on its figures, the plainest kind.

# The title as code: one word with a tip built from a template, a record, a long form, a look, a mark and a pin.
start_hero_code <- function() {
  record <- list(
    name = "hovercraft",
    stage = "experimental",
    tone = "info",
    about = "Tips on any HTML tag in a Shiny app or a bslib page.",
    shows = "hints, pins, templates, live output",
    pages = "Shiny, bslib, DT, reactable",
    code = "tip_on(\n  htmltools::span(\"word\", tabindex = \"0\"),\n  \"What the word means.\"\n)"
  )
  template <- code_call(
    "tip_template",
    head = code_call(
      "list",
      code_call("tip_title", code_str("{{ name }}")),
      code_call("tip_badge", code_str("{{ stage }}"), status = code_str("{{ tone }}"))
    ),
    body = code_call(
      "list",
      code_call("tip_text", code_str("{{ about }}")),
      code_call("tip_fields", Shows = code_str("{{ shows }}"), "Works in" = code_str("{{ pages }}")),
      code_call("tip_code", code_str("code"), language = code_str("r"), copy = "TRUE")
    ),
    foot = code_call("tip_text", code_str("Pin it, then Copy: that line is a first tip of your own."))
  )
  more <- paste(
    "Each part of this tip is a section of the book: the template, badge and fields (2.1), the pin and Copy",
    "(3.1), this long form (3.2), the editorial look and the slide (4.1), the book's colours (4.3), the icon (4.4)."
  )
  theme <- code_call(
    "tip_theme",
    light = code_list(list(bg = "#ffffff", ink = "#1b1f24", line = "#8fb2cf", code_bg = "#e2eaf1")),
    dark = code_list(list(bg = "#173b5e", ink = "#eef3f7", line = "#4b7aa3", code_bg = "#0f2c47"))
  )
  word <- code_call(
    "tip_on",
    code_word("hovercraft"),
    template = code_str("hovercraft"),
    record = code_list(record),
    more = code_str(more),
    style = code_call("tip_style", code_str("editorial"), motion = code_str("slide"), theme = "book_colours"),
    marker = code_call("tip_marker", code_str("icon")),
    pin = "TRUE",
    id = code_str("hero"),
    events = code_value(c("hint", "pin", "copy"))
  )
  paste(
    c(
      "# The tip on the word hovercraft, in the title above",
      code_call("tip_define", hovercraft = template),
      "# The book's own colours, light and dark",
      paste0("book_colours <- ", theme),
      code_call("htmltools::h1", class = code_str("hero"), word)
    ),
    collapse = "\n"
  )
}

# The figure as code: three figures in a sentence, each with an underlined tip of plain words.
start_figure_code <- function() {
  word <- function(text, tip, id) {
    code_call(
      "tip_on",
      code_word(text),
      code_str(tip),
      marker = code_str("underline"),
      id = code_str(id),
      events = code_str("hint")
    )
  }
  sentence <- code_sentence(
    "Week 46: the North region logged",
    word("4,812 visits", "Visits that ended in a booking or a reply, Monday to Sunday.", "visits"),
    "and stood",
    word("6% above target", "Target: **4,540** visits a week, from the Q4 plan.", "target"),
    "while the",
    word("median wait", "Time from a ticket opening to its first reply, in business hours.", "wait_term"),
    "rose to 4 h 12 min."
  )
  paste(c("# A tip of plain words on each figure of a sentence", sentence), collapse = "\n")
}

# The book's title: the word and its tip, run from the same code the margin shows, so the two can't differ.
start_hero <- function() {
  htmltools::tagList(
    run_code(start_hero_code())$ui,
    htmltools::p(class = "hero-sub", "book: every feature, live, on one page")
  )
}


start_steps <- function() {
  list(
    step(
      "Rest the pointer on *hovercraft* in the title: its tip slides in, in the serif look, with a badge and fields.",
      "it takes the page's look, or opens without its icon.",
      tip_target("hero"),
      on_hint("hero")
    ),
    step(
      "Pin that tip with its small pin button, then press *Copy*: the line of code is on your clipboard.",
      "Copy copies nothing, or the long form doesn't open once pinned.",
      tip_target("hero"),
      on_copy("hero")
    ),
    step(
      "Rest the pointer on *4,812 visits*: the hint opens after a short wait and closes when you leave.",
      "it flickers on the underline or stays open.",
      tip_target("visits"),
      on_hint("visits")
    ),
    step(
      "Tab to *median wait*: the hint opens on focus; Escape closes it and focus stays put.",
      "Escape moves focus or needs two presses.",
      tip_target("wait_term"),
      on_hint("wait_term", state = "hidden", reason = "escape")
    )
  )
}

plate_start_ui <- function() {
  plate(
    "start",
    lede = inline_code(paste(
      "Rest the pointer on a word, or reach it with Tab. Each section is one feature: the figure is live, and its",
      "margin holds the R code, what to test and the inputs your Shiny server would get. Open this book in your own R",
      "with `hovercraft_book()`; `hovercraft_book(\"pins\")` opens it at a section, `checklist = TRUE` opens the",
      "checklist of what to test, and `run = FALSE` returns the app without running it."
    )),
    caption = "Three figures in a sentence, each with a tip of plain words.",
    steps = start_steps(),
    code = code_text(paste0(start_hero_code(), "\n\n", start_figure_code())),
    log_empty = "Nothing yet. Rest on the title or an underlined figure.",
    before = start_hero()
  )
}

plate_start_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "start", start_steps())
  plate_code(
    input,
    output,
    session,
    "start",
    ui_code = shiny::reactive(paste0(start_hero_code(), "\n\n", start_figure_code())),
    figure_code = shiny::reactive(start_figure_code())
  )
  log_inputs(
    input,
    session,
    "start",
    c("hero_hint", "hero_pin", "hero_copy", paste0(c("visits", "target", "wait_term"), "_hint"))
  )
  ticks
}
