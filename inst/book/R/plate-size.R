# Section "size": how far a tip sits from its tag, how wide a hint and a pin grow, and a class of your own
# (`tip_style(gap =, max_width =, pin_width =, class =)`, given to one tag).

size_gaps <- c("4 px" = "4", "10 px" = "10", "18 px" = "18")
size_max_widths <- c("240 px" = "240", "320 px" = "320", "440 px" = "440")
size_pin_widths <- c("320 px" = "320", "480 px" = "480")
size_classes <- c("card-tip" = "card-tip", "ledger-tip" = "ledger-tip")

size_hint <- paste(
  "Median time from a ticket opening to its first reply, over the last 7 days. The clock runs in business hours",
  "only, so a ticket opened on Friday evening starts counting on Monday morning."
)
size_code <- function(settings) {
  gap <- choice(settings$gap, size_gaps, "10")
  max_width <- choice(settings$max_width, size_max_widths, "320")
  pin_width <- choice(settings$pin_width, size_pin_widths, "480")
  class <- choice(settings$class, size_classes, "card-tip")
  code_call(
    "tip_on",
    code_word("Median wait", class = "target"),
    code_str(size_hint),
    more = code_str(book_wait_more),
    style = code_call(
      "tip_style",
      gap = gap,
      max_width = max_width,
      pin_width = pin_width,
      class = code_str(class)
    ),
    pin = "TRUE",
    id = code_str("size_wait"),
    events = code_chr(c("hint", "pin"))
  )
}

size_steps <- function() {
  list(
    step(
      paste(
        "Choose a gap of *18 px*, then rest on *Median wait*: the hint opens that far from the tag, with room for",
        "the arrow."
      ),
      "the hint touches the tag, or the gap changes when the hint is long.",
      tip_target("size_wait"),
      on_hint("size_wait", when = chose("size_gap", "18", "10"))
    ),
    step(
      paste(
        "Choose *440 px* for the widest hint, then rest on the tag: the sentence runs on a wider line and takes",
        "fewer rows."
      ),
      "the hint grows past the widest value, or stays narrow.",
      "#size_max_width",
      on_hint("size_wait", when = chose("size_max_width", "440", "320"))
    ),
    step(
      paste(
        "Pin it with its corner button or P, then press *More*: the long form opens in the same tip, which grows to",
        "the widest pin, wider than a hint."
      ),
      "the pinned tip is as narrow as the hint, or runs off the page.",
      tip_target("size_wait"),
      on_pin("size_wait")
    ),
    step(
      "Open your browser's inspector on the open tip: its box carries the class you chose, and no style comes with it.",
      "the class is missing, or the book gives it a look of its own.",
      tip_target("size_wait")
    )
  )
}

plate_size_ui <- function() {
  plate(
    "size",
    lede = htmltools::tagList(
      htmltools::tags$code("tip_style()"),
      "sets how far a tip sits from its tag and how wide a hint and a pinned tip grow, on one tag or on the whole",
      "page.",
      "All three are whole numbers of pixels: a gap from 0 to 24, a widest hint from 160 to 640 and a widest pin from",
      "240 to 720. A value set on a tag beats the page's, one value at a time."
    ),
    caption = paste(
      "One tag with a hint and a long form. The class is a hook for your own CSS: the book ships no style for",
      "card-tip or ledger-tip, so choosing one changes nothing you can see here. A class on a tag replaces one the",
      "page sets, and is not added to it."
    ),
    steps = size_steps(),
    code = code_text(size_code(list())),
    log_empty = "Nothing yet. Rest on the tag, then pin it.",
    controls = htmltools::tagList(
      "Put the tip",
      choice_set("size_gap", "Gap", size_gaps, selected = "10"),
      "from its tag; let a hint grow to",
      choice_set("size_max_width", "Widest hint", size_max_widths, selected = "320"),
      "and a pinned tip to",
      choice_set("size_pin_width", "Widest pin", size_pin_widths, selected = "480"),
      "wide, and give its box the class",
      choice_set("size_class", "Class", size_classes)
    )
  )
}

plate_size_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "size", size_steps())
  code <- shiny::reactive(
    size_code(
      list(
        gap = input$size_gap,
        max_width = input$size_max_width,
        pin_width = input$size_pin_width,
        class = input$size_class
      )
    )
  )
  plate_code(input, output, session, "size", code)
  log_inputs(input, session, "size", c("size_wait_hint", "size_wait_pin", "size_wait_pinned"))
  ticks
}

