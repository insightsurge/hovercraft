# Section 3.2, Short and long: one line on a glance, the whole method on request (`more =`, `tip_more()`).

more_on_choices <- c("behind a More button" = "button", "pinned" = "pin", "hovered" = "hover")
more_pin_choices <- c("its corner button or P" = "true", "a click as well" = "click")

more_code <- function(settings) {
  more_on <- choice(settings$more_on, more_on_choices, "button")
  pin <- if (identical(choice(settings$pin, more_pin_choices, "true"), "click")) code_str("click") else "TRUE"
  figure <- function(label, value, unit, id, text, more, on, pin) {
    code_call(
      "tip_on",
      code_call(
        "htmltools::div",
        code_call("htmltools::span", code_str(label), class = code_str("label")),
        code_call(
          "htmltools::span",
          code_str(value),
          code_call("htmltools::tags$small", code_str(unit)),
          class = code_str("value")
        ),
        class = code_str("figure-num"),
        tabindex = code_str("0")
      ),
      code_str(text),
      more = if (is.null(on)) code_str(more) else code_call("tip_more", code_str(more), on = code_str(on)),
      pin = pin,
      id = code_str(id),
      events = code_chr(c("hint", "pin"))
    )
  }
  wait <- figure(
    "Median wait",
    "4 h 12",
    "min",
    "median_wait",
    "Median time from a ticket opening to its first reply, last 7 days.",
    book_wait_more,
    if (more_on != "button") more_on,
    pin
  )
  reopened <- figure(
    "Reopened",
    "3.4",
    "%",
    "reopened",
    "Share of solved tickets opened again within 14 days.",
    paste(
      "Up from 2.9% in week 45. Most reopenings (61%) are the same booking question asked again after an automatic",
      "reply."
    ),
    "hover",
    "TRUE"
  )
  paste(
    c(
      "# The long form waits behind a More button (the default), or opens when the tip is pinned or hovered",
      code_call("htmltools::div", wait, reopened, class = code_str("figures"))
    ),
    collapse = "\n"
  )
}

more_covers <- c(
  "tip_on(tag)",
  "tip_on(text)",
  "tip_on(more)",
  "tip_more(content)",
  "tip_more(on)",
  "tip_on(pin)",
  "tip_on(id)",
  "tip_on(events)"
)

more_steps <- function() {
  on <- function(value) function(input) identical(input$more_on %||% "button", value)
  list(
    step(
      "Rest on *Median wait*: one line, and a More button at the bottom of the tip.",
      "the long text shows before More is pressed.",
      tip_target("median_wait"),
      on_hint("median_wait", when = on("button"))
    ),
    step(
      "Press More, or M with the focus on the figure: the tip grows into the long form in place, and Less shrinks it.",
      "it jumps to another side, closes under the pointer, or opens a second tip.",
      tip_target("median_wait")
    ),
    step(
      "Pin it: the pinned tip has the buttons too.",
      "a click on the figure pins it, or the pin opens a second tip.",
      tip_target("median_wait"),
      on_pin("median_wait")
    ),
    step(
      paste(
        "Choose *hovered*, then rest on it again: the long form opens on hover and scrolls instead of running off",
        "the page."
      ),
      "a long tip runs off the screen.",
      "#more_on",
      on_hint("median_wait", when = chose("more_on", "hover", "pin"))
    ),
    step(
      "Escape closes the pinned tip, and focus goes back to the figure.",
      "focus is lost to the page.",
      tip_target("median_wait"),
      on_pin("median_wait", state = "closed", reason = "escape")
    )
  )
}

plate_more_ui <- function() {
  plate(
    "more",
    lede = htmltools::tagList(
      "A tip can say one line on a glance and the whole method on request.",
      htmltools::tags$code("more ="),
      "holds the long form; a More button opens it, or it opens when the tip is pinned, or as soon as it is hovered",
      "(",
      htmltools::tags$code("tip_more()"),
      ")."
    ),
    caption = "Two figures from a support desk. Reopened always grows on hover, for comparison.",
    steps = more_steps(),
    code = code_text(more_code(list())),
    log_empty = "Nothing yet. Pin a figure.",
    controls = htmltools::tagList(
      "For Median wait, show the long form when the tip is",
      choice_set("more_on", "Long form opens on", more_on_choices),
      "and pin it with",
      choice_set("more_pin", "Pin with", more_pin_choices)
    )
  )
}

plate_more_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "more", more_steps())
  code <- shiny::reactive(more_code(list(more_on = input$more_on, pin = input$more_pin)))
  plate_code(input, output, session, "more", code)
  log_inputs(input, session, "more", c("median_wait_hint", "median_wait_pin", "reopened_pin"))
  ticks
}
