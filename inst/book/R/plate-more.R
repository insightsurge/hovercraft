# Section 3.2, Short and long: one line on a glance, the whole method on request (`more =`, `more_on =`).

more_on_choices <- c("pinned" = "pin", "hovered" = "hover")
more_pin_choices <- c("its corner button or P" = "true", "a click as well" = "click")

more_code <- function(settings) {
  more_on <- choice(settings$more_on, more_on_choices, "pin")
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
      more = code_str(more),
      more_on = if (!is.null(on)) code_str(on),
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
    if (more_on != "pin") more_on,
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
      "# The long form opens when the tip is pinned (more_on = \"pin\", the default) or hovered",
      code_call("htmltools::div", wait, reopened, class = code_str("figures"))
    ),
    collapse = "\n"
  )
}

more_covers <- c(
  "tip_on(tag)",
  "tip_on(text)",
  "tip_on(more)",
  "tip_on(more_on)",
  "tip_on(pin)",
  "tip_on(id)",
  "tip_on(events)"
)

more_steps <- function() {
  list(
    step(
      "Rest on *Median wait*: one line, and a sign that there is more.",
      "the long text shows while it opens on pin.",
      tip_target("median_wait"),
      on_hint("median_wait", when = chose("more_on", "pin", "pin"))
    ),
    step(
      "Pin it: it grows into the long form in place.",
      "it jumps to another side, or opens a second tip.",
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
      "holds the long form; it opens when the tip is pinned, or as soon as it is hovered."
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
  log_inputs(input, session, "more", c("median_wait_hint", "median_wait_tip", "reopened_tip"))
  ticks
}
