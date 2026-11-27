# Section "more_button": the control that opens a tip's long form: its words (`label =`), look (`as =`), place
# (`where =`) and whether it pins the tip too (`pin =`), in `tip_more()`.

more_button_as <- c("a button" = "button", "a text link" = "link")
more_button_where <- c("its usual place" = "usual", "the end" = "end", "the start" = "start")
more_button_label <- c("the built-in words" = "built_in", "your own words" = "own")
more_button_pin <- c("only opens the long form" = "open", "pins the tip too" = "pin")

more_button_code <- function(settings) {
  as <- choice(settings$as, more_button_as, "button")
  where <- choice(settings$where, more_button_where, "usual")
  label <- choice(settings$label, more_button_label, "built_in")
  pin <- choice(settings$pin, more_button_pin, "open")
  more <- code_call(
    "tip_more",
    code_str(paste(
      "**How it is counted.** Every hour, from the helpdesk export. Spam, tickets closed by a rule and tickets on hold",
      "are left out.\n\n**Query.** `subset(tickets, queue == 'Billing')`"
    )),
    label = if (label == "own") code_call("c", more = code_str("Show all"), less = code_str("Show less")),
    pin = if (pin == "pin") "TRUE",
    as = code_str(as),
    where = if (where != "usual") code_str(where)
  )
  figure <- code_call(
    "tip_on",
    code_call(
      "htmltools::div",
      code_call("htmltools::span", code_str("Open tickets"), class = code_str("label")),
      code_call("htmltools::span", code_str("42"), class = code_str("value")),
      class = code_str("figure-num"),
      tabindex = code_str("0")
    ),
    code_str("Tickets that wait for a first reply."),
    more = more,
    pin = "TRUE",
    id = code_str("more_button_open"),
    events = code_chr(c("hint", "pin"))
  )
  paste(
    c(
      "# The control that opens the long form: its words, its look and its place",
      code_call("htmltools::div", figure, class = code_str("figures"))
    ),
    collapse = "\n"
  )
}

more_button_steps <- function() {
  list(
    step(
      "Rest on *Open tickets*: the control sits at the bottom of the tip, as the choices say.",
      "no control, or it covers the words.",
      tip_target("more_button_open"),
      on_hint("more_button_open")
    ),
    step(
      "Press it, or press M with the focus on the figure: the tip grows in place, and the control says Less.",
      "the tip jumps to another side, or closes.",
      tip_target("more_button_open")
    ),
    step(
      paste(
        "Choose *pins the tip too*, then press More: the tip stays open, and `input$more_button_open_pin` arrives with",
        "state `pinned`. Less then closes the long form only."
      ),
      "the tip closes with the pointer, or Less closes the pin.",
      tip_target("more_button_open"),
      on_pin("more_button_open", when = chose("more_button_pin", "pin", "open"))
    ),
    step(
      "Choose *a text link* at *the start*: an italic, underlined link starts the bottom line. Tab reaches it.",
      "the link is a button shape, or Enter does not open it.",
      "#more_button_as"
    ),
    step(
      "Choose *your own words*, then open the tip: the control says Show all, and Show less when it is open.",
      "the control still says More.",
      "#more_button_label"
    )
  )
}

plate_more_button_ui <- function() {
  plate(
    "more_button",
    lede = htmltools::tagList(
      "The More button is a control inside the tip. With",
      htmltools::tags$code("tip_more()"),
      "you choose its words (",
      htmltools::tags$code("label"),
      "), its look and place (",
      htmltools::tags$code("as"),
      "and",
      htmltools::tags$code("where"),
      ") and whether it pins the tip as well (",
      htmltools::tags$code("pin"),
      "). It needs no pin and no id of its own; the figure has them so that pinning can be tried."
    ),
    caption = "One figure from a support desk. Its long form is the same whatever the control looks like.",
    steps = more_button_steps(),
    code = code_text(more_button_code(list())),
    log_empty = "Nothing yet. Rest on the figure, then pin it.",
    controls = htmltools::tagList(
      "Show the control as",
      choice_set("more_button_as", "Look", more_button_as),
      "at",
      choice_set("more_button_where", "Place", more_button_where),
      "with",
      choice_set("more_button_label", "Words", more_button_label),
      "; it",
      choice_set("more_button_pin", "Pinning", more_button_pin)
    )
  )
}

plate_more_button_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "more_button", more_button_steps())
  code <- shiny::reactive(
    more_button_code(list(
      as = input$more_button_as,
      where = input$more_button_where,
      label = input$more_button_label,
      pin = input$more_button_pin
    ))
  )
  plate_code(input, output, session, "more_button", code)
  log_inputs(
    input,
    session,
    "more_button",
    paste0("more_button_open", c("_hint", "_pin", "_pinned"))
  )
  ticks
}
