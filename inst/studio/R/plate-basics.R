# Plate 2, Basics: tip text with markdown, a placement, a look for one tag, and the marker that shows a tip is there.

basics_markers <- c("an underline" = "underline", "an icon" = "icon", "a corner" = "corner", "nothing" = "none")
basics_placements <- c("where it fits" = "auto", "above" = "top", "below" = "bottom", "to the right" = "right")
basics_looks <- c(
  "the page's look" = "page",
  stats::setNames(c("dense", "editorial", "lively"), c("dense", "editorial", "lively"))
)

basics_code <- function(settings) {
  marker <- choice(settings$marker, basics_markers, "underline")
  placement <- choice(settings$placement, basics_placements, "auto")
  look <- choice(settings$look, basics_looks, "page")
  code_call(
    "htmltools::p",
    code_str("This week the desk answered"),
    code_call(
      "tip_on",
      code_call("htmltools::tags$span", code_str("1,906 tickets"), tabindex = code_str("0")),
      code_str("Tickets with a **first reply** from a person, Monday to Sunday. *Bot* replies are not counted."),
      placement = if (placement != "auto") code_str(placement),
      style = if (look != "page") code_str(look),
      marker = if (marker != "none") code_str(marker),
      id = code_str("basics_word"),
      events = code_str("hint")
    ),
    code_str("across three regions."),
    class = code_str("specimen-text")
  )
}

basics_steps <- function() {
  is <- function(name, value) function(input) identical(input[[name]], value)
  list(
    step(
      "Rest on *1,906 tickets*: the tip opens, with the words first reply in bold and Bot in italic.",
      "stars shown as typed.",
      tip_target("basics_word"),
      on_hint("basics_word")
    ),
    step(
      "Choose each marker in turn: a cue shows that the words hold a tip, without moving them.",
      "the sentence shifts when the marker changes.",
      "#basics_marker"
    ),
    step(
      "Choose *above*, then rest on the words: the tip opens above, and flips only when there is no room.",
      "it opens on another side with room to spare.",
      "#basics_placement",
      on_hint("basics_word", when = is("basics_placement", "top"))
    ),
    step(
      "Choose *editorial*: this one tip takes that look, while the page keeps its own.",
      "every tip on the page changes.",
      "#basics_look",
      on_hint("basics_word", when = is("basics_look", "editorial"))
    ),
    step(
      "Tab to the words, then press Escape: the tip opens on focus and Escape closes it.",
      "no tip for the keyboard.",
      tip_target("basics_word"),
      on_hint("basics_word", state = "hidden", reason = "escape")
    )
  )
}

plate_basics_ui <- function() {
  plate(
    "basics",
    2,
    "Basics",
    lede = paste(
      "Tip text with markdown, a placement, a look for one tag, and the marker that tells a reader a tip is there."
    ),
    caption = "One figure in a sentence. Its tip, its side, its look and its marker come from the choices above.",
    steps = basics_steps(),
    code = code_text(basics_code(list())),
    log_empty = "Nothing yet. Rest on the underlined words.",
    controls = htmltools::tagList(
      "Mark the words with",
      choice_set("basics_marker", "Marker", basics_markers),
      ", open the tip",
      choice_set("basics_placement", "Placement", basics_placements),
      "in",
      choice_set("basics_look", "Look", basics_looks)
    )
  )
}

plate_basics_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "basics", basics_steps())
  code <- shiny::reactive(basics_code(list(
    marker = input$basics_marker,
    placement = input$basics_placement,
    look = input$basics_look
  )))
  plate_code(input, output, session, "basics", code)
  log_inputs(input, session, "basics", "basics_word_hint")
  ticks
}
