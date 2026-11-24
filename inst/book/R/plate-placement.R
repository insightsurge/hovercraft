# Section 1.2, Where it opens: the five values of tip_on(placement =), one featured tag and four fixed at a side.

placement_sides <- c(
  "where it fits" = "auto",
  "above" = "top",
  "below" = "bottom",
  "to the left" = "left",
  "to the right" = "right"
)

placement_code <- function(settings) {
  side <- choice(settings$side, placement_sides, "auto")
  featured <- code_call(
    "htmltools::p",
    code_str("The tip of"),
    code_call(
      "tip_on",
      code_call("htmltools::tags$span", code_str("this word"), tabindex = code_str("0")),
      code_str("Opens **where you chose**. With *auto* it takes the side with room."),
      placement = code_str(side),
      marker = code_str("underline"),
      id = code_str("placement_main"),
      events = code_str("hint")
    ),
    code_str("opens where you chose."),
    class = code_str("specimen-text")
  )
  fixed <- paste0(
    "htmltools::p(\"Fixed at each side, to compare:\", class = \"strip-title\")\n",
    "htmltools::tags$ul(\n",
    "  class = \"strip\",\n",
    "  lapply(c(\"top\", \"bottom\", \"left\", \"right\"), function(side) {\n",
    "    htmltools::tags$li(tip_on(\n",
    "      htmltools::tags$span(side, tabindex = \"0\"),\n",
    "      paste0(\"Asked to open on the \", side, \".\"),\n",
    "      placement = side,\n",
    "      id = paste0(\"placement_\", side),\n",
    "      events = \"hint\"\n",
    "    ))\n",
    "  })\n",
    ")"
  )
  paste(
    c("# placement = \"auto\" is the default; a chosen side flips when there is no room", featured, "", fixed),
    collapse = "\n"
  )
}

placement_covers <- c("tip_on(tag)", "tip_on(text)", "tip_on(placement)")

placement_steps <- function() {
  side <- function(value) function(input) identical(input$placement_side %||% "auto", value)
  list(
    step(
      "Rest on *this word* with *where it fits*: the tip opens on the side the engine finds room.",
      "no tip, or a tip cut off by the stage.",
      tip_target("placement_main"),
      on_hint("placement_main", when = side("auto"))
    ),
    step(
      "Choose *above*, then rest on the word: the tip opens above it.",
      "it opens on another side while there is room above.",
      "#placement_side",
      on_hint("placement_main", when = side("top"))
    ),
    step(
      "Choose *to the left*, then rest on the word: the tip opens on its left, arrow towards the word.",
      "it opens on the right, or the arrow points away.",
      "#placement_side",
      on_hint("placement_main", when = side("left"))
    ),
    step(
      "Rest on *right* in the row below: a tag fixed at one side opens on that side, whatever you chose above.",
      "the fixed tags follow the choice.",
      "#placement .strip",
      on_hint("placement_right")
    ),
    step(
      paste(
        "Scroll until the word sits at the top edge of the window, choose *above* and rest on it: with no room",
        "above, the tip flips below."
      ),
      "the tip is cut off at the window's edge.",
      tip_target("placement_main")
    )
  )
}

plate_placement_ui <- function() {
  plate(
    "placement",
    lede = htmltools::tagList(
      htmltools::tags$code("placement ="),
      "takes \"auto\", \"top\", \"bottom\", \"left\" or \"right\". A chosen side flips to the other when there is no",
      "room; \"auto\" always takes the side with room. Leave it out to take the page's choice."
    ),
    caption = "One word whose side you choose, and four words each fixed at one side.",
    steps = placement_steps(),
    code = code_text(placement_code(list())),
    log_empty = "Nothing yet. Rest on a word.",
    controls = htmltools::tagList(
      "Open the tip",
      choice_set("placement_side", "Placement", placement_sides)
    )
  )
}

plate_placement_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "placement", placement_steps())
  code <- shiny::reactive(placement_code(list(side = input$placement_side)))
  plate_code(input, output, session, "placement", code)
  log_inputs(
    input,
    session,
    "placement",
    paste0(paste0("placement_", c("main", "top", "bottom", "left", "right")), "_hint")
  )
  ticks
}
