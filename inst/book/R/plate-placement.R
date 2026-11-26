# Section "placement": where a tip opens. One word whose side you choose, or leave out, and four words each fixed at
# one side. Its number and title are in contents.R.

placement_sides <- c(
  "the page's choice" = "page",
  "where it fits" = "auto",
  "above" = "top",
  "below" = "bottom",
  "to the left" = "left",
  "to the right" = "right"
)

placement_code <- function(settings) {
  side <- choice(settings$side, placement_sides, "page")
  featured <- code_sentence(
    "The tip of",
    code_call(
      "tip_on",
      code_word("this word"),
      code_str("Opens **where you chose**, or where there is room."),
      placement = if (side != "page") code_str(side),
      marker = code_str("underline"),
      id = code_str("placement_main"),
      events = code_str("hint")
    ),
    "opens where you chose."
  )
  fixed <- code_strip(
    "Fixed at each side, to compare:",
    code_chr(c("top", "bottom", "left", "right")),
    "side",
    code_call(
      "tip_on",
      code_word(I("side")),
      "paste0(\"Asked to open on the \", side, \".\")",
      placement = "side",
      id = "paste0(\"placement_\", side)",
      events = code_str("hint")
    )
  )
  paste(
    c(
      "# Without placement the tip takes the page's choice, which is \"auto\": the side with room",
      "# A chosen side flips to the other when there is no room",
      featured,
      "",
      fixed
    ),
    collapse = "\n"
  )
}


placement_steps <- function() {
  list(
    step(
      "Rest on *this word* with *the page's choice*: the tip opens on the side where there is room.",
      "no tip, or a tip cut off by the stage.",
      tip_target("placement_main"),
      on_hint("placement_main", when = chose("placement_side", "page", "page"))
    ),
    step(
      "Choose *above*, then rest on the word: the tip opens above it.",
      "it opens on another side while there is room above.",
      "#placement_side",
      on_hint("placement_main", when = chose("placement_side", "top", "page"))
    ),
    step(
      "Choose *to the left*, then rest on the word: the tip opens on its left, arrow towards the word.",
      "it opens on the right, or the arrow points away.",
      "#placement_side",
      on_hint("placement_main", when = chose("placement_side", "left", "page"))
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
      "room; \"auto\" always takes the side with room. Left out, it is NULL: the tip takes the page's choice, which",
      "is \"auto\" unless a tagged ancestor sets a side. \"left\" and \"right\" mirror in right-to-left text, and on a",
      "phone the tip always chooses its own side."
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
