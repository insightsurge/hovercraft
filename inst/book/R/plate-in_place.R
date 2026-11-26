# A pinned tip opened as a card under its row: a placeholder section. It names its section and draws one tip until the book's rebuild fills it in
# (issue #99); contents.R already lists what it will show.

in_place_code <- function(settings) {
  code_call("tip_on", code_word("A pinned tip opened as a card under its row"), code_str("This section is being written."), id = code_str("in_place_tip"))
}

in_place_steps <- function() {
  list(
    step("Rest on the words: a tip opens.", "no tip opens.", tip_target("in_place_tip")),
    step("Move away: the tip closes.", "it stays open.", tip_target("in_place_tip")),
    step("Focus the words with Tab: the tip opens.", "no tip opens.", tip_target("in_place_tip"))
  )
}

plate_in_place_ui <- function() {
  plate(
    "in_place",
    lede = "This section is being written.",
    caption = "A placeholder.",
    steps = in_place_steps(),
    code = code_text(in_place_code(list()))
  )
}

plate_in_place_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "in_place", in_place_steps())
  plate_code(input, output, session, "in_place", shiny::reactive(in_place_code(list())))
  ticks
}
