# The More button: its words, look and place: a placeholder section. It names its section and draws one tip until the book's rebuild fills it in
# (issue #99); contents.R already lists what it will show.

more_button_code <- function(settings) {
  code_call("tip_on", code_word("The More button: its words, look and place"), code_str("This section is being written."), id = code_str("more_button_tip"))
}

more_button_steps <- function() {
  list(
    step("Rest on the words: a tip opens.", "no tip opens.", tip_target("more_button_tip")),
    step("Move away: the tip closes.", "it stays open.", tip_target("more_button_tip")),
    step("Focus the words with Tab: the tip opens.", "no tip opens.", tip_target("more_button_tip"))
  )
}

plate_more_button_ui <- function() {
  plate(
    "more_button",
    lede = "This section is being written.",
    caption = "A placeholder.",
    steps = more_button_steps(),
    code = code_text(more_button_code(list()))
  )
}

plate_more_button_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "more_button", more_button_steps())
  plate_code(input, output, session, "more_button", shiny::reactive(more_button_code(list())))
  ticks
}
