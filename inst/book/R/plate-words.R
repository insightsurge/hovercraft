# The tip's built-in words (Copy, Close, More): a placeholder section. It names its section and draws one tip until the book's rebuild fills it in
# (issue #99); contents.R already lists what it will show.

words_code <- function(settings) {
  code_call("tip_on", code_word("The tip's built-in words (Copy, Close, More)"), code_str("This section is being written."), id = code_str("words_tip"))
}

words_steps <- function() {
  list(
    step("Rest on the words: a tip opens.", "no tip opens.", tip_target("words_tip")),
    step("Move away: the tip closes.", "it stays open.", tip_target("words_tip")),
    step("Focus the words with Tab: the tip opens.", "no tip opens.", tip_target("words_tip"))
  )
}

plate_words_ui <- function() {
  plate(
    "words",
    lede = "This section is being written.",
    caption = "A placeholder.",
    steps = words_steps(),
    code = code_text(words_code(list()))
  )
}

plate_words_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "words", words_steps())
  plate_code(input, output, session, "words", shiny::reactive(words_code(list())))
  ticks
}
