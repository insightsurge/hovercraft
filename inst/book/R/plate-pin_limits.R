# Several pinned tips, and keyboard keys: a placeholder section. It names its section and draws one tip until the book's rebuild fills it in
# (issue #99); contents.R already lists what it will show.

pin_limits_code <- function(settings) {
  code_call("tip_on", code_word("Several pinned tips, and keyboard keys"), code_str("This section is being written."), id = code_str("pin_limits_tip"))
}

pin_limits_steps <- function() {
  list(
    step("Rest on the words: a tip opens.", "no tip opens.", tip_target("pin_limits_tip")),
    step("Move away: the tip closes.", "it stays open.", tip_target("pin_limits_tip")),
    step("Focus the words with Tab: the tip opens.", "no tip opens.", tip_target("pin_limits_tip"))
  )
}

plate_pin_limits_ui <- function() {
  plate(
    "pin_limits",
    lede = "This section is being written.",
    caption = "A placeholder.",
    steps = pin_limits_steps(),
    code = code_text(pin_limits_code(list()))
  )
}

plate_pin_limits_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "pin_limits", pin_limits_steps())
  plate_code(input, output, session, "pin_limits", shiny::reactive(pin_limits_code(list())))
  ticks
}
