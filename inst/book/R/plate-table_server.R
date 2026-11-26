# Table tips (DT, reactable) from the server: a placeholder section. It names its section and draws one tip until the book's rebuild fills it in
# (issue #99); contents.R already lists what it will show.

table_server_code <- function(settings) {
  code_call("tip_on", code_word("Table tips (DT, reactable) from the server"), code_str("This section is being written."), id = code_str("table_server_tip"))
}

table_server_steps <- function() {
  list(
    step("Rest on the words: a tip opens.", "no tip opens.", tip_target("table_server_tip")),
    step("Move away: the tip closes.", "it stays open.", tip_target("table_server_tip")),
    step("Focus the words with Tab: the tip opens.", "no tip opens.", tip_target("table_server_tip"))
  )
}

plate_table_server_ui <- function() {
  plate(
    "table_server",
    lede = "This section is being written.",
    caption = "A placeholder.",
    steps = table_server_steps(),
    code = code_text(table_server_code(list()))
  )
}

plate_table_server_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "table_server", table_server_steps())
  plate_code(input, output, session, "table_server", shiny::reactive(table_server_code(list())))
  ticks
}
