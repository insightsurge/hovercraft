# Accessibility, HC-061 to HC-066 (HC-060 and HC-064 need a table: the tables tab): Escape, the More and Less states, a
# disabled control's reason, and the two passes a person does by hand.

tab_a11y_ui <- function() {
  htmltools::tagList(
    spot(
      "HC-061",
      note = "Hover, press Escape: the tip goes and stays gone until the pointer leaves and comes back.",
      tip_button("a11y-escape", "Escape me", "Escape closes me")
    ),
    spot(
      "HC-062",
      pin_button("a11y-more", "More and Less", "Pin, then More", more = "The long form a screen reader hears as new.")
    ),
    spot(
      "HC-063",
      shiny::actionButton("a11y_toggle", "Switch the control off and on"),
      hovercraft::tip_on(
        shiny::actionButton("a11y_disabled", "A control"),
        "What the control does",
        id = "a11y_disabled_tip",
        when_disabled = "the data is still loading"
      )
    ),
    spot(
      "HC-065",
      note = paste(
        "Five minutes with VoiceOver (Mac) and NVDA (Windows):",
        "every tagged item on this bench is read once, More and Less say their state."
      )
    ),
    spot(
      "HC-066",
      note = paste(
        "Ten minutes, keyboard only, whole bench: Tab, Shift+Tab, Enter, Space, Escape, P and M;",
        "nothing traps focus, every tip opens by focus."
      )
    )
  )
}

tab_a11y_server <- function(input, output, session) {
  shiny::observeEvent(input$a11y_toggle, {
    session$sendCustomMessage("bench-toggle-disabled", list(id = "a11y_disabled"))
  })
}
