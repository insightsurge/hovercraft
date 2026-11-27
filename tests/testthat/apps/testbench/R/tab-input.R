# Keyboard, pointer and touch, HC-022 to HC-041 (the table ones, HC-033 and HC-039, are on the tables tab): the P and M
# keys, Escape, focus, the ways the long form opens, odd pointer moves and touch.

tab_input_ui <- function() {
  long <- "The long form, opened by More or the M key."
  htmltools::tagList(
    htmltools::tags$p(
      class = "small",
      "Keys: hover or focus an item, then P pins and M opens the long form. Turn them off: ",
      bench_link("P off", only = "input", keys = "no_pin"),
      " | ",
      bench_link("M off", only = "input", keys = "no_more"),
      " | ",
      bench_link("both off", only = "input", keys = "off"),
      " | ",
      bench_link("both on", only = "input")
    ),
    spot(
      "HC-022",
      htmltools::div(
        class = "d-flex gap-1",
        lapply(1:4, function(i) {
          pin_button(sprintf("inp-key-%d", i), sprintf("Item %d", i), sprintf("Tip %d", i), more = long)
        })
      )
    ),
    spot("HC-023", note = "Hold P down on an item above: it pins once."),
    spot(
      "HC-024",
      hovercraft::tip_on(shiny::textInput("inp_field", "Type P and M here", ""), "A field's tip", id = "inp_field_tip"),
      hovercraft::tip_on(
        shiny::textAreaInput("inp_area", "And here", ""),
        "A text area's tip",
        id = "inp_area_tip",
        pin = TRUE
      )
    ),
    spot("HC-025", note = "Use the links at the top of this tab; the page says which keys are off."),
    spot(
      "HC-026",
      note = "Tab through: the tip never takes focus; a pinned tip's button comes right after its item.",
      pin_button("inp-focus", "Focus me", "Pinned, with a button", more = long),
      htmltools::tags$button(type = "button", class = "btn btn-outline-secondary btn-sm", "Next stop")
    ),
    spot(
      "HC-027",
      note = htmltools::tagList(
        bench_link("Open with three pins", only = "input", max_pins = 3),
        ": pin two, hover a third, then press Escape three times."
      )
    ),
    spot(
      "HC-028",
      shiny::actionButton("inp_modal", "Open a modal with a field"),
      htmltools::div(
        class = "dropdown d-inline-block",
        htmltools::tags$button(
          id = "inp-menu",
          type = "button",
          class = "btn btn-secondary btn-sm dropdown-toggle",
          `data-bs-toggle` = "dropdown",
          "Menu"
        ),
        htmltools::tags$ul(
          class = "dropdown-menu",
          htmltools::tags$li(htmltools::tags$a(class = "dropdown-item", href = "#", "One")),
          htmltools::tags$li(htmltools::tags$a(class = "dropdown-item", href = "#", "Two"))
        )
      ),
      pin_button("inp-near-menu", "Beside the menu", "Open me over the menu")
    ),
    spot("HC-029", note = "Checked in R, not in the page:", shiny::verbatimTextOutput("inp_refused")),
    spot(
      "HC-030",
      hovercraft::tip_on(
        htmltools::tags$span(id = "inp-click", class = "border rounded px-2", tabindex = "0", "Click to expand"),
        "Click me for the long form",
        id = "inp_click_tip",
        more = hovercraft::tip_more(long, on = "click")
      )
    ),
    spot("HC-031", pin_button("inp-more-pin", "More pins too", "More pins this tip", more = long, pin = TRUE)),
    spot(
      "HC-032",
      note = htmltools::tagList(bench_link("Open this tab right to left", only = "input", dir = "rtl"), "."),
      pin_button("inp-link-start", "Link at the start", "More as a link", more = long, as = "link", where = "start"),
      pin_button("inp-link-end", "Link at the end", "More as a link", more = long, as = "link", where = "end")
    ),
    spot(
      "HC-034",
      pin_button("inp-travel", "Travel onto the tip", "Move onto me and my button", more = long)
    ),
    spot(
      "HC-035",
      pin_button("inp-own-click", "Has its own click", "Clicking runs the item's click"),
      htmltools::tags$span(id = "inp-own-count", "Clicks: 0"),
      htmltools::tags$script(htmltools::HTML(
        "document.getElementById('inp-own-click').addEventListener('click', () => {
          const out = document.getElementById('inp-own-count');
          out.textContent = 'Clicks: ' + (Number(out.textContent.split(': ')[1]) + 1);
        });"
      ))
    ),
    spot("HC-036", note = "Right-click, double-click and drag off the items above."),
    spot("HC-037", note = "Open a hint, then switch to another window or browser tab and back."),
    spot(
      "HC-038",
      note = "Turn on touch in the browser's device mode: a tap shows the hint, the handle pins, a tap away closes.",
      pin_button("inp-touch", "Tap me", "A tap shows me", more = long)
    ),
    spot(
      "HC-040",
      hovercraft::tip_on(
        htmltools::tags$button(
          id = "inp-targets",
          type = "button",
          class = "btn btn-outline-primary btn-sm",
          "Every control"
        ),
        template = hovercraft::tip_template(
          hovercraft::tip_text("Pin me: handle, More, Copy and close."),
          hovercraft::tip_code("code", copy = TRUE)
        ),
        record = list(code = "tip_on(tag, \"Hello\")"),
        id = "inp_targets_tip",
        pin = TRUE,
        more = hovercraft::tip_more(long)
      )
    ),
    spot("HC-041", note = "With touch on: scroll and pinch with a hint open, and tap the click-to-expand item above.")
  )
}

tab_input_server <- function(input, output, session) {
  shiny::observeEvent(input$inp_modal, {
    shiny::showModal(shiny::modalDialog(
      title = "A field in a modal",
      hovercraft::tip_on(
        shiny::textInput("inp_modal_field", "Press Escape here", ""),
        "The field's tip",
        id = "inp_modal_tip"
      ),
      easyClose = TRUE
    ))
  })
  output$inp_refused <- shiny::renderText({
    result <- tryCatch(
      hovercraft::tip_on(
        shiny::actionButton("inp_refused_btn", "A control"),
        "A control",
        more = hovercraft::tip_more("Long", on = "click")
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) conditionMessage(result) else "Not refused: this is a finding."
  })
}
