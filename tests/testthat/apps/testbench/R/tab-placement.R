# Placement and collisions, HC-007 to HC-019 (HC-020 and HC-021 need a table: the tables tab): where a tip sits at the
# edges, in scroll boxes, on a phone, with very long content, and when pins crowd each other.

# Lines of made-up text for a long form.
place_lines <- function(n) paste(sprintf("Line %d of the long form.", seq_len(n)), collapse = "\n\n")

tab_placement_ui <- function() {
  medium <- place_lines(6L)
  htmltools::tagList(
    spot(
      "HC-007",
      note = "Hover, then press P (or the pin handle): the pin opens where the hint was.",
      pin_button("place-from-hint", "Pin from the hint", "Pin me from here", more = medium)
    ),
    spot(
      "HC-008",
      note = "Scroll so each button touches an edge of the window, then hover it.",
      htmltools::div(
        class = "d-flex justify-content-between",
        tip_button("place-left", "Left edge", "A hint at the left edge, long enough to need room"),
        tip_button("place-right", "Right edge", "A hint at the right edge, long enough to need room")
      )
    ),
    spot(
      "HC-009",
      htmltools::div(
        class = "d-flex justify-content-end",
        pin_button("place-corner", "Bottom right", "Pin for a long form", more = place_lines(25L))
      )
    ),
    spot(
      "HC-010",
      htmltools::div(
        class = "border",
        style = "height: 10rem; overflow: auto;",
        htmltools::div(style = "height: 4rem;"),
        pin_button("place-scroll", "Pin, then scroll this box", "I follow my item", more = medium),
        htmltools::div(style = "height: 25rem;")
      )
    ),
    spot(
      "HC-011",
      note = "Pin any tip here, then resize the window or rotate the phone.",
      pin_button("place-resize", "Pin and resize", "I stay with my item", more = medium)
    ),
    spot(
      "HC-012",
      note = "Set the window to 375 px wide, pin A then B: one bottom sheet at a time.",
      pin_button("place-sheet-a", "A", "Sheet A", more = medium),
      pin_button("place-sheet-b", "B", "Sheet B", more = medium)
    ),
    spot(
      "HC-013",
      note = "Zoom the browser to 200% and 400%, and enlarge text only; hover, pin and open the long form.",
      pin_button("place-zoom", "Zoom me", "A hint, a pin and a long form", more = place_lines(10L))
    ),
    spot(
      "HC-014",
      pin_button("place-word", "500 characters, one word", strrep("Unbroken", 63L)),
      pin_button("place-sixty", "60-line long form", "Pin for sixty lines", more = place_lines(60L))
    ),
    spot(
      "HC-015",
      note = htmltools::tagList(bench_link("Open this tab right to left", only = "placement", dir = "rtl"), "."),
      pin_button("place-rtl", "Mirror me", "The handle and More mirror", more = medium)
    ),
    spot(
      "HC-016",
      note = "Pin it: the plot arrives a second later and the tip grows.",
      hovercraft::tip_on(
        htmltools::tags$button(id = "place-grow", type = "button", class = "btn btn-outline-primary btn-sm", "Grows"),
        template = hovercraft::tip_template(
          hovercraft::tip_text("A plot is on its way."),
          hovercraft::tip_output("place_plot")
        ),
        id = "place_grow_tip",
        pin = TRUE
      )
    ),
    spot(
      "HC-017",
      note = htmltools::tagList(
        bench_link("Open with three pins", only = "placement", max_pins = 3),
        ", then pin neighbours."
      ),
      htmltools::div(
        class = "d-flex gap-1 flex-wrap",
        style = "max-width: 22rem;",
        lapply(1:12, function(i) {
          pin_button(sprintf("place-crowd-%02d", i), sprintf("%02d", i), sprintf("Crowded %02d", i), more = medium)
        })
      )
    ),
    spot(
      "HC-018",
      note = htmltools::tagList(
        bench_link("Open with three pins", only = "placement", max_pins = 3),
        ", then set the window to 599 and 601 px wide and pin the crowded buttons above."
      )
    ),
    spot("HC-019", note = "Pin a crowded button above: its tip covers neither it nor the next one.")
  )
}

tab_placement_server <- function(input, output, session) {
  output$place_plot <- shiny::renderPlot(
    {
      Sys.sleep(1) # the plot arrives after the tip has opened
      graphics::plot(seq_len(10), main = "Arrived")
    },
    width = 240,
    height = 160
  )
}
