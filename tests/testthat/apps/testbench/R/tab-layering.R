# Layering, HC-001 to HC-006: which box is on top when tips, pins, modals, dialogs and sticky headers meet.

tab_layering_ui <- function() {
  long <- "The long form: a few more lines that make the pinned tip tall enough to meet its neighbour's hint."
  htmltools::tagList(
    spot(
      "HC-001",
      note = "Pin A, then hover B: B's hint sits over A's pinned tip.",
      htmltools::div(
        class = "d-flex flex-column gap-1 align-items-start",
        pin_button("lay-b", "B (below in the page, first in the markup)", "B's hint", more = long),
        pin_button("lay-a", "A: pin me first", "A's tip", more = long)
      )
    ),
    spot(
      "HC-002",
      shiny::actionButton("lay_modal", "Open a Shiny modal with a pinnable tip")
    ),
    spot(
      "HC-003",
      htmltools::div(
        class = "d-flex gap-3 flex-wrap",
        htmltools::div(
          class = "border p-2 bench-clip",
          style = "overflow: hidden; height: 4rem; width: 12rem;",
          pin_button("lay-overflow", "In overflow: hidden", "Not cut by the box", more = long)
        ),
        htmltools::div(
          class = "border p-2",
          style = "transform: translateZ(0); width: 12rem;",
          pin_button("lay-transform", "In a transform", "Not cut by the transform", more = long)
        ),
        htmltools::div(
          class = "border p-2",
          style = "position: relative; z-index: 1; width: 12rem;",
          pin_button("lay-zlow", "In z-index 1", "Above the low layer", more = long)
        )
      )
    ),
    spot(
      "HC-004",
      htmltools::tags$button(
        id = "lay-dialog-open",
        type = "button",
        class = "btn btn-secondary btn-sm",
        onclick = "document.getElementById('lay-dialog').showModal()",
        "Open a native dialog"
      ),
      htmltools::tags$dialog(
        id = "lay-dialog",
        pin_button("lay-dialog-btn", "In the dialog", "A tip in the Top Layer"),
        htmltools::tags$button(
          type = "button",
          class = "btn btn-secondary btn-sm",
          onclick = "this.closest('dialog').close()",
          "Close"
        )
      ),
      htmltools::tags$button(
        id = "lay-offcanvas-open",
        type = "button",
        class = "btn btn-secondary btn-sm",
        `data-bs-toggle` = "offcanvas",
        `data-bs-target` = "#lay-offcanvas",
        "Open an offcanvas"
      ),
      htmltools::div(
        id = "lay-offcanvas",
        class = "offcanvas offcanvas-end",
        tabindex = "-1",
        htmltools::div(class = "offcanvas-body", pin_button("lay-offcanvas-btn", "In the offcanvas", "Above it"))
      )
    ),
    spot(
      "HC-005",
      htmltools::div(
        style = "height: 12rem; overflow: auto;",
        class = "border",
        htmltools::div(
          class = "bg-body-tertiary p-2",
          style = "position: sticky; top: 0; z-index: 3;",
          "A sticky header"
        ),
        htmltools::div(class = "p-2", tip_button("lay-sticky", "Just below the header", "Above the sticky header")),
        htmltools::div(style = "height: 30rem;", class = "p-2", "Scroll this box")
      ),
      DT::DTOutput("lay_dt")
    ),
    spot(
      "HC-006",
      note = htmltools::tagList(
        "Needs two pins: ",
        bench_link("open with max_pins = 2", only = "layering", max_pins = 2)
      ),
      htmltools::div(
        class = "d-flex gap-1",
        pin_button("lay-touch-1", "Pin one", "The first pin", more = long),
        pin_button("lay-touch-2", "Pin two", "The second pin", more = long)
      )
    )
  )
}

tab_layering_server <- function(input, output, session) {
  shiny::observeEvent(input$lay_modal, {
    shiny::showModal(shiny::modalDialog(
      title = "A Shiny modal",
      pin_button("lay-modal-btn", "In the modal", "Above the backdrop", more = "And its long form."),
      easyClose = TRUE
    ))
  })
  output$lay_dt <- DT::renderDT(
    hovercraft::tip_table(
      DT::datatable(
        bench_sites(40L),
        rownames = FALSE,
        extensions = "FixedHeader",
        options = list(fixedHeader = TRUE, pageLength = 40, dom = "t")
      ),
      rows = bench_rows(),
      headers = c(site = "The site's name", patients = "Patients enrolled"),
      id = "lay_dt"
    ),
    server = FALSE
  )
}
