# Performance and cleanup, HC-072 to HC-077: many items, many cells, many instances, and what redraws leave behind.
# The heavy parts draw on request, so the tab stays light until asked. The counted checks are on the leak tab
# (R/tab-leaks.R, test-leak-browser.R).

tab_scale_ui <- function() {
  htmltools::tagList(
    htmltools::tags$p(
      class = "small",
      "Open this tab on its own for timing: ",
      bench_link("scale tab alone", only = "scale"),
      ". Tip nodes on the page: ",
      htmltools::tags$strong(id = "scale-count", "0"),
      " (refreshed every second)."
    ),
    spot(
      "HC-072",
      note = "Sweep the pointer fast across all thirty, then stop: one tip at most, then none.",
      htmltools::div(
        class = "d-flex flex-wrap gap-1",
        lapply(1:30, function(i) {
          tip_button(sprintf("scale-thrash-%02d", i), sprintf("%02d", i), sprintf("Item %02d", i))
        })
      )
    ),
    spot(
      "HC-073",
      note = "Draw them, then time the first hover and look at memory after 50 hovers in the browser's tools.",
      shiny::actionButton("scale_cells", "Draw 1,000 tagged cells"),
      shiny::uiOutput("scale_cells_out")
    ),
    spot(
      "HC-074",
      shiny::actionButton("scale_many", "Draw five tables, three modules and 200 items"),
      shiny::uiOutput("scale_many_out")
    ),
    spot(
      "HC-075",
      note = "Redraw thirty times and watch the tip-node count: it must not grow.",
      shiny::actionButton("scale_redraw", "Redraw thirty times"),
      shiny::uiOutput("scale_redrawn")
    ),
    spot(
      "HC-076",
      note = "Pin one, pin another (the limit closes the first), redraw it, remove it: the count comes back down.",
      pin_button("scale-pin-a", "Pin A", "Pin A"),
      pin_button("scale-pin-b", "Pin B", "Pin B")
    ),
    spot("HC-077", note = "Every spot ends with no console error: the log strip shows errors in red.")
  )
}

tab_scale_server <- function(input, output, session) {
  output$scale_cells_out <- shiny::renderUI({
    shiny::req(input$scale_cells)
    cells <- lapply(seq_len(1000L), function(i) {
      hovercraft::tip_on(htmltools::tags$span(class = "scale-cell", sprintf("%04d", i)), sprintf("Cell %04d", i))
    })
    htmltools::div(class = "scale-cells", cells)
  })

  output$scale_many_out <- shiny::renderUI({
    shiny::req(input$scale_many)
    sites <- bench_sites(10L)
    tables <- lapply(1:5, function(i) {
      hovercraft::tip_table(
        DT::datatable(sites, rownames = FALSE, options = list(dom = "t")),
        rows = bench_rows(),
        id = paste0("scale_table_", i)
      )
    })
    modules <- lapply(1:3, function(i) practice_card_ui(paste0("scale-mod-", i), paste("Scale", i)))
    items <- lapply(1:200, function(i) {
      tip_button(sprintf("scale-item-%03d", i), sprintf("%03d", i), sprintf("Item %03d", i), pin = TRUE)
    })
    htmltools::tagList(tables, modules, htmltools::div(class = "d-flex flex-wrap gap-1", items))
  })

  redraws <- shiny::reactiveVal(0L)
  shiny::observeEvent(input$scale_redraw, redraws(0L))
  shiny::observe({
    shiny::req(input$scale_redraw)
    if (shiny::isolate(redraws()) < 30L) {
      shiny::invalidateLater(50)
      redraws(shiny::isolate(redraws()) + 1L)
    }
  })
  output$scale_redrawn <- shiny::renderUI({
    pin_button("scale-redrawn", paste("Redraw", redraws()), "Redrawn", more = "A long form, redrawn.")
  })
}
