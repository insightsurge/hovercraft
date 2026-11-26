# Tables, HC-042 to HC-049, with the scenarios that need a table from other categories (HC-020, HC-021, HC-033,
# HC-039, HC-060, HC-064): DT in client and server mode, reactable, in-place panels, column changes, a hidden tab, a
# modal and two tables side by side. test-practice-table-browser.R, test-table-*-browser.R and test-leak-browser.R
# hold the automated checks; this tab is for trying them by hand.

tbl_long <- "The long form of {{ site }}: {{ patients }} patients in {{ country }}."

# A DT table with pinnable rows.
tbl_dt <- function(data, id, ..., options = list(pageLength = 5), filter = "none", extensions = list(), rows = NULL) {
  hovercraft::tip_table(
    DT::datatable(data, rownames = FALSE, filter = filter, options = options, extensions = extensions, ...),
    rows = rows %||% bench_rows(more = hovercraft::tip_more(tbl_long, on = "pin")),
    cells = list(status = "Status: {{ status }}"),
    id = id
  )
}

# A small module with a table of its own, for separate pin counts.
tbl_module_ui <- function(id) {
  DT::DTOutput(shiny::NS(id, "table"))
}

tbl_module_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    output$table <- DT::renderDT(tbl_dt(bench_sites(12L), "tbl_module_table"), server = FALSE)
  })
}

tab_tables_ui <- function() {
  htmltools::tagList(
    htmltools::tags$h3(class = "h6", "Client-side DT: sort, filter, search and page with a pin open"),
    DT::DTOutput("tbl_client"),
    spot("HC-042"),
    spot("HC-043"),
    spot("HC-020", note = "Pin a row, move the pointer away and back onto it."),
    spot("HC-033", note = "Move from a pinnable row onto the status cell: the handle goes."),
    spot("HC-039", note = "With touch on: tap a row, then tap its handle."),
    spot("HC-060", note = "With a screen reader or the accessibility tree: sort and page, the description follows."),
    htmltools::tags$h3(class = "h6", "Server-side DT and replaceData()"),
    shiny::actionButton("tbl_replace", "replaceData()"),
    shiny::actionButton("tbl_replace_ten", "replaceData() ten times"),
    DT::DTOutput("tbl_server"),
    spot("HC-044"),
    htmltools::tags$h3(class = "h6", "In-place panels, and a floating pin beside them"),
    pin_button("tbl-float", "A floating pin", "Floating, next to the panel table"),
    DT::DTOutput("tbl_panel"),
    spot("HC-021"),
    spot("HC-045"),
    spot("HC-064", note = "Open a panel with P, tab into it, close it: focus goes back to the row."),
    htmltools::tags$h3(class = "h6", "Columns: hide, show, reorder and fold"),
    DT::DTOutput("tbl_columns"),
    spot("HC-046", note = "Use Column visibility, drag a header, and narrow the window so columns fold."),
    htmltools::tags$h3(class = "h6", "A table in a hidden tab and in a modal"),
    bslib::navset_tab(
      id = "tbl_tabs",
      bslib::nav_panel("Open", value = "open", htmltools::p("Switch to the table tab, pin a row, come back.")),
      bslib::nav_panel("Table", value = "table", DT::DTOutput("tbl_hidden"))
    ),
    shiny::actionButton("tbl_modal", "A table in a modal"),
    spot("HC-047"),
    htmltools::tags$h3(class = "h6", "Search and scrollY"),
    DT::DTOutput("tbl_scroll"),
    spot("HC-048"),
    htmltools::tags$h3(class = "h6", "Two tables, a module and a reactable"),
    htmltools::div(
      class = "row",
      htmltools::div(class = "col-md-4", DT::DTOutput("tbl_two_a")),
      htmltools::div(class = "col-md-4", tbl_module_ui("tbl_mod")),
      htmltools::div(class = "col-md-4", reactable::reactableOutput("tbl_react"))
    ),
    spot("HC-049", note = htmltools::tagList(bench_link("Open with two pins", only = "tables", max_pins = 2), "."))
  )
}

tab_tables_server <- function(input, output, session) {
  output$tbl_client <- DT::renderDT(tbl_dt(bench_sites(), "tbl_client", filter = "top"), server = FALSE)

  version <- shiny::reactiveVal(0L)
  server_data <- function() {
    sites <- bench_sites()
    sites$patients <- sites$patients + version()
    sites
  }
  output$tbl_server <- DT::renderDT(tbl_dt(shiny::isolate(server_data()), "tbl_server"))
  proxy <- DT::dataTableProxy("tbl_server")
  shiny::observeEvent(input$tbl_replace, {
    version(version() + 1L)
    DT::replaceData(proxy, server_data(), rownames = FALSE, resetPaging = FALSE)
  })
  shiny::observeEvent(input$tbl_replace_ten, {
    for (i in 1:10) {
      version(version() + 1L)
      DT::replaceData(proxy, server_data(), rownames = FALSE, resetPaging = FALSE)
    }
  })

  output$tbl_panel <- DT::renderDT(
    tbl_dt(
      bench_sites(20L),
      "tbl_panel",
      rows = bench_rows(to = "in_place", more = hovercraft::tip_more(tbl_long, on = "button"))
    ),
    server = FALSE
  )
  output$tbl_columns <- DT::renderDT(
    tbl_dt(
      bench_sites(20L),
      "tbl_columns",
      extensions = c("Buttons", "ColReorder", "Responsive"),
      options = list(pageLength = 5, dom = "Bfrtip", buttons = list("colvis"), colReorder = TRUE)
    ),
    server = FALSE
  )
  output$tbl_hidden <- DT::renderDT(tbl_dt(bench_sites(10L), "tbl_hidden"), server = FALSE)
  shiny::observeEvent(input$tbl_modal, {
    shiny::showModal(shiny::modalDialog(
      title = "A table in a modal",
      DT::DTOutput("tbl_in_modal"),
      easyClose = TRUE,
      size = "l"
    ))
  })
  output$tbl_in_modal <- DT::renderDT(tbl_dt(bench_sites(10L), "tbl_in_modal"), server = FALSE)
  output$tbl_scroll <- DT::renderDT(
    tbl_dt(bench_sites(), "tbl_scroll", options = list(scrollY = "12rem", paging = FALSE)),
    server = FALSE
  )
  output$tbl_two_a <- DT::renderDT(tbl_dt(bench_sites(12L), "tbl_two_a"), server = FALSE)
  tbl_module_server("tbl_mod")
  output$tbl_react <- reactable::renderReactable(
    hovercraft::tip_table(
      reactable::reactable(bench_sites(12L), defaultPageSize = 5),
      rows = bench_rows(),
      id = "tbl_react"
    )
  )
}
