# Demo for the experimental tip_table(): a DT table and a reactable table, both with tips on rows, cells and headers.
# Run it from the package folder: shiny::runApp("tools/demo-tables"). Needs DT, reactable, bslib and shiny.
pkgload::load_all(".", quiet = TRUE)

# Made-up study sites; no random numbers, so the table is the same on every run.
n <- 60
study <- data.frame(
  site = sprintf("Site %02d", seq_len(n)),
  country = rep(c("Kenya", "Peru", "Vietnam", "Norway", "Chile", "Ghana"), length.out = n),
  patients = (seq_len(n) * 37) %% 133 + 8,
  response = round(((seq_len(n) * 53) %% 71 + 20) / 100, 3),
  status = rep(c("On track", "Late", "Closed"), length.out = n)[(seq_len(n) * 7) %% n + 1],
  note = ifelse(
    seq_len(n) %% 5 < 2,
    paste("Visit", c("delayed", "moved", "audited")[seq_len(n) %% 3 + 1]),
    NA
  )
)

rows_tip <- "**{{ site }}** ({{ country }}): {{ patients | number:0 }} patients, {{ response | percent:1 }} responded"
cell_tips <- c(status = "{{ status }}: see the site's monthly report", note = "{{ note }}")
head_tips <- c(response = "Share of patients who responded to treatment", patients = "Patients enrolled so far")

bs_theme <- reactable::reactableTheme(
  color = "var(--bs-body-color)",
  backgroundColor = "var(--bs-body-bg)",
  borderColor = "var(--bs-border-color)",
  highlightColor = "var(--bs-tertiary-bg)",
  inputStyle = list(backgroundColor = "var(--bs-body-bg)", color = "var(--bs-body-color)"),
  pageButtonHoverStyle = list(backgroundColor = "var(--bs-tertiary-bg)")
)

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5),
  tip_dependency(),
  tip_define(site = tip_template(tip_title("{{ site }}"), tip_text("{{ patients }} patients in {{ country }}"))),
  bslib::input_dark_mode(id = "mode"),
  shiny::h2("tip_table() demo (experimental)"),
  shiny::h4("DT"),
  DT::DTOutput("dt"),
  shiny::verbatimTextOutput("dt_selected"),
  shiny::h4("reactable"),
  reactable::reactableOutput("rt"),
  shiny::h4("DT, tip_on() as HTML in a column (escape = FALSE), no tip_table()"),
  DT::DTOutput("dt_html"),
  shiny::h4("reactable, tip_on() in a cell function (no tip_table(), like reactable's tippy cookbook example)"),
  reactable::reactableOutput("rt_cell")
)

server <- function(input, output, session) {
  output$dt <- DT::renderDT({
    DT::datatable(study, rownames = FALSE, filter = "top", options = list(pageLength = 8)) |>
      tip_table(rows = rows_tip, cells = cell_tips, headers = head_tips, placement = "bottom")
  })
  output$dt_selected <- shiny::renderPrint(study$site[input$dt_rows_selected])
  output$rt <- reactable::renderReactable({
    reactable::reactable(
      study,
      filterable = TRUE,
      searchable = TRUE,
      defaultPageSize = 8,
      highlight = TRUE,
      theme = bs_theme,
      columns = list(response = reactable::colDef(format = reactable::colFormat(percent = TRUE, digits = 1)))
    ) |>
      tip_table(rows = rows_tip, cells = cell_tips, headers = head_tips, placement = "bottom")
  })
  # The DT way people already use: the cell's HTML is built in R and the column is not escaped.
  output$dt_html <- DT::renderDT({
    shown <- study[c("site", "country", "status")]
    shown$status <- vapply(
      seq_len(nrow(study)),
      function(i) {
        as.character(tip_on(
          htmltools::tags$span(study$status[[i]], tabindex = "0"),
          template = "site",
          record = study[i, c("site", "country", "patients")]
        ))
      },
      ""
    )
    DT::datatable(shown, rownames = FALSE, escape = FALSE, options = list(pageLength = 5))
  })

  # The cookbook way: a `cell` function returns a tag, and tip_on() puts the tip on that tag. Only that column has
  # tips, and the tip sits on the text inside the cell, not on the cell.
  output$rt_cell <- reactable::renderReactable({
    reactable::reactable(
      study[c("site", "country", "status")],
      defaultPageSize = 8,
      theme = bs_theme,
      columns = list(
        country = reactable::colDef(
          header = function(value) tip_on(htmltools::tags$span(value, tabindex = "0"), "Where the site is")
        ),
        status = reactable::colDef(
          cell = function(value, index) {
            tip_on(
              htmltools::tags$span(value, tabindex = "0"),
              template = "site",
              record = study[index, c("site", "country", "patients")]
            )
          }
        )
      )
    )
  })
}

shiny::shinyApp(ui, server)
