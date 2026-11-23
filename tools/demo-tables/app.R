# Demo for the experimental tip_table(): each real table sits beside the code that makes it.
# Run it from the package folder: shiny::runApp("tools/demo-tables"). Needs DT, reactable, bslib and shiny.
#
# Every table is built by running the R code you see next to it (the text is evaluated, so what you read is what
# runs). The second tab shows what tip_table() added to the widget behind the scenes; the third shows, from the live
# page, the first row and cell as the browser holds them. Press "Show" again after a sort or a page change to compare.
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

# The theme that makes reactable follow the page's light and dark mode.
bs_theme <- reactable::reactableTheme(
  color = "var(--bs-body-color)",
  backgroundColor = "var(--bs-body-bg)",
  borderColor = "var(--bs-border-color)",
  highlightColor = "var(--bs-tertiary-bg)",
  inputStyle = list(backgroundColor = "var(--bs-body-bg)", color = "var(--bs-body-color)"),
  pageButtonHoverStyle = list(backgroundColor = "var(--bs-tertiary-bg)")
)

# The code of each example, as text. It is shown as written and then run.
code <- list(
  dt = 'DT::datatable(study, rownames = FALSE, filter = "top", options = list(pageLength = 8)) |>
  tip_table(
    rows = "**{{ site }}** ({{ country }}): {{ patients | number:0 }} patients, {{ response | percent:1 }} responded",
    cells = c(status = "{{ status }}: see the site\'s monthly report", note = "{{ note }}"),
    headers = c(response = "Share of patients who responded to treatment"),
    placement = "bottom"
  )',
  rt = 'reactable::reactable(
  study,
  filterable = TRUE, searchable = TRUE, defaultPageSize = 8, highlight = TRUE, theme = bs_theme,
  columns = list(response = reactable::colDef(format = reactable::colFormat(percent = TRUE, digits = 1)))
) |>
  tip_table(
    rows = "**{{ site }}** ({{ country }}): {{ patients | number:0 }} patients, {{ response | percent:1 }} responded",
    cells = c(status = "{{ status }}: see the site\'s monthly report", note = "{{ note }}"),
    headers = c(response = "Share of patients who responded to treatment"),
    placement = "bottom"
  )',
  rt_cell = '# In the UI, once: tip_define(site = tip_template(tip_title("{{ site }}"), tip_text("{{ patients }} patients in {{ country }}")))
reactable::reactable(
  study[c("site", "country", "status")],
  defaultPageSize = 8, theme = bs_theme,
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
)',
  dt_html = '# In the UI, once: tip_dependency() and the same tip_define() as above
shown <- study[c("site", "country", "status")]
shown$status <- vapply(seq_len(nrow(study)), function(i) {
  as.character(tip_on(
    htmltools::tags$span(study$status[[i]], tabindex = "0"),
    template = "site",
    record = study[i, c("site", "country", "patients")]
  ))
}, "")
DT::datatable(shown, rownames = FALSE, escape = FALSE, options = list(pageLength = 5))'
)

run <- function(text) {
  eval(parse(text = text), envir = list2env(list(study = study, bs_theme = bs_theme), parent = globalenv()))
}
widgets <- lapply(code, run)

# What tip_table() wrote into each widget, as text to read.
as_json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, null = "null")
added_dt <- function(w) {
  defs <- Filter(function(d) !is.null(d$createdCell), w$x$options$columnDefs)
  hidden <- Filter(function(d) isTRUE(d$visible == FALSE), w$x$options$columnDefs)
  paste(
    c(
      "# 1. One hidden column, '.hovercraft', holds each row's record as JSON (first two rows):",
      utils::head(w$x$data[[".hovercraft"]], 2),
      "",
      "# 2. Its column definition (hidden, not searchable, named so server-side paging can find it):",
      as_json(hidden[[1]]),
      "",
      "# 3. options$createdRow: DT calls it for each row it draws; it writes the tip's attributes onto the row:",
      as.character(w$x$options$createdRow),
      "",
      "# 4. columnDefs[[...]]$createdCell: the same for the cells of one column (status):",
      as.character(defs[[1]]$createdCell),
      "",
      "# 5. options$initComplete: marks the header cells once DT has drawn its header:",
      as.character(w$x$options$initComplete),
      "",
      "# 6. A render hook defines the templates in the browser (name -> blocks):",
      as_json(w$jsHooks$render[[1]]$data$defines)
    ),
    collapse = "\n"
  )
}
added_rt <- function(w) {
  tag <- w$x$tag
  ids <- vapply(tag$attribs$columns, function(column) column$id, "")
  cfg <- w$jsHooks$render[[1]]$data
  paste(
    c(
      "# 1. rowClass becomes a function that puts the data row number in a class (reactable has no row attribute hook):",
      as.character(tag$attribs$rowClassName),
      "",
      "# 2. The status column's class function does the same for its cells:",
      as.character(tag$attribs[["columns"]][[match("status", ids)]]$className),
      "",
      "# 3. The response header gets a class naming its column position:",
      tag$attribs[["columns"]][[match("response", ids)]]$headerClassName,
      "",
      "# 4. The page layer (inst/table/table.js) reads those classes and gets this data: the specs that say which",
      "#    attributes each row, cell and header gets, and one JSON record per data row (first two shown):",
      as_json(list(rows = cfg$rows, cells = cfg$cells, headers = cfg$headers, records = utils::head(cfg$records, 2))),
      "",
      "# 5. And the templates to define in the browser (name -> blocks):",
      as_json(cfg$defines)
    ),
    collapse = "\n"
  )
}
added_plain <- function(text) text
added <- list(
  dt = added_dt(widgets$dt),
  rt = added_rt(widgets$rt),
  rt_cell = paste(
    "tip_table() is not used here. reactable calls the cell function for each cell and hands the returned tag to the",
    "page, so tip_on() writes the tip's attributes into that tag itself: no layer, no hidden column.",
    "",
    "The tag for the first status cell, as R made it:",
    as.character(tip_on(
      htmltools::tags$span(study$status[[1]], tabindex = "0"),
      template = "site",
      record = study[1, c("site", "country", "patients")]
    )),
    sep = "\n"
  ),
  dt_html = paste(
    "tip_table() is not used here. The status column is HTML text built in R (escape = FALSE), so each cell holds the",
    "tag tip_on() made. DT sorts and searches that column on its text.",
    "",
    "The cell text for the first row:",
    as.character(tip_on(
      htmltools::tags$span(study$status[[1]], tabindex = "0"),
      template = "site",
      record = study[1, c("site", "country", "patients")]
    )),
    sep = "\n"
  )
)

# The package's own files, shown whole at the bottom.
root <- pkgload::pkg_path(".")
package_files <- c(
  "R/table.R (what R does)" = file.path(root, "R", "table.R"),
  "inst/table/table.js (what the page layer does)" = file.path(root, "inst", "table", "table.js")
)

# Reads the first row and cell from the live page and writes their attributes, one per line.
inspect_js <- function(out, row, cell) {
  sprintf(
    "showDom('%s', '%s', '%s')",
    out,
    gsub("'", "\\\\'", row),
    gsub("'", "\\\\'", cell)
  )
}

example <- function(id, title, row, cell, table) {
  bslib::card(
    fill = FALSE,
    bslib::card_header(title),
    bslib::layout_columns(
      fill = FALSE,
      col_widths = c(6, 6),
      shiny::div(style = "overflow-x: auto;", table),
      bslib::navset_tab(
        bslib::nav_panel("Your R code", shiny::tags$pre(class = "demo-code", code[[id]])),
        bslib::nav_panel("What was added", shiny::tags$pre(class = "demo-code", added[[id]])),
        bslib::nav_panel(
          "On the page",
          shiny::tags$button(
            "Show the first row and cell",
            class = "btn btn-sm btn-outline-secondary my-2",
            onclick = inspect_js(paste0("dom_", id), row, cell)
          ),
          shiny::tags$pre(id = paste0("dom_", id), class = "demo-code", "Press the button.")
        )
      )
    )
  )
}

demo_head <- shiny::tags$head(
  shiny::tags$style(shiny::HTML(
    ".demo-code { max-height: 420px; overflow: auto; font-size: 0.78rem; white-space: pre-wrap; word-break: break-word; }
     .card { margin-bottom: 1.5rem; }"
  )),
  shiny::tags$script(shiny::HTML(
    "function showDom(out, rowSel, cellSel) {
       const show = (el) => {
         if (!el) return '(not found: the element is not on this page of the table)';
         const clone = el.cloneNode(false);
         const lines = ['<' + clone.tagName.toLowerCase()];
         for (const a of clone.attributes) lines.push('  ' + a.name + '=\"' + a.value + '\"');
         return lines.join('\\n') + '>';
       };
       const text = ['The row:', show(document.querySelector(rowSel)), '', 'The cell:', show(document.querySelector(cellSel))];
       document.getElementById(out).textContent = text.join('\\n');
     }"
  ))
)

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5),
  demo_head,
  tip_dependency(),
  tip_define(site = tip_template(tip_title("{{ site }}"), tip_text("{{ patients }} patients in {{ country }}"))),
  bslib::input_dark_mode(id = "mode"),
  shiny::h2("tip_table() demo (experimental)"),
  shiny::p("Hover rows, status and note cells, and the response header. Sort, search, change page, switch to dark."),
  example(
    "dt",
    "DT with tip_table()",
    "#dt tbody tr:first-child",
    "#dt tbody tr:first-child td:nth-child(5)",
    DT::DTOutput("dt", height = "auto", fill = FALSE)
  ),
  shiny::verbatimTextOutput("dt_selected"),
  example(
    "rt",
    "reactable with tip_table()",
    "#rt [role=row][data-hovercraft-tip-template]",
    "#rt [role=cell][data-hovercraft-tip-template]",
    reactable::reactableOutput("rt", height = "auto")
  ),
  example(
    "rt_cell",
    "reactable with tip_on() in a cell function (no tip_table(), like reactable's tippy cookbook example)",
    "#rt_cell [role=columnheader] span[data-hovercraft-tip]",
    "#rt_cell [role=cell] span[data-hovercraft-tip-template]",
    reactable::reactableOutput("rt_cell", height = "auto")
  ),
  example(
    "dt_html",
    "DT with tip_on() as HTML in a column (no tip_table())",
    "#dt_html tbody tr:first-child",
    "#dt_html tbody span[data-hovercraft-tip-template]",
    DT::DTOutput("dt_html", height = "auto", fill = FALSE)
  ),
  bslib::card(
    fill = FALSE,
    bslib::card_header("The package's own code"),
    shiny::selectInput("file", NULL, names(package_files), width = "100%"),
    shiny::tags$pre(class = "demo-code", style = "max-height: 600px;", shiny::textOutput("file_text"))
  )
)

server <- function(input, output, session) {
  # DT picks its Bootstrap look from the session, so the tables are built inside the render calls.
  output$dt <- DT::renderDT(run(code$dt))
  output$dt_selected <- shiny::renderPrint(cat(
    "Selected site (a DT row click is the data row):",
    study$site[input$dt_rows_selected]
  ))
  output$rt <- reactable::renderReactable(run(code$rt))
  output$rt_cell <- reactable::renderReactable(run(code$rt_cell))
  output$dt_html <- DT::renderDT(run(code$dt_html))
  output$file_text <- shiny::renderText(paste(readLines(package_files[[input$file]], warn = FALSE), collapse = "\n"))
}

shiny::shinyApp(ui, server)
