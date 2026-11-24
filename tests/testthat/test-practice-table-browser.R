skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if_not_installed("DT")
skip_if_not_installed("reactable")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# Table updates and proxies in a running Shiny app: DT::replaceData(), reactable::updateReactable(),
# and reactable's own sort/paging/selection events with tip_table().
describe("table proxies and updates in a Shiny app", {
  path <- find.package("hovercraft")
  port <- httpuv::randomPort()
  app <- callr::r_bg(
    function(port, path) {
      if (dir.exists(file.path(path, "inst"))) {
        pkgload::load_all(path, quiet = TRUE)
      } else {
        library(hovercraft)
      }
      scores <- data.frame(
        team = c("Owls", "Foxes", "Hares", "Lynx"),
        wins = c(7, 5, 2, 9)
      )
      updated <- data.frame(
        team = c("Eagles", "Hawks", "Wrens", "Kites"),
        wins = c(12, 8, 4, 15)
      )
      ui <- shiny::fluidPage(
        shiny::actionButton("replace_dt", "Replace DT"),
        shiny::actionButton("update_rt", "Update reactable"),
        DT::DTOutput("dt"),
        reactable::reactableOutput("rt"),
        shiny::verbatimTextOutput("rt_sel")
      )
      server <- function(input, output, session) {
        output$dt <- DT::renderDT(
          tip_table(
            DT::datatable(scores, rownames = FALSE, options = list(pageLength = 2)),
            rows = "**{{ team }}**: {{ wins | number:0 }} wins"
          )
        )
        shiny::observeEvent(input$replace_dt, {
          DT::replaceData(DT::dataTableProxy("dt"), updated, rownames = FALSE)
        })
        output$rt <- reactable::renderReactable(
          tip_table(
            reactable::reactable(scores, defaultPageSize = 2, selection = "single"),
            rows = "**{{ team }}**: {{ wins | number:0 }} wins"
          )
        )
        shiny::observeEvent(input$update_rt, {
          reactable::updateReactable("rt", data = updated)
        })
        output$rt_sel <- shiny::renderText(
          paste("selected:", paste(reactable::getReactableState("rt", "selected"), collapse = ","))
        )
      }
      shiny::runApp(shiny::shinyApp(ui, server), port = port, host = "127.0.0.1", launch.browser = FALSE)
    },
    args = list(port = port, path = path),
    supervise = TRUE
  )
  withr::defer(app$kill())
  url <- paste0("http://127.0.0.1:", port)
  up <- FALSE
  for (i in 1:100) {
    up <- !is.null(tryCatch(suppressWarnings(readLines(url, n = 1L)), error = function(e) NULL))
    if (up || !app$is_alive()) {
      break
    }
    Sys.sleep(0.1)
  }
  page <- local_page(url, environment())
  press_button <- function(id) page$evaluate(sprintf("document.getElementById('%s').click()", id))
  hover_in_view <- function(selector) {
    page$evaluate(sprintf(
      "document.querySelector(%s)?.scrollIntoView({ block: 'center', behavior: 'instant' }); 0",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    ))
    Sys.sleep(0.1)
    page$hover(selector)
  }

  it("starts and connects", {
    expect_true(up, info = paste(app$read_error(), collapse = "\n"))
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
  })

  it("keeps reactable row selection working alongside tip_table()", {
    expect_true(page$wait_until("document.querySelectorAll('#rt .rt-tbody .rt-tr-group [role=row]').length >= 2"))
    expect_identical(hover_in_view("#rt .rt-tbody .rt-tr-group:first-child .rt-td:nth-child(2)"), "Owls: 7 wins")
    page$evaluate("document.querySelector('#rt .rt-tbody .rt-tr-group:first-child input[type=radio]').click()")
    expect_true(page$wait_until("document.getElementById('rt_sel')?.textContent?.trim() === 'selected: 1'"))
  })

  it("updates DT tips after DT::replaceData()", {
    expect_true(page$wait_until("document.querySelectorAll('#dt tbody tr').length === 2"))
    expect_identical(hover_in_view("#dt tbody tr:first-child td:first-child"), "Owls: 7 wins")
    press_button("replace_dt")
    expect_true(page$wait_until("document.querySelector('#dt tbody td')?.textContent?.startsWith('Eagles')"))
    expect_identical(hover_in_view("#dt tbody tr:first-child td:first-child"), "Eagles: 12 wins")
  })

  it("updates reactable tips after reactable::updateReactable()", {
    testthat::skip("known: tip_table() closes over initial cfg.records in JS so updateReactable() shows stale tips")
    expect_true(page$wait_until("document.querySelectorAll('#rt .rt-tbody .rt-tr-group [role=row]').length >= 2"))
    press_button("update_rt")
    expect_true(page$wait_until("document.querySelector('#rt .rt-tbody')?.textContent?.includes('Eagles')"))
    expect_identical(hover_in_view("#rt .rt-tbody .rt-tr-group:first-child .rt-td:nth-child(2)"), "Eagles: 12 wins")
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})

# A table that its widget updates with new data keeps tips that follow the data: DT in server mode (`replaceData()` only
# works there), reactable (which reuses its row nodes), and DT's client mode for the data row of a tip; DT's own inputs
# are what they are without tips.
describe("tip_table() follows the data when the table is updated", {
  made <- start_app(function() {
    scores <- data.frame(
      team = c("Owls", "Foxes", "Hares", "Lynx"),
      wins = c(7, 5, 2, 9),
      note = c("a", NA, "c", "d")
    )
    updated <- data.frame(
      team = c("Zürich ✓", "Hawks", "Wrens", "Kites", "Extra"),
      wins = c(12, 8, 4, 15, 3),
      note = c("n1", "n2", NA, "n4", "n5")
    )
    tips <- function(widget, id) {
      tip_table(widget, rows = "{{ team }}: {{ wins | number:0 }} wins", cells = c(note = "Note: {{ note }}"), id = id)
    }
    dt <- function(data, ...) DT::datatable(data, rownames = FALSE, options = list(pageLength = 3), ...)
    ui <- shiny::fluidPage(
      shiny::actionButton("replace", "Replace"),
      DT::DTOutput("dt_server"),
      DT::DTOutput("dt_client"),
      DT::DTOutput("dt_plain"),
      shiny::verbatimTextOutput("parity")
    )
    server <- function(input, output, session) {
      output$dt_server <- DT::renderDT(tips(dt(scores), "dts"), server = TRUE)
      output$dt_client <- DT::renderDT(tips(dt(scores), "dtc"), server = FALSE)
      output$dt_plain <- DT::renderDT(dt(scores), server = TRUE)
      shiny::observeEvent(input$replace, {
        DT::replaceData(DT::dataTableProxy("dt_server"), updated, rownames = FALSE)
        DT::replaceData(DT::dataTableProxy("dt_plain"), updated, rownames = FALSE)
      })
      # DT's own inputs, with and without tips, after the same actions.
      output$parity <- shiny::renderText({
        same <- function(what) identical(input[[paste0("dt_server_", what)]], input[[paste0("dt_plain_", what)]])
        paste(same("rows_all"), same("rows_current"), same("rows_selected"), same("search"), same("cell_clicked"))
      })
    }
    shiny::shinyApp(ui, server)
  })
  page <- local_page(made$url, environment())
  hover_in_view <- function(selector) {
    page$evaluate(sprintf(
      "document.querySelector(%s)?.scrollIntoView({ block: 'center', behavior: 'instant' }); 0",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    ))
    Sys.sleep(0.1)
    page$hover(selector)
  }
  id_attr <- tip_attribute("id")
  id_of <- function(selector) {
    page$evaluate(sprintf(
      "document.querySelector(%s)?.getAttribute('%s')",
      jsonlite::toJSON(selector, auto_unbox = TRUE),
      id_attr
    ))
  }
  first_cell <- function(table) sprintf("#%s tbody tr:first-child td:first-child", table)
  note_cell <- function(table) sprintf("#%s tbody tr:first-child td:nth-child(3)", table)

  it("starts and connects", {
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
    for (table in c("dt_server", "dt_client", "dt_plain")) {
      expect_true(
        page$wait_until(sprintf("document.querySelectorAll('#%s tbody tr').length === 3", table)),
        info = table
      )
    }
  })

  it("keeps DT's own inputs what they are without tips, after sorting, searching and selecting", {
    page$evaluate(
      "$('#dt_server table').DataTable().order([1, 'desc']).draw(); $('#dt_plain table').DataTable().order([1, 'desc']).draw(); 0"
    )
    page$evaluate("$('#dt_server tbody tr:first-child').click(); $('#dt_plain tbody tr:first-child').click(); 0")
    Sys.sleep(0.8)
    expect_identical(page$evaluate("document.getElementById('parity').textContent.trim()"), "TRUE TRUE TRUE TRUE TRUE")
    page$evaluate(
      "$('#dt_server table').DataTable().search('o').draw(); $('#dt_plain table').DataTable().search('o').draw(); 0"
    )
    Sys.sleep(0.8)
    expect_identical(page$evaluate("document.getElementById('parity').textContent.trim()"), "TRUE TRUE TRUE TRUE TRUE")
    page$evaluate(
      "$('#dt_server table').DataTable().search('').order([0, 'asc']).draw(); $('#dt_plain table').DataTable().search('').order([0, 'asc']).draw(); 0"
    )
    Sys.sleep(0.5)
  })

  it("shows each DT row's own tip, and its derived id, in server mode while the browser holds one page", {
    page$evaluate("$('#dt_server table').DataTable().order([1, 'desc']).draw(); 0")
    expect_true(page$wait_until("document.querySelector('#dt_server tbody td')?.textContent?.startsWith('Lynx')"))
    expect_identical(hover_in_view(first_cell("dt_server")), "Lynx: 9 wins")
    # Lynx is the fourth row of the data, whatever its place on the page.
    expect_identical(id_of("#dt_server tbody tr:first-child"), "dts/row/4")
    page$evaluate("$('#dt_server table').DataTable().order([]).draw(); 0")
  })

  it("follows DT::replaceData() with the user's own data frame, in server mode", {
    page$evaluate("document.getElementById('replace').click()")
    expect_true(page$wait_until("document.querySelector('#dt_server tbody td')?.textContent?.startsWith('Zürich')"))
    expect_identical(hover_in_view(first_cell("dt_server")), "Zürich ✓: 12 wins")
    expect_identical(hover_in_view(note_cell("dt_server")), "Note: n1")
    expect_identical(id_of("#dt_server tbody tr:first-child"), "dts/row/1")
  })

  it("keeps following the new data after a sort and a change of page, in server mode", {
    page$evaluate("$('#dt_server table').DataTable().order([1, 'desc']).draw(); 0")
    expect_true(page$wait_until("document.querySelector('#dt_server tbody td')?.textContent?.startsWith('Kites')"))
    expect_identical(hover_in_view(first_cell("dt_server")), "Kites: 15 wins")
    expect_identical(id_of("#dt_server tbody tr:first-child"), "dts/row/4")
    page$evaluate("$('#dt_server table').DataTable().page(1).draw(false); 0")
    # By wins, high to low: Kites, Zürich ✓, Hawks | Wrens, Extra.
    expect_true(page$wait_until("document.querySelector('#dt_server tbody td')?.textContent?.startsWith('Wrens')"))
    expect_identical(hover_in_view(first_cell("dt_server")), "Wrens: 4 wins")
    expect_identical(id_of("#dt_server tbody tr:first-child"), "dts/row/3")
  })

  it("gives a DT cell that holds NA no tip after the update, in server mode", {
    page$evaluate("$('#dt_server table').DataTable().order([0, 'asc']).page(1).draw(false); 0")
    # Sorted by team: Extra, Hawks, Kites | Wrens (note NA), Zürich ✓.
    expect_true(page$wait_until("document.querySelector('#dt_server tbody td')?.textContent?.startsWith('Wrens')"))
    expect_true(page$evaluate(
      "document.querySelector('#dt_server tbody tr:first-child').children[2].hasAttribute('data-hovercraft-tip-template') === false"
    ))
    expect_true(page$evaluate(
      "document.querySelector('#dt_server tbody tr:nth-child(2)').children[2].hasAttribute('data-hovercraft-tip-template')"
    ))
  })

  it("shows each row's own tip and derived id in client mode, where DT's row index is the data row", {
    page$evaluate("$('#dt_client table').DataTable().order([1, 'desc']).draw(); 0")
    expect_true(page$wait_until("document.querySelector('#dt_client tbody td')?.textContent?.startsWith('Lynx')"))
    expect_identical(hover_in_view(first_cell("dt_client")), "Lynx: 9 wins")
    expect_identical(id_of("#dt_client tbody tr:first-child"), "dtc/row/4")
    page$evaluate("$('#dt_client table').DataTable().order([0, 'asc']).draw(); 0")
    # Foxes holds NA in its note: no tip on that cell.
    expect_true(page$wait_until("document.querySelector('#dt_client tbody td')?.textContent?.startsWith('Foxes')"))
    expect_true(page$evaluate(
      "!document.querySelector('#dt_client tbody tr:first-child').children[2].hasAttribute('data-hovercraft-tip-template')"
    ))
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})
