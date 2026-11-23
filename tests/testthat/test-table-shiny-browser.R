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

# tip_table() in a running Shiny app: DT in its server mode (the browser holds one page of rows; the data row number in
# a tip id and the row a click selects must still be the data row), inside a module's namespace.
describe("tip_table() in a Shiny app, DT in server mode", {
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
        team = c("Owls", "Foxes", "Hares", "Lynx", "Voles"),
        wins = c(7, 5, 2, 9, 1),
        note = c("Top", NA, "Late", "Best", NA)
      )
      module_ui <- function(id) {
        ns <- shiny::NS(id)
        shiny::tagList(DT::DTOutput(ns("dt")), reactable::reactableOutput(ns("rt")))
      }
      module_server <- function(id) {
        shiny::moduleServer(id, function(input, output, session) {
          tips <- function(widget) {
            tip_table(
              widget,
              rows = "**{{ team }}**: {{ wins | number:0 }} wins",
              cells = c(note = "Note: {{ note }}"),
              id = session$ns("tips")
            )
          }
          output$dt <- DT::renderDT(
            tips(DT::datatable(scores, rownames = FALSE, options = list(pageLength = 2)))
          )
          output$rt <- reactable::renderReactable(tips(reactable::reactable(scores, defaultPageSize = 2)))
        })
      }
      ui <- shiny::fluidPage(module_ui("mod1"))
      server <- function(input, output, session) module_server("mod1")
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
  ids <- function(scope) {
    page$evaluate(sprintf(
      "JSON.stringify([...document.querySelectorAll('%s')].map((n) => n.getAttribute('data-hovercraft-tip-id')))",
      scope
    ))
  }

  it("starts", {
    expect_true(up, info = paste(app$read_error(), collapse = "\n"))
  })

  it("gives the rows ids from a namespaced id, and the ids follow the data row after a sort and a page", {
    expect_true(page$wait_until("document.querySelectorAll('#mod1-dt tbody tr[data-hovercraft-tip-id]').length === 2"))
    expect_identical(
      jsonlite::fromJSON(ids("#mod1-dt tbody tr[data-hovercraft-tip-id]"))[1:2],
      c("mod1-tips/row/1", "mod1-tips/row/2")
    )
    expect_identical(page$hover("#mod1-dt tbody tr:first-child td:first-child"), "Owls: 7 wins")
    page$evaluate("$('#mod1-dt table').DataTable().order([1, 'desc']).draw(); 0")
    expect_true(page$wait_until("document.querySelector('#mod1-dt tbody td').textContent.startsWith('Lynx')"))
    expect_true(page$wait_until(
      "document.querySelector('#mod1-dt tbody tr').getAttribute('data-hovercraft-tip-id') === 'mod1-tips/row/4'"
    ))
    expect_identical(page$hover("#mod1-dt tbody tr:first-child td:first-child"), "Lynx: 9 wins")
    page$evaluate("$('#mod1-dt table').DataTable().page(1).draw(false); 0")
    expect_true(page$wait_until("document.querySelector('#mod1-dt tbody td').textContent.startsWith('Foxes')"))
    expect_true(page$wait_until(
      "document.querySelector('#mod1-dt tbody tr').getAttribute('data-hovercraft-tip-id') === 'mod1-tips/row/2'"
    ))
    expect_identical(page$hover("#mod1-dt tbody tr:first-child td:first-child"), "Foxes: 5 wins")
  })

  it("selects the data row a clicked tip belongs to", {
    page$click("#mod1-dt tbody tr:first-child td:first-child")
    expect_true(page$wait_until("Shiny.shinyapp.$inputValues['mod1-dt_rows_selected'] !== undefined"))
    # Page two of the sort by wins, descending: Foxes is data row 2.
    expect_identical(page$evaluate("JSON.stringify(Shiny.shinyapp.$inputValues['mod1-dt_rows_selected'])"), "[2]")
  })

  it("gives a reactable table in the module ids too", {
    expect_true(page$wait_until("document.querySelectorAll('#mod1-rt [role=row][data-hovercraft-tip-id]').length >= 2"))
    expect_identical(
      jsonlite::fromJSON(ids("#mod1-rt [role=row][data-hovercraft-tip-id]"))[1:2],
      c("mod1-tips/row/1", "mod1-tips/row/2")
    )
    expect_identical(page$problems(), character())
  })
})
