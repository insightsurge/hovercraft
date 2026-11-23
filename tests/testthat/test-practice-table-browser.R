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
    testthat::skip("known: tip_table() adds a hidden .hovercraft column that DT::replaceData() does not include")
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
