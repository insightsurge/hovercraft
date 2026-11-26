skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if_not_installed("DT")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# A row's long form opens on hover or pin. The engine reads it from the nearest tagged ancestor, so a cell with a tip of
# its own must say it has no long form, or it shows its row's (on hover) or warns that it cannot pin (on pin).
describe("a tip_table() cell next to a row with a long form", {
  keep_warnings <- "window.__warnings = [];
    const warn = console.warn.bind(console);
    console.warn = (...args) => { window.__warnings.push(args.map(String).join(' ')); warn(...args); };"
  cell_page <- function(on, env) {
    app <- start_app(
      function(on) {
        scores <- data.frame(team = c("Owls", "Foxes"), wins = c(7, 5))
        ui <- shiny::fluidPage(hovercraft::use_hovercraft(), DT::DTOutput("dt"))
        server <- function(input, output, session) {
          output$dt <- DT::renderDT(hovercraft::tip_table(
            DT::datatable(scores, rownames = FALSE),
            rows = hovercraft::tip_spec(
              "Row {{ team }}",
              pin = identical(on, "pin"),
              more = hovercraft::tip_more("LONG of {{ team }}", on = on)
            ),
            cells = c(wins = "Cell {{ wins }}"),
            id = "scores"
          ))
        }
        shiny::shinyApp(ui, server)
      },
      env,
      on = on
    )
    page <- local_page(app$url, env, init = keep_warnings)
    expect_true(page$wait_until("document.querySelector('#dt tbody tr[data-hovercraft-tip-id]') !== null"))
    page$settle()
    page
  }

  for (on in c("hover", "pin")) {
    it(paste("shows only its own tip, with the long form opening on", on), {
      page <- cell_page(on, environment())
      expect_identical(page$hover("#dt tbody tr:first-child td:nth-child(2)"), "Cell 7")
      expect_identical(
        grep("can pin", jsonlite::fromJSON(page$evaluate("JSON.stringify(window.__warnings)")), value = TRUE),
        character()
      )
    })
  }

  it("leaves the row's own long form alone", {
    page <- cell_page("hover", environment())
    expect_identical(page$hover("#dt tbody tr:first-child td:nth-child(1)"), "Row OwlsLONG of Owls")
  })
})
