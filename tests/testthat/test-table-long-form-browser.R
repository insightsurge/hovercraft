skip_if_no_browser("DT")

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

  # A cell with a long form of its own behind the More button: the button's long form takes the place of the guard while
  # it is open, and the guard must come back when it closes, or the next hover shows the row's long form.
  for (on in c("hover", "pin")) {
    it(paste("keeps its own tip when its own long form opens and closes, under a row that opens on", on), {
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
                more = hovercraft::tip_more("LONGROW {{ team }}", on = on)
              ),
              cells = list(wins = hovercraft::tip_spec("Cell {{ wins }}", more = hovercraft::tip_more("CELLMORE"))),
              id = "scores"
            ))
          }
          shiny::shinyApp(ui, server)
        },
        environment(),
        on = on
      )
      page <- local_page(app$url, environment(), init = keep_warnings)
      expect_true(page$wait_until("document.querySelector('#dt tbody tr[data-hovercraft-tip-id]') !== null"))
      page$settle()
      cell <- "#dt tbody tr:first-child td:nth-child(2)"
      button <- ".hovercraft-more-strip-button"
      own_tip_only <- function() {
        got <- page$hover(cell)
        expect_match(got, "^Cell 7", all = FALSE)
        expect_no_match(got, "LONGROW")
      }
      own_tip_only()
      # The pointer leaves and comes back: a hint that closes puts its item back to the short form.
      own_tip_only()
      # More, then Less, then back again.
      page$hover(cell)
      # The button moves when the tip grows, so its place is read at each press.
      press <- function() {
        at <- jsonlite::fromJSON(page$evaluate(sprintf(
          "(() => { const r = document.querySelector('%s').getBoundingClientRect(); return JSON.stringify([(r.left + r.right) / 2, (r.top + r.bottom) / 2]); })()",
          button
        )))
        page$mouse(at[[1]], at[[2]], click = TRUE)
      }
      press()
      page$settle()
      expect_true(page$wait_until(sprintf(
        "document.querySelector('%s').getAttribute('aria-expanded') === 'true'",
        button
      )))
      expect_match(
        paste(
          page$evaluate(sprintf(
            "JSON.stringify([...document.querySelectorAll('.%s')].filter((t) => !t.hidden).map((t) => t.textContent))",
            tip_names()$css
          )),
          collapse = " "
        ),
        "CELLMORE"
      )
      press()
      expect_true(page$wait_until(sprintf(
        "document.querySelector('%s').getAttribute('aria-expanded') === 'false'",
        button
      )))
      page$settle()
      own_tip_only()
      expect_identical(page$problems(), character())
      expect_identical(
        grep("can pin", jsonlite::fromJSON(page$evaluate("JSON.stringify(window.__warnings)")), value = TRUE),
        character()
      )
    })
  }
})
