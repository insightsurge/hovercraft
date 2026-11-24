skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if_not_installed("DT")
skip_if_not_installed("reactable")
skip_if_not_installed("bslib")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# tip_show(), tip_hide() and tip_update() with `row` and `column` on the tips of DT (client and server mode) and
# reactable tables in a running Shiny app, and after the table sorts. The calls are driven by an input the page sets, not
# by a click, so what is tested is the call and not the pointer.
describe("server calls on the tips of a table in a Shiny app", {
  scores <- data.frame(
    team = c("Owls", "Foxes", "Hares", "Lynx", "Voles", "Moles"),
    wins = c(7, 5, 2, 9, 1, 4)
  )
  app <- start_app(
    function(scores) {
      tipped <- function(widget, id) {
        tip_table(
          widget,
          rows = "**{{ team }}**: {{ wins | number:0 }} wins",
          cells = c(wins = "Wins of {{ team }}: {{ wins | number:0 }}"),
          headers = c(wins = "The wins column"),
          id = id
        )
      }
      ui <- bslib::page_fluid(
        DT::DTOutput("dtc"),
        DT::DTOutput("dts"),
        reactable::reactableOutput("rt"),
        DT::DTOutput("m-dt")
      )
      server <- function(input, output, session) {
        output$dtc <- DT::renderDT(
          tipped(DT::datatable(scores, rownames = FALSE, options = list(pageLength = 6)), "dtc"),
          server = FALSE
        )
        output$dts <- DT::renderDT(
          tipped(DT::datatable(scores, rownames = FALSE, options = list(pageLength = 2)), "dts")
        )
        output$rt <- reactable::renderReactable(
          tipped(
            reactable::reactable(
              scores,
              defaultPageSize = 6,
              columns = list(wins = reactable::colDef(defaultSortOrder = "desc"))
            ),
            "rt"
          )
        )
        shiny::observeEvent(input$call, {
          call <- input$call
          args <- list(call$table, row = call$row, column = call$column)
          args <- args[!vapply(args, is.null, NA)]
          switch(
            call$fn,
            show = do.call(tip_show, args),
            hide = do.call(tip_hide, args),
            update = do.call(tip_update, c(args, list(text = call$text)))
          )
        })
        shiny::moduleServer("m", function(input, output, session) {
          output$dt <- DT::renderDT(
            tipped(DT::datatable(scores, rownames = FALSE), "tips"),
            server = FALSE
          )
          shiny::observeEvent(input$call, tip_show("tips", row = 4))
        })
      }
      shiny::shinyApp(ui, server)
    },
    scores = scores
  )
  page <- local_page(app$url, environment())
  css <- tip_names()$css
  open_text <- sprintf("document.querySelector('.%s:not([hidden])')?.textContent", css)
  # Asks the server for one call, as the page would by setting an input.
  call <- function(fn, table, row = NULL, column = NULL, text = NULL) {
    payload <- list(fn = fn, table = table, row = row, column = column, text = text)
    payload <- payload[!vapply(payload, is.null, NA)]
    page$evaluate(sprintf(
      "Shiny.setInputValue('call', %s, { priority: 'event' }); 0",
      as.character(jsonlite::toJSON(payload, auto_unbox = TRUE))
    ))
  }
  shows <- function(words) page$wait_until(sprintf("%s === %s", open_text, jsonlite::toJSON(words, auto_unbox = TRUE)))
  open_id <- "document.querySelector('[data-hovercraft-tip-open]')?.getAttribute('data-hovercraft-tip-id')"
  has_rows <- function(table) {
    page$wait_until(sprintf(
      "document.querySelectorAll('#%s tbody tr, #%s .rt-tbody [role=row]').length >= 2",
      table,
      table
    ))
  }

  it("starts and connects", {
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
    for (table in c("dtc", "dts", "rt")) {
      expect_true(has_rows(table), info = table)
    }
  })

  it("opens the tip of a row, a cell and a header cell of a DT table by row and column", {
    call("show", "dtc", row = 2)
    expect_true(shows("Foxes: 5 wins"))
    expect_identical(page$evaluate(open_id), "dtc/row/2")
    call("show", "dtc", row = 2, column = "wins")
    expect_true(shows("Wins of Foxes: 5"))
    call("show", "dtc", column = "wins")
    expect_true(shows("The wins column"))
    # A position counts the data columns, from 1: 2 is `wins`.
    call("show", "dtc", row = 3, column = 2)
    expect_true(shows("Wins of Hares: 2"))
  })

  it("keeps meaning the data row after DT sorts, in client mode", {
    page$evaluate("$('#dtc table').DataTable().order([1, 'desc']).draw(); 0")
    expect_true(page$wait_until("document.querySelector('#dtc tbody tr td')?.textContent.startsWith('Lynx')"))
    call("show", "dtc", row = 4)
    expect_true(shows("Lynx: 9 wins"))
    expect_identical(page$evaluate(open_id), "dtc/row/4")
    # The tip belongs to the first row on screen now: the open tag is the first row of the body.
    expect_true(page$evaluate(
      "document.querySelector('[data-hovercraft-tip-open]') === document.querySelector('#dtc tbody tr')"
    ))
  })

  it("hides the hint it opened, and changes the words of a row's tip", {
    call("hide", "dtc", row = 4)
    expect_true(page$wait_until(sprintf("%s === undefined", open_text)))
    call("update", "dtc", row = 4, text = "Changed **row**")
    call("show", "dtc", row = 4)
    expect_true(shows("Changed row"))
    # Another row keeps its own words.
    call("show", "dtc", row = 5)
    expect_true(shows("Voles: 1 wins"))
  })

  it("opens the tip of a row of a server-side DT table after it sorts, and nothing for a row off the page", {
    page$evaluate("$('#dts table').DataTable().order([1, 'desc']).draw(); 0")
    expect_true(page$wait_until("document.querySelector('#dts tbody tr td')?.textContent.startsWith('Lynx')"))
    call("show", "dts", row = 4)
    expect_true(shows("Lynx: 9 wins"))
    expect_identical(page$evaluate(open_id), "dts/row/4")
    # Row 5 (Voles) is on the second page, which has no element yet: nothing opens, and nothing fails.
    call("show", "dts", row = 5)
    expect_true(page$wait_until(sprintf("%s !== 'Voles: 1 wins'", open_text)))
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
  })

  it("opens the tip of a row, a cell and a header cell of a reactable table, also after it sorts", {
    call("show", "rt", row = 2)
    expect_true(shows("Foxes: 5 wins"))
    call("show", "rt", row = 2, column = "wins")
    expect_true(shows("Wins of Foxes: 5"))
    call("show", "rt", column = "wins")
    expect_true(shows("The wins column"))
    page$evaluate("document.querySelectorAll('#rt [role=columnheader]')[1].click(); 0")
    expect_true(page$wait_until(
      "document.querySelector('#rt .rt-tbody [role=row] [role=cell]')?.textContent.startsWith('Lynx')"
    ))
    call("show", "rt", row = 4)
    expect_true(shows("Lynx: 9 wins"))
    call("show", "rt", row = 1, column = "wins")
    expect_true(shows("Wins of Owls: 7"))
  })

  it("finds a table inside a module by its plain id", {
    expect_true(page$wait_until("document.querySelectorAll('#m-dt tbody tr').length >= 2"))
    page$evaluate("Shiny.setInputValue('m-call', 1, { priority: 'event' }); 0")
    expect_true(shows("Lynx: 9 wins"))
    expect_identical(page$evaluate(open_id), "m-tips/row/4")
  })
})
