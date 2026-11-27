skip_if_no_browser(c("DT", "reactable"))

# The Shiny inputs of a table: one input per event, not one per row, with the address of the row (`row` is the 1-based
# data row, also in server mode and after a sort), `input$<id>_pinned`, the plain id inside a module, and the warnings
# of the page's script for two tips or two tables with one id. Real DT (client and server mode) and reactable tables.
# The pins are made with the engine's own door (`Hovercraft.pin()`), so the tests do not depend on where a pointer lands.
describe("the inputs of tip_table() in a Shiny app", {
  scores <- data.frame(
    team = c("Owls", "Foxes", "Hares", "Lynx", "Voles", "Moles"),
    wins = c(7, 5, 2, 9, 1, 4),
    note = c("Top", NA, "Late", "Best", NA, "New")
  )
  app <- start_app(
    function(scores) {
      listen <- function(id) {
        tip_spec("**{{ team }}**: {{ wins | number:0 }} wins", pin = TRUE, events = c("hint", "pin"))
      }
      tipped <- function(widget, id) tip_table(widget, rows = listen(id), cells = c(note = "Note: {{ note }}"), id = id)
      ui <- bslib::page_fluid(
        shiny::actionButton("rerender", "Draw again"),
        tip_on(shiny::actionButton("go", "Go"), "Go tip", id = "go_tip", pin = TRUE, events = "pin"),
        DT::DTOutput("dtc"),
        DT::DTOutput("dts"),
        DT::DTOutput("dtx"),
        reactable::reactableOutput("rt"),
        reactable::reactableOutput("rt2"),
        DT::DTOutput("m-dt"),
        DT::DTOutput("m-dts"),
        reactable::reactableOutput("m-rt"),
        shiny::verbatimTextOutput("seen"),
        shiny::textOutput("domain")
      )
      server <- function(input, output, session) {
        output$dtc <- DT::renderDT(
          {
            input$rerender
            tipped(DT::datatable(scores, rownames = FALSE, options = list(pageLength = 6)), "dtc")
          },
          server = FALSE
        )
        output$dts <- DT::renderDT(tipped(
          DT::datatable(scores, rownames = FALSE, options = list(pageLength = 3)),
          "dts"
        ))
        output$dtx <- DT::renderDT(
          tipped(DT::datatable(scores, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 3)), "dtx"),
          server = FALSE
        )
        output$rt <- reactable::renderReactable(tipped(reactable::reactable(scores, defaultPageSize = 6), "rt"))
        # A small table of its own, for the test of two pins.
        output$rt2 <- reactable::renderReactable(
          tip_table(
            reactable::reactable(data.frame(team = c("A", "B", "C"), wins = c(1, 2, 3))),
            rows = listen("rt2"),
            id = "rt2"
          )
        )
        shiny::moduleServer("m", function(input, output, session) {
          output$dt <- DT::renderDT(tipped(DT::datatable(scores, rownames = FALSE), "tips"), server = FALSE)
          output$dts <- DT::renderDT(
            tipped(DT::datatable(scores, rownames = FALSE, options = list(pageLength = 3)), "tips_srv")
          )
          output$rt <- reactable::renderReactable(tipped(reactable::reactable(scores, defaultPageSize = 6), "rtips"))
        })
        # Every input of the three kinds that exists, as the server sees it.
        output$seen <- shiny::renderText({
          everything <- shiny::reactiveValuesToList(input)
          at <- grepl("_(pin|hint|pinned)$", names(everything))
          jsonlite::toJSON(everything[at][order(names(everything[at]))], auto_unbox = TRUE, null = "null")
        })
      }
      shiny::shinyApp(ui, server)
    },
    scores = scores
  )
  warns <- paste(
    "window.warned = [];",
    "const original = console.warn.bind(console);",
    "console.warn = (...args) => { window.warned.push(args.join(' ')); original(...args); };"
  )
  page <- local_page(app$url, environment(), init = warns)
  global <- tip_names()$global
  # What the server sees under `name`, as parsed JSON, or NULL when the input has not been set.
  # The row of a table whose text starts with the team, found whatever place it has on the screen.
  row_of <- function(scope, team) {
    sprintf(
      "[...document.querySelectorAll('%s')].find((r) => r.textContent.trim().startsWith('%s'))",
      scope,
      team
    )
  }
  pin_row <- function(scope, team) {
    page$evaluate(sprintf("window.%s.pin(%s); 0", global, row_of(scope, team)))
  }
  close_all <- function() page$evaluate(sprintf("window.%s.close(); 0", global))
  open_pins <- function() {
    as.character(jsonlite::fromJSON(page$evaluate(
      "JSON.stringify([...document.querySelectorAll('[data-hovercraft-tip-open=pin]')].map((n) => n.getAttribute('data-hovercraft-tip-id')))"
    )))
  }
  rows_in <- function(id, type = "dt") {
    if (type == "dt") {
      sprintf("#%s tbody tr[data-hovercraft-tip-id]", id)
    } else {
      sprintf("#%s [role=row][data-hovercraft-tip-id]", id)
    }
  }

  it("starts, and every input is NULL before its first event", {
    expect_true(app$proc$is_alive(), info = paste(app$proc$read_error(), collapse = "\n"))
    expect_true(page$wait_connected())
    expect_true(page$wait_until(sprintf("document.querySelectorAll('%s').length === 6", rows_in("dtc"))))
    expect_true(page$wait_until(sprintf("document.querySelectorAll('%s').length >= 6", rows_in("rt", "rt"))))
    # A server that has not heard of an event reads NULL: no input of the three kinds exists at all.
    expect_identical(page$evaluate("document.getElementById('seen').textContent"), "{}")
    for (name in c("dtc_pin", "dtc_hint", "dtc_pinned", "rt_pin", "rt_pinned", "m-tips_pinned", "go_tip_pinned")) {
      expect_null(page$evaluate(sprintf("Shiny.shinyapp.$inputValues['%s']", name)), info = name)
    }
    expect_identical(page$evaluate("window.warned.filter((w) => /two (tables|tagged)/.test(w)).length"), 0L)
  })

  it("sends TRUE and FALSE as the _pinned input of a tag of tip_on()", {
    page$evaluate(sprintf("window.%s.pin(document.getElementById('go')); 0", global))
    expect_true(page$wait_reported("go_tip_pin", "v.state === 'pinned'"))
    expect_true(page$wait_reported("go_tip_pinned", "v === true"))
    close_all()
    expect_true(page$wait_reported("go_tip_pin", "v.state === 'closed'"))
    expect_true(page$wait_reported("go_tip_pinned", "v === false"))
    expect_identical(page$reported("go_tip_pinned"), FALSE)
  })

  it("sends the data row of a pin in DT after a sort, not the row's place on the screen", {
    page$evaluate("$('#dtc table').DataTable().order([1, 'desc']).draw(); 0")
    expect_true(page$wait_until("document.querySelector('#dtc tbody td').textContent.startsWith('Lynx')"))
    expect_true(page$wait_until(
      "document.querySelector('#dtc tbody tr:nth-child(1)').getAttribute('data-hovercraft-tip-id') === 'dtc/row/4'"
    ))
    # Hares is the fifth row on the screen and the third in the data.
    pin_row(rows_in("dtc"), "Hares")
    expect_true(page$wait_reported("dtc_pin", "v.state === 'pinned'"))
    got <- page$reported("dtc_pin")
    expect_identical(got$target, "row")
    expect_identical(got$row, 3L)
    expect_null(got$column)
    expect_identical(got$id, "dtc/row/3")
    expect_identical(got$state, "pinned")
    expect_identical(got$text, "Hares: 2 wins")
    expect_true(page$wait_reported("dtc_pinned", "v.length === 1"))
    pinned <- page$reported("dtc_pinned")
    expect_identical(pinned[[1]], list(target = "row", row = 3L, column = NULL, id = "dtc/row/3"))
    # The tip changes size and side when the page gets narrow; it is still open, and the server is not told otherwise.
    page$viewport(420, 700)
    page$hold(0.3) # the pin must stay open when the page narrows: nothing happening has no signal
    page$settle()
    expect_identical(open_pins(), "dtc/row/3")
    expect_identical(page$reported("dtc_pin")$state, "pinned")
    page$viewport(900, 700)
    page$hold(0.3) # the pin must stay open when the page widens: nothing happening has no signal
    page$settle()
    expect_identical(open_pins(), "dtc/row/3")
    close_all()
    expect_true(page$wait_reported("dtc_pin", "v.state === 'closed'"))
    expect_true(page$wait_reported("dtc_pinned", "v.length === 0"))
    # An empty list, not NULL: a pin has happened.
    expect_identical(page$reported("dtc_pinned"), list())
    expect_identical(page$reported("dtc_pin")$row, 3L)
  })

  it("sends the data row in DT's server mode, where the browser holds one page", {
    page$evaluate("$('#dts table').DataTable().order([1, 'asc']).draw(); 0")
    expect_true(page$wait_until("document.querySelector('#dts tbody td').textContent.startsWith('Voles')"))
    expect_true(page$wait_until(
      "document.querySelector('#dts tbody tr:nth-child(1)').getAttribute('data-hovercraft-tip-id') === 'dts/row/5'"
    ))
    # Sorted by wins ascending, the third row on the page is Moles (data row 6), and Hares (data row 3) the second.
    pin_row(rows_in("dts"), "Hares")
    expect_true(page$wait_reported("dts_pin", "v.state === 'pinned'"))
    expect_identical(page$reported("dts_pin")$row, 3L)
    expect_identical(page$reported("dts_pin")$id, "dts/row/3")
    expect_identical(page$reported("dts_pinned")[[1]]$row, 3L)
    # The page changes: the row leaves it, and the pin goes with it (by the data), as the server hears.
    page$evaluate("$('#dts table').DataTable().page(1).draw(false); 0")
    expect_true(page$wait_reported("dts_pin", "v.state === 'closed'"))
    expect_identical(page$reported("dts_pin")$reason, "gone")
    expect_identical(page$reported("dts_pin")$row, 3L)
    expect_true(page$wait_reported("dts_pinned", "v.length === 0"))
    pin_row(rows_in("dts"), "Lynx")
    expect_true(page$wait_reported("dts_pin", "v.state === 'pinned'"))
    expect_identical(page$reported("dts_pin")$row, 4L)
    close_all()
    expect_true(page$wait_reported("dts_pinned", "v.length === 0"))
  })

  it("sends the data row of a reactable row after a sort", {
    page$click_now("#rt [role=columnheader]:nth-child(2)")
    # The sort has landed when the rows carry the addresses of their new places, not only their new text: Voles (data
    # row 5) first, and the Hares row (data row 3) with its own address. Then the page is left to settle (#175).
    expect_true(page$wait_until(
      "(() => { const first = document.querySelector('#rt [role=row][data-hovercraft-tip-id]'); return first.textContent.trim().startsWith('Voles') && first.getAttribute('data-hovercraft-tip-id') === 'rt/row/5'; })()"
    ))
    expect_true(page$wait_until(sprintf(
      "%s?.getAttribute('data-hovercraft-tip-id') === 'rt/row/3'",
      row_of(rows_in("rt", "rt"), "Hares")
    )))
    page$settle()
    pin_row(rows_in("rt", "rt"), "Hares")
    expect_true(
      page$wait_reported("rt_pin", "v.state === 'pinned'"),
      info = paste(
        "the last rt_pin input:",
        jsonlite::toJSON(page$reported("rt_pin"), auto_unbox = TRUE, null = "null")
      )
    )
    got <- page$reported("rt_pin")
    expect_identical(got$row, 3L)
    expect_identical(got$target, "row")
    expect_null(got$column)
    expect_identical(page$reported("rt_pinned")[[1]]$row, 3L)
    close_all()
    expect_true(page$wait_reported("rt_pinned", "v.length === 0"))
  })

  it("sends a hint input for the table too, with the same address", {
    page$evaluate("window.Hovercraft.hide(); 0")
    # Foxes is data row 2, wherever the table has put it on the screen.
    page$hover("#dtc tbody tr[data-hovercraft-tip-id='dtc/row/2'] td:nth-child(1)")
    expect_true(page$wait_reported("dtc_hint", "v.state === 'shown'"))
    got <- page$reported("dtc_hint")
    expect_identical(got$target, "row")
    expect_null(got$column)
    expect_identical(got$row, 2L)
    expect_identical(got$id, "dtc/row/2")
    expect_match(got$text, "^Foxes")
    page$mouse(5, 5, FALSE)
  })

  it("lists several pins oldest first, and removes the one that was closed", {
    page$evaluate("document.documentElement.setAttribute('data-hovercraft-tip-pins', '3'); 0")
    withr::defer(page$evaluate("document.documentElement.removeAttribute('data-hovercraft-tip-pins'); 0"))
    # The table was sorted by wins, descending: pin the data rows in the order 2, 1, 3.
    for (i in 1:3) {
      pin_row(rows_in("dtc"), c("Foxes", "Owls", "Hares")[[i]])
      expect_true(page$wait_reported("dtc_pinned", sprintf("v.length === %d", i)))
    }
    expect_true(page$wait_reported("dtc_pinned", "v.length === 3"))
    rows <- vapply(page$reported("dtc_pinned"), function(a) a$row, 0L)
    expect_identical(rows, c(2L, 1L, 3L))
    expect_setequal(open_pins(), c("dtc/row/2", "dtc/row/1", "dtc/row/3"))
    page$evaluate(sprintf("window.%s.close(%s); 0", global, row_of(rows_in("dtc"), "Owls")))
    expect_true(page$wait_reported("dtc_pinned", "v.length === 2"))
    expect_identical(vapply(page$reported("dtc_pinned"), function(a) a$row, 0L), c(2L, 3L))
    # `_pin` is the event that just happened: the close of row 1.
    expect_identical(page$reported("dtc_pin")$state, "closed")
    expect_identical(page$reported("dtc_pin")$row, 1L)
    close_all()
    expect_true(page$wait_reported("dtc_pinned", "v.length === 0"))
  })

  it("closes only the pin of a row that is given another data row, when several pins are allowed", {
    expect_true(page$wait_until(sprintf("document.querySelectorAll('%s').length === 3", rows_in("rt2", "rt"))))
    page$evaluate("document.documentElement.setAttribute('data-hovercraft-tip-pins', '3'); 0")
    withr::defer(page$evaluate("document.documentElement.removeAttribute('data-hovercraft-tip-pins'); 0"))
    pin_row(rows_in("rt2", "rt"), "A")
    expect_true(page$wait_reported("rt2_pinned", "v.length === 1"))
    pin_row(rows_in("rt2", "rt"), "B")
    expect_true(page$wait_reported("rt2_pinned", "v.length === 2"))
    expect_setequal(open_pins(), c("rt2/row/1", "rt2/row/2"))
    # React gives the first row's element another data row (a sort does this to a reactable table, but a click on the
    # header would be a click away, which closes every pin): the table script re-marks the element, and that closes the
    # pin made for the old row. The pin of the second row stays.
    page$evaluate(sprintf(
      "(() => {
        const el = %s;
        const id = { attribute: 'data-hovercraft-tip-id', base: 'rt2', target: 'row' };
        HovercraftTable.mark(el, { attrs: {}, record: 'data-hovercraft-tip-record', needs: [], id }, null, 3);
      })()",
      row_of(rows_in("rt2", "rt"), "A")
    ))
    expect_true(page$wait_reported("rt2_pinned", "v.length === 1"))
    page$hold(0.5) # the count must stay at one: a second report, were it coming, has no signal to wait for
    expect_identical(length(page$reported("rt2_pinned")), 1L)
    expect_identical(page$reported("rt2_pinned")[[1]]$row, 2L)
    expect_identical(open_pins(), "rt2/row/2")
    expect_identical(page$reported("rt2_pin")$state, "closed")
    expect_identical(page$reported("rt2_pin")$row, 1L)
    close_all()
    expect_true(page$wait_reported("rt2_pinned", "v.length === 0"))
  })

  it("keeps the list of pins true when Shiny draws the table again while a row is pinned", {
    pin_row(rows_in("dtc"), "Owls")
    expect_true(page$wait_reported("dtc_pinned", "v.length === 1"))
    page$evaluate("window.oldRow = document.querySelector('[data-hovercraft-tip-open=pin]'); 0")
    page$evaluate("Shiny.setInputValue('rerender', Date.now(), { priority: 'event' })")
    expect_true(page$wait_until("window.oldRow.isConnected === false"))
    expect_true(page$wait_until("document.querySelector('[data-hovercraft-tip-open=pin]')?.isConnected === true"))
    page$settle()
    # What the server believes is pinned is what the page holds: the same count, the same row.
    held <- open_pins()
    expect_length(held, 1L)
    expect_true(page$wait_reported("dtc_pinned", "v.length === 1"))
    expect_identical(sprintf("dtc/row/%d", page$reported("dtc_pinned")[[1]]$row), held)
    close_all()
    expect_true(page$wait_reported("dtc_pinned", "v.length === 0"))
  })

  it("names the inputs by the plain id in a module, for DT, DT in server mode and reactable", {
    expect_true(page$wait_until(sprintf("document.querySelectorAll('%s').length === 6", rows_in("m-dt"))))
    expect_true(page$wait_until(sprintf("document.querySelectorAll('%s').length >= 6", rows_in("m-rt", "rt"))))
    expect_true(page$wait_until(sprintf("document.querySelectorAll('%s').length === 3", rows_in("m-dts"))))
    for (case in list(
      list(scope = rows_in("m-dt"), input = "m-tips", team = "Foxes", row = 2L),
      list(scope = rows_in("m-dts"), input = "m-tips_srv", team = "Hares", row = 3L),
      list(scope = rows_in("m-rt", "rt"), input = "m-rtips", team = "Owls", row = 1L)
    )) {
      pin_row(case$scope, case$team)
      expect_true(page$wait_reported(paste0(case$input, "_pin"), "v.state === 'pinned'"), info = case$input)
      expect_identical(page$reported(paste0(case$input, "_pin"))$row, case$row, info = case$input)
      expect_identical(page$reported(paste0(case$input, "_pinned"))[[1]]$row, case$row, info = case$input)
      close_all()
      expect_true(page$wait_reported(paste0(case$input, "_pinned"), "v.length === 0"), info = case$input)
    }
    # No input of the plain, un-prefixed name was made: the prefix was added in the module's own render function.
    for (name in c("tips_pin", "tips_srv_pin", "rtips_pin")) {
      expect_null(page$evaluate(sprintf("Shiny.shinyapp.$inputValues['%s']", name)), info = name)
    }
  })

  it("writes the table, the target, the data row and the column's name for a tip that sends events", {
    mark <- function(target, row) {
      page$evaluate(sprintf(
        "(() => {
          const el = document.createElement('td');
          document.body.append(el);
          const id = { attribute: 'data-x-id', base: 'scores', target: '%s', column: 2, name: 'wins', address: 'data-x-address' };
          HovercraftTable.mark(el, { attrs: {}, record: 'data-x-record', needs: [], id }, null, %s);
          const got = el.getAttribute('data-x-address');
          el.remove();
          return got;
        })()",
        target,
        row
      ))
    }
    expect_identical(mark("row", 3), "{\"table\":\"scores\",\"target\":\"row\",\"row\":3,\"column\":null}")
    expect_identical(mark("cell", 3), "{\"table\":\"scores\",\"target\":\"cell\",\"row\":3,\"column\":\"wins\"}")
    expect_identical(
      mark("header", "null"),
      "{\"table\":\"scores\",\"target\":\"header\",\"row\":null,\"column\":\"wins\"}"
    )
    # A row that is not known yet has no address.
    expect_null(mark("cell", "null"))
  })

  it("does not warn about a table that draws a copy of its header, or about a table drawn again", {
    expect_true(page$wait_until("document.querySelectorAll('#dtx .dataTables_scrollHead').length === 1"))
    expect_identical(page$evaluate("window.warned.filter((w) => /two (tables|tagged)/.test(w)).length"), 0L)
    expect_identical(page$problems(), character())
  })
})

describe("the page's warnings about an id used twice", {
  app <- start_app(
    function() {
      scores <- data.frame(team = c("Owls", "Foxes"), wins = c(7, 5))
      tipped <- function(widget) {
        tip_table(widget, rows = tip_spec("{{ team }}", pin = TRUE, events = "pin"), id = "same")
      }
      ui <- bslib::page_fluid(
        tip_on(shiny::actionButton("one", "One"), "First", id = "dupe"),
        tip_on(shiny::actionButton("two", "Two"), "Second", id = "dupe"),
        tip_on(shiny::actionButton("three", "Three"), "Third", id = "alone"),
        DT::DTOutput("a"),
        DT::DTOutput("b")
      )
      server <- function(input, output, session) {
        output$a <- DT::renderDT(tipped(DT::datatable(scores, rownames = FALSE)), server = FALSE)
        output$b <- DT::renderDT(tipped(DT::datatable(scores, rownames = FALSE)), server = FALSE)
      }
      shiny::shinyApp(ui, server)
    }
  )
  warns <- paste(
    "window.warned = [];",
    "const original = console.warn.bind(console);",
    "console.warn = (...args) => { window.warned.push(args.join(' ')); original(...args); };"
  )
  page <- local_page(app$url, environment(), init = warns)

  it("warns once on the console about two tagged items with one tip id, and not about a lone one", {
    expect_true(app$proc$is_alive(), info = paste(app$proc$read_error(), collapse = "\n"))
    expect_true(page$wait_until("window.warned.some((w) => w.includes('two tagged items'))"))
    warned <- jsonlite::fromJSON(page$evaluate("JSON.stringify(window.warned.filter((w) => w.includes('two tagged')))"))
    expect_length(warned, 1L)
    expect_match(warned, "\"dupe\"", fixed = TRUE)
    expect_false(any(grepl("alone", warned)))
  })

  it("warns once about two tables with one id", {
    expect_true(page$wait_until("window.warned.some((w) => w.includes('two tables'))"))
    warned <- jsonlite::fromJSON(page$evaluate("JSON.stringify(window.warned.filter((w) => w.includes('two tables')))"))
    expect_length(warned, 1L)
    expect_match(warned, "\"same\"", fixed = TRUE)
  })
})
