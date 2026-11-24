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

# tip_table(pin = TRUE, events = ) in a running Shiny app: the pin handle in a row's tip, the pin key, the Shiny inputs
# of a row's id, and what happens to a pinned row when the table sorts, pages, is given new data or is drawn again.
describe("tip_table(pin = TRUE) in a Shiny app", {
  scores <- data.frame(
    team = c("Owls", "Foxes", "Hares", "Lynx", "Voles", "Moles"),
    wins = c(7, 5, 2, 9, 1, 4),
    note = c("Top", NA, "Late", "Best", NA, "New"),
    sql = c("SELECT 1;", "SELECT 2;", "SELECT 3;", "SELECT 4;", "SELECT 5;", "SELECT 6;")
  )
  app <- start_app(
    function(scores) {
      pinned <- function(widget, id) {
        tip_table(
          widget,
          rows = "**{{ team }}**: {{ wins | number:0 }} wins",
          cells = c(note = "Note: {{ note }}"),
          id = id,
          pin = TRUE,
          events = c("hint", "pin", "copy")
        )
      }
      ui <- bslib::page_fluid(
        shiny::actionButton("rerender", "Draw again"),
        shiny::actionButton("fresh", "Reactable new data"),
        shiny::actionButton("replace", "DT new data"),
        DT::DTOutput("dtc"),
        DT::DTOutput("dts"),
        DT::DTOutput("dtq"),
        reactable::reactableOutput("rt"),
        DT::DTOutput("m-dt"),
        shiny::textOutput("m-seen_in"),
        shiny::verbatimTextOutput("seen")
      )
      server <- function(input, output, session) {
        output$dtc <- DT::renderDT(
          {
            input$rerender
            pinned(DT::datatable(scores[1:3], rownames = FALSE, options = list(pageLength = 3)), "dtc")
          },
          server = FALSE
        )
        output$dts <- DT::renderDT({
          pinned(DT::datatable(scores[1:3], rownames = FALSE, options = list(pageLength = 2)), "dts")
        })
        output$dtq <- DT::renderDT(
          {
            tip_table(
              DT::datatable(scores, rownames = FALSE, options = list(pageLength = 2)),
              rows = tip_spec(
                tip_template(tip_title("{{ team }}"), tip_text("Query used"), tip_code("sql", copy = TRUE)),
                more = "{{ wins }} wins this season",
                more_on = "pin"
              ),
              id = "dtq",
              pin = TRUE,
              events = c("pin", "copy")
            )
          },
          server = FALSE
        )
        data <- shiny::reactiveVal(scores[1:3])
        output$rt <- reactable::renderReactable({
          pinned(
            reactable::reactable(
              data(),
              defaultPageSize = 3,
              selection = "single",
              onClick = "select",
              searchable = TRUE
            ),
            "rt"
          )
        })
        shiny::observeEvent(input$page_to, reactable::updateReactable("rt", page = input$page_to))
        shiny::observeEvent(input$fresh, reactable::updateReactable("rt", data = scores[c(4, 5, 6, 1), 1:3]))
        proxy <- DT::dataTableProxy("dts")
        shiny::observeEvent(input$replace, DT::replaceData(proxy, scores[c(4, 5, 6), 1:3], rownames = FALSE))
        shiny::moduleServer("m", function(input, output, session) {
          output$dt <- DT::renderDT(
            {
              pinned(DT::datatable(scores[1:3], rownames = FALSE), session$ns("tips"))
            },
            server = FALSE
          )
          # The module's own input names: the tip id without the module's prefix.
          output$seen_in <- shiny::renderText({
            mine <- shiny::reactiveValuesToList(input)
            mine <- mine[grepl("^tips/row/[0-9]+_tip$", names(mine))]
            paste("inside:", paste(vapply(mine, function(x) x$state, ""), collapse = ","))
          })
        })
        output$seen <- shiny::renderText({
          everything <- shiny::reactiveValuesToList(input)
          at <- grepl("_tip$|_copy$", names(everything))
          state <- lapply(everything[at], function(x) if (is.null(x$state)) x$field else x$state)
          jsonlite::toJSON(state[order(names(state))], auto_unbox = TRUE, null = "null")
        })
      }
      shiny::shinyApp(ui, server)
    },
    scores = scores
  )
  page <- local_page(app$url, environment())
  css <- tip_names()$css
  open_pins <- "[...document.querySelectorAll('[data-hovercraft-tip-open=pin]')].map((n) => n.getAttribute('data-hovercraft-tip-id'))"
  pinned_box <- sprintf(".%s-pin:not([hidden])", css)
  strip <- ".hovercraft-pin-strip-button"
  pins <- function() as.character(jsonlite::fromJSON(page$evaluate(sprintf("JSON.stringify(%s)", open_pins))))
  seen <- function() jsonlite::fromJSON(page$evaluate("document.getElementById('seen').textContent"))
  seen_has <- function(name, value) {
    page$wait_until(sprintf(
      "(() => { try { return JSON.parse(document.getElementById('seen').textContent)[%s] === %s; } catch (e) { return false; } })()",
      jsonlite::toJSON(name, auto_unbox = TRUE),
      jsonlite::toJSON(value, auto_unbox = TRUE)
    ))
  }
  rect <- function(selector) {
    jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => { const r = document.querySelector(%s).getBoundingClientRect(); return JSON.stringify({ left: r.left, top: r.top, right: r.right, bottom: r.bottom, width: r.width, height: r.height }); })()",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    )))
  }
  mouse_at <- function(selector, click = FALSE) {
    box <- rect(selector)
    page$mouse((box$left + box$right) / 2, (box$top + box$bottom) / 2, click)
  }
  view <- function(selector) {
    page$evaluate(sprintf(
      "document.querySelector(%s).scrollIntoView({ block: 'center' })",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    ))
    Sys.sleep(0.3)
  }
  reset <- function() {
    page$press("Escape", wait = 0.2)
    page$evaluate("document.activeElement?.blur(); window.scrollTo(0, 0)")
    page$mouse(5, 5, FALSE)
    Sys.sleep(0.4)
  }
  # Opens the row's hint with the pointer, then presses the handle in the hint's corner with the mouse.
  pin_by_handle <- function(row) {
    view(row)
    page$hover(row)
    expect_true(page$wait_until("document.querySelector('.hovercraft-pin-strip-button') !== null"))
    mouse_at(strip, click = TRUE)
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", pinned_box)))
  }
  # Presses an action button's input from the page without a click: a click would close a pin as a click away.
  poke <- function(id, value = "Date.now()") {
    page$evaluate(sprintf("Shiny.setInputValue('%s', %s, { priority: 'event' })", id, value))
  }
  rows_dt <- function(id) sprintf("#%s tbody tr:nth-child(%d)", id, 1:3)
  rt_row <- function(n) sprintf("#rt [role=row][data-hovercraft-tip-id='rt/row/%d']", n)

  it("starts and draws every table with its rows ready to pin", {
    expect_true(app$proc$is_alive(), info = paste(app$proc$read_error(), collapse = "\n"))
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
    expect_true(page$wait_until(
      "document.querySelectorAll('#dtc tbody tr[data-hovercraft-tip-pin=manual]').length === 3"
    ))
    expect_true(page$wait_until(
      "document.querySelectorAll('#rt [role=row][data-hovercraft-tip-pin=manual]').length === 3"
    ))
    expect_identical(page$evaluate("document.querySelector('#dtc tbody tr').getAttribute('aria-keyshortcuts')"), "P")
  })

  it("pins a DT row with the handle in the corner of its tip, and the row is not selected", {
    reset()
    pin_by_handle(rows_dt("dtc")[[2]])
    expect_identical(pins(), "dtc/row/2")
    expect_true(page$evaluate(sprintf("document.querySelector('%s').textContent.includes('Foxes')", pinned_box)))
    expect_true(seen_has("dtc/row/2_tip", "pinned"))
    expect_true(page$evaluate("document.querySelector('#dtc tbody tr.selected') === null"))
    # The reader's own pin: by the reader, with a click on the handle.
    expect_identical(
      page$evaluate(
        "Shiny.shinyapp.$inputValues['dtc/row/2_tip'].by + '/' + Shiny.shinyapp.$inputValues['dtc/row/2_tip'].via"
      ),
      "reader/click"
    )
    page$press("Escape")
    expect_true(seen_has("dtc/row/2_tip", "closed"))
    expect_identical(pins(), character())
  })

  it("does not pin on a click of the row, and the click still selects it", {
    reset()
    page$click(rows_dt("dtc")[[1]])
    Sys.sleep(0.3)
    expect_identical(pins(), character())
    expect_true(page$wait_until("document.querySelector('#dtc tbody tr.selected') !== null"))
  })

  it("pins by the pin key from the row, and from a cell of the row, and nowhere else", {
    reset()
    page$evaluate("document.querySelector('#dtc tbody tr:nth-child(3)').focus()")
    page$press("p")
    expect_true(page$wait_until("document.querySelector('[data-hovercraft-tip-open=pin]') !== null"))
    expect_identical(pins(), "dtc/row/3")
    expect_true(seen_has("dtc/row/3_tip", "pinned"))
    expect_identical(page$evaluate("Shiny.shinyapp.$inputValues['dtc/row/3_tip'].via"), "key")
    page$press("Escape")
    expect_identical(pins(), character())
    # A cell with a tip of its own, inside a pinnable row: only the row can pin, so the key pins the row's tip.
    page$evaluate("document.querySelector('#dtc tbody tr:nth-child(1) td:nth-child(3)').focus()")
    page$press("p")
    expect_identical(pins(), "dtc/row/1")
    # The cell's own hint may be open too: Escape closes the hint, then the pin.
    for (i in 1:2) {
      if (length(pins())) page$press("Escape")
    }
    expect_identical(pins(), character())
    # No focus on the table: the key does nothing.
    page$evaluate("document.activeElement.blur()")
    page$press("p")
    expect_identical(pins(), character())
  })

  it("keeps a pin on the same row number when Shiny draws the table again, on the new table's row", {
    reset()
    pin_by_handle(rows_dt("dtc")[[1]])
    expect_identical(pins(), "dtc/row/1")
    page$evaluate("window.oldRow = document.querySelector('[data-hovercraft-tip-open=pin]'); 0")
    poke("rerender")
    expect_true(page$wait_until("window.oldRow.isConnected === false"))
    expect_true(page$wait_until(sprintf("document.querySelector('%s')?.textContent.includes('Owls')", pinned_box)))
    expect_identical(pins(), "dtc/row/1")
    expect_true(page$evaluate("document.querySelector('[data-hovercraft-tip-open=pin]').isConnected"))
    page$press("Escape")
    expect_identical(pins(), character())
    pin_by_handle(rows_dt("dtc")[[2]])
    expect_identical(pins(), "dtc/row/2")
    page$press("Escape")
  })

  it("keeps a pin on its row when DT sorts, and closes it when the row leaves the page", {
    reset()
    pin_by_handle(rows_dt("dtc")[[1]])
    expect_identical(pins(), "dtc/row/1")
    page$evaluate("$('#dtc table').DataTable().order([1, 'desc']).draw(); 0")
    expect_true(page$wait_until("document.querySelector('#dtc tbody td').textContent.startsWith('Lynx')"))
    # Owls (data row 1) is now the second row on the page, and it still holds the pin.
    expect_true(page$wait_until(
      "document.querySelector('#dtc tbody tr:nth-child(2)').getAttribute('data-hovercraft-tip-open') === 'pin'"
    ))
    expect_identical(pins(), "dtc/row/1")
    expect_true(page$evaluate(sprintf("document.querySelector('%s').textContent.includes('Owls')", pinned_box)))
    page$evaluate("$('#dtc table').DataTable().search('Voles').draw(); 0")
    expect_true(page$wait_until("document.querySelector('[data-hovercraft-tip-open=pin]') === null"))
    expect_true(seen_has("dtc/row/1_tip", "closed"))
    expect_identical(page$evaluate("Shiny.shinyapp.$inputValues['dtc/row/1_tip'].reason"), "gone")
    page$evaluate("$('#dtc table').DataTable().search('').draw(); 0")
  })

  it("closes a pinned DT row of the server mode when the page changes", {
    reset()
    pin_by_handle(rows_dt("dts")[[1]])
    expect_identical(pins(), "dts/row/1")
    page$evaluate("$('#dts table').DataTable().page(1).draw(false); 0")
    expect_true(page$wait_until("document.querySelector('[data-hovercraft-tip-open=pin]') === null"))
    expect_true(seen_has("dts/row/1_tip", "closed"))
    page$evaluate("$('#dts table').DataTable().page(0).draw(false); 0")
  })

  it("pins a reactable row, keeps the row selection as it was, and closes the pin when the row nodes show other rows", {
    reset()
    pin_by_handle(rt_row(2))
    expect_identical(pins(), "rt/row/2")
    expect_true(seen_has("rt/row/2_tip", "pinned"))
    expect_true(page$evaluate("document.querySelector('#rt .rt-tr.-selected, #rt [aria-selected=true]') === null"))
    # The server moves the table to its second page: React keeps the row nodes and gives them other rows, which must
    # not show under the pin made for the row it was.
    page$evaluate(sprintf(
      "window.sawRow5 = false; new MutationObserver(() => { const box = document.querySelector('%s'); if (box && box.textContent.includes('Voles')) window.sawRow5 = true; }).observe(document.body, { subtree: true, childList: true, characterData: true, attributes: true }); 0",
      pinned_box
    ))
    poke("page_to", "2")
    expect_true(page$wait_until(
      "document.querySelector('#rt [role=row][data-hovercraft-tip-id=\"rt/row/5\"]') !== null"
    ))
    expect_true(seen_has("rt/row/2_tip", "closed"))
    expect_identical(pins(), character())
    expect_false(page$evaluate("window.sawRow5"))
    # The layer closes the pin through the engine's host command, so the engine says who and why that way (a DT row that
    # leaves the page is closed by the engine itself, as "gone" by "data").
    expect_identical(
      page$evaluate("JSON.stringify(Shiny.shinyapp.$inputValues['rt/row/2_tip'])"),
      "{\"state\":\"closed\",\"text\":\"Foxes: 5 wins\",\"by\":\"host\",\"via\":\"command\",\"reason\":\"host\",\"kind\":\"table_row\"}"
    )
    poke("page_to", "1")
    expect_true(page$wait_until(
      "document.querySelector('#rt [role=row][data-hovercraft-tip-id=\"rt/row/2\"]') !== null"
    ))
  })

  it("keeps a pinned reactable row when it is given new data, with the new values in its tip", {
    reset()
    pin_by_handle(rt_row(1))
    expect_identical(pins(), "rt/row/1")
    expect_true(page$evaluate(sprintf("document.querySelector('%s').textContent.includes('Owls')", pinned_box)))
    poke("fresh")
    expect_true(page$wait_until("document.querySelector('#rt').textContent.includes('Lynx')"))
    expect_true(page$wait_until(sprintf("document.querySelector('%s')?.textContent.includes('Lynx')", pinned_box)))
    expect_identical(pins(), "rt/row/1")
    page$press("Escape")
    expect_identical(pins(), character())
  })

  it("keeps a pinned DT row of the server mode when DT::replaceData() gives it new values", {
    reset()
    pin_by_handle(rows_dt("dts")[[2]])
    expect_identical(pins(), "dts/row/2")
    poke("replace")
    expect_true(page$wait_until("document.querySelector('#dts').textContent.includes('Lynx')"))
    expect_true(page$wait_until(sprintf("document.querySelector('%s')?.textContent.includes('Voles')", pinned_box)))
    expect_identical(pins(), "dts/row/2")
    page$press("Escape")
  })

  it("keeps a pin on its row when DT in server mode sorts it to another place on the page", {
    reset()
    # After DT::replaceData() the page shows Lynx (9 wins), Voles (1): sorted by wins, Voles comes first.
    page$evaluate("$('#dts table').DataTable().page(0).draw(false); 0")
    expect_true(page$wait_until(
      "document.querySelector('#dts tbody tr:nth-child(2)[data-hovercraft-tip-id]') !== null"
    ))
    row <- page$evaluate("document.querySelector('#dts tbody tr:nth-child(2)').getAttribute('data-hovercraft-tip-id')")
    team <- page$evaluate(
      "JSON.parse(document.querySelector('#dts tbody tr:nth-child(2)').getAttribute('data-hovercraft-tip-record')).team"
    )
    pin_by_handle(rows_dt("dts")[[2]])
    expect_identical(pins(), row)
    page$evaluate("$('#dts table').DataTable().order([1, 'asc']).draw(); 0")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('#dts tbody tr:nth-child(1)').textContent.startsWith('%s')",
      team
    )))
    expect_true(page$wait_until(
      "document.querySelector('#dts tbody tr:nth-child(1)').getAttribute('data-hovercraft-tip-open') === 'pin'"
    ))
    expect_identical(pins(), row)
    expect_true(page$evaluate(sprintf("document.querySelector('%s').textContent.includes('%s')", pinned_box, team)))
    page$press("Escape")
  })

  it("names the inputs by the row's tip id inside a module, as the module's server reads them", {
    reset()
    expect_true(page$wait_until("document.querySelectorAll('#m-dt tbody tr[data-hovercraft-tip-pin]').length === 6"))
    pin_by_handle("#m-dt tbody tr:nth-child(1)")
    id <- pins()
    expect_match(id, "^m-tips/row/[0-9]+$")
    expect_true(seen_has(paste0(id, "_tip"), "pinned"))
    expect_true(page$wait_until("document.getElementById('m-seen_in').textContent === 'inside: pinned'"))
    page$press("Escape")
    expect_true(seen_has(paste0(id, "_tip"), "closed"))
    expect_true(page$wait_until("document.getElementById('m-seen_in').textContent === 'inside: closed'"))
  })

  it("copies the code in a pinned row's tip, and grows the long form on the pin", {
    reset()
    page$evaluate(
      "Object.defineProperty(window, 'isSecureContext', { value: false }); window.copyFired = 0; document.addEventListener('copy', () => { window.copyFired += 1; }, true)"
    )
    pin_by_handle(rows_dt("dtq")[[1]])
    expect_true(page$wait_until(sprintf("document.querySelector('%s .%s-copy') !== null", pinned_box, css)))
    expect_true(page$wait_until(sprintf(
      "document.querySelector('%s').textContent.includes('7 wins this season')",
      pinned_box
    )))
    Sys.sleep(1)
    mouse_at(sprintf("%s .%s-copy", pinned_box, css), click = TRUE)
    expect_true(seen_has("dtq/row/1_copy", "sql"))
    expect_equal(page$evaluate("window.copyFired"), 1)
    page$press("Escape")
  })

  it("shows a row's tip on a tap and pins it with a tap on the handle, in DT and in reactable", {
    reset()
    hint_box <- sprintf(".%s:not(.%s-pin):not([hidden])", css, css)
    for (row in c(rows_dt("dtq")[[1]], rt_row(1))) {
      view(row)
      box <- rect(row)
      page$touch_tap(box$left + 20, (box$top + box$bottom) / 2)
      expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", hint_box)))
      expect_true(page$wait_until("document.querySelector('.hovercraft-pin-strip-button') !== null"))
      expect_true(page$evaluate(sprintf("document.querySelector('%s') === null", pinned_box)))
      button <- rect(strip)
      page$touch_tap((button$left + button$right) / 2, (button$top + button$bottom) / 2)
      expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", pinned_box)))
      page$touch_off()
      page$press("Escape")
      reset()
    }
  })

  it("keeps the handle at least 24 px and the words at least 11 px on a phone, and the handle reads in light and dark", {
    reset()
    page$viewport(375, 700)
    withr::defer(page$viewport(900, 700))
    # The colours the browser computed (not the ones the code set): rgb or rgba text as numbers.
    rgb <- function(js) as.numeric(strsplit(gsub("[^0-9.,]", "", page$evaluate(js)), ",")[[1]])
    luminance <- function(c) {
      lin <- function(v) if (v <= 0.03928) v / 12.92 else ((v + 0.055) / 1.055)^2.4
      0.2126 * lin(c[[1]] / 255) + 0.7152 * lin(c[[2]] / 255) + 0.0722 * lin(c[[3]] / 255)
    }
    contrast <- function(a, b) {
      hi <- max(luminance(a), luminance(b))
      lo <- min(luminance(a), luminance(b))
      (hi + 0.05) / (lo + 0.05)
    }
    hint_sel <- sprintf(".%s:not(.%s-pin)", css, css)
    backgrounds <- list()
    for (scheme in c("light", "dark")) {
      page$evaluate(sprintf("document.documentElement.setAttribute('data-bs-theme', '%s'); 0", scheme))
      reset()
      view(rows_dt("dtc")[[2]])
      page$hover(rows_dt("dtc")[[2]])
      expect_true(page$wait_until("document.querySelector('.hovercraft-pin-strip-button') !== null"))
      button <- rect(strip)
      expect_gte(button$width, 24)
      expect_gte(button$height, 24)
      expect_gte(
        page$evaluate(sprintf("parseFloat(getComputedStyle(document.querySelector('%s')).fontSize)", hint_sel)),
        11
      )
      Sys.sleep(0.5)
      background <- rgb(sprintf("getComputedStyle(document.querySelector('%s')).backgroundColor", hint_sel))
      glyph <- rgb("getComputedStyle(document.querySelector('.hovercraft-pin-strip-pin'), '::before').backgroundColor")
      expect_length(background, 3L + (length(background) == 4L))
      expect_gte(contrast(glyph[1:3], background[1:3]), 3)
      backgrounds[[scheme]] <- background[1:3]
      expect_lte(rect(hint_sel)$right, 375)
      mouse_at(strip, click = TRUE)
      expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", pinned_box)))
      expect_lte(rect(pinned_box)$right, 375)
      page$press("Escape")
    }
    # The dark page really gave the tip another background: the check above ran against both.
    expect_gt(luminance(backgrounds$light), luminance(backgrounds$dark) + 0.3)
    expect_identical(page$problems(), character())
  })
})
