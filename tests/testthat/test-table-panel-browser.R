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

# tip_spec(to = "in_place") in real DT (client and server mode) and reactable tables: a row's pinned tip is drawn as a panel
# under its row, follows its row through a sort, closes with it when the row leaves the page, leaves the widget's own
# inputs alone, shares the pin limit, and has its own Escape. The pins are made by the pin key with the focus on the row.
describe("a row's pinned tip drawn in place", {
  scores <- data.frame(
    team = c("Owls", "Foxes", "Hares", "Lynx", "Voles", "Moles"),
    wins = c(7, 5, 2, 9, 1, 4)
  )
  app <- start_app(
    function(scores) {
      row <- function() {
        tip_spec(
          "**{{ team }}**: {{ wins | number:0 }} wins",
          more = tip_more(
            tip_template(tip_text("The long form of {{ team }}."), tip_output("live")),
            on = "button"
          ),
          pin = TRUE,
          events = "pin",
          to = "in_place"
        )
      }
      tipped <- function(widget, id, max_pins = NULL) tip_table(widget, rows = row(), id = id, max_pins = max_pins)
      ui <- bslib::page_fluid(
        use_hovercraft(max_pins = 3),
        shiny::actionButton("rerender", "Draw again"),
        shiny::actionButton("fresh", "New data"),
        shiny::actionButton("replace", "Replace data"),
        DT::DTOutput("dtp"),
        DT::DTOutput("dtc"),
        DT::DTOutput("dts"),
        DT::DTOutput("dtm"),
        DT::DTOutput("dtx"),
        reactable::reactableOutput("rt"),
        shiny::textOutput("live"),
        shiny::verbatimTextOutput("seen"),
        shiny::textOutput("rsel")
      )
      server <- function(input, output, session) {
        output$live <- shiny::renderText("Live output text")
        output$dtc <- DT::renderDT(
          {
            input$rerender
            tipped(DT::datatable(scores, rownames = FALSE, options = list(pageLength = 6)), "dtc")
          },
          server = FALSE
        )
        output$dts <- DT::renderDT({
          tipped(DT::datatable(scores, rownames = FALSE, options = list(pageLength = 3)), "dts")
        })
        proxy <- DT::dataTableProxy("dts")
        shiny::observeEvent(input$replace, DT::replaceData(proxy, scores[c(4, 5, 6), ], rownames = FALSE))
        output$dtm <- DT::renderDT(
          tipped(DT::datatable(scores, rownames = FALSE, options = list(pageLength = 6)), "dtm", max_pins = 1),
          server = FALSE
        )
        # The long form shows on the pin: a pinned tip that grows from the hint, which a panel must not do.
        output$dtp <- DT::renderDT(
          tip_table(
            DT::datatable(scores, rownames = FALSE, options = list(pageLength = 6)),
            rows = tip_spec(
              "**{{ team }}**: {{ wins | number:0 }} wins",
              more = tip_more("The long form of {{ team }}.", on = "pin"),
              pin = TRUE,
              to = "in_place"
            ),
            id = "dtp"
          ),
          server = FALSE
        )
        wide <- cbind(scores, setNames(lapply(1:10, function(i) scores$wins + i), paste0("col", 1:10)))
        output$dtx <- DT::renderDT(
          tipped(DT::datatable(wide, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 3)), "dtx"),
          server = FALSE
        )
        output$rt <- reactable::renderReactable(
          tipped(reactable::reactable(scores, defaultPageSize = 3, selection = "single", onClick = "select"), "rt")
        )
        shiny::observeEvent(input$fresh, reactable::updateReactable("rt", data = scores[c(4, 5, 6, 1), ]))
        output$rsel <- shiny::renderText(paste("selected:", toString(reactable::getReactableState("rt", "selected"))))
        output$seen <- shiny::renderText({
          everything <- shiny::reactiveValuesToList(input)
          at <- grepl("_(pin|pinned|rows_selected|cell_clicked|row_last_clicked)$", names(everything))
          jsonlite::toJSON(everything[at][order(names(everything[at]))], auto_unbox = TRUE, null = "null")
        })
      }
      shiny::shinyApp(ui, server)
    },
    scores = scores
  )
  page <- local_page(app$url, environment())
  css <- tip_names()$css
  pin_sel <- sprintf(".%s.%s-pin:not([hidden])", css, css)
  seen <- function(name) {
    all <- jsonlite::fromJSON(page$evaluate("document.getElementById('seen').textContent"), simplifyVector = FALSE)
    all[[name]]
  }
  wait_seen <- function(name, test) {
    page$wait_until(sprintf(
      "(() => { try { const v = JSON.parse(document.getElementById('seen').textContent)[%s]; return !!(%s); } catch (e) { return false; } })()",
      jsonlite::toJSON(name, auto_unbox = TRUE),
      test
    ))
  }
  count <- function(selector) page$evaluate(sprintf("document.querySelectorAll('%s').length", selector))
  panels <- function(scope) count(sprintf("%s [data-hovercraft-panel], %s[data-hovercraft-panel]", scope, scope))
  dt_row <- function(id, n) sprintf("#%s tbody tr[data-hovercraft-tip-id=\"%s/row/%d\"]", id, id, n)
  rt_row <- function(n) sprintf("#rt [role=row][data-hovercraft-tip-id=\"rt/row/%d\"]", n)
  # Pins the row by the pin key, with the focus on it, as a keyboard reader does.
  pin_key <- function(selector) {
    # The table's keyboard layer gives a row its tab stop a frame after the row is drawn.
    expect_true(page$wait_until(sprintf(
      "document.querySelector(%s)?.hasAttribute('tabindex') === true",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    )))
    page$evaluate(sprintf("document.querySelector(%s).focus()", jsonlite::toJSON(selector, auto_unbox = TRUE)))
    page$press("p", wait = 0.2)
  }
  after_row <- function(selector) {
    page$evaluate(sprintf(
      "(() => { const r = document.querySelector(%s); return r?.nextElementSibling?.hasAttribute('data-hovercraft-panel') === true; })()",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    ))
  }
  open_pins <- function() {
    as.character(jsonlite::fromJSON(page$evaluate(
      "JSON.stringify([...document.querySelectorAll('[data-hovercraft-tip-open=pin]')].map((n) => n.getAttribute('data-hovercraft-tip-id')))"
    )))
  }
  close_all <- function() {
    page$evaluate("window.Hovercraft.close(); 0")
    expect_true(page$wait_until("document.querySelectorAll('[data-hovercraft-panel]').length === 0"))
  }
  poke <- function(id) page$evaluate(sprintf("Shiny.setInputValue('%s', Date.now(), { priority: 'event' })", id))

  it("starts, and draws the rows with a pin, the panel mark and no panel", {
    expect_true(app$proc$is_alive(), info = paste(app$proc$read_error(), collapse = "\n"))
    expect_true(page$wait_until("document.querySelectorAll('#dtc tbody tr[data-hovercraft-tip-id]').length === 6"))
    expect_true(page$wait_until("document.querySelectorAll('#rt [role=row][data-hovercraft-tip-id]').length === 3"))
    expect_identical(
      page$evaluate("document.querySelector('#dtc tbody tr').hasAttribute('data-hovercraft-table-panel')"),
      TRUE
    )
    expect_identical(count("[data-hovercraft-panel]"), 0L)
  })

  it("draws the pinned tip of a DT row as a panel row right under it, a region, with the row marked open", {
    pin_key(dt_row("dtc", 3))
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", pin_sel)))
    expect_true(after_row(dt_row("dtc", 3)))
    expect_identical(page$evaluate("document.querySelector('[data-hovercraft-panel]').tagName"), "TR")
    expect_identical(page$evaluate(sprintf("document.querySelector('%s').getAttribute('role')", pin_sel)), "region")
    expect_identical(page$evaluate(sprintf("document.querySelector('%s').dataset.side", pin_sel)), "in")
    expect_match(page$evaluate(sprintf("document.querySelector('%s').textContent", pin_sel)), "Hares")
    expect_identical(
      page$evaluate(sprintf("document.querySelector('%s').getAttribute('aria-expanded')", dt_row("dtc", 3))),
      "true"
    )
    expect_identical(page$evaluate("document.querySelector('[data-hovercraft-panel] td').colSpan"), 2L)
    expect_true(wait_seen("dtc_pinned", "v.length === 1 && v[0].row === 3"))
    expect_identical(seen("dtc_pin")$state, "pinned")
  })

  it("keeps the panel under the same data row when DT sorts, with the pin open and the inputs unchanged", {
    page$evaluate("$('#dtc table').DataTable().order([1, 'desc']).draw(); 0")
    expect_true(page$wait_until("document.querySelector('#dtc tbody td').textContent.startsWith('Lynx')"))
    expect_true(page$wait_until(sprintf(
      "document.querySelector('%s')?.nextElementSibling?.hasAttribute('data-hovercraft-panel') === true",
      dt_row("dtc", 3)
    )))
    expect_identical(count("[data-hovercraft-panel]"), 1L)
    expect_identical(open_pins(), "dtc/row/3")
    expect_match(page$evaluate(sprintf("document.querySelector('%s').textContent", pin_sel)), "Hares")
    expect_identical(length(seen("dtc_pinned")), 1L)
    expect_identical(seen("dtc_pin")$state, "pinned")
  })

  it("changes the panel's colspan when a column is hidden and shown, and keeps the focus in the panel through a sort", {
    colspan <- function() page$evaluate("document.querySelector('[data-hovercraft-panel] td').colSpan")
    expect_identical(colspan(), 2L)
    page$evaluate("$('#dtc table').DataTable().column(1).visible(false); 0")
    expect_true(page$wait_until("document.querySelector('[data-hovercraft-panel] td').colSpan === 1"))
    page$evaluate("$('#dtc table').DataTable().column(1).visible(true); 0")
    expect_true(page$wait_until("document.querySelector('[data-hovercraft-panel] td').colSpan === 2"))
    # The reader is in the panel; the table sorts; the focus is still in the panel, not on the page.
    page$evaluate(sprintf("document.querySelector('%s').focus()", pin_sel))
    expect_identical(
      page$evaluate("document.querySelector('[data-hovercraft-panel]').contains(document.activeElement)"),
      TRUE
    )
    page$evaluate("$('#dtc table').DataTable().order([1, 'asc']).draw(); 0")
    expect_true(page$wait_until("document.querySelector('#dtc tbody td').textContent.startsWith('Voles')"))
    expect_true(page$wait_until(
      "document.querySelector('[data-hovercraft-panel]')?.contains(document.activeElement) === true"
    ))
    page$evaluate("$('#dtc table').DataTable().order([1, 'desc']).draw(); 0")
    expect_true(page$wait_until("document.querySelector('#dtc tbody td').textContent.startsWith('Lynx')"))
  })

  it("leaves DT's own inputs alone for a click inside the panel, and does not close for a click away", {
    expect_null(seen("dtc_rows_selected"))
    page$evaluate(sprintf("document.querySelector('%s').click(); 0", pin_sel))
    page$evaluate("document.querySelector('[data-hovercraft-panel] td').click(); 0")
    page$evaluate(
      "document.querySelector('[data-hovercraft-panel] td').dispatchEvent(new MouseEvent('dblclick', { bubbles: true })); 0"
    )
    Sys.sleep(0.5)
    expect_null(seen("dtc_rows_selected"))
    # DT says `list()` for a click it has not had.
    expect_length(seen("dtc_cell_clicked"), 0L)
    expect_null(seen("dtc_row_last_clicked"))
    # A click away (on the page, outside the table) does not close a panel; it is a panel, not a floating tip.
    page$mouse(5, 5, click = TRUE)
    Sys.sleep(0.5)
    expect_identical(open_pins(), "dtc/row/3")
    expect_identical(count("[data-hovercraft-panel]"), 1L)
    # The pin key on the open row does nothing: no toggle.
    pin_key(dt_row("dtc", 3))
    Sys.sleep(0.4)
    expect_identical(open_pins(), "dtc/row/3")
  })

  it("opens the long form with the More button inside the panel, in flow, and Escape on the button closes its own panel", {
    strip <- "[data-hovercraft-panel] .hovercraft-more-strip-in button"
    expect_identical(count(strip), 1L)
    expect_identical(
      page$evaluate(sprintf("getComputedStyle(document.querySelector('%s').parentElement).position", strip)),
      "static"
    )
    page$evaluate(sprintf("document.querySelector('%s').click(); 0", strip))
    expect_true(page$wait_until(sprintf(
      "document.querySelector('%s').textContent.includes('long form of Hares')",
      pin_sel
    )))
    expect_true(page$wait_until(sprintf(
      "document.querySelector('%s').textContent.includes('Live output text')",
      pin_sel
    )))
    expect_identical(
      page$evaluate(sprintf("document.querySelector('%s').getAttribute('aria-expanded')", strip)),
      "true"
    )
    # The tip drew again and is still the same pin, in its panel.
    expect_identical(count("[data-hovercraft-panel]"), 1L)
    page$evaluate(sprintf("document.querySelector('%s').focus()", strip))
    page$press("Escape", wait = 0.2)
    expect_true(page$wait_until("document.querySelectorAll('[data-hovercraft-panel]').length === 0"))
    expect_identical(open_pins(), character())
    expect_identical(
      page$evaluate(sprintf("document.activeElement === document.querySelector('%s')", dt_row("dtc", 3))),
      TRUE
    )
    # The row no longer says it is expanded.
    expect_null(page$evaluate(sprintf("document.querySelector('%s').getAttribute('aria-expanded')", dt_row("dtc", 3))))
  })

  it("closes the pin as gone when a filter hides the row, takes the panel away, and reopens nothing", {
    pin_key(dt_row("dtc", 3))
    expect_true(wait_seen("dtc_pin", "v.state === 'pinned'"))
    page$evaluate("$('#dtc table').DataTable().search('Voles').draw(); 0")
    expect_true(wait_seen("dtc_pin", "v.state === 'closed'"))
    expect_identical(seen("dtc_pin")$reason, "gone")
    expect_true(page$wait_until("document.querySelectorAll('[data-hovercraft-panel]').length === 0"))
    page$evaluate("$('#dtc table').DataTable().search('').draw(); 0")
    Sys.sleep(0.6)
    expect_identical(count("[data-hovercraft-panel]"), 0L)
    expect_identical(open_pins(), character())
    expect_identical(length(seen("dtc_pinned")), 0L)
  })

  it("keeps a pin on its row in DT's server mode through a sort, and closes it as gone on another page", {
    pin_key(dt_row("dts", 3))
    expect_true(wait_seen("dts_pin", "v.state === 'pinned'"))
    expect_true(after_row(dt_row("dts", 3)))
    # Sorted by wins, ascending, the page shows Voles, Hares, Moles: Hares is data row 3 and has moved up one place.
    page$evaluate("$('#dts table').DataTable().order([1, 'asc']).draw(); 0")
    expect_true(page$wait_until("document.querySelector('#dts tbody td').textContent.startsWith('Voles')"))
    expect_true(page$wait_until(sprintf(
      "document.querySelector('%s')?.nextElementSibling?.hasAttribute('data-hovercraft-panel') === true",
      dt_row("dts", 3)
    )))
    expect_identical(count("[data-hovercraft-panel]"), 1L)
    expect_identical(open_pins(), "dts/row/3")
    page$evaluate("$('#dts table').DataTable().page(1).draw(false); 0")
    expect_true(wait_seen("dts_pin", "v.state === 'closed'"))
    expect_identical(seen("dts_pin")$reason, "gone")
    expect_true(page$wait_until("document.querySelectorAll('[data-hovercraft-panel]').length === 0"))
    page$evaluate("$('#dts table').DataTable().order([0, 'asc']).page(0).draw(false); 0")
    expect_true(page$wait_until("document.querySelector('#dts tbody td').textContent.startsWith('Foxes')"))
  })

  it("re-seats the panel in the new row when DT::replaceData() gives the table new values, with one panel", {
    pin_key("#dts tbody tr:nth-child(1)")
    expect_true(wait_seen("dts_pin", "v.state === 'pinned'"))
    first_id <- page$evaluate(
      "document.querySelector('#dts tbody tr:nth-child(1)').getAttribute('data-hovercraft-tip-id')"
    )
    expect_identical(open_pins(), first_id)
    poke("replace")
    expect_true(page$wait_until("document.querySelector('#dts').textContent.includes('Lynx')"))
    expect_true(page$wait_until(sprintf(
      "document.querySelector('[data-hovercraft-tip-id=\"%s\"]')?.nextElementSibling?.hasAttribute('data-hovercraft-panel') === true",
      first_id
    )))
    expect_identical(count("#dts [data-hovercraft-panel]"), 1L)
    expect_identical(open_pins(), first_id)
    close_all()
  })

  it("survives Shiny drawing a DT again while open: one panel, in the new row, and its output still bound", {
    page$evaluate("$('#dtc table').DataTable().order([0, 'asc']).draw(); 0")
    pin_key(dt_row("dtc", 2))
    expect_true(wait_seen("dtc_pin", "v.state === 'pinned'"))
    page$evaluate("document.querySelector('[data-hovercraft-panel] .hovercraft-more-strip-in button').click(); 0")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('%s').textContent.includes('Live output text')",
      pin_sel
    )))
    page$evaluate(sprintf("window.oldRow = document.querySelector('%s'); 0", dt_row("dtc", 2)))
    poke("rerender")
    expect_true(page$wait_until("window.oldRow.isConnected === false"))
    expect_true(page$wait_until(sprintf(
      "document.querySelector('%s')?.nextElementSibling?.hasAttribute('data-hovercraft-panel') === true",
      dt_row("dtc", 2)
    )))
    expect_identical(count("#dtc [data-hovercraft-panel]"), 1L)
    expect_identical(open_pins(), "dtc/row/2")
    # The new row starts short: More opens the long form again, and the output in it is bound and shows its text.
    page$evaluate("document.querySelector('[data-hovercraft-panel] .hovercraft-more-strip-in button').click(); 0")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('%s').textContent.includes('Live output text')",
      pin_sel
    )))
    expect_identical(
      page$evaluate("document.querySelectorAll('[data-hovercraft-panel] .shiny-bound-output').length"),
      1L
    )
    close_all()
  })

  it("counts an in-place pin in the table's own limit: the second pin replaces the first, and one panel is left", {
    pin_key(dt_row("dtm", 1))
    expect_true(wait_seen("dtm_pin", "v.state === 'pinned' && v.row === 1"))
    pin_key(dt_row("dtm", 2))
    expect_true(wait_seen("dtm_pin", "v.state === 'pinned' && v.row === 2"))
    expect_true(page$wait_until("document.querySelectorAll('#dtm [data-hovercraft-panel]').length === 1"))
    expect_identical(open_pins(), "dtm/row/2")
    expect_true(after_row(dt_row("dtm", 2)))
    expect_true(wait_seen("dtm_pinned", "v.length === 1 && v[0].row === 2"))
    close_all()
  })

  it("gives a Shiny output to the newest of two panels, and the older one says it is shown in another tip", {
    for (n in c(1, 2)) {
      pin_key(dt_row("dtc", n))
      expect_true(page$wait_until(sprintf("document.querySelectorAll('[data-hovercraft-panel]').length === %d", n)))
      page$evaluate(sprintf(
        "document.querySelector('%s').nextElementSibling.querySelector('.hovercraft-more-strip-in button').click(); 0",
        dt_row("dtc", n)
      ))
      expect_true(page$wait_until(sprintf(
        "document.querySelector('%s').nextElementSibling.textContent.includes('The long form of')",
        dt_row("dtc", n)
      )))
    }
    expect_identical(count("[data-hovercraft-panel]"), 2L)
    newest <- page$evaluate(sprintf("document.querySelector('%s').nextElementSibling.textContent", dt_row("dtc", 2)))
    oldest <- page$evaluate(sprintf("document.querySelector('%s').nextElementSibling.textContent", dt_row("dtc", 1)))
    expect_match(newest, "Live output text", fixed = TRUE)
    expect_match(oldest, "Shown in another tip", fixed = TRUE)
    # Each More button is its own: the older panel's button still works, and its Escape closes its own panel only.
    page$evaluate(sprintf(
      "document.querySelector('%s').nextElementSibling.querySelector('.hovercraft-more-strip-in button').focus(); 0",
      dt_row("dtc", 1)
    ))
    page$press("Escape", wait = 0.2)
    expect_true(page$wait_until("document.querySelectorAll('[data-hovercraft-panel]').length === 1"))
    expect_identical(open_pins(), "dtc/row/2")
    close_all()
  })

  it("opens a reactable row's pinned tip as a panel in the row's group, with the roles of a table", {
    pin_key(rt_row(2))
    expect_true(wait_seen("rt_pin", "v.state === 'pinned'"))
    expect_true(after_row(rt_row(2)))
    expect_identical(
      page$evaluate(
        "document.querySelector('#rt [data-hovercraft-panel]').parentElement.className.includes('rt-tr-group')"
      ),
      TRUE
    )
    expect_identical(page$evaluate("document.querySelector('#rt [data-hovercraft-panel]').getAttribute('role')"), "row")
    expect_identical(
      page$evaluate("document.querySelector('#rt [data-hovercraft-panel] > div').getAttribute('role')"),
      "cell"
    )
    expect_identical(
      page$evaluate(sprintf("document.querySelector('%s').getAttribute('aria-expanded')", rt_row(2))),
      "true"
    )
    expect_identical(page$evaluate(sprintf("document.querySelector('%s').getAttribute('role')", pin_sel)), "region")
    # The widget's own selection is as it was: nothing is selected.
    expect_identical(page$evaluate("document.getElementById('rsel').textContent"), "selected: ")
    # The same after a click inside the panel.
    page$evaluate("document.querySelector('#rt [data-hovercraft-panel] .hc-tip').click(); 0")
    page$evaluate("document.querySelector('#rt [data-hovercraft-panel]').click(); 0")
    Sys.sleep(0.6)
    expect_identical(page$evaluate("document.getElementById('rsel').textContent"), "selected: ")
  })

  it("keeps a Shiny output bound in a reactable panel across a close and a reopen", {
    for (round in 1:2) {
      pin_key(rt_row(1))
      expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", pin_sel)))
      page$evaluate("document.querySelector('#rt [data-hovercraft-panel] .hovercraft-more-strip-in button').click(); 0")
      expect_true(page$wait_until(sprintf(
        "document.querySelector('%s').textContent.includes('Live output text')",
        pin_sel
      )))
      close_all()
    }
  })

  it("closes the pin and takes the panel away when reactable shows other rows in its nodes", {
    page$evaluate(
      "[...document.querySelectorAll('#rt .rt-next-button, #rt [aria-label=\"Next page\"]')][0]?.click(); 0"
    )
    expect_true(wait_seen("rt_pin", "v.state === 'closed'"))
    expect_identical(seen("rt_pin")$reason, "host")
    expect_true(page$wait_until("document.querySelectorAll('[data-hovercraft-panel]').length === 0"))
    expect_identical(open_pins(), character())
    page$evaluate(
      "[...document.querySelectorAll('#rt .rt-prev-button, #rt [aria-label=\"Previous page\"]')][0]?.click(); 0"
    )
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", rt_row(1))))
  })

  it("keeps a reactable panel on the same row number when the table is given new data, with the new values", {
    pin_key(rt_row(1))
    expect_true(wait_seen("rt_pin", "v.state === 'pinned'"))
    expect_match(page$evaluate(sprintf("document.querySelector('%s').textContent", pin_sel)), "Owls")
    poke("fresh")
    expect_true(page$wait_until(sprintf("document.querySelector('%s')?.textContent.includes('Lynx')", pin_sel)))
    expect_true(after_row(rt_row(1)))
    expect_identical(count("#rt [data-hovercraft-panel]"), 1L)
    expect_identical(open_pins(), "rt/row/1")
    close_all()
  })

  it("keeps a panel inside a phone's width, and takes the page's colours in the dark", {
    page$evaluate("window.scrollTo(0, 0); 0")
    pin_key(dt_row("dtc", 1))
    expect_true(wait_seen("dtc_pin", "v.state === 'pinned'"))
    page$viewport(375, 700)
    expect_true(page$wait_until(sprintf("document.querySelector('%s').getBoundingClientRect().right <= 376", pin_sel)))
    expect_identical(
      page$evaluate(
        "document.documentElement.scrollWidth <= 376 || document.querySelector('#dtc .dataTables_wrapper') !== null"
      ),
      TRUE
    )
    page$viewport(900, 700)
    background <- function() {
      page$evaluate(sprintf("getComputedStyle(document.querySelector('%s')).backgroundColor", pin_sel))
    }
    light <- background()
    page$evaluate("document.documentElement.setAttribute('data-bs-theme', 'dark'); 0")
    Sys.sleep(0.5)
    expect_false(identical(background(), light))
    page$evaluate("document.documentElement.setAttribute('data-bs-theme', 'light'); 0")
    close_all()
  })

  it("keeps the panel of a DT that scrolls sideways as wide as its visible part, at the left edge, on a phone", {
    page$viewport(375, 700)
    withr::defer(page$viewport(900, 700))
    expect_true(page$wait_until("document.querySelectorAll('#dtx tbody tr[data-hovercraft-tip-id]').length === 3"))
    pin_key(dt_row("dtx", 2))
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", pin_sel)))
    box <- function() {
      jsonlite::fromJSON(page$evaluate(
        "JSON.stringify((() => { const r = document.querySelector('#dtx .hovercraft-panel-box').getBoundingClientRect(); const s = document.querySelector('#dtx .dataTables_scrollBody'); return { left: r.left, right: r.right, scroller: s.clientWidth, scrollWidth: s.scrollWidth }; })())"
      ))
    }
    before <- box()
    expect_gt(before$scrollWidth, before$scroller)
    expect_lte(before$right, 376)
    page$evaluate("document.querySelector('#dtx .dataTables_scrollBody').scrollLeft = 250; 0")
    expect_true(page$wait_until("document.querySelector('#dtx .dataTables_scrollBody').scrollLeft > 100"))
    after <- box()
    expect_equal(after$left, before$left, tolerance = 2)
    expect_equal(after$right, before$right, tolerance = 2)
    close_all()
  })

  it("closes the newest panel on Escape from a row that has no pin of its own, as it does for a floating pin", {
    pin_key(dt_row("dtc", 4))
    expect_true(wait_seen("dtc_pin", "v.state === 'pinned'"))
    page$evaluate(sprintf("document.querySelector('%s').focus()", dt_row("dtc", 5)))
    page$press("Escape", wait = 0.2)
    expect_true(wait_seen("dtc_pin", "v.state === 'closed'"))
    expect_true(page$wait_until("document.querySelectorAll('[data-hovercraft-panel]').length === 0"))
  })

  it("draws no growth from the hint when a long form that shows on the pin is pinned in a panel", {
    page$evaluate(
      "window.morphs = 0; const animate = Element.prototype.animate; Element.prototype.animate = function (frames, ...rest) { if (this.classList?.contains('hc-tip-pin') && Array.isArray(frames) && frames.some((f) => 'width' in f || 'height' in f)) window.morphs += 1; return animate.call(this, frames, ...rest); }; 0"
    )
    page$evaluate("window.scrollTo(0, 0); 0")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('%s').getBoundingClientRect().top > 0",
      dt_row("dtp", 2)
    )))
    # The engine holds a hint back for a moment after a tip closes: rest on the row until one opens.
    for (try in 1:5) {
      page$hover(dt_row("dtp", 2))
      if (page$wait_until("document.querySelector('#dtp [data-hovercraft-tip-open=hint]') !== null", 3)) break
    }
    expect_true(page$wait_until("document.querySelector('#dtp [data-hovercraft-tip-open=hint]') !== null", 1))
    # What the pin button does, without aiming at a button that follows the hint: the table layer makes the panel for the
    # row whose hint is open, and the engine draws the pinned tip into it.
    page$evaluate(
      "(() => { const row = document.querySelector('#dtp [data-hovercraft-tip-open=hint]'); Hovercraft.pin(row, { by: 'reader', via: 'click', into: HovercraftTable.panelFor(row) }); })()"
    )
    # The hint that was pinned (whichever row the pointer rested on) shows its long form at once, in its panel.
    expect_true(page$wait_until(sprintf(
      "document.querySelector('%s')?.textContent.includes('The long form of')",
      pin_sel
    )))
    Sys.sleep(0.5)
    expect_identical(count("#dtp [data-hovercraft-panel]"), 1L)
    expect_match(open_pins(), "^dtp/row/")
    expect_identical(page$evaluate("window.morphs"), 0L)
    page$mouse(5, 5)
    close_all()
  })

  it("has no console error", {
    expect_identical(page$problems(), character())
  })
})
