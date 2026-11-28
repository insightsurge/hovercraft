skip_if_no_browser()

# tip_on(pin = ) and the pin and copy inputs in a running Shiny app: the button in the hint's corner, the pin key, a
# pin that the item's own click never makes, a pin by click, a Copy button, the inputs, a module and a narrow window.
describe("pins in a Shiny app", {
  app <- start_app(function() {
    snippet <- tip_define(
      snippet = tip_template(
        tip_title("{{ label }}"),
        tip_text("Query used"),
        tip_code("sql", language = "sql", copy = TRUE)
      )
    )
    row <- function(id, label, ...) {
      htmltools::tags$div(label, id = id, tabindex = "0", class = "p-2 border", style = "width: 18rem", ...)
    }
    show <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
    module_ui <- function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        tip_on(row(ns("row"), "Module row"), "Module tip", id = ns("m_tip"), pin = TRUE, events = c("hint", "pin")),
        shiny::verbatimTextOutput(ns("seen"))
      )
    }
    module_server <- function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        output$seen <- shiny::renderText(show(list(hint = input$m_tip_hint$state, pin = input$m_tip_pin$state)))
      })
    }
    ui <- bslib::page_fluid(
      snippet,
      htmltools::tags$div(
        style = "padding: 70px 20px; display: flex; flex-direction: column; gap: 26px; align-items: flex-start",
        tip_on(
          row(
            "row_a",
            "Row A",
            onclick = "window.rowClicks = (window.rowClicks || 0) + 1; Shiny.setInputValue('row_click', Date.now())"
          ),
          "Row A says something short.",
          id = "a_tip",
          pin = TRUE,
          events = c("hint", "pin")
        ),
        tip_on(row("row_b", "Row B"), "Row B pins on a click.", id = "b_tip", pin = "click", events = "pin"),
        tip_on(
          row("row_c", "Row C"),
          "Row C is a hint only, with a longer sentence that wraps onto a second line so the corner shows.",
          id = "c_tip"
        ),
        tip_on(
          row("row_d", "Row D"),
          paste(
            "Row D is pinnable and its sentence is long enough to wrap onto more than one line in the hint box,",
            "so the button sits by the first line only."
          ),
          id = "d_tip",
          pin = TRUE
        ),
        tip_on(
          row("row_e", "Row E"),
          template = "snippet",
          record = list(label = "Query", sql = "SELECT 1;"),
          id = "e_tip",
          pin = TRUE,
          events = c("pin", "copy")
        ),
        tip_on(
          row("row_f", "Row F"),
          "Row F slides in.",
          id = "f_tip",
          pin = TRUE,
          style = tip_style(motion = "slide")
        ),
        tip_on(
          row(
            "row_g",
            "Row G",
            tip_on(
              htmltools::tags$span("Cell G", id = "cell_g", style = "margin-left: 4rem", class = "px-1 border"),
              "Cell G is a plain tip.",
              id = "g_cell_tip"
            )
          ),
          "Row G is pinnable.",
          id = "g_tip",
          pin = TRUE
        ),
        htmltools::tags$div(
          style = "display: flex; gap: 12px",
          tip_on(
            htmltools::tags$div(
              "Stack A",
              id = "stack_a",
              tabindex = "0",
              class = "p-2 border",
              style = "width: 8rem"
            ),
            paste(
              "Stack A is a pinned tip whose box is wide and tall enough",
              "to overlap a neighbour's hint in the middle."
            ),
            id = "stack_a_tip",
            pin = TRUE
          ),
          tip_on(
            htmltools::tags$div(
              "Stack B",
              id = "stack_b",
              tabindex = "0",
              class = "p-2 border",
              style = "width: 8rem"
            ),
            paste(
              "Stack B is a hint whose box overlaps the pinned tip next to it",
              "so the pointer's hint stays on top."
            ),
            id = "stack_b_tip",
            pin = TRUE
          )
        ),
        shiny::actionButton("show_a", "Show A"),
        shiny::actionButton("open_modal", "Open modal"),
        htmltools::tags$input(id = "field", type = "text", placeholder = "type here"),
        module_ui("m"),
        shiny::verbatimTextOutput("seen_a"),
        shiny::verbatimTextOutput("seen_b"),
        shiny::verbatimTextOutput("seen_c"),
        shiny::verbatimTextOutput("seen_e"),
        shiny::verbatimTextOutput("seen_modal")
      )
    )
    server <- function(input, output, session) {
      output$seen_a <- shiny::renderText(show(list(
        hint = input$a_tip_hint$state,
        hint_by = input$a_tip_hint$by,
        hint_reason = input$a_tip_hint$reason,
        pin = input$a_tip_pin,
        click = !is.null(input$row_click)
      )))
      output$seen_b <- shiny::renderText(show(input$b_tip_pin))
      output$seen_c <- shiny::renderText(show(list(
        hint = input$c_tip_hint,
        pin = input$c_tip_pin,
        copy = input$c_tip_copy
      )))
      output$seen_e <- shiny::renderText(show(list(
        pin = input$e_tip_pin$state,
        copy = input$e_tip_copy
      )))
      output$seen_modal <- shiny::renderText(show(input$modal_tip_pin$state))
      shiny::observeEvent(input$show_a, tip_show("a_tip"))
      shiny::observeEvent(input$open_modal, {
        shiny::showModal(shiny::modalDialog(
          tip_on(row("row_modal", "Modal row"), "Modal tip", id = "modal_tip", pin = TRUE, events = "pin"),
          title = "A modal",
          easyClose = TRUE
        ))
      })
      module_server("m")
    }
    shiny::shinyApp(ui, server)
  })
  url <- app$url
  page <- local_page(url, environment())
  css <- tip_names()$css
  read <- function(id) page$evaluate(sprintf("document.getElementById('%s').textContent", id))
  view <- function(id) {
    page$evaluate(sprintf("document.getElementById('%s').scrollIntoView({ block: 'center' })", id))
    page$settle()
  }
  strip <- ".hovercraft-pin-strip-button"
  pinned_box <- sprintf(".%s-pin:not([hidden])", css)
  hint_box <- sprintf(".%s:not(.%s-pin):not([hidden])", css, css)
  # Real mouse on the centre of an element, then a press and release: what a reader does.
  mouse_at <- function(selector, click = FALSE) {
    box <- page$rect(selector)
    page$mouse((box$left + box$right) / 2, (box$top + box$bottom) / 2, click)
  }
  # The first line of the hint's words, as a rectangle, and the second line's top (or NA): where the strip must not be.
  lines <- function() {
    jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => { const hint = document.querySelector('%s'); const rows = new Map(); const walk = document.createTreeWalker(hint, NodeFilter.SHOW_TEXT); for (let n = walk.nextNode(); n; n = walk.nextNode()) { const range = document.createRange(); range.selectNodeContents(n); for (const r of range.getClientRects()) { const key = Math.round(r.top); const row = rows.get(key) ?? { left: Infinity, right: 0 }; row.left = Math.min(row.left, r.left); row.right = Math.max(row.right, r.right); rows.set(key, row); } } const tops = [...rows.keys()].sort((a, b) => a - b); return JSON.stringify({ first_left: rows.get(tops[0]).left, first_right: rows.get(tops[0]).right, second_top: tops[1] ?? null, second_right: tops[1] === undefined ? null : rows.get(tops[1]).right, count: tops.length }); })()",
      hint_box
    )))
  }
  # Closes the pinned tip with Escape and waits until it is gone.
  close_pin <- function() {
    page$press("Escape", wait = 0)
    expect_true(page$wait_unpinned())
  }
  reset <- function() {
    page$evaluate("document.activeElement?.blur(); window.scrollTo(0, 0)")
    page$mouse(5, 5, FALSE)
    expect_true(page$wait_closed())
    # The engine's own timers, open and close delay, for the tip the pointer just left: no signal says they have run out.
    page$hold(0.4)
  }

  it("starts and connects", {
    expect_true(app$proc$is_alive())
    expect_true(page$wait_connected())
  })

  it("draws a button in the corner of the hint of an item that can pin, and none on a hint with no pin", {
    reset()
    page$hover("#row_c")
    expect_true(page$evaluate("document.querySelector('.hovercraft-pin-strip') === null"))
    reset()
    expect_identical(page$hover("#row_a"), "Row A says something short.")
    expect_true(page$wait_until("document.querySelector('.hovercraft-pin-strip') !== null"))
    hint <- page$rect(hint_box)
    button <- page$rect(strip)
    expect_gte(button$left, hint$left)
    expect_lte(button$right, hint$right)
    expect_gte(button$top, hint$top)
    expect_lte(button$bottom, hint$bottom)
    expect_lt(hint$right - button$right, 8)
    expect_lt(button$top - hint$top, 8)
    expect_lt(lines()$first_right, button$left)
    # A one-sentence tip keeps its one line, with the button at the end of it.
    expect_equal(lines()$count, 1)
  })

  it("closes the strip at once when moving from a pinnable item to a plain-tip item inside it", {
    reset()
    view("row_g")
    expect_identical(page$hover("#row_g"), "Row G is pinnable.")
    expect_true(page$wait_until("document.querySelector('.hovercraft-pin-strip') !== null"))
    mouse_at("#cell_g")
    expect_true(page$wait_until(
      sprintf("document.querySelector('%s')?.textContent?.includes('Cell G is a plain tip.')", hint_box)
    ))
    expect_true(page$wait_until("document.querySelector('.hovercraft-pin-strip') === null", seconds = 0.2))
  })

  it("keeps the button beside the first line of a tip of several lines, and the words clear of it", {
    reset()
    page$hover("#row_d")
    expect_true(page$wait_until("document.querySelector('.hovercraft-pin-strip') !== null"))
    expect_gt(lines()$count, 1)
    button <- page$rect(strip)
    expect_lt(lines()$first_right, button$left)
    # Every line of the first block keeps clear of the button, also a second line that the button reaches down beside.
    expect_lte(lines()$second_right, button$left)
  })

  it("does not pin when the item itself is clicked, and its own click runs", {
    reset()
    before <- page$evaluate("window.rowClicks || 0")
    page$click("#row_a", wait = 0.7) # a click that must not pin: nothing happening has no signal
    expect_equal(page$evaluate("window.rowClicks || 0"), before + 1)
    expect_true(page$evaluate(sprintf("document.querySelector('%s') === null", pinned_box)))
  })

  it("keeps the hint open while the pointer is on the button", {
    reset()
    page$hover("#row_a")
    expect_true(page$wait_pin_button())
    mouse_at(strip)
    page$hold(1) # the hint must still be open after the engine's close delay: no signal for that
    expect_true(page$evaluate(sprintf("document.querySelector('%s') !== null", hint_box)))
    expect_true(page$evaluate("document.querySelector('.hovercraft-pin-strip') !== null"))
  })

  it("pins when the button is pressed, without a click on the item, and tells the server who and how", {
    reset()
    page$hover("#row_a")
    expect_true(page$wait_pin_button())
    before <- page$evaluate("window.rowClicks || 0")
    mouse_at(strip, click = TRUE)
    expect_true(page$wait_pinned())
    expect_equal(page$evaluate("window.rowClicks || 0"), before)
    expect_true(page$wait_text(
      "seen_a",
      "\"pin\":\\{\"state\":\"pinned\",\"text\":\"Row A says something short.\",\"by\":\"reader\",\"via\":\"click\""
    ))
    # The pinned tip keeps the engine's own close button; the strip belongs to the hint.
    expect_true(page$evaluate("document.querySelector('.hovercraft-pin-strip') === null"))
    expect_true(page$evaluate(sprintf("document.querySelector('%s .%s-close') !== null", pinned_box, css)))
    page$press("Escape", wait = 0)
    expect_true(page$wait_text("seen_a", "\"state\":\"closed\""))
  })

  it("pins by the pin key while the focus is on the item, and nowhere else", {
    reset()
    page$evaluate("document.getElementById('row_c').focus()")
    page$press("p", wait = 0.6) # the key must pin nothing: nothing happening has no signal
    expect_true(page$evaluate(sprintf("document.querySelector('%s') === null", pinned_box)))
    page$evaluate("document.getElementById('field').focus()")
    page$press("p", wait = 0.6) # the key must pin nothing: nothing happening has no signal
    expect_true(page$evaluate(sprintf("document.querySelector('%s') === null", pinned_box)))
    page$evaluate("document.activeElement.blur()")
    page$press("p", wait = 0.6) # the key must pin nothing: nothing happening has no signal
    expect_true(page$evaluate(sprintf("document.querySelector('%s') === null", pinned_box)))
    page$evaluate("document.getElementById('row_a').focus()")
    page$press("p", wait = 0)
    expect_true(page$wait_pinned())
    expect_true(page$wait_text("seen_a", "\"by\":\"reader\",\"via\":\"key\""))
    page$press("Escape", wait = 0)
    expect_true(page$wait_unpinned())
  })

  it("pins on a click of the item when pin is \"click\"", {
    reset()
    page$click("#row_b", wait = 0)
    expect_true(page$wait_pinned())
    expect_true(page$wait_text("seen_b", "\"state\":\"pinned\".*\"by\":\"reader\",\"via\":\"click\""))
    close_pin()
  })

  it("sends an input only to a tag that asked for it", {
    reset()
    page$hover("#row_c")
    page$click("#row_c", wait = 0.7) # a click that must send nothing: nothing happening has no signal
    page$settle()
    expect_identical(read("seen_c"), "{\"hint\":null,\"pin\":null,\"copy\":null}")
    reset()
    page$hover("#row_a")
    expect_true(page$wait_text("seen_a", "\"hint\":\"shown\""))
  })

  it("copies the code of a pinned tip, and tells the server, also where the clipboard is closed to a page", {
    reset()
    page$evaluate(
      "Object.defineProperty(window, 'isSecureContext', { value: false }); window.copyFired = 0; document.addEventListener('copy', () => { window.copyFired += 1; }, true)"
    )
    view("row_e")
    page$hover("#row_e")
    expect_true(page$wait_pin_button())
    mouse_at(strip, click = TRUE)
    expect_true(page$wait_until(sprintf("document.querySelector('%s .%s-copy') !== null", pinned_box, css)))
    mouse_at(sprintf("%s .%s-copy", pinned_box, css), click = TRUE)
    expect_true(page$wait_text(
      "seen_e",
      "\"copy\":\\{\"field\":\"sql\",\"block\":0,\"ok\":true,\"by\":\"reader\",\"via\":\"click\"\\}"
    ))
    expect_equal(page$evaluate("window.copyFired"), 1)
    expect_true(page$wait_text("seen_e", "\"pin\":\"pinned\""))
    close_pin()
  })

  it("works inside a module, with the id as the module's own", {
    reset()
    view("m-row")
    page$hover("#m-row")
    expect_true(page$wait_text("m-seen", "\"hint\":\"shown\""))
    mouse_at(strip, click = TRUE)
    expect_true(page$wait_text("m-seen", "\"pin\":\"pinned\""))
    close_pin()
  })

  # Follows the strip against the hint on every animation frame from now on: where its corner is against the hint's, the two
  # opacities, and where the hint's own corner is.
  follow <- function() {
    page$evaluate(
      "window.follow = []; (function loop() { const s = document.querySelector('.hovercraft-pin-strip'); const h = document.querySelector('.hc-tip:not(.hc-tip-pin):not([hidden])'); if (s && h) { const a = s.getBoundingClientRect(), b = h.getBoundingClientRect(); window.follow.push([a.right - b.right, a.top - b.top, parseFloat(getComputedStyle(s).opacity), parseFloat(getComputedStyle(h).opacity), b.right, b.top]); } if (window.follow.length < 400) requestAnimationFrame(loop); })(); 0"
    )
  }
  followed <- function() jsonlite::fromJSON(page$evaluate("JSON.stringify(window.follow)"))

  it("puts the button in the hint's corner, as visible as the hint, once the hint has slid in", {
    reset()
    view("row_f")
    page$hover("#row_f")
    expect_true(page$wait_pin_button())
    page$settle()
    # `hover()` waits for the slide to end, so this is where the button rests. The hint is still open.
    expect_true(page$evaluate(sprintf("document.querySelector('%s') !== null", hint_box)))
    hint <- page$rect(hint_box)
    button <- page$rect(strip)
    expect_gte(button$left, hint$left)
    expect_lte(button$right, hint$right)
    expect_gte(button$top, hint$top)
    expect_lte(button$bottom, hint$bottom)
    expect_lt(hint$right - button$right, 8)
    expect_lt(button$top - hint$top, 8)
    opacity <- function(selector) {
      page$evaluate(sprintf("parseFloat(getComputedStyle(document.querySelector('%s')).opacity)", selector))
    }
    expect_equal(opacity(hint_box), 1, tolerance = 0.02)
    expect_equal(opacity(".hovercraft-pin-strip"), 1, tolerance = 0.02)
  })

  it("draws the hint and the button without animation when the reader asks for reduced motion", {
    reset()
    page$emulate("prefers-reduced-motion", "reduce")
    follow()
    view("row_f")
    page$hover("#row_f")
    expect_true(page$wait_pin_button())
    expect_true(page$wait_until("window.follow.length >= 30"))
    got <- followed()
    expect_gt(nrow(got), 5)
    expect_gte(min(got[, 4]), 0.99)
    expect_gte(min(got[, 3]), 0.99)
    expect_lt(diff(range(got[, 1])), 1.5)
    page$emulate("prefers-reduced-motion", "no-preference")
  })

  it("writes nothing to the button while the hint stands still", {
    reset()
    page$hover("#row_a")
    expect_true(page$wait_pin_button())
    page$settle()
    page$evaluate(
      "window.writes = 0; new MutationObserver((records) => { window.writes += records.length; }).observe(document.querySelector('.hovercraft-pin-strip'), { attributes: true }); 0"
    )
    page$hold(0.8) # watch for writes while the hint stands still: nothing happening has no signal
    expect_equal(page$evaluate("window.writes"), 0)
  })

  it("closes the hint, as a reader's leave, when the pointer goes from the button to the page", {
    reset()
    page$hover("#row_a")
    expect_true(page$wait_pin_button())
    mouse_at(strip)
    page$hold(0.4) # the hint must stay open past the engine's close delay
    expect_true(page$evaluate(sprintf("document.querySelector('%s') !== null", hint_box)))
    page$mouse(690, 520, FALSE)
    expect_true(page$wait_text("seen_a", "\"hint\":\"hidden\",\"hint_by\":\"reader\",\"hint_reason\":\"leave\""))
    expect_true(page$wait_until("document.querySelector('.hovercraft-pin-strip') === null"))
  })

  it("keeps a hint that the server asked to show open when the pointer passes over the button", {
    reset()
    view("show_a")
    page$click_id("show_a")
    expect_true(page$wait_pin_button())
    mouse_at(strip)
    page$hold(0.3) # the pointer rests on the button for a moment, as a reader's does
    page$mouse(690, 520, FALSE)
    page$hold(1) # then leaves it: the hint must still be open after the engine's close delay
    expect_true(page$evaluate(sprintf("document.querySelector('%s') !== null", hint_box)))
    expect_true(page$wait_text("seen_a", "\"hint\":\"shown\",\"hint_by\":\"host\""))
    page$press("Escape", wait = 0)
    expect_true(page$wait_until(sprintf("document.querySelector('%s') === null", hint_box)))
  })

  it("shows the hint on a tap and does not pin; the button then pins, and a tap away closes the hint", {
    reset()
    box <- page$rect("#row_a")
    page$touch_tap((box$left + box$right) / 2, (box$top + box$bottom) / 2)
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", hint_box)))
    expect_true(page$wait_pin_button())
    expect_true(page$evaluate(sprintf("document.querySelector('%s') === null", pinned_box)))
    page$touch_tap(690, 520)
    expect_true(page$wait_until(sprintf("document.querySelector('%s') === null", hint_box)))
    box <- page$rect("#row_a")
    page$touch_tap((box$left + box$right) / 2, (box$top + box$bottom) / 2)
    expect_true(page$wait_pin_button())
    button <- page$rect(strip)
    page$touch_tap((button$left + button$right) / 2, (button$top + button$bottom) / 2)
    expect_true(page$wait_pinned())
    page$touch_off()
    close_pin()
  })

  it("shows the button on an item that pins by a click too", {
    reset()
    page$hover("#row_b")
    expect_true(page$wait_pin_button())
  })

  it("keeps the button in the corner of a hint inside a modal, above it", {
    reset()
    page$click_now("#open_modal")
    expect_true(page$wait_until(
      "document.getElementById('row_modal') !== null && document.getElementById('row_modal').getBoundingClientRect().width > 0"
    ))
    page$settle()
    page$hover("#row_modal")
    expect_true(page$wait_pin_button())
    button <- page$rect(strip)
    top <- page$evaluate(sprintf(
      "(() => { const e = document.elementFromPoint(%s, %s); return !!e?.closest('.hovercraft-pin-strip-button'); })()",
      (button$left + button$right) / 2,
      (button$top + button$bottom) / 2
    ))
    expect_true(top)
    mouse_at(strip, click = TRUE)
    expect_true(page$wait_text("seen_modal", "pinned"))
    close_pin()
    page$evaluate("document.querySelector('.modal.show .btn-close, [data-bs-dismiss=\"modal\"]')?.click(); 0")
    expect_true(page$wait_until("document.querySelector('.modal.show') === null"))
    page$settle()
  })

  it("puts the button in the left corner, and the room beside it, on a right-to-left page", {
    reset()
    page$evaluate("document.documentElement.dir = 'rtl'")
    page$hover("#row_d")
    expect_true(page$wait_pin_button())
    hint <- page$rect(hint_box)
    button <- page$rect(strip)
    expect_lt(button$left - hint$left, 8)
    expect_gt(lines()$first_left, button$right - 1)
    page$evaluate("document.documentElement.dir = 'ltr'")
  })

  it("keeps the button inside the hint and the window at 320 px", {
    page$viewport(320, 640)
    reset()
    page$hover("#row_d")
    expect_true(page$wait_pin_button())
    hint <- page$rect(hint_box)
    button <- page$rect(strip)
    expect_gte(hint$left, 0)
    expect_lte(hint$right, 320)
    expect_gte(button$left, hint$left)
    expect_lte(button$right, min(hint$right, 320))
    expect_lt(lines()$first_right, button$left)
    page$viewport(900, 700)
  })

  it("draws a hint and its pin button above a pinned tip where they overlap", {
    # Close every tip this test leaves open, whichever line fails.
    withr::defer({
      page$press("Escape", wait = 0.3)
      page$press("Escape", wait = 0.3)
    })
    reset()
    view("stack_a")
    page$hover("#stack_a")
    expect_true(page$wait_pin_button())
    mouse_at(strip, click = TRUE)
    expect_true(page$wait_pinned())
    expect_true(page$wait_until("document.querySelector('.hovercraft-pin-strip') === null"))
    page$hover("#stack_b")
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", hint_box)))
    expect_true(page$wait_pin_button())
    page$settle()
    pinned <- page$rect(pinned_box)
    hint <- page$rect(hint_box)
    button <- page$rect(strip)
    ox1 <- max(pinned$left, hint$left)
    ox2 <- min(pinned$right, hint$right)
    oy1 <- max(pinned$top, hint$top)
    oy2 <- min(pinned$bottom, hint$bottom)
    expect_gt(ox2 - ox1, 10)
    expect_gt(oy2 - oy1, 30)
    px <- (ox1 + ox2) / 2
    py <- oy2 - 8
    hit_overlap <- jsonlite::fromJSON(page$evaluate(sprintf(
      paste0(
        "(() => { const e = document.elementFromPoint(%s, %s);",
        " return JSON.stringify({ in_hint: !!e?.closest('%s'), in_pin: !!e?.closest('%s') }); })()"
      ),
      px,
      py,
      hint_box,
      pinned_box
    )))
    expect_true(hit_overlap$in_hint)
    expect_false(hit_overlap$in_pin)
    hit_button <- page$evaluate(sprintf(
      "(() => { const e = document.elementFromPoint(%s, %s); return !!e?.closest('%s'); })()",
      (button$left + button$right) / 2,
      (button$top + button$bottom) / 2,
      strip
    ))
    expect_true(hit_button)
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})
