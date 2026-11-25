skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if_not_installed("bslib")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# tip_more(): the long form behind a "More" button (the default), on a tag, with the M key, in a pinned tip, and in a DT
# and a reactable table. The tip keeps its place when it grows and shrinks because the engine does (0.58.0): these tests
# prove the three behaviours the button depends on (it stays on its side and at its edge, it grows away from the item,
# and Less does not close it), and that a fresh open still picks the side that has room.
describe("the More button on a tag", {
  long_words <- paste(
    "**How it is counted.** Tickets opened Monday to Sunday, UTC. The clock runs in business hours only.",
    "Auto-closed spam is left out. A reply from a bot does not stop the clock; a reply from a person does.",
    "The figures are refreshed every hour from the helpdesk export, and a ticket that is reopened starts a new clock.",
    "Weekends and public holidays of the desk's own country are not counted, and neither are the hours when no agent",
    "is on duty. Median, not mean, so one very old ticket does not move it."
  )
  app <- start_app(
    function(long_words) {
      place <- function(x, style) htmltools::div(style = paste("position: absolute;", style), x)
      ui <- bslib::page_fluid(
        htmltools::tags$style("body { min-height: 700px; }"),
        place(
          tip_on(
            shiny::actionButton("go", "Go"),
            "Short **tip**",
            more = long_words,
            id = "go_tip",
            events = "hint",
            placement = "bottom"
          ),
          "top: 60px; left: 60px;"
        ),
        place(
          tip_on(
            shiny::actionButton("mid", "Middle"),
            "Short middle tip",
            more = long_words,
            placement = "bottom"
          ),
          "top: 470px; left: 60px;"
        ),
        place(
          tip_on(
            shiny::actionButton("edge", "Edge"),
            "Short edge tip",
            more = "A second line of words for the edge.",
            placement = "bottom"
          ),
          "top: 655px; left: 360px;"
        ),
        place(
          tip_on(
            shiny::textInput("txt", NULL, placeholder = "type here"),
            "Short text tip",
            more = "Long words for the field.",
            id = "txt_tip"
          ),
          "top: 60px; left: 300px;"
        ),
        place(
          tip_on(
            shiny::actionButton("pn", "Pin me"),
            "Short pinned tip",
            more = "The long form of the pinned tip.",
            id = "pn_tip",
            pin = TRUE,
            events = c("hint", "pin")
          ),
          "top: 60px; left: 560px;"
        ),
        place(
          tip_on(
            shiny::actionButton("lb", "Labels"),
            "Short labelled tip",
            more = tip_more("Long labelled words.", label = c(more = "Show all", less = "Show less")),
            placement = "bottom"
          ),
          "top: 160px; left: 60px;"
        ),
        place(
          tip_on(
            shiny::actionButton("pb", "Pin B"),
            "Short second pinned tip",
            more = "The long form of the second pin.",
            id = "pb_tip",
            pin = TRUE
          ),
          "top: 260px; left: 560px;"
        ),
        place(shiny::uiOutput("dyn"), "top: 260px; left: 300px;"),
        place(
          tip_on(shiny::checkboxInput("ck", "Check"), "Short check tip", more = "Long check words."),
          "top: 360px; left: 300px;"
        ),
        place(
          tip_on(
            shiny::actionButton("po", "Output"),
            "Short output tip",
            more = tip_template(tip_text("Above the output."), tip_output(shiny::textOutput("more_txt"))),
            id = "po_tip",
            pin = TRUE
          ),
          "top: 360px; left: 60px;"
        ),
        place(
          tip_on(
            shiny::actionButton("hv", "Hover"),
            "Short hover tip",
            more = tip_more("Long hover words.", on = "hover"),
            placement = "bottom"
          ),
          "top: 160px; left: 300px;"
        ),
        shiny::verbatimTextOutput("seen")
      )
      server <- function(input, output, session) {
        output$dyn <- shiny::renderUI({
          if (isTRUE(input$hide)) {
            return(NULL)
          }
          tip_on(shiny::actionButton("dy", "Dynamic"), "Short dynamic tip", more = "Long dynamic words.", id = "dy_tip")
        })
        output$more_txt <- shiny::renderText("Hello from the output")
        output$seen <- shiny::renderText({
          everything <- shiny::reactiveValuesToList(input)
          at <- grepl("_(pin|hint)$", names(everything))
          jsonlite::toJSON(everything[at][order(names(everything[at]))], auto_unbox = TRUE, null = "null")
        })
      }
      shiny::shinyApp(ui, server)
    },
    long_words = long_words
  )
  page <- local_page(app$url, environment())
  css <- tip_names()$css
  hint_sel <- sprintf(".%s:not(.%s-pin):not([hidden])", css, css)
  pin_sel <- sprintf(".%s.%s-pin:not([hidden])", css, css)
  strip_button <- ".hovercraft-more-strip-button"
  rect <- function(selector) {
    jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => { const r = document.querySelector(%s).getBoundingClientRect(); return JSON.stringify({ left: r.left, top: r.top, right: r.right, bottom: r.bottom, width: r.width, height: r.height }); })()",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    )))
  }
  centre <- function(selector) {
    box <- rect(selector)
    c((box$left + box$right) / 2, (box$top + box$bottom) / 2)
  }
  press_button <- function() {
    at <- centre(strip_button)
    page$mouse(at[[1]], at[[2]], click = TRUE)
    Sys.sleep(0.5)
  }
  box_of <- function(selector = hint_sel) {
    jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => { const t = document.querySelector(%s); if (!t) return JSON.stringify(null); const r = t.getBoundingClientRect(); return JSON.stringify({ side: t.dataset.side, top: r.top, bottom: r.bottom, left: r.left, right: r.right, height: r.height, text: t.textContent }); })()",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    )))
  }
  button_text <- function() page$evaluate(sprintf("document.querySelector('%s').textContent", strip_button))
  expanded <- function() {
    page$evaluate(sprintf("document.querySelector('%s').getAttribute('aria-expanded')", strip_button))
  }
  rest <- function() {
    page$press("Escape", wait = 0.3)
    page$evaluate("document.activeElement?.blur()")
    page$mouse(890, 5, FALSE)
    Sys.sleep(0.8)
  }
  seen <- function(name) {
    all <- jsonlite::fromJSON(page$evaluate("document.getElementById('seen').textContent"), simplifyVector = FALSE)
    all[[name]]
  }
  wait_hint <- function(selector) {
    page$hover(selector)
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", strip_button)))
  }

  it("starts, and a tip with a long form announces the M key and keeps the long form out of the page's tip", {
    expect_true(app$proc$is_alive(), info = paste(app$proc$read_error(), collapse = "\n"))
    expect_true(page$wait_until("document.getElementById('go') !== null"))
    expect_identical(
      page$evaluate("document.getElementById('go').closest('[aria-keyshortcuts]')?.getAttribute('aria-keyshortcuts')"),
      "M"
    )
    expect_identical(
      page$evaluate("document.getElementById('go').closest('[data-hovercraft-tip-more]') === null"),
      TRUE
    )
  })

  it("shows the short tip with a More button, with the pointer still, and the long form on a press", {
    wait_hint("#go")
    short <- box_of()
    expect_match(short$text, "Short tip")
    expect_no_match(short$text, "business hours")
    expect_identical(button_text(), "More")
    expect_identical(expanded(), "false")
    expect_identical(short$side, "below")
    # From here on, count the hint events the page hears: a tip that grows or shrinks sends none.
    page$evaluate("window.hints = 0; document.addEventListener('hovercraft:hint', () => { window.hints += 1; }); 0")
    press_button()
    long <- box_of()
    expect_match(long$text, "business hours")
    expect_identical(button_text(), "Less")
    expect_identical(expanded(), "true")
    # The tip is the same one, still open after the pointer has rested for a while, on its side, at its edge, and it grew
    # away from the item (down, as it is below).
    Sys.sleep(1)
    after <- box_of()
    expect_false(is.null(after))
    expect_identical(after$side, "below")
    expect_equal(after$top, short$top, tolerance = 1)
    expect_gt(after$bottom, short$bottom + 20)
    expect_equal(after$left, short$left, tolerance = 1)
    # The hint's own input said it was shown once, and the growth sent nothing.
    expect_identical(seen("go_tip_hint")$state, "shown")
    expect_identical(page$evaluate("window.hints"), 0L)
    expect_identical(page$problems(), character())
  })

  it("closes the long form with Less, and the tip stays open, short, on its side", {
    before <- box_of()
    press_button()
    Sys.sleep(1)
    after <- box_of()
    expect_false(is.null(after))
    expect_identical(button_text(), "More")
    expect_identical(expanded(), "false")
    expect_no_match(after$text, "business hours")
    expect_lt(after$height, before$height - 20)
    expect_identical(after$side, "below")
    expect_equal(after$top, before$top, tolerance = 1)
    expect_identical(seen("go_tip_hint")$state, "shown")
    expect_identical(page$evaluate("window.hints"), 0L)
  })

  it("starts short the next time it opens", {
    press_button()
    expect_match(box_of()$text, "business hours")
    rest()
    expect_true(page$wait_until(sprintf("document.querySelector('%s') === null", hint_sel)))
    wait_hint("#go")
    expect_no_match(box_of()$text, "business hours")
    expect_identical(button_text(), "More")
    rest()
  })

  it("stays on its side when the long form does not fit there, and the tip stays open", {
    # The item is low in the page: the short tip fits below, the long form does not.
    wait_hint("#mid")
    short <- box_of()
    expect_identical(short$side, "below")
    press_button()
    Sys.sleep(1)
    long <- box_of()
    expect_false(is.null(long))
    expect_match(long$text, "business hours")
    expect_identical(long$side, "below")
    expect_equal(long$top, short$top, tolerance = 1)
    expect_lte(long$bottom, 700 + 1)
    # It is capped to the room it has, and scrolls, instead of flipping to the other side.
    expect_identical(
      page$evaluate(sprintf("(document.querySelector('%s').dataset.cap ?? '') === 'true'", hint_sel)),
      TRUE
    )
    # It scrolls instead of flipping to the other side.
    press_button()
    Sys.sleep(1)
    expect_identical(box_of()$side, "below")
    expect_no_match(box_of()$text, "business hours")
    rest()
  })

  it("still picks the side that has room on a fresh open, near the bottom of the page", {
    wait_hint("#edge")
    fresh <- box_of()
    expect_identical(fresh$side, "above")
    press_button()
    Sys.sleep(1)
    grown <- box_of()
    expect_identical(grown$side, "above")
    # Grown away from the item: the edge that faces it did not move.
    expect_equal(grown$bottom, fresh$bottom, tolerance = 1)
    expect_lt(grown$top, fresh$top)
    rest()
  })

  it("opens and closes the long form with the M key while the focus is on the item", {
    page$evaluate("document.getElementById('go').focus()")
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", hint_sel)))
    page$press("m", wait = 0.6)
    expect_match(box_of()$text, "business hours")
    expect_identical(button_text(), "Less")
    page$press("m", wait = 0.6)
    expect_no_match(box_of()$text, "business hours")
    expect_identical(button_text(), "More")
    expect_identical(page$evaluate("document.activeElement.id"), "go")
    rest()
  })

  it("does not take the M key inside a text field, where a letter is a letter", {
    page$evaluate("document.getElementById('txt').focus()")
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", hint_sel)))
    # A key that the layer took would be prevented: in a field the letter goes through to the field.
    prevented <- page$evaluate(
      "(() => { const e = new KeyboardEvent('keydown', { key: 'm', bubbles: true, cancelable: true }); document.activeElement.dispatchEvent(e); return e.defaultPrevented; })()"
    )
    expect_false(prevented)
    Sys.sleep(0.4)
    expect_no_match(box_of()$text, "Long words for the field")
    rest()
  })

  it("takes the M key on a check box, which takes no letters, and holds it down toggles once", {
    page$evaluate("document.getElementById('ck').focus()")
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", hint_sel)))
    page$press("m", wait = 0.7)
    expect_match(box_of()$text, "Long check words")
    # A key held down sends repeats: they do not toggle again.
    page$evaluate(
      "document.activeElement.dispatchEvent(new KeyboardEvent('keydown', { key: 'm', repeat: true, bubbles: true, cancelable: true })); 0"
    )
    Sys.sleep(0.6)
    expect_match(box_of()$text, "Long check words")
    rest()
  })

  it("says the buttons' own words", {
    wait_hint("#lb")
    expect_identical(button_text(), "Show all")
    press_button()
    expect_identical(button_text(), "Show less")
    rest()
  })

  it("draws no button when the long form is on hover", {
    page$hover("#hv")
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", hint_sel)))
    expect_match(box_of()$text, "Long hover words")
    expect_identical(page$evaluate(sprintf("document.querySelector('%s') === null", strip_button)), TRUE)
    rest()
  })

  it("gives a pinned tip the buttons too, and says 'updated' when it grows", {
    page$evaluate("document.getElementById('pn').focus()")
    page$press("p", wait = 0.8)
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", pin_sel)))
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", strip_button)))
    expect_identical(seen("pn_tip_pin")$state, "pinned")
    expect_no_match(box_of(pin_sel)$text, "long form of the pinned")
    expect_identical(button_text(), "More")
    press_button()
    expect_true(page$wait_until(sprintf(
      "document.querySelector('%s').textContent.includes('long form of the pinned')",
      pin_sel
    )))
    expect_true(page$wait_until(
      "JSON.parse(document.getElementById('seen').textContent).pn_tip_pin?.state === 'updated'"
    ))
    expect_identical(button_text(), "Less")
    # The pin is still there, drawn again, not closed.
    expect_false(is.null(box_of(pin_sel)))
    press_button()
    expect_true(page$wait_until(sprintf(
      "!document.querySelector('%s').textContent.includes('long form of the pinned')",
      pin_sel
    )))
    expect_false(is.null(box_of(pin_sel)))
    page$press("Escape", wait = 0.4)
    expect_true(page$wait_until(
      "JSON.parse(document.getElementById('seen').textContent).pn_tip_pin?.state === 'closed'"
    ))
    expect_identical(page$problems(), character())
  })

  it("opens the long form of a pinned tip with the M key, from the item and from inside the tip", {
    page$evaluate("document.getElementById('pn').focus()")
    page$press("p", wait = 0.8)
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", pin_sel)))
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", strip_button)))
    has_long <- function() box_of(pin_sel)$text |> grepl("long form of the pinned", x = _, fixed = TRUE)
    expect_false(has_long())
    page$press("m", wait = 0.7)
    expect_true(has_long())
    page$press("m", wait = 0.7)
    expect_false(has_long())
    # The reader tabs into the pinned tip: the key works from there too.
    page$evaluate(sprintf("document.querySelector('%s').focus()", pin_sel))
    expect_identical(
      page$evaluate(sprintf("document.querySelector('%s').contains(document.activeElement)", pin_sel)),
      TRUE
    )
    page$press("m", wait = 0.7)
    expect_true(has_long())
    page$press("m", wait = 0.7)
    expect_false(has_long())
    page$press("Escape", wait = 0.4)
  })

  it("keeps a pinned tip's button and its M key when a hint opens somewhere else", {
    page$evaluate("document.getElementById('pb').focus()")
    page$press("p", wait = 0.8)
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", pin_sel)))
    pinned_buttons <- function() page$evaluate(sprintf("document.querySelectorAll('%s').length", strip_button))
    expect_identical(pinned_buttons(), 1L)
    # A hint opens on another item: the pinned tip keeps its button, and the hint has its own.
    wait_hint("#go")
    expect_true(page$wait_until(sprintf("document.querySelectorAll('%s').length === 2", strip_button)))
    page$evaluate("document.getElementById('pb').focus()")
    page$press("m", wait = 0.8)
    expect_true(page$wait_until(sprintf(
      "document.querySelector('%s').textContent.includes('long form of the second pin')",
      pin_sel
    )))
    page$press("m", wait = 0.8)
    page$press("Escape", wait = 0.4)
    page$press("Escape", wait = 0.4)
    rest()
    expect_identical(pinned_buttons(), 0L)
  })

  it("makes the pinned tip's button a tab stop after the tip's own, and the buttons say their state", {
    page$evaluate("document.getElementById('pn').focus()")
    page$press("p", wait = 0.8)
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", strip_button)))
    last_stop <- sprintf(
      "(() => { const l = [...document.querySelector('%s').querySelectorAll('button, a[href]')]; return l[l.length - 1]; })()",
      pin_sel
    )
    page$evaluate(sprintf("%s.focus()", last_stop))
    page$press("Tab", wait = 0.5)
    expect_identical(
      page$evaluate(sprintf("document.activeElement === document.querySelector('%s')", strip_button)),
      TRUE
    )
    # A key press on a button is a click: the test sends the click a keyboard would.
    page$evaluate("document.activeElement.click(); 0")
    Sys.sleep(0.8)
    expect_identical(expanded(), "true")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('%s').textContent.includes('long form of the pinned')",
      pin_sel
    )))
    # Focus is still on the button, which says Less now; Shift+Tab goes back into the tip.
    expect_identical(
      page$evaluate(sprintf("document.activeElement === document.querySelector('%s')", strip_button)),
      TRUE
    )
    expect_identical(button_text(), "Less")
    page$evaluate(
      "document.activeElement.dispatchEvent(new KeyboardEvent('keydown', { key: 'Tab', shiftKey: true, bubbles: true, cancelable: true })); 0"
    )
    Sys.sleep(0.3)
    expect_identical(
      page$evaluate(sprintf("document.querySelector('%s').contains(document.activeElement)", pin_sel)),
      TRUE
    )
    page$press("Escape", wait = 0.4)
  })

  it("starts the long form of a pinned tip with a hairline, and takes it away with Less", {
    page$evaluate("document.getElementById('pn').focus()")
    page$press("p", wait = 0.8)
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", strip_button)))
    hairlines <- function() page$evaluate(sprintf("document.querySelectorAll('%s .hc-long-first').length", pin_sel))
    expect_identical(hairlines(), 0L)
    press_button()
    expect_true(page$wait_until(sprintf("document.querySelectorAll('%s .hc-long-first').length === 1", pin_sel)))
    press_button()
    expect_true(page$wait_until(sprintf("document.querySelectorAll('%s .hc-long-first').length === 0", pin_sel)))
    page$press("Escape", wait = 0.4)
  })

  it("says only the long form to a screen reader, and clears it when the tip goes", {
    live <- function() page$evaluate("document.querySelector('.hovercraft-more-strip-live')?.textContent ?? ''")
    # "Short middle tip" would show if the short form were read again; the long words hold "Unless" nowhere, so the check
    # is on what is said.
    wait_hint("#go")
    press_button()
    expect_true(page$wait_until(
      "(document.querySelector('.hovercraft-more-strip-live')?.textContent ?? '').includes('business hours')"
    ))
    expect_no_match(live(), "Short tip", fixed = TRUE)
    press_button()
    expect_true(page$wait_until("document.querySelector('.hovercraft-more-strip-live')?.textContent === 'Less'"))
    press_button()
    rest()
    expect_true(page$wait_until("(document.querySelector('.hovercraft-more-strip-live')?.textContent ?? '') === ''"))
  })

  it("removes the button when a renderUI() draws the item again, with no error", {
    wait_hint("#dy")
    press_button()
    expect_match(box_of()$text, "Long dynamic words")
    page$evaluate("Shiny.setInputValue('hide', true, { priority: 'event' })")
    expect_true(page$wait_until("document.getElementById('dy') === null"))
    expect_true(page$wait_until(sprintf("document.querySelector('%s') === null", strip_button)))
    expect_identical(page$problems(), character())
  })

  it("shows a Shiny output that the long form of a pinned tip holds", {
    page$evaluate("document.getElementById('po').focus()")
    page$press("p", wait = 0.8)
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", strip_button)))
    press_button()
    expect_true(page$wait_until(sprintf(
      "document.querySelector('%s').textContent.includes('Hello from the output')",
      pin_sel
    )))
    press_button()
    page$press("Escape", wait = 0.4)
    expect_identical(page$problems(), character())
  })
})

describe("the More button in a DT and a reactable table", {
  skip_if_not_installed("DT")
  skip_if_not_installed("reactable")
  scores <- data.frame(
    team = c("Owls", "Foxes"),
    wins = c(7, 5),
    detail = c("Won last four games away", "Strong home record")
  )
  app <- start_app(
    function(scores) {
      ui <- bslib::page_fluid(
        htmltools::div(style = "padding: 40px 24px;", DT::DTOutput("dt")),
        htmltools::div(style = "padding: 40px 24px;", reactable::reactableOutput("rt"))
      )
      tipped <- function(widget, id) {
        tip_table(
          widget,
          cells = list(team = tip_spec("**{{ team }}**", more = "Detail: {{ detail }}")),
          headers = list(wins = tip_spec("Games won", more = "Counted from the first match.")),
          id = id
        )
      }
      server <- function(input, output, session) {
        output$dt <- DT::renderDT(tipped(DT::datatable(scores, rownames = FALSE, options = list(dom = "t")), "tdt"))
        output$rt <- reactable::renderReactable(tipped(reactable::reactable(scores), "trt"))
      }
      shiny::shinyApp(ui, server)
    },
    scores = scores
  )
  page <- local_page(app$url, environment())
  css <- tip_names()$css
  hint_sel <- sprintf(".%s:not(.%s-pin):not([hidden])", css, css)
  strip_button <- ".hovercraft-more-strip-button"
  centre <- function(selector) {
    jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => { const r = document.querySelector(%s).getBoundingClientRect(); return JSON.stringify([(r.left + r.right) / 2, (r.top + r.bottom) / 2]); })()",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    )))
  }
  text <- function() page$evaluate(sprintf("document.querySelector('%s')?.textContent ?? ''", hint_sel))

  it("starts and draws the tables with the long form held, not shown", {
    expect_true(app$proc$is_alive(), info = paste(app$proc$read_error(), collapse = "\n"))
    expect_true(page$wait_until("document.querySelectorAll('#dt tbody td[data-hovercraft-tip-id]').length === 2"))
    expect_true(page$wait_until("document.querySelectorAll('#rt [role=cell][data-hovercraft-tip-id]').length === 2"))
    expect_identical(page$evaluate("document.querySelectorAll('[data-hovercraft-tip-more-on]').length"), 0L)
    expect_identical(page$evaluate("document.querySelector('#dt tbody td').getAttribute('aria-keyshortcuts')"), "M")
  })

  for (case in list(
    list(name = "a DT cell", cell = "#dt tbody tr:first-child td:first-child", long = "Won last four games away"),
    list(name = "a reactable cell", cell = "#rt [role=cell]:first-child", long = "Won last four games away"),
    list(name = "a DT header cell", cell = "#dt thead th:nth-child(2)", long = "Counted from the first match"),
    list(
      name = "a reactable header cell",
      cell = "#rt [role=columnheader]:nth-child(2)",
      long = "Counted from the first match"
    )
  )) {
    local({
      case <- case
      it(paste("opens the long form of", case$name, "with More and shrinks it with Less, the tip staying open"), {
        page$mouse(5, 5, FALSE)
        Sys.sleep(0.6)
        page$hover(case$cell)
        expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", strip_button)))
        expect_no_match(text(), case$long, fixed = TRUE)
        at <- centre(strip_button)
        page$mouse(at[[1]], at[[2]], click = TRUE)
        expect_true(page$wait_until(sprintf(
          "document.querySelector('%s')?.textContent.includes('%s')",
          hint_sel,
          case$long
        )))
        Sys.sleep(0.8)
        expect_match(text(), case$long, fixed = TRUE)
        at <- centre(strip_button)
        page$mouse(at[[1]], at[[2]], click = TRUE)
        expect_true(page$wait_until(sprintf(
          "!document.querySelector('%s')?.textContent.includes('%s')",
          hint_sel,
          case$long
        )))
        Sys.sleep(0.8)
        expect_false(identical(text(), ""))
        page$mouse(5, 5, FALSE)
        Sys.sleep(0.8)
      })
    })
  }

  it("opens and closes the long form of a table cell with the M key, beside the table's own arrow keys", {
    page$mouse(5, 5, FALSE)
    Sys.sleep(0.6)
    page$evaluate("document.querySelector('#dt tbody tr:first-child td:first-child').focus()")
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", hint_sel)))
    page$press("m", wait = 0.6)
    expect_match(text(), "Won last four games away", fixed = TRUE)
    page$press("m", wait = 0.6)
    expect_no_match(text(), "Won last four games away", fixed = TRUE)
    # The arrow keys still move between the cells that have a tip.
    page$press("ArrowDown", wait = 0.6)
    expect_identical(
      page$evaluate("document.activeElement === document.querySelector('#dt tbody tr:nth-child(2) td:first-child')"),
      TRUE
    )
    page$evaluate("document.activeElement.blur()")
  })

  it("has no console error", {
    expect_identical(page$problems(), character())
  })
})
