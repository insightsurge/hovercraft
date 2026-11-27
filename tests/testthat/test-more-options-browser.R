skip_if_no_browser()

# tip_more(on = "click") and tip_more(pin = TRUE): a click on the item opens the long form without pinning, and the More
# button (and the M key) can pin the tip so the long form stays. Real pointer and key events, in headless Chrome.
describe("tip_more() options for opening the long form", {
  words <- function(id) paste("LONG words of", id, "that go on for a while, so the tip has to grow to hold them all.")
  span <- function(id, left, top, ..., pin = NULL, more = NULL) {
    place(
      tip_on(
        htmltools::tags$span(id, id = id, tabindex = "0", style = "padding: 4px 8px; border: 1px solid;"),
        paste("Short", id),
        more = more %||% tip_more(words(id), ...),
        id = id,
        pin = pin,
        placement = "bottom"
      ),
      sprintf("left: %dpx; top: %dpx;", left, top)
    )
  }
  button <- function(id, left, top, ..., can_pin = TRUE) {
    place(
      tip_on(
        htmltools::tags$button(id, id = id),
        paste("Short", id),
        more = tip_more(words(id), ...),
        id = id,
        pin = can_pin,
        placement = "bottom"
      ),
      sprintf("left: %dpx; top: %dpx;", left, top)
    )
  }
  css <- tip_names()$css
  open_attr <- tip_contract()$attributes$open
  hint_sel <- sprintf(".%s:not(.%s-pin):not([hidden])", css, css)
  pin_sel <- sprintf(".%s.%s-pin:not([hidden])", css, css)
  ready <- "window.HovercraftMore !== undefined && window.Hovercraft !== undefined"
  text_of <- function(page, selector) {
    page$evaluate(sprintf(
      "document.querySelector(%s)?.textContent ?? ''",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    ))
  }
  state_of <- function(page, id) {
    page$evaluate(sprintf("document.getElementById('%s').getAttribute('%s')", id, open_attr))
  }
  buttons <- function(page) {
    page$evaluate(
      "document.querySelectorAll('.hovercraft-more-strip:not([hidden]) .hovercraft-more-strip-button').length"
    )
  }

  # A real click where the pointer already rests on the element: the hint stays open, as for a reader who clicks twice.
  click_again <- function(page, selector) {
    at <- jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => { const r = document.querySelector(%s).getBoundingClientRect(); return JSON.stringify([r.x + 10, r.y + r.height / 2]); })()",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    )))
    page$mouse(at[[1]], at[[2]], click = TRUE)
    page$settle()
  }
  # A real click on the More / Less button that is on show.
  press_more <- function(page) {
    at <- jsonlite::fromJSON(page$evaluate(
      "(() => { const r = document.querySelector('.hovercraft-more-strip:not([hidden]) .hovercraft-more-strip-button').getBoundingClientRect(); return JSON.stringify([r.x + r.width / 2, r.y + r.height / 2]); })()"
    ))
    page$mouse(at[[1]], at[[2]], click = TRUE)
    page$settle()
  }
  long_in <- function(page, selector) {
    sprintf("document.querySelector('%s')?.textContent.includes('LONG')", selector)
  }

  it("opens and closes the long form on a click of the item, without a button and without pinning", {
    page <- local_page(page_file(use_hovercraft(), span("one", 60, 60, on = "click", pin = FALSE)), environment())
    expect_true(page$wait_until(ready))
    expect_length(page$hover("#one"), 1L)
    expect_no_match(text_of(page, hint_sel), "LONG")
    expect_identical(buttons(page), 0L)
    page$click("#one", wait = 0)
    expect_true(page$wait_until(sprintf("document.querySelector('%s')?.textContent.includes('LONG')", hint_sel)))
    expect_identical(buttons(page), 0L)
    click_again(page, "#one")
    expect_true(page$wait_until(sprintf("!document.querySelector('%s')?.textContent.includes('LONG')", hint_sel)))
    page$hold(0.3) # a click must pin nothing: nothing happening has no signal
    expect_false(page$evaluate(sprintf("document.querySelector('%s') !== null", pin_sel)))
    expect_identical(state_of(page, "one"), "hint")
  })

  it("opens the long form of a click tip with the M key too, which the item announces", {
    page <- local_page(page_file(use_hovercraft(), span("one", 60, 60, on = "click")), environment())
    expect_true(page$wait_until(ready))
    expect_identical(page$evaluate("document.getElementById('one').getAttribute('aria-keyshortcuts')"), "M")
    page$evaluate("document.getElementById('one').focus(); 0")
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", hint_sel)))
    page$press("m", wait = 0)
    expect_true(page$wait_until(sprintf("document.querySelector('%s')?.textContent.includes('LONG')", hint_sel)))
    page$press("m", wait = 0)
    expect_true(page$wait_until(sprintf("!document.querySelector('%s')?.textContent.includes('LONG')", hint_sel)))
  })

  it("shows the short tip first on a tap: the tap that opens the tip does not open the long form too", {
    page <- local_page(page_file(use_hovercraft(), span("one", 60, 60, on = "click")), environment())
    expect_true(page$wait_until(ready))
    at <- jsonlite::fromJSON(page$evaluate(
      "(() => { const r = document.getElementById('one').getBoundingClientRect(); return JSON.stringify([r.x + r.width / 2, r.y + r.height / 2]); })()"
    ))
    withr::defer(page$touch_off())
    page$touch_tap(at[[1]], at[[2]])
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", hint_sel)))
    page$hold(0.5) # the tap must not open the long form: nothing happening has no signal
    expect_no_match(text_of(page, hint_sel), "LONG")
    expect_identical(state_of(page, "one"), "hint")
  })

  it("leaves the click of a control inside the item to the control", {
    page <- local_page(page_file(use_hovercraft(), span("one", 60, 60, on = "click")), environment())
    expect_true(page$wait_until(ready))
    page$evaluate(
      "(() => { const b = document.createElement('button'); b.id = 'inner'; b.textContent = 'inner'; window.innerClicks = 0; b.addEventListener('click', () => { window.innerClicks += 1; }); document.getElementById('one').append(' ', b); })()"
    )
    page$click("#inner")
    expect_true(page$wait_until("window.innerClicks === 1"))
    page$hold(0.4) # the control's click must not open the long form: nothing happening has no signal
    expect_no_match(text_of(page, hint_sel), "LONG")
    # The item's own words still open it.
    click_again(page, "#one")
    expect_true(page$wait_until(long_in(page, hint_sel)))
  })

  it("opens a pinned click tip's long form on a click of the item, and that tip has no button to tab to", {
    page <- local_page(page_file(use_hovercraft(), span("one", 60, 60, on = "click", pin = TRUE)), environment())
    expect_true(page$wait_until(ready))
    page$evaluate("window.Hovercraft.pin(document.getElementById('one'), { by: 'reader', via: 'key' }); 0")
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", pin_sel)))
    page$click("#one", wait = 0)
    expect_true(page$wait_until(sprintf("document.querySelector('%s')?.textContent.includes('LONG')", pin_sel)))
    expect_identical(buttons(page), 0L)
    expect_identical(state_of(page, "one"), "pin")
    page$click("#one", wait = 0)
    expect_true(page$wait_until(sprintf("!document.querySelector('%s')?.textContent.includes('LONG')", pin_sel)))
    expect_identical(state_of(page, "one"), "pin")
  })

  it("pins the tip with the More button when the tip says so, and keeps the long form; Less leaves it pinned", {
    page <- local_page(page_file(use_hovercraft(), button("one", 60, 60, pin = TRUE)), environment())
    expect_true(page$wait_until(ready))
    page$hover("#one")
    expect_true(page$wait_until("document.querySelector('.hovercraft-more-strip-button') !== null"))
    press_more(page)
    expect_true(page$wait_until(long_in(page, pin_sel)))
    expect_identical(state_of(page, "one"), "pin")
    expect_identical(text_of(page, ".hovercraft-more-strip-button"), "Less")
    press_more(page)
    expect_true(page$wait_until(sprintf("!%s", long_in(page, pin_sel))))
    expect_identical(state_of(page, "one"), "pin")
    press_more(page)
    expect_true(page$wait_until(long_in(page, pin_sel)))
    expect_identical(page$evaluate(sprintf("document.querySelectorAll('[%s=\"pin\"]').length", open_attr)), 1L)
  })

  it("does not pin with the More button unless the tip says so", {
    page <- local_page(page_file(use_hovercraft(), button("one", 60, 60)), environment())
    expect_true(page$wait_until(ready))
    page$hover("#one")
    expect_true(page$wait_until("document.querySelector('.hovercraft-more-strip-button') !== null"))
    press_more(page)
    expect_true(page$wait_until(long_in(page, hint_sel)))
    page$hold(0.3) # the button must pin nothing: nothing happening has no signal
    expect_identical(state_of(page, "one"), "hint")
  })

  it("says the long form to a screen reader when the button pins the tip", {
    page <- local_page(page_file(use_hovercraft(), button("one", 60, 60, pin = TRUE)), environment())
    expect_true(page$wait_until(ready))
    page$hover("#one")
    expect_true(page$wait_until("document.querySelector('.hovercraft-more-strip-button') !== null"))
    press_more(page)
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", pin_sel)))
    expect_true(page$wait_until("document.querySelector('.hovercraft-more-strip-live')?.textContent.includes('LONG')"))
  })

  it("pins with the M key too when the tip says so", {
    page <- local_page(page_file(use_hovercraft(), button("one", 60, 60, pin = TRUE)), environment())
    expect_true(page$wait_until(ready))
    page$evaluate("document.getElementById('one').focus(); 0")
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", hint_sel)))
    page$press("m", wait = 0)
    expect_true(page$wait_until(long_in(page, pin_sel)))
    expect_identical(state_of(page, "one"), "pin")
  })

  # Where the control sits in the tip: its distance from the tip's left and right edges, and how it looks.
  control <- function(page) {
    jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => { const box = document.querySelector('%s'); const b = document.querySelector('.hovercraft-more-strip-button'); const r = box.getBoundingClientRect(); const q = b.getBoundingClientRect(); const s = getComputedStyle(b); const t = getComputedStyle(box);
        return JSON.stringify({ from_left: q.left - r.left, from_right: r.right - q.right, bottom_gap: r.bottom - q.bottom, tag: b.tagName, words: b.textContent, expanded: b.getAttribute('aria-expanded'),
          italic: s.fontStyle, underline: s.textDecorationLine, border: s.borderTopColor, colour: s.color, box_colour: t.color, tab: b.tabIndex, height: q.height }); })()",
      hint_sel
    )))
  }
  show <- function(page, id = "one") {
    expect_true(page$wait_until(ready))
    page$hover(sprintf("#%s", id))
    expect_true(page$wait_until("document.querySelector('.hovercraft-more-strip-button') !== null"))
    page$settle()
    control(page)
  }

  it("draws the control as an italic text link at the start when as is a link, and it works like the button", {
    page <- local_page(page_file(use_hovercraft(), button("one", 60, 60, as = "link", can_pin = FALSE)), environment())
    got <- show(page)
    # A real button with the look of a link: italic, underlined, no visible border, in the tip's own colour.
    expect_identical(got$tag, "BUTTON")
    expect_identical(got$words, "More")
    expect_identical(got$expanded, "false")
    expect_identical(got$italic, "italic")
    expect_identical(got$underline, "underline")
    expect_match(got$border, "rgba\\(0, 0, 0, 0\\)")
    expect_identical(got$colour, got$box_colour)
    # At the start (the left in English) of the bottom of the tip, not in the corner at its end.
    expect_lt(got$from_left, 14)
    expect_gt(got$from_right, got$from_left + 10)
    expect_gte(got$height, 24)
    press_more(page)
    expect_true(page$wait_until(long_in(page, hint_sel)))
    expect_identical(text_of(page, ".hovercraft-more-strip-button"), "Less")
    expect_identical(control(page)$expanded, "true")
    # The M key does the same as the click, and the item announces it.
    expect_identical(page$evaluate("document.getElementById('one').getAttribute('aria-keyshortcuts')"), "M")
  })

  it("keeps the button in the corner at the end by default, and puts either control on the side asked for", {
    cases <- list(
      list(args = list(), left = FALSE),
      list(args = list(where = "start"), left = TRUE),
      list(args = list(as = "link", where = "end"), left = FALSE),
      list(args = list(as = "link"), left = TRUE)
    )
    for (case in cases) {
      page <- local_page(
        page_file(use_hovercraft(), do.call(button, c(list("one", 60, 60, can_pin = FALSE), case$args))),
        environment()
      )
      got <- show(page)
      if (case$left) {
        expect_lt(got$from_left, 14, label = jsonlite::toJSON(case$args, auto_unbox = TRUE))
        expect_gt(got$from_right, got$from_left + 10, label = jsonlite::toJSON(case$args, auto_unbox = TRUE))
      } else {
        expect_lt(got$from_right, 14, label = jsonlite::toJSON(case$args, auto_unbox = TRUE))
        expect_gt(got$from_left, got$from_right + 10, label = jsonlite::toJSON(case$args, auto_unbox = TRUE))
      }
    }
  })

  it("follows the reading direction: the start is the right in a right-to-left page", {
    link <- local_page(
      page_file(use_hovercraft(), button("one", 60, 60, as = "link", can_pin = FALSE), dir = "rtl"),
      environment()
    )
    got <- show(link)
    expect_lt(got$from_right, 14)
    expect_gt(got$from_left, got$from_right + 10)
    corner <- local_page(
      page_file(use_hovercraft(), button("one", 60, 60, can_pin = FALSE), dir = "rtl"),
      environment()
    )
    got <- show(corner)
    expect_lt(got$from_left, 14)
    expect_gt(got$from_right, got$from_left + 10)
  })

  it("takes the tip's colour in the dark too, and the words of label", {
    page <- local_page(
      page_file(use_hovercraft(), button("one", 60, 60, as = "link", label = "Details", can_pin = FALSE)),
      environment()
    )
    page$emulate("prefers-color-scheme", "dark")
    got <- show(page)
    expect_identical(got$words, "Details")
    expect_identical(got$colour, got$box_colour)
  })

  it("is a tab stop in a pinned tip, and the key opens and closes the long form there", {
    page <- local_page(page_file(use_hovercraft(), button("one", 60, 60, as = "link")), environment())
    expect_true(page$wait_until(ready))
    page$evaluate("window.Hovercraft.pin(document.getElementById('one'), { by: 'reader', via: 'key' }); 0")
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", pin_sel)))
    expect_true(page$wait_until("document.querySelector('.hovercraft-more-strip-button') !== null"))
    expect_identical(page$evaluate("document.querySelector('.hovercraft-more-strip-button').tabIndex"), 0L)
    expect_identical(page$evaluate("document.querySelector('.hovercraft-more-strip-button').tagName"), "BUTTON")
    page$evaluate("document.querySelector('%s').focus(); 0" |> sprintf(pin_sel))
    page$press("m", wait = 0)
    expect_true(page$wait_until(long_in(page, pin_sel)))
    page$press("m", wait = 0)
    expect_true(page$wait_until(sprintf("!%s", long_in(page, pin_sel))))
  })

  it("refuses as and where where there is no control", {
    expect_error(tip_more("Long", on = "click", as = "link"), "no control|none", class = "hovercraft_error_argument")
    expect_error(tip_more("Long", on = "hover", where = "start"), class = "hovercraft_error_argument")
  })
})

# The link inside the card of a row's panel (inst/table/table.css): at the start of the card's inner edge, below the text,
# and at the end when asked.
describe("the More link in a row's panel", {
  skip_if_not_installed("DT")
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("callr")
  skip_if_not_installed("httpuv")
  skip_if_not_installed("pkgload")
  scores <- data.frame(team = c("Owls", "Foxes", "Hares"), wins = c(7, 5, 2))
  app <- start_app(
    function(scores) {
      long <- "The long form of {{ team }}. It runs a few lines so the card has some body to it and wraps at narrow widths."
      spec <- function(...) {
        tip_spec("**{{ team }}**: {{ wins }} wins", pin = TRUE, to = "in_place", more = tip_more(long, ...))
      }
      ui <- bslib::page_fluid(
        use_hovercraft(max_pins = 6),
        shiny::div(style = "max-width: 360px", DT::DTOutput("start"), DT::DTOutput("end"), DT::DTOutput("corner"))
      )
      server <- function(input, output, session) {
        dt <- function(rows, id) {
          DT::renderDT(
            tip_table(DT::datatable(scores, rownames = FALSE, options = list(dom = "t")), rows = rows, id = id),
            server = FALSE
          )
        }
        output$start <- dt(spec(as = "link"), "start")
        output$end <- dt(spec(as = "link", where = "end"), "end")
        output$corner <- dt(spec(), "corner")
      }
      shiny::shinyApp(ui, server)
    },
    scores = scores
  )
  page <- local_page(app$url, environment())
  row_of <- function(id) sprintf("#%s tbody tr[data-hovercraft-tip-id=\"%s/row/2\"]", id, id)
  measure <- function(id) {
    jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => {
        const panel = document.querySelector('#%s [data-hovercraft-panel]');
        const card = panel.querySelector('.hc-tip');
        const b = panel.querySelector('.hovercraft-more-strip-in button');
        const style = getComputedStyle(card);
        const r = card.getBoundingClientRect(); const q = b.getBoundingClientRect();
        const texts = [...card.querySelectorAll('.hc-tip-text')];
        return JSON.stringify({
          from_left: q.left - r.left - parseFloat(style.paddingLeft) - parseFloat(style.borderLeftWidth),
          from_right: r.right - q.right - parseFloat(style.paddingRight) - parseFloat(style.borderRightWidth),
          inside: q.top >= r.top && q.bottom <= r.bottom,
          below_text: q.top >= texts[texts.length - 1].getBoundingClientRect().bottom - 1,
          italic: getComputedStyle(b).fontStyle
        });
      })()",
      id
    )))
  }
  for (case in list(
    list(id = "start", left = TRUE, italic = "italic"),
    list(id = "end", left = FALSE, italic = "italic"),
    list(id = "corner", left = FALSE, italic = "normal")
  )) {
    it(paste("sits inside the card, at the", if (case$left) "start" else "end", "of its inner edge:", case$id), {
      id <- case$id
      withr::defer({
        page$evaluate("window.Hovercraft.close(); 0")
        expect_true(page$wait_until("document.querySelectorAll('[data-hovercraft-panel]').length === 0"))
      })
      selector <- row_of(id)
      expect_true(page$wait_until(sprintf("document.querySelector('%s')?.hasAttribute('tabindex') === true", selector)))
      page$evaluate(sprintf("document.querySelector('%s').focus()", selector))
      page$press("p", wait = 0)
      expect_true(page$wait_until(sprintf(
        "document.querySelector('#%s [data-hovercraft-panel] .hovercraft-more-strip-in button') !== null",
        id
      )))
      m <- measure(id)
      info <- paste(id, jsonlite::toJSON(m, auto_unbox = TRUE))
      expect_true(m$inside, label = info)
      expect_true(m$below_text, label = info)
      expect_identical(m$italic, case$italic, label = info)
      if (case$left) {
        expect_lte(abs(m$from_left), 4, label = info)
      } else {
        expect_lte(abs(m$from_right), 4, label = info)
      }
    })
  }

  it("has no console error", {
    expect_identical(page$problems(), character())
  })
})
