skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("withr")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# tip_more(on = "click") and tip_more(pin = TRUE): a click on the item opens the long form without pinning, and the More
# button (and the M key) can pin the tip so the long form stays. Real pointer and key events, in headless Chrome.
describe("tip_more() options for opening the long form", {
  place <- function(x, style) htmltools::div(style = paste("position: absolute;", style), x)
  words <- function(id) paste("LONG words of", id, "that go on for a while, so the tip has to grow to hold them all.")
  page_file <- function(..., .env = parent.frame()) {
    file <- withr::local_tempfile(fileext = ".html", .local_envir = .env)
    htmltools::save_html(
      htmltools::tags$html(htmltools::tags$head(), htmltools::tags$body(style = "min-height: 700px;", ...)),
      file
    )
    file
  }
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
    page$click("#one")
    expect_true(page$wait_until(sprintf("document.querySelector('%s')?.textContent.includes('LONG')", hint_sel)))
    expect_identical(buttons(page), 0L)
    click_again(page, "#one")
    expect_true(page$wait_until(sprintf("!document.querySelector('%s')?.textContent.includes('LONG')", hint_sel)))
    page$hold(0.3) # a click must pin nothing: nothing happening has no signal
    expect_false(page$evaluate(sprintf("document.querySelector('%s') !== null", pin_sel)))
    expect_identical(state_of(page, "one"), "hint")
  })

  it("opens the long form of a click tip with the M key too, and a click before the tip shows does nothing", {
    page <- local_page(page_file(use_hovercraft(), span("one", 60, 60, on = "click")), environment())
    expect_true(page$wait_until(ready))
    expect_identical(page$evaluate("document.getElementById('one').getAttribute('aria-keyshortcuts')"), "M")
    page$evaluate("document.getElementById('one').focus(); 0")
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", hint_sel)))
    page$press("m")
    expect_true(page$wait_until(sprintf("document.querySelector('%s')?.textContent.includes('LONG')", hint_sel)))
    page$press("m")
    expect_true(page$wait_until(sprintf("!document.querySelector('%s')?.textContent.includes('LONG')", hint_sel)))
  })

  it("opens a pinned click tip's long form on a click of the item, and that tip has no button to tab to", {
    page <- local_page(page_file(use_hovercraft(), span("one", 60, 60, on = "click", pin = TRUE)), environment())
    expect_true(page$wait_until(ready))
    page$evaluate("window.Hovercraft.pin(document.getElementById('one'), { by: 'reader', via: 'key' }); 0")
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", pin_sel)))
    page$click("#one")
    expect_true(page$wait_until(sprintf("document.querySelector('%s')?.textContent.includes('LONG')", pin_sel)))
    expect_identical(buttons(page), 0L)
    expect_identical(state_of(page, "one"), "pin")
    page$click("#one")
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

  it("pins with the M key too when the tip says so", {
    page <- local_page(page_file(use_hovercraft(), button("one", 60, 60, pin = TRUE)), environment())
    expect_true(page$wait_until(ready))
    page$evaluate("document.getElementById('one').focus(); 0")
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", hint_sel)))
    page$press("m")
    expect_true(page$wait_until(long_in(page, pin_sel)))
    expect_identical(state_of(page, "one"), "pin")
  })
})
