skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("withr")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# tip_more() with several pins open: each pinned tip has its own More / Less button, the M key acts on the pinned tip that
# holds the focus, a hint that opens elsewhere leaves the pinned tips' buttons alone, and `keys = c(more = FALSE)` leaves
# the key to the page (the button stays).
describe("the More button with several pinned tips", {
  place <- function(x, style) htmltools::div(style = paste("position: absolute;", style), x)
  item <- function(id, left, top, pin = TRUE) {
    place(
      tip_on(
        htmltools::tags$button(id, id = id),
        paste("Short", id),
        more = paste("LONG words of", id, "that go on for a while, so the tip has to grow to hold them all of them."),
        id = id,
        pin = if (pin) TRUE,
        placement = "bottom"
      ),
      sprintf("left: %dpx; top: %dpx;", left, top)
    )
  }
  page_file <- function(..., .env = parent.frame()) {
    file <- withr::local_tempfile(fileext = ".html", .local_envir = .env)
    htmltools::save_html(
      htmltools::tags$html(htmltools::tags$head(), htmltools::tags$body(style = "min-height: 700px;", ...)),
      file
    )
    file
  }
  boxes_js <- paste(
    "(() => {",
    "  const buttons = [...document.querySelectorAll('.hovercraft-more-strip-button')];",
    "  const inside = (b, r) => { const q = b.getBoundingClientRect(); const x = q.left + q.width / 2; const y = q.top + q.height / 2;",
    "    return x >= r.left && x <= r.right && y >= r.top && y <= r.bottom; };",
    "  return JSON.stringify([...document.querySelectorAll('.hc-tip-pin:not([hidden])')].map((box) => {",
    "    const r = box.getBoundingClientRect();",
    "    return { short: box.textContent.match(/Short (one|two|hov)/)?.[1] ?? null, long: box.textContent.includes('LONG'),",
    "      buttons: buttons.filter((b) => inside(b, r)).map((b) => b.textContent) };",
    "  }));",
    "})()"
  )
  # What each pinned box says and the buttons that sit on it, by the item it is for.
  state <- function(page) {
    got <- jsonlite::fromJSON(page$evaluate(boxes_js), simplifyVector = FALSE)
    stats::setNames(got, vapply(got, function(x) x$short %||% "?", ""))
  }
  # Presses the button that sits on the box of this item.
  press <- function(page, id) {
    page$evaluate(sprintf(
      paste(
        "(() => { const box = [...document.querySelectorAll('.hc-tip-pin:not([hidden])')].find((b) => b.textContent.includes('Short %s'));",
        "const r = box.getBoundingClientRect();",
        "const btn = [...document.querySelectorAll('.hovercraft-more-strip-button')].find((b) => { const q = b.getBoundingClientRect();",
        "  const x = q.left + q.width / 2; const y = q.top + q.height / 2; return x >= r.left && x <= r.right && y >= r.top && y <= r.bottom; });",
        "btn.click(); })()"
      ),
      id
    ))
  }
  # Waits for the box of this item to be long or short and to have this button on it (the strip follows a box that moves
  # when its neighbour grows, on the next frame).
  is_now <- function(page, id, long, button) {
    page$wait_until(sprintf(
      "JSON.parse(%s).some((b) => b.short === '%s' && b.long === %s && b.buttons.length === 1 && b.buttons[0] === '%s')",
      boxes_js,
      id,
      tolower(as.character(long)),
      button
    ))
  }
  pin <- function(page, id) {
    page$evaluate(sprintf(
      "window.Hovercraft.pin(document.getElementById('%s'), { by: 'reader', via: 'key' }); 0",
      id
    ))
  }

  it("gives each pinned tip its own button, and the two grow and shrink on their own", {
    page <- local_page(
      page_file(
        use_hovercraft(max_pins = 3),
        item("one", 40, 40),
        item("two", 400, 40),
        item("hov", 40, 400, pin = FALSE)
      ),
      environment()
    )
    expect_true(page$wait_until("window.HovercraftMore !== undefined && window.Hovercraft !== undefined"))
    pin(page, "one")
    pin(page, "two")
    expect_true(page$wait_until("document.querySelectorAll('.hovercraft-more-strip-button').length === 2"))
    got <- state(page)
    expect_setequal(names(got), c("one", "two"))
    for (id in c("one", "two")) {
      expect_identical(unlist(got[[id]]$buttons), "More")
      expect_false(got[[id]]$long)
    }
    # One grows; the other stays short, and each button says what its own tip is.
    press(page, "one")
    expect_true(page$wait_until(paste0("JSON.parse(", boxes_js, ").some((b) => b.short === 'one' && b.long)")))
    expect_true(is_now(page, "one", TRUE, "Less"))
    expect_true(is_now(page, "two", FALSE, "More"))
    # The second grows too, then the first shrinks while the second stays.
    press(page, "two")
    expect_true(page$wait_until(paste0("JSON.parse(", boxes_js, ").every((b) => b.long)")))
    press(page, "one")
    expect_true(page$wait_until(paste0("JSON.parse(", boxes_js, ").some((b) => b.short === 'one' && !b.long)")))
    expect_true(is_now(page, "one", FALSE, "More"))
    expect_true(is_now(page, "two", TRUE, "Less"))
    # Both pins are still open, the page said nothing was wrong.
    expect_identical(page$evaluate("document.querySelectorAll('[data-hovercraft-tip-open=\"pin\"]').length"), 2L)
    expect_identical(page$problems(), character())
  })

  it("leaves the pinned tips' buttons alone while a hint opens on a third item", {
    page <- local_page(
      page_file(
        use_hovercraft(max_pins = 3),
        item("one", 40, 40),
        item("two", 400, 40),
        item("hov", 700, 400, pin = FALSE)
      ),
      environment()
    )
    expect_true(page$wait_until("window.HovercraftMore !== undefined && window.Hovercraft !== undefined"))
    pin(page, "one")
    pin(page, "two")
    expect_true(page$wait_until("document.querySelectorAll('.hovercraft-more-strip-button').length === 2"))
    page$hover("#hov")
    expect_true(page$wait_until("document.querySelectorAll('.hovercraft-more-strip-button').length === 3"))
    got <- state(page)
    expect_setequal(names(got), c("one", "two"))
    for (id in c("one", "two")) {
      expect_identical(unlist(got[[id]]$buttons), "More")
    }
    # The hint's own button opens the hint's long form without touching the pins.
    page$evaluate(
      "[...document.querySelectorAll('.hovercraft-more-strip-button')].find((b) => !b.closest('.hc-tip-pin') && b.getBoundingClientRect().left > 600).click(); 0"
    )
    expect_true(page$wait_until(
      "[...document.querySelectorAll('.hc-tip:not(.hc-tip-pin):not([hidden])')].some((b) => b.textContent.includes('LONG'))"
    ))
    got <- state(page)
    expect_false(got$one$long)
    expect_false(got$two$long)
  })

  it("lets the M key act on the pinned tip that holds the focus", {
    page <- local_page(
      page_file(use_hovercraft(max_pins = 3), item("one", 40, 40), item("two", 400, 40)),
      environment()
    )
    expect_true(page$wait_until("window.HovercraftMore !== undefined && window.Hovercraft !== undefined"))
    pin(page, "one")
    pin(page, "two")
    expect_true(page$wait_until("document.querySelectorAll('.hovercraft-more-strip-button').length === 2"))
    focus_box <- function(id) {
      page$evaluate(sprintf(
        "[...document.querySelectorAll('.hc-tip-pin:not([hidden])')].find((b) => b.textContent.includes('Short %s')).focus(); 0",
        id
      ))
    }
    focus_box("two")
    page$press("m")
    expect_true(page$wait_until(paste0("JSON.parse(", boxes_js, ").some((b) => b.short === 'two' && b.long)")))
    got <- state(page)
    expect_false(got$one$long)
    focus_box("one")
    page$press("m")
    expect_true(page$wait_until(paste0("JSON.parse(", boxes_js, ").every((b) => b.long)")))
    focus_box("two")
    page$press("m")
    expect_true(page$wait_until(paste0("JSON.parse(", boxes_js, ").some((b) => b.short === 'two' && !b.long)")))
    expect_true(state(page)$one$long)
  })

  it("does nothing on the M key when the key is switched off, and the button still works", {
    page <- local_page(
      page_file(use_hovercraft(keys = c(more = FALSE)), item("one", 40, 40)),
      environment()
    )
    expect_true(page$wait_until("window.HovercraftMore !== undefined && window.Hovercraft !== undefined"))
    pin(page, "one")
    expect_true(page$wait_until("document.querySelectorAll('.hovercraft-more-strip-button').length === 1"))
    page$evaluate("document.querySelector('.hc-tip-pin:not([hidden])').focus(); 0")
    page$press("m")
    page$hold(0.3) # the key must not toggle a pinned tip: nothing happening has no signal
    expect_false(state(page)$one$long)
    press(page, "one")
    expect_true(page$wait_until(paste0("JSON.parse(", boxes_js, ").some((b) => b.long)")))
    # The key is on again for a page that does not switch it off.
    on <- local_page(page_file(use_hovercraft(), item("one", 40, 40)), environment())
    expect_true(on$wait_until("window.HovercraftMore !== undefined && window.Hovercraft !== undefined"))
    pin(on, "one")
    expect_true(on$wait_until("document.querySelectorAll('.hovercraft-more-strip-button').length === 1"))
    on$evaluate("document.querySelector('.hc-tip-pin:not([hidden])').focus(); 0")
    on$press("m")
    expect_true(on$wait_until(paste0("JSON.parse(", boxes_js, ").some((b) => b.long)")))
  })
})
