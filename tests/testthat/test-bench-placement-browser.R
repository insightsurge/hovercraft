skip_if_no_browser(bench_packages)
skip_if_no_bench()

# The bench's placement scenarios that had no test (#158): a pin in a scroll box (HC-010), a window resize with pins
# open (HC-011), enlarged text (HC-013, partly), and very long content (HC-014). Zoom levels and rotation are not
# tested: they reach hovercraft as a window resize, which the HC-011 test covers.
describe("the bench's placement scenarios", {
  bench <- start_bench(environment())

  # Is every tip drawn (or every pinned one) inside a window of this size, with no sideways page scroll? A tip whose item
  # has scrolled out of the window is hidden, and the engine does not place it in the window.
  in_view <- function(page, width, height, pinned = FALSE) {
    tips <- bench_tips(page)
    tips <- tips[tips$drawn, , drop = FALSE]
    if (pinned) {
      tips <- tips[tips$pinned, , drop = FALSE]
    }
    page_width <- page$evaluate("document.documentElement.scrollWidth")
    nrow(tips) > 0L &&
      all(tips$left >= -0.5 & tips$top >= -0.5 & tips$right <= width + 0.5 & tips$bottom <= height + 0.5) &&
      page_width <= width
  }
  # Does the tip sit beside its item, above or below it without covering it, or as a bottom sheet?
  beside <- function(page, text, selector) {
    tips <- bench_tips(page)
    tip <- tips[tips$text == text, , drop = FALSE]
    item <- bench_rect(page, selector)
    nrow(tip) == 1L && (tip$side == "sheet" || tip$top >= item$bottom - 0.5 || tip$bottom <= item$top + 0.5)
  }
  # Presses the More (or Less) button on the pinned tip on show.
  press_more <- function(page) {
    page$click_now(".hovercraft-more-strip-button")
    page$settle()
  }

  it("moves a pin with its item in a scroll box and the page, and hides it once the item is out (HC-010)", {
    page <- bench_page(bench, "placement")
    bench_scroll_to(page, "#place-scroll")
    page$hover("#place-scroll")
    page$press("p", wait = 0)
    expect_true(bench_wait_tips(page, "I follow my item"))
    gap <- function() bench_tips(page)$top - bench_rect(page, "#place-scroll")$bottom
    first <- gap()
    scroll_box <- function(js) {
      page$evaluate(sprintf("document.getElementById('place-scroll').parentElement.%s; 0", js))
      page$settle()
    }

    scroll_box("scrollTop += 50")
    expect_equal(gap(), first, tolerance = 1)
    expect_true(bench_tips(page)$drawn)
    scroll_box("scrollTop += 300")
    expect_lt(bench_rect(page, "#place-scroll")$bottom, bench_rect(page, "#spot-HC-010 .border")$top)
    expect_false(bench_tips(page)$drawn)
    scroll_box("scrollTop = 30")
    expect_true(bench_tips(page)$drawn)
    expect_equal(gap(), first, tolerance = 1)
    page$evaluate("window.scrollBy(0, 40); 0")
    page$hold(0.3) # the tip must keep its place when the page scrolls: nothing happening has no signal
    page$settle()
    expect_equal(gap(), first, tolerance = 1)
    expect_identical(bench_tip_events(page)[grepl("closed", bench_tip_events(page))], character())
    expect_bench_clean(page)
  })

  it("keeps pins in the window and beside their items through resizes (HC-011)", {
    page <- bench_page(bench, "placement", list(max_pins = 2))
    page$resize(1280, 800)
    bench_scroll_to(page, "#place-resize")
    page$hover("#place-resize")
    page$press("p", wait = 0)
    expect_true(bench_wait_tips(page, "I stay with my item"))
    page$hover("#place-sheet-a")
    page$press("p", wait = 0)
    expect_true(bench_wait_tips(page, c("I stay with my item", "Sheet A")))
    page$resize(700, 800)
    expect_true(wait_polls(function() in_view(page, 700, 800), browser_wait()))
    expect_true(beside(page, "I stay with my item", "#place-resize"))
    expect_true(beside(page, "Sheet A", "#place-sheet-a"))

    # The sizes below are walked with one pin; the step with two pins narrowed below 600 px comes last.
    page$evaluate(sprintf(
      "[...document.querySelectorAll('.%1$s-pin:not([hidden])')].find((t) => t.textContent.includes('Sheet A'))
        .querySelector('.%1$s-close').click(); 0",
      tip_names()$css
    ))
    # Narrowing the window with two pins still open is the last step, not this one: it would hang a page of an engine
    # that still has the freeze (#164) and take the test process with it.
    if (!expect_true(bench_wait_tips(page, "I stay with my item"))) {
      return()
    }
    for (size in list(c(599, 800), c(375, 667), c(1280, 800))) {
      page$resize(size[[1]], size[[2]])
      bench_scroll_to(page, "#place-resize")
      # Pins only: the resting pointer may land on another item once the layout moves, and its hint is not this step's.
      expect_true(in_view(page, size[[1]], size[[2]], pinned = TRUE), info = paste(size, collapse = "x"))
      expect_true(beside(page, "I stay with my item", "#place-resize"), info = paste(size, collapse = "x"))
    }
    tips <- bench_tips(page)
    expect_identical(tips$side[tips$pinned] == "sheet", FALSE)

    # Two pins open when the window narrows below 600 px, with one item below the fold (#164): the page keeps answering,
    # one sheet stays and the older pin closes. (With 0.59.0 of the tip engine this froze the page for good.)
    expect_true(bench_close_tips(page))
    page$resize(1280, 800)
    bench_scroll_to(page, "#place-resize")
    page$hover("#place-resize")
    page$press("p", wait = 0)
    expect_true(bench_wait_tips(page, "I stay with my item"))
    page$hover("#place-sheet-a")
    page$press("p", wait = 0)
    expect_true(bench_wait_tips(page, c("I stay with my item", "Sheet A")))
    since <- page$mark()
    page$resize(599, 667)
    expect_true(page$wait_until(sprintf(
      "document.querySelectorAll('.%s-pin:not([hidden])').length === 1",
      tip_names()$css
    )))
    page$settle()
    expect_true(max(bench_rect(page, "#place-resize")$top, bench_rect(page, "#place-sheet-a")$top) > 667)
    expect_identical(
      bench_tip_events(page, since)[startsWith(bench_tip_events(page, since), "closed")],
      "closed place_resize_tip replaced"
    )
    expect_identical(bench_tips(page)$text[bench_tips(page)$pinned], "Sheet A")
    expect_bench_clean(page)
  })

  it("keeps a hint, a pin and its long form readable with large text (HC-013, partly)", {
    page <- bench_page(bench, "placement", list(max_pins = 2))
    page$evaluate("document.documentElement.style.fontSize = '32px'; 0")
    page$settle()
    bench_scroll_to(page, "#place-zoom")
    page$hover("#place-zoom")
    page$press("p", wait = 0)
    expect_true(bench_wait_tips(page, "A hint, a pin and a long form"))
    press_more(page)
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin:not([hidden])')?.textContent.includes('Line 10')"))
    expect_true(in_view(page, 900, 700))
    # Off its item here too (#166): the long form is capped and scrolls, and the item stays visible.
    long_form <- bench_tips(page)
    long_form <- long_form[long_form$pinned & long_form$drawn, , drop = FALSE]
    item <- bench_rect(page, "#place-zoom")
    expect_identical(nrow(long_form), 1L)
    expect_true(long_form$top >= item$bottom - 0.5 || long_form$bottom <= item$top + 0.5)
    expect_bench_clean(page)
  })

  it("wraps a 500-character word and keeps a 60-line long form in the window (HC-014, partly)", {
    page <- bench_page(bench, "placement")
    bench_scroll_to(page, "#place-word")
    word <- strrep("Unbroken", 63L)
    expect_identical(page$hover("#place-word"), word)
    expect_true(in_view(page, 900, 700))
    wraps <- "(() => { const t = document.querySelector('.hc-tip:not([hidden])'); return t.scrollWidth <= t.clientWidth; })()"
    expect_true(page$evaluate(wraps))
    page$press("p", wait = 0)
    expect_true(bench_wait_tips(page, word))
    expect_true(in_view(page, 900, 700))
    expect_true(page$evaluate(wraps))
    expect_true(bench_close_tips(page))

    bench_scroll_to(page, "#place-sixty")
    page$hover("#place-sixty")
    page$press("p", wait = 0)
    expect_true(page$wait_pinned())
    press_more(page)
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin:not([hidden])')?.textContent.includes('Line 60')"))
    expect_true(in_view(page, 900, 700))
    # The long form scrolls inside the tip, so its last line can be reached.
    expect_true(page$evaluate(
      "(() => { const t = document.querySelector('.hc-tip-pin:not([hidden])');
        const s = [t, ...t.querySelectorAll('*')].find((n) => n.scrollHeight > n.clientHeight + 1 &&
          ['auto', 'scroll'].includes(getComputedStyle(n).overflowY));
        return s !== undefined; })()"
    ))
    expect_bench_clean(page)
  })
})
