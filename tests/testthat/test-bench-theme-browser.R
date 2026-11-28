skip_if_no_browser(bench_packages)
skip_if_no_bench()

# The bench's theme scenarios that had no test (#158): forced colours, as Windows high contrast sets them (HC-068), and
# a page with large text and a serif (HC-071).
describe("the bench's theme scenarios", {
  bench <- start_bench(environment())

  # The computed border, colours and outline of the first element on show that `selector` names.
  look <- function(page, selector) {
    jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => { const n = [...document.querySelectorAll(%s)].find((x) => !x.hidden && x.getClientRects().length);
        const s = getComputedStyle(n);
        return JSON.stringify({ border: parseFloat(s.borderTopWidth), style: s.borderTopStyle, border_colour: s.borderTopColor,
          colour: s.color, background: s.backgroundColor, outline: parseFloat(s.outlineWidth), outline_style: s.outlineStyle,
          outline_colour: s.outlineColor }); })()",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    )))
  }
  visible_border <- function(x) x$border > 0 && x$style != "none" && !grepl("rgba\\(.*, 0\\)$", x$border_colour)
  overlap <- function(a, b) a$left < b$right && b$left < a$right && a$top < b$bottom && b$top < a$bottom

  it("keeps the border, words, buttons and focus ring visible under forced colours (HC-068, partly)", {
    page <- bench_page(bench, "theme")
    page$emulate("forced-colors", "active")
    withr::defer(page$emulate("forced-colors", "none"))
    expect_true(page$evaluate("matchMedia('(forced-colors: active)').matches"))
    bench_scroll_to(page, "#theme-dark")
    expect_identical(page$hover("#theme-dark"), "Readable in both")
    hint <- look(page, paste0(".", tip_names()$css))
    expect_true(visible_border(hint))
    expect_false(identical(hint$colour, hint$background))
    expect_true(visible_border(look(page, ".hovercraft-pin-strip-button")))

    page$press("p", wait = 0)
    expect_true(bench_wait_tips(page, "Readable in both"))
    pin <- look(page, sprintf(".%s-pin", tip_names()$css))
    expect_true(visible_border(pin))
    expect_false(identical(pin$colour, pin$background))
    expect_true(visible_border(look(page, sprintf(".%s-pin .%s-close", tip_names()$css, tip_names()$css))))
    expect_true(visible_border(look(page, ".hovercraft-more-strip-button")))

    # From the item, Tab reaches the pin's close button, then its More button: each shows a ring.
    page$evaluate("document.getElementById('theme-dark').focus(); 0")
    for (stop in c("close", "More")) {
      page$press("Tab", wait = 0)
      page$settle(0.1)
      ring <- look(page, ":focus-visible")
      expect_true(ring$outline > 0 && ring$outline_style != "none", info = stop)
      expect_false(grepl("rgba\\(.*, 0\\)$", ring$outline_colour), info = stop)
    }
    expect_bench_clean(page)
  })

  it("sizes tips with a large page font and keeps the close button off the words and two pins apart (HC-071)", {
    page <- bench_page(bench, "theme", list(font = "large", max_pins = 2))
    expect_identical(page$evaluate("getComputedStyle(document.documentElement).fontSize"), "24px")
    # A tall window, so both items and their pins are in view together.
    page$resize(900, 1800)
    bench_scroll_to(page, "#theme-font")
    page$hover("#theme-font")
    hint_size <- page$evaluate(sprintf(
      "parseFloat(getComputedStyle(document.querySelector('.%s:not([hidden])')).fontSize)",
      tip_names()$css
    ))
    expect_gt(hint_size, 16)
    page$press("p", wait = 0)
    expect_true(page$wait_pinned())
    page$click_now(".hovercraft-more-strip-button")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s-pin:not([hidden])')?.textContent.includes('A long form in large text.')",
      tip_names()$css
    )))
    page$settle()
    # The close button sits clear of every line of words.
    clear <- page$evaluate(sprintf(
      "(() => { const t = document.querySelector('.%1$s-pin:not([hidden])');
        const c = t.querySelector('.%1$s-close').getBoundingClientRect();
        return [...t.querySelectorAll('p')].every((p) => { const r = document.createRange(); r.selectNodeContents(p);
          return [...r.getClientRects()].every((q) => q.right <= c.left || c.right <= q.left || q.bottom <= c.top || c.bottom <= q.top); }); })()",
      tip_names()$css
    ))
    expect_true(clear)

    page$hover("#theme-dark")
    page$press("p", wait = 0)
    expect_true(page$wait_until(sprintf(
      "document.querySelectorAll('.%s-pin:not([hidden])').length === 2",
      tip_names()$css
    )))
    page$settle()
    tips <- bench_tips(page)
    expect_true(all(tips$drawn))
    one <- as.list(tips[1, ])
    two <- as.list(tips[2, ])
    expect_false(overlap(one, two))
    for (item in c("#theme-font", "#theme-dark")) {
      expect_false(overlap(one, bench_rect(page, item)), info = item)
      expect_false(overlap(two, bench_rect(page, item)), info = item)
    }
    page$resize(900, 700)
    expect_bench_clean(page)
  })
})
