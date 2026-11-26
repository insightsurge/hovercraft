skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if_not_installed("DT")
skip_if_not_installed("reactable")
skip_if_not_installed("shinyjs")
skip_if_not_installed("promises")
skip_if_not_installed("later")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")
skip_if_no_bench()

# The test bench itself in a browser: every tab loads alone and together with nothing on the console, the log records
# the engine's events and the page's inputs, and the address's settings reach the page.
describe("the test bench in a browser", {
  bench <- start_bench(environment())
  tabs <- c("practice", "js", "layering", "placement", "input", "tables", "host", "a11y", "theme", "scale")

  it("loads every tab alone, and the whole bench, with nothing on the console", {
    for (tab in tabs) {
      page <- bench_page(bench, tab)
      expect_true(page$wait_until(sprintf("document.querySelector('.tab-pane[data-value=\"%s\"]') !== null", tab)))
      page$settle()
      expect_identical(page$problems(), character(), info = tab)
    }
    page <- bench_page(bench)
    expect_true(page$wait_until("document.getElementById('bench-log') !== null"))
    expect_true(page$wait_until(sprintf(
      "document.querySelectorAll('.tab-pane[data-value]').length >= %d",
      length(tabs)
    )))
    page$settle()
    expect_identical(page$problems(), character())
  })

  it("logs a hint shown and hidden, and an input the page sends", {
    page <- bench_page(bench, "a11y")
    since <- page$mark()
    expect_identical(page$hover("#a11y-escape"), "Escape closes me")
    expect_true(page$log_has("hint", "a11y_escape_tip", state = "shown", since = since))
    page$mouse(890, 690)
    expect_true(page$log_has("hint", "a11y_escape_tip", state = "hidden", since = since))
    page$click_now("#a11y_toggle")
    expect_true(page$log_has("input", "a11y_toggle", since = since))
    rows <- page$log(since)
    expect_true(all(vapply(rows, function(row) row$n > since, logical(1))))
  })

  it("takes page-wide settings from the address: right to left, and two pins", {
    page <- bench_page(bench, "layering", list(max_pins = 2, dir = "rtl"))
    expect_true(page$wait_until("document.documentElement.dir === 'rtl'"))
    since <- page$mark()
    page$evaluate("document.getElementById('spot-HC-006').scrollIntoView({ block: 'center', behavior: 'instant' }); 0")
    page$settle(0.1)
    page$hover("#lay-touch-1")
    page$press("p")
    page$hover("#lay-touch-2")
    page$press("p")
    expect_true(page$log_has("tip", "lay_touch_1_tip", state = "pinned", since = since))
    expect_true(page$log_has("tip", "lay_touch_2_tip", state = "pinned", since = since))
    expect_true(page$wait_until(sprintf(
      "document.querySelectorAll('.%s-pin:not([hidden])').length === 2",
      tip_names()$css
    )))
  })
})
