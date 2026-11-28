skip_if_no_browser(bench_packages)
skip_if_no_bench()

# The bench's Shiny-host scenario that had no test (#158): the session ends with a pin open (HC-058). The page is no
# longer connected afterwards, so this test waits on the page, not on Shiny being idle (`settle()` would wait for a
# Shiny that never answers again).
describe("the bench's Shiny host scenarios", {
  bench <- start_bench(environment())

  it("keeps tips, pins and Escape working with no error after the session ends (HC-058, partly)", {
    page <- bench_page(bench, "host", list(max_pins = 2))
    bench_scroll_to(page, "#host-words-own")
    bench_scroll_to(page, "#host-before-end")
    page$hover("#host-before-end")
    page$press("p", wait = 0)
    expect_true(bench_wait_tips(page, "Open when the session ends"))

    # The server ends the session (a click on the page's button would be a click away and close the pin first).
    page$evaluate("Shiny.setInputValue('host_end', 1, { priority: 'event' }); 0")
    expect_true(page$wait_until("document.getElementById('shiny-disconnected-overlay') !== null"))
    expect_true(page$wait_until("window.Shiny.shinyapp.isConnected() === false"))
    tips <- bench_tips(page)
    expect_identical(tips$text, "Open when the session ends")
    expect_true(tips$pinned && tips$drawn && tips$on_top)

    page$evaluate(
      "document.getElementById('host-words-own').scrollIntoView({ block: 'center', behavior: 'instant' }); 0"
    )
    expect_true(page$wait_until(
      "(() => { const r = document.getElementById('host-words-own').getBoundingClientRect();
        return r.top > 100 && r.bottom < innerHeight - 100; })()"
    ))
    expect_true("Pinned with its own words" %in% page$hover("#host-words-own"))
    tips <- bench_tips(page)
    expect_true(tips$on_top[tips$text == "Pinned with its own words"])
    page$evaluate("document.getElementById('host-words-own').focus(); 0")
    page$press("p", wait = 0)
    expect_true(page$wait_until(sprintf(
      "[...document.querySelectorAll('.%s-pin:not([hidden])')].some((t) => t.textContent.includes('Pinned with its own words'))",
      tip_names()$css
    )))
    expect_bench_clean(page)
    expect_length(Filter(function(row) identical(row$kind, "warn"), page$log(0)), 0L)
  })
})
