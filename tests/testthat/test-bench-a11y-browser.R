skip_if_no_browser(bench_packages)
skip_if_no_bench()

# The bench's accessibility scenario that had no test (#158): Escape dismisses a hover tip without moving the focus or
# the pointer, and the tip stays away until the pointer leaves and comes back (HC-061). HC-065 and HC-066 are passes a
# person does by hand with a screen reader and the keyboard; they have no browser test.
describe("the bench's accessibility scenarios", {
  bench <- start_bench(environment())

  it("dismisses a hover tip with Escape, keeps the focus, and shows it again only after a leave and return (HC-061)", {
    page <- bench_page(bench, "a11y")
    bench_scroll_to(page, "#a11y-escape")
    page$evaluate("document.getElementById('a11y-escape').focus(); 0")
    expect_identical(page$hover("#a11y-escape"), "Escape closes me")
    since <- page$mark()
    page$press("Escape", wait = 0)
    expect_true(page$wait_closed())
    expect_identical(page$evaluate("document.activeElement.id"), "a11y-escape")
    page$hold(2) # the scenario's two seconds without a move: the tip must stay away, which has no signal
    expect_length(page$shown(), 0L)
    box <- bench_rect(page, "#a11y-escape")
    at <- c(box$left + 10, (box$top + box$bottom) / 2)
    page$mouse(at[[1]] + 1, at[[2]])
    page$hold(0.7) # the engine's open delay: a move of 1 px must not bring the tip back
    expect_length(page$shown(), 0L)
    expect_identical(page$evaluate("document.activeElement.id"), "a11y-escape")

    page$mouse(890, 690)
    page$hold(0.2) # the engine's close delay, for the item the pointer leaves
    page$mouse(at[[1]], at[[2]])
    expect_true(page$wait_open())
    expect_identical(page$shown(), "Escape closes me")
    expect_true(any(startsWith(bench_tip_events(page, since), "hidden a11y_escape_tip escape")))
    expect_bench_clean(page)
  })
})
