---
paths:
  - "tests/**"
---

# Tests

- Tests use testthat, in `tests/testthat/`, one file per R file: `R/clean.R` is tested in `test-clean.R`. Test kinds
  (browser, snapshot, extended) and how to run them are in `tests/README.md`.
- **Every new behaviour and every fixed bug gets a test.** For a bug, the test fails before the fix and passes after.
- **Every R line you add is run by a test.** The quality gate checks this. `# nocov` is only for a line that can't
  be tested, with a comment saying why.
- **Don't change or delete an existing test to make it pass.** If a test is wrong, say so and ask.
- Use made-up data. No network or files outside `tempdir()` in plain tests; anything that needs them gets a skip line
  (`skip_on_cran()`, `skip_if_offline()`).
- A test that uses a `Suggests` package starts with `skip_if_not_installed("pkg")`.
- Clean up with `withr::local_*()`: temporary files, options and environment variables.
- **Snapshots:** commit the files in `tests/testthat/_snaps/`. Accept a changed snapshot only when the change is on
  purpose, and say so in the pull request.
- Coverage has a lower limit, set in `DESCRIPTION` (`Config/quality-gate/coverage`). Don't lower it.

- **Browser tests** drive headless Chrome through chromote. They skip on CRAN and where Chrome is missing. A Shiny app
  under test runs in a background R process (callr).
  - **One Chrome per test process, shared** by the files that process runs. `local_page()` opens a tab in it, closed
    when the test ends; never start a Chrome in a test. Its profile and temporary files are in `tempdir()`. A Chrome
    whose process has ended is replaced when a test asks for it; after 2 replacements in a run the browser tests skip
    with a message instead of looping.
  - **One wait limit,** `browser_timeout` (30 s), for every browser wait; starting Chrome gets 60 s. A wait that passes
    returns at once, so a long limit costs nothing on a fast machine. No wait has a shorter limit of its own, except the
    0.2 s that `hover()` gives the tip the pointer leaves to close.
  - **No fixed sleeps.** Wait for the signal the test needs: `page$wait_until()` for a DOM state or value,
    `page$settle()` after an action that redraws (the page is quiet and Shiny is idle), `page$wait_open()` and
    `page$wait_closed()` for tips. Put each wait in `expect_true()`, so a timeout fails the test. `page$hold()` is for
    what no signal can tell, that nothing happens or that the engine's own timer has run out; give it a comment saying
    which.
  - **`settle()` is not a signal that a change happened,** only that nothing changed for a moment: a change that
    starts late (a Shiny round trip, the engine's timer) is not under way yet. An action that changes something says
    what it changes: `page$click()`, `click_now()`, `click_id()` and `press()` take `until =`, a JavaScript expression
    they wait for (and fail, naming the action and the expression, when it never holds); after a `mouse()` click,
    call `wait_until()` yourself. Where the action must change nothing, hold on purpose (`wait = 0.6` for a key,
    `0.7` for a click, or `page$hold()`) with a comment. For a Shiny round trip that changes nothing you can
    see, count `shiny:idle` events before the action and wait for the count to rise.
  - **`hover()` fails the test when the element's own hint does not open** (the engine marks the tipped element
    `-open="hint"`; the hint of the tip the pointer left does not count), within the shared limit. The failure names the
    selector and says whether no hint of its own showed or it was still moving. Where no tip is expected (a tag without
    a tip, a disabled tip, a check that nothing opens), pass `opens = FALSE`: it holds the engine's open delay and
    returns. `click()` and `hover_in_view()` take `opens` too. A test that then reads what a closing tip changes waits
    for it (`wait_closed()`, or `wait_until()` on the value).
  - **A guard test** (`test-helper-browser-support.R`) reads the browser test files as text and fails, listing
    `file:line`, where a click, key, scroll, resize, focus, redraw or Shiny input is followed by a measurement with no
    wait, `until =` or hold in between. `test-helper-browser-waits-browser.R` tests that the waits fail, and say what
    they waited for, when their signal never comes.
  - **Apps under test** start through `start_app()`: its `make` function builds the app (`shiny::shinyApp(ui, server)`)
    in the background process, and `start_app()` stops it in the calling frame, pass or fail. Only a server that is not
    a Shiny app starts with `callr::r_bg()` and `wait_for_app()`, with the same limit, and stops with
    `withr::defer(app$kill())` right after it starts.
  - **Click what Shiny can redraw in one step:** `page$click_now(selector)` finds and clicks in the page, while
    `page$click()` and `mouse_at()` read a position first and press there, so a redraw in between misses.
  - **Test only what the package handles.** A browser test is for an input or a state hovercraft itself acts on: a
    hover, a key, a click, a tap, a resize, a Shiny input. Never for what the browser does without hovercraft (middle
    click, pinch zoom, autoscroll, a rotation or a zoom level that reaches the page only as a window size), however much
    the scenario asks for it: such a test costs days when it flakes on one platform, and it protects nothing.
  - **Write a new browser test with the shared helpers** in `tests/testthat/helper-browser.R` (read the comments there):
    `skip_if_no_browser()` as the file's first line (`skip_if_no_browser(c("DT", "reactable"))` for extra packages;
    a file-only skip such as `skip_if_no_bench()` goes under it), `start_app()` for an app, `page_file()` and `place()`
    for a small static page, and the page's named waits: `wait_connected()`, `wait_pin_button()`, `wait_pinned()`,
    `wait_unpinned()`, `wait_shown()`, `wait_text()`, `wait_reported()`. `page$press()` and `page$click()` hold 0.6 and
    0.7 s unless you pass `until =` (wait for what the action changes) or `wait = 0` (the test goes on to wait for a
    signal itself); keep the hold, written out (`wait = 0.6`), where it checks that nothing happens. Do not copy a
    helper (`rect`, `centre`, `seen`) into a file: use the page's own, or add it to the helper.
  - Each file runs on its own: files run in parallel, in any order, so a file never relies on another's Chrome tabs,
    apps or state.
  - **One browser run at a time.** Two test runs at once (two sessions, or a gate and a test file) slow each other and
    make the waits that are right look flaky: start one, wait for it to end, then start the next.
  - **A short-lived state** (loading, a flash, an animation) is recorded in the page or held by the test (the app waits
    for a signal the test sends, such as a file it deletes); it is never polled after a `hover()`, `press()` or
    `click()`, and an app's own `Sys.sleep()` is not a state to wait for. (Older tests that still poll are listed in
    #175.)
  - **A frame-by-frame comparison** of positions or opacities allows for one dropped frame (compare with the frame
    before as well), and puts the worst frames in the failure message. Do not widen a number to pass.
  - **A test that defines or reads templates** (`tip_define()`, `tip_known()`) starts with `tip_reset()` and defers one
    (`withr::defer(tip_reset())`), and never relies on another file having cleaned up.
- **The engine's shared cases** (`inst/engine/placeholders.json`) are run through the R parser; a new placeholder rule
  needs a case there first, in the engine's source, not here.
