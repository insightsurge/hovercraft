# Tests

Tests live in `tests/testthat/`, one file per topic, named `test-<topic>.R`. Run them all with `devtools::test()`, or
in RStudio choose Build, then Test Package. Learn more: <https://r-pkgs.org/testing-basics.html>.

## Test file kinds

Most tests go in a plain `test-<topic>.R` file and run everywhere. Some tests can't run everywhere, so
they go in a file whose name ends in a kind. The examples next to each are in `tests/testthat/`:

| File name ends in | What it holds | Example | Runs in |
| --- | --- | --- | --- |
| `.R` (plain) | Quick tests that run everywhere | `test-say_hello.R` | `r-cmd-check`, `coverage`, `full-tests`, `r-cmd-check-extended` |
| `-browser.R` | Tests that drive a browser (chromote); `local_page()` opens a tab in one shared Chrome (see "Browser tests" below) | `test-demo-browser.R` | `full-tests` |
| `-snap.R` | Snapshot tests (`expect_snapshot()`): the output is saved in `_snaps/` and compared on later runs | `test-say_hello-snap.R` | `full-tests` |
| `-extended.R` | Slow or detailed tests | `test-say_hello-extended.R` | `coverage`, `full-tests`, `r-cmd-check-extended` |

`full-tests` runs every kind and fails if any test skips there. CRAN skips every kind but the plain one. Snapshot tests
skip on CRAN by themselves; start each other file, or each test in it, with a skip line, so it skips there too:

```r
skip_if_no_browser()                                              # -browser.R files (skips on CRAN and without Chrome)
skip_if_not(identical(Sys.getenv("EXTENDED_TESTS"), "true"), "an extended test")   # -extended.R files
```

## On your computer

`devtools::test()` runs the plain, snapshot and browser tests. The extended ones run too when `EXTENDED_TESTS` is
`true`: the package's `.Renviron` sets it when one of your checks runs them in CI. `.Renviron` is kept out of git, so a
fresh clone doesn't have it; to run the extended tests there, add the line `EXTENDED_TESTS=true` to a `.Renviron` in
the package's folder, or run:

```r
Sys.setenv(EXTENDED_TESTS = "true")
devtools::test()
```

When a snapshot changes on purpose, accept the new one with `testthat::snapshot_accept()`, or look first with
`testthat::snapshot_review()`. Commit the files in `tests/testthat/_snaps/`: CI compares with them, and refuses to
create a missing one.

## Browser tests

`helper-browser.R` starts one headless Chrome per test process and `local_page()` opens a tab in it, so a test never
waits for a Chrome to start. A Chrome that has died is replaced (twice at most per run). Every wait uses one limit,
`browser_timeout` (30 seconds): wait for a signal (`page$wait_until()`, `page$settle()`) and never for a fixed time
(`page$hold()` is the exception; its comment says why). Click anything Shiny can redraw with
`page$click_now()`, which finds and clicks in one step in the page. Apps under test start with `start_app()` (its `make`
function builds the app) and are stopped when the test ends. A file starts with `skip_if_no_browser()` (extra packages
as `skip_if_no_browser("DT")`). `page$hover()` fails the test when the element's own hint does not open within that limit; pass `opens = FALSE` where
none is expected. `page$press()`, `page$click()`, `page$click_now()` and `page$click_id()` take `until =`, a JavaScript
expression to wait for after the action (it fails the test when it never holds); `press()` and `click()` otherwise hold
a fixed time unless you pass `wait = 0`, for a test that goes on to wait for a signal. A hold is for a check that
nothing happens, and `page$settle()` does not tell that a change happened. `test-helper-browser-support.R` fails where
an action is read before a wait; `test-helper-browser-waits-browser.R` tests the failures of the waits themselves. The
page also has the named waits `wait_connected()`, `wait_pin_button()`, `wait_pinned()`, `wait_unpinned()`,
`wait_shown()`, `wait_text()` and `wait_reported()`, and the readers `rect()`, `centre()` and `reported()`;
`page_file()` and `place()` build a small static page. A browser test covers what
hovercraft handles (an input or a state it acts on), not what the browser does alone, such as a middle click or a pinch.
Each test file must run on its own, as files run in parallel and in any order.

The test bench in `apps/testbench/` holds the Shiny-practice and behaviour scenarios in one app. Start it in a test with
`start_bench()` and open one tab with `bench_page(bench, "<tab>")` (`helper-bench.R`); its README says how to run it by
hand. The behaviour scenarios are tested in `test-bench-<tab>-browser.R`, with `helper-bench-scenarios.R`; each ends with
`expect_bench_clean()` (no tip left open, no `inert` or locked scroll, nothing on the console). For input the page
helpers do not cover (a touch scroll, a drag), `page$session` is the tab's chromote session.
The bench's scripts and styles (`apps/testbench/www`) are checked with `biome ci .`; the quality gate and CI check only
`inst/` until launchR's Biome step is widened.
