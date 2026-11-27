# hovercraft test bench

One Shiny app with every check a person or a browser test needs: the Shiny practices, the app's own JavaScript
changing the page, and a spot for each behaviour scenario (HC-001 to HC-077).

## Run it

From the package folder:

```r
shiny::runApp("tests/testthat/apps/testbench", port = 8148)
```

Then open <http://127.0.0.1:8148>. It loads hovercraft from this checkout. It needs shiny, bslib, DT, reactable,
shinyjs, promises and later (all in the package's Suggests).

## The tabs

| Tab | What it holds |
| --- | --- |
| Shiny practices | renderUI, insertUI and removeUI, modules, hidden containers, a modal, a full-screen card, input updates, dynamic tabs, bindCache, the iframe (`?host=1`) |
| JavaScript-driven UI | custom messages, `Shiny.setInputValue()`, HTML put in by JavaScript then bound, `Shiny.renderContent()`, moved, replaced and removed nodes, shinyjs, Bootstrap's own JavaScript, a widget's render callback, the app's own message names |
| Layering, Placement, Keys pointer touch, Tables, Shiny host, Accessibility, Theme, Scale and leaks | the behaviour scenarios, each in a spot with its id, its steps, what to expect and the tests that cover it |
| Table updates, Leak counts | no scenarios: the tables and redraws that `test-practice-table-browser.R` and `test-leak-browser.R` drive (how DT and reactable tips follow changed data, DT's inputs with and without `tip_table()`, DT's escaping, and what a redraw leaves behind). Their ids start with `updates_` and `leaks_`; the leak tab is meant to run alone with `?only=leaks&max_pins=2` |

Each spot says whether a test covers it ("Tested", "Partly tested", "No test yet", "By hand only"). Open "Steps" to
see what to do and what to expect.

## The address

| Setting | Does |
| --- | --- |
| `?tab=<id>` | opens on that tab |
| `?only=<id>` | draws that tab alone (the tests use it; the scale tab is lighter this way) |
| `?host=1` | the bench inside an iframe |
| `?strip=0` or `1` | hides or shows the log strip (shown on the whole bench, hidden with `?only`) |
| `?dir=rtl` | a right-to-left page |
| `?theme=dark` | starts dark |
| `?font=large` | a 24 px root font with a serif |
| `?keys=no_pin`, `no_more`, `off` | turns the P key, the M key or both off |
| `?max_pins=2` | how many pins stay open |
| `?labels=custom` | other words for More, Less, Close, Pin and Copy |
| `?style=<preset>` | the page's look, one of `tip_presets()` |

A spot that needs one of these has a link that opens it.

## The log

The strip at the bottom lists every tip event (hint shown or hidden, tip pinned, closed or updated, copy, link), every
input the page sends, every custom message from the server and every page error, newest first. Clear empties it. The
same lines are in `window.benchLog` for the browser's console and for the tests.

## Tests that use it

`start_bench()` and `bench_page()` in `tests/testthat/helper-bench.R`. `test-bench.R` checks that the scenario list and
the spots match; `test-bench-browser.R`, `test-bench-js-browser.R`, `test-practice-browser.R`,
`test-practice-table-browser.R` and `test-leak-browser.R` drive it in Chrome, and
the scenarios are tested tab by tab in `test-bench-<tab>-browser.R` (helpers in `helper-bench-scenarios.R`). The `tests`
column of `scenarios.csv` names the file for each scenario.
