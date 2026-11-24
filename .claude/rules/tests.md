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
  - **One Chrome per test process, shared.** `local_page()` opens a tab in it, closed when the test ends; never start a
    Chrome in a test. Its profile and temporary files are in `tempdir()`. A Chrome whose process has ended is replaced
    when a test asks for it; after 2 replacements in a run the browser tests skip with a message instead of looping.
  - **One wait limit,** `browser_timeout` (30 s), for every browser wait; starting Chrome gets 60 s. A wait that passes
    returns at once, so a long limit costs nothing on a fast machine. Don't give one wait a shorter limit of its own.
  - **No fixed sleeps.** Wait for the signal the test needs: `page$wait_until()` for a DOM state or value,
    `page$settle()` after an action that redraws (the page is quiet and Shiny is idle), `page$wait_open()` and
    `page$wait_closed()` for tips. `page$hold()` is for what no signal can tell, that nothing happens or that the
    engine's own timer has run out; give it a comment saying which.
  - **Apps under test** start through `start_app()`, or `wait_for_app()` after `callr::r_bg()`, with the same limit,
    and stop with `withr::defer(app$kill())` right after they start, pass or fail.
  - Each file runs on its own: files run in parallel, in any order, each in a process with its own Chrome.
- **The engine's shared cases** (`inst/engine/placeholders.json`) are run through the R parser; a new placeholder rule
  needs a case there first, in the engine's source, not here.
