# Contributing to hovercraft

Thank you for helping with hovercraft. To report a bug or suggest an idea, open an issue with one of the issue forms.
To change something yourself, read the workflow below and open a pull request.

## Setup

- **R**: the current release is recommended. The minimum version is in `DESCRIPTION`.
- **Development packages**: from the package root, run `pak::local_install_dev_deps()`.
- **Air**, the formatter for R code: <https://posit-dev.github.io/air/>. The settings are in `air.toml`.
- **Biome**, the formatter and linter for JavaScript and CSS in `inst/`: <https://biomejs.dev>. The settings are in
  `biome.json`.
- **Google Chrome or Chromium**, for the browser tests.
- If you use renv, set up your own library; the repository does not ship one.

The checks you run on your computer are described under "Before you open a pull request" below.

## Workflow

1. **Start with an issue**, using an issue form. State the goal and a "done when" checklist. One issue per piece of
   work; ask first if the change is large. **Every issue sits under an epic** (a sub-issue of it); only the root issue
   has no parent, and if no epic fits, ask which to use. Keep the description current when scope or decisions change.
   Tick the items a pull request delivers, and say what stays open.
2. **Make a branch** from `main` for that issue.
3. **Commit with Conventional Commits**, short: `feat:`, `fix:`, `docs:`, `chore:`, `test:`, `ci:`, `refactor:`.
4. **Open one pull request for the issue**, using the template. Its body starts with "In brief": one plain sentence on
   what changes and why, then a few bullets for a reader with no context, and "Closes #n". Keep the sections that
   apply, put repeatable checks in its table and one-off observations in Notes.
5. **Suggest a label** in the pull request: one of `feature`, `enhancement`, `bug`, `documentation` or `chore`. The maintainer
   applies it; the release notes are built from it.
6. **The maintainer merges** once CI is green. Pull requests are squash-merged, so the pull request title becomes the
   commit message on `main`: keep it a Conventional Commit.

Update every text that describes what you changed, in the same pull request: help pages (through roxygen2), articles,
the README, `NEWS.md` and the examples; workflow comments and the instruction files if the repository has them
(`AGENTS.md`, `CLAUDE.md`); and any other page or file that describes the behaviour, including ones added later. It
works both ways: if you edit an article or an example, check that the help page still agrees. Search the repository
for the old wording and list the search terms in the pull request.

Changes reach `main` through pull requests. Never force-push, push to `main`, or skip checks with `--no-verify`. CI on
a pull request from a fork waits for a maintainer to approve the run. Leave "Allow edits by maintainers" on, so a
maintainer can fix a small thing, such as a typo or a failing format check, on your branch.

Stacked pull requests are fine. Base the second one on the branch below it, and say so in its description.

## Before you open a pull request

From the package root:

```r
devtools::document()   # help pages and NAMESPACE, from the roxygen2 comments
devtools::test()       # the tests
devtools::check()      # the same R CMD check that CI runs, with the same pass mark
```

```sh
air format .           # R code style
```

- Commit the files `devtools::document()` changes together with the source.
- An unknown word fails the spelling check: list it in your pull request, and the maintainer adds agreed words to
  `inst/WORDLIST`.
- The spelling check reads `Language: en-GB` in `DESCRIPTION`, so it expects British spelling. For American spelling
  (color, license), change it to `en-US`.
- To run the same checks CI runs, including style, docs in sync and spelling, use launchR's quality gate
  (<https://launch-r.web.app/SKILL.md>), from the package root: mode `lint` before a commit, `fast` before a push,
  `tests` when you touched `R/` or `tests/`, and `full` before the pull request. Send its output to a file and read the
  PASS, FAIL and SKIP lines.
- A failing check on a pull request is read from its log in the Checks tab. Fix the first failure and push again.

## Code in short

- **Style:** the tidyverse style guide, formatted by Air (`air.toml`) and linted by lintr (`.lintr`). Names are
  snake_case, lines at most 120 characters.
- Write help and offline examples with roxygen2. Every exported function documents its return value.
- Call other packages as `pkg::fn()` and add them to `DESCRIPTION` first.
- Check arguments at the top of exported functions, and give messages that say what to do next.
- Keep JavaScript and CSS in `inst/` plain and readable. Bundle what the package needs; do not load anything from a
  CDN.
- Accessibility: meaning never rests on colour alone. Honour `prefers-reduced-motion`.

## Tests

Tests use testthat 3rd edition, one test file per R file. Add a test for every new behaviour and every fixed bug, and keep example
data made up.
Browser and snapshot tests that CRAN skips live in their own files and run in CI.

## The studio

hovercraft studio (`inst/studio/`, opened by `tip_studio()`) shows every tip feature on one page, one numbered plate
per feature. A pull request that changes an export or an argument updates the studio in the same pull request.

- **Files.** `app.R` starts the app. Shiny loads every file in `R/` first: the shared helpers in `R/shared-*.R` (code
  as text, running it, the plate frame and its margin, the test steps and their proofs, the inputs log), one file
  per plate in `R/plate-<id>.R`, the made-up data in `R/data.R` and the page in `R/page.R`. `www/shared.css` and
  `www/shared.js` go with the shared helpers; `www/studio.css` and `www/studio.js` are the page's own look, its phone
  layout and Guide me.
- **One source of truth.** Each plate builds its R code as text from its choices (`<id>_code()`); the margin shows
  that text and the figure is the same text, run with only the package's exports in reach. A plate's server code
  (Live output's observers and render, Disabled's update, From the server's observers) is run from its shown text the same way.
  Code that would fail in a reader's app fails in the studio too. The one exception is the page's look on the Start
  plate: a dependency loads once per page, so the studio writes the attribute and variable that
  `use_hovercraft(style = )` writes, with the names from the engine's `tip.json`.
- **Test steps tick from real inputs.** A step is `step(text, bug, target, proof)`; a proof names the Shiny input that
  shows the step was done (`on_hint()`, `on_pin()`, `on_copy()`). By default a proof counts only the reader's own
  actions (`by = "reader"`), so a tip the page closes and pins again never ticks a step. A step without a proof is
  ticked by hand; a step with one is ticked only by its proof.
- **App code** calls other packages as `pkg::fn()`, and nothing in it writes an attribute or class name of the tip
  engine: they come from the engine's `tip.json`.
- **Tests:** `tests/testthat/test-studio.R` (the launcher, the page, every plate's code for every combination of its
  choices, the proofs)
  and `test-studio-browser.R` (the app in Chrome).

## The test bench

The test bench (`tests/testthat/apps/testbench/`) is one Shiny app with the Shiny-practice checks, a tab of
JavaScript-driven UI and a spot for every behaviour scenario (HC-001 to HC-077). A person runs it by hand for a
walkthrough, and the browser tests start it with `start_bench()` (`tests/testthat/helper-bench.R`). Its `README.md`
says how to run it and which settings its address takes.

- **Files.** `R/contents.R` lists the tabs; each tab `<id>` is `tab_<id>_ui()` and `tab_<id>_server()` in
  `R/tab-<id>.R`. `scenarios.csv` lists the scenarios and the tab of each; `spot()` in `R/shared-spot.R` draws one.
- **Ids** in a tab start with the tab's id, so all tabs can share a page. The practice tab keeps the ids its test uses.
- **Page-wide settings** of `use_hovercraft()` (keys, pins, labels, style) go in the address, never in a tab: the page
  reads them once.
- **The log** (`www/bench-log.js`) keeps every tip event, input and custom message in `window.benchLog`. A test reads it
  with `page$log_has()`, which waits, and checks console errors with `page$problems()`.
- **Tests:** `test-bench.R` (the list and the spots match, every tab draws and starts its server), `test-bench-browser.R`
  (every tab in Chrome), `test-bench-js-browser.R` and `test-practice-browser.R`; and one `test-bench-<tab>-browser.R`
  per scenario tab, each scenario's test ending with `expect_bench_clean()` (`helper-bench-scenarios.R`). A scenario's
  `tests` column in `scenarios.csv` names the file that covers it.

## NEWS.md

Add one bullet for each user-facing change (a new or changed function, argument or message), in the same pull request. Write it for users, in the present tense, start
with the function or feature, and end with the issue number:

```md
* `say_hello()` gains a `name` argument (#123).
```

Release headings and versions belong to the maintainer.

## Licence of contributions

Contributions use the package's licence (see `LICENSE`). Before adding third-party files, check that their licence
allows it, and say where they came from.

## Conduct and data

Everyone taking part follows the [code of conduct](CODE_OF_CONDUCT.md). Be kind and patient, and give feedback on the
work, not the person. For a security problem, do not open an issue; follow the [security policy](SECURITY.md). For
questions, use [Discussions](https://github.com/insightsurge/hovercraft/discussions) rather than an issue.

Use made-up data in examples and screenshots. Leave out private data, credentials and machine paths.
