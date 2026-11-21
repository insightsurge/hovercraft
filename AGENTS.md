# Working on this package

Rules for everyone who works here, person or AI. This file loads in every AI session, so a new session can start from
it alone. The detailed rules are in `.agents/rules/`: read the file that fits before you start.

| Before you change | Read |
| --- | --- |
| R code in `R/` | `.agents/rules/r-code.md` |
| Tests in `tests/` | `.agents/rules/tests.md` |
| Help pages, README, NEWS or articles | `.agents/rules/docs.md` |
| `DESCRIPTION`, CI, or before a pull request | `.agents/rules/checks.md` |
| JavaScript or CSS in `inst/` | `.agents/rules/js-css.md` |
| Branches, commits and pull requests | `.agents/rules/git.md` |

How launchR's checks, CI report and upgrades work: <https://launch-r.web.app/SKILL.md>.

## What this package is

hovercraft puts a tip on any HTML tag in a Shiny app or a bslib page. The tip shows when the pointer rests on the tag
or keyboard focus arrives. It holds plain text or a named template filled in from one record, such as a table row.
Pinned tips (click to keep open, a Copy button, Shiny inputs), themes and charts inside a pin are the later parts. The
package is private for now and not on CRAN; the version is `0.1.0`.

The R side builds tags and writes data attributes; a small JavaScript file does the showing. R reads the engine's names
and rules from `engine.json` and does not retype them; the one port, the placeholder parser, is held to the engine by
shared test cases.

| File in `R/` | Holds |
| --- | --- |
| `tip.R` | `tip_on()`: writes the tag's attributes, checks the template and record, warns about a tag with no focus stop |
| `template.R`, `blocks.R` | `tip_define()`, `tip_template()`, the block constructors (`tip_title()`, `tip_text()`, `tip_badge()`, `tip_fields()`, `tip_code()`) |
| `json.R`, `rows.R`, `placeholders.R` | Records as JSON (`tip_rows()` for a data frame), the placeholder parser |
| `engine.R` | `tip_dependency()` and the readers of `inst/engine/engine.json` (`tip_names()`, `tip_rules()`) |
| `theme.R`, `theme-colour.R` | `tip_theme()`, the bslib bridge, the contrast check |
| `abort.R`, `arguments.R` | Classed errors and warnings (`hovercraft_error_*`, `hovercraft_warning_*`), argument checks |

## The engine under `inst/engine/`

The JavaScript and CSS are a built copy of an engine that lives in a separate private source repository. They are
copied in by `tools/copy-engine.R`, which only the maintainer runs, from a clean checkout of that repository:
`Rscript tools/copy-engine.R <path to the checkout>`, with `ENGINE_BRANCH` set when the commit is not on `main`.

- **Never edit anything under `inst/engine/` by hand**, and never rename its names in R. Run the script again.
- The script refuses a dirty checkout, a commit that is not on `origin/<ENGINE_BRANCH>` and a build that still holds
  the engine's own names, then builds the engine under this package's names (prefix `hovercraft`, CSS class prefix
  `hc`) and records what it copied.
- `engine.json` records the version, the commit, the branch, each file's sha256, the names the page uses
  (`attribute`, `css`, `global`, `prefix`, `product`) and the engine's tooltip rules. R reads names and rules from it
  (`tip_names()`, `tip_rules()`); do not write an attribute name, a class name or a rule such as the template-name
  pattern into R code.
- Only `inst/engine/www/` (`tip.js`, `tip.css`) is given to a page, so no commit or branch reaches a rendered page.
  `engine.json`, `placeholders.json` and `tip.sha256` stay beside it. The engine's page contract, `tip.json` (the
  attributes, the `kind` pattern, the refused tags and the events with their typed fields), is read the same way once
  the script copies it.
- `placeholders.json` is a shared set of cases for the placeholder grammar; the tests run the R parser over it.
- The public text of this package (README, help pages, vignettes, `DESCRIPTION`, `NEWS.md`) calls it "the tip engine"
  and never names the project it is built from.

## Names, records and checks

- **Names:** a template's name follows the engine's pattern (lower-case letters, digits and `_`, not a word R reserves);
  `tip_define()` reads the pattern from `engine.json`. The attribute name comes from `tip_names()$attribute`; its suffixes
  (`-template`, `-record`) are the one place R still writes names itself, until it reads them from `tip.json`.
- **Records:** a record is one row or a named list, written as JSON with 15 significant digits. Once `tip_define()` has
  seen a template, `tip_on()` keeps only the columns the template names, so a review app does not ship the whole row.
  `tip_rows(data, columns = )` does the same for a table. Data frame and matrix columns are refused with a classed
  error; `NULL` is written as `null` and shows as a dash.
- **Placeholders:** `{{ field }}`, with one filter at most: `number:n` or `percent:n`. `tip_index` and `tip_id` are the
  engine's own fields.
- **Themes:** `tip_theme()` takes roles (`bg`, `ink`, `line`, `code_bg`, `radius`, `shadow`, `font`) for light and dark,
  or reads them from a bslib theme, writes CSS variables, and warns (`hovercraft_contrast`) under a 4.5:1 contrast.
- **Errors and warnings** are classed and say the fix. A new one needs a test and a snapshot where it prints.
- **Tests** live in `tests/testthat/`, one file per R file, with `-snap` files for printed output. The browser tests
  (`*-browser.R`) drive headless Chrome through chromote and skip on CRAN or without Chrome; keep Chrome's profile in
  `tempdir()`.

## Issues and pull requests

Every change starts from an issue and lands by pull request; the maintainer merges, squashing. The tree: root #1
("About this repository", with the set-up, before-public and before-CRAN checklists #2 to #4) and the epics below. A
new issue uses an issue form, gets one type label and one `area:` label, and is linked as a sub-issue of the epic it
belongs to. The pull request uses the template: it starts with "In brief", says "Closes #n", and lists the checks run.

| Epic | Is about |
| --- | --- |
| #5 Tooltips on any tag | `tip_on()`, templates, records, `tip_reason()` for disabled controls, placement |
| #10 Themes | `tip_theme()` and the bslib bridge; the options-based theme |
| #12 Pins, Copy and Shiny events | `tip_on(pin =, id =, kind =)`, Copy, pins and copies as Shiny inputs |
| #15 Release | licence and details, `tools/check-release.R`, the release checklist |
| #18 Charts inside a pinned tip | later; echarts and plotly in a pin |

## Running the checks

Use launchR's quality gate (`gate <mode>`, see the link above), never `devtools::check()` on its own. Send its output to
a log file outside the repository and read the summary lines.

| When | Mode |
| --- | --- |
| After editing roxygen, or `DESCRIPTION`'s Title or Description | `devtools::document()`, commit `man/` and `NAMESPACE` |
| Before every commit | `lint` |
| Before every push | `fast` |
| Before a push that touches `R/` or `tests/` | `tests` |
| Before a pull request, and when asked whether it is ready for CRAN | `full` |

`tools/check-release.R` is the release gate, run by hand before any release (`Rscript tools/check-release.R`). It fails
while the engine was not copied from `main`, a vendored file does not match `engine.json`, public text names the
engine's project, the maintainer address is a `noreply` address, a `TODO(founder)` is left, or `URL`, `BugReports` or
the pkgdown `url` is missing or does not load. It is not a test, so the normal tests never skip; it is expected to fail
while the repository is private.

## Always

- Change only what the task needs. Ask before adding a dependency, changing a check or deleting a file.
- `man/` and `NAMESPACE` come from `devtools::document()`, never from hand edits.
- Add a test for every change, and a `NEWS.md` bullet for anything users notice.
- Never push to the default branch (`main` or `master`), force-push or skip a check. When a check fails, fix the
  cause; don't loosen the check.
- Keep secrets, private data and machine paths out of code, tests, examples and commits.
- Report what you ran and what it printed, failures and skipped steps included.
