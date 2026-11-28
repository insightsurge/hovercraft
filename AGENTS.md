# Working on this package

hovercraft puts a tip on any HTML tag in a Shiny app or a bslib page: plain text, or a named template filled in from
one record, such as a table row. R builds the tags and writes data attributes; a small JavaScript engine, vendored
under `inst/engine/`, does the showing. The package is young and private, and its files may still move.

Rules for everyone who works here, person or AI. Claude loads the matching rule file in `.claude/rules/` by itself; any
other AI reads the file for its area in full before starting, and before creating a file in `R/`, `tests/`, `inst/`
or `.github/`.

| Area | Rule file |
| --- | --- |
| `R/`, `tools/*.R` | `.claude/rules/r-code.md` |
| `tests/` | `.claude/rules/tests.md` |
| Help pages, README, NEWS, articles | `.claude/rules/docs.md` |
| `DESCRIPTION`, the gate, releases | `.claude/rules/checks.md` |
| `inst/`, the vendored engine | `.claude/rules/js-css.md` |
| Issues, branches and pull requests | `.claude/rules/git.md` |

How to contribute is in `.github/CONTRIBUTING.md`; launchR's checks: <https://launch-r.web.app/SKILL.md>.

## Always

- Never edit the vendored engine in `inst/engine/`; `tools/copy-engine.R` writes it.
- Every export starts with `tip_`, except the two that carry the package's name: `use_hovercraft()` and
  `hovercraft_book()`. Names are snake_case.
- Public text never names the project the engine is built from; call it "the tip engine".
- Check arguments with checkmate, send messages through cli, call other packages as `pkg::fn()`.
- `man/` and `NAMESPACE` come from `devtools::document()`, never from hand edits.
- Every change starts from an issue and lands by pull request; never push to `main`, force-push or skip a check.
- **Every feature goes in the book:** a pull request that adds, removes or changes an export or an argument (its
  name, values, default or behaviour) also updates hovercraft book (`inst/book/`, opened by `hovercraft_book()`) in
  the same pull request; `test-book.R` fails while an export or argument is in no section. How the book is built:
  "The book" in `.github/CONTRIBUTING.md`.
- Change only what the task needs. Ask before adding a dependency, changing a check or deleting a file.
- Keep secrets, private data and machine paths out of code, tests, examples and commits.
- Report what you ran and what it printed, failures and skipped steps included.
