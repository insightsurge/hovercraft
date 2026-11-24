---
paths:
  - "R/**/*.R"
  - "tools/**/*.R"
---

# R code

- **Every exported function** has roxygen2 docs with `@param` for each argument, `@returns`, and an `@examples` that
  runs offline in a few seconds. Internal helpers get `@noRd`.
- **Other packages:** call them as `pkg::fn()`, after adding them to `DESCRIPTION`. `Imports` for what the package
  needs; `Suggests` for what is optional, checked first with `rlang::check_installed()` or `requireNamespace()`.
  Never `library()` or `require()` in `R/`, and never `:::` into another package.
- **Ask before adding a dependency.** Prefer base R or a package already in `DESCRIPTION`.
- **Check arguments** at the top of exported functions. Error messages say what went wrong and what to do next
  (`cli::cli_abort()`).
- **Messages** go through cli (`cli::cli_inform()`, `cli::cli_warn()`), never `print()` or `cat()`; a `print()`
  method is the exception.
- **Leave the user's session as it was:** no `setwd()`, `options()`, `Sys.setenv()` or `set.seed()` without restoring
  it; use `withr::local_*()`. Write files only where the user asks, or under `tempdir()`.
- **Style:** Air formats the code (`air format .`, settings in `air.toml`; Air is installed on its own, see
  `checks.md`); lintr checks it (rules in `.lintr`).
  snake_case names, lines of at most 120 characters, `TRUE` and `FALSE` in full.
- Keep functions small and named for what they do. Comment why, not what.

## Names, records and checks

- **Prefix:** every exported function starts with `tip_`. The one exception is `hovercraft_book()`: it opens the
  package's book, not a tip, and a `tip_` name would read as a kind of tip.
- **Names come from the engine, not from R.** The vendored engine's names (the attribute, the CSS class prefix, the
  JavaScript global) and its rules (the template-name pattern, the placeholder grammar, the block shapes) are read from
  `inst/engine/engine.json` through the package's readers. Never write an attribute name, a class name or a rule into R
  code. The placeholder parser is the one port, and shared test cases hold it to the engine. The attribute suffixes
  (`-template`, `-record`) are the one place R still writes names itself, until it reads them from the engine's page
  contract (`tip.json`).
- **Template names** follow the engine's pattern (lower-case letters, digits and `_`, not a word R reserves).
- **Records** are one row or a named list, written as JSON with 15 significant digits. Once a template has been
  defined, a tag's record keeps only the columns that template names, so a review app does not ship the whole row.
  Data frame and matrix columns are refused with a classed error; `NULL` is written as `null` and shows as a dash.
- **Placeholders:** `{{ field }}` with one filter at most, `number:n` or `percent:n`; `tip_index` and `tip_id` are the
  engine's own fields.
- **Themes:** a theme takes roles for light and dark, or reads them from a bslib theme, writes CSS variables, and warns
  with the class `hovercraft_contrast` under a 4.5:1 contrast.
- **Errors and warnings** are classed (`hovercraft_error_*`, `hovercraft_warning_*`) and say the fix. Each has a test,
  and a snapshot where it prints.
- **Arguments** are checked with checkmate and wrapped in a classed error; messages go through cli. Names are
  snake_case, exported and internal.
