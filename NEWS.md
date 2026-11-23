# hovercraft 0.1.0

* `tip_on()` puts a tip on an HTML tag: plain text, or a named template filled from a record.
* `tip_define()` and `tip_template()` build and name templates, with `tip_title()`, `tip_text()`, `tip_badge()`,
  `tip_fields()` and `tip_code()` for the blocks. Placeholders are checked when the template is made.
* `tip_on()` looks inside the tag for something that can take keyboard focus before it warns, so the `div` Shiny puts
  around an input no longer gets a warning (#32).
* `tip_on()` gives an icon-only button, with no `aria-label` of its own, the words of a plain-text tip as its
  `aria-label`, so a screen reader has a name to read (#32). Engine 0.49.0 reads that name once: a tip that says the same as the tag's name is no longer also given as its description (#38).
* `tip_badge()` takes `status` (`"success"`, `"warning"`, `"danger"` or `"info"`; any other word is an error) in place
  of `tone`. The badge is coloured from the page's Bootstrap colours by a few lines of CSS that come with the engine's
  dependency (#32).
* `tip_table()` is an experimental spike that puts tips on the rows, cells and header cells of a `DT::datatable()` or `reactable::reactable()` table, drawn from each row's own values and following the data row through sorting, filtering and paging. It works by pointer only, with no keyboard reach yet, and its shape may change (#36). Its `id` is a Shiny id, the same as `tip_on()`'s will be: one string with no whitespace, so `ns()` ids work, and every marked row, cell and header cell gets its own tip id derived from it (#41). The engine's kind of the tips is fixed (`table_row`, `table_cell`, `table_header`) and no longer set by `id`.
* `tip_spec()` (experimental) gives one `tip_table()` target its own settings, for now a `placement`: as `rows`, or as an entry of the named list given to `cells` or `headers`. A setting left as `NULL` is taken from `tip_table()`; a cell follows the table's placement, not its row's (#41).
* `tip_dependency()` gives the tip engine's script and CSS as an HTML dependency.
* `tip_theme()` and `tip_dependency(theme = )` set how the tips look: roles for light and dark (fill, words, border, code
  background, corner, shadow, typeface), a `bslib::bs_theme()`, or CSS variables. The tip follows `data-bs-theme`, so it turns
  dark with `bslib::input_dark_mode()`. Words below 4.5:1 on their fill give a `hovercraft_contrast` warning.
* The tip is now a shade of its page (Bootstrap's tertiary background with the page's text colour, in light and in dark), no longer the page inverted. A `bslib::bs_theme()` given to `tip_dependency()` is read the same way, and the contrast check measures a role you leave out against that default. The bundled engine is now 0.49.0 and ships `tip.json` and its five preset files.
* Two vignettes, "Get started with hovercraft" and "Themes".
* The tip has a small arrow pointing at its tag, on by default and drawn when its fill is opaque (#31).
* `tip_theme(font = )` now sets the engine's own tip font variable, so a theme's font beats the font of a preset (#31).
* `tip_template()` stops with the "comes with a later version" error for a plain-list `group` or `host` block, or a block with the engine's new `parent`, `kicker` or `meta` fields, which have no R constructor yet; `tip_template_fields()` reads the title's new placeholders (#31).
