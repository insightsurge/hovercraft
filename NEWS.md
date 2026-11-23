# hovercraft 0.1.0

* `tip_on()` puts a tip on an HTML tag: plain text, or a named template filled from a record.
* `tip_define()` and `tip_template()` build and name templates, with `tip_title()`, `tip_text()`, `tip_badge()`,
  `tip_fields()` and `tip_code()` for the blocks. Placeholders are checked when the template is made.
* `tip_rows()` turns a data frame into one record per row, for a tip on every row of a table.
* `tip_dependency()` gives the tip engine's script and CSS as an HTML dependency.
* `tip_theme()` and `tip_dependency(theme = )` set how the tips look: roles for light and dark (fill, words, border, code
  background, corner, shadow, typeface), a `bslib::bs_theme()`, or CSS variables. The tip follows `data-bs-theme`, so it turns
  dark with `bslib::input_dark_mode()`. Words below 4.5:1 on their fill give a `hovercraft_contrast` warning.
* The tip is now a shade of its page (Bootstrap's tertiary background with the page's text colour, in light and in dark), no longer the page inverted. A `bslib::bs_theme()` given to `tip_dependency()` is read the same way, and the contrast check measures a role you leave out against that default. The bundled engine is now 0.48.0 and ships `tip.json` and its five preset files.
* Two vignettes, "Get started with hovercraft" and "Themes".
* The tip has a small arrow pointing at its tag, on by default and drawn when its fill is opaque. Tips also work inside modal windows, and the engine now lets the nearer setting win when settings repeat (#31).
* `tip_theme(font = )` now sets the engine's own tip font variable, so a theme's font beats the font of a preset (#31).
* `tip_template()` stops with the "comes with a later version" error for a plain-list `group` or `host` block, or a block with the engine's new `parent`, `kicker` or `meta` fields, which have no R constructor yet; `tip_template_fields()` reads the title's new placeholders (#31).
