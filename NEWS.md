# hovercraft 0.1.0

* `tip_on()` puts a tip on an HTML tag: plain text, or a named template filled from a record.
* `tip_define()` and `tip_template()` build and name templates, with `tip_title()`, `tip_text()`, `tip_badge()`,
  `tip_fields()` and `tip_code()` for the blocks. Placeholders are checked when the template is made.
* `tip_rows()` turns a data frame into one record per row, for a tip on every row of a table.
* `tip_dependency()` gives the tip engine's script and CSS as an HTML dependency.
* `tip_theme()` and `tip_dependency(theme = )` set how the tips look: roles for light and dark (fill, words, border, code
  background, corner, shadow, typeface), a `bslib::bs_theme()`, or CSS variables. The tip follows `data-bs-theme`, so it turns
  dark with `bslib::input_dark_mode()`. Words below 4.5:1 on their fill give a `hovercraft_contrast` warning.
* The tip is now a shade of its page (Bootstrap's tertiary background with the page's text colour, in light and in dark), no longer the page inverted. A `bslib::bs_theme()` given to `tip_dependency()` is read the same way, and the contrast check measures a role you leave out against that default. The bundled engine is 0.42.0 and ships `tip.json`.
* Two vignettes, "Get started with hovercraft" and "Themes".
