# the tip_table() errors / list the columns when a placeholder or a name is not one

    Code
      tip_table(dt(), rows = "{{ nope }}")
    Condition
      Error in `tip_table()`:
      ! `rows` names columns the table does not have.
      x Missing: "nope".
      i The table's columns are "team", "wins", and "losses".

---

    Code
      tip_table(dt(), cells = c(winz = "x"))
    Condition
      Error in `tip_table()`:
      ! `cells` names columns the table does not have: "winz".
      i The table's columns are "team", "wins", and "losses". Did you mean "wins"?

# the tip_table() errors / say what a header tip and a reserved name cannot do

    Code
      tip_table(dt(), headers = c(wins = "{{ wins }}"))
    Condition
      Error in `tip_table()`:
      ! `headers[["wins"]]` reads "wins", but a header tip has no row to read from.
      i Write the words of the header tip without placeholders.

---

    Code
      tip_table(dt(), rows = "{{ .value }}")
    Condition
      Error in `tip_table()`:
      ! `rows` names ".value", and names that start with a dot are kept for hovercraft.
      i Name a column of the table.

# the tip_table() errors / say what the widget and the content must be

    Code
      tip_table(scores, rows = "x")
    Condition
      Error in `tip_table()`:
      ! `widget` must be a DT table or a reactable table, not a data frame.
      i Build one with `DT::datatable()` or `reactable::reactable()`, then pipe it into `tip_table()`.

---

    Code
      tip_table(dt(), rows = 1)
    Condition
      Error in `tip_table()`:
      ! `rows` must be a string or a `tip_template()`, not a number.
      i Give the words of the tip, with `{{ column }}` to read a value of the row.

---

    Code
      tip_table(dt())
    Condition
      Error in `tip_table()`:
      ! There is no tip to put on the table.
      i Give `rows`, `cells` or `headers` some words.

# the tip_table() errors / say what an id and a spec must be

    Code
      tip_table(dt(), rows = "x", id = "two words")
    Condition
      Error in `tip_table()`:
      ! `id` is not right: Must be one non-empty string without whitespace or a colon.
      i Give one non-empty string with no spaces and no `:` (Shiny reads what follows a colon as the type of an input), such as `"scores"`, or `NULL`.

---

    Code
      tip_table(dt(), cells = tip_spec("x"))
    Condition
      Error in `tip_table()`:
      ! `cells` must be a named vector or list, one entry per column.
      i For example `cells = c(team = "words")`, or `cells = list(team = tip_spec("words"))`.

---

    Code
      tip_spec(1)
    Condition
      Error in `tip_spec()`:
      ! `content` must be a string or a `tip_template()`, not a number.
      i Give the words of the tip, or `NA` for no tip.

---

    Code
      tip_spec("x", placement = "above")
    Condition
      Error in `tip_spec()`:
      ! `placement` is not right: Must be element of set {'auto','top','bottom','left','right'}, but is 'above'
      i Give one of "auto", "top", "bottom", "left", "right", or `NULL`.

# the tip_table() errors / say what a table can mark

    Code
      tip_table(dt(), cells = c(wins = "x"), marker = "underline")
    Condition
      Error in `tip_table()`:
      ! A table draws only a "corner" marker for now, not "underline".
      i Use `marker = "corner"`, or mark the words with `tip_on()` outside the table.

---

    Code
      tip_table(dt(), rows = tip_spec("x", marker = "corner"))
    Condition
      Error in `tip_table()`:
      ! `rows` has a marker, but a row takes none.
      i Put the marker on `cells` or `headers`, where a reader can see it.

---

    Code
      tip_table(dt(), rows = "x", marker = "corner")
    Condition
      Error in `tip_table()`:
      ! `marker` has no cell or header cell to mark.
      i A row takes no marker. Give `cells` or `headers` some words too.

# the tip_table() errors / say what a pin and the events of a row need

    Code
      tip_table(dt(), rows = tip_spec("x", pin = "click"), id = "t")
    Condition
      Error in `tip_table()`:
      ! `pin` is "click", but a click on a table row selects it, so it cannot pin.
      i Give `pin = TRUE`: a reader pins a row's tip with the pin handle in its corner or with the pin key.

---

    Code
      tip_table(dt(), rows = tip_spec("x", pin = TRUE))
    Condition
      Error in `tip_table()`:
      ! `pin` needs an `id`: the table's inputs are named after it.
      i Give `tip_table()` an `id`, such as `id = "scores_tips"`.

---

    Code
      tip_table(dt(), cells = list(wins = tip_spec("x", pin = TRUE)), id = "t")
    Condition
      Error in `tip_table()`:
      ! `cells[["wins"]]` has a pin: only rows pin for now.
      i Give `pin` to the `tip_spec()` of `rows`, or leave it out.

---

    Code
      tip_table(dt(), rows = tip_spec("x", events = "pin"))
    Condition
      Error in `tip_table()`:
      ! `events` needs an `id`: the table's inputs are named after it.
      i Give `tip_table()` an `id`, such as `id = "scores_tips"`.

---

    Code
      tip_table(dt(), headers = list(wins = tip_spec("x", events = "pin")), id = "t")
    Condition
      Error in `tip_table()`:
      ! `headers[["wins"]]` has events: only rows send events for now.
      i Give `events` to the `tip_spec()` of `rows`, or leave it out.

---

    Code
      tip_table(dt(), rows = "x", id = "t", pin = TRUE)
    Condition
      Error in `tip_table()`:
      ! `pin` moved: `tip_table()` no longer takes it.
      i Give it to the `tip_spec()` of `rows`: `rows = tip_spec("...", pin = ...)`.

---

    Code
      tip_table(dt(), cells = list(wins = tip_spec("x", more = "y", more_on = "pin")),
      rows = tip_spec("x", pin = TRUE), id = "t")
    Condition
      Error in `tip_table()`:
      ! `cells[["wins"]]` sets `more` with `more_on = "pin"`, but this tip cannot pin.
      i Only the tips of `rows` can pin, with `tip_spec(pin = TRUE)`. Or set `more_on = "hover"`.

# the tip_table() errors / say what a column list and a template name must be

    Code
      tip_table(dt(), cells = list(wins = "a", wins = "b"))
    Condition
      Error in `tip_table()`:
      ! `cells` names a column more than once: "wins".
      i Give each column one entry. Put the words of both in one `tip_spec()`, or in one string.

---

    Code
      tip_table(dt(), rows = "row_tip")
    Condition
      Error in `tip_table()`:
      ! `rows` is "row_tip", the name of a template made with `tip_define()`; a table cannot use a name.
      i Give the template itself: a `tip_template()`, such as `rows = tip_template(tip_text("..."))`.

# the tip_table() errors / say what a style must be

    Code
      tip_table(dt(), rows = "x", style = "nope")
    Condition
      Error in `tip_table()`:
      ! `style` is "nope", which is not a preset.
      i The presets are "dense", "editorial", "lively", "modern", "sober", "none". Did you mean "dense" or "modern" or "sober" or "none"?

---

    Code
      tip_spec("x", style = "color: red")
    Condition
      Error in `tip_spec()`:
      ! `style` is not inline CSS: "color: red".
      i It takes nothing (`NULL`), a preset name from `tip_presets()`, a `tip_style()`, or one part of a style such as a `tip_theme()`.
      i Use `tip_theme()` for colours, the corner and the font, and `tip_style(class = )` for your own CSS.

