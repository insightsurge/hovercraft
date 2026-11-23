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
      ! `id` is not right: Must be one non-empty string without whitespace.
      i Give one non-empty string with no spaces, such as `ns("scores")`, or `NULL`.

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

