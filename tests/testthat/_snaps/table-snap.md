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

