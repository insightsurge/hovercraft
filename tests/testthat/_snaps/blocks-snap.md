# the block errors / say what a bad argument was

    Code
      tip_title(1)
    Condition
      Error in `tip_title()`:
      ! `text` is not right: Must be of type 'string', not 'double'
      i Give one non-empty string.

---

    Code
      tip_fields(A = 1)
    Condition
      Error in `tip_fields()`:
      ! `A` is not right: Must be of type 'string', not 'double'
      i Give one non-empty string.

# the block errors / say where a bad placeholder is

    Code
      tip_text("{{ x | nope }}")
    Condition
      Error in `tip_text()`:
      ! The text of `tip_text()` has a placeholder the engine cannot use.
      x {{ x | nope }}: no filter "nope"; the filters are number, percent
      i A placeholder is `{{ field }}` or `{{ field | number:2 }}`, with one filter at most.

---

    Code
      tip_fields(Records = "{{ records")
    Condition
      Error in `tip_fields()`:
      ! The value of "Records" in `tip_fields()` has a placeholder the engine cannot use.
      x "{{" at character 1 never closes; add "}}", or write the words without it
      i A placeholder is `{{ field }}` or `{{ field | number:2 }}`, with one filter at most.

# the block errors / say Copy comes later

    Code
      tip_code("sql", copy = TRUE)
    Condition
      Error in `tip_code()`:
      ! `copy` is `TRUE`, but a Copy button lives in a tip that stays open.
      i That comes in a later version; leave `copy` out for now.

# the block errors / warn about a placeholder in a label

    Code
      tip_fields(`{{ a }}` = "b")
    Condition
      Warning in `tip_fields()`:
      The label "{{ a }}" in `tip_fields()` holds a placeholder mark.
      i Labels are plain text; only the values are filled in.
    Output
      <tip block: fields>

