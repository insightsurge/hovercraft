# the tip_on() errors / say a tag list is not a tag

    Code
      tip_on(htmltools::tagList(button(), button()), "x")
    Condition
      Error in `tip_on()`:
      ! `tag` is a list of tags; give one tag.
      i Call `tip_on()` on each tag.

---

    Code
      tip_on("x", "x")
    Condition
      Error in `tip_on()`:
      ! `tag` must be an HTML tag, not a string.

# the tip_on() errors / say there is nothing to show

    Code
      tip_on(button())
    Condition
      Error in `tip_on()`:
      ! Give `text` or `template`; there is nothing to show.

# the tip_on() errors / say a misspelt argument is not taken

    Code
      tip_on(button(), "x", tmplate = "a")
    Condition
      Error in `tip_on()`:
      ! `...` must be empty.
      x Problematic argument:
      * tmplate = "a"

# the tip_on() errors / say what a record lacks, and what it holds

    Code
      tip_on(button(), template = "diff", record = list(label = "x"))
    Condition
      Error in `tip_on()`:
      ! The template "diff" names fields the record does not hold.
      x Missing: "n" and "m".
      i The record holds "label".

---

    Code
      tip_on(button(), template = "diff")
    Condition
      Error in `tip_on()`:
      ! The template "diff" names fields the record does not hold.
      x Missing: "label", "n", and "m".
      i No record was given.

# the tip_on() errors / warn about a name not defined, and an engine name

    Code
      invisible(tip_on(button(), template = "dif", record = list(label = "x")))
    Condition
      Warning in `tip_on()`:
      No template "dif" has been defined with `tip_define()`.
      i Did you mean "diff"?
      i If it is defined later in the page, ignore this.
      This warning is displayed once per session.

---

    Code
      invisible(tip_on(button(), template = "withid", record = list(label = "x",
        tip_id = "u")))
    Condition
      Warning in `tip_on()`:
      The record has "tip_id", which the engine also fills in.
      i The record's own value is shown; rename the column if you meant the engine's.

---

    Code
      invisible(tip_on(button(), "x", record = list(a = 1)))
    Condition
      Warning in `tip_on()`:
      `record` is ignored without a `template`.
      i A plain text tip has no placeholders to fill in.

