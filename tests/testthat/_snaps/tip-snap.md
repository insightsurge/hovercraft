# the tip_on() errors / say a tag with no focus gives keyboard users no way to the tip

    Code
      invisible(tip_on(htmltools::tags$tr("x"), "Row"))
    Condition
      Warning in `tip_on()`:
      A `<tr>` tag cannot take keyboard focus, so a keyboard user will not reach its tip.
      i Add `tabindex = "0"` to the tag, or put the tip on a button or a link.
      i `tip_on()` never adds a tab stop itself.
      This warning is displayed once per session.

# the tip_on() errors / say a template name is already taken

    Code
      invisible(tip_define(demo = tip_template(tip_text("two"))))
    Condition
      Warning in `tip_define()`:
      The template "demo" was already defined with different blocks; this one replaces it.
      i Templates are shared by the whole page. Put a module's id in the names it defines, such as "<id>_demo".

# the tip_on() errors / say what a bad argument was

    Code
      tip_on(button(), 1)
    Condition
      Error in `tip_on()`:
      ! `text` is not right: Must be of type 'string' (or 'NULL'), not 'double'
      i Give one string, or `NULL`.

---

    Code
      tip_define(tip_template(tip_text("a")))
    Condition
      Error in `tip_define()`:
      ! `...` is not right: Must have names
      i Give each template a name: `tip_define(name = tip_template(...))`.

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
      x Not an argument: "tmplate".
      i The arguments are `tag`, `text`, `template`, `record`, `style`, `placement`, `marker`, `id`, `events`, and `when_disabled`.

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
      i Did you mean "demo" or "diff"?
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

# the tip_on() errors / say what a placement and an id must be

    Code
      tip_on(button(), "x", placement = "above")
    Condition
      Error in `tip_on()`:
      ! `placement` is not right: Must be element of set {'auto','top','bottom','left','right'}, but is 'above'
      i Give one of "auto", "top", "bottom", "left", "right", or `NULL`.

---

    Code
      tip_on(button(), "x", id = "two words")
    Condition
      Error in `tip_on()`:
      ! `id` is not right: Must be one non-empty string without whitespace.
      i Give one non-empty string with no spaces, such as `ns("scores")`, or `NULL`.

