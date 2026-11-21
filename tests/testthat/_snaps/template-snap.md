# the template errors / say which block is wrong

    Code
      tip_template()
    Condition
      Error in `tip_template()`:
      ! A template needs at least one block.

---

    Code
      tip_template(tip_title("a"), "text")
    Condition
      Error in `tip_template()`:
      ! Block 2 is not a block.
      i Make one with `tip_title()`, `tip_text()`, `tip_badge()`, `tip_fields()` or `tip_code()`.

---

    Code
      tip_template(list(type = "blink"))
    Condition
      Error in `tip_template()`:
      ! Block 1 has the type "blink", which is not a block. The types are "title", "text", "badge", "fields", and "code".

---

    Code
      tip_template(list(type = "rows"))
    Condition
      Error in `tip_template()`:
      ! Block 1 is a "rows" block, which comes with a later version.

---

    Code
      tip_template(list(type = "text", text = "x", colour = "red"))
    Condition
      Error in `tip_template()`:
      ! Block 1 has fields a "text" block does not take: "colour". It takes "text".

# the template errors / say what a template name may be

    Code
      tip_define(`My Template` = tip_template(tip_text("a")))
    Condition
      Error in `tip_define()`:
      ! The template name "My Template" is not allowed.
      i Use lower-case letters, digits and `_`, starting with a letter (not an R keyword).

# the template errors / print a template

    Code
      tip_template(tip_title("a"), tip_text("b"))
    Output
      <tip template: title, text>

