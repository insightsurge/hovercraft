# the marker errors / say what a marker takes

    Code
      tip_marker("undrline")
    Condition
      Error in `tip_marker()`:
      ! `type` is not a kind of marker: "undrline".
      i Give one of "none", "underline", "icon", "corner".
      Did you mean "underline"?

---

    Code
      tip_marker()
    Condition
      Error in `tip_marker()`:
      ! `type` is missing.
      i Give one of "none", "underline", "icon", and "corner".

---

    Code
      tip_as_marker(list("underline"))
    Condition
      Error:
      ! `marker` is not a marker: a list.
      i It takes "none", "underline", "icon" or "corner", a `tip_marker()`, or a tag to use as the icon.

---

    Code
      tip_on(htmltools::tags$span("Word"), "A tip", marker = "under")
    Condition
      Error in `tip_on()`:
      ! `marker` is not a kind of marker: "under".
      i Give one of "none", "underline", "icon", "corner".
      Did you mean "underline" or "corner"?

# the marker errors / say a line is only for an underline, and which lines there are

    Code
      tip_marker("corner", line = "wavy")
    Condition
      Error in `tip_marker()`:
      ! `line` is only for `type = "underline"`, not `type = "corner"`.
      i Use `tip_marker("underline", line = ...)` to choose the line.

---

    Code
      tip_marker("underline", line = "wavey")
    Condition
      Error in `tip_marker()`:
      ! `line` is not a line: "wavey".
      i Give one of "dotted", "dashed", "wavy", or `NULL`.
      Did you mean "wavy"?

# the marker errors / say an icon is a tag, and where to get one

    Code
      tip_marker("icon", icon = "info")
    Condition
      Error in `tip_marker()`:
      ! `icon` is a string, not a tag.
      i Give an icon as a tag, such as `bsicons::bs_icon("info-circle")` or `fontawesome::fa("circle-info")`.
      i A name such as "info" is not looked up: hovercraft does not pick an icon library for you.

---

    Code
      tip_marker("underline", icon = htmltools::tags$b("?"))
    Condition
      Error in `tip_marker()`:
      ! `icon` is only for `type = "icon"`, not `type = "underline"`.
      i Use `tip_marker("icon", icon = ...)` to give an icon.

# the marker errors / say a tag that holds nothing has nowhere for a marker

    Code
      tip_on(htmltools::tags$input(), "A tip", marker = "icon")
    Condition
      Error in `tip_on()`:
      ! A `<input>` tag holds nothing, so a marker has nowhere to go.
      i Put the tip and the marker on the tag around it, such as its label.

# the marker errors / say a block with no words of its own has nothing to underline

    Code
      tip_on(htmltools::tags$div(htmltools::tags$p("Only a block"), tabindex = "0"),
      "A tip", marker = "underline")
    Condition
      Error in `tip_on()`:
      ! A `<div>` tag holds no words of its own to underline: what is inside it is other tags.
      i Put the marker on the tag that holds the words, such as its label, or use "corner" or "icon".

# the marker errors / say ... must be empty

    Code
      tip_marker("underline", lines = "wavy")
    Condition
      Error in `tip_marker()`:
      ! `...` must be empty.
      x Not an argument: "lines".
      i The arguments are `type`, `line`, and `icon`.

