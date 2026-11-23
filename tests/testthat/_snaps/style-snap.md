# the style errors / say style is not CSS, and what it takes

    Code
      tip_as_style("color: red")
    Condition
      Error:
      ! `style` is not inline CSS: "color: red".
      i It takes nothing (`NULL`), a preset name from `tip_presets()`, a `tip_style()`, or one part of a style such as a `tip_theme()`.
      i Use `tip_theme()` for colours, the corner and the font, and `tip_style(class = )` for your own CSS.

---

    Code
      tip_style("color: red; font-size: 12px")
    Condition
      Error in `tip_style()`:
      ! `preset` is not inline CSS: "color: red; font-size: 12px".
      i It takes nothing (`NULL`), a preset name from `tip_presets()`, a `tip_style()`, or one part of a style such as a `tip_theme()`.
      i Use `tip_theme()` for colours, the corner and the font, and `tip_style(class = )` for your own CSS.

---

    Code
      tip_on(htmltools::tags$button("Run"), "t", style = "color:red")
    Condition
      Error in `tip_on()`:
      ! `style` is not inline CSS: "color:red".
      i It takes nothing (`NULL`), a preset name from `tip_presets()`, a `tip_style()`, or one part of a style such as a `tip_theme()`.
      i Use `tip_theme()` for colours, the corner and the font, and `tip_style(class = )` for your own CSS.

# the style errors / say a list is not a style, and where the roles go

    Code
      tip_as_style(list(bg = "red"))
    Condition
      Error:
      ! `style` is not a style: a list.
      i A list is not taken here: give `tip_theme()` the roles, or `tip_style(theme = list(...))`.

---

    Code
      tip_as_style(3)
    Condition
      Error:
      ! `style` is not a style: a number.
      i It takes `NULL`, a preset name, a `tip_style()`, or one part of a style such as a `tip_theme()`.

# the style errors / say a name is not a preset, and which are

    Code
      tip_style("sobre")
    Condition
      Error in `tip_style()`:
      ! `preset` is "sobre", which is not a preset.
      i The presets are "dense", "editorial", "lively", "modern", "sober", "none". Did you mean "sober"?

# the style errors / say what a class name is, and which start is reserved

    Code
      tip_style(class = c("fine", "1a", "b.c"))
    Condition
      Error in `tip_style()`:
      ! `class` has "1a" and "b.c", which are not a class name.
      i A class name has letters, digits, "-" and "_", and does not start with a digit.

---

    Code
      tip_style(class = "hc-tip-wide")
    Condition
      Error in `tip_style()`:
      ! `class` has "hc-tip-wide", which starts with "hc-tip".
      i The tip's own classes use that start; choose another name.

# the style errors / say a size is outside the engine's range, and which range

    Code
      tip_style(gap = 30)
    Condition
      Error in `tip_style()`:
      ! `gap` is 30, which is not a whole number from 0 to 24.
      i Px between item and tip; with an arrow, five px more for its point.

---

    Code
      tip_style(max_width = 100)
    Condition
      Error in `tip_style()`:
      ! `max_width` is 100, which is not a whole number from 160 to 640.
      i Maximum width of a hint in px; still capped at the window less sixteen px.

---

    Code
      tip_style(pin_width = 300.5)
    Condition
      Error in `tip_style()`:
      ! `pin_width` is 300.5, which is not a whole number from 240 to 720.
      i Maximum width of a pinned tip in px (and the fixed width of a tip with a host slot or row group); capped at the window less sixteen px.

---

    Code
      tip_style(gap = "wide")
    Condition
      Error in `tip_style()`:
      ! `gap` is not right: Must be of type 'number', not 'character'
      i Give one whole number from 0 to 24 (px), or `NULL`.

# the style errors / say a motion is not one of the choices, and the close one

    Code
      tip_style(motion = "slid")
    Condition
      Error in `tip_style()`:
      ! `motion` is "slid", which is not a motion.
      i The choices are "fade", "slide", "none". Did you mean "slide"?

---

    Code
      tip_style(motion = "bounce")
    Condition
      Error in `tip_style()`:
      ! `motion` is "bounce", which is not a motion.
      i The choices are "fade", "slide", "none".

# the style errors / say ... must be empty

    Code
      tip_style("sober", themee = 1)
    Condition
      Error in `tip_style()`:
      ! `...` must be empty.
      x Not an argument: "themee".
      i The arguments are `preset`, `theme`, `class`, `motion`, `gap`, `max_width`, and `pin_width`.

---

    Code
      tip_style(delay = 300)
    Condition
      Error in `tip_style()`:
      ! `...` must be empty.
      x Not an argument: "delay".
      i The arguments are `preset`, `theme`, `class`, `motion`, `gap`, `max_width`, and `pin_width`.

# the style errors / print a style

    Code
      tip_style("dense", theme = tip_theme(light = list(radius = 6)), class = "wide")
    Output
      tip_style("dense", theme = tip_theme(light = list(radius = "6px")), class = "wide")

---

    Code
      tip_style()
    Output
      tip_style()

---

    Code
      tip_style("modern", motion = "slide", gap = 12, max_width = 280, pin_width = 500)
    Output
      tip_style("modern", motion = "slide", gap = 12, max_width = 280, pin_width = 500)

