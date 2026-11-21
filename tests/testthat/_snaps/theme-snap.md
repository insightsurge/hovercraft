# the theme errors and warnings / say a role is misspelt, and what the roles are

    Code
      tip_theme(light = list(bgg = "red"))
    Condition
      Error in `tip_theme()`:
      ! `light` has "bgg", which is not a role.
      i The roles are "bg", "ink", "line", "code_bg", "radius", "shadow", "font". Did you mean "bg" or "code_bg"?

# the theme errors and warnings / say what a role takes

    Code
      tip_theme(light = list(bg = "notacolour"))
    Condition
      Error in `tip_theme()`:
      ! The bg role in `light` is not valid: "notacolour".
      i It takes a colour name, hex, `rgb()` or `rgba()` text, three numbers from 0 to 255, or a CSS variable.

---

    Code
      tip_theme(dark = list(radius = "1;}"))
    Condition
      Error in `tip_theme()`:
      ! The radius role in `dark` is not valid: "1;}".
      i It takes a number of pixels, a length such as "0.5rem", or a CSS variable.

---

    Code
      tip_theme(dark = list(font = "a; b"))
    Condition
      Error in `tip_theme()`:
      ! The font role in `dark` is not valid: "a; b".
      i It takes CSS `font-family` text (letters, digits, spaces and , . ' " ( ) # % _ + * - only), or a CSS variable.

# the theme errors and warnings / say a mode is not one

    Code
      tip_theme(mode = "x")
    Condition
      Error in `tip_theme()`:
      ! `mode` is "x".
      i It is "auto", "light" or "dark".

# the theme errors and warnings / say a mix of roles and CSS variables is not a theme

    Code
      tip_as_theme(list(bg = "red", `--a` = "1"))
    Condition
      Error:
      ! `theme` mixes roles and CSS variables: "bg" and "--a".
      i Use roles (such as "bg") or CSS variables (names that start with "--"), not both.

---

    Code
      tip_as_theme(3)
    Condition
      Error:
      ! `theme` is not a theme.
      i Give `tip_theme()`, a `bslib::bs_theme()`, or a named list of roles or CSS variables.

# the theme errors and warnings / warn about low contrast with the pair and the ratio

    Code
      invisible(tip_theme(light = list(bg = "#ffffff", ink = "#dddddd")))
    Condition
      Warning in `tip_theme()`:
      In light and dark mode the tip's ink `#dddddd` on its bg `#ffffff` has contrast 1.36:1.
      i Text needs 4.5:1. The colours are kept; change one of them.

---

    Code
      invisible(tip_theme(light = list(bg = "#1b3a6b", ink = "#ffffff", code_bg = "#7a9cc6")))
    Condition
      Warning in `tip_theme()`:
      In light and dark mode the tip's ink `#ffffff` on its code_bg `#7a9cc6` has contrast 2.84:1.
      i Text needs 4.5:1. The colours are kept; change one of them.

# the theme errors and warnings / print a theme

    Code
      tip_theme(light = list(bg = "navy", radius = 8), dark = list(bg = "#9ec5fe"))
    Output
      tip_theme(light = list(bg = "#000080", radius = "8px"), dark = list(bg = "#9ec5fe"))

---

    Code
      tip_theme()
    Output
      tip_theme()

