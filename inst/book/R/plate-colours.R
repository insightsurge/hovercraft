# Section "colours": a tip's fill, words, border, corner and font, for light and for dark (`tip_theme(light =, dark =,
# mode =)`, given as `tip_style(theme =)`), or the colours of a bslib theme.

colours_text <- paste(
  "Median time from a ticket opening to its first reply.",
  "The **business-hours** clock stops at night; `status` says which."
)

# Each palette has its roles for light and for dark. "Washed out" is low contrast on purpose, so the package warns.
colours_roles <- list(
  harbour = list(
    light = list(bg = "#1b3a6b", text = "#ffffff", border = "#0f2547", code_bg = "#12294d", radius = 8),
    dark = list(bg = "#9ec5fe", text = "#0b1f3a", border = "#6fa3ec", code_bg = "#7fb0f5")
  ),
  linen = list(
    light = list(
      bg = "#fdf6e3",
      text = "#3b3224",
      border = "#d9c9a3",
      code_bg = "#f1e6c8",
      radius = 12,
      font = "Georgia, serif"
    ),
    dark = list(bg = "#2b2418", text = "#f1e6cc", border = "#5a4c33", code_bg = "#1f1a11")
  ),
  washed = list(
    light = list(bg = "#ffffff", text = "#c9c9c9"),
    dark = list(bg = "#2a2a2a", text = "#4a4a4a")
  )
)

# The palettes on offer. bslib is always there: the book needs it to run.
colours_palettes <- c(
  "harbour blue" = "harbour",
  "warm linen" = "linen",
  "washed out" = "washed",
  "a bslib theme" = "bslib"
)

colours_modes <- c("follows the page" = "auto", "always light" = "light", "always dark" = "dark")

colours_code <- function(settings) {
  palette <- choice(settings$palette, colours_palettes, "harbour")
  mode <- choice(settings$mode, colours_modes, "auto")
  tag <- function(style) {
    code_call(
      "tip_on",
      code_word("Median wait", class = "target"),
      code_str(colours_text),
      style = style,
      id = code_str("colours_wait"),
      events = code_str("hint")
    )
  }
  if (palette == "bslib") {
    bs <- code_call(
      "bslib::bs_theme",
      bg = code_str("#fff8ef"),
      fg = code_str("#2b1d0e"),
      primary = code_str("#b4531a")
    )
    return(
      paste(
        c("# A page that has a bslib theme needs none of this; a theme is also accepted as it is", tag(bs)),
        collapse = "\n"
      )
    )
  }
  roles <- colours_roles[[palette]]
  theme <- code_call(
    "tip_theme",
    light = code_list(roles$light),
    dark = code_list(roles$dark),
    mode = if (mode != "auto") code_str(mode)
  )
  paste(
    c(
      "# The colours, once for light and once for dark; a role left out of both keeps the page's",
      paste0("theme <- ", theme),
      "",
      tag(code_call("tip_style", theme = "theme"))
    ),
    collapse = "\n"
  )
}

colours_steps <- function() {
  list(
    step(
      "Rest on *Median wait*: the tip takes the palette's fill and words, and its corner.",
      "the tip keeps the page's colours.",
      tip_target("colours_wait"),
      on_hint("colours_wait")
    ),
    step(
      paste(
        "Switch the page to dark, with the mode that *follows the page*, then rest on the tag: the tip takes the",
        "dark colours."
      ),
      "the tip stays in its light colours, or the words fade into the fill.",
      "#colours .theme-set",
      on_hint(
        "colours_wait",
        when = function(input) {
          identical(input$page_mode, "dark") && chose("colours_mode", "auto", "auto")(input)
        }
      )
    ),
    step(
      paste(
        "On a light page choose *always dark*, then rest on the tag: the tip is dark whatever the page says. (Not with",
        "the bslib theme, which follows the page.)"
      ),
      "the tip follows the page.",
      "#colours_mode",
      on_hint(
        "colours_wait",
        when = function(input) {
          identical(input$colours_mode, "dark") &&
            !identical(input$page_mode, "dark") &&
            !chose("colours_palette", "bslib", "harbour")(input)
        }
      )
    ),
    step(
      paste(
        "Choose *washed out*: the note under the figure shows the package's warning (class `hovercraft_contrast`)",
        "for a `text` under 4.5:1 on its `bg`, once for light and once for dark, and the colours are kept."
      ),
      "no warning, or the tip is drawn in other colours.",
      "#colours_notes"
    ),
    step(
      "Choose *a bslib theme*, then rest on the tag: the tip takes the colours of the theme given as `style =`.",
      "the tip keeps the page's colours.",
      tip_target("colours_wait"),
      on_hint("colours_wait", when = chose("colours_palette", "bslib", "harbour"))
    )
  )
}

plate_colours_ui <- function() {
  plate(
    "colours",
    lede = htmltools::tagList(
      htmltools::tags$code("tip_theme()"),
      "sets the colours, corner and font of a tip for light and for dark. The roles are",
      htmltools::tags$code("bg, text, border, code_bg, radius, shadow, font"),
      "and a role you leave out keeps the page's. One you set only for light holds in dark too. A page that has a",
      "bslib theme needs no theme at all. A list of CSS variables is for the whole page: give it to",
      htmltools::tags$code("use_hovercraft()"),
      "since on one tag it stops with an error."
    ),
    caption = paste(
      "One tag, drawn in each palette. Washed out is low contrast on purpose: the package measures the words on the",
      "fill and warns, and keeps the colours."
    ),
    steps = colours_steps(),
    code = code_text(colours_code(list())),
    log_empty = "Nothing yet. Rest on the tag.",
    controls = htmltools::tagList(
      "Dress the tip in",
      choice_set("colours_palette", "Palette", colours_palettes),
      "and let it use the mode that",
      choice_set("colours_mode", "Mode", colours_modes),
      "on a",
      theme_set("theme_colours", "Page colours", show_legend = FALSE),
      "page."
    )
  )
}

plate_colours_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "colours", colours_steps())
  code <- shiny::reactive(colours_code(list(palette = input$colours_palette, mode = input$colours_mode)))
  plate_code(input, output, session, "colours", code)
  log_inputs(input, session, "colours", "colours_wait_hint")
  ticks
}
