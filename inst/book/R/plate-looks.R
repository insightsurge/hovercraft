# Section 4.1, Presets and the page's look: the five presets, the look of every tip on the page (preset and motion),
# and a look for one tag.

looks_motions <- c("a fade" = "fade", "a slide" = "slide", "no motion" = "none")

# The page's look as code: the one line that goes once in the page's UI.
looks_page_code <- function(preset, motion) {
  preset <- choice(preset, hovercraft::tip_presets()$name, "sober")
  motion <- choice(motion, looks_motions, "fade")
  paste0(
    "# Once in the page's UI: the look of every tip on it\n",
    code_call("use_hovercraft", style = code_call("tip_style", code_str(preset), motion = code_str(motion)))
  )
}

# The figure as code: one word in each preset. style = on a tag beats the page's look.
looks_figure_code <- function() {
  strip <- code_strip(
    "The five looks, each on its own word.",
    "seq_len(nrow(looks))",
    "i",
    code_call(
      "tip_on",
      code_word(I("looks$name[[i]]")),
      "paste0(\"**\", looks$label[[i]], \".** \", looks$about[[i]])",
      style = "looks$name[[i]]",
      id = "paste0(\"look_\", looks$name[[i]])",
      events = code_str("hint")
    )
  )
  page <- code_sentence(
    "And this",
    code_call(
      "tip_on",
      code_word("page word"),
      code_str("No **style** here: this tip takes the page's look."),
      marker = code_str("underline"),
      id = code_str("look_page"),
      events = code_str("hint")
    ),
    "takes the page's look."
  )
  paste(
    c(
      "# One word in each look: style = on a tag beats the page's look",
      "looks <- tip_presets()",
      strip,
      "# A word with no look of its own takes the page's",
      page
    ),
    collapse = "\n"
  )
}

looks_covers <- c("tip_presets", "use_hovercraft(style)", "tip_style(preset)", "tip_style(motion)", "tip_on(style)")

looks_steps <- function() {
  dark <- function(input) identical(input$page_mode, "dark")
  list(
    step(
      "Rest on each of the five words: each tip has its own look, whatever the page's look is.",
      "a word's tip changes when the page's look changes.",
      "#looks .strip",
      on_hint("look_editorial")
    ),
    step(
      "Choose *editorial* for the page, then rest on *page word*: it takes the serif look; the five words keep theirs.",
      "a tip keeps the old look.",
      "#looks_preset",
      on_hint("look_page", when = chose("looks_preset", "editorial", "sober"))
    ),
    step(
      "Choose *a slide*, then rest on *page word*: the tip moves a few pixels as it fades in.",
      "no motion, or motion although the system asks for less.",
      "#looks_motion",
      on_hint("look_page", when = chose("looks_motion", "slide", "fade"))
    ),
    step(
      "Switch the page to dark, then rest on *page word*: page and tips turn together.",
      "a tip stays light on a dark page.",
      "#looks .theme-set",
      on_hint("look_page", when = dark)
    )
  )
}

plate_looks_ui <- function(query) {
  plate(
    "looks",
    lede = paste(
      "Five presets come with the package. The page takes one look for every tip, once, in its UI; a tag can take",
      "its own with style =."
    ),
    caption = "The five looks, each on its own word, and a word that takes the page's look.",
    steps = looks_steps(),
    code = code_text(paste0(looks_page_code(query$preset, query$motion), "\n\n", looks_figure_code())),
    log_empty = "Nothing yet. Rest on a look.",
    controls = htmltools::tagList(
      "Set every tip on this page in",
      choice_set("looks_preset", "Preset", stats::setNames(nm = hovercraft::tip_presets()$name), query$preset),
      "entering with",
      choice_set("looks_motion", "Motion", looks_motions, query$motion),
      "on a",
      theme_set("theme_looks", "Page colours", show_legend = FALSE),
      "page."
    )
  )
}

# The engine's names for a page's preset and motion, read from its page contract (a file every install has), so the
# book never writes an attribute or a variable name itself.
looks_names <- function() {
  contract <- book_contract()
  list(
    attribute = contract$attributes$preset,
    variable = paste0(
      "--",
      sub("^data-", "", contract$attributes$tip),
      "-",
      contract$presets_fields$fields$motion$maps_to
    )
  )
}

plate_looks_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "looks", looks_steps())
  page <- shiny::reactive(looks_page_code(input$looks_preset, input$looks_motion))
  plate_code(
    input,
    output,
    session,
    "looks",
    ui_code = shiny::reactive(paste0(page(), "\n\n", looks_figure_code())),
    figure_code = shiny::reactive(looks_figure_code())
  )
  names <- looks_names()
  # The page's look is set once, when the page loads; a new choice is written onto the page by shared.js the way the
  # look's own operation writes it, and a pinned tip is pinned again to take it.
  shiny::observeEvent(
    list(input$looks_preset, input$looks_motion),
    {
      session$sendCustomMessage(
        "book-look",
        list(
          attribute = names$attribute,
          preset = choice(input$looks_preset, hovercraft::tip_presets()$name, "sober"),
          variable = names$variable,
          motion = choice(input$looks_motion, looks_motions, "fade")
        )
      )
    },
    ignoreInit = TRUE
  )
  log_inputs(input, session, "looks", paste0(c("look_page", paste0("look_", hovercraft::tip_presets()$name)), "_hint"))
  ticks
}
