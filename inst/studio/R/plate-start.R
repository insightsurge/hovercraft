# Plate 1, Start: what hovercraft is, and the look of every tip on the page (preset and motion), light or dark.

start_motions <- c("a fade" = "fade", "a slide" = "slide", "no motion" = "none")

# The page's look as code: the one line that goes once in the page's UI.
start_look_code <- function(preset, motion) {
  preset <- choice(preset, hovercraft::tip_presets()$name, "sober")
  motion <- choice(motion, start_motions, "fade")
  paste0(
    "# Once in the page's UI: the look of every tip on it\n",
    code_call("tip_dependency", style = code_call("tip_style", code_str(preset), motion = code_str(motion)))
  )
}

# The figure as code: three figures in a sentence, each with an underlined tip, and one word per look.
start_figure_code <- function() {
  word <- function(text, tip, id) {
    code_call(
      "tip_on",
      code_call("htmltools::tags$span", code_str(text), tabindex = code_str("0")),
      code_str(tip),
      marker = code_str("underline"),
      id = code_str(id),
      events = code_str("hint")
    )
  }
  sentence <- code_call(
    "htmltools::p",
    code_str("Week 46: the North region logged"),
    word("4,812 visits", "Visits that ended in a booking or a reply, Monday to Sunday.", "visits"),
    code_str("and stood"),
    word("6% above target", "Target: **4,540** visits a week, from the Q4 plan.", "target"),
    code_str("while the"),
    word("median wait", "Time from a ticket opening to its first reply, in business hours.", "wait_term"),
    code_str("rose to 4 h 12 min."),
    class = code_str("specimen-text")
  )
  looks <- paste0(
    "# One word in each look: style = on a tag overrides the page's look\n",
    "looks <- tip_presets()\n",
    "htmltools::p(\"The five looks, each on its own word.\", class = \"strip-title\")\n",
    "htmltools::tags$ul(\n",
    "  class = \"strip\",\n",
    "  lapply(seq_len(nrow(looks)), function(i) {\n",
    "    htmltools::tags$li(tip_on(\n",
    "      htmltools::tags$span(looks$name[[i]], tabindex = \"0\"),\n",
    "      paste0(\"**\", looks$label[[i]], \".** \", looks$about[[i]]),\n",
    "      style = looks$name[[i]],\n",
    "      id = paste0(\"look_\", looks$name[[i]]),\n",
    "      events = \"hint\"\n",
    "    ))\n",
    "  })\n",
    ")"
  )
  paste(c("# A tip on a figure in running text", sentence, "", looks), collapse = "\n")
}

start_steps <- function() {
  dark <- function(input) identical(input$page_mode, "dark")
  list(
    step(
      "Rest the pointer on *4,812 visits*: the hint opens after a short wait and closes when you leave.",
      "it flickers on the underline or stays open.",
      tip_target("visits"),
      on_hint("visits")
    ),
    step(
      "Tab to *median wait*: the hint opens on focus; Escape closes it and focus stays put.",
      "Escape moves focus or needs two presses.",
      tip_target("wait_term"),
      on_hint("wait_term", state = "hidden", reason = "escape")
    ),
    step(
      paste(
        "Choose *editorial*, then rest on a figure: every tip on the page takes the serif look, except the five",
        "words that set their own."
      ),
      "a tip keeps the old look.",
      "#start_preset"
    ),
    step(
      "Switch the page to dark, then rest on a figure: page and tips turn together.",
      "a tip stays light on a dark page.",
      "#start .theme-set",
      on_hint(c("visits", "target", "wait_term"), when = dark)
    )
  )
}

plate_start_ui <- function(look) {
  plate(
    "start",
    1,
    "Start",
    lede = NULL,
    caption = paste(
      "Three underlined figures in a sentence and the five looks. The choices above change every tip on the page at",
      "once."
    ),
    steps = start_steps(),
    code = code_text(paste0(start_look_code(look$preset, look$motion), "\n\n", start_figure_code())),
    log_empty = "Nothing yet. Rest on an underlined figure.",
    head = htmltools::tagList(
      htmltools::h1(
        class = "hero",
        hovercraft::tip_on(
          htmltools::span("hovercraft", tabindex = "0"),
          "An R package. It puts a tip on any HTML tag in a **Shiny** app or a **bslib** page.",
          id = "hero"
        )
      ),
      htmltools::p(class = "hero-sub", "studio: every tip it draws, on one page"),
      htmltools::tags$header(
        class = "plate-head",
        htmltools::p(class = "plate-no", `aria-hidden` = "true", "1"),
        htmltools::p(class = "plate-kicker", "Plate 1"),
        htmltools::h2(id = "start-h", "Start")
      ),
      htmltools::p(
        class = "lede",
        "Rest the pointer on a word, or reach it with Tab. Each plate below is one feature: the figure is live, the",
        "margin holds its R code, what to test, and the inputs your Shiny server would get."
      )
    ),
    controls = htmltools::tagList(
      "Set every tip on this page in",
      choice_set("start_preset", "Preset", stats::setNames(nm = hovercraft::tip_presets()$name), look$preset),
      "entering with",
      choice_set("start_motion", "Motion", start_motions, look$motion),
      "on a",
      theme_set("theme_start", "Page colours", show_legend = FALSE),
      "page."
    )
  )
}

# The engine's names for a page's preset and motion, read from its page contract (a file every install has), so the
# studio never writes an attribute or a variable name itself.
start_look_names <- function() {
  contract <- studio_contract()
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

plate_start_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "start", start_steps())
  look <- shiny::reactive(start_look_code(input$start_preset, input$start_motion))
  plate_code(
    input,
    output,
    session,
    "start",
    ui_code = shiny::reactive(paste0(look(), "\n\n", start_figure_code())),
    figure_code = shiny::reactive(start_figure_code())
  )
  names <- start_look_names()
  # The page's look is set once, when the page loads; a new choice is written onto the page by studio.js the way the
  # look's own operation writes it, and a pinned tip is pinned again to take it.
  shiny::observeEvent(
    list(input$start_preset, input$start_motion),
    {
      session$sendCustomMessage(
        "studio-look",
        list(
          attribute = names$attribute,
          preset = choice(input$start_preset, hovercraft::tip_presets()$name, "sober"),
          variable = names$variable,
          motion = choice(input$start_motion, start_motions, "fade")
        )
      )
    },
    ignoreInit = TRUE
  )
  ids <- c("visits", "target", "wait_term", paste0("look_", hovercraft::tip_presets()$name))
  log_inputs(input, session, "start", paste0(ids, "_hint"))
  ticks
}
