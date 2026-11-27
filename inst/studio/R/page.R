# The studio's page and server: the index, the compact bar for phones, and the nine plates in order.

# The page's starting settings, from its address (tip_studio() puts them there): only values the studio offers.
studio_query <- function(request) {
  query <- shiny::parseQueryString(request$QUERY_STRING %||% "")
  list(
    plate = choice(query$plate, studio_plates$id, ""),
    guide = identical(query$guide, "1"),
    preset = choice(query$preset, hovercraft::tip_presets()$name, "sober"),
    motion = choice(query$motion, start_motions, "fade")
  )
}

# The look of the studio's own page: the specimen sheet's paper, ink and Prussian blue, on Bootstrap 5.
studio_theme <- function() {
  bslib::bs_theme(version = 5, bg = "#f4f6f7", fg = "#1b1f24", primary = "#1a4b77")
}

# The list of plates, as the index and the phone's plate sheet show it.
studio_toc <- function(id = NULL) {
  htmltools::div(
    class = "toc",
    id = id,
    htmltools::div(class = "rail", `aria-hidden` = "true", htmltools::tags$i()),
    htmltools::tags$ol(
      role = "list",
      lapply(seq_len(nrow(studio_plates)), function(i) {
        htmltools::tags$li(htmltools::tags$a(
          href = paste0("#", studio_plates$id[[i]]),
          `data-plate` = studio_plates$id[[i]],
          htmltools::span(class = "n", i),
          htmltools::span(class = "t", studio_plates$title[[i]]),
          htmltools::span(class = "g-mark g-ui")
        ))
      })
    )
  )
}

guide_switch <- function(kbd = TRUE) {
  htmltools::tags$button(
    type = "button",
    class = "g-switch js-guide",
    `aria-pressed` = "false",
    title = "Guide me (G)",
    htmltools::span(class = "box", `aria-hidden` = "true"),
    if (kbd) htmltools::tagList("Guide me ", htmltools::tags$kbd("G")) else "Guide"
  )
}

# What the page's script needs to start: each plate's step targets, and the settings from the address.
studio_boot <- function(query) {
  steps <- list(
    start = start_steps(),
    basics = basics_steps(),
    templates = templates_steps(),
    more = more_steps(),
    pins = pins_steps(),
    disabled = disabled_steps(),
    output = output_steps(),
    tables = tables_steps(),
    server = server_steps()
  )
  contract <- studio_contract()
  boot <- list(
    names = list(
      global = contract$global,
      css = contract$css,
      id = contract$attributes$id,
      events = lapply(contract$events, function(e) e$name)
    ),
    targets = lapply(steps, function(s) lapply(s, function(x) x$target)),
    plate = query$plate,
    guide = query$guide
  )
  htmltools::tags$script(
    type = "application/json",
    id = "studio-boot",
    htmltools::HTML(jsonlite::toJSON(boot, auto_unbox = TRUE))
  )
}

studio_page <- function(query) {
  bslib::page(
    title = "hovercraft studio",
    theme = studio_theme(),
    lang = "en",
    htmltools::tags$head(
      htmltools::tags$link(rel = "stylesheet", href = "shared.css"),
      htmltools::tags$link(rel = "stylesheet", href = "studio.css"),
      htmltools::tags$script(src = "shared.js"),
      htmltools::tags$script(src = "studio.js")
    ),
    hovercraft::use_hovercraft(style = hovercraft::tip_style(query$preset, motion = query$motion)),
    studio_boot(query),
    htmltools::a(class = "vh skip", href = "#start", "Skip to the first plate"),
    htmltools::tags$header(
      class = "bar",
      id = "bar",
      htmltools::a(class = "wordmark", href = "#start", htmltools::tags$b("hovercraft"), htmltools::span("studio")),
      htmltools::tags$button(
        type = "button",
        class = "where",
        id = "where",
        `aria-expanded` = "false",
        `aria-controls` = "toc-sheet",
        htmltools::span(class = "n", id = "where-n", "1"),
        htmltools::span(id = "where-t", "Start")
      ),
      guide_switch(kbd = FALSE),
      htmltools::tags$i(class = "prog")
    ),
    htmltools::tags$nav(
      class = "toc-sheet",
      id = "toc-sheet",
      `aria-label` = "Plates, as a list to open",
      studio_toc(),
      theme_set("theme_sheet")
    ),
    htmltools::tags$button(
      type = "button",
      class = "pull",
      id = "pull",
      `aria-expanded` = "false",
      htmltools::span(class = "word", "Margin"),
      htmltools::span(class = "cnt", id = "pull-cnt", hidden = NA, "0")
    ),
    htmltools::div(
      class = "cuebar",
      id = "cuebar",
      role = "group",
      `aria-label` = "Guide: the current step",
      htmltools::tags$button(
        type = "button",
        class = "g-tick",
        `data-g` = "tick",
        `aria-pressed` = "false",
        `aria-label` = "Current step done"
      ),
      htmltools::tags$button(
        type = "button",
        class = "cue-text",
        `data-g` = "open",
        title = "Open the margin",
        htmltools::span(class = "w"),
        htmltools::span(class = "x")
      ),
      htmltools::tags$button(type = "button", class = "g-btn", `data-g` = "back", `aria-label` = "Back a step", "Back"),
      htmltools::tags$button(type = "button", class = "g-btn", `data-g` = "next", `aria-label` = "Next step", "Next")
    ),
    htmltools::tags$svg(
      class = "g-leader",
      id = "g-leader",
      `aria-hidden` = "true",
      hidden = NA,
      htmltools::tags$polyline(points = ""),
      htmltools::tags$circle(r = "3.5", cx = "-9", cy = "-9")
    ),
    htmltools::div(
      class = "book",
      htmltools::tags$nav(
        class = "index",
        `aria-label` = "Plates",
        htmltools::a(class = "wordmark", href = "#start", htmltools::tags$b("hovercraft"), htmltools::span("studio")),
        htmltools::div(
          class = "g-head",
          guide_switch(),
          htmltools::tags$button(type = "button", class = "g-resume g-ui", id = "g-resume")
        ),
        studio_toc("toc"),
        htmltools::div(class = "index-foot", theme_set("theme_rail"))
      ),
      htmltools::tags$main(
        id = "main",
        plate_start_ui(query),
        plate_basics_ui(),
        plate_templates_ui(),
        plate_more_ui(),
        plate_pins_ui(),
        plate_disabled_ui(),
        plate_output_ui(),
        plate_tables_ui(),
        plate_server_ui(),
        htmltools::p(
          class = "colophon",
          "Set in Charter and Avenir Next where your system has them, with a monospace for code. Tips drawn by the",
          "tip engine that ships with hovercraft. Figures are made-up data."
        )
      )
    )
  )
}

studio_server <- function(input, output, session) {
  plate_start_server(input, output, session)
  plate_basics_server(input, output, session)
  plate_templates_server(input, output, session)
  plate_more_server(input, output, session)
  plate_pins_server(input, output, session)
  plate_disabled_server(input, output, session)
  plate_output_server(input, output, session)
  plate_tables_server(input, output, session)
  plate_server_server(input, output, session)
}
