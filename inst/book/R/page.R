# The book's page and server: the index of parts and sections with its search box, the compact bar for phones, and
# the sections in order. Each section's functions are named after its id in `book_sections`.

# The page's starting settings, from its address (hovercraft_book() puts them there): only values the book offers.
book_query <- function(request) {
  query <- shiny::parseQueryString(request$QUERY_STRING %||% "")
  list(
    section = choice(query$section, book_sections$id, ""),
    checklist = identical(query$checklist, "1"),
    preset = choice(query$preset, hovercraft::tip_presets()$name, "sober"),
    motion = choice(query$motion, looks_motions, "fade")
  )
}

# A section's own function, such as `plate_pins_ui` or `pins_steps`, found by its id.
section_fn <- function(id, pattern) get(sprintf(pattern, id), mode = "function")

# What a section's search finds it by: its number, title and part, and the functions and arguments it shows.
section_search <- function(i) {
  at <- book_sections[i, ]
  covers <- get0(paste0(at$id, "_covers"), ifnotfound = character())
  part <- book_parts$title[book_parts$n == at$part]
  tolower(paste(c(at$n, at$title, part, gsub("[()]", " ", covers)), collapse = " "))
}

# The look of the book's own page: the specimen sheet's paper, ink and Prussian blue, on Bootstrap 5.
book_theme <- function() {
  bslib::bs_theme(version = 5, bg = "#f4f6f7", fg = "#1b1f24", primary = "#1a4b77")
}

# The index: the parts, each with its sections under it, as the index and the phone's sheet show it.
book_toc <- function(id = NULL) {
  htmltools::div(
    class = "toc",
    id = id,
    htmltools::tags$ol(
      role = "list",
      class = "parts",
      lapply(seq_len(nrow(book_parts)), function(p) {
        rows <- which(book_sections$part == book_parts$n[[p]])
        htmltools::tags$li(
          class = "part",
          htmltools::a(
            class = "part-t",
            href = paste0("#", book_sections$id[[rows[[1]]]]),
            htmltools::span(class = "pn", book_parts$n[[p]]),
            book_parts$title[[p]]
          ),
          htmltools::tags$ol(
            role = "list",
            lapply(rows, function(i) {
              htmltools::tags$li(htmltools::tags$a(
                href = paste0("#", book_sections$id[[i]]),
                `data-plate` = book_sections$id[[i]],
                `data-search` = section_search(i),
                htmltools::span(class = "n", book_sections$n[[i]]),
                htmltools::span(class = "t", book_sections$title[[i]]),
                htmltools::span(class = "g-mark g-ui")
              ))
            })
          )
        )
      })
    )
  )
}

# The search box over the index: it hides the sections whose title, part, functions and arguments don't hold every
# word typed. book.js does the filtering; with no script the box does nothing and the index stays whole.
toc_search <- function(id) {
  htmltools::div(
    class = "toc-search",
    role = "search",
    htmltools::tags$label(`for` = id, class = "vh", "Search the book"),
    htmltools::tags$input(
      type = "search",
      id = id,
      class = "js-toc-search",
      placeholder = "Search, e.g. pin",
      autocomplete = "off",
      spellcheck = "false"
    ),
    htmltools::p(class = "toc-none", role = "status", `aria-live` = "polite")
  )
}

checklist_switch <- function(kbd = TRUE) {
  htmltools::tags$button(
    type = "button",
    class = "g-switch js-checklist",
    `aria-pressed` = "false",
    title = "Checklist (C)",
    htmltools::span(class = "box", `aria-hidden` = "true"),
    if (kbd) htmltools::tagList("Checklist ", htmltools::tags$kbd("C")) else "Checklist"
  )
}

# What the page's script needs to start: each section's step targets, and the settings from the address.
book_boot <- function(query) {
  steps <- stats::setNames(lapply(book_sections$id, function(id) section_fn(id, "%s_steps")()), book_sections$id)
  contract <- book_contract()
  boot <- list(
    names = list(
      global = contract$global,
      css = contract$css,
      id = contract$attributes$id,
      events = lapply(contract$events, function(e) e$name)
    ),
    targets = lapply(steps, function(s) lapply(s, function(x) x$target)),
    section = query$section,
    checklist = query$checklist
  )
  htmltools::tags$script(
    type = "application/json",
    id = "book-boot",
    htmltools::HTML(jsonlite::toJSON(boot, auto_unbox = TRUE))
  )
}

book_page <- function(query) {
  bslib::page(
    title = "hovercraft book",
    theme = book_theme(),
    lang = "en",
    htmltools::tags$head(
      htmltools::tags$link(rel = "stylesheet", href = "shared.css"),
      htmltools::tags$link(rel = "stylesheet", href = "book.css"),
      htmltools::tags$script(src = "shared.js"),
      htmltools::tags$script(src = "book.js")
    ),
    hovercraft::tip_dependency(style = hovercraft::tip_style(query$preset, motion = query$motion)),
    book_boot(query),
    htmltools::a(class = "vh skip", href = "#start", "Skip to the first section"),
    htmltools::tags$header(
      class = "bar",
      id = "bar",
      htmltools::a(class = "wordmark", href = "#start", htmltools::tags$b("hovercraft"), htmltools::span("book")),
      htmltools::tags$button(
        type = "button",
        class = "where",
        id = "where",
        `aria-expanded` = "false",
        `aria-controls` = "toc-sheet",
        htmltools::span(class = "n", id = "where-n", book_sections$n[[1]]),
        htmltools::span(id = "where-t", book_sections$title[[1]])
      ),
      checklist_switch(kbd = FALSE),
      htmltools::tags$i(class = "prog")
    ),
    htmltools::tags$nav(
      class = "toc-sheet",
      id = "toc-sheet",
      `aria-label` = "Contents, as a list to open",
      toc_search("toc-q-sheet"),
      book_toc(),
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
      `aria-label` = "Checklist: the current step",
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
        `aria-label` = "Contents",
        htmltools::a(class = "wordmark", href = "#start", htmltools::tags$b("hovercraft"), htmltools::span("book")),
        htmltools::div(
          class = "g-head",
          checklist_switch(),
          htmltools::tags$button(type = "button", class = "g-resume g-ui", id = "g-resume")
        ),
        toc_search("toc-q"),
        book_toc("toc"),
        htmltools::div(class = "index-foot", theme_set("theme_rail"))
      ),
      htmltools::tags$main(
        id = "main",
        lapply(book_sections$id, function(id) {
          ui <- section_fn(id, "plate_%s_ui")
          if (length(formals(ui)) > 0) ui(query) else ui()
        }),
        htmltools::p(
          class = "colophon",
          "Set in Charter and Avenir Next where your system has them, with a monospace for code. Tips drawn by the",
          "tip engine that ships with hovercraft. Figures are made-up data."
        )
      )
    )
  )
}

book_server <- function(input, output, session) {
  for (id in book_sections$id) {
    section_fn(id, "plate_%s_server")(input, output, session)
  }
}
