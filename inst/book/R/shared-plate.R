# Shared helpers: the frame of a plate (a numbered feature on the sheet), its margin notes and the choices set as a
# sentence. Each returns tags; the server fills the parts it owns by custom messages (see shared-log.R and
# shared-steps.R) or by outputs named after the plate (`<plate>_figure`, `<plate>_notes`).

# One plate, a section of the book: the head (number and title, and the part's title on the part's first section),
# the figure with its sentence of choices and caption, and the margin. Its number, title and part come from
# `book_sections`. `figure` is what goes on the stage; by default the plate's own output, `<plate>_figure`.
plate <- function(
  id,
  lede,
  caption,
  steps,
  code = "",
  controls = NULL,
  figure = shiny::uiOutput(paste0(id, "_figure")),
  log_empty = "Nothing yet.",
  log_title = "Inputs sent",
  wide = FALSE,
  before = NULL
) {
  at <- book_sections[book_sections$id == id, ]
  heading <- paste0(id, "-h")
  caption_id <- paste0(id, "-cap")
  htmltools::tags$section(
    class = paste(c("plate", if (wide) "wide"), collapse = " "),
    id = id,
    `data-n` = at$n,
    `data-title` = at$title,
    `data-part` = at$part,
    `aria-labelledby` = heading,
    before,
    part_head(at),
    htmltools::tags$header(
      class = "plate-head",
      htmltools::p(class = "plate-no", `aria-hidden` = "true", at$n),
      htmltools::h3(id = heading, title_text(at))
    ),
    if (!is.null(lede)) htmltools::p(class = "lede", lede),
    htmltools::div(
      class = "plate-body",
      htmltools::tags$figure(
        class = "specimen",
        `aria-labelledby` = caption_id,
        htmltools::div(class = "stage", figure),
        shiny::uiOutput(paste0(id, "_notes"), class = "figure-notes"),
        if (!is.null(controls)) htmltools::div(class = "legend-line controls", controls),
        htmltools::tags$figcaption(id = caption_id, htmltools::tags$b(paste0(at$n, ".")), caption)
      ),
      htmltools::tags$aside(
        class = paste(c("margin", if (wide) "wide-margin"), collapse = " "),
        `aria-label` = paste("Margin of section", at$n),
        htmltools::div(
          class = "margin-head",
          htmltools::p(htmltools::span(paste("Margin of section", at$n)), at$title),
          htmltools::tags$button(type = "button", class = "btn-quiet js-close-drawer", "Close")
        ),
        code_note(id, code),
        steps_note(id, steps),
        log_note(id, log_empty, log_title),
        covers_note(id)
      )
    )
  )
}

# The title of a section as its heading reads: a hidden number first, for a screen reader.
title_text <- function(at) {
  htmltools::tagList(htmltools::span(class = "vh", paste0(at$n, " ")), at$title)
}

# On a part's first section, the part's own heading: its number, title and a line about it.
part_head <- function(at) {
  first <- book_sections$id[book_sections$part == at$part][[1]]
  if (!identical(first, at$id)) {
    return(NULL)
  }
  part <- book_parts[book_parts$n == at$part, ]
  htmltools::div(
    class = "part-head",
    id = paste0("part-", part$n),
    htmltools::h2(htmltools::span(class = "part-no", paste("Part", part$n)), htmltools::span(part$title)),
    htmltools::p(class = "part-lede", part$lede)
  )
}

# The R code note: the code for the plate's settings, with a Copy button. The server sends new code with a revision
# (shared.js keeps the page's own count), so Copy never copies code drawn for an older choice. Long code shows its
# first lines and a button that opens the rest in place; shared.js shows the button only when the code is long, so
# with no script the code is whole.
code_note <- function(id, code) {
  htmltools::tags$section(
    class = "note",
    # The button sits beside the heading, not inside it, so the heading reads "R code".
    htmltools::div(
      class = "note-head",
      htmltools::h4("R code"),
      htmltools::tags$button(type = "button", class = "btn-quiet js-copy", `aria-label` = "Copy the R code", "Copy")
    ),
    htmltools::tags$pre(
      id = paste0(id, "-code"),
      class = "code",
      `data-code` = id,
      `data-rev` = "0",
      tabindex = "0",
      code
    ),
    htmltools::tags$button(
      type = "button",
      class = "btn-quiet code-more js-code-more",
      `aria-controls` = paste0(id, "-code"),
      `aria-expanded` = "false",
      hidden = NA,
      "Show all"
    )
  )
}

# The What to test note: each step, with the box Checklist ticks and a line that says when a step ticked itself.
steps_note <- function(id, steps) {
  htmltools::tags$section(
    class = "note tests-note",
    htmltools::h4("What to test", htmltools::span(class = "aside ck-count-h", `data-count` = id)),
    htmltools::tags$ol(
      class = "tests",
      role = "list",
      `data-steps` = id,
      lapply(seq_along(steps), function(i) {
        s <- steps[[i]]
        htmltools::tags$li(
          `data-i` = i - 1L,
          `data-manual` = if (is.null(s$proof)) "true",
          htmltools::tags$button(
            type = "button",
            class = "ck-tick ck-ui",
            `data-ck` = "tick",
            `data-i` = i - 1L,
            `aria-pressed` = "false",
            `aria-label` = paste("Step", i, "done"),
            htmltools::span(i)
          ),
          htmltools::span(
            inline_code(s$text),
            if (!is.null(s$bug)) htmltools::span(class = "bug", inline_code(s$bug))
          )
        )
      })
    ),
    htmltools::div(
      class = "ck-fold ck-ui",
      htmltools::p(class = "ck-step"),
      htmltools::tags$button(
        type = "button",
        class = "btn-quiet",
        `data-ck` = "back",
        "Back ",
        htmltools::tags$kbd("B")
      ),
      htmltools::tags$button(
        type = "button",
        class = "btn-quiet",
        `data-ck` = "next",
        "Next ",
        htmltools::tags$kbd("N")
      ),
      htmltools::p(class = "ck-said", `aria-live` = "polite")
    )
  )
}

# The Inputs sent note: what the server received while you tried the plate, newest first.
# What the section shows, from contents.R, grouped by the badge the help gives each: stable, experimental, or stable
# for some values only (such as `style` by a preset's name). Closed until opened; the words say the group, so the
# badge never rests on colour.
covers_note <- function(id) {
  covers <- book_sections$covers[[match(id, book_sections$id)]]
  if (is.null(book_cache$badges)) {
    args <- book_rd_args()
    book_cache$badges <- stats::setNames(args$badge, book_rd_names(args))
  }
  badge <- book_cache$badges[covers]
  groups <- c(stable = "Stable", mixed = "Stable for some values", experimental = "Experimental")
  htmltools::tags$details(
    class = "note covers-note",
    htmltools::tags$summary(htmltools::h4("What this section shows")),
    lapply(names(groups), function(g) {
      these <- covers[badge %in% g]
      if (!length(these)) {
        return(NULL)
      }
      htmltools::tags$p(
        class = paste0("covers covers-", g),
        htmltools::span(class = "covers-badge", groups[[g]]),
        lapply(these, htmltools::tags$code)
      )
    })
  )
}

log_note <- function(id, empty, title = "Inputs sent") {
  htmltools::tags$section(
    class = "note",
    htmltools::h4(title),
    htmltools::tags$ol(class = "log", `data-log` = id, `aria-live` = "polite"),
    htmltools::p(class = "log-empty", `data-empty` = id, empty)
  )
}

# Text with `code` between backticks and *words* between stars, as HTML. One string, so no space is added between a
# piece of code and the punctuation after it; every piece is escaped first.
inline_code <- function(text) {
  pieces <- strsplit(text, "`", fixed = TRUE)[[1]]
  html <- vapply(
    seq_along(pieces),
    function(i) {
      piece <- htmltools::htmlEscape(pieces[[i]])
      if (i %% 2 == 0) {
        return(paste0("<code>", piece, "</code>"))
      }
      gsub("\\*([^*]+)\\*", "<i>\\1</i>", piece)
    },
    ""
  )
  htmltools::HTML(paste(html, collapse = ""))
}

# A choice set inside a sentence: a run of words where the chosen one sits on a soft fill. A real Shiny radio group (Shiny's own
# radio binding reads `.shiny-input-radiogroup` and the radios named after its id), so `input[[id]]` holds the value.
# `choices` is a named vector: values, named by the words shown.
choice_set <- function(id, legend, choices, selected = choices[[1]]) {
  words <- names(choices) %||% choices
  items <- lapply(seq_along(choices), function(i) {
    radio <- htmltools::tags$input(type = "radio", name = id, value = choices[[i]])
    if (identical(choices[[i]], selected)) {
      radio <- htmltools::tagAppendAttributes(radio, checked = NA)
    }
    # The slash stays on the line of the word before it, so a row that wraps never starts with one.
    htmltools::span(
      class = "pick",
      htmltools::tags$label(radio, htmltools::span(words[[i]])),
      if (i < length(choices)) htmltools::span(class = "sep", `aria-hidden` = "true", "/")
    )
  })
  htmltools::tags$fieldset(
    id = id,
    class = "choice shiny-input-radiogroup",
    htmltools::tags$legend(class = "vh", legend),
    items
  )
}

# The page colours: auto, light or dark. Not a Shiny input of its own: shared.js keeps every copy in step, sets the
# page's `data-bs-theme` and sends the colours the page really shows as `input$page_mode`.
theme_set <- function(name, legend = "Page", show_legend = TRUE) {
  choices <- c(auto = "Auto", light = "Light", dark = "Dark")
  htmltools::tags$fieldset(
    class = "choice theme-set",
    `data-sync` = "theme",
    htmltools::tags$legend(class = if (show_legend) "lab" else "vh", legend),
    lapply(seq_along(choices), function(i) {
      radio <- htmltools::tags$input(type = "radio", name = name, value = names(choices)[[i]])
      if (i == 1L) {
        radio <- htmltools::tagAppendAttributes(radio, checked = NA)
      }
      htmltools::tags$label(radio, htmltools::span(if (show_legend) choices[[i]] else tolower(choices[[i]])))
    })
  )
}
