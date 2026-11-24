# Shared helpers: the frame of a plate (a numbered feature on the sheet), its margin notes and the choices set as a
# sentence. Each returns tags; the server fills the parts it owns by custom messages (see shared-log.R and
# shared-steps.R) or by outputs named after the plate (`<plate>_figure`, `<plate>_notes`).

# One plate: the head (number, title, a line about it), the figure with its sentence of choices and caption, and the
# margin. `figure` is what goes on the stage; by default the plate's own output, `<plate>_figure`.
plate <- function(
  id,
  n,
  title,
  lede,
  caption,
  steps,
  code = "",
  controls = NULL,
  figure = shiny::uiOutput(paste0(id, "_figure")),
  log_empty = "Nothing yet.",
  log_title = "Inputs sent",
  wide = FALSE,
  head = NULL
) {
  heading <- paste0(id, "-h")
  caption_id <- paste0(id, "-cap")
  htmltools::tags$section(
    class = paste(c("plate", if (wide) "wide"), collapse = " "),
    id = id,
    `data-n` = n,
    `data-title` = title,
    `aria-labelledby` = heading,
    head %||%
      htmltools::tags$header(
        class = "plate-head",
        htmltools::p(class = "plate-no", `aria-hidden` = "true", n),
        htmltools::p(class = "plate-kicker", paste("Plate", n)),
        htmltools::h2(id = heading, title)
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
        htmltools::tags$figcaption(id = caption_id, htmltools::tags$b(paste0("Plate ", n, ".")), caption)
      ),
      htmltools::tags$aside(
        class = paste(c("margin", if (wide) "wide-margin"), collapse = " "),
        `aria-label` = paste("Margin of plate", n),
        htmltools::div(
          class = "margin-head",
          htmltools::p(htmltools::span(paste("Margin of plate", n)), title),
          htmltools::tags$button(type = "button", class = "btn-quiet js-close-drawer", "Close")
        ),
        code_note(id, code),
        steps_note(id, steps),
        log_note(id, log_empty, log_title)
      )
    )
  )
}

# The R code note: the code for the plate's settings, with a Copy button. The server sends new code with a revision
# (shared.js keeps the page's own count), so Copy never copies code drawn for an older choice.
code_note <- function(id, code) {
  htmltools::tags$section(
    class = "note",
    htmltools::h3(
      "R code",
      htmltools::tags$button(
        type = "button",
        class = "btn-quiet js-copy",
        `aria-label` = "Copy the R code",
        "Copy"
      )
    ),
    htmltools::tags$pre(class = "code", `data-code` = id, `data-rev` = "0", tabindex = "0", code)
  )
}

# The What to test note: each step, with the box Guide me ticks and a line that says when a step ticked itself.
steps_note <- function(id, steps) {
  htmltools::tags$section(
    class = "note tests-note",
    htmltools::h3("What to test", htmltools::span(class = "aside g-count-h", `data-count` = id)),
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
            class = "g-tick g-ui",
            `data-g` = "tick",
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
      class = "g-fold g-ui",
      htmltools::p(class = "g-step"),
      htmltools::tags$button(type = "button", class = "g-btn", `data-g` = "back", "Back ", htmltools::tags$kbd("B")),
      htmltools::tags$button(type = "button", class = "g-btn", `data-g` = "next", "Next ", htmltools::tags$kbd("N")),
      htmltools::p(class = "g-said", `aria-live` = "polite")
    )
  )
}

# The Inputs sent note: what the server received while you tried the plate, newest first.
log_note <- function(id, empty, title = "Inputs sent") {
  htmltools::tags$section(
    class = "note",
    htmltools::h3(title),
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

# A choice set inside a sentence: a run of words where the chosen one is boxed. A real Shiny radio group (Shiny's own
# radio binding reads `.shiny-input-radiogroup` and the radios named after its id), so `input[[id]]` holds the value.
# `choices` is a named vector: values, named by the words shown.
choice_set <- function(id, legend, choices, selected = choices[[1]]) {
  words <- names(choices) %||% choices
  items <- lapply(seq_along(choices), function(i) {
    radio <- htmltools::tags$input(type = "radio", name = id, value = choices[[i]])
    if (identical(choices[[i]], selected)) {
      radio <- htmltools::tagAppendAttributes(radio, checked = NA)
    }
    # The slash stays on the line of the word after it.
    htmltools::span(
      class = "pick",
      if (i > 1L) htmltools::span(class = "sep", `aria-hidden` = "true", "/"),
      htmltools::tags$label(radio, htmltools::span(words[[i]]))
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
