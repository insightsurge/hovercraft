# The book's contents: its parts and sections in reading order, and what each section shows. This is the one place a
# section is named. The numbers ("3.2") come from the order; the ids stay as they are, since links and
# hovercraft_book(section =) use them. Each section also has its own file, R/plate-<id>.R, with plate_<id>_ui(),
# plate_<id>_server() and <id>_steps().
#
# `covers` lists the exports and arguments the section's shown R code uses, as list(function = arguments), with
# character() for a function shown without arguments. test-book.R reads the arguments from the help pages, fails
# while one is in no section, and runs every section's choices to check that its code really uses each one it lists.
# `prose` is only for hovercraft_book()'s own arguments, which the first section shows in its words rather than its
# code; the test looks for them in the section's text instead.
#
# hovercraft_book() reads this file on its own, so it holds only plain data and the two helpers below.

book_part <- function(title, lede, ...) list(title = title, lede = lede, sections = list(...))

book_section <- function(id, title, covers = list(), prose = list()) {
  list(id = id, title = title, covers = covers, prose = prose)
}

book_contents <- list(
  book_part(
    "Getting started",
    "One tip, and where it opens.",
    book_section(
      "start",
      "Your first tip",
      covers = list(
        tip_on = c("tag", "text", "id", "events", "template", "record", "more", "pin", "style", "marker"),
        tip_define = character(),
        tip_template = c("head", "body", "foot"),
        tip_title = "text",
        tip_text = "text",
        tip_badge = c("text", "status"),
        tip_fields = character(),
        tip_code = c("field", "language", "copy"),
        tip_style = c("preset", "motion", "theme"),
        tip_theme = c("light", "dark"),
        tip_marker = "type"
      ),
      prose = list(hovercraft_book = c("section", "checklist", "run"))
    ),
    book_section("placement", "Where the tip opens", covers = list(tip_on = c("tag", "text", "placement")))
  ),
  book_part(
    "Content",
    "What a tip says: words, templates, and the reason a control is off.",
    book_section(
      "templates",
      "Templates: a tip filled in from a record",
      covers = list(
        tip_template = c("head", "body", "foot"),
        tip_title = "text",
        tip_text = "text",
        tip_badge = c("text", "status"),
        tip_fields = character(),
        tip_define = character(),
        tip_on = c("template", "record")
      )
    ),
    book_section(
      "disabled",
      "Tips on disabled controls",
      covers = list(tip_on = c("tag", "text", "when_disabled", "id", "events"))
    )
  ),
  book_part(
    "Interaction",
    "What a reader does with a tip, and what your server hears.",
    book_section(
      "pins",
      "Pinning a tip open",
      covers = list(
        tip_define = character(),
        tip_template = c("head", "body"),
        tip_title = "text",
        tip_text = "text",
        tip_fields = character(),
        tip_code = c("field", "language", "copy"),
        tip_on = c("tag", "text", "template", "record", "placement", "pin", "id", "events")
      )
    ),
    book_section(
      "pin_limits",
      "Several pinned tips, and keyboard keys",
      covers = list(use_hovercraft = c("max_pins", "keys"))
    ),
    book_section(
      "more",
      "A short tip, with more on request",
      covers = list(
        tip_on = c("tag", "text", "more", "pin", "id", "events"),
        tip_more = c("content", "on")
      )
    ),
    book_section(
      "more_button",
      "The More button: its words, look and place",
      covers = list(tip_on = c("more", "pin", "id"), tip_more = c("content", "label", "pin", "as", "where"))
    ),
    book_section(
      "inputs",
      "What your Shiny server receives",
      covers = list(
        tip_define = character(),
        tip_template = c("head", "body"),
        tip_title = "text",
        tip_text = "text",
        tip_code = c("field", "language", "copy"),
        tip_on = c("tag", "text", "template", "record", "pin", "id", "events")
      )
    ),
    book_section(
      "words",
      "The tip's built-in words (Copy, Close, More)",
      covers = list(use_hovercraft = "labels", tip_on = "labels")
    )
  ),
  book_part(
    "Look",
    "How tips look: presets, size, your own colours and the marks that show a tip is there.",
    book_section(
      "looks",
      "Preset looks for the whole page",
      covers = list(
        tip_presets = character(),
        use_hovercraft = "style",
        tip_style = c("preset", "motion"),
        tip_on = "style"
      )
    ),
    book_section(
      "size",
      "Size and spacing",
      covers = list(tip_style = c("gap", "max_width", "pin_width", "class"), tip_on = c("style", "more"))
    ),
    book_section(
      "colours",
      "Your own colours",
      covers = list(tip_theme = c("light", "dark", "mode"), tip_style = "theme", tip_on = "style")
    ),
    book_section(
      "markers",
      "Markers: showing that a tag has a tip",
      covers = list(tip_on = "marker", tip_marker = c("type", "line", "icon"))
    )
  ),
  book_part(
    "From Shiny",
    "Tips your server opens, changes and fills with live outputs.",
    book_section(
      "server",
      "Opening, closing and changing tips from the server",
      covers = list(
        tip_on = c("tag", "text", "id", "events"),
        tip_show = c("id", "session"),
        tip_hide = c("id", "session"),
        tip_update = c("id", "text", "template", "record", "placement", "session")
      )
    ),
    book_section(
      "output",
      "Live Shiny output inside a tip",
      covers = list(
        tip_define = character(),
        tip_template = c("head", "body"),
        tip_title = "text",
        tip_fields = character(),
        tip_output = c("output", "keep_alive"),
        tip_on = c("tag", "template", "record", "pin", "id", "events")
      )
    )
  ),
  book_part(
    "Tables",
    "Tips on the rows, cells and headers of DT and reactable tables.",
    book_section(
      "tables",
      "Tips on table rows, cells and headers",
      covers = list(
        tip_table = c("widget", "rows", "cells", "headers", "style", "placement", "marker", "id")
      )
    ),
    book_section(
      "columns",
      "A different tip for one column",
      covers = list(
        tip_table = c("widget", "cells", "headers", "placement", "style", "marker"),
        tip_spec = c("content", "placement", "style", "marker", "more")
      )
    ),
    book_section(
      "table_pins",
      "Pinned table tips and their Shiny inputs",
      covers = list(
        tip_table = c("widget", "rows", "id", "max_pins"),
        tip_spec = c("content", "pin", "events"),
        tip_template = c("head", "body"),
        tip_title = "text",
        tip_text = "text",
        tip_code = c("field", "language", "copy")
      )
    ),
    book_section(
      "in_place",
      "A pinned tip opened as a card under its row",
      covers = list(tip_table = c("widget", "rows", "id"), tip_spec = c("content", "pin", "to"))
    ),
    book_section(
      "table_server",
      "Table tips (DT, reactable) from the server",
      covers = list(
        tip_table = c("widget", "rows", "id"),
        tip_show = c("id", "row", "column"),
        tip_hide = c("id", "row", "column"),
        tip_update = c("id", "row", "column")
      )
    )
  )
)

# The contents as two tables, which the page reads: the parts (n, title, lede) and the sections (id, part, n, title,
# and covers and prose as "fn(arg)" strings, or "fn" for a function shown without arguments).
book_covers_text <- function(covers) {
  text <- lapply(names(covers), function(fn) if (length(covers[[fn]])) paste0(fn, "(", covers[[fn]], ")") else fn)
  as.character(unlist(text, use.names = FALSE))
}

book_parts <- data.frame(
  n = as.character(seq_along(book_contents)),
  title = vapply(book_contents, `[[`, "", "title"),
  lede = vapply(book_contents, `[[`, "", "lede")
)

book_sections <- local({
  rows <- lapply(seq_along(book_contents), function(p) {
    sections <- book_contents[[p]]$sections
    data.frame(
      id = vapply(sections, `[[`, "", "id"),
      part = as.character(p),
      n = paste0(p, ".", seq_along(sections)),
      title = vapply(sections, `[[`, "", "title")
    )
  })
  sections <- do.call(rbind, rows)
  all <- unlist(lapply(book_contents, `[[`, "sections"), recursive = FALSE)
  sections$covers <- lapply(all, function(s) book_covers_text(s$covers))
  sections$prose <- lapply(all, function(s) book_covers_text(s$prose))
  sections
})
