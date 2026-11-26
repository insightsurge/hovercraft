# The bench's page and server. The address chooses what is drawn:
#   ?tab=<id>       open on that tab
#   ?only=<id>      draw that tab alone and run only its server (the tests use this, and the heavy tabs need it)
#   ?host=1         a page that holds the bench in an iframe (with ?only, the frame shows that tab)
#   ?strip=0|1      the log strip at the bottom; on for the whole bench, off with ?only (the tests point at the page)
#   ?dir=rtl        a right-to-left page
#   ?theme=dark     start in dark mode
#   ?font=large     a 24 px root font, a tight line-height and a serif font
#   ?keys=no_pin|no_more|off, ?max_pins=1..10, ?labels=custom, ?style=<preset>: page-wide settings of
#                   use_hovercraft(). The page reads
#                   them once, so they live in the address and a change reloads the page; a setting left out is not
#                   written at all, as a reader's own app leaves it out.
# A value the bench does not know falls back to the default.

# `value` when it is one of `allowed`, else `default`.
bench_choice <- function(value, allowed, default) {
  if (length(value) == 1L && value %in% allowed) value else default
}

# The page's settings, from its address (the request's query string, or the session's `url_search`).
bench_query <- function(search) {
  query <- shiny::parseQueryString(search %||% "")
  only <- bench_choice(query$only, bench_tabs$id, "")
  list(
    tab = if (nzchar(only)) only else bench_choice(query$tab, bench_tabs$id, bench_tabs$id[[1]]),
    only = only,
    host = identical(query$host, "1"),
    strip = if (is.null(query$strip)) !nzchar(only) else identical(query$strip, "1"),
    dir = bench_choice(query$dir, c("ltr", "rtl"), "ltr"),
    theme = bench_choice(query$theme, c("light", "dark"), "light"),
    keys = bench_choice(query$keys, c("no_pin", "no_more", "off"), ""),
    max_pins = bench_choice(query$max_pins, as.character(1:10), ""),
    labels = bench_choice(query$labels, "custom", ""),
    style = bench_choice(query$style, hovercraft::tip_presets()$name, ""),
    font = bench_choice(query$font, "large", "")
  )
}

# The tabs this page draws: all of them, or the one ?only names.
bench_shown <- function(settings) {
  if (nzchar(settings$only)) settings$only else bench_tabs$id
}

# The words ?labels=custom gives every tip on the page.
bench_custom_labels <- c(more = "Show all", less = "Show less", close = "Dismiss", pin = "Keep", copy = "Copy code")

# use_hovercraft() with only the settings the address gives: it tells a written default from one left out.
bench_hovercraft <- function(settings) {
  args <- list()
  if (nzchar(settings$keys)) {
    args$keys <- switch(settings$keys, no_pin = c(pin = FALSE), no_more = c(more = FALSE), off = FALSE)
  }
  if (nzchar(settings$max_pins)) {
    args$max_pins <- as.integer(settings$max_pins)
  }
  if (nzchar(settings$labels)) {
    args$labels <- bench_custom_labels
  }
  if (nzchar(settings$style)) {
    args$style <- hovercraft::tip_style(settings$style)
  }
  do.call(hovercraft::use_hovercraft, args)
}

# The engine's contract (tip.json): its event, attribute and class names, so nothing here spells them out.
bench_contract <- function() {
  jsonlite::read_json(system.file("engine", "tip.json", package = "hovercraft"), simplifyVector = FALSE)
}

# What the page's scripts need from the contract: the names of its events and attributes. The engine also sends
# `<prefix>:link` when a link in a tip is followed; its contract does not list that one, so it is added here.
bench_boot <- function() {
  contract <- bench_contract()
  events <- lapply(contract$events, function(event) event$name)
  events$link <- paste0(contract$prefix, ":link")
  htmltools::tags$script(
    id = "bench-boot",
    type = "application/json",
    htmltools::HTML(jsonlite::toJSON(
      list(events = events, attributes = contract$attributes, css = contract$css),
      auto_unbox = TRUE
    ))
  )
}

# The page that holds the bench in an iframe, for the practice of embedding.
bench_host_page <- function(settings) {
  src <- paste0("/?only=", if (nzchar(settings$only)) settings$only else bench_tabs$id[[1]], "&strip=0")
  htmltools::tags$html(
    htmltools::tags$body(htmltools::tags$iframe(id = "f", src = src, width = "850", height = "650"))
  )
}

bench_ui <- function(request) {
  settings <- bench_query(request$QUERY_STRING)
  if (settings$host) {
    return(bench_host_page(settings))
  }
  panels <- lapply(bench_shown(settings), function(id) {
    bslib::nav_panel(
      bench_tabs$title[bench_tabs$id == id],
      value = id,
      get(paste0("tab_", id, "_ui"), mode = "function")()
    )
  })
  page <- do.call(
    bslib::page_navbar,
    c(
      list(
        title = "hovercraft test bench",
        id = "bench_tab",
        selected = settings$tab,
        fillable = FALSE,
        theme = bslib::bs_theme(version = 5),
        header = htmltools::tagList(
          htmltools::tags$head(
            htmltools::tags$link(rel = "stylesheet", href = "bench.css"),
            htmltools::tags$script(src = "bench-log.js"),
            htmltools::tags$script(src = "bench.js")
          ),
          bench_boot(),
          if (settings$dir == "rtl") htmltools::tags$script(htmltools::HTML("document.documentElement.dir = 'rtl';")),
          if (nzchar(settings$font)) {
            htmltools::tags$style("html { font-size: 24px; line-height: 1.1; font-family: Georgia, serif; }")
          },
          bench_hovercraft(settings)
        ),
        footer = if (settings$strip) bench_log_strip()
      ),
      panels,
      list(bslib::nav_spacer(), bslib::nav_item(bslib::input_dark_mode(id = "bench_dark", mode = settings$theme)))
    )
  )
  page
}

bench_server <- function(input, output, session) {
  settings <- bench_query(shiny::isolate(session$clientData$url_search))
  for (id in bench_shown(settings)) {
    get(paste0("tab_", id, "_server"), mode = "function")(input, output, session)
  }
}
