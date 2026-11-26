# Shared helpers for the scenario tabs: the scenario list, a spot for each scenario, tagged buttons and made-up data.

# The 77 behaviour scenarios (HC-001 to HC-077), one row each: id, title, category, the tab that holds its spot, its
# kind (browser, r-only, human), how far tests cover it (covered, partly, none), the test files that do, and its
# before, steps and expected text. Read from scenarios.csv beside app.R (the working folder of a running app).
bench_scenarios <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- utils::read.csv("scenarios.csv", colClasses = "character", na.strings = character())
    }
    cache
  }
})

# The spot of scenario `id`: its id and title, how tests cover it, its steps folded away, and what to try it on (`...`,
# the stage). `note` says how to open it when it needs a setting in the address, or anything the stage can't show.
spot <- function(id, ..., note = NULL) {
  all <- bench_scenarios()
  row <- all[all$id == id, , drop = FALSE]
  if (nrow(row) != 1L) {
    stop("No scenario ", id, " in scenarios.csv.", call. = FALSE)
  }
  coverage <- switch(
    row$coverage,
    covered = c("text-bg-success", "Tested"),
    partly = c("text-bg-warning", "Partly tested"),
    c("text-bg-secondary", if (row$kind == "human") "By hand only" else "No test yet")
  )
  htmltools::tags$section(
    id = paste0("spot-", id),
    class = "bench-spot",
    `data-scenario` = id,
    htmltools::tags$header(
      class = "d-flex flex-wrap gap-2 align-items-baseline",
      htmltools::tags$span(class = "bench-spot-id", id),
      htmltools::tags$strong(row$title),
      htmltools::tags$span(class = paste("badge", coverage[[1]]), coverage[[2]])
    ),
    htmltools::tags$details(
      htmltools::tags$summary("Steps"),
      htmltools::tags$p(htmltools::tags$strong("Before: "), row$before),
      htmltools::tags$p(htmltools::tags$strong("Do: "), row$steps),
      htmltools::tags$p(htmltools::tags$strong("Expect: "), row$expected),
      if (nzchar(row$tests)) htmltools::tags$p("Tests: ", htmltools::tags$code(row$tests))
    ),
    if (!is.null(note)) htmltools::tags$p(class = "small text-body-secondary mb-1", note),
    if (...length() > 0L) htmltools::div(class = "bench-spot-stage", ...)
  )
}

# A link to this bench with other settings in its address, for a scenario that needs a page-wide setting.
bench_link <- function(label, ...) {
  query <- list(...)
  htmltools::tags$a(href = paste0("?", paste0(names(query), "=", unlist(query), collapse = "&")), label)
}

# A button with a tip. The tip id is the button id with underscores and "_tip"; `...` goes to tip_on().
tip_button <- function(id, label, tip, ..., class = "btn btn-outline-primary btn-sm") {
  hovercraft::tip_on(
    htmltools::tags$button(id = id, type = "button", class = class, label),
    tip,
    id = paste0(gsub("-", "_", id, fixed = TRUE), "_tip"),
    ...
  )
}

# A button whose tip pins, with a long form behind a More button when `more` is given.
pin_button <- function(id, label, tip, more = NULL, ...) {
  tip_button(id, label, tip, pin = TRUE, more = if (!is.null(more)) hovercraft::tip_more(more, ...))
}

# Made-up study sites, the same on every run.
bench_sites <- function(n = 60L) {
  i <- seq_len(n)
  data.frame(
    site = sprintf("Site %02d", i),
    country = rep(c("Kenya", "Peru", "Vietnam", "Norway", "Chile", "Ghana"), length.out = n),
    patients = (i * 37L) %% 133L + 8L,
    status = rep(c("On track", "Late", "Closed"), length.out = n)[(i * 7L) %% n + 1L]
  )
}

# The row tips every bench table uses: a pin, and a long form on the pin.
bench_rows <- function(..., pin = TRUE) {
  hovercraft::tip_spec(
    "**{{ site }}** ({{ country }}): {{ patients }} patients, {{ status }}",
    pin = pin,
    ...
  )
}
