# The log strip at the bottom of the page. www/bench-log.js keeps every tip event, input, custom message and page
# error in `window.benchLog` and draws the newest lines here; the tests read `window.benchLog` itself.
bench_log_strip <- function() {
  htmltools::tags$details(
    id = "bench-log",
    class = "bench-log",
    htmltools::tags$summary(
      "Log ",
      htmltools::tags$span(id = "bench-log-count", class = "badge text-bg-secondary", "0"),
      htmltools::tags$button(
        id = "bench-log-clear",
        type = "button",
        class = "btn btn-sm btn-outline-secondary",
        "Clear"
      )
    ),
    htmltools::tags$ol(id = "bench-log-lines", class = "bench-log-lines", reversed = NA)
  )
}
