# The test bench (apps/testbench): one Shiny app with the Shiny practices and the behaviour scenarios, which a person
# runs by hand and the browser tests start here. Found from the test folder, so it is there when the tests run from the
# checkout (devtools::test(), and the full-tests job, which runs the installed package against this folder).

bench_dir <- function() {
  normalizePath(testthat::test_path("apps", "testbench"), mustWork = FALSE)
}

# Skips where the tests run without the bench's folder beside them.
skip_if_no_bench <- function() {
  testthat::skip_if_not(dir.exists(bench_dir()), "The test bench is not beside these tests.")
}

# Starts the bench in a background R process, as start_app() starts any app, and stops it when `env` ends.
start_bench <- function(env = parent.frame()) {
  start_app(function(dir) shiny::shinyAppDir(dir), env, dir = bench_dir())
}

# The address of the bench drawing one tab alone (`only`), with other settings of its address in `query`.
bench_url <- function(bench, only = NULL, query = list()) {
  query <- c(if (!is.null(only)) list(only = only), query)
  if (length(query) == 0L) {
    return(bench$url)
  }
  paste0(bench$url, "/?", paste0(names(query), "=", unlist(query), collapse = "&"))
}

# Lines of the page's log, given as JSON, as a list of lists (n, t, kind, id, detail): read from JSON so nested details
# keep their shape.
bench_log_rows <- function(json) {
  jsonlite::fromJSON(json, simplifyVector = FALSE)
}

# Opens the bench in a tab, waits for Shiny, and adds the log's helpers to the page's own:
#   log(since)                     the lines after line number `since` (0 for all)
#   mark()                         the number of the newest line, to pass as `since` later
#   log_has(kind, id, ..., since)  waits until a line of that kind (and id, and detail fields) arrives after `since`
bench_page <- function(bench, only = NULL, query = list(), env = parent.frame()) {
  page <- local_page(bench_url(bench, only, query), env)
  page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true")
  log <- function(since = 0) {
    bench_log_rows(page$evaluate(sprintf(
      "JSON.stringify((window.benchLog ?? []).filter((l) => l.n > %d))",
      as.integer(since)
    )))
  }
  mark <- function() {
    page$evaluate("(window.benchLog ?? []).reduce((n, l) => Math.max(n, l.n), 0)")
  }
  log_has <- function(kind, id = NULL, ..., since = 0) {
    want <- list(...)
    id_js <- if (is.null(id)) "null" else jsonlite::toJSON(id, auto_unbox = TRUE)
    page$wait_until(sprintf(
      "(window.benchLog ?? []).some((l) => l.n > %d && l.kind === %s && (%s === null || l.id === %s) &&
        Object.entries(%s).every(([k, v]) => l.detail && l.detail[k] === v))",
      as.integer(since),
      jsonlite::toJSON(kind, auto_unbox = TRUE),
      id_js,
      id_js,
      if (length(want) == 0L) "{}" else jsonlite::toJSON(want, auto_unbox = TRUE)
    ))
  }
  c(page, list(log = log, mark = mark, log_has = log_has))
}
