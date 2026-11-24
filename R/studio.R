# The studio's plates, in the order of the sheet. The app keeps the same list (`studio_plates` in
# inst/studio/R/data.R); a test holds the two together.
tip_studio_plates <- c("start", "basics", "templates", "more", "pins", "disabled", "output", "tables", "server")

# Whether R can open an app here; a function so a test can stand in for an interactive session.
tip_studio_interactive <- function() interactive()

#' Open hovercraft studio
#'
#' `r lifecycle::badge("experimental")`
#' Opens hovercraft studio, a Shiny app that shows every tip feature on one page. Each feature is a numbered plate:
#' a live figure to rest the pointer on, a sentence of choices that change it, the R code that makes what you see
#' (ready to copy), a short list of what to test, and the Shiny inputs the page sends. The code a plate shows is the
#' code that draws its figure. "Guide me" (the key G) shows one test step at a time and ticks a step when the server
#' receives the input that proves it.
#'
#' The studio needs the suggested packages shiny and bslib; its Tables plate also needs `DT` or `reactable`. Its
#' files are in `system.file("studio", package = "hovercraft")`.
#'
#' @param plate The plate to open on, or `NULL` (the default) for the first: one of `"start"`, `"basics"`,
#'   `"templates"`, `"more"`, `"pins"`, `"disabled"`, `"output"`, `"tables"` or `"server"`.
#' @param ... Must be empty. It makes a misspelt argument an error.
#' @param guide `TRUE` to open with Guide me on; `FALSE` (the default) to open without it.
#' @param run `TRUE` (the default) to run the app, which needs an interactive R session; `FALSE` to return it.
#'
#' @returns With `run = FALSE`, the app, a `shiny.appobj`. With `run = TRUE`, what [shiny::runApp()] returns when the
#'   app stops.
#' @export
#' @examplesIf requireNamespace("shiny", quietly = TRUE) && requireNamespace("bslib", quietly = TRUE)
#' app <- tip_studio(plate = "pins", run = FALSE)
#' inherits(app, "shiny.appobj")
tip_studio <- function(plate = NULL, ..., guide = FALSE, run = TRUE) {
  tip_check_dots(..., args = c("plate", "guide", "run"))
  tip_arg(
    checkmate::check_choice(plate, tip_studio_plates, null.ok = TRUE),
    "plate",
    paste0("Give one of ", paste0("\"", tip_studio_plates, "\"", collapse = ", "), ", or {.code NULL}.")
  )
  tip_arg_flag(guide, "guide")
  tip_arg_flag(run, "run")
  rlang::check_installed(c("shiny", "bslib"), reason = "to open hovercraft studio.")
  app <- shiny::shinyAppDir(system.file("studio", package = "hovercraft", mustWork = TRUE))
  # The first page request carries the choices in its address, as a link would; an address that names its own plate
  # or guide keeps it.
  handler <- app$httpHandler
  start <- Filter(Negate(is.null), list(plate = plate, guide = if (guide) "1"))
  app$httpHandler <- function(request) {
    query <- shiny::parseQueryString(request$QUERY_STRING %||% "")
    values <- utils::modifyList(start, query)
    if (length(values)) {
      request$QUERY_STRING <- paste0(
        "?",
        paste(names(values), vapply(values, utils::URLencode, "", reserved = TRUE), sep = "=", collapse = "&")
      )
    }
    handler(request)
  }
  if (!run) {
    return(app)
  }
  if (!tip_studio_interactive()) {
    tip_abort(
      c(
        "{.fn tip_studio} runs the app only in an interactive R session.",
        i = "Use {.code tip_studio(run = FALSE)} to get the app without running it."
      ),
      class = "hovercraft_error_studio"
    )
  }
  shiny::runApp(app)
}
