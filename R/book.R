# The book's sections, in order. The app keeps the same list (`book_sections` in inst/book/R/data.R); a test holds the
# two together.
book_section_ids <- c(
  "start",
  "placement",
  "templates",
  "disabled",
  "pins",
  "more",
  "inputs",
  "looks",
  "size",
  "colours",
  "markers",
  "server",
  "output",
  "tables",
  "columns",
  "table_pins"
)

# Whether R can open an app here; a function so a test can stand in for an interactive session.
book_interactive <- function() interactive()

#' Open hovercraft book
#'
#' `r lifecycle::badge("experimental")`
#' Opens hovercraft book, a Shiny app that shows every feature of the package on one page, in six parts: getting
#' started, content, interaction, look, tips from Shiny, and tables. Each section is one feature: a live figure to rest
#' the pointer on, a sentence of choices that change it, the R code that makes what you see (ready to copy), a short
#' list of what to test, and the Shiny inputs the page sends. The code a section shows is the code that draws its
#' figure. The index has a search box, and the checklist (the key C) shows one test step at a time and ticks a step
#' when the server receives the input that proves it.
#'
#' The book needs the suggested packages shiny and bslib; its table sections also need `DT` or `reactable`. Its
#' files are in `system.file("book", package = "hovercraft")`.
#'
#' Unlike the other functions of the package, its name does not start with `tip_`: it opens the package's own book,
#' not a tip.
#'
#' @param section The section to open on, or `NULL` (the default) for the first: one of `"start"`, `"placement"`,
#'   `"templates"`, `"disabled"`, `"pins"`, `"more"`, `"inputs"`, `"looks"`, `"size"`, `"colours"`, `"markers"`,
#'   `"server"`, `"output"`, `"tables"`, `"columns"` or `"table_pins"`.
#' @param ... Must be empty. It makes a misspelt argument an error.
#' @param checklist `TRUE` to open with the checklist on; `FALSE` (the default) to open without it.
#' @param run `TRUE` (the default) to run the app, which needs an interactive R session; `FALSE` to return it.
#'
#' @returns With `run = FALSE`, the app, a `shiny.appobj`. With `run = TRUE`, what [shiny::runApp()] returns when the
#'   app stops.
#' @export
#' @examplesIf requireNamespace("shiny", quietly = TRUE) && requireNamespace("bslib", quietly = TRUE)
#' app <- hovercraft_book(section = "pins", run = FALSE)
#' inherits(app, "shiny.appobj")
hovercraft_book <- function(section = NULL, ..., checklist = FALSE, run = TRUE) {
  tip_check_dots(..., args = c("section", "checklist", "run"))
  tip_arg(
    checkmate::check_choice(section, book_section_ids, null.ok = TRUE),
    "section",
    paste0("Give one of ", paste0("\"", book_section_ids, "\"", collapse = ", "), ", or {.code NULL}.")
  )
  tip_arg_flag(checklist, "checklist")
  tip_arg_flag(run, "run")
  rlang::check_installed(c("shiny", "bslib"), reason = "to open hovercraft book.")
  app <- shiny::shinyAppDir(system.file("book", package = "hovercraft", mustWork = TRUE))
  # The first page request carries the choices in its address, as a link would; an address that names its own
  # section or checklist keeps it.
  handler <- app$httpHandler
  start <- Filter(Negate(is.null), list(section = section, checklist = if (checklist) "1"))
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
  if (!book_interactive()) {
    tip_abort(
      c(
        "{.fn hovercraft_book} runs the app only in an interactive R session.",
        i = "Use {.code hovercraft_book(run = FALSE)} to get the app without running it."
      ),
      class = "hovercraft_error_book"
    )
  }
  shiny::runApp(app)
}
