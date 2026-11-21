# The tip engine is a JavaScript file and a stylesheet copied into inst/engine/www/ by tools/copy-engine.R, with
# engine.json beside it (not inside www/, so no page ever holds it), which records what was copied: the engine's
# version, commit and branch, each file's sha256, the names the engine uses on the page and the rules it follows. Nothing in here is typed twice: R reads the names and the rules.

tip_state <- new.env(parent = emptyenv())

tip_path <- function(...) {
  system.file("engine", ..., package = "hovercraft")
}

tip_engine <- function() {
  if (is.null(tip_state$engine)) {
    tip_state$engine <- jsonlite::read_json(tip_path("engine.json"), simplifyVector = FALSE)
  }
  tip_state$engine
}

tip_engine_version <- function() tip_engine()$engine

tip_names <- function() tip_engine()$names

tip_rules <- function() tip_engine()$rules$tooltip

# The templates this session has defined with tip_define(), by name: their fields. Kept so tip_on() can check a record
# against a template; it is cleared by tip_reset() (tests, and a session that starts a new page).
tip_known <- function() {
  if (is.null(tip_state$templates)) list() else tip_state$templates
}

tip_reset <- function() {
  tip_state$templates <- NULL
  invisible()
}

#' The tip engine as an HTML dependency
#'
#' `r lifecycle::badge("experimental")`
#' The engine's script and CSS, ready to add to a page. [tip_on()] and [tip_define()] attach it for you; use this when
#' you build the page's head yourself. The dependency is versioned with the engine, not with this package. The engine
#' reads Bootstrap's colours where the page has them (bslib), and plain colours otherwise.
#'
#' @returns An [htmltools::htmlDependency()].
#' @family tooltips
#' @examples
#' tip_dependency()
#' @export
tip_dependency <- function() {
  htmltools::htmlDependency(
    name = "hovercraft",
    version = tip_engine_version(),
    src = "engine/www",
    package = "hovercraft",
    script = "tip.js",
    stylesheet = "tip.css",
    head = sprintf("<script>%s.attach(document);</script>", tip_names()$global)
  )
}
