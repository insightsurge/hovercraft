# The tip engine is a JavaScript file and a stylesheet copied into inst/engine/www/ by tools/copy-engine.R, with
# engine.json beside it (not inside www/, so no page ever holds it), which records what was copied: the engine's
# version, commit and branch, each file's sha256, the names the engine uses on the page and the rules it follows.
# Nothing in here is typed twice: R reads the names and the rules.

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
#' you build the page's head yourself, or to give the page a theme. The dependency is versioned with the engine, not
#' with this package. The engine reads Bootstrap's colours where the page has them (bslib), and plain colours otherwise.
#'
#' @param theme How the tips look, or `NULL` (the default) to take the page's own bslib variables, so a page with a
#'   `bslib::bs_theme()` follows it, light and dark, with nothing more. One of:
#'   * a [tip_theme()];
#'   * a [bslib::bs_theme()], read through its variables (bslib 0.6.0 or later). The tip is the page inverted: its fill is
#'     the theme's text colour and its words the theme's background, with the border colour, corner and font;
#'   * a named list whose names are roles (`list(bg = "#1b3a6b", ink = "#ffffff")`), the same as
#'     `tip_theme(light = )`, so one set for both modes;
#'   * a named list whose names all start with `--`, CSS variables set as they are for both modes, such as
#'     `list("--hovercraft-tip-bg" = "#1b3a6b")`: the engine's own variables, or any variable the page's CSS reads.
#'
#' @returns An [htmltools::htmlDependency()]; with a `theme`, a list of two: the engine and `hovercraft-theme`, which
#'   holds only the theme's CSS. Put it once in the page's UI, for example
#'   `bslib::page_fluid(tip_dependency(theme = my_theme), ...)`.
#' @family tooltips
#' @examples
#' tip_dependency()
#' tip_dependency(theme = tip_theme(light = list(bg = "#1b3a6b", ink = "#ffffff")))
#' tip_dependency(theme = list("--hovercraft-tip-radius" = "8px"))
#' @export
tip_dependency <- function(theme = NULL) {
  engine <- htmltools::htmlDependency(
    name = "hovercraft",
    version = tip_engine_version(),
    src = "engine/www",
    package = "hovercraft",
    script = "tip.js",
    stylesheet = "tip.css",
    head = sprintf("<script>%s.attach(document);</script>", tip_names()$global)
  )
  theme <- tip_as_theme(theme)
  css <- if (is.null(theme)) "" else tip_theme_css(theme)
  if (!nzchar(css)) {
    return(engine)
  }
  list(
    engine,
    htmltools::htmlDependency(
      name = "hovercraft-theme",
      version = tip_engine_version(),
      src = "engine/www",
      package = "hovercraft",
      head = css,
      all_files = FALSE
    )
  )
}
