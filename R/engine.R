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

# The engine's page contract, tip.json: its preset names, the class pattern and the attribute names.
tip_contract <- function() {
  if (is.null(tip_state$contract)) {
    tip_state$contract <- jsonlite::read_json(tip_path("tip.json"), simplifyVector = FALSE)
  }
  tip_state$contract
}

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
#' you build the page's head yourself, or to give the page a look. The dependency is versioned with the engine, not
#' with this package. The engine reads Bootstrap's colours where the page has them (bslib), and plain colours otherwise.
#'
#' @param style How the tips look on the whole page, or `NULL` (the default) to take the page's own bslib variables,
#'   so a page with a `bslib::bs_theme()` follows it, light and dark, with nothing more. One of:
#'   * a preset name from [tip_presets()], such as `"sober"`;
#'   * a [tip_style()], which bundles a preset, a theme, a class for the tip box, and its motion and size;
#'   * one part of a style, read as `tip_style(<part>)`: a [tip_theme()], or a [bslib::bs_theme()], read through its
#'     variables (bslib 0.6.0 or later). The tip is a shade of the page: its fill is the theme's tertiary background
#'     and its words the theme's text colour, with the border colour, corner and font.
#'
#'   A tag's own `style` (see [tip_on()]) beats the page's, and a value set nearer wins, value by value. A named
#'   list is not taken: give `tip_theme()` the roles, or `tip_style(theme = list(...))` a list of roles or of CSS
#'   variables such as `list("--hovercraft-tip-bg" = "#1b3a6b")`. A string that holds `:` or `;` is not a style: it
#'   looks like CSS, and `style` takes no CSS.
#'
#' @returns An [htmltools::htmlDependency()]; with a `style` that sets something, a list of two: the engine and
#'   `hovercraft-style`, which holds the page's theme CSS and sets the page's preset and class as the engine's
#'   attributes on the page. Put it once in the page's UI, for example
#'   `bslib::page_fluid(tip_dependency(style = "sober"), ...)`.
#' @family tooltips
#' @examples
#' tip_dependency()
#' tip_dependency(style = "modern")
#' tip_dependency(style = tip_theme(light = list(bg = "#1b3a6b", ink = "#ffffff")))
#' tip_dependency(style = tip_style("sober", theme = list("--hovercraft-tip-radius" = "8px")))
#' @export
tip_dependency <- function(style = NULL) {
  engine <- htmltools::htmlDependency(
    name = "hovercraft",
    version = tip_engine_version(),
    src = "engine/www",
    package = "hovercraft",
    script = "tip.js",
    stylesheet = "tip.css",
    head = paste0(sprintf("<script>%s.attach(document);</script>", tip_names()$global), tip_status_css())
  )
  style <- tip_as_style(style, "style")
  page <- if (!is.null(style)) tip_style_page(style)
  if (is.null(page)) engine else list(engine, page)
}
