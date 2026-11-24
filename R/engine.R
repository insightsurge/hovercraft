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

# The Shiny session that is running, or NULL outside one (and when Shiny is not installed).
tip_session <- function() {
  # nocov start: Shiny is installed wherever the package is tested; without it there is no session to find
  if (!requireNamespace("shiny", quietly = TRUE)) {
    return(NULL)
  }
  # nocov end
  shiny::getDefaultReactiveDomain()
}

# Where tip_define() keeps what it has seen, so that tip_on() can check a record against a template. Inside a Shiny
# session the templates are that session's own (in the session's `userData`), so two sessions of one app that define one
# name with different fields do not change each other's checks. Outside a session, at the page's UI and in tests, they
# are the process's, which every session also sees: the UI of an app is built once and serves all its sessions.
tip_session_templates <- function(create = FALSE, session = tip_session()) {
  if (is.null(session) || !is.environment(session$userData)) {
    return(NULL)
  }
  key <- "hovercraft_templates"
  if (is.null(session$userData[[key]]) && create) {
    session$userData[[key]] <- new.env(parent = emptyenv())
  }
  session$userData[[key]]
}

# The templates defined so far, by name: their fields. The session's own come over the process's.
tip_known <- function(session = tip_session()) {
  known <- if (is.null(tip_state$templates)) list() else tip_state$templates
  own <- tip_session_templates(session = session)
  if (is.null(own)) {
    return(known)
  }
  for (name in ls(own, all.names = TRUE)) {
    known[[name]] <- own[[name]]
  }
  known
}

# The templates that a definition in `session` can replace: the session's own, or, outside a session, the process's. One
# defined page-wide that a session defines again for itself is not a replacement in the session's scope.
tip_known_scope <- function(session = tip_session()) {
  own <- tip_session_templates(session = session)
  if (is.null(own)) {
    if (is.null(tip_state$templates)) list() else tip_state$templates
  } else {
    as.list(own, all.names = TRUE)
  }
}

# Keeps one template's fields and body in the registry of the running session, or the process's outside one.
tip_known_set <- function(name, value, session = tip_session()) {
  own <- tip_session_templates(create = TRUE, session = session)
  if (is.null(own)) {
    tip_state$templates[[name]] <- value
  } else {
    own[[name]] <- value
  }
  invisible()
}

# Forgets the templates: the process's, and those of the running session. For tests, and a session that starts a page
# again.
tip_reset <- function() {
  tip_state$templates <- NULL
  own <- tip_session_templates()
  if (!is.null(own)) {
    rm(list = ls(own, all.names = TRUE), envir = own)
  }
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
#' @returns A list of [htmltools::htmlDependency()] objects: the engine's script and style sheet, then, with a `style`
#'   that sets something, `hovercraft-style`, which holds the page's theme CSS and sets the page's preset and class as
#'   the engine's attributes on the page, and last `hovercraft-runtime`, which starts the engine and the layers. Put it
#'   once in the page's UI, for example `bslib::page_fluid(tip_dependency(style = "sober"), ...)`.
#'
#'   Nothing in them is inline script or style: they are files and data, so a page served with
#'   `Content-Security-Policy: script-src 'self'; style-src 'self'` works. A style sheet that is made from a theme is
#'   data too, which a script applies as a constructed style sheet; it comes after the page's own style sheets in the
#'   cascade. The scripts read their names and [tip_define()]'s templates from JSON, which a browser does not run, and
#'   set styles through the browser's style object, which a policy on inline markup does not cover. Every dependency is
#'   a folder of the installed package, so a cached document or a shared cache can use them in another R process.
#'
#'   What such a page still needs, because it is not hovercraft's markup:
#'   * a policy that has `default-src` or `img-src` must also allow `data:` images, or three small icons are not
#'     drawn: the lock before a reason (`when_disabled`), the pin button (`pin`) and the built-in icon of
#'     `marker = "icon"`. The words of the first two still show; the marker has no words, so an icon marker is then
#'     invisible (an icon of your own, given as a tag, is not affected);
#'   * a `style` attribute written in markup, such as the width that [shiny::actionButton()] and [shiny::plotOutput()]
#'     write, is blocked by `style-src 'self'` whoever writes it. `style-src-attr 'unsafe-inline'` allows them, and
#'     nothing else, or the page can use classes and a style sheet instead.
#'
#' @family tooltips
#' @examples
#' tip_dependency()
#' tip_dependency(style = "modern")
#' tip_dependency(style = tip_theme(light = list(bg = "#1b3a6b", ink = "#ffffff")))
#' tip_dependency(style = tip_style("sober", theme = list("--hovercraft-tip-radius" = "8px")))
#' @export
tip_dependency <- function(style = NULL) {
  style <- tip_as_style(style, "style")
  page <- if (!is.null(style)) tip_style_page(style)
  c(list(tip_engine_dependency()), if (!is.null(page)) list(page), list(tip_runtime_dependency()))
}

# The engine's own two files; it starts and is told about templates by the runtime dependency.
tip_engine_dependency <- function() {
  htmltools::htmlDependency(
    name = "hovercraft",
    version = tip_engine_version(),
    src = "engine/www",
    package = "hovercraft",
    script = "tip.js",
    stylesheet = "tip.css"
  )
}

# The engine and the runtime, which every tag that has a tip attaches. The runtime comes last: it starts the engine and
# reads the data the other dependencies carry.
tip_base_dependencies <- function() list(tip_engine_dependency(), tip_runtime_dependency())
