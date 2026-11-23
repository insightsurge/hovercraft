# Styles. A style is how the tip box looks: a preset (a named look the engine ships), a theme (colours, corner, font)
# and a class on the box. `style =` takes a preset name, a tip_style() or one part of a style, and one helper turns
# every accepted form into the one object. R writes what the engine already reads: its preset and class attributes,
# and CSS variables for a theme. Nothing here edits the engine.

tip_style_abort <- function(message, ..., class = NULL, call, .envir = parent.frame()) {
  tip_abort(message, class = c(class, "hovercraft_error_style"), ..., call = call, .envir = .envir)
}

# The preset names the engine ships, from its page contract, and the word that turns an inherited preset off.
tip_preset_names <- function() unlist(tip_contract()$presets)

tip_preset_none <- function() tip_contract()$presets_none

# A preset: NULL, or one name from tip_presets() or "none". A string that holds `:` or `;` is CSS, which `style` never
# takes (a user comes from `tags$span(style = )` or reactable's `colDef(style = )`), and says so.
tip_style_preset <- function(x, arg, call) {
  if (is.null(x)) {
    return(NULL)
  }
  tip_arg_string(x, arg, call = call)
  if (grepl("[:;]", x)) {
    tip_style_abort(
      c(
        "{.arg {arg}} is not inline CSS: {.val {x}}.",
        i = paste0(
          "It takes nothing ({.code NULL}), a preset name from {.fn tip_presets}, a {.fn tip_style}, ",
          "or one part of a style such as a {.fn tip_theme}."
        ),
        i = paste0(
          "Use {.fn tip_theme} for colours, the corner and the font, ",
          "and {.code tip_style(class = )} for your own CSS."
        )
      ),
      class = "hovercraft_error_style_css",
      call = call
    )
  }
  known <- c(tip_preset_names(), tip_preset_none())
  if (!x %in% known) {
    tip_style_abort(
      c(
        "{.arg {arg}} is {.val {x}}, which is not a preset.",
        i = paste0("The presets are ", paste0("{.val ", known, "}", collapse = ", "), ".", tip_near(x, known))
      ),
      call = call
    )
  }
  x
}

# A class for the tip box: NULL (inherit), "" (clear an inherited class) or class names, as a vector or as text with
# spaces. Each name is checked against the engine's pattern and its reserved prefix, so a bad name stops here and not
# as a console line in the browser.
tip_style_class <- function(x, arg, call) {
  if (is.null(x)) {
    return(NULL)
  }
  tip_arg(
    checkmate::check_character(x, any.missing = FALSE, min.len = 1L),
    arg,
    paste0(
      "Give class names as a character vector or as text with spaces, {.code \"\"} to clear an inherited class, ",
      "or {.code NULL}."
    ),
    call
  )
  tokens <- unique(unlist(strsplit(trimws(x), "[[:space:]]+")))
  tokens <- tokens[nzchar(tokens)]
  bad <- tokens[!grepl(tip_rules()$class$pattern, tokens, perl = TRUE)]
  if (length(bad)) {
    tip_style_abort(
      c(
        "{.arg {arg}} has {.val {bad}}, which {?is/are} not a class name.",
        i = "A class name has letters, digits, {.val -} and {.val _}, and does not start with a digit."
      ),
      call = call
    )
  }
  reserved <- tip_names()$css
  clash <- tokens[startsWith(tokens, reserved)]
  if (length(clash)) {
    tip_style_abort(
      c(
        "{.arg {arg}} has {.val {clash}}, which starts with {.val {reserved}}.",
        i = "The tip's own classes use that start; choose another name."
      ),
      call = call
    )
  }
  paste(tokens, collapse = " ")
}

new_tip_style <- function(preset = NULL, theme = NULL, class = NULL) {
  structure(list(preset = preset, theme = theme, class = class), class = "hovercraft_style")
}

# `near` over `far`, part by part: a spec's style and the table's, where a part set nearer wins and the rest comes from
# the table. Either may be NULL.
tip_style_over <- function(near, far) {
  if (is.null(near) || is.null(far)) {
    return(near %||% far)
  }
  new_tip_style(near$preset %||% far$preset, near$theme %||% far$theme, near$class %||% far$class)
}

#' How the tips look
#'
#' `r lifecycle::badge("experimental")`
#' Bundles what makes a tip look the way it does: a preset (a named look), a theme (colours, corner and font) and a
#' class on the tip box. Give the result as `style =` to [tip_on()] for one tag, or to [tip_dependency()] for the whole
#' page.
#' A part left out is left alone: it is inherited from the page, or from the page's own bslib variables.
#'
#' A value set nearer the tip wins: a tag's `style` beats the page's. A theme is merged role by role, so a tag that sets
#' only `bg` keeps the page's `ink`. A preset is replaced as a whole by the nearer one, and `"none"` clears an inherited
#' preset. A theme beats a preset set at the same place or further out, role by role, so
#' `tip_style("sober", theme = ...)` is sober with those roles changed; a preset set nearer the tip than a theme beats
#' that theme.
#'
#' @section The class:
#' `class` puts class names on the tip box, so your own CSS can restyle it. Give class names as a vector
#' (`c("wide", "audit-tip")`) or as text with spaces (`"wide audit-tip"`), `""` to clear a class the page or a parent
#' set, or `NULL` to inherit. A name has letters, digits, `-` and `_` and does not start with a digit, and must not
#' start with `hc-tip`, which the tip's own classes use; R stops on a bad name, where the browser would only drop it
#' with a line in its console. The nearest class wins as a whole and the classes are not added together: a tag's
#' `class = "wide"` replaces a page's `class = "brand"`.
#'
#' @section Themes on one tag:
#' A theme given to a tag sets the roles `bg`, `ink`, `line`, `code_bg`, `radius`, `shadow` and `font`. CSS variables
#' given as a list of names that start with `--` reach the tip only from the page, not from a tag, because a tip is
#' drawn away from its tag.
#'
#' @param preset A preset name from [tip_presets()], `"none"` to clear an inherited preset, or `NULL` (the default) to
#'   inherit. A string with `:` or `;` is not a preset: `style` is not CSS.
#' @param ... Must be empty. It makes a misspelt argument an error.
#' @param theme A [tip_theme()], a [bslib::bs_theme()], a named list of roles (`list(bg = "#1b3a6b")`), a named list of
#'   CSS variables, or `NULL` (the default).
#' @param class Class names for the tip box, `""` to clear an inherited class, or `NULL` (the default) to inherit. See
#'   the section on the class.
#'
#' @returns A `hovercraft_style` object, to give as `style =` to [tip_on()] or [tip_dependency()]. Its fields are
#'   internal; pass the object on.
#' @family tooltips
#' @examples
#' tip_style("sober")
#' tip_style("sober", theme = tip_theme(light = list(bg = "#1b3a6b", ink = "#ffffff")))
#' tip_style(class = "wide")
#' tip_style(class = "") # clears a class set above
#' tip_on(
#'   htmltools::tags$button("Run"),
#'   "Runs the **check**",
#'   style = tip_style("dense", class = "audit-tip")
#' )
#' @export
tip_style <- function(preset = NULL, ..., theme = NULL, class = NULL) {
  call <- rlang::current_env()
  tip_check_dots(..., args = c("preset", "theme", "class"), class = "hovercraft_error_style")
  new_tip_style(
    tip_style_preset(preset, "preset", call),
    tip_as_theme(theme, call),
    tip_style_class(class, "class", call)
  )
}

#' The presets
#'
#' `r lifecycle::badge("experimental")`
#' Lists the presets the tip engine ships, to give as `style =` to [tip_on()] and [tip_dependency()], or as the first
#' argument of [tip_style()]. The name of a preset is stable; its values may be tuned between releases.
#'
#' @returns A data frame with one row for each preset and the columns `name`, `label` (a person's name for it) and
#'   `about` (one sentence on what it is for).
#' @family tooltips
#' @examples
#' tip_presets()
#' @export
tip_presets <- function() {
  names <- tip_preset_names()
  read <- function(name) jsonlite::read_json(tip_path("presets", paste0(name, ".json")))
  files <- lapply(names, read)
  data.frame(
    name = names,
    label = vapply(files, function(file) file$label, ""),
    about = vapply(files, function(file) file$about, ""),
    stringsAsFactors = FALSE
  )
}

# Every accepted form of `style =` as one object: NULL, a preset name, a tip_style(), or one part of a style, read as
# tip_style(<part>) (in 0.1 a tip_theme() or a bslib theme). A bare list is not taken: it is ambiguous once a style has
# more parts than a theme.
tip_as_style <- function(x, arg = "style", call = rlang::caller_env()) {
  if (is.null(x) || inherits(x, "hovercraft_style")) {
    return(x)
  }
  if (inherits(x, c("hovercraft_theme", "bs_theme"))) {
    return(new_tip_style(theme = tip_as_theme(x, call)))
  }
  if (is.character(x) && !inherits(x, "AsIs")) {
    return(new_tip_style(preset = tip_style_preset(x, arg, call)))
  }
  hint <- if (is.list(x) && !is.object(x)) {
    "A list is not taken here: give {.fn tip_theme} the roles, or {.code tip_style(theme = list(...))}."
  } else {
    "It takes {.code NULL}, a preset name, a {.fn tip_style}, or one part of a style such as a {.fn tip_theme}."
  }
  tip_style_abort(c("{.arg {arg}} is not a style: {.obj_type_friendly {x}}.", i = hint), call = call)
}

# The tag marker a tag's theme is scoped to, and the hash of the theme that names it.
tip_theme_attribute <- function() paste0("data-", tip_names()$prefix, "-theme")

# A page's style: its theme as CSS on `body`, and its preset and class as the engine's own attributes on the page. The
# engine reads a preset from the attribute before its settings, and a script in the head does not depend on the engine
# having loaded. Its own dependency, not part of the engine's, so that the plain engine dependency every tag attaches
# can never replace it. NULL when the style sets nothing.
tip_style_page <- function(style) {
  css <- if (is.null(style$theme)) "" else tip_theme_css(style$theme, "body")
  set <- c(
    stats::setNames(list(style$preset), tip_attribute("preset")),
    stats::setNames(list(style$class), tip_attribute("class"))
  )
  set <- Filter(Negate(is.null), set)
  script <- if (length(set)) {
    sprintf(
      paste0(
        "<script>(function () { var set = %s; ",
        "for (var name in set) document.documentElement.setAttribute(name, set[name]); })();</script>"
      ),
      tip_json(set)
    )
  } else {
    ""
  }
  if (!nzchar(css) && !nzchar(script)) {
    return(NULL)
  }
  htmltools::htmlDependency(
    name = "hovercraft-style",
    version = tip_engine_version(),
    src = "engine/www",
    package = "hovercraft",
    head = paste0(css, script),
    all_files = FALSE
  )
}

# A tag's style: the attributes to write on the tag, and the dependencies they need. The preset and class are the
# engine's attributes; a theme is a marker on the tag, named by a hash of the theme, with its CSS in a dependency of
# that name, so two tags with one theme share it.
tip_style_tag <- function(style) {
  attribs <- list()
  deps <- list()
  if (is.null(style)) {
    return(list(attribs = attribs, deps = deps))
  }
  attribs[[tip_attribute("preset")]] <- style$preset
  attribs[[tip_attribute("class")]] <- style$class
  if (!is.null(style$theme)) {
    hash <- substr(rlang::hash(style$theme), 1L, 12L)
    marker <- tip_theme_attribute()
    css <- tip_theme_css(style$theme, sprintf("[%s=\"%s\"]", marker, hash))
    if (nzchar(css)) {
      attribs[[marker]] <- hash
      deps <- list(htmltools::htmlDependency(
        name = paste0("hovercraft-theme-", hash),
        version = tip_engine_version(),
        src = "engine/www",
        package = "hovercraft",
        head = css,
        all_files = FALSE
      ))
    }
  }
  list(attribs = attribs, deps = deps)
}

#' @export
format.hovercraft_style <- function(x, ...) {
  args <- c(
    if (!is.null(x$preset)) encodeString(x$preset, quote = "\""),
    if (!is.null(x$theme)) paste0("theme = ", format(x$theme)),
    if (!is.null(x$class)) paste0("class = ", encodeString(x$class, quote = "\""))
  )
  paste0("tip_style(", paste(args, collapse = ", "), ")")
}

#' @export
print.hovercraft_style <- function(x, ...) {
  cli::cat_line(format(x))
  invisible(x)
}
