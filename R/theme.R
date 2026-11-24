# Themes. A theme is a few roles (colours, a corner, a shadow, a typeface) for light and for dark, written as CSS
# variables in a <style> block. The engine reads its own variables, falling back to the page's bslib variables, so a
# role left out keeps whatever the page gives. Shapes follow mascot's theme API: a constructor with `light`, `dark` and
# `mode`, one function that turns every accepted form into the one object, and `format()`/`print()` that show a
# recreating call.

# role -> the engine's variable, after `--<prefix>-`.
tip_roles <- c(
  bg = "tip-bg",
  text = "tip-ink",
  border = "tip-line",
  code_bg = "tip-code-bg",
  radius = "tip-radius",
  shadow = "tip-shadow",
  font = "tip-font"
)
tip_colour_roles <- c("bg", "text", "border", "code_bg")

# What the engine draws by default on a bslib page, for a role the theme leaves out when a pair is measured: a shade of
# the page, Bootstrap's tertiary background with the page's own text colour, in the page's mode (a little darker than a
# light page, a little lighter than a dark one). The values are those of bslib's default Bootstrap 5 theme, read from
# the CSS that `bslib::bs_theme(version = 5)` compiles (bslib 0.12.0): `--bs-tertiary-bg` and `--bs-body-color`, in
# light and under `[data-bs-theme="dark"]`. A page without bslib gets the engine's own fallbacks, a little different but
# as readable.
tip_assumed <- list(
  light = c(bg = "#f8f8f8", text = "#1d1f21"),
  dark = c(bg = "#292d34", text = "#dee2e6")
)

# The page colour of each mode that a translucent fill is measured over: bslib's default `--bs-body-bg`.
tip_page <- list(light = "#ffffff", dark = "#1d1f21")

tip_theme_abort <- function(message, ..., call, .envir = parent.frame()) {
  tip_abort(message, class = "hovercraft_error_theme", ..., call = call, .envir = .envir)
}

# One role's value as CSS, or an error that says what the role takes.
tip_role_value <- function(value, role, where, call) {
  if (role %in% tip_colour_roles) {
    got <- tip_colour(value)
    fix <- "a colour name, hex, {.code rgb()} or {.code rgba()} text, three numbers from 0 to 255, or a CSS variable"
  } else if (role == "radius") {
    got <- tip_length(value)
    fix <- "a number of pixels, a length such as {.val 0.5rem}, or a CSS variable"
  } else if (tip_is_var(value)) {
    got <- tip_var(value)
    fix <- ""
  } else {
    got <- tip_css_text(value)
    fix <- if (role == "shadow") "CSS {.code box-shadow} text" else "CSS {.code font-family} text"
    fix <- paste0(fix, " (letters, digits, spaces and , . ' \" ( ) # % _ + * - only), or a CSS variable")
  }
  if (is.null(got)) {
    tip_theme_abort(
      c(
        "The {.field {role}} role in {.arg {where}} is not valid: {.val {paste(format(value), collapse = ' ')}}.",
        i = paste0("It takes ", fix, ".")
      ),
      call = call
    )
  }
  got
}

# A named list of roles as a named list of CSS values, in the order given.
tip_roles_clean <- function(x, where, call) {
  if (is.null(x)) {
    return(list())
  }
  renamed <- c(ink = "text", line = "border")
  old <- intersect(names(x), names(renamed))
  if (length(old)) {
    tip_theme_abort(
      c(
        "{.arg {where}} has {.val {old}}, which {?is no longer a role/are no longer roles}.",
        i = paste0("Use ", paste0("{.val ", renamed[old], "}", collapse = " and "), " instead.")
      ),
      call = call
    )
  }
  unknown <- setdiff(names(x), names(tip_roles))
  if (length(unknown)) {
    tip_theme_abort(
      c(
        "{.arg {where}} has {.val {unknown}}, which {?is/are} not a role.",
        i = paste0(
          "The roles are ",
          paste0("{.val ", names(tip_roles), "}", collapse = ", "),
          ".",
          tip_near(unknown[[1]], names(tip_roles))
        )
      ),
      call = call
    )
  }
  out <- lapply(names(x), function(role) tip_role_value(x[[role]], role, where, call))
  stats::setNames(out, names(x))
}

new_tip_theme <- function(light = list(), dark = list(), mode = "auto", tokens = list()) {
  structure(list(light = light, dark = dark, mode = mode, tokens = tokens), class = "hovercraft_theme")
}

#' A theme for the tips
#'
#' `r lifecycle::badge("experimental")`
#' Sets how the tips look: the fill, words, border and code background, the corner, the shadow and the typeface, for
#' light and for dark. A role left out keeps what the page gives: the tip reads Bootstrap's variables where the page has
#' them (bslib), so a page that already has a `bslib::bs_theme()` needs no theme, and its tips follow light and dark on
#' their own. Use a theme when the tips must look different from the page, or when the page has no bslib. Give the
#' result to [use_hovercraft()] once, in the page's UI, or to [tip_on()] for one tag, as `style =`.
#'
#' The roles are `bg` (the tip's fill), `text` (its words), `border` (its border, seen in forced-colours mode and in a
#' tip that stays open), `code_bg` (behind code), `radius` (the corner), `shadow` and `font`. Colours may be names, hex,
#' [grDevices::rgb()] output, `"rgba(0, 0, 0, 0.8)"` text or three numbers from 0 to 255. A value that is a CSS
#' variable, `"--team-blue"` or `"var(--team-blue)"`, is read by the browser where the page defines it; R cannot check
#' it. A theme can also be a [bslib::bs_theme()] or a plain list, see [tip_style()].
#'
#' A role set in `light` and not in `dark` is used in dark mode too, so a brand colour set once holds in both modes.
#'
#' The tip has a small arrow pointing at its tag. It takes the colour of the tip, so it is drawn only when the `bg` is
#' opaque: with a translucent `bg` (`"rgba(0, 0, 0, 0.8)"`) the tip has no arrow.
#'
#' @section Contrast warnings:
#' The words of a tip must be readable on its fill. For each mode, the `text` on the `bg` (and on the `code_bg`, if you
#' give one) is measured, and a ratio below 4.5:1, the usual level for text, gives a warning of class
#' `hovercraft_contrast`; the colours are kept. A role you leave out is taken to be what Bootstrap's default theme
#' gives, a shade of the page: Bootstrap's tertiary background with the page's text colour (a `#f8f8f8` fill with
#' `#1d1f21` words in light mode, `#292d34` with `#dee2e6` in dark mode). CSS variables
#' cannot be measured and are skipped. A translucent `bg` is measured over the page's colour (white, or `#1d1f21` in
#' dark mode), and a translucent `code_bg` over the `bg`. Set `options(hovercraft.contrast_warnings = FALSE)` to turn
#' the warnings off, or
#' silence only them:
#' ```r
#' withCallingHandlers(
#'   tip_theme(light = list(bg = "#ffffff", text = "#dddddd")),
#'   hovercraft_contrast = function(w) invokeRestart("muffleWarning")
#' )
#' ```
#'
#' @param ... Must be empty. It makes a misspelt argument an error.
#' @param light,dark Named lists of roles, or `NULL`. Roles: `bg`, `text`, `border`, `code_bg`, `radius`, `shadow`,
#'   `font`. `radius` is a number of pixels or a length such as `"0.5rem"`; `shadow` is CSS `box-shadow` text or
#'   `"none"`; `font` is CSS `font-family` text. Values may only hold letters, digits, spaces and
#'   `, . ' " ( ) # % _ + * -`.
#' @param mode `"auto"` (the default) follows the page: the `data-bs-theme` attribute that `input_dark_mode()` in
#'   bslib sets on the page, and with none, the visitor's own setting. `"light"` and `"dark"` use that mode whatever
#'   the page says. A tip is drawn at the page level, so a `data-bs-theme` on a smaller part of the page does not
#'   reach it.
#'
#' @returns A `hovercraft_theme` object, to give as `style =` to [use_hovercraft()] or [tip_on()], or to
#'   [tip_style()]. Its fields are internal; pass the object on.
#' @family tooltips
#' @examples
#' tip_theme()
#' tip_theme(light = list(bg = "#1b3a6b", text = "#ffffff", radius = 8))
#' tip_theme(
#'   light = list(bg = "#1b3a6b", text = "#ffffff"),
#'   dark = list(bg = "#9ec5fe", text = "#0b1f3a"),
#'   mode = "auto"
#' )
#' tip_theme(light = list(bg = "--brand-blue", font = "Georgia, serif"))
#' @export
tip_theme <- function(..., light = NULL, dark = NULL, mode = "auto") {
  call <- rlang::current_env()
  tip_check_dots(..., args = c("light", "dark", "mode"), class = "hovercraft_error_theme")
  tip_arg_list(light, "light", null_ok = TRUE, fix = "Give a named list of roles, or {.code NULL}.")
  tip_arg_list(dark, "dark", null_ok = TRUE, fix = "Give a named list of roles, or {.code NULL}.")
  tip_arg_string(mode, "mode")
  if (!mode %in% c("auto", "light", "dark")) {
    tip_theme_abort(
      c("{.arg mode} is {.val {mode}}.", i = "It is {.val auto}, {.val light} or {.val dark}."),
      call = call
    )
  }
  theme <- new_tip_theme(tip_roles_clean(light, "light", call), tip_roles_clean(dark, "dark", call), mode)
  tip_theme_contrast(theme, call)
  theme
}

# A role's value in a mode: dark falls back to light, role by role.
tip_theme_roles <- function(theme, mode) {
  out <- theme$light
  if (mode == "dark") {
    out[names(theme$dark)] <- theme$dark
  }
  out
}

# Warns when the words of a tip, in a mode the theme uses, are below 4.5:1 on their fill (or on a code background). One
# warning for each pair of colours, naming every mode it holds in.
tip_theme_contrast <- function(theme, call) {
  option <- getOption("hovercraft.contrast_warnings", TRUE)
  if (!isTRUE(option) && !isFALSE(option)) {
    tip_warn(
      "{.code options(hovercraft.contrast_warnings)} must be {.code TRUE} or {.code FALSE}; using {.code TRUE}.",
      class = "hovercraft_warning_option",
      .frequency = "once",
      .frequency_id = "hovercraft-contrast-option"
    )
  } else if (isFALSE(option)) {
    return(invisible())
  }
  found <- list()
  for (mode in if (theme$mode == "auto") c("light", "dark") else theme$mode) {
    roles <- tip_theme_roles(theme, mode)
    if (!length(intersect(names(roles), c("bg", "text", "code_bg")))) {
      next
    }
    ink <- roles$text %||% tip_assumed[[mode]][["text"]]
    # A translucent fill shows over the page of its mode, and a translucent code background over the fill.
    bg <- roles$bg %||% tip_assumed[[mode]][["bg"]]
    if (!tip_is_var(bg)) {
      bg <- tip_composite(bg, tip_page[[mode]])
    }
    for (back in c("bg", "code_bg")) {
      fill <- if (back == "bg") bg else roles$code_bg
      if (!is.null(fill) && back == "code_bg" && !tip_is_var(fill) && !tip_is_var(bg)) {
        fill <- tip_composite(fill, bg)
      }
      if (is.null(fill) || tip_is_var(fill) || tip_is_var(ink)) {
        next
      }
      key <- paste(back, ink, fill)
      found[[key]] <- list(back = back, ink = ink, fill = fill, modes = c(found[[key]]$modes, mode))
    }
  }
  for (pair in found) {
    ratio <- tip_contrast(pair$ink, pair$fill)
    if (ratio < 4.5) {
      said <- sprintf(
        "In %s mode the tip's {.field text} {.code %s} on its {.field %s} {.code %s} has contrast %s:1.",
        paste(pair$modes, collapse = " and "),
        pair$ink,
        pair$back,
        pair$fill,
        format(round(ratio, 2), nsmall = 2)
      )
      tip_warn(
        c(said, i = "Text needs 4.5:1. The colours are kept; change one of them."),
        class = "hovercraft_contrast",
        call = call
      )
    }
  }
  invisible()
}

# The theme as CSS text, for the tags under `scope`: the light roles on `scope`, and the dark roles that differ
# under data-bs-theme="dark" (and under the visitor's own dark setting when the page names no theme). A page's theme is
# scoped to `body`, never `:root`: the engine takes a variable it finds on the page's root as inherited, so a page
# preset would overwrite a theme written there. A tag's theme is scoped to the tag's own marker.
tip_theme_css <- function(theme, scope = "body") {
  prefix <- tip_names()$prefix
  declare <- function(roles, tokens = list()) {
    own <- vapply(names(roles), function(role) paste0("--", prefix, "-", tip_roles[[role]]), "")
    lines <- c(
      if (length(roles)) paste0("  ", own, ": ", unlist(roles), ";"),
      if (length(tokens)) paste0("  ", names(tokens), ": ", unlist(tokens), ";")
    )
    paste(lines, collapse = "\n")
  }
  block <- function(selector, body) if (nzchar(body)) paste0(selector, " {\n", body, "\n}\n") else ""
  light <- tip_theme_roles(theme, "light")
  dark <- tip_theme_roles(theme, "dark")
  differs <- names(dark)[!vapply(names(dark), function(role) identical(dark[[role]], light[[role]]), NA)]
  # The engine reads the page's mode from the root or the body, so the dark rule matches both.
  dark_scope <- paste0("[data-bs-theme=\"dark\"] ", scope)
  if (identical(scope, "body")) {
    dark_scope <- paste0(dark_scope, ", body[data-bs-theme=\"dark\"]")
  }
  body <- switch(
    theme$mode,
    light = block(scope, declare(light, theme$tokens)),
    dark = block(scope, declare(dark, theme$tokens)),
    paste0(
      block(scope, declare(light, theme$tokens)),
      block(dark_scope, declare(dark[differs])),
      if (length(differs)) {
        paste0(
          "@media (prefers-color-scheme: dark) {\n",
          block(paste0(":root:not([data-bs-theme]) ", scope), declare(dark[differs])),
          "}\n"
        )
      }
    )
  )
  body
}

# A font that is a bslib variable (a Bootswatch theme's `var(--bs-font-sans-serif)`) gets a fallback: a page with no
# bslib does not define the variable, and the browser would draw its default serif font.
tip_font_fallback <- function(x) {
  if (is.null(x) || !tip_is_var(x)) {
    return(x)
  }
  sub("[[:space:]]*\\)$", ", system-ui, sans-serif)", tip_var(x))
}

# Whether bslib is installed and new enough to read a theme's variables.
tip_has_bslib <- function() {
  requireNamespace("bslib", quietly = TRUE) && utils::packageVersion("bslib") >= "0.6.0"
}

# `share` of the colour `ink` laid over the colour `page`, as #rrggbb, or NULL when either is not a colour R can read
# (a CSS variable, say). The engine's own fallback for a page with no tertiary background is the same: 8% ink on the
# page.
tip_page_shade <- function(ink, page, share = 0.08) {
  if (is.null(ink) || is.null(page) || tip_is_var(ink) || tip_is_var(page)) {
    return(NULL)
  }
  front <- tip_channels(ink)[1:3]
  tip_composite(paste0("rgba(", paste(front, collapse = ", "), ", ", share, ")"), page)
}

# The colours, corner and font of a bslib theme, as roles. The tip is a shade of the page, as the engine's default is:
# its fill is the theme's tertiary background (`body-tertiary-bg`; a mix of 8% of the text colour over the page's
# background when the theme has none) and its words the theme's text colour, in each mode.
tip_theme_from_bslib <- function(x, call) {
  if (!tip_has_bslib()) {
    tip_theme_abort(
      c("A {.fn bslib::bs_theme} needs bslib 0.6.0 or later.", i = "Install it, or give {.fn tip_theme} the roles."),
      call = call
    )
  }
  names <- c(
    "body-bg",
    "body-color",
    "body-tertiary-bg",
    "border-color",
    "border-radius",
    "font-family-base",
    "body-bg-dark",
    "body-color-dark",
    "body-tertiary-bg-dark",
    "border-color-dark"
  )
  vars <- as.list(bslib::bs_get_variables(x, names))
  vars <- lapply(vars, function(v) if (is.null(v) || is.na(v)) NULL else v)
  read <- function(role, value) {
    if (is.null(value)) {
      return(NULL)
    }
    got <- tryCatch(
      tip_role_value(value, role, "theme", call),
      hovercraft_error_theme = function(e) NULL
    )
    if (is.null(got)) {
      tip_warn(
        c(
          "The bslib value {.val {value}} for the {.field {role}} role could not be read.",
          i = "That role keeps what the page gives."
        ),
        class = "hovercraft_warning_theme",
        call = call
      )
    }
    got
  }
  keep <- function(roles) roles[!vapply(roles, is.null, NA)]
  # The fill: the tertiary background, else a shade of the page made from the words and the page's background.
  shade <- function(fill, ink, page) {
    read("bg", vars[[fill]]) %||% tip_page_shade(ink, read("bg", vars[[page]]))
  }
  light_ink <- read("text", vars[["body-color"]])
  dark_ink <- read("text", vars[["body-color-dark"]])
  light <- keep(list(
    bg = shade("body-tertiary-bg", light_ink, "body-bg"),
    text = light_ink,
    border = read("border", vars[["border-color"]]),
    radius = read("radius", vars[["border-radius"]]),
    font = read("font", tip_font_fallback(vars[["font-family-base"]]))
  ))
  dark <- keep(list(
    bg = shade("body-tertiary-bg-dark", dark_ink, "body-bg-dark"),
    text = dark_ink,
    border = read("border", vars[["border-color-dark"]])
  ))
  theme <- new_tip_theme(light, dark, "auto")
  tip_theme_contrast(theme, call)
  theme
}

# Every accepted form of a theme as one object: NULL (the page's own variables), a tip_theme(), a bslib theme, or a
# named list (roles for both modes, or CSS variables set as given).
tip_as_theme <- function(x, call = rlang::caller_env()) {
  if (is.null(x) || inherits(x, "hovercraft_theme")) {
    return(x)
  }
  if (inherits(x, "bs_theme")) {
    return(tip_theme_from_bslib(x, call))
  }
  if (!is.list(x) || (length(x) && (is.null(names(x)) || anyNA(names(x)) || anyDuplicated(names(x))))) {
    tip_theme_abort(
      c(
        "{.arg theme} is not a theme.",
        i = "Give {.fn tip_theme}, a {.fn bslib::bs_theme}, or a named list of roles or CSS variables."
      ),
      call = call
    )
  }
  if (!length(x)) {
    return(new_tip_theme())
  }
  tokens <- startsWith(names(x), "--")
  if (any(tokens) && !all(tokens)) {
    tip_theme_abort(
      c(
        "{.arg theme} mixes roles and CSS variables: {.val {names(x)}}.",
        i = "Use roles (such as {.val bg}) or CSS variables (names that start with {.val --}), not both."
      ),
      call = call
    )
  }
  if (all(tokens)) {
    return(tip_theme_tokens(x, call))
  }
  roles <- tip_roles_clean(x, "theme", call)
  theme <- new_tip_theme(roles)
  tip_theme_contrast(theme, call)
  theme
}

# CSS variables set as given, for both modes.
tip_theme_tokens <- function(x, call) {
  bad <- names(x)[!grepl("^--[A-Za-z][A-Za-z0-9_-]*$", names(x))]
  if (length(bad)) {
    tip_theme_abort("{.val {bad}} {?is/are} not a CSS variable name.", call = call)
  }
  values <- lapply(names(x), function(name) {
    value <- x[[name]]
    got <- if (tip_is_var(value)) tip_var(value) else tip_colour(value) %||% tip_length(value) %||% tip_css_text(value)
    if (is.null(got)) {
      tip_theme_abort(
        c(
          "The value of {.val {name}} is not valid: {.val {paste(format(value), collapse = ' ')}}.",
          i = paste0(
            "Give a colour, a length or other short CSS text ",
            "(letters, digits, spaces and , . ' \" ( ) # % _ + * - only)."
          )
        ),
        call = call
      )
    }
    got
  })
  new_tip_theme(tokens = stats::setNames(values, names(x)))
}

#' @export
format.hovercraft_theme <- function(x, ...) {
  if (length(x$tokens)) {
    return(paste0(
      "<tip theme, CSS variables: ",
      paste0(names(x$tokens), " = ", unlist(x$tokens), collapse = ", "),
      ">"
    ))
  }
  args <- list()
  for (field in c("light", "dark")) {
    if (length(x[[field]])) {
      args[[field]] <- as.call(c(list(quote(list)), x[[field]]))
    }
  }
  if (!identical(x$mode, "auto")) {
    args$mode <- x$mode
  }
  paste(trimws(deparse(as.call(c(list(as.name("tip_theme")), args)), width.cutoff = 80L)), collapse = " ")
}

#' @export
print.hovercraft_theme <- function(x, ...) {
  cli::cat_line(format(x))
  invisible(x)
}
