# Markers. A marker is a visible cue on a tipped tag, so a reader can tell it holds a tip: a line under the words, an
# icon after them, or a small triangle in a corner. It is off by default and chosen per tag. R writes a data attribute
# (and, for words, a wrapper; for a custom icon, a hidden child) and a small stylesheet, in hovercraft's own layer on
# top of the tip engine, draws it. Nothing here edits the engine, and the stylesheet reads only the engine's public
# attributes.

tip_marker_types <- function() c("none", "underline", "icon", "corner")

tip_marker_lines <- function() c("dotted", "dashed", "wavy")

# The attributes the marker layer writes, named after the engine's own prefix, and the class of the two children it
# adds. inst/marker/marker.css spells the same names; test-marker.R holds the two to each other.
tip_marker_attribute <- function(key = NULL) {
  base <- paste0("data-", tip_names()$prefix, "-marker")
  if (is.null(key)) base else paste0(base, "-", key)
}

tip_marker_class <- function(key) paste0(sub("-tip$", "", tip_names()$css), "-marker-", key)

tip_marker_abort <- function(message, ..., class = NULL, call, .envir = parent.frame()) {
  tip_abort(message, class = c(class, "hovercraft_error_marker"), ..., call = call, .envir = .envir)
}

# Something an icon can be: a tag (`bsicons::bs_icon()`), a list of tags, or markup (`fontawesome::fa()` is an `html`
# string). Plain text is a word, which this is not.
tip_marker_is_icon <- function(x) {
  inherits(x, c("shiny.tag", "shiny.tag.list", "html"))
}

tip_marker_icon <- function(x, call) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!tip_marker_is_icon(x)) {
    tip_marker_abort(
      c(
        "{.arg icon} is {.obj_type_friendly {x}}, not a tag.",
        i = paste0(
          "Give an icon as a tag, such as {.code bsicons::bs_icon(\"info-circle\")} or ",
          "{.code fontawesome::fa(\"circle-info\")}."
        ),
        i = "A name such as {.val info} is not looked up: {.pkg hovercraft} does not pick an icon library for you."
      ),
      class = "hovercraft_error_marker_icon",
      call = call
    )
  }
  x
}

new_tip_marker <- function(type, line = NULL, icon = NULL) {
  structure(list(type = type, line = line, icon = icon), class = "hovercraft_marker")
}

#' A marker for a tipped tag
#'
#' `r lifecycle::badge("experimental")`
#' Describes the visible cue on a tipped tag, to give as `marker =` to [tip_on()], [tip_table()] or [tip_spec()]. For
#' the common cases `marker = "underline"`, `"icon"` or `"corner"` is enough; use `tip_marker()` to choose the line of
#' an underline or to give your own icon.
#'
#' @section The types:
#' * `"underline"` draws a line under the tag's words. It takes no room, so nothing moves. The line follows the tip
#'   preset in use (see [tip_style()]): dotted for `"none"`, `"sober"` and `"dense"`, dashed for `"modern"` and
#'   `"editorial"`, and a long gentle wave for `"lively"`. Choose one with `line`.
#' * `"icon"` adds a small icon after the words: a circle with an `i`, or the icon you give. It takes a little room.
#' * `"corner"` draws a small triangle in the top corner of the tag, as a spreadsheet does for a note. It takes no room
#'   and suits a table cell or a card; on a single word it can look like a stray mark, so prefer `"underline"` there.
#' * `"none"` draws nothing. It is what a tag has when `marker` is left out.
#'
#' A marker is drawn in the tag's own text colour, so it keeps its contrast in light and dark. The pointer over a marked
#' tag is the help cursor, unless the tip can be pinned (it is then a hand), or the tag is a button or a link, which
#' keep their own. A tag that takes more than words, such as the `div` Shiny puts around an input, is marked
#' as a whole: put the marker on the label tag, or on the words, when that is what you mean.
#'
#' @section While the tip is open:
#' A marked tag is highlighted while its tip shows, so a reader sees which marker the tip belongs to: a soft grey tint
#' on the words of an underline and on the tag of an icon or a corner (a table cell, a label), and a glow round a
#' [bslib::value_box()], which has a background of its own. It goes when the tip closes. There is no argument for it,
#' and a tag with no marker is not highlighted. It follows the tip engine's open mark on the tag, for a tip that shows
#' on hover or focus and for one that is pinned.
#'
#' @section Words that wrap:
#' For an underline, the words of a block tag (a `td`, a `p`, a `div` and so on) are put in a `span` inside the tag, so
#' that the line goes under the words and not across the whole cell or paragraph. In a [bslib::value_box()] the title
#' is marked. Tags inside the block that are not words are left as they are. A block that holds no words of its own,
#' such as a `div` of other tags, has nothing to underline and stops with an error: put the marker on the tag that holds
#' the words, or use `"corner"` or `"icon"`.
#'
#' @param type The kind of marker: `"underline"`, `"icon"`, `"corner"` or `"none"`. A table draws only `"corner"`.
#' @param ... Must be empty. It makes a misspelt argument an error.
#' @param line Only for `type = "underline"`: `"dotted"`, `"dashed"` or `"wavy"`, or `NULL` (the default) to follow the
#'   tip preset in use. `"wavy"` is a long gentle wave in the text's own colour, not the browser's short zigzag.
#' @param icon Only for `type = "icon"`: an icon as a tag, such as `fontawesome::fa()` or `bsicons::bs_icon()`, or
#'   `NULL` (the default) for the built-in one. It is added to the tag as a child that a screen reader skips.
#'
#' @returns A `hovercraft_marker` object, to give as `marker =` to [tip_on()], [tip_table()] or [tip_spec()]. Its
#'   fields are internal; pass the object on.
#' @family tooltips
#' @examples
#' tip_marker("underline")
#' tip_marker("underline", line = "wavy")
#' tip_marker("icon", icon = htmltools::tags$span("?"))
#'
#' tip_on(
#'   htmltools::tags$span("Baseline", tabindex = "0"),
#'   "The first **screening** value",
#'   marker = tip_marker("underline", line = "dashed")
#' )
#' @export
tip_marker <- function(type, ..., line = NULL, icon = NULL) {
  call <- rlang::current_env()
  tip_check_dots(..., args = c("type", "line", "icon"), class = "hovercraft_error_marker")
  if (missing(type)) {
    tip_marker_abort(
      c("{.arg type} is missing.", i = "Give one of {.val {tip_marker_types()}}."),
      class = "hovercraft_error_marker_type",
      call = call
    )
  }
  tip_marker_type(type, "type", call)
  if (!is.null(line)) {
    tip_marker_line(line, type, call)
  }
  if (!is.null(icon) && !identical(type, "icon")) {
    tip_marker_abort(
      c(
        "{.arg icon} is only for {.code type = \"icon\"}, not {.code type = {.val {type}}}.",
        i = "Use {.code tip_marker(\"icon\", icon = ...)} to give an icon."
      ),
      class = "hovercraft_error_marker_icon",
      call = call
    )
  }
  new_tip_marker(type, line, tip_marker_icon(icon, call))
}

tip_marker_type <- function(x, arg, call) {
  if (!(is.character(x) && length(x) == 1L && !is.na(x) && x %in% tip_marker_types())) {
    tip_marker_abort(
      c(
        "{.arg {arg}} is not a kind of marker: {.val {x}}.",
        i = paste0("Give one of ", paste0("{.val ", tip_marker_types(), "}", collapse = ", "), "."),
        if (is.character(x) && length(x) == 1L && !is.na(x)) trimws(tip_near(x, tip_marker_types()))
      ),
      class = "hovercraft_error_marker_type",
      call = call
    )
  }
  invisible(x)
}

tip_marker_line <- function(x, type, call) {
  if (!identical(type, "underline")) {
    tip_marker_abort(
      c(
        "{.arg line} is only for {.code type = \"underline\"}, not {.code type = {.val {type}}}.",
        i = "Use {.code tip_marker(\"underline\", line = ...)} to choose the line."
      ),
      class = "hovercraft_error_marker_line",
      call = call
    )
  }
  if (!(is.character(x) && length(x) == 1L && !is.na(x) && x %in% tip_marker_lines())) {
    tip_marker_abort(
      c(
        "{.arg line} is not a line: {.val {x}}.",
        i = paste0("Give one of ", paste0("{.val ", tip_marker_lines(), "}", collapse = ", "), ", or {.code NULL}."),
        if (is.character(x) && length(x) == 1L && !is.na(x)) trimws(tip_near(x, tip_marker_lines()))
      ),
      class = "hovercraft_error_marker_line",
      call = call
    )
  }
  invisible(x)
}

# Every accepted form of `marker =` as one object: NULL or "none" (no marker), a word, a bare tag (a custom icon) or a
# tip_marker(). Never a list: it is ambiguous once a marker has more parts.
tip_as_marker <- function(x, arg = "marker", call = rlang::caller_env()) {
  if (inherits(x, "hovercraft_marker")) {
    return(x)
  }
  if (is.null(x)) {
    return(new_tip_marker("none"))
  }
  if (tip_marker_is_icon(x)) {
    return(new_tip_marker("icon", icon = x))
  }
  if (is.character(x) && !inherits(x, "AsIs")) {
    tip_marker_type(x, arg, call)
    return(new_tip_marker(x))
  }
  tip_marker_abort(
    c(
      "{.arg {arg}} is not a marker: {.obj_type_friendly {x}}.",
      i = paste0(
        "It takes {.val none}, {.val underline}, {.val icon} or {.val corner}, a {.fn tip_marker}, ",
        "or a tag to use as the icon."
      )
    ),
    class = "hovercraft_error_marker_type",
    call = call
  )
}

#' @export
format.hovercraft_marker <- function(x, ...) {
  args <- c(
    encodeString(x$type, quote = "\""),
    if (!is.null(x$line)) paste0("line = ", encodeString(x$line, quote = "\"")),
    if (!is.null(x$icon)) "icon = <tag>"
  )
  paste0("tip_marker(", paste(args, collapse = ", "), ")")
}

#' @export
print.hovercraft_marker <- function(x, ...) {
  cli::cat_line(format(x))
  invisible(x)
}

# The tags that sit in a line of text. A marker on one of these is drawn on the tag itself; the words of any other tag
# are put in a span first, so the line goes under the words and not across the tag's whole box.
tip_marker_inline_tags <- function() {
  c(
    "a",
    "abbr",
    "b",
    "bdi",
    "bdo",
    "br",
    "cite",
    "code",
    "data",
    "dfn",
    "em",
    "i",
    "kbd",
    "mark",
    "q",
    "s",
    "samp",
    "small",
    "span",
    "strong",
    "sub",
    "sup",
    "time",
    "u",
    "var",
    "wbr"
  )
}

# Tags that hold no children, so a marker has nowhere to go.
tip_marker_void_tags <- function() {
  c("area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "source", "track", "wbr")
}

tip_marker_has_class <- function(tag, name) {
  classes <- htmltools::tagGetAttribute(tag, "class")
  !is.null(classes) && name %in% strsplit(paste(classes, collapse = " "), "[[:space:]]+")[[1L]]
}

# A child that is words or sits in a line of words: text, markup, or an inline tag.
tip_marker_is_inline_child <- function(x) {
  if (inherits(x, "shiny.tag")) {
    return(x$name %in% tip_marker_inline_tags() || x$name %in% c("svg", "img"))
  }
  is.character(x) || is.numeric(x)
}

# Children as one flat list: a list of children inside the list (what `tagList()` makes) is opened.
tip_marker_flat <- function(x) {
  if (inherits(x, "shiny.tag") || !is.list(x) || inherits(x, "html_dependency")) {
    return(list(x))
  }
  unlist(lapply(x, tip_marker_flat), recursive = FALSE)
}

# The children of `tag` with each run of words (and inline tags) put in one span, so a drawn underline has an inline box
# to sit under. A child that is a block tag stays where it is.
tip_marker_wrap_words <- function(tag) {
  span <- function(run) htmltools::tags$span(class = tip_marker_class("text"), run)
  out <- list()
  run <- list()
  for (child in tip_marker_flat(tag$children)) {
    if (tip_marker_is_inline_child(child)) {
      run <- c(run, list(child))
    } else {
      if (length(run)) {
        out <- c(out, list(span(run)))
      }
      run <- list()
      out <- c(out, list(child))
    }
  }
  if (length(run)) {
    out <- c(out, list(span(run)))
  }
  tag$children <- out
  tag
}

# Applies `fn` to the tag that holds a value box's title. The title is the one thing a value box marks (an underline
# goes under its words, an icon after them), so the marker never meets the showcase.
tip_marker_in_title <- function(x, fn) {
  if (inherits(x, "shiny.tag")) {
    if (tip_marker_has_class(x, "value-box-title")) {
      return(fn(x))
    }
    x$children <- lapply(x$children, tip_marker_in_title, fn)
  } else if (is.list(x) && !inherits(x, "html_dependency")) {
    x[] <- lapply(x, tip_marker_in_title, fn)
  }
  x
}

tip_marker_set <- function(tag, name, value) {
  do.call(htmltools::tagAppendAttributes, c(list(tag), stats::setNames(list(value), name)))
}

# Whether a tag holds a words span, at any depth: a value box keeps its in the title.
tip_marker_has_span <- function(x) {
  if (inherits(x, "shiny.tag")) {
    return(tip_marker_has_class(x, tip_marker_class("text")) || tip_marker_has_span(x$children))
  }
  is.list(x) && !inherits(x, "html_dependency") && any(vapply(x, tip_marker_has_span, NA))
}

# Takes a marker's earlier work off a tag: its attributes, its icon child, and the span round its words, so tip_on()
# called twice on one tag leaves one marker.
tip_marker_clear <- function(tag) {
  own <- startsWith(names(tag$attribs) %||% character(), tip_marker_attribute())
  tag$attribs[own] <- NULL
  unwrap <- function(x) {
    if (inherits(x, "html") && startsWith(as.character(x), paste0("&#8288;<span class=\"", tip_marker_class("icon")))) {
      return(list())
    }
    if (is.list(x) && !inherits(x, c("shiny.tag", "html_dependency"))) {
      return(list(unlist(lapply(x, unwrap), recursive = FALSE)))
    }
    if (!inherits(x, "shiny.tag")) {
      return(list(x))
    }
    if (tip_marker_has_class(x, tip_marker_class("text"))) {
      return(unlist(lapply(x$children, unwrap), recursive = FALSE))
    }
    x$children <- unlist(lapply(x$children, unwrap), recursive = FALSE)
    list(x)
  }
  tag$children <- unlist(lapply(tag$children, unwrap), recursive = FALSE)
  tag
}

tip_marker_dependency <- function() {
  htmltools::htmlDependency(
    name = "hovercraft-marker",
    version = as.character(getNamespaceVersion("hovercraft")),
    src = "marker",
    package = "hovercraft",
    stylesheet = "marker.css",
    all_files = FALSE
  )
}

# Puts a marker on a tag: the attributes, any child, and the dependency. A tag with no marker comes back as it was.
tip_marker_apply <- function(tag, marker, call) {
  tag <- tip_marker_clear(tag)
  if (identical(marker$type, "none")) {
    return(tag)
  }
  if (tag$name %in% tip_marker_void_tags()) {
    tip_marker_abort(
      c(
        "A {.code <{tag$name}>} tag holds nothing, so a marker has nowhere to go.",
        i = "Put the tip and the marker on the tag around it, such as its label."
      ),
      class = "hovercraft_error_marker_tag",
      call = call
    )
  }
  is_box <- tip_marker_has_class(tag, "bslib-value-box")
  tag <- tip_marker_set(tag, tip_marker_attribute(), marker$type)
  if (!is.null(marker$line)) {
    tag <- tip_marker_set(tag, tip_marker_attribute("line"), marker$line)
  }
  if (identical(marker$type, "underline")) {
    if (is_box) {
      tag <- tip_marker_in_title(tag, tip_marker_wrap_words)
    } else if (!tag$name %in% tip_marker_inline_tags()) {
      tag <- tip_marker_wrap_words(tag)
    }
    if (!tip_marker_has_span(tag) && (is_box || !tag$name %in% tip_marker_inline_tags())) {
      tip_marker_abort(
        c(
          "A {.code <{tag$name}>} tag holds no words of its own to underline: what is inside it is other tags.",
          i = "Put the marker on the tag that holds the words, such as its label, or use {.val corner} or {.val icon}."
        ),
        class = "hovercraft_error_marker_tag",
        call = call
      )
    }
    # The line is then drawn on the span, not on the tag.
    if (tip_marker_has_span(tag)) {
      tag <- tip_marker_set(tag, tip_marker_attribute("text"), "words")
    }
  }
  if (identical(marker$type, "icon") && !is.null(marker$icon)) {
    tag <- tip_marker_set(tag, tip_marker_attribute("icon"), "custom")
    # One string with a word joiner in front, so the icon stays on the line of the last word and no space is written
    # between them; the icon's own dependencies move to the tag.
    icon <- htmltools::tags$span(class = tip_marker_class("icon"), `aria-hidden` = "true", marker$icon)
    rendered <- htmltools::renderTags(icon)
    joined <- htmltools::HTML(paste0("&#8288;", rendered$html))
    tag <- if (is_box) {
      tip_marker_in_title(tag, function(title) htmltools::tagAppendChild(title, joined))
    } else {
      htmltools::tagAppendChild(tag, joined)
    }
    tag <- htmltools::attachDependencies(tag, rendered$dependencies, append = TRUE)
  }
  htmltools::attachDependencies(tag, tip_marker_dependency(), append = TRUE)
}
