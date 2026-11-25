# tip_more(): the long form of a tip and when it shows. The engine knows two moments, "pin" and "hover"; the third,
# "button", is hovercraft's own (inst/more/more.js): the long form waits in attributes of its own, and a "More" button
# in the tip, or the M key, copies it onto the tag's engine attributes, which the engine draws again at once. Keeping
# the tip where it is when it grows or shrinks is the engine's job (0.58.0).

tip_more_ons <- function() c("button", "hover", "pin")

#' The long form of a tip, and when it shows
#'
#' `r lifecycle::badge("experimental")`
#' Wraps the words of a tip's long form with the moment it shows, to give as `more` to [tip_on()] or [tip_spec()]. With
#' no `tip_more()`, `more = "words"` is the same as `more = tip_more("words")`: the tip is short, and a "More" button
#' in it opens the long form.
#'
#' With `on = "button"` (the default) a "More" button at the bottom of the short tip opens the long form in the same
#' tip, and a "Less" button closes it again. It does not pin, and needs no `pin` and no `id`. The tip is short again
#' the next time it opens. A pinned tip gets the same buttons. The M key opens and closes the long form while the
#' keyboard focus is on the tagged item, except inside a text field, where a letter is a letter; a tag with a long
#' form announces the key as `aria-keyshortcuts`. The tip stays open and on its side when it grows or shrinks, grows
#' away from its item and scrolls when there is no more room, instead of flipping to the other side; that is the
#' engine's own behaviour (0.58.0).
#'
#' With `on = "hover"` the long form shows at once, with the short one. With `on = "pin"` it shows when the tip is
#' pinned, so the tip must be able to pin: `tip_on(pin = TRUE)` with an `id`, or a row of [tip_table()] with
#' `tip_spec(pin = TRUE)`; without a pin it is an error.
#'
#' A tip that expands sends no Shiny input. A pinned tip drawn again, such as one that opens its long form, sends
#' `input$<id>_pin` with `state = "updated"`; the values of `state` may grow in a later version, so an app should
#' not assume it knows them all.
#'
#' @param content The words of the long form (markdown, with `{{ field }}` placeholders filled from the tip's record
#'   or, in a table, from the row), or a [tip_template()]. `NA` and `""` mean no long form.
#' @param ... Must be empty. It makes a misspelt argument an error.
#' @param on When the long form shows: `"button"` (a "More" button opens it), `"hover"` (at once) or `"pin"` (when the
#'   tip is pinned). `NULL`, the default, is `"button"`. This is the same in a table.
#' @param label The words of the buttons, only with `"button"`: one string for the "More" button (the "Less" button
#'   keeps its word), or `c(more = "Show all", less = "Show less")`. `NULL` gives the built-in "More" and "Less". With
#'   `"hover"` or `"pin"` there is no button, so a `label` is an error.
#' @returns An object of class `hovercraft_more`, to give as `more` to [tip_on()] or [tip_spec()].
#' @family tooltips
#' @examples
#' tip_on(
#'   htmltools::tags$button("Run"),
#'   "Runs the checks.",
#'   more = tip_more("Takes about a minute on the full data.")
#' )
#'
#' # The long form shows with the short one, and the buttons say other words
#' tip_on(
#'   htmltools::tags$button("Run"),
#'   "Runs the checks.",
#'   more = tip_more("Takes about a minute.", on = "hover")
#' )
#' tip_on(
#'   htmltools::tags$button("Run"),
#'   "Runs the checks.",
#'   more = tip_more("Takes about a minute.", label = c(more = "Details", less = "Fewer details"))
#' )
#' @export
tip_more <- function(content, ..., on = NULL, label = NULL) {
  call <- rlang::current_env()
  if (missing(content)) {
    tip_abort(
      c(
        "{.arg content} is missing: a {.fn tip_more} needs the words of the long form.",
        i = "Give the words, a {.fn tip_template}, or {.code NA} for no long form."
      ),
      class = "hovercraft_error_argument",
      call = call
    )
  }
  tip_check_dots(..., args = c("content", "on", "label"))
  if (!tip_content_ok(content)) {
    tip_abort(
      c(
        "{.arg content} must be a string or a {.fn tip_template}, not {.obj_type_friendly {content}}.",
        i = "Give the words of the long form, a {.fn tip_template}, or {.code NA} for no long form."
      ),
      class = "hovercraft_error_argument",
      call = call
    )
  }
  tip_arg(
    checkmate::check_choice(on, tip_more_ons(), null.ok = TRUE),
    "on",
    paste0("Give one of ", paste0("\"", tip_more_ons(), "\"", collapse = ", "), ", or {.code NULL} for \"button\"."),
    call
  )
  label <- tip_check_more_label(label, on, call)
  if (!is.null(label) && tip_more_empty(content)) {
    tip_abort(
      c(
        "{.arg label} names the words of the buttons, but {.arg content} is empty, so there is no long form.",
        i = "Give the words of the long form, or take {.arg label} out."
      ),
      class = "hovercraft_error_argument",
      call = call
    )
  }
  structure(list(content = content, on = on, label = label), class = "hovercraft_more")
}

# Words that are NA or "" are no long form.
tip_more_empty <- function(content) {
  is.atomic(content) && length(content) == 1L && (is.na(content) || identical(content, ""))
}

# The words of the buttons as a named character vector (`more`, `less`), or NULL. One unnamed string is the "More" word.
tip_check_more_label <- function(label, on, call = rlang::caller_env()) {
  if (is.null(label)) {
    return(NULL)
  }
  fix <- "Give one string, or {.code c(more = \"...\", less = \"...\")}, or {.code NULL} for the built-in words."
  if (length(label) == 1L && is.null(names(label))) {
    names(label) <- "more"
  }
  ok <- checkmate::test_character(label, any.missing = FALSE, min.chars = 1L, min.len = 1L, max.len = 2L) &&
    !is.null(names(label)) &&
    all(names(label) %in% c("more", "less")) &&
    !anyDuplicated(names(label))
  tip_arg(
    if (ok) TRUE else "Must be a string, or a named character vector with the names \"more\" and \"less\".",
    "label",
    fix,
    call
  )
  if (!is.null(on) && !identical(on, "button")) {
    tip_abort(
      c(
        "{.arg label} names the words of the buttons, but {.code on = \"{on}\"} has no button.",
        i = "Take {.arg label} out, or use {.code on = \"button\"}."
      ),
      class = "hovercraft_error_argument",
      call = call
    )
  }
  label
}

# What `more` holds, whatever form it was given in: list(content, on, label). `on` is "button" unless `tip_more()` said.
tip_more_unpack <- function(more) {
  if (inherits(more, "hovercraft_more")) {
    return(list(content = more$content, on = more$on %||% "button", label = more$label))
  }
  list(content = more, on = "button", label = NULL)
}

# The attributes that hold the long form for the button, its words and its state, named after the engine's prefix.
tip_more_attribute <- function(key) paste0("data-", tip_names()$prefix, "-more-", key)

# The M key: used only while the focus is on the tagged item, never in a text field (WCAG 2.1.4).
tip_more_key <- function() "m"

# The attributes a tip carries for its long form. The engine's own (`more`, `more_template`, `more_on`) for "hover" and
# "pin"; for "button" the long form waits in attributes the script reads, and the script puts it on the engine's when
# the reader asks. `template` is the name of the long form's template, `text` its plain words.
tip_more_values <- function(on, template = NULL, text = NULL, label = NULL) {
  values <- list()
  if (identical(on, "button")) {
    if (!is.null(template)) {
      values[[tip_more_attribute("held-template")]] <- template
    } else {
      values[[tip_more_attribute("held")]] <- text
    }
    if (!is.null(label)) {
      values[[tip_more_attribute("label")]] <- tip_json(as.list(label))
    }
    return(values)
  }
  if (!is.null(template)) {
    values[[tip_attribute("more_template")]] <- template
  } else {
    values[[tip_attribute("more")]] <- text
  }
  values[[tip_attribute("more_on")]] <- on
  values
}

# `aria-keyshortcuts` for a tip: P to pin and M for the long form's button, as the item announces them.
tip_keyshortcuts <- function(pin, more_button) {
  keys <- c(if (pin) toupper(tip_pin_key()), if (more_button) toupper(tip_more_key()))
  if (length(keys)) paste(keys, collapse = " ")
}

tip_more_dependency <- function() {
  attrs <- tip_contract()$attributes
  config <- list(
    attributes = list(
      more = attrs$more,
      more_template = attrs$more_template,
      more_on = attrs$more_on,
      open = attrs$open,
      part = attrs$part,
      held = tip_more_attribute("held"),
      held_template = tip_more_attribute("held-template"),
      label = tip_more_attribute("label"),
      state = tip_more_attribute("state"),
      room = tip_more_attribute("room"),
      id = tip_attribute("id"),
      keysOff = tip_keys_off_attribute()
    ),
    events = list(hint = tip_contract()$events$hint$name, tip = tip_contract()$events$tip$name),
    css = tip_names()$css,
    key = tip_more_key(),
    labels = list(more = "More", less = "Less")
  )
  htmltools::htmlDependency(
    name = "hovercraft-more",
    version = tip_package_version(),
    src = "more",
    package = "hovercraft",
    script = "more.js",
    stylesheet = "more.css",
    meta = list("hovercraft-more-config" = tip_meta_value(config)),
    all_files = FALSE
  )
}
