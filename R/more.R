# tip_more(): the long form of a tip and when it shows. The engine knows two moments, "pin" and "hover"; the third,
# "button", is hovercraft's own (inst/more/more.js): the long form waits in attributes of its own, and a "More" button
# in the tip, the M key (or, with "click", a click on the item) copies it onto the tag's engine attributes, which the
# engine draws again at once. Keeping the tip where it is when it grows or shrinks is the engine's job (0.58.0).

tip_more_ons <- function() c("button", "hover", "pin", "click")
tip_more_looks <- function() c("button", "link")
tip_more_places <- function() c("end", "start")

#' The long form of a tip, and when it shows
#'
#' `r lifecycle::badge("experimental")`
#' Wraps the words of a tip's long form with the moment it shows, to give as `more` to [tip_on()] or [tip_spec()]. With
#' no `tip_more()`, `more = "words"` is the same as `more = tip_more("words")`: the tip is short, and a "More" button
#' in it opens the long form.
#'
#' With `on = "button"` (the default) a "More" button at the bottom of the short tip opens the long form in the same
#' tip, and a "Less" button closes it again. It does not pin (unless `pin = TRUE`, below), and needs no tip `pin` and
#' no `id`. The tip is short again the next time it opens. A pinned tip gets the same buttons. The M key opens and
#' closes the long form while the keyboard focus is on the tagged item or the pointer rests on it, except while the
#' focus is in a text field, where a letter is a letter. When the focus is on one item and the pointer rests on
#' another, the key acts on the item under the pointer, the tip the reader sees, not on one around it; a pinned tip
#' that holds the focus keeps the key. A tip that a tap or `tip_show()` opened does not take the key from the pointer.
#' It is a letter key that works from a hover, so a page switches it off for every reader with `use_hovercraft(keys =
#' c(more = FALSE))`; there is no switch for one reader. A tag with a long form announces the key as
#' `aria-keyshortcuts`. The tip stays open and on its side when it grows or shrinks, grows away from its item and
#' scrolls when there is no more room, instead of flipping to the other side; that is the engine's own behaviour
#' (0.58.0).
#'
#' `as` and `where` choose the look and the place of the control that opens the long form (only with `on = "button"`).
#' `as = "button"` (the default) is the small button in the bottom corner of the tip, at its end; `as = "link"` is an
#' italic, underlined text link at the bottom of the tip, at its start. `where` says the side, `"end"` or `"start"`,
#' and follows the reading direction, so the end is the right in English and the left in Arabic. Both are the same
#' control to a keyboard and a screen reader: the link is a real `<button>` styled as a link, which says "button" and
#' "expanded" or "collapsed" to a screen reader, takes Enter and Space, and does not navigate; an `<a>` would need
#' `role = "button"` and its own key handling to do the same. It is a tab stop in a pinned tip, `M` and
#' `aria-keyshortcuts` work as for the button, and the words are still the ones of `label` and `labels`.
#'
#' With `on = "click"` a click on the tagged item opens and closes the long form, and the tip has no button. The item
#' should take keyboard focus (`tabindex = "0"` on a `<span>` or `<div>`), because the M key is the keyboard route:
#' it does the same, and the item announces it as `aria-keyshortcuts`. The long form is announced to a screen reader
#' when it opens; the item gets no `aria-expanded` and no role, since a click on a plain tag is not a control. The
#' tip must be open: a click that opens the tip (a first tap on a touch screen) only opens it, short, and the next
#' click opens or closes the long form (on a touch screen the engine closes the tip on a second tap, so use the
#' default button there). The click never pins, so `pin = "click"` with it is an error. A control, or a tag that
#' holds one (a button, an input, a link, a label ...), is refused, because a click on it has its own job, and so is
#' a table: a click on a row selects it. Use the default button there.
#'
#' With `pin = TRUE` the "More" button, and the M key, also pin the tip, so the long form stays; "Less" then only
#' closes the long form and the tip stays pinned. It needs a tip that can pin, the same as `on = "pin"`. Today's
#' default, `pin = FALSE`, never pins: a pinned tip stays pinned when it opens its long form.
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
#' @param on When the long form shows: `"button"` (a "More" button opens it), `"hover"` (at once), `"pin"` (when the
#'   tip is pinned) or `"click"` (a click on the item opens it, with no button). `NULL`, the default, is `"button"`.
#'   `"click"` is not for a table, where a click selects a row.
#' @param label The words of the buttons, only with `"button"`: one string for the "More" button (the "Less" button
#'   keeps its word), or `c(more = "Show all", less = "Show less")`. `NULL` gives the built-in "More" and "Less". With
#'   `"hover"`, `"pin"` or `"click"` there is no button, so a `label` is an error. It beats the `more` and `less` words
#'   of `labels` (see [tip_on()] and [use_hovercraft()]) for this tip.
#' @param as How the control looks: `"button"` (a small button) or `"link"` (an italic text link). `NULL`, the default,
#'   is `"button"`. Only with `on = "button"`; with another moment there is no control, so it is an error.
#' @param where Which side of the bottom of the tip the control sits on: `"end"` or `"start"`, in the reading
#'   direction. `NULL`, the default, puts a button at the end and a link at the start. Only with `on = "button"`.
#' @param pin `TRUE` makes the "More" button (and the M key) pin the tip as well as open its long form, so the long form
#'   stays. `FALSE`, the default, never pins. Only with `"button"`, and only on a tip that can pin: [tip_on()] with
#'   `pin = TRUE` and an `id`, or a row of [tip_table()] with `tip_spec(pin = TRUE)`.
#' @returns An object of class `hovercraft_more`, to give as `more` to [tip_on()] or [tip_spec()].
#' @family tooltips
#' @examples
#' tip_on(
#'   htmltools::tags$button("Run"),
#'   "Runs the checks.",
#'   more = tip_more("Takes about a minute on the full data.")
#' )
#'
#' # A click on the item opens the long form, and the More button pins the tip
#' tip_on(
#'   htmltools::tags$span("Run", tabindex = "0"),
#'   "Runs the checks.",
#'   more = tip_more("Takes about a minute.", on = "click")
#' )
#' tip_on(
#'   htmltools::tags$button("Run", id = "run"),
#'   "Runs the checks.",
#'   id = "run_tip",
#'   pin = TRUE,
#'   more = tip_more("Takes about a minute.", pin = TRUE)
#' )
#'
#' # A text link at the start of the bottom of the tip, instead of the button
#' tip_on(
#'   htmltools::tags$button("Run"),
#'   "Runs the checks.",
#'   more = tip_more("Takes about a minute.", as = "link")
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
tip_more <- function(content, ..., on = NULL, label = NULL, pin = FALSE, as = NULL, where = NULL) {
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
  tip_check_dots(..., args = c("content", "on", "label", "pin", "as", "where"))
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
  pin <- tip_check_more_pin(pin, on, call)
  as <- tip_check_more_choice(as, "as", tip_more_looks(), on, call)
  where <- tip_check_more_choice(where, "where", tip_more_places(), on, call)
  given <- c(label = !is.null(label), pin = pin, as = !is.null(as), where = !is.null(where))
  if (any(given) && tip_more_empty(content)) {
    first <- names(given)[given][[1]]
    tip_abort(
      c(
        paste0(
          "{.arg ",
          first,
          "} sets how the long form opens, but {.arg content} is empty, so there is no long form."
        ),
        i = paste0("Give the words of the long form, or take {.arg ", first, "} out.")
      ),
      class = "hovercraft_error_argument",
      call = call
    )
  }
  structure(
    list(content = content, on = on, label = label, pin = pin, as = as, where = where),
    class = "hovercraft_more"
  )
}

# `as` and `where` shape the control, so only the button moment has them; NULL is the default.
tip_check_more_choice <- function(value, arg, choices, on, call = rlang::caller_env()) {
  tip_arg(
    checkmate::check_choice(value, choices, null.ok = TRUE),
    arg,
    paste0("Give one of ", paste0("\"", choices, "\"", collapse = ", "), ", or {.code NULL} for the default."),
    call
  )
  if (!is.null(value) && !is.null(on) && !identical(on, "button")) {
    tip_abort(
      c(
        "{.arg {arg}} shapes the control that opens the long form, but {.code on = \"{on}\"} has none.",
        i = "Take {.arg {arg}} out, or use {.code on = \"button\"}."
      ),
      class = "hovercraft_error_argument",
      call = call
    )
  }
  value
}

# `pin = TRUE` is for the button (or the M key) that opens the long form: with another moment there is none, or the tip
# is pinned already.
tip_check_more_pin <- function(pin, on, call = rlang::caller_env()) {
  tip_arg(
    checkmate::check_flag(pin),
    "pin",
    "Give {.code TRUE} to make the button pin the tip, or {.code FALSE}.",
    call
  )
  if (pin && !is.null(on) && !identical(on, "button")) {
    tip_abort(
      c(
        "{.arg pin} makes the button pin the tip, but {.code on = \"{on}\"} has no button.",
        i = "Take {.arg pin} out, or use {.code on = \"button\"}."
      ),
      class = "hovercraft_error_argument",
      call = call
    )
  }
  pin
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

# What `more` holds, whatever form it was given in: list(content, on, label, pin, as, where). `on` is "button" unless
# `tip_more()` said; `as` and `where` are NULL for the defaults.
tip_more_unpack <- function(more) {
  if (inherits(more, "hovercraft_more")) {
    return(list(
      content = more$content,
      on = more$on %||% "button",
      label = more$label,
      pin = isTRUE(more$pin),
      as = more$as,
      where = more$where
    ))
  }
  list(content = more, on = "button", label = NULL, pin = FALSE, as = NULL, where = NULL)
}

# A click on the item opens the long form: refused where the click has its own job, with the same test as
# `pin = "click"`: a control, or a tag that holds one.
tip_check_more_click <- function(tag, call = rlang::caller_env()) {
  found <- Filter(tip_is_control, c(list(tag), tip_tags_in(tag)))
  if (length(found)) {
    where <- if (tip_is_control(tag)) "is a control" else paste0("holds a control, a {.code <", found[[1]]$name, ">}")
    tip_abort(
      c(
        paste0(
          "{.code tip_more(on = \"click\")} cannot go on a tag that ",
          where,
          ": a click would open the long form, not use it."
        ),
        i = "Use the default {.code on = \"button\"}: the More button and the M key open it, and a click still works."
      ),
      class = "hovercraft_error_more",
      call = call
    )
  }
  invisible()
}

# The attributes that hold the long form for the button, its words and its state, named after the engine's prefix.
tip_more_attribute <- function(key) paste0("data-", tip_names()$prefix, "-more-", key)

# The M key: used while the focus is on the tagged item or the pointer rests on it, never in a text field. A page can
# switch it off with `use_hovercraft(keys = )`; a reader has no switch.
tip_more_key <- function() "m"

# The attributes a tip carries for its long form. The engine's own (`more`, `more_template`, `more_on`) for "hover" and
# "pin"; for "button" the long form waits in attributes the script reads, and the script puts it on the engine's when
# the reader asks (and for "click", where a click on the item asks). `template` is the name of the long form's
# template, `text` its plain words; `pin` makes the button pin the tip too; `as` and `where` are its look and side.
tip_more_values <- function(on, template = NULL, text = NULL, label = NULL, pin = FALSE, as = NULL, where = NULL) {
  values <- list()
  if (on %in% c("button", "click")) {
    if (!is.null(template)) {
      values[[tip_more_attribute("held-template")]] <- template
    } else {
      values[[tip_more_attribute("held")]] <- text
    }
    if (!is.null(label)) {
      values[[tip_more_attribute("label")]] <- tip_json(as.list(label))
    }
    if (pin) {
      values[[tip_more_attribute("pin")]] <- "true"
    }
    if (!is.null(as)) {
      values[[tip_more_attribute("as")]] <- as
    }
    if (!is.null(where)) {
      values[[tip_more_attribute("where")]] <- where
    }
    if (identical(on, "click")) {
      values[[tip_more_attribute("click")]] <- "true"
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
      pin_too = tip_more_attribute("pin"),
      click = tip_more_attribute("click"),
      as = tip_more_attribute("as"),
      where = tip_more_attribute("where"),
      state = tip_more_attribute("state"),
      room = tip_more_attribute("room"),
      id = tip_attribute("id"),
      keysOff = tip_keys_off_attribute()
    ),
    events = list(hint = tip_contract()$events$hint$name, tip = tip_contract()$events$tip$name),
    css = tip_names()$css,
    global = tip_names()$global,
    key = tip_more_key(),
    labels = as.list(tip_label_defaults()[c("more", "less")])
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
