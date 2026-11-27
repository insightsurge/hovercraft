# tip_on(pin = ): a tip that a reader can keep open. The engine pins a tagged item (its `pin` attribute) and draws the
# pinned tip with its own close button; hovercraft adds what the engine leaves to its host (inst/pin/pin.js): a small
# button in the corner of the hint, and a pin key. The engine's pin value `manual` is an item that pins only when told,
# so a click on it, a key or a tap on it never pins, and a table row can be selected without a pin; the corner button
# and the pin key tell it. A pin `"click"` is the engine's classic pin, where a click on the item pins it too.

# The attribute value for the argument, or NULL for no pin: TRUE is the engine's `manual`; "click" is the classic pin,
# which the engine takes for any other value.
tip_check_pin <- function(pin, call = rlang::caller_env()) {
  if (is.null(pin)) {
    return(NULL)
  }
  ok <- checkmate::test_flag(pin) || checkmate::test_string(pin, pattern = "^click$")
  tip_arg(
    if (ok) TRUE else "Must be `NULL`, `FALSE`, `TRUE` or \"click\".",
    "pin",
    "Give {.code FALSE} for no pin, {.code TRUE} to pin by the corner button and the pin key, or {.code \"click\"}.",
    call
  )
  if (isFALSE(pin)) {
    NULL
  } else if (isTRUE(pin)) {
    "manual"
  } else {
    "true"
  }
}

# A pin needs an id: it is the Shiny id of the pin and copy inputs and the id the engine names the pinned tip by.
tip_check_pin_id <- function(effective_id, call = rlang::caller_env()) {
  if (is.null(effective_id)) {
    tip_abort(
      c(
        "{.arg pin} needs an id, and the tag has none.",
        i = "Give {.fn tip_on} an {.arg id}, or the tag an {.code id} of its own."
      ),
      class = "hovercraft_error_pin",
      call = call
    )
  }
  invisible()
}

# The tags the engine does not pin, because their own click does something else (tip.json, `refused`): a link, a submit
# or reset button, a field, a summary, a label, an editable tag. The engine would say so on the console and show a hint;
# a message at build time says it first.
tip_pin_refused <- function(tag) {
  type <- tolower(as.character(htmltools::tagGetAttribute(tag, "type") %||% ""))
  editable <- htmltools::tagGetAttribute(tag, "contenteditable")
  switch(
    tag$name,
    a = !is.null(htmltools::tagGetAttribute(tag, "href")),
    button = type %in% c("submit", "reset"),
    input = ,
    select = ,
    textarea = ,
    summary = ,
    label = TRUE,
    !is.null(editable) && !identical(tolower(as.character(editable)), "false")
  )
}

tip_check_pin_tag <- function(tag, call = rlang::caller_env()) {
  if (tip_pin_refused(tag)) {
    tip_abort(
      c(
        "{.arg pin} is given, but a {.code <{tag$name}>} tag cannot pin: its own click does something else.",
        i = "Put the tip on a button that does not submit a form, or on a {.code <div>} or {.code <span>} around it."
      ),
      class = "hovercraft_error_pin",
      call = call
    )
  }
  invisible()
}

# `pin = "click"` makes a click on the item pin it, which would take a control's own click. A control, or a tag that
# holds one, takes `pin = TRUE` instead, and the tip's pin button and key do the pinning.
# The roles the engine treats as a control that acts (its own list), so a tag with one of them takes `pin = TRUE` and
# the default `tip_more()`, whatever the tag is.
tip_control_roles <- function() {
  c(
    "button",
    "tab",
    "link",
    "menuitem",
    "menuitemcheckbox",
    "menuitemradio",
    "option",
    "treeitem",
    "checkbox",
    "radio",
    "switch"
  )
}

tip_is_control <- function(tag) {
  role <- tolower(as.character(htmltools::tagGetAttribute(tag, "role") %||% ""))
  if (any(strsplit(trimws(role), "[[:space:]]+")[[1]] %in% tip_control_roles())) {
    return(TRUE)
  }
  type <- tolower(as.character(htmltools::tagGetAttribute(tag, "type") %||% ""))
  editable <- htmltools::tagGetAttribute(tag, "contenteditable")
  switch(
    tag$name,
    button = ,
    select = ,
    textarea = ,
    summary = ,
    label = TRUE,
    input = !identical(type, "hidden"),
    a = !is.null(htmltools::tagGetAttribute(tag, "href")),
    !is.null(editable) && !identical(tolower(as.character(editable)), "false")
  )
}

tip_check_pin_click <- function(tag, call = rlang::caller_env()) {
  if (tip_pin_refused(tag)) {
    tip_abort(
      c(
        "{.code pin = \"click\"} is given, but a {.code <{tag$name}>} tag cannot pin: its click does something else.",
        i = "Put the tip on a {.code <div>} or {.code <span>} around it, and give that {.code pin = TRUE}."
      ),
      class = "hovercraft_error_pin",
      call = call
    )
  }
  found <- Filter(tip_is_control, tip_tags_in(tag))
  if (length(found)) {
    where <- if (tip_is_control(tag)) "is a control" else paste0("holds a control, a {.code <", found[[1]]$name, ">}")
    tip_abort(
      c(
        paste0("{.code pin = \"click\"} cannot go on a tag that ", where, ": a click would pin it, not use it."),
        i = "Use {.code pin = TRUE}: the pin button in the tip's corner and the pin key pin it; a click still works."
      ),
      class = "hovercraft_error_pin",
      call = call
    )
  }
  invisible()
}

# The pin key: one letter, used while the focus is on the tagged item or the pointer rests on it, never in a field. A
# page can switch it off with `use_hovercraft(keys = )`; a reader has no switch. The tagged item announces it
# (`aria-keyshortcuts`) where a keyboard and a screen reader meet the item; the layer listens for it.
tip_pin_key <- function() "p"

tip_pin_dependency <- function() {
  config <- list(
    attributes = list(
      pin = tip_attribute("pin"),
      open = tip_contract()$attributes$open,
      part = tip_contract()$attributes$part,
      id = tip_attribute("id"),
      keysOff = tip_keys_off_attribute()
    ),
    events = list(hint = tip_contract()$events$hint$name),
    global = tip_names()$global,
    css = tip_names()$css,
    key = tip_pin_key(),
    labels = list(pin = tip_label_defaults()[["pin"]])
  )
  # The names are data in a <meta>, which inst/pin/pin.js reads when it loads; no inline script starts it.
  htmltools::htmlDependency(
    name = "hovercraft-pin",
    version = as.character(getNamespaceVersion("hovercraft")),
    src = "pin",
    package = "hovercraft",
    script = "pin.js",
    stylesheet = "pin.css",
    meta = list("hovercraft-pin-config" = tip_meta_value(config)),
    all_files = FALSE
  )
}

# How many pinned tips the app keeps: one whole number from 1 to 10, as an integer.
tip_check_max_pins <- function(x, arg = "max_pins", call = rlang::caller_env()) {
  ok <- checkmate::test_int(x, lower = 1, upper = 10, tol = 0)
  tip_arg(
    if (ok) TRUE else "Must be one whole number from 1 to 10.",
    arg,
    "Give how many pinned tips stay open at once, from 1 (the default) to 10, the engine's cap.",
    call
  )
  as.integer(x)
}

# The letter keys a tip has: the names, in order, of those switched off. TRUE is none off, FALSE all, and a named
# logical vector one switch each; a key left out stays on.
tip_keys <- function() c("pin", "more")

# The attribute on the page's root that lists the keys switched off, named after the engine's prefix like the others.
tip_keys_off_attribute <- function() paste0("data-", tip_names()$prefix, "-keys-off")

tip_check_keys <- function(keys, call = rlang::caller_env()) {
  if (checkmate::test_flag(keys) && is.null(names(keys))) {
    return(if (isTRUE(keys)) character() else tip_keys())
  }
  named <- is.logical(keys) && length(keys) > 0L && !anyNA(keys) && !is.null(names(keys)) && all(nzchar(names(keys)))
  ok <- named && !anyDuplicated(names(keys)) && all(names(keys) %in% tip_keys())
  tip_arg(
    if (ok) TRUE else "Must be `TRUE`, `FALSE`, or a named logical vector such as `c(pin = FALSE)`.",
    "keys",
    paste0(
      "Give {.code TRUE} for every key, {.code FALSE} for none, or one switch per key, named ",
      paste0("{.val ", tip_keys(), "}", collapse = " or "),
      "; a key left out stays on."
    ),
    call
  )
  tip_keys()[tip_keys() %in% names(keys)[!keys]]
}

# The number of pins in a dependency of its own, named by the number: htmltools keeps one dependency of a name, so two
# numbers are two dependencies, and the runtime script sees both and warns when they differ.
tip_max_pins_dependency <- function(max_pins) {
  tip_gen_dependency(paste0("hovercraft-max-pins-", max_pins), ops = list(list(op = "maxPins", value = max_pins)))
}

tip_keys_dependency <- function(off) {
  name <- paste0("hovercraft-keys-", if (length(off)) paste(off, collapse = "-") else "none")
  tip_gen_dependency(name, ops = list(list(op = "keys", off = as.list(off))))
}

# A table's own count of pins, by the table's id as the inputs know it.
tip_table_pins_dependency <- function(id, max) {
  tip_gen_dependency(
    paste0("hovercraft-pins-", substr(rlang::hash(list(id, max)), 1L, 12L)),
    ops = list(list(op = "tablePins", id = id, max = max))
  )
}
