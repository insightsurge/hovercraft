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
tip_is_control <- function(tag) {
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

# The pin key: one letter, used only while the focus is on the tagged item (WCAG 2.1.4). The tagged item announces it
# (`aria-keyshortcuts`) where a keyboard and a screen reader meet the item; the layer listens for it.
tip_pin_key <- function() "p"

tip_pin_dependency <- function() {
  config <- list(
    attributes = list(pin = tip_attribute("pin"), part = tip_contract()$attributes$part, id = tip_attribute("id")),
    events = list(hint = tip_contract()$events$hint$name),
    global = tip_names()$global,
    css = tip_names()$css,
    key = tip_pin_key(),
    labels = list(pin = "Pin this tip")
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
