# tip_on(when_disabled = ): the reason a control is switched off, shown only while it is. This is hovercraft's own
# layer on top of the tip engine, and it never edits the engine. A control that is natively disabled is skipped by the
# keyboard and, with Bootstrap's `pointer-events: none` on a disabled button, by the pointer, so the engine cannot reach
# it. The control is therefore put in a small wrapper that the pointer and the keyboard do reach. R writes the wrapper
# with the reason in its own attribute. While the control is disabled, inst/disabled/disabled.js copies the reason into
# the engine's reason attribute, makes the wrapper a tab stop and gives it the control's tip id and events; it takes all
# of that away when the control is enabled again. The engine keeps saying "Not available" for as long as its reason
# attribute stays, and blocks the clicks of an enabled control, so it must go.

# The wrapper's class and the attribute that holds the reason, named after the engine's own prefix like the marker's.
# inst/disabled/disabled.css spells the same class; test-disabled.R holds the two to each other.
tip_disabled_class <- function() paste0(tip_names()$prefix, "-when-disabled")

tip_disabled_attribute <- function() paste0("data-", tip_disabled_class())

# The tags that can be switched off by `disabled`, which is what Shiny's `update*(disabled = )` and the other packages
# that toggle a control use.
tip_disabled_tags <- function() c("button", "input", "select", "textarea", "fieldset")

tip_disabled_dependency <- function() {
  config <- list(
    reason = tip_attribute("reason"),
    own = tip_disabled_attribute(),
    id = tip_attribute("id"),
    events = tip_shiny_attribute()
  )
  htmltools::htmlDependency(
    name = "hovercraft-disabled",
    version = as.character(getNamespaceVersion("hovercraft")),
    src = "disabled",
    package = "hovercraft",
    script = "disabled.js",
    stylesheet = "disabled.css",
    head = sprintf(
      "<script>%sDisabled.start(%s);</script>",
      tip_names()$global,
      jsonlite::toJSON(config, auto_unbox = TRUE)
    ),
    all_files = FALSE
  )
}

# `when_disabled` as one string, after checking that the tag has a control inside it that can be disabled. A tag with
# none would never show the reason, and a message at build time says so. A reason of blanks only shows nothing in the
# tip, and would leave a tab stop that says nothing.
tip_check_when_disabled <- function(when_disabled, tag, call = rlang::caller_env()) {
  tip_arg_string(when_disabled, "when_disabled", null_ok = TRUE, call = call)
  if (is.null(when_disabled)) {
    return(invisible())
  }
  if (!nzchar(trimws(when_disabled))) {
    tip_abort(
      c(
        "{.arg when_disabled} is only blanks, so the tip would have nothing to say.",
        i = "Give the reason in words, such as {.code \"Pick a dataset first\"}."
      ),
      class = "hovercraft_error_argument",
      call = call
    )
  }
  if (!any(vapply(tip_tags_in(tag), function(x) x$name %in% tip_disabled_tags(), NA))) {
    tip_abort(
      c(
        "{.arg when_disabled} needs a control that can be disabled, and {.code <{tag$name}>} has none inside it.",
        i = "Give {.fn tip_on} a button, an input, a select or a textarea, such as {.fn shiny::actionButton}."
      ),
      class = "hovercraft_error_when_disabled",
      call = call
    )
  }
  invisible()
}

# A tag that an earlier tip_on(when_disabled = ) made: its wrapper. A second call changes the reason of this wrapper and
# does not put a second one around it.
tip_is_disabled_wrapper <- function(tag) {
  tip_marker_has_class(tag, tip_disabled_class()) &&
    length(tag$children) == 1L &&
    inherits(tag$children[[1]], "shiny.tag")
}

# The inline declarations and the Bootstrap classes of a control that give it its place in a layout: its width, its
# share of a flex row, its side. They belong to the wrapper, which is what the layout now sees, and the control fills
# the wrapper. `d-block` stays on both, since the control must be a block as well. A width the control had becomes
# `100%`.
tip_disabled_move_style <- function() {
  c(
    "width",
    "min-width",
    "max-width",
    "flex",
    "flex-grow",
    "flex-shrink",
    "flex-basis",
    "align-self",
    "order",
    "float"
  )
}

tip_disabled_move_class <- function() {
  paste0(
    "^(w-(25|50|75|100|auto)|flex-(fill|grow-[01]|shrink-[01])|(ms|me|mx)-auto|float-(start|end|none)|",
    "(order|align-self|col)(-[a-z0-9]+)*|(flex-(sm|md|lg|xl|xxl)-(fill|grow-[01]|shrink-[01])))$"
  )
}

tip_disabled_split <- function(tag) {
  style <- trimws(strsplit(paste(htmltools::tagGetAttribute(tag, "style"), collapse = ";"), ";", fixed = TRUE)[[1L]])
  style <- style[nzchar(style)]
  property <- tolower(trimws(sub(":.*$", "", style)))
  moved <- property %in% tip_disabled_move_style()
  classes <- strsplit(paste(htmltools::tagGetAttribute(tag, "class"), collapse = " "), "[[:space:]]+")[[1L]]
  classes <- classes[nzchar(classes)]
  to_wrapper <- grepl(tip_disabled_move_class(), classes)
  # What makes the wrapper wider than its content makes the control fill the wrapper.
  stretches <- any(property %in% c("width", "min-width", "flex", "flex-grow", "flex-basis"))
  stretch_class <- any(grepl("^(w-|col|flex-(fill|grow|(sm|md|lg|xl|xxl)-(fill|grow)))", classes[to_wrapper]))
  keep_style <- c(style[!moved], if (stretches) "width: 100%")
  keep_class <- c(classes[!to_wrapper], if (stretch_class) "w-100")
  tag$attribs[names(tag$attribs) %in% c("style", "class")] <- NULL
  tag <- do.call(
    htmltools::tagAppendAttributes,
    c(
      list(tag),
      list(style = if (length(keep_style)) paste0(paste(keep_style, collapse = "; "), ";")),
      list(class = if (length(keep_class)) paste(keep_class, collapse = " "))
    )
  )
  list(
    tag = tag,
    style = if (any(moved)) paste0(paste(style[moved], collapse = "; "), ";"),
    class = unique(c(classes[to_wrapper], intersect(classes, "d-block")))
  )
}

# The tag inside its wrapper. The wrapper is an inline-block `span`, so a control keeps its size and its place in a line
# of buttons, or a block `div` for a tag that is a block (the `div` Shiny puts around an input); the stylesheet gives it
# the control's width and place. `attribs` are what the engine reads by walking up from the tip's item, which is the
# wrapper while the control is disabled: the look and the placement.
tip_disabled_wrap <- function(tag, when_disabled, attribs = list(), wrapper = NULL) {
  if (is.null(wrapper)) {
    layout <- tip_disabled_split(tag)
    tag <- layout$tag
    block <- tag$name %in% c("div", "fieldset", "p", "form", "section")
    wrapper <- htmltools::tag(
      if (block) "div" else "span",
      list(
        class = paste(
          c(tip_disabled_class(), if (block) paste0(tip_disabled_class(), "-block"), layout$class),
          collapse = " "
        )
      )
    )
    wrapper <- htmltools::tagAppendAttributes(wrapper, style = layout$style)
  }
  wrapper$children <- list(tag)
  own <- c(
    vapply(c("preset", "class", "placement"), tip_attribute, ""),
    tip_theme_attribute(),
    tip_disabled_attribute()
  )
  wrapper$attribs[names(wrapper$attribs) %in% own] <- NULL
  do.call(
    htmltools::tagAppendAttributes,
    c(list(wrapper), attribs, stats::setNames(list(when_disabled), tip_disabled_attribute()))
  )
}
