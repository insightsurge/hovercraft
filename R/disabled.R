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
# none would never show the reason, and a message at build time says so.
tip_check_when_disabled <- function(when_disabled, tag, call = rlang::caller_env()) {
  tip_arg_string(when_disabled, "when_disabled", null_ok = TRUE, call = call)
  if (is.null(when_disabled)) {
    return(invisible())
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

# What the wrapper takes from the control so that the page's layout stays as it was: a width written in the control's
# `style` (Shiny's `actionButton(width = "100%")` writes one), and the Bootstrap classes that give a width or a block.
tip_disabled_layout <- function(tag) {
  style <- paste(htmltools::tagGetAttribute(tag, "style"), collapse = ";")
  width <- regmatches(style, regexpr("(?<![-a-z])width[[:space:]]*:[[:space:]]*[^;]+", style, perl = TRUE))
  classes <- strsplit(paste(htmltools::tagGetAttribute(tag, "class"), collapse = " "), "[[:space:]]+")[[1L]]
  list(style = if (length(width)) paste0(width, ";"), class = intersect(classes, c("w-100", "d-block")))
}

# The tag inside its wrapper. The wrapper is inline-block, so a control keeps its size and its place in a line of
# buttons; the stylesheet makes it a block when the control asked for a full width.
tip_disabled_wrap <- function(tag, when_disabled) {
  layout <- tip_disabled_layout(tag)
  wrapper <- htmltools::tags$span(class = paste(c(tip_disabled_class(), layout$class), collapse = " "), tag)
  wrapper <- htmltools::tagAppendAttributes(wrapper, style = layout$style)
  do.call(
    htmltools::tagAppendAttributes,
    c(list(wrapper), stats::setNames(list(when_disabled), tip_disabled_attribute()))
  )
}
