# Which attributes tip_on() writes: the engine's attribute name, and the same with -template and -record.
tip_attributes <- function() {
  base <- tip_names()$attribute
  c(text = base, template = paste0(base, "-template"), record = paste0(base, "-record"))
}

# What tip_on() can check in R about the template it was given: its name against the ones defined in this session, and
# the fields it names against the record. Only a template tip_define() has seen can be checked; any other name is the
# page's to report (a UI is often written before the templates it uses).
tip_check_use <- function(template, record, call) {
  known <- tip_known()
  if (!length(known)) {
    return(invisible())
  }
  if (!template %in% names(known)) {
    near <- tip_near(template, names(known))
    tip_warn(
      c(
        "No template {.val {template}} has been defined with {.fn tip_define}.",
        i = if (nzchar(near)) trimws(near) else "The templates defined so far are {.val {names(known)}}.",
        i = "If it is defined later in the page, ignore this."
      ),
      class = "hovercraft_warning_template",
      call = call,
      .frequency = "once",
      .frequency_id = paste0("hovercraft-template-", template)
    )
    return(invisible())
  }
  fields <- known[[template]]$fields
  engine <- unlist(tip_rules()$engine_fields)
  missing <- setdiff(fields, c(names(record), engine))
  if (length(missing)) {
    holds <- if (length(record)) "The record holds {.val {names(record)}}." else "No record was given."
    tip_abort(
      c(
        "The template {.val {template}} names fields the record does not hold.",
        x = "Missing: {.val {missing}}.",
        i = holds
      ),
      class = "hovercraft_error_field",
      call = call
    )
  }
  clash <- intersect(intersect(fields, engine), names(record))
  if (length(clash)) {
    tip_warn(
      c(
        "The record has {.val {clash}}, which the engine also fills in.",
        i = "The record's own value is shown; rename the column if you meant the engine's."
      ),
      class = "hovercraft_warning_engine_name",
      call = call
    )
  }
  invisible()
}

#' Put a tip on a tag
#'
#' `r lifecycle::badge("experimental")`
#' Gives one HTML tag a tip that shows after the pointer rests on it, or keyboard focus arrives, and hides when either
#' leaves. The tip is plain text, or drawn from a named template ([tip_define()]) filled in from a record: the values of
#' one item, such as a row of a table. The engine's script and CSS are attached to the tag, so it works in a bslib page,
#' in a plain Shiny page and in output made later with `renderUI()`. A tag inside [bslib::tooltip()] or
#' [bslib::popover()], or with `data-bs-toggle`, keeps the tip that bslib draws and this one is left off.
#'
#' If both `text` and `template` are given, the text is shown for this tag. A template whose name has been defined in
#' this session is checked against the record here: a field it names that the record does not hold is an error. A
#' name that has not been defined is left to the page, which says so once in the browser's console.
#'
#' @param tag One HTML tag, such as `htmltools::tags$tr()` or `shiny::actionButton()`.
#' @param text One string of markdown (strong, emphasis, code, links, short lists), or `NULL`. It is shown as written:
#'   build it with [paste0()] or [sprintf()]. `NA` and `""` count as none.
#' @param ... Must be empty. It makes a misspelt argument an error.
#' @param template The name of a template defined with [tip_define()], or `NULL`.
#' @param record A named list, or a data frame of one row: the values the template's placeholders read, one value each.
#'   `NA` and `NULL` show a dash. See [tip_rows()] to get one record per row of a data frame.
#'
#' @returns The tag, with the tip's attributes and the engine's dependency added.
#' @family tooltips
#' @examples
#' tip_on(htmltools::tags$button("Run"), "Runs the **check**")
#'
#' tip_define(diff = tip_template(
#'   tip_title("{{ label }}"),
#'   tip_text("{{ differences | number:0 }} differences")
#' ))
#' rows <- data.frame(label = c("ADAE", "ADSL"), differences = c(3, 0))
#' records <- tip_rows(rows)
#' lapply(records, function(record) {
#'   tip_on(htmltools::tags$tr(htmltools::tags$td(record$label)), template = "diff", record = record)
#' })
#' @export
tip_on <- function(tag, text = NULL, ..., template = NULL, record = NULL) {
  if (inherits(tag, "shiny.tag.list")) {
    tip_abort(
      c("{.arg tag} is a list of tags; give one tag.", i = "Call {.fn tip_on} on each tag."),
      class = "hovercraft_error_tag"
    )
  }
  if (!inherits(tag, "shiny.tag")) {
    tip_abort("{.arg tag} must be an HTML tag, not {.obj_type_friendly {tag}}.", class = "hovercraft_error_tag")
  }
  rlang::check_dots_empty()
  checkmate::assert_string(text, na.ok = TRUE, null.ok = TRUE)
  checkmate::assert_string(template, min.chars = 1L, null.ok = TRUE)
  has_text <- !is.null(text) && !is.na(text) && nzchar(text)
  if (!has_text && is.null(template)) {
    tip_abort(
      "Give {.arg text} or {.arg template}; there is nothing to show.",
      class = "hovercraft_error_tip"
    )
  }
  if (!is.null(record)) {
    record <- tip_record(record)
  }
  if (!is.null(template)) {
    tip_check_use(template, record, rlang::current_env())
  } else if (!is.null(record)) {
    tip_warn(
      c("{.arg record} is ignored without a {.arg template}.", i = "A plain text tip has no placeholders to fill in."),
      class = "hovercraft_warning_record"
    )
    record <- NULL
  }
  attrs <- tip_attributes()
  values <- list()
  if (has_text) {
    values[[attrs[["text"]]]] <- text
  }
  if (!is.null(template)) {
    values[[attrs[["template"]]]] <- template
  }
  if (!is.null(record)) {
    values[[attrs[["record"]]]] <- tip_json(record)
  }
  tag$attribs[names(tag$attribs) %in% attrs] <- NULL
  tag <- do.call(htmltools::tagAppendAttributes, c(list(tag), values))
  htmltools::attachDependencies(tag, tip_dependency(), append = TRUE)
}
