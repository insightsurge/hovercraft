# Which attributes tip_on() writes: the engine's attribute name, and the same with -template and -record.
tip_attributes <- function() {
  base <- tip_names()$attribute
  c(text = base, template = paste0(base, "-template"), record = paste0(base, "-record"))
}

# A tip shows when the pointer rests on a tag or keyboard focus arrives on it, so a tag that cannot take focus gives a
# keyboard user no way to the tip. This only says so, once; it never adds a tabindex (a table of 3,000 rows would become
# 3,000 Tab stops), because the choice of which tags are worth a Tab stop is the page's.
tip_can_focus <- function(tag) {
  type <- htmltools::tagGetAttribute(tag, "type")
  switch(
    tag$name,
    button = ,
    select = ,
    textarea = ,
    summary = TRUE,
    a = ,
    area = !is.null(htmltools::tagGetAttribute(tag, "href")),
    input = !identical(tolower(as.character(type)), "hidden"),
    !is.null(htmltools::tagGetAttribute(tag, "tabindex"))
  ) ||
    !is.null(htmltools::tagGetAttribute(tag, "tabindex"))
}

tip_check_focus <- function(tag, call) {
  if (!tip_can_focus(tag)) {
    tip_warn(
      c(
        "A {.code <{tag$name}>} tag cannot take keyboard focus, so a keyboard user will not reach its tip.",
        i = "Add {.code tabindex = \"0\"} to the tag, or put the tip on a button or a link.",
        i = "{.fn tip_on} never adds a tab stop itself."
      ),
      class = "hovercraft_warning_focus",
      call = call,
      .frequency = "once",
      .frequency_id = "hovercraft-focus"
    )
  }
  invisible()
}

# What tip_on() can check in R about the template it was given: its name against the ones defined in this session, and
# the fields it names against the record (`held`: the names the record has). Only a template tip_define() has seen can
# be checked; any other name is the page's to report (a UI is often written before the templates it uses). This page
# has no list of items, so {{ tip_index }} has nothing to count, and {{ tip_id }} is the tag's own id.
tip_check_use <- function(template, held, id, call) {
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
  if ("tip_index" %in% fields && !"tip_index" %in% held) {
    tip_abort(
      c(
        "The template {.val {template}} names {.code tip_index}, which has nothing to count here.",
        i = paste0(
          "It comes with a later version, with tips that belong to a list. ",
          "Give the record a value of that name for now."
        )
      ),
      class = "hovercraft_error_field",
      call = call
    )
  }
  if ("tip_id" %in% fields && !"tip_id" %in% held && is.null(id)) {
    tip_abort(
      c(
        "The template {.val {template}} names {.code tip_id}, which is the tag's own id, and the tag has none.",
        i = "Give the tag an {.arg id}, or put a value named {.code tip_id} in the record."
      ),
      class = "hovercraft_error_field",
      call = call
    )
  }
  missing <- setdiff(fields, c(held, engine))
  if (length(missing)) {
    holds <- if (length(held)) "The record holds {.val {held}}." else "No record was given."
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
  clash <- intersect(intersect(fields, engine), held)
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
#' A tip shows on keyboard focus as well as on a pointer, so a tag that cannot take focus (a table row, a `div`) gives a
#' keyboard user no way to the tip. `tip_on()` warns once about such a tag and never adds a `tabindex` itself, because a
#' table of 3,000 rows would become 3,000 Tab stops: give the tags worth a stop `tabindex = "0"`, or put the tip on a
#' button or a link.
#'
#' If both `text` and `template` are given, the text is shown for this tag. A template whose name has been defined in
#' this session is checked against the record here: a field it names that the record does not hold is an error. A
#' name that has not been defined is left to the page, which says so once in the browser's console.
#'
#' @param tag One HTML tag, such as `htmltools::tags$tr()` or `shiny::actionButton()`.
#' @param text One string of markdown (strong, emphasis, code, links, short lists), or `NULL`. It is shown as written:
#'   build it with [paste0()] or [sprintf()]. `NA` and `""` count as none: the tag comes back as it was.
#' @param ... Must be empty. It makes a misspelt argument an error.
#' @param template The name of a template defined with [tip_define()], or `NULL`.
#' @param record A named list, or a data frame of one row: the values the template's placeholders read, one value each.
#'   `NA` and `NULL` show a dash. Dates are written as `YYYY-MM-DD`, times in UTC as `YYYY-MM-DDTHH:MM:SSZ` and
#'   factors as their labels. Once [tip_define()] has seen the template, only the columns it names are kept, so nothing
#'   else in a row is written into the page; for a template not seen here, every column goes, so choose them with
#'   `tip_rows(data, columns = )`. See [tip_rows()] to get one record per row of a data frame.
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
#'   tip_on(
#'     htmltools::tags$tr(htmltools::tags$td(record$label), tabindex = "0"),
#'     template = "diff",
#'     record = record
#'   )
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
  tip_arg_string(text, "text", min_chars = 0L, null_ok = TRUE, na_ok = TRUE)
  tip_arg_string(template, "template", null_ok = TRUE)
  has_text <- !is.null(text) && !is.na(text) && nzchar(text)
  if (!has_text && is.null(template)) {
    if (is.null(text)) {
      tip_abort("Give {.arg text} or {.arg template}; there is nothing to show.", class = "hovercraft_error_tip")
    }
    return(tag) # a missing or empty text is no tip, so a table row with no note stays as it was
  }
  tip_check_focus(tag, rlang::current_env())
  held <- names(record)
  keep <- tip_known()[[template %||% ""]]$fields
  if (!is.null(record)) {
    record <- tip_record(record, keep)
  }
  if (!is.null(template)) {
    tip_check_use(template, held, htmltools::tagGetAttribute(tag, "id"), rlang::current_env())
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
