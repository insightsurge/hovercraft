# Templates: a named list of blocks a tip is drawn from. tip_template() builds one, tip_define() puts named ones on the
# page. The template names follow the engine's name rule (lower-case letters, digits and _, starting with a letter, not
# a word R reserves), read from engine.json.

# The fields a template's blocks name: the placeholders in their text and the field of a code block.
tip_template_fields <- function(template) {
  blocks <- tip_rules()$blocks
  out <- character()
  for (block in template) {
    spec <- blocks[[block$type]]
    for (path in unlist(spec$placeholders)) {
      texts <- if (path == "text") {
        block$text
      } else {
        vapply(block$fields, function(pair) pair$value, "")
      }
      out <- c(out, unlist(lapply(texts, function(text) tip_parse(text)$fields)))
    }
    for (key in unlist(spec$names_fields)) {
      out <- c(out, block[[key]])
    }
  }
  unique(out)
}

# A block given as a plain list, `list(type = "text", text = "...")`, made through its constructor so it is checked the
# same way. `where` says which one it is, for the messages.
tip_as_block <- function(x, where, call) {
  rules <- tip_rules()
  if (inherits(x, "hovercraft_block")) {
    return(x)
  }
  if (!is.list(x) || !is.character(x$type) || length(x$type) != 1L) {
    tip_abort(
      c(
        "{where} is not a block.",
        i = "Make one with {.fn tip_title}, {.fn tip_text}, {.fn tip_badge}, {.fn tip_fields} or {.fn tip_code}."
      ),
      class = "hovercraft_error_block",
      call = call
    )
  }
  if (x$type %in% unlist(rules$blocks_later)) {
    tip_abort(
      "{where} is a {.val {x$type}} block, which comes with a later version.",
      class = "hovercraft_error_later",
      call = call
    )
  }
  if (!x$type %in% names(rules$blocks)) {
    tip_abort(
      "{where} has the type {.val {x$type}}, which is not a block. The types are {.val {names(rules$blocks)}}.",
      class = "hovercraft_error_block",
      call = call
    )
  }
  known <- names(rules$blocks[[x$type]]$fields)
  unknown <- setdiff(names(x), c("type", known))
  if (length(unknown)) {
    tip_abort(
      "{where} has fields a {.val {x$type}} block does not take: {.val {unknown}}. It takes {.val {known}}.",
      class = "hovercraft_error_block",
      call = call
    )
  }
  maker <- switch(x$type, title = tip_title, text = tip_text, badge = tip_badge, fields = tip_fields, code = tip_code)
  rest <- x[names(x) != "type"]
  if (x$type == "fields") {
    rest <- stats::setNames(
      lapply(rest$fields, function(pair) pair$value),
      vapply(rest$fields, function(pair) pair$label, "")
    )
  }
  do.call(maker, rest)
}

#' A template: the blocks a tip is drawn from
#'
#' `r lifecycle::badge("experimental")`
#' A template is a list of blocks, shown in order. Name it with [tip_define()], then use it on a tag with [tip_on()]. A
#' block can be made with [tip_title()] and the other block functions, or written as a plain list such as
#' `list(type = "text", text = "...")`; both are checked in the same way.
#'
#' @param ... One or more blocks.
#'
#' @returns A template, to give to [tip_define()].
#' @family tooltips
#' @examples
#' tip_template(
#'   tip_title("{{ label }}"),
#'   tip_text("{{ differences | number:0 }} differences"),
#'   list(type = "badge", text = "{{ status }}")
#' )
#' @export
tip_template <- function(...) {
  blocks <- list(...)
  if (!length(blocks)) {
    tip_abort("A template needs at least one block.", class = "hovercraft_error_template")
  }
  call <- rlang::current_env()
  blocks <- lapply(seq_along(blocks), function(i) tip_as_block(blocks[[i]], paste0("Block ", i), call))
  structure(blocks, class = "hovercraft_template")
}

#' @export
print.hovercraft_template <- function(x, ...) {
  cli::cat_line("<tip template: ", paste(vapply(x, function(block) block$type, ""), collapse = ", "), ">")
  invisible(x)
}

#' Name the templates for a page
#'
#' `r lifecycle::badge("experimental")`
#' Puts named templates on the page, so a tag can use one with `tip_on(tag, template = "name")`. Put the result in the
#' page's UI, once. A name is lower-case letters, digits and `_`, starting with a letter. Defining a name again replaces
#' the earlier template under it, with a warning when the blocks differ. Templates belong to the whole page, so a Shiny
#' module should put its id in the names it defines, such as `paste0(id, "_row")`.
#'
#' @param ... Templates, each with a name: `tip_define(diff = tip_template(...))`. A template may also be a list of
#'   blocks.
#'
#' @returns Tags to put in the page's UI.
#' @family tooltips
#' @examples
#' tip_define(
#'   diff = tip_template(
#'     tip_title("{{ label }}"),
#'     tip_text("{{ differences | number:0 }} differences")
#'   )
#' )
#' @export
tip_define <- function(...) {
  templates <- list(...)
  tip_arg_list(
    templates,
    "...",
    min.len = 1L,
    fix = "Give each template a name: {.code tip_define(name = tip_template(...))}."
  )
  rules <- tip_engine()$rules$names
  js <- character()
  for (name in names(templates)) {
    if (!grepl(rules$pattern, name) || name %in% unlist(rules$r_reserved)) {
      tip_abort(
        c(
          "The template name {.val {name}} is not allowed.",
          i = "Use lower-case letters, digits and {.code _}, starting with a letter (not an R keyword)."
        ),
        class = "hovercraft_error_name"
      )
    }
    template <- templates[[name]]
    if (!inherits(template, "hovercraft_template")) {
      template <- if (is.list(template) && !is.null(template$type)) {
        tip_template(template)
      } else {
        do.call(tip_template, unname(template))
      }
    }
    body <- tip_json(lapply(template, unclass))
    before <- tip_state$templates[[name]]
    if (!is.null(before) && !identical(before$body, body)) {
      tip_warn(
        c(
          "The template {.val {name}} was already defined with different blocks; this one replaces it.",
          i = "Templates are shared by the whole page. Put a module's id in the names it defines, such as {.val {paste0(\"<id>_\", name)}}."
        ),
        class = "hovercraft_warning_template"
      )
    }
    tip_state$templates[[name]] <- list(fields = tip_template_fields(template), body = body)
    js <- c(js, sprintf("%s.define(%s, %s);", tip_names()$global, tip_json(name), body))
  }
  htmltools::attachDependencies(
    htmltools::tagList(htmltools::tags$script(htmltools::HTML(paste(js, collapse = "\n")))),
    tip_dependency(),
    append = TRUE
  )
}
