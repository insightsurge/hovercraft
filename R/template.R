# Templates: a named list of blocks a tip is drawn from. tip_template() builds one, tip_define() puts named ones on the
# page. The template names follow the engine's name rule (lower-case letters, digits and _, starting with a letter, not
# a word R reserves), read from engine.json.

# The fields a template's blocks name: the placeholders in their text, a badge's dynamic status, and the field of a
# code block.
tip_template_fields <- function(template) {
  blocks <- tip_rules()$blocks
  out <- character()
  for (block in template) {
    spec <- blocks[[block$type]]
    for (path in unlist(spec$placeholders)) {
      # "text" is a field of the block; "fields.value" is the `value` of each item in its `fields`.
      parts <- strsplit(path, ".", fixed = TRUE)[[1]]
      texts <- if (length(parts) == 1L) {
        block[[parts]]
      } else {
        vapply(block[[parts[[1]]]], function(item) item[[parts[[2]]]], "")
      }
      out <- c(out, unlist(lapply(texts, function(text) tip_parse(text)$fields)))
    }
    if (identical(block$type, "badge") && tip_status_has_placeholder(block$tone)) {
      out <- c(out, tip_parse(block$tone)$fields)
    }
    for (key in unlist(spec$names_fields)) {
      out <- c(out, block[[key]])
    }
  }
  unique(out)
}

# The constructor of each block type this package can make. The engine may have more types (a group, a host): those wait
# for a later version.
tip_block_makers <- function() {
  list(title = tip_title, text = tip_text, badge = tip_badge, fields = tip_fields, code = tip_code)
}

# Serializes a template's blocks into plain lists for the engine JSON: strips R attributes, routes dynamic-status
# badges to the package's `badge_data` block, and adds the footer group/divider when `foot` slot blocks are present.
tip_blocks_plain <- function(template) {
  plain_one <- function(block, parent = NULL) {
    out <- unclass(block)
    attributes(out) <- list(names = names(out))
    if (identical(out$type, "badge") && tip_status_has_placeholder(out$tone)) {
      out$type <- "badge_data"
    }
    if (!is.null(parent)) {
      out$parent <- parent
    }
    out
  }
  slots <- attr(template, "slots")
  if (is.null(slots) || !length(slots$foot)) {
    return(lapply(template, plain_one))
  }
  above <- c(slots$head, slots$body)
  out <- lapply(template[above], plain_one)
  if (length(above)) {
    out <- c(out, list(list(type = "slot_divider")))
  }
  out <- c(
    out,
    list(list(type = "group", id = "hc_foot", direction = "column", gap = "s")),
    lapply(template[slots$foot], plain_one, parent = "hc_foot")
  )
  out
}

# Normalizes one template slot (head, body or foot): NULL, a single block, or a list of blocks.
tip_slot_blocks <- function(x, slot, call) {
  if (is.null(x)) {
    return(list())
  }
  items <- if (inherits(x, "hovercraft_block") || (is.list(x) && !is.null(x$type))) {
    list(x)
  } else if (is.list(x)) {
    unname(x)
  } else {
    tip_abort(
      c(
        "{.arg {slot}} must be a block or a list of blocks, not {.obj_type_friendly {x}}.",
        i = "Give a block such as {.fn tip_title} or a list of blocks."
      ),
      class = "hovercraft_error_block",
      call = call
    )
  }
  lapply(seq_along(items), function(i) {
    where <- if (length(items) == 1L) paste0("Slot `", slot, "`") else paste0("Slot `", slot, "`, block ", i)
    tip_as_block(items[[i]], where, call)
  })
}

# Checks a template name: plain engine name, or a Shiny ns()-prefixed module name (`mod-name`, `outer-inner-name`).
tip_template_name_ok <- function(name, rules) {
  reserved <- unlist(rules$r_reserved)
  if (!grepl("-", name, fixed = TRUE)) {
    return(grepl(rules$pattern, name) && !name %in% reserved)
  }
  if (endsWith(name, "-") || startsWith(name, "-")) {
    return(FALSE)
  }
  parts <- strsplit(name, "-", fixed = TRUE)[[1L]]
  if (any(!nzchar(parts))) {
    return(FALSE)
  }
  prefixes <- parts[-length(parts)]
  base <- parts[[length(parts)]]
  all(grepl("^[A-Za-z0-9_]+$", prefixes)) && grepl(rules$pattern, base) && !base %in% reserved
}

# The block types the engine has and this package can make: what a message may offer.
tip_block_types <- function(rules) intersect(names(rules$blocks), names(tip_block_makers()))

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
  if (identical(x$type, "output")) {
    unknown <- setdiff(names(x), c("type", "output", "keep_alive"))
    if (length(unknown)) {
      tip_abort(
        paste0(
          "{where} has fields an {.val output} block does not take: {.val {unknown}}. ",
          "It takes {.val {c('output', 'keep_alive')}}."
        ),
        class = "hovercraft_error_block",
        call = call
      )
    }
    return(do.call(tip_output, x[names(x) != "type"]))
  }
  # The engine's block types that have no constructor here yet (a group, a host) wait for a later version too.
  makers <- tip_block_makers()
  if (x$type %in% unlist(rules$blocks_later) || (x$type %in% names(rules$blocks) && is.null(makers[[x$type]]))) {
    tip_abort(
      "{where} is a {.val {x$type}} block, which comes with a later version.",
      class = "hovercraft_error_later",
      call = call
    )
  }
  if (!x$type %in% names(rules$blocks)) {
    tip_abort(
      "{where} has the type {.val {x$type}}, which is not a block. The types are {.val {tip_block_types(rules)}}.",
      class = "hovercraft_error_block",
      call = call
    )
  }
  maker <- makers[[x$type]]
  known <- names(rules$blocks[[x$type]]$fields)
  # What the constructor takes now; a `fields` block's list of pairs is one engine field, `fields`.
  # A badge's `status` is the engine's `tone`.
  args <- sub("^status$", "tone", names(formals(maker)))
  takes <- intersect(known, c(args, if (x$type == "fields") "fields"))
  unknown <- setdiff(names(x), c("type", known))
  if (length(unknown)) {
    tip_abort(
      "{where} has fields a {.val {x$type}} block does not take: {.val {unknown}}. It takes {.val {takes}}.",
      class = "hovercraft_error_block",
      call = call
    )
  }
  # A field the engine takes but the constructor does not yet (`kicker`, `meta`, `parent`) waits for a later version.
  later <- setdiff(intersect(names(x), known), takes)
  if (length(later)) {
    tip_abort(
      "{where} has {.val {later}}, which a {.val {x$type}} block takes in the engine but comes with a later version.",
      class = "hovercraft_error_later",
      call = call
    )
  }
  rest <- x[names(x) != "type"]
  if (x$type == "fields") {
    rest <- stats::setNames(
      lapply(rest$fields, function(pair) pair$value),
      vapply(rest$fields, function(pair) pair$label, "")
    )
  }
  if (x$type == "badge") {
    names(rest)[names(rest) == "tone"] <- "status" # the engine's word for what R calls `status`
  }
  do.call(maker, rest)
}

#' A template: the blocks a tip is drawn from
#'
#' `r lifecycle::badge("experimental")`
#' A template is a list of blocks, shown in order. Name it with [tip_define()] and use the name in [tip_on()], or give
#' the template itself to `template =` of [tip_on()]. A block can be made with [tip_title()] and the other block
#' functions, or written as a plain list such as `list(type = "text", text = "...")`; both are checked in the same way.
#'
#' Blocks can be given in order through `...`, or placed into the named slots `head`, `body` and `foot`. Each slot
#' takes one block or a list of blocks; `foot` is drawn below a thin divider in muted text, suited for a source line or
#' footnote. Do not pass both positional blocks in `...` and `body`.
#'
#' @param ... One or more blocks in order (cannot be named; use `head`, `body` and `foot` for slots).
#' @param head Optional block or list of blocks drawn at the top of the tip (for example a [tip_title()] and a
#'   [tip_badge()]).
#' @param body Optional block or list of blocks drawn in the middle of the tip. Cannot be combined with `...`.
#' @param foot Optional block or list of blocks drawn at the bottom of the tip below a subtle divider.
#'
#' @returns A template, to give to [tip_define()] or to `template =` of [tip_on()].
#' @family tooltips
#' @examples
#' tip_template(
#'   tip_title("{{ label }}"),
#'   tip_text("{{ differences | number:0 }} differences"),
#'   list(type = "badge", text = "{{ status }}")
#' )
#'
#' tip_template(
#'   head = list(tip_title("{{ label }}"), tip_badge("{{ status }}", status = "{{ tone }}")),
#'   body = tip_fields(Records = "{{ records | number:0 }}"),
#'   foot = tip_text("Source: {{ source }}")
#' )
#' @export
tip_template <- function(..., head = NULL, body = NULL, foot = NULL) {
  call <- rlang::current_env()
  dots <- list(...)
  dot_names <- names(dots)
  if (!is.null(dot_names) && any(nzchar(dot_names))) {
    bad <- dot_names[nzchar(dot_names)]
    known <- c("head", "body", "foot")
    tip_abort(
      c(
        "Unknown template slot{?s}: {.val {bad}}.",
        i = paste0("The slots are {.arg head}, {.arg body} and {.arg foot}.", tip_near(bad[[1L]], known))
      ),
      class = "hovercraft_error_template",
      call = call
    )
  }
  if (length(dots) && !is.null(body)) {
    tip_abort(
      c(
        "Blocks were given in both {.arg ...} and {.arg body}.",
        i = "Pass blocks either positionally in {.arg ...}, or in {.arg head}, {.arg body} and {.arg foot}."
      ),
      class = "hovercraft_error_template",
      call = call
    )
  }
  use_slots <- !is.null(head) || !is.null(body) || !is.null(foot)
  if (!use_slots) {
    if (!length(dots)) {
      tip_abort("A template needs at least one block.", class = "hovercraft_error_template", call = call)
    }
    blocks <- lapply(seq_along(dots), function(i) tip_as_block(dots[[i]], paste0("Block ", i), call))
    return(structure(blocks, class = "hovercraft_template"))
  }
  head_blocks <- tip_slot_blocks(head, "head", call)
  body_blocks <- if (length(dots)) {
    lapply(seq_along(dots), function(i) tip_as_block(dots[[i]], paste0("Block ", i), call))
  } else {
    tip_slot_blocks(body, "body", call)
  }
  foot_blocks <- tip_slot_blocks(foot, "foot", call)
  blocks <- c(head_blocks, body_blocks, foot_blocks)
  if (!length(blocks)) {
    tip_abort("A template needs at least one block.", class = "hovercraft_error_template", call = call)
  }
  n_head <- length(head_blocks)
  n_body <- length(body_blocks)
  n_foot <- length(foot_blocks)
  slots <- list(
    head = seq_len(n_head),
    body = if (n_body) n_head + seq_len(n_body) else integer(),
    foot = if (n_foot) n_head + n_body + seq_len(n_foot) else integer()
  )
  structure(blocks, slots = slots, class = "hovercraft_template")
}

#' @export
print.hovercraft_template <- function(x, ...) {
  slots <- attr(x, "slots")
  if (is.null(slots)) {
    cli::cat_line("<tip template: ", paste(vapply(x, function(block) block$type, ""), collapse = ", "), ">")
  } else {
    parts <- character()
    for (slot in c("head", "body", "foot")) {
      idx <- slots[[slot]]
      if (length(idx)) {
        types <- paste(vapply(x[idx], function(block) block$type, ""), collapse = ", ")
        parts <- c(parts, paste0(slot, "(", types, ")"))
      }
    }
    cli::cat_line("<tip template: ", paste(parts, collapse = ", "), ">")
  }
  invisible(x)
}

#' Name the templates for a page
#'
#' `r lifecycle::badge("experimental")`
#' Puts named templates on the page, so a tag can use one with `tip_on(tag, template = "name")`. Put the result in the
#' page's UI, once. A name is lower-case letters, digits and `_`, starting with a letter; inside a Shiny module, prefix
#' it with `ns()` (for example `tip_define(!!ns("row") := tip_template(...))`) so two copies of the module keep their
#' own templates without overwriting each other. Defining the same name again replaces the earlier template under it,
#' with a warning when the blocks differ.
#'
#' The templates are also kept in R, so that [tip_on()] can check a record against them. Defined in a Shiny session (in
#' `renderUI()` or a module's server), they belong to that session: two sessions of one app may define one name with
#' different fields and each is checked against its own. Defined where the UI is built, they belong to the process and
#' every session is checked against them; a session may define the name again for itself, without a warning. A
#' `renderUI()` that is cached with `bindCache()` does not run again for the next session, so a template it defines is
#' known in R only to the session that ran it: define a template that sessions share where the UI is built.
#'
#' @param ... Templates, each with a name: `tip_define(diff = tip_template(...))`, or in a Shiny module
#'   `tip_define(!!ns("diff") := tip_template(...))`. A template may also be a list of blocks. Supports dynamic dots
#'   (`rlang::list2()`).
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
  templates <- rlang::list2(...)
  tip_arg_list(
    templates,
    "...",
    min.len = 1L,
    fix = "Give each template a name: {.code tip_define(name = tip_template(...))}."
  )
  rules <- tip_engine()$rules$names
  ops <- list()
  built <- list()
  for (name in names(templates)) {
    if (!tip_template_name_ok(name, rules)) {
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
    built[[name]] <- template
    body <- tip_json(tip_blocks_plain(template))
    before <- tip_known_scope()[[name]]
    if (!is.null(before) && !identical(before$body, body)) {
      tip_warn(
        c(
          "The template {.val {name}} was already defined with different blocks; this one replaces it.",
          i = paste0(
            "Templates are shared by the whole page. Put a module's id in the names it defines, ",
            "such as {.val {paste0(\"<id>_\", name)}}."
          )
        ),
        class = "hovercraft_warning_template"
      )
    }
    tip_known_set(name, list(fields = tip_template_fields(template), body = body))
    ops <- c(ops, list(list(op = "define", name = name, blocks = tip_blocks_plain(template))))
  }
  for (out_id in tip_output_ids(built)) {
    ops <- c(ops, list(list(op = "park", id = out_id)))
  }
  park_tags <- tip_output_tags(built)
  ui <- list(tip_ops_script(ops))
  if (length(park_tags)) {
    ui <- c(
      ui,
      list(htmltools::tags$div(
        class = tip_output_park_class(),
        hidden = NA,
        `aria-hidden` = "true",
        `data-hovercraft-keep` = "",
        park_tags
      ))
    )
  }
  htmltools::attachDependencies(
    htmltools::tagList(ui),
    tip_base_dependencies(),
    append = TRUE
  )
}
