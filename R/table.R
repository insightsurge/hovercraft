# tip_table(): tips on the rows, cells and header cells of a DT or reactable table (an experimental spike, built on
# top of the tip engine). R reads the table's own data, writes each row's record and the tip's template name onto the
# row (or cell) as the engine's attributes, and defines the template. Both widgets draw their rows in the browser, so
# the attributes are written there: DT through callbacks in its options, reactable through a class on each row that
# the layer in inst/table/table.js reads. The layer uses only the engine's public attributes and `Hovercraft.define()`.

# JavaScript as DT and reactable read it: `JS()` of htmlwidgets (which both re-export) is this class on a string.
tip_js <- function(code) structure(code, class = "JS_EVAL")

# The same as `htmlwidgets::onRender()`: a function the widget runs in the browser once it is drawn, given `data`.
tip_table_on_render <- function(widget, code, data) {
  widget$jsHooks$render <- c(widget$jsHooks$render, list(list(code = tip_js(code), data = data)))
  widget
}

# The engine's attribute name for one of its suffixes (see tip.json): "template", "record", "placement" or "kind".
tip_attribute <- function(key) {
  paste0(tip_names()$attribute, tip_rules()$pin$attributes[[key]])
}

tip_table_kind <- function(widget) {
  if (inherits(widget, "datatables")) {
    "dt"
  } else if (inherits(widget, "reactable")) {
    "reactable"
  } else {
    NA_character_
  }
}

# The data the table shows, as a data frame (no row names column), read from the widget itself.
tip_table_data <- function(widget, kind) {
  if (kind == "dt") {
    data <- widget$x$data
    if (isTRUE(attr(widget$x, "rownames"))) {
      data <- data[-1L]
    }
    return(data)
  }
  raw <- jsonlite::fromJSON(as.character(widget$x$tag$attribs$data), simplifyVector = TRUE)
  as.data.frame(raw, stringsAsFactors = FALSE, check.names = FALSE)
}

# Everything a content can be: a string (markdown, with placeholders if you like), or a tip_template(). NA and "" are no
# tip: the function returns NULL for them.
tip_table_content <- function(x, arg, columns, header = FALSE, call) {
  if (inherits(x, "hovercraft_template")) {
    template <- x
    fields <- tip_template_fields(template)
  } else if (length(x) == 1L && is.na(x)) {
    return(NULL)
  } else if (is.character(x) && length(x) == 1L) {
    if (!nzchar(x)) {
      return(NULL)
    }
    fields <- tip_check_text(x, paste0("The text given as ", arg), call = call)
    template <- tip_template(tip_text(x))
  } else {
    tip_abort(
      c(
        "{.arg {arg}} must be a string or a {.fn tip_template}, not {.obj_type_friendly {x}}.",
        i = paste0(
          "Give the words of the tip, with {.code ",
          tip_escape("{{ column }}"),
          "} to read a value of the row."
        )
      ),
      class = "hovercraft_error_argument",
      call = call
    )
  }
  reserved <- fields[startsWith(fields, ".")]
  if (length(reserved)) {
    tip_abort(
      c(
        "{.arg {arg}} names {.val {reserved}}, and names that start with a dot are kept for hovercraft.",
        i = "Name a column of the table."
      ),
      class = "hovercraft_error_field",
      call = call
    )
  }
  if (header && length(fields)) {
    tip_abort(
      c(
        "{.arg {arg}} reads {.val {fields}}, but a header tip has no row to read from.",
        i = "Write the words of the header tip without placeholders."
      ),
      class = "hovercraft_error_field",
      call = call
    )
  }
  missing <- setdiff(fields, columns)
  if (length(missing)) {
    tip_abort(
      c(
        "{.arg {arg}} names columns the table does not have.",
        x = "Missing: {.val {missing}}.",
        i = "The table's columns are {.val {columns}}."
      ),
      class = "hovercraft_error_field",
      call = call
    )
  }
  body <- tip_json(lapply(template, unclass))
  list(name = paste0("table_", substr(rlang::hash(body), 1L, 12L)), body = body, fields = unique(fields))
}

# `x` (cells or headers) as a named list of contents, one per column; entries that are NA or "" are left out.
tip_table_named <- function(x, arg, columns, header, call) {
  if (is.null(x)) {
    return(list())
  }
  if (
    inherits(x, "hovercraft_template") ||
      !is.vector(x) ||
      is.null(names(x)) ||
      anyNA(names(x)) ||
      !all(nzchar(names(x)))
  ) {
    tip_abort(
      c(
        "{.arg {arg}} must be a named vector or list, one entry per column.",
        i = "For example {.code {arg} = c({columns[[1]]} = \"words\")}."
      ),
      class = "hovercraft_error_argument",
      call = call
    )
  }
  unknown <- setdiff(names(x), columns)
  if (length(unknown)) {
    tip_abort(
      c(
        "{.arg {arg}} names columns the table does not have: {.val {unknown}}.",
        i = "The table's columns are {.val {columns}}.{tip_near(unknown[[1]], columns)}"
      ),
      class = "hovercraft_error_field",
      call = call
    )
  }
  out <- lapply(seq_along(x), function(i) {
    tip_table_content(x[[i]], paste0(arg, "[[\"", names(x)[[i]], "\"]]"), columns, header, call)
  })
  names(out) <- names(x)
  out[!vapply(out, is.null, NA)]
}

# One record per row as JSON text, holding only `fields`, written the way tip_on() writes a record.
tip_table_records <- function(data, fields, call) {
  if (!length(fields)) {
    return(rep("", nrow(data)))
  }
  tip_check_columns(data[fields], call)
  vapply(seq_len(nrow(data)), function(i) tip_json(tip_record(data[i, fields, drop = FALSE], call = call)), "")
}

# What an element carries for one content: its template, and the placement and kind when given.
tip_table_spec <- function(content, placement, id) {
  attrs <- stats::setNames(list(content$name), tip_attribute("template"))
  if (!is.null(placement)) {
    attrs[[tip_attribute("placement")]] <- placement
  }
  if (!is.null(id)) {
    attrs[[tip_attribute("kind")]] <- id
  }
  # `needs`: the fields the tip reads; a record where all of them are blank gets no tip (a cell that holds NA).
  list(attrs = attrs, record = tip_attribute("record"), needs = as.list(content$fields))
}

tip_table_dependencies <- function(widget) {
  table <- htmltools::htmlDependency(
    name = "hovercraft-table",
    version = tip_engine_version(),
    src = "table",
    package = "hovercraft",
    script = "table.js"
  )
  engine <- tip_dependency()
  if (inherits(engine, "html_dependency")) {
    engine <- list(engine)
  }
  widget$dependencies <- c(widget$dependencies, engine, list(table))
  widget
}

tip_table_defines <- function(contents) {
  contents <- Filter(Negate(is.null), contents)
  names <- vapply(contents, function(content) content$name, "")
  stats::setNames(lapply(contents, function(content) content$body), names)[!duplicated(names)]
}

# DT's header is drawn from the table's own HTML (`container`), so a column added to the data needs a header cell too.
tip_table_add_header_cell <- function(container, name, call) {
  head <- regmatches(container, regexpr("(?s)<thead>.*?</thead>", container, perl = TRUE))
  if (length(head) != 1L || lengths(regmatches(head, gregexpr("<tr", head, fixed = TRUE))) != 1L) {
    tip_abort(
      c(
        "The table has a header with more than one row, which {.fn tip_table} cannot add its record column to.",
        i = "Use a one-row header for now."
      ),
      class = "hovercraft_error_widget",
      call = call
    )
  }
  sub("</tr>", paste0("<th>", name, "</th></tr>"), container, fixed = TRUE)
}

tip_table_dt <- function(widget, plan, call) {
  x <- widget$x
  taken <- intersect(c("createdRow", "initComplete"), names(x$options))
  if (length(taken)) {
    tip_abort(
      c(
        "The table already sets {.field {taken}} in its options, which {.fn tip_table} needs.",
        i = "Take it out of {.arg options} for now."
      ),
      class = "hovercraft_error_widget",
      call = call
    )
  }
  data <- tip_table_data(widget, "dt")
  position <- function(column) match(column, names(x$data)) - 1L
  fields <- unique(c(plan$rows$fields, unlist(lapply(plan$cells, function(c) c$fields))))
  record <- NA_integer_
  if (length(fields)) {
    record <- ncol(x$data)
    name <- ".hovercraft"
    x$data[[name]] <- tip_table_records(data, fields, call)
    x$container <- tip_table_add_header_cell(x$container, name, call)
    attr(x, "colnames") <- c(attr(x, "colnames"), name)
    x$options$columnDefs <- c(
      x$options$columnDefs,
      list(list(targets = record, name = name, visible = FALSE, searchable = FALSE, orderable = FALSE))
    )
  }
  record_at <- if (is.na(record)) "null" else sprintf("data[%d]", record)
  if (!is.null(plan$rows)) {
    x$options$createdRow <- tip_js(sprintf(
      "function(row, data) { HovercraftTable.mark(row, %s, %s); }",
      tip_json(plan$specs$rows),
      record_at
    ))
  }
  for (column in names(plan$cells)) {
    def <- list(
      targets = position(column),
      createdCell = tip_js(sprintf(
        "function(td, cell, data) { HovercraftTable.mark(td, %s, %s); }",
        tip_json(plan$specs$cells[[column]]),
        record_at
      ))
    )
    x$options$columnDefs <- c(x$options$columnDefs, list(def))
  }
  if (length(plan$headers)) {
    specs <- stats::setNames(plan$specs$headers, vapply(names(plan$headers), position, 0L))
    x$options$initComplete <- tip_js(sprintf(
      "function() { HovercraftTable.headers(this.api(), %s); }",
      tip_json(specs)
    ))
  }
  widget$x <- x
  tip_table_on_render(
    widget,
    "function(el, x, cfg) { HovercraftTable.define(cfg.defines); }",
    list(defines = plan$defines)
  )
}

# A reactable class option is a string or a JS function; the layer's class is added to whatever the table has.
tip_table_class <- function(existing, own, args) {
  user <- if (is.null(existing)) {
    "''"
  } else if (inherits(existing, "JS_EVAL")) {
    sprintf("(%s)(%s)", as.character(existing), args)
  } else {
    tip_json(existing)
  }
  tip_js(sprintf("function(%s) { var user = %s; return (user ? user + ' ' : '') + %s; }", args, user, own))
}

tip_table_reactable <- function(widget, plan, call) {
  tag <- widget$x$tag
  columns <- tag$attribs$columns
  ids <- vapply(columns, function(column) column$id, "")
  data <- tip_table_data(widget, "reactable")
  fields <- unique(c(plan$rows$fields, unlist(lapply(plan$cells, function(c) c$fields))))
  if (!is.null(plan$rows)) {
    tag$attribs$rowClassName <- tip_table_class(
      tag$attribs$rowClassName,
      "(rowInfo && !rowInfo.aggregated ? 'hovercraft-row-' + rowInfo.index : '')",
      "rowInfo, state"
    )
  }
  cells <- list()
  for (column in names(plan$cells)) {
    at <- match(column, ids) - 1L
    columns[[at + 1L]]$className <- tip_table_class(
      columns[[at + 1L]]$className,
      sprintf("(rowInfo && !rowInfo.aggregated ? 'hovercraft-cell-%d-' + rowInfo.index : '')", at),
      "rowInfo, column, state"
    )
    cells[[as.character(at)]] <- tip_table_spec_for(plan, "cells", column)
  }
  heads <- list()
  for (column in names(plan$headers)) {
    at <- match(column, ids) - 1L
    existing <- columns[[at + 1L]]$headerClassName
    if (inherits(existing, "JS_EVAL")) {
      tip_abort(
        c(
          paste0(
            "The column {.val {column}} has a {.field headerClass} that is a JavaScript function, ",
            "which {.fn tip_table} cannot add to."
          ),
          i = "Give {.field headerClass} as a string for now."
        ),
        class = "hovercraft_error_widget",
        call = call
      )
    }
    columns[[at + 1L]]$headerClassName <- paste(c(existing, sprintf("hovercraft-head-%d", at)), collapse = " ")
    heads[[as.character(at)]] <- tip_table_spec_for(plan, "headers", column)
  }
  tag$attribs$columns <- columns
  widget$x$tag <- tag
  cfg <- list(
    rows = plan$specs$rows,
    cells = cells,
    headers = heads,
    records = as.list(tip_table_records(data, fields, call)),
    defines = plan$defines
  )
  tip_table_on_render(
    widget,
    "function(el, x, cfg) { HovercraftTable.define(cfg.defines); HovercraftTable.reactable(el, cfg); }",
    cfg
  )
}

tip_table_spec_for <- function(plan, what, column) plan$specs[[what]][[column]]

#' Put tips on the rows, cells and headers of a table
#'
#' `r lifecycle::badge("experimental")`
#' An experimental spike, built on top of the tip engine: the API may change in upcoming releases. Gives the rows of a
#' table made by `DT::datatable()` or `reactable::reactable()` a tip drawn from the row's own values, and
#' optionally a tip of their own to chosen cells and header cells. The tips follow the data row, so sorting, filtering
#' and paging need no code of yours: each row is its own record, whatever place it has on the screen.
#'
#' A cell's tip wins over its row's, and a row's tip opens at the pointer. A tip is made of words (markdown) with
#' `{{ column }}` placeholders, or of a [tip_template()]; every placeholder is checked against the table's columns, and
#' a miss lists them. Placeholders that start with a dot are kept for hovercraft. A header tip has no row to read from,
#' so it takes words only.
#'
#' Tips show where the pointer rests: **pointer only, no keyboard reach yet.** A table row or cell cannot take keyboard
#' focus, and `tip_table()` never adds a tab stop. A click on a row selects it in both widgets, so a row's tip does
#' not pin on click.
#'
#' Give `tip_table()` the finished widget, last in the pipe or at least after the columns are settled.
#' `DT::datatable()` gets one hidden column that holds each row's record (a table with a header of more than one row,
#' or with its own `createdRow` or `initComplete` option, is refused for now). `reactable::reactable()` gets its own
#' `rowClass`, `class` and `headerClass` added to yours. Only the columns a tip names are written to the page.
#'
#' @param widget A table made by `DT::datatable()` or by `reactable::reactable()`.
#' @param ... Must be empty. It makes a misspelt argument an error.
#' @param rows One string or [tip_template()] for every row, or `NULL`. `NA` and `""` are no tip.
#' @param cells A named vector or list, one entry per column, for the cells of that column. Each entry is a string or a
#'   [tip_template()]; `NA` and `""` give that column no tip.
#' @param headers The same for the header cells; words only.
#' @param placement Where the tips show: `"auto"`, `"top"`, `"bottom"`, `"left"` or `"right"`, or `NULL` for the page's
#'   choice.
#' @param id A name for this table's tips (lower-case words joined with `_`), written as the tip's kind, which the
#'   engine's events will carry, or `NULL`.
#'
#' @returns The widget, with the tips added.
#' @family tooltips
#' @examplesIf requireNamespace("DT", quietly = TRUE)
#' scores <- data.frame(team = c("Owls", "Foxes"), wins = c(7, 5), losses = c(3, 5))
#' DT::datatable(scores) |>
#'   tip_table(
#'     rows = "**{{ team }}**: {{ wins | number:0 }} wins, {{ losses | number:0 }} losses",
#'     headers = c(wins = "Games won this season")
#'   )
#' @export
tip_table <- function(
  widget,
  ...,
  rows = NULL,
  cells = NULL,
  headers = NULL,
  placement = NULL,
  id = NULL
) {
  call <- rlang::current_env()
  tip_check_dots(..., args = c("widget", "rows", "cells", "headers", "placement", "id"))
  kind <- tip_table_kind(widget)
  if (is.na(kind)) {
    tip_abort(
      c(
        "{.arg widget} must be a DT table or a reactable table, not {.obj_type_friendly {widget}}.",
        i = paste0(
          "Build one with {.code DT::datatable()} or {.code reactable::reactable()}, ",
          "then pipe it into {.fn tip_table}."
        )
      ),
      class = "hovercraft_error_widget"
    )
  }
  rlang::check_installed(if (kind == "dt") "DT" else "reactable")
  places <- unlist(tip_rules()$placement$values)
  tip_arg(
    checkmate::check_choice(placement, places, null.ok = TRUE),
    "placement",
    paste0("Give one of ", paste0("\"", places, "\"", collapse = ", "), ", or {.code NULL}.")
  )
  tip_arg(
    checkmate::check_string(id, pattern = tip_rules()$pin$kind_pattern, null.ok = TRUE),
    "id",
    "Give lower-case words joined with {.code _}, or {.code NULL}."
  )
  data <- tip_table_data(widget, kind)
  columns <- names(data)
  plan <- list(
    rows = if (!is.null(rows)) tip_table_content(rows, "rows", columns, call = call),
    cells = tip_table_named(cells, "cells", columns, FALSE, call),
    headers = tip_table_named(headers, "headers", columns, TRUE, call)
  )
  if (is.null(plan$rows) && !length(plan$cells) && !length(plan$headers)) {
    tip_abort(
      c("There is no tip to put on the table.", i = "Give {.arg rows}, {.arg cells} or {.arg headers} some words."),
      class = "hovercraft_error_tip",
      call = call
    )
  }
  plan$specs <- list(
    rows = if (!is.null(plan$rows)) tip_table_spec(plan$rows, placement, id),
    cells = lapply(plan$cells, tip_table_spec, placement = placement, id = id),
    headers = lapply(plan$headers, tip_table_spec, placement = placement, id = id)
  )
  plan$defines <- tip_table_defines(c(list(plan$rows), plan$cells, plan$headers))
  widget <- if (kind == "dt") tip_table_dt(widget, plan, call) else tip_table_reactable(widget, plan, call)
  tip_table_dependencies(widget)
}
