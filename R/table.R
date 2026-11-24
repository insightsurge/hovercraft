# tip_table(): tips on the rows, cells and header cells of a DT or reactable table (an experimental spike, built on
# top of the tip engine). R reads the table's own data to check the tips, and the layer in inst/table/table.js writes
# each row's record and the tip's template name onto the row (or cell) as the engine's attributes, and defines the
# template. Both widgets draw their rows in the browser, so the attributes are written there: DT through callbacks in
# its options, reactable through a class on each row. The record is read from the row's own data in the browser, so no
# data is added to the table and a table that is given new data keeps tips that follow it. The layer uses only the
# engine's public attributes and `Hovercraft.define()`.

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

# The engine's class for the hidden description it adds inside a tagged element.
tip_description_class <- function() paste0(tip_names()$css, "-desc")

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

# Everything a content can be: a string (markdown, with placeholders if you like), a tip_template(), or a tip_spec()
# that holds one of those and a placement of its own. NA and "" are no tip: the function returns NULL for them. The
# placement is the spec's, or else the table's `placement`.
tip_table_template_part <- function(x, arg, columns, header, call) {
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
  # A table's tips are defined from the widget's own script, which cannot carry an output tag into the page: the tag
  # goes in the page's UI and the template names it by its id.
  if (length(tip_output_tags(list(template)))) {
    tip_abort(
      c(
        "{.arg {arg}} holds a {.fn tip_output} given as a tag, which a table cannot place in the page.",
        i = paste0(
          "Put the output tag (such as {.code shiny::plotOutput(\"detail\")}) in your UI and give ",
          "{.fn tip_output} its id: {.code tip_output(\"detail\")}."
        )
      ),
      class = "hovercraft_error_output",
      call = call
    )
  }
  body <- tip_json(tip_blocks_plain(template))
  list(
    name = paste0("table_", substr(rlang::hash(body), 1L, 12L)),
    body = body,
    fields = unique(fields),
    outputs = tip_output_ids(list(template))
  )
}

tip_table_content <- function(
  x,
  arg,
  columns,
  header = FALSE,
  placement = NULL,
  call,
  marker = NULL,
  row = FALSE,
  style = NULL
) {
  own <- NULL
  own_style <- NULL
  own_more <- NULL
  own_more_on <- NULL
  if (inherits(x, "hovercraft_spec")) {
    placement <- x$placement %||% placement
    own <- x$marker
    own_style <- x$style
    own_more <- x$more
    own_more_on <- x$more_on
    x <- x$content
  }
  part <- tip_table_template_part(x, arg, columns, header, call)
  if (is.null(part)) {
    return(NULL)
  }
  more_part <- if (!is.null(own_more)) {
    tip_table_template_part(own_more, paste0(arg, "$more"), columns, header, call)
  }
  # A row or a cell has no pin, so the long form of a table tip opens on hover unless `more_on` says otherwise.
  if (!is.null(more_part)) {
    effective_on <- own_more_on %||% "hover"
    if (identical(effective_on, "pin")) {
      tip_abort(
        c(
          "{.arg {arg}} sets {.arg more} with {.code more_on = \"pin\"}, but table tips cannot pin yet.",
          i = "Leave {.arg more_on} out, or set {.code more_on = \"hover\"}, to show the long form on hover."
        ),
        class = "hovercraft_error_pin",
        call = call
      )
    }
  }
  list(
    name = part$name,
    body = part$body,
    fields = unique(c(part$fields, more_part$fields)),
    more = more_part,
    more_on = if (!is.null(more_part)) own_more_on %||% "hover",
    outputs = unique(c(part$outputs, more_part$outputs)),
    placement = placement,
    style = tip_style_over(own_style, style),
    marker = tip_table_marker(own, marker, arg, row, call)
  )
}

# The marker of one target as a type, or NULL for none. A spec's own marker beats the table's; a row never takes the
# table's (a row is not marked), and a marker of its own on the rows stops. Only the corner is drawn in a table for now:
# the other markers need the widget's own markup (a words span, DT's sort arrows), which hovercraft does not write.
tip_table_marker <- function(own, table, arg, row, call) {
  if (row) {
    if (!is.null(own) && !identical(own$type, "none")) {
      tip_marker_abort(
        c(
          "{.arg {arg}} has a marker, but a row takes none.",
          i = "Put the marker on {.arg cells} or {.arg headers}, where a reader can see it."
        ),
        class = c("hovercraft_error_marker_table", "hovercraft_error_marker_tag"),
        call = call
      )
    }
    return(NULL)
  }
  marker <- own %||% table
  if (is.null(marker) || identical(marker$type, "none")) {
    return(NULL)
  }
  if (!identical(marker$type, "corner")) {
    tip_marker_abort(
      c(
        "A table draws only a {.val corner} marker for now, not {.val {marker$type}}.",
        i = "Use {.code marker = \"corner\"}, or mark the words with {.fn tip_on} outside the table."
      ),
      class = c("hovercraft_error_marker_table", "hovercraft_error_marker_type"),
      call = call
    )
  }
  marker$type
}

# `x` (cells or headers) as a named list of contents, one per column; entries that are NA or "" are left out.
tip_table_named <- function(x, arg, columns, header, placement, call, marker = NULL, style = NULL) {
  if (is.null(x)) {
    return(list())
  }
  if (
    inherits(x, c("hovercraft_template", "hovercraft_spec")) ||
      !is.vector(x) ||
      is.null(names(x)) ||
      anyNA(names(x)) ||
      !all(nzchar(names(x)))
  ) {
    tip_abort(
      c(
        "{.arg {arg}} must be a named vector or list, one entry per column.",
        i = paste0(
          "For example {.code {arg} = c({columns[[1]]} = \"words\")}, ",
          "or {.code {arg} = list({columns[[1]]} = tip_spec(\"words\"))}."
        )
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
    tip_table_content(
      x[[i]],
      paste0(arg, "[[\"", names(x)[[i]], "\"]]"),
      columns,
      header,
      placement,
      call,
      marker = marker,
      style = style
    )
  })
  names(out) <- names(x)
  out[!vapply(out, is.null, NA)]
}

# The values a tip reads are taken from the table's own data in the browser, so none is written to the page. What R can
# still check is that the columns a tip names hold one value per row: no data frame or matrix column, and values a tip
# can show (looked at in the first row, as the kinds are the same all down a column).
tip_table_check_values <- function(data, fields, call) {
  if (!length(fields)) {
    return(invisible(data))
  }
  tip_check_columns(data[fields], call)
  if (nrow(data)) {
    tip_record(data[1L, fields, drop = FALSE], call = call)
  }
  invisible(data)
}

# What an element carries for one content: its template, the fixed kind of its target, the placement when there is
# one, and, for a cell or a header cell, the anchor `content`, so the tip points at the text and not at the whole cell
# (a right-aligned header would point at empty space). A row keeps the engine's pointer alignment. With a table `id`,
# the layer also gives it a derived tip id from the base id, the target, the data row and the data column (`column`,
# 1-based, for a cell or a header cell); the format is the layer's own and may change.
tip_table_spec <- function(content, target, column = NULL, id = NULL) {
  attrs <- stats::setNames(list(content$name), tip_attribute("template"))
  attrs[[tip_attribute("kind")]] <- paste0("table_", target)
  if (!is.null(content$placement)) {
    attrs[[tip_attribute("placement")]] <- content$placement
  }
  if (!is.null(content$more)) {
    attrs[[tip_attribute("more_template")]] <- content$more$name
    attrs[[tip_attribute("more_on")]] <- content$more_on
  }
  if (!identical(target, "row")) {
    attrs[[tip_attribute("anchor")]] <- "content"
  }
  if (!is.null(content$style)) {
    attrs <- c(attrs, tip_style_tag(content$style)$attribs)
  }
  # The marker layer's attributes (inst/marker/marker.css draws the corner on a table's cell with a background, because
  # DT keeps a header cell's ::before and ::after for its sort arrows).
  if (!is.null(content$marker)) {
    attrs[[tip_marker_attribute()]] <- content$marker
    attrs[[tip_marker_attribute("in")]] <- "table"
  }
  ident <- NULL
  if (!is.null(id)) {
    ident <- list(attribute = tip_attribute("id"), base = id, target = target)
    ident$column <- column
  }
  # `needs`: the fields the tip reads; a record where all of them are blank gets no tip (a cell that holds NA).
  list(attrs = attrs, record = tip_attribute("record"), needs = as.list(content$fields), id = ident)
}

tip_table_dependencies <- function(widget, marker = FALSE, looks = list()) {
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
  widget$dependencies <- c(widget$dependencies, engine, list(table), if (marker) list(tip_marker_dependency()), looks)
  widget
}

tip_table_defines <- function(contents) {
  contents <- Filter(Negate(is.null), contents)
  parts <- unlist(
    lapply(contents, function(content) c(list(content), if (!is.null(content$more)) list(content$more))),
    recursive = FALSE
  )
  names <- vapply(parts, function(part) part$name, "")
  stats::setNames(lapply(parts, function(part) part$body), names)[!duplicated(names)]
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
  tip_table_check_values(data, fields, call)
  # DT hands its row callbacks the row's own data, so the layer reads the record from it: `field_at` says at which
  # position of the row each field sits. A table that DT updates with new data (`DT::replaceData()`) keeps its tips,
  # and no column is added to the data.
  field_at <- if (length(fields)) tip_json(stats::setNames(as.list(vapply(fields, position, 0L)), fields)) else "null"
  record <- sprintf("HovercraftTable.dtRecord(data, %s)", field_at)
  row_at <- "HovercraftTable.dtRow(this, index)"
  if (!is.null(plan$rows)) {
    x$options$createdRow <- tip_js(sprintf(
      "function(row, data, index) { HovercraftTable.mark(row, %s, %s, %s); }",
      tip_json(plan$specs$rows),
      record,
      row_at
    ))
  }
  for (column in names(plan$cells)) {
    def <- list(
      targets = position(column),
      createdCell = tip_js(sprintf(
        "function(td, cell, data, index) { HovercraftTable.mark(td, %s, %s, %s); }",
        tip_json(plan$specs$cells[[column]]),
        record,
        row_at
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
    paste(
      "function(el, x, cfg) {",
      "HovercraftTable.define(cfg.defines, cfg.desc, cfg.park);",
      "HovercraftTable.keyboard(el, cfg.kind);",
      "}"
    ),
    list(
      defines = plan$defines,
      kind = tip_attribute("kind"),
      desc = tip_description_class(),
      park = as.list(plan$outputs)
    )
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
  tip_table_check_values(data, fields, call)
  # A row's class says its data row and holds its record, read from reactable's own row values each time the row is
  # drawn, so a table that `reactable::updateReactable()` gives new data keeps tips that follow it.
  if (!is.null(plan$rows) || length(fields)) {
    tag$attribs$rowClassName <- tip_table_class(
      tag$attribs$rowClassName,
      sprintf("HovercraftTable.rowClass(rowInfo, %s)", tip_json(as.list(fields))),
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
    defines = plan$defines,
    kind = tip_attribute("kind"),
    desc = tip_description_class(),
    park = as.list(plan$outputs)
  )
  tip_table_on_render(
    widget,
    paste(
      "function(el, x, cfg) {",
      "HovercraftTable.define(cfg.defines, cfg.desc, cfg.park);",
      "HovercraftTable.reactable(el, cfg);",
      "HovercraftTable.keyboard(el, cfg.kind);",
      "}"
    ),
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
#' A cell's tip wins over its row's, and a row's tip opens at the pointer. The tip of a cell or a header cell points at
#' its text, not at the whole cell, so a right-aligned number in a wide column, or a right-aligned header, gets a tip
#' beside the text and not far to its left; there is no argument for it. A target that needs its own settings (style,
#' placement, marker, or `more` / `more_on`) takes a [tip_spec()] in place of words. A tip is made of words (markdown)
#' with
#' `{{ column }}` placeholders, or of a [tip_template()]; every placeholder is checked against the table's columns, and
#' a miss lists them. Placeholders that start with a dot are kept for hovercraft. A header tip has no row to read from,
#' so it takes words only.
#'
#' Tips also show for the keyboard. A row, a cell or a header cell cannot take focus by itself, so `tip_table()` gives
#' the table one tab stop, and the arrow keys move between the rows, cells and header cells that have a tip: left and
#' right inside a row (the row first, then its cells), up and down between rows to the same column, Home and End to the
#' ends of the row, and Ctrl with Home or End to the ends of the table. The tip shows for what has focus. A header that
#' its widget already makes focusable (the sortable headers of `DT`) keeps its own tab stop. Enter and Space do not
#' select a row. A click on a row selects it in both widgets, so a row's tip does not pin on click.
#'
#' What works, each checked in a browser:
#' * `DT`, in client mode and in server mode under Shiny, with `scrollX`, `scrollY`, `fixedHeader`, `colReorder` and
#'   `responsive`. With `responsive`, a column folded into the details row has no tip there; the row's tip is
#'   unchanged.
#' * `reactable`, with sorting, filtering, paging, row selection (a click selects the row and its tip stays),
#'   `groupBy` (the rows inside a group have tips, the group's own row has none) and `details` (the details panel has
#'   none).
#' * An `id` made by [shiny::NS()] inside a module.
#' * Tables that Shiny updates with new data: `DT::replaceData()` (it works in server mode) with the user's own data
#'   frame, and `reactable::updateReactable(data = )`. The tips follow the new rows, as they do after sorting,
#'   filtering and paging.
#'
#' Not supported: a `DT` table with its own `createdRow` or `initComplete` option; a `reactable` column whose
#' `headerClass` is a JavaScript function; any other widget.
#'
#' Large tables: `tip_table()` adds nothing to the data of the page, since each tip reads its values from the table's
#' own data in the browser. A table with every row drawn at once is slow in `DT` when cells or headers have tips
#' (thousands of rows take seconds); page it, or tip its rows only.
#'
#' Give `tip_table()` the finished widget, last in the pipe or at least after the columns are settled.
#' `DT::datatable()` keeps its columns and its inputs as they are: `tip_table()` adds no column and changes no event
#' (a table with its own `createdRow` or `initComplete` option is refused for now). `reactable::reactable()` gets its
#' own `rowClass`, `class` and `headerClass` added to yours; the class of a row holds the values its tips read, so it
#' changes with the data.
#'
#' @param widget A table made by `DT::datatable()` or by `reactable::reactable()`.
#' @param ... Must be empty. It makes a misspelt argument an error.
#' @param rows One string, [tip_template()] or [tip_spec()] for every row, or `NULL`. `NA` and `""` are no tip.
#' @param cells A named vector or list, one entry per column, for the cells of that column. Each entry is a string, a
#'   [tip_template()] or a [tip_spec()]; `NA` and `""` give that column no tip. Use a list to mix them:
#'   `list(status = tip_spec("{{ note }}", placement = "right"), wins = "{{ wins }} wins")`.
#' @param headers The same for the header cells; words only, so a [tip_spec()] here holds words with no placeholders.
#' @param style How every tip in the table looks, or `NULL` (the default) to take the page's: whatever `style` takes in
#'   [tip_on()] (a preset name, a [tip_style()], a [tip_theme()] or a [bslib::bs_theme()]). A [tip_spec()] with its own
#'   `style` beats it for that target. A row's own style also reaches the cells in that row that have none of their
#'   own, because the tip engine reads the nearest tagged element.
#' @param placement Where the tips show: `"auto"`, `"top"`, `"bottom"`, `"left"` or `"right"`, or `NULL` for the page's
#'   choice. A [tip_spec()] with its own placement beats it. A cell's tip follows this, not its row's.
#' @param marker The visible cue on every cell and header cell that has a tip, so a reader can see where the tips are:
#'   `"corner"` (a small triangle in the top corner, as a spreadsheet shows a note), or `NULL` or `"none"` for no
#'   marker (the default). A [tip_spec()] with its own `marker` beats it. A row takes no marker, and a table draws only
#'   the corner for now: `"underline"`, `"icon"` and an icon of your own stop with an error. A cell with a marker is
#'   highlighted while its tip is open. See [tip_marker()].
#' @param id A Shiny id for this table's tips, the same as [tip_on()]'s `id` will be: one non-empty string with no
#'   whitespace, so `ns("scores_tips")` works inside a module. Give it a different id from every other tip source on
#'   the page. With an `id`, every marked row, cell and header cell gets its own tip id derived from it, which follows
#'   the data row through sorting, filtering and paging; how that id is built is not part of the API. The copy of a
#'   header cell that `DT` draws for `scrollX` or `fixedHeader` carries the id of the header it copies. `NULL` gives the
#'   tips no id. The engine's kind of the tips is not yours to set: `tip_table()` writes `table_row`, `table_cell` and
#'   `table_header`.
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
  style = NULL,
  placement = NULL,
  marker = NULL,
  id = NULL
) {
  call <- rlang::current_env()
  tip_check_dots(..., args = c("widget", "rows", "cells", "headers", "style", "placement", "marker", "id"))
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
  tip_check_placement(placement)
  tip_check_id(id)
  style <- tip_as_style(style, call = call)
  marker <- if (!is.null(marker)) tip_as_marker(marker, call = call)
  data <- tip_table_data(widget, kind)
  columns <- names(data)
  plan <- list(
    rows = if (!is.null(rows)) {
      tip_table_content(rows, "rows", columns, placement = placement, call = call, row = TRUE, style = style)
    },
    cells = tip_table_named(cells, "cells", columns, FALSE, placement, call, marker, style),
    headers = tip_table_named(headers, "headers", columns, TRUE, placement, call, marker, style)
  )
  if (is.null(plan$rows) && !length(plan$cells) && !length(plan$headers)) {
    tip_abort(
      c("There is no tip to put on the table.", i = "Give {.arg rows}, {.arg cells} or {.arg headers} some words."),
      class = "hovercraft_error_tip",
      call = call
    )
  }
  # The engine reads a placement from the nearest tagged ancestor, so a cell with none of its own would follow its
  # row's. A cell follows the table's: when the row's differs, the cell says the table's (or "auto") itself.
  if (!is.null(plan$rows$placement) && !identical(plan$rows$placement, placement)) {
    plan$cells <- lapply(plan$cells, function(content) {
      content$placement <- content$placement %||% placement %||% "auto"
      content
    })
  }
  column_of <- function(column) match(column, columns)
  plan$specs <- list(
    rows = if (!is.null(plan$rows)) tip_table_spec(plan$rows, "row", id = id),
    cells = Map(
      function(content, column) tip_table_spec(content, "cell", column_of(column), id),
      plan$cells,
      names(plan$cells)
    ),
    headers = Map(
      function(content, column) tip_table_spec(content, "header", column_of(column), id),
      plan$headers,
      names(plan$headers)
    )
  )
  plan$defines <- tip_table_defines(c(list(plan$rows), plan$cells, plan$headers))
  plan$outputs <- unique(unlist(lapply(c(list(plan$rows), plan$cells, plan$headers), function(content) {
    content$outputs
  })))
  marked <- any(vapply(c(plan$cells, plan$headers), function(content) !is.null(content$marker), NA))
  if (!is.null(marker) && !marked && !identical(marker$type, "none")) {
    tip_marker_abort(
      c(
        "{.arg marker} has no cell or header cell to mark.",
        i = "A row takes no marker. Give {.arg cells} or {.arg headers} some words too."
      ),
      class = "hovercraft_error_marker_table",
      call = call
    )
  }
  widget <- if (kind == "dt") tip_table_dt(widget, plan, call) else tip_table_reactable(widget, plan, call)
  looks <- unlist(
    lapply(c(list(plan$rows), plan$cells, plan$headers), function(content) tip_style_tag(content$style)$deps),
    recursive = FALSE
  )
  tip_table_dependencies(widget, marked, looks[!duplicated(vapply(looks, function(dep) dep$name, ""))])
}
