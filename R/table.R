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
    if (x %in% names(tip_known())) {
      tip_abort(
        c(
          "{.arg {arg}} is {.val {x}}, the name of a template made with {.fn tip_define}; a table cannot use a name.",
          i = "Give the template itself: a {.fn tip_template}, such as {.code rows = tip_template(tip_text(\"...\"))}."
        ),
        class = "hovercraft_error_template",
        call = call
      )
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
  own_pin <- NULL
  own_events <- NULL
  if (inherits(x, "hovercraft_spec")) {
    own_pin <- x$pin
    own_events <- x$events
    placement <- x$placement %||% placement
    own <- x$marker
    own_style <- x$style
    own_more <- x$more
    x <- x$content
  }
  part <- tip_table_template_part(x, arg, columns, header, call)
  if (is.null(part)) {
    return(NULL)
  }
  more_given <- if (!is.null(own_more)) tip_more_unpack(own_more)
  more_part <- if (!is.null(more_given)) {
    tip_table_template_part(more_given$content, paste0(arg, "$more"), columns, header, call)
  }
  # Only a row can pin (`pin = TRUE`), so the long form of a table tip waits behind a button unless `tip_more(on = )`
  # says otherwise, and `on = "pin"` needs a row with a pin.
  if (!is.null(more_part) && identical(more_given$on, "pin") && !(row && !is.null(own_pin))) {
    tip_abort(
      c(
        "{.arg {arg}} sets {.arg more} with {.code tip_more(on = \"pin\")}, but this tip cannot pin.",
        i = paste0(
          "Only the tips of {.arg rows} can pin, with {.code tip_spec(pin = TRUE)}. ",
          "Or use {.code on = \"hover\"} or {.code \"button\"}."
        )
      ),
      class = "hovercraft_error_pin",
      call = call
    )
  }
  list(
    name = part$name,
    body = part$body,
    fields = unique(c(part$fields, more_part$fields)),
    more = more_part,
    more_on = if (!is.null(more_part)) more_given$on,
    more_label = if (!is.null(more_part)) more_given$label,
    outputs = unique(c(part$outputs, more_part$outputs)),
    placement = placement,
    style = tip_style_over(own_style, style),
    marker = tip_table_marker(own, marker, arg, row, call),
    pin = own_pin,
    events = own_events
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
  repeated <- unique(names(x)[duplicated(names(x))])
  if (length(repeated)) {
    tip_abort(
      c(
        "{.arg {arg}} names a column more than once: {.val {repeated}}.",
        i = "Give each column one entry. Put the words of both in one {.fn tip_spec}, or in one string."
      ),
      class = "hovercraft_error_field",
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
  for (i in seq_along(x)) {
    tip_table_check_target(x[[i]], paste0(arg, "[[\"", names(x)[[i]], "\"]]"), call)
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
# still check is that the columns a tip names hold one value per row: no data frame, matrix or list column (each is a
# property of the whole column), and values a tip can show, looked at in the first row, since a column has one class
# all down.
tip_table_check_values <- function(data, fields, call) {
  if (!length(fields)) {
    return(invisible(data))
  }
  tip_check_columns(data[fields], call)
  lists <- names(data[fields])[vapply(data[fields], is.list, NA)]
  if (length(lists)) {
    tip_abort(
      c(
        "Columns that hold a list: {.val {lists}}. A tip takes one value per column.",
        i = "Spread them into plain columns first, one column for each value."
      ),
      class = "hovercraft_error_record",
      call = call
    )
  }
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
tip_table_spec <- function(content, target, column = NULL, id = NULL, column_name = NULL) {
  pin <- content$pin
  events <- content$events
  attrs <- stats::setNames(list(content$name), tip_attribute("template"))
  attrs[[tip_attribute("kind")]] <- paste0("table_", target)
  if (!is.null(content$placement)) {
    attrs[[tip_attribute("placement")]] <- content$placement
  }
  reset <- NULL
  if (!is.null(content$more)) {
    attrs <- c(attrs, tip_more_values(content$more_on, content$more$name, label = content$more_label))
    # The attributes the "More" button writes while the long form is open go when the element is given another row.
    if (identical(content$more_on, "button")) {
      reset <- c(tip_attribute("more"), tip_attribute("more_template"), tip_attribute("more_on"))
      reset <- c(reset, tip_more_attribute("state"))
    }
  }
  if (!identical(target, "row")) {
    attrs[[tip_attribute("anchor")]] <- "content"
  }
  if (!is.null(content$style)) {
    attrs <- c(attrs, tip_style_tag(content$style)$attribs)
  }
  # A row that pins takes the engine's manual pin, so a click on it (which selects it) never pins; the pin handle in the
  # tip's corner and the pin key do (inst/pin/pin.js). The row announces the key, as tip_on() does.
  if (!is.null(pin)) {
    attrs[[tip_attribute("pin")]] <- pin
  }
  keys <- tip_keyshortcuts(!is.null(pin), identical(content$more_on, "button"))
  if (!is.null(keys)) {
    attrs[["aria-keyshortcuts"]] <- keys
  }
  if (!is.null(events)) {
    attrs[[tip_shiny_attribute()]] <- events
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
    # A tip that sends events says where it is (the table, the target, the data row and the column's name) in an
    # attribute of its own, which the Shiny layer reads for the input's value (inst/runtime/shiny.js).
    if (!is.null(events)) {
      ident$name <- column_name
      ident$address <- tip_table_address_attribute()
    }
  }
  # `needs`: the fields the tip reads; a record where all of them are blank gets no tip (a cell that holds NA).
  list(
    attrs = attrs,
    record = tip_attribute("record"),
    needs = as.list(content$fields),
    id = ident,
    reset = as.list(reset)
  )
}

# The attribute that holds a table tip's address, named after the engine's prefix as the events attribute is.
tip_table_address_attribute <- function() paste0("data-", tip_names()$prefix, "-table-address")

tip_table_dependencies <- function(widget, marker = FALSE, looks = list(), pin = FALSE, more = FALSE) {
  table <- htmltools::htmlDependency(
    name = "hovercraft-table",
    version = tip_package_version(),
    src = "table",
    package = "hovercraft",
    script = "table.js"
  )
  widget$dependencies <- c(
    widget$dependencies,
    tip_base_dependencies(),
    list(table),
    if (marker) list(tip_marker_dependency()),
    if (pin) list(tip_pin_dependency()),
    if (more) list(tip_more_dependency()),
    looks
  )
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

# DT escapes the HTML of the columns named by its `escapeIdx` option before a row reaches the browser (`escape = TRUE`,
# its default, escapes every column): the layer must give the tip the value as it is in the data. This is TRUE for every
# column, FALSE for none, or the 0-based positions of the escaped columns.
tip_table_dt_escaped <- function(x) {
  idx <- attr(x$options, "escapeIdx") %||% "true"
  if (identical(idx, "true")) {
    return(TRUE)
  }
  if (identical(idx, "false")) {
    return(FALSE)
  }
  as.list(as.integer(strsplit(gsub("\"", "", idx, fixed = TRUE), ",", fixed = TRUE)[[1]]) - 1L)
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
  # position of the row each field sits, and the escaped columns say what to undo (see `tip_table_dt_escaped()`). A
  # table that DT updates with new data (`DT::replaceData()`) keeps its tips, and no column is added to the data.
  field_at <- if (length(fields)) tip_json(stats::setNames(as.list(vapply(fields, position, 0L)), fields)) else "null"
  record <- sprintf("HovercraftTable.dtRecord(data, %s, %s)", field_at, tip_json(tip_table_dt_escaped(x)))
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
      "HovercraftTable.define(cfg.defines, cfg.desc, cfg.park, cfg.open);",
      "HovercraftTable.claim(el, cfg.id);",
      "HovercraftTable.keyboard(el, cfg.kind);",
      "}"
    ),
    c(
      list(
        defines = plan$defines,
        kind = tip_attribute("kind"),
        desc = tip_description_class(),
        park = as.list(plan$outputs),
        open = tip_table_open()
      ),
      plan$table_id
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
    park = as.list(plan$outputs),
    open = tip_table_open()
  )
  cfg <- c(cfg, plan$table_id)
  tip_table_on_render(
    widget,
    paste(
      "function(el, x, cfg) {",
      "HovercraftTable.define(cfg.defines, cfg.desc, cfg.park, cfg.open);",
      "HovercraftTable.claim(el, cfg.id);",
      "HovercraftTable.reactable(el, cfg);",
      "HovercraftTable.keyboard(el, cfg.kind);",
      "}"
    ),
    cfg
  )
}

# A spec for cells or headers cannot pin or send events: only a row can, for now (a click on a cell or a header cell
# does not pin, and the inputs hold a row's address).
tip_table_check_target <- function(x, arg, call) {
  if (!inherits(x, "hovercraft_spec")) {
    return(invisible())
  }
  for (what in c("pin", "events")) {
    if (!is.null(x[[what]])) {
      tip_abort(
        c(
          if (what == "pin") {
            "{.arg {arg}} has a pin: only rows pin for now."
          } else {
            "{.arg {arg}} has events: only rows send events for now."
          },
          i = "Give {.arg {what}} to the {.fn tip_spec} of {.arg rows}, or leave it out."
        ),
        class = if (what == "pin") "hovercraft_error_pin" else "hovercraft_error_events",
        call = call
      )
    }
  }
  invisible()
}

# The `pin` and `events` of the spec for `rows`, checked. Both belong to the rows and are named after the table's `id`,
# so they need it. A row pins by its handle and the pin key only: its own click selects it in both widgets, so the click
# pin is refused.
tip_table_check_pin <- function(rows, id, call) {
  pin <- rows$pin
  events <- rows$events
  if (identical(pin, "true")) {
    tip_abort(
      c(
        "{.arg pin} is {.val click}, but a click on a table row selects it, so it cannot pin.",
        i = "Give {.code pin = TRUE}: a reader pins a row's tip with the pin handle in its corner or with the pin key."
      ),
      class = "hovercraft_error_pin",
      call = call
    )
  }
  for (arg in c(if (!is.null(pin)) "pin", if (!is.null(events)) "events")) {
    if (is.null(id)) {
      tip_abort(
        c(
          "{.arg {arg}} needs an {.arg id}: the table's inputs are named after it.",
          i = "Give {.fn tip_table} an {.arg id}, such as {.code id = \"scores_tips\"}."
        ),
        class = if (arg == "pin") "hovercraft_error_pin" else "hovercraft_error_events",
        call = call
      )
    }
  }
  invisible()
}

# The table's id as the Shiny inputs know it. Inside a module the plain id is taken as the module's own: the module's
# prefix is added from the session the table is drawn in (what shiny::getDefaultReactiveDomain() gives, also inside
# DT::renderDT() and reactable::renderReactable()), as tip_show() does. An id that already has the prefix is left as it
# is, so `session$ns("x")` still works; outside a session the id is used as given.
tip_table_id <- function(id, domain = tip_session()) {
  if (is.null(id) || is.null(domain) || !is.function(domain$ns)) {
    return(id)
  }
  prefix <- domain$ns("")
  if (!nzchar(prefix) || startsWith(id, prefix)) {
    return(id)
  }
  paste0(prefix, id)
}

# The engine's `open` attribute and its value for a pinned tip: the layer closes a pin whose row it is about to change.
tip_table_open <- function() {
  list(attribute = tip_contract()$attributes$open, pinned = tip_rules()$open$values[[2]])
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
#' placement, marker or `more`) takes a [tip_spec()] in place of words. A tip is made of words (markdown)
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
#' Pins: with `tip_spec(pin = TRUE)` as `rows`, a reader can keep a row's tip open. The tip that shows on hover or
#' focus gets a small pin handle in its corner, the same as [tip_on()]'s (`pin = TRUE`): a click or tap on it pins the
#' row's tip, and the tip's own close button, `Escape` or a click away closes it. The pin key `P` pins the tip of the
#' row that has focus, or of the row a focused cell belongs to; a cell or a header cell has no pin of its own ("only
#' rows pin for now"). A long form (`more`) opens with its "More" button, or with the `M` key when the focus is on the
#' row, a cell or a header cell. A tap on a row shows its tip and a tap on the handle pins it. The handle is not a
#' column and not a tab stop, so the table keeps its one tab stop and its columns; keyboard readers use the key, which
#' each row that can pin announces as `aria-keyshortcuts`. A pin follows its row. It stays on a row when `DT` sorts
#' (client and server mode), and on the same row number when `DT` draws the table again or `DT::replaceData()` or
#' `reactable::updateReactable(data = )` give it new data, with the new values in its tip. It closes when its row
#' leaves the page (a filter or another page in `DT`, another page in `reactable`), when `reactable` shows another row
#' in the row's place, when the table goes, and when a click lands elsewhere. The input then says why: `DT` closes a
#' pin of a row that left the page itself (`reason` `"gone"`, `by` `"data"`); `reactable` keeps its row nodes and gives
#' them other rows, so the layer closes the pin through the tip engine's host command (`reason` `"host"`, `by`
#' `"host"`, `via` `"command"`). A server that acts on `by == "data"` sees the two widgets differently.
#'
#' @section Shiny inputs:
#' `tip_spec(events = )` on `rows` turns on inputs that belong to the table, one per event and not one per row, named
#' after the table's `id`: `input$<id>_hint`, `input$<id>_pin` and `input$<id>_copy`. Each is `NULL` until its first
#' event. Its value is the event that just happened, with the fields of [tip_on()]'s (`state`, `text`, `by`, `via` and
#' `reason`), and says which tip it was:
#'
#' * `target`: `"row"` (`"cell"` and `"header"` are kept for later; only rows send events for now);
#' * `row`: the row's place in the data the widget has now, counted from 1, in client mode and in server mode alike,
#'   and not the row's place on the screen: after a sort, the third data row is still 3. It is `NULL` for a header.
#'   After `DT::replaceData()`, `reactable::updateReactable(data = )` or a re-render with other data, row 3 is whatever
#'   is third now, so do not keep it as a record's id;
#' * `column`: the data column's name (the name in the data frame, not the header's text), or `NULL` for a row;
#' * `id`: the tip's id. How it is written is not part of the API.
#'
#' `input$<id>_pinned` (with `events = "pin"`) is what is pinned now: a list of such addresses (`target`, `row`,
#' `column`, `id`), the oldest pin first. It is `NULL` until the first pin event, and an empty list once nothing is
#' pinned. It is sent in the same batch as `_pin`, and a value equal to the one before does not fire an observer again.
#' Read it with `length(input$scores_pinned)`; a table keeps one pin at a time for now. For a tag of [tip_on()] with
#' `events = "pin"`, `input$<id>_pinned` is `TRUE` or `FALSE` (read it with `isTRUE(input$run_pinned)`).
#'
#' `input$<id>_pin` and `input$<id>_hint` are told apart by `state`, whose values may grow in a later version. A pin's
#' `state` is `"pinned"`, `"closed"` or `"updated"` (the tip is pinned and was drawn again because its words changed:
#' an app that only tests for `"closed"` should not take it for a pin); a hint's is `"shown"` or `"hidden"`. The values
#' of `by` are `"reader"`, `"data"` (the page changed under the tip) and `"host"` (a command of the page did it). The
#' values of `reason` (the same for the inputs of [tip_on()]) are, for a pin, `"select"` (the same element again),
#' `"escape"`, `"button"` (the tip's close button), `"outside"` (a click away), `"replaced"` (another tip was pinned),
#' `"gone"` (the row left the page), `"below"` (a layer around it closed), `"host"` (a command of the page closed it)
#' and `"redraw"` (the tip was drawn again with other host slots, `by = "data"`); for a hint, `"leave"` (the pointer
#' left), `"blur"` (the focus left), `"escape"`, `"outside"`, `"pinned"` (the hint became a pin), `"replaced"`,
#' `"gone"` and `"host"`. `reason` is `NULL` when a tip opens.
#'
#' What works, each checked in a browser:
#' * `DT`, in client mode and in server mode under Shiny, with `scrollX`, `scrollY`, `fixedHeader`, `colReorder` and
#'   `responsive`. With `responsive`, a column folded into the details row has no tip there; the row's tip is
#'   unchanged.
#' * `reactable`, with sorting, filtering, paging, row selection (a click selects the row and its tip stays),
#'   `groupBy` (the rows inside a group have tips, the group's own row has none), `details` (the details panel has
#'   none) and `filterable = TRUE` (the tip of a header cell stays on the header cell; the filter box under it has no
#'   tip and is not described twice). `DT`'s filter row has none either.
#' * An `id` inside a module: give the plain id and the module's prefix is added (see `id`).
#' * Tables that Shiny updates with new data: `DT::replaceData()` (it works in server mode) with the user's own data
#'   frame, and `reactable::updateReactable(data = )`. The tips follow the new rows, as they do after sorting,
#'   filtering and paging.
#'
#' Not supported: a `DT` table with its own `createdRow` or `initComplete` option; a `reactable` column whose
#' `headerClass` is a JavaScript function; any other widget.
#'
#' Large tables: `tip_table()` adds nothing to the data of the page, since each tip reads its values from the table's
#' own data in the browser. A `reactable` row carries the values its tips read in its class, as well as in the engine's
#' attribute, so they sit twice in the page for every row that is drawn: page the table. A table with every row drawn
#' at once is slow in `DT` when cells or headers have tips (a table of 2,000 rows with a row tip and three cell tips
#' took about a second to draw, five times as long as without tips; thousands of rows take seconds); page it, or tip
#' its rows only.
#'
#' Give `tip_table()` the finished widget, last in the pipe or at least after the columns are settled.
#' `DT::datatable()` keeps its columns and its inputs as they are: `tip_table()` adds no column and changes no event
#' (a table with its own `createdRow` or `initComplete` option is refused for now). `reactable::reactable()` gets its
#' own `rowClass`, `class` and `headerClass` added to yours; the class of a row holds the values its tips read, so it
#' changes with the data.
#'
#' @param widget A table made by `DT::datatable()` or by `reactable::reactable()`.
#' @param ... Must be empty. It makes a misspelt argument an error.
#' @param rows One string, [tip_template()] or [tip_spec()] for every row, or `NULL`. `NA` and `""` are no tip. A
#'   [tip_spec()] here may also set `pin` and `events`. A [tip_template()] is given as an object: the name of a template
#'   made with [tip_define()] stops with an error that says so.
#' @param cells A named vector or list, one entry per column (by the column's name; a name given twice is an error), for
#'   the cells of that column. Each entry is a string, a
#'   [tip_template()] or a [tip_spec()]; `NA` and `""` give that column no tip. Use a list to mix them:
#'   `list(status = tip_spec("{{ note }}", placement = "right"), wins = "{{ wins }} wins")`.
#' @param headers The same for the header cells; words only, so a [tip_spec()] here holds words with no placeholders.
#' @param style How every tip in the table looks, or `NULL` (the default) to take the page's: whatever `style` takes in
#'   [tip_on()] (a preset name, a [tip_style()], a [tip_theme()] or a [bslib::bs_theme()]). A [tip_spec()] with its own
#'   `style` beats it for that target. A row's own style also reaches the cells in that row that have none of their
#'   own, because the tip engine reads the nearest tagged element. A list of CSS variables
#'   (`tip_style(theme = list("--x" = ...))`) is for the whole page, so a table refuses it with an error: give it to
#'   [use_hovercraft()], or use the roles of [tip_theme()].
#' @param placement Where the tips show: `"auto"`, `"top"`, `"bottom"`, `"left"` or `"right"`, or `NULL` for the page's
#'   choice. A [tip_spec()] with its own placement beats it. A cell's tip follows this, not its row's.
#' @param marker The visible cue on every cell and header cell that has a tip, so a reader can see where the tips are:
#'   `"corner"` (a small triangle in the top corner, as a spreadsheet shows a note), or `NULL` or `"none"` for no
#'   marker (the default). A [tip_spec()] with its own `marker` beats it. A row takes no marker, and a table draws only
#'   the corner for now: `"underline"`, `"icon"` and an icon of your own stop with an error. A cell with a marker is
#'   highlighted while its tip is open. See [tip_marker()].
#' @param id The id this table's Shiny inputs hang on (`input$<id>_pin`), and the base of its tips' ids: one non-empty
#'   string with no whitespace. It is the table's id, not one tip's. Inside a module, give the plain id: `tip_table()`
#'   adds the module's prefix from the session the table is drawn in, as [tip_show()] does, so `id = "scores"` in
#'   module `"mod1"` is `input[["mod1-scores_pin"]]`. An id that already has the prefix (`session$ns("scores")`) is left
#'   alone, and outside a session (a static page) the id is used as given. Give it a different id from every other tip
#'   source on the page: two tables with one id make the page warn in the browser console. With an `id`, every marked
#'   row, cell and header cell gets its own tip id derived from it, which follows the data row through sorting,
#'   filtering and paging; how that id is built is not part of the API: [tip_show()], [tip_hide()] and [tip_update()]
#'   take the table's `id` with `row` and `column` instead. `NULL` gives the tips no id. A pin or events
#'   given to `rows` (see [tip_spec()]) need an `id`. The engine's kind of the tips is not yours to set.
#' @returns The widget, with the tips added.
#' @family tooltips
#' @examplesIf requireNamespace("DT", quietly = TRUE)
#' scores <- data.frame(team = c("Owls", "Foxes"), wins = c(7, 5), losses = c(3, 5))
#' DT::datatable(scores) |>
#'   tip_table(
#'     rows = tip_spec(
#'       "**{{ team }}**: {{ wins | number:0 }} wins, {{ losses | number:0 }} losses",
#'       pin = TRUE,
#'       events = "pin"
#'     ),
#'     headers = c(wins = "Games won this season"),
#'     id = "scores_tips"
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
  moved <- intersect(...names(), c("pin", "events"))
  if (length(moved)) {
    tip_abort(
      c(
        "{.arg {moved}} moved: {.fn tip_table} no longer takes it.",
        i = "Give it to the {.fn tip_spec} of {.arg rows}: {.code rows = tip_spec(\"...\", {moved[[1]]} = ...)}."
      ),
      class = "hovercraft_error_argument",
      call = call
    )
  }
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
  id <- tip_table_id(id)
  if (inherits(rows, "hovercraft_spec")) {
    tip_table_check_pin(rows, id, call)
  }
  style <- tip_as_style(style, call = call)
  tip_check_style_tokens(style, call)
  marker <- if (!is.null(marker)) tip_as_marker(marker, call = call)
  data <- tip_table_data(widget, kind)
  columns <- names(data)
  # A column's position counts the data columns; the row names of a reactable (".rownames") are not one.
  shown <- setdiff(columns, ".rownames")
  if (!is.null(id)) {
    tip_table_register(id, shown)
  }
  plan <- list(
    rows = if (!is.null(rows)) {
      tip_table_content(
        rows,
        "rows",
        columns,
        placement = placement,
        call = call,
        row = TRUE,
        style = style
      )
    },
    cells = tip_table_named(cells, "cells", columns, FALSE, placement, call, marker, style),
    headers = tip_table_named(headers, "headers", columns, TRUE, placement, call, marker, style)
  )
  if (is.null(plan$rows) && inherits(rows, "hovercraft_spec") && !(is.null(rows$pin) && is.null(rows$events))) {
    tip_abort(
      c(
        "{.arg rows} has no words, so there is no row tip to pin or to send events.",
        i = "Give the {.fn tip_spec} some words, or leave {.arg pin} and {.arg events} out."
      ),
      class = if (is.null(rows$pin)) "hovercraft_error_events" else "hovercraft_error_pin",
      call = call
    )
  }
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
  column_of <- function(column) match(column, shown)
  plan$specs <- list(
    rows = if (!is.null(plan$rows)) tip_table_spec(plan$rows, "row", id = id),
    cells = Map(
      function(content, column) tip_table_spec(content, "cell", column_of(column), id, column),
      plan$cells,
      names(plan$cells)
    ),
    headers = Map(
      function(content, column) tip_table_spec(content, "header", column_of(column), id, column),
      plan$headers,
      names(plan$headers)
    )
  )
  plan$table_id <- if (!is.null(id)) list(id = id)
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
  tip_table_dependencies(
    widget,
    marked,
    looks[!duplicated(vapply(looks, function(dep) dep$name, ""))],
    pin = !is.null(plan$rows$pin),
    more = any(vapply(
      c(list(plan$rows), plan$cells, plan$headers),
      function(content) identical(content$more_on, "button"),
      NA
    ))
  )
}
