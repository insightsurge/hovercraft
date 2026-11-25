# Server calls and the hint input: hovercraft's own layer for Shiny, on top of the tip engine. A server call sends one
# custom message; inst/runtime/shiny.js finds the element by its tip id and uses only the engine's public doors
# (`Hovercraft.show()` and `Hovercraft.hide()`) and its public attributes. A tag asks for an input with the attribute
# this file writes (`events =` on tip_on()); the same file names the message, so R and the script agree.

# The events a tag can ask for: "hint" sends `input$<id>_hint`, "pin" sends `input$<id>_pin` and "copy" sends
# `input$<id>_copy`.
tip_event_names <- function() c("hint", "pin", "copy")

# The attribute that lists the inputs a tag sends, named after the engine's own prefix like the marker's.
tip_shiny_attribute <- function() paste0("data-", tip_names()$prefix, "-shiny-events")

# The name of the custom message a server call sends; inst/runtime/shiny.js listens on the same names.
tip_message <- function(what) paste0(tip_names()$prefix, "-tip-", what)

# What the runtime script (inst/runtime/runtime.js) and the Shiny layer (inst/runtime/shiny.js) are told, as one JSON
# object in a `<meta>` of the runtime dependency: the names they use. It is data, so a page whose policy allows no
# inline script runs the same files.
tip_runtime_config <- function() {
  list(
    attributes = list(
      id = tip_attribute("id"),
      pins = tip_contract()$attributes$pins,
      pin = tip_attribute("pin"),
      keysOff = tip_keys_off_attribute(),
      placement = tip_attribute("placement"),
      disabled = tip_disabled_attribute(),
      open = tip_contract()$attributes$open,
      events = tip_shiny_attribute(),
      address = tip_table_address_attribute(),
      kind = tip_attribute("kind"),
      more = tip_attribute("more"),
      moreTemplate = tip_attribute("more_template")
    ),
    global = tip_names()$global,
    css = tip_names()$css,
    pinnedType = tip_pinned_input_type(),
    parkClass = tip_output_park_class(),
    statuses = as.list(tip_statuses),
    # The engine's `open` attribute holds the kind of tip that is open; engine.json lists "hint" first, then "pin".
    states = list(hint = tip_rules()$open$values[[1]], pin = tip_rules()$open$values[[2]]),
    events = list(
      hint = tip_contract()$events$hint$name,
      tip = tip_contract()$events$tip$name,
      copy = tip_contract()$events$copy$name
    ),
    messages = list(show = tip_message("show"), hide = tip_message("hide"), update = tip_message("update"))
  )
}

# The type of the input `_pinned` of a table: Shiny turns an unnamed JSON array into one flat vector (a list of
# addresses would lose its fields, an empty one would be NULL), so the page names a type and this handler gives the
# list back as it was sent: one named list per pinned tip, oldest first, and `list()` once nothing is pinned.
tip_pinned_input_type <- function() paste0(tip_names()$prefix, ".pinned")

# What the handler gives Shiny for `_pinned`: the list as it was sent, one named list per pinned tip.
tip_pinned_value <- function(value, session, name) as.list(value)

# Registers the handler. It is registered when the package loads, or when Shiny does if it comes later (`.onLoad`), so a
# table built in another process or read from a cache still finds it; `force` because a package that is loaded again
# (pkgload) registers it again.
tip_register_pinned_input <- function() {
  shiny::registerInputHandler(tip_pinned_input_type(), tip_pinned_value, force = TRUE)
  invisible()
}

# `events` as the attribute value, or NULL: a subset of "hint", "pin" and "copy".
tip_check_events <- function(events, call = rlang::caller_env()) {
  if (is.null(events)) {
    return(NULL)
  }
  all <- tip_event_names()
  tip_arg(
    checkmate::check_subset(events, all, empty.ok = FALSE),
    "events",
    paste0("Give one or more of ", paste0("\"", all, "\"", collapse = ", "), ", or {.code NULL}."),
    call
  )
  paste(unique(events), collapse = " ")
}

# The pin and copy inputs belong to a pinned tip: without a pin there is nothing to send.
tip_check_events_pin <- function(events, pin_value, call = rlang::caller_env()) {
  if (is.null(events) || !is.null(pin_value)) {
    return(invisible())
  }
  asked <- intersect(strsplit(events, " ", fixed = TRUE)[[1]], c("pin", "copy"))
  if (length(asked)) {
    tip_abort(
      c(
        "{.arg events} asks for {.val {asked}}, and the tip has no pin.",
        i = "Give {.arg pin} as {.code TRUE} or {.code \"click\"}, or ask only for {.val hint}."
      ),
      class = "hovercraft_error_events",
      call = call
    )
  }
  invisible()
}

# A server call needs a Shiny session: the one given, or the one the call runs in. Outside a server function there is
# none, and the message says so instead of failing deep inside Shiny.
tip_check_session <- function(session, fn, call = rlang::caller_env()) {
  if (is.null(session) || !is.function(session$sendCustomMessage) || !is.function(session$ns)) {
    tip_abort(
      c(
        "{.fn {fn}} needs a Shiny session, and there is none.",
        i = "Call it from a server function, an observer or a reactive; inside a module, from the module's server."
      ),
      class = "hovercraft_error_session",
      call = call
    )
  }
  session
}

# The columns of the tables this session has drawn with `tip_table(id = )`, by the table's id, so a server call can
# turn a column name into the position the tip's id holds. They are the session's own, as the templates are.
tip_session_tables <- function(create = FALSE, session = tip_session()) {
  if (is.null(session) || !is.environment(session$userData)) {
    return(NULL)
  }
  key <- "hovercraft_tables"
  if (is.null(session$userData[[key]]) && create) {
    session$userData[[key]] <- new.env(parent = emptyenv())
  }
  session$userData[[key]]
}

# Keeps the data columns of a table, under its id as the Shiny inputs know it. Outside a session nothing is kept.
tip_table_register <- function(id, columns, session = tip_session()) {
  own <- tip_session_tables(create = TRUE, session = session)
  if (!is.null(own)) {
    own[[id]] <- columns
  }
  invisible()
}

# The tip id of a table's tip, as inst/table/table.js writes it: `<table>/row/<n>`, `<table>/cell/<n>/<column>` and
# `<table>/header/<column>`, with `<n>` the 1-based data row and `<column>` the position of the data column. The
# string is this layer's own and not API: the server calls take `row` and `column`.
tip_table_tip_id <- function(table, row, position) {
  if (is.null(row)) {
    paste0(table, "/header/", position)
  } else if (is.null(position)) {
    paste0(table, "/row/", row)
  } else {
    paste0(table, "/cell/", row, "/", position)
  }
}

tip_check_table_error <- function(message, ..., call, .envir = parent.frame()) {
  tip_abort(message, class = "hovercraft_error_table", ..., call = call, .envir = .envir)
}

# What a server call addresses, checked at the call: a tag by its id, or a table's tip by the table, `row` and `column`.
# `row` is the 1-based row of the data the widget has now; `column` the data column's name or its 1-based position,
# without the row-names column. A name is turned into a position from the columns `tip_table()` kept for the session:
# when the table is not known yet (it is drawn after this call, in the same flush or later) the name waits and is
# resolved when the message is sent. Everything that can be wrong is stopped here, in the caller's own call.
tip_server_target <- function(id, row, column, session, call) {
  if (is.null(row) && is.null(column)) {
    return(list(tag = session$ns(id)))
  }
  tip_arg_count <- function(x, arg) {
    ok <- checkmate::test_count(x, positive = TRUE) && length(x) == 1L
    tip_arg(
      if (ok) TRUE else "Must be one whole number from 1.",
      arg,
      "Give the row's position in the data the table has now, counting from 1.",
      call
    )
  }
  if (!is.null(row)) {
    tip_arg_count(row, "row")
  }
  target <- list(table = tip_table_id(id, session), id = id, row = if (!is.null(row)) as.integer(row))
  if (is.null(column)) {
    return(target)
  }
  by_name <- is.character(column) && length(column) == 1L && !is.na(column)
  if (!by_name) {
    tip_arg_count(column, "column")
  }
  known <- tip_session_tables(session = session)[[target$table]]
  if (by_name && is.null(known)) {
    target$name <- column
    return(target)
  }
  position <- if (by_name) match(column, known) else column
  if (is.na(position) || (!is.null(known) && position > length(known))) {
    tip_check_table_error(
      c(
        "{.arg column} is not a column of the table {.val {id}}: {.val {column}}.",
        i = paste0("The data columns are ", paste0("\"", known, "\"", collapse = ", "), ". A position counts these."),
        i = "A column is the data's own, not the header's words, and the row names are not one."
      ),
      call = call
    )
  }
  target$position <- as.integer(position)
  target
}

# The tip id of a target, when the message is sent. A tag, or a position, is known; a column name that waited for its
# table is read now, from the columns the table has kept since. A name that is still not a column of a table is a
# warning and no message: this runs in a callback of the flush, where an error would end the R process.
tip_server_id <- function(target, session) {
  if (!is.null(target$tag)) {
    return(target$tag)
  }
  position <- target$position
  if (is.null(position) && !is.null(target$name)) {
    known <- tip_session_tables(session = session)[[target$table]]
    position <- if (is.null(known)) NA_integer_ else match(target$name, known)
    if (is.na(position)) {
      tip_warn(
        c(
          "{.arg column} {.val {target$name}} is not a column of a table {.val {target$id}} that has been drawn.",
          i = "The call was not sent. A column's name is read from the table that {.fn tip_table} drew.",
          i = "Call this after the table is drawn, or give the column's position."
        ),
        class = "hovercraft_warning_table"
      )
      return(NULL)
    }
  }
  tip_table_tip_id(target$table, target$row, position)
}

# Sends one message after the flush. The message is built from `build()` when it is sent, so the call itself has already
# been checked and only reads what the flush has drawn since. Every call waits for a flush and asks for one, so it also
# reaches the page from a callback where none is due (a `later` callback, `isolate()` outside a flush); a tag that
# `renderUI()` draws in the flush of the call is there by then. A session that cannot say when it flushes gets the
# message at once.
tip_send <- function(session, what, build) {
  send <- function() {
    message <- build()
    if (!is.null(message)) {
      session$sendCustomMessage(tip_message(what), message)
    }
  }
  if (is.function(session$onFlushed)) {
    session$onFlushed(send, once = TRUE)
    # A real session takes the request; a mock one (shiny::testServer()) says it is for internal use only, and a flush
    # is not due there anyway.
    if (is.function(session$requestFlush)) {
      suppressWarnings(tryCatch(session$requestFlush(), error = function(e) NULL))
    }
  } else {
    send()
  }
  invisible()
}

#' Show, hide and change a tip from the server
#'
#' `r lifecycle::badge("experimental")`
#' `tip_show()` opens the tip of a tag, `tip_hide()` closes it, and `tip_update()` changes what the tip says, from a
#' Shiny server function. The tag is found by the `id` given to [tip_on()], or, when [tip_on()] had none, by the tag's
#' own `id`. For a tip of a [tip_table()] give the table's `id` and the address of the tip: `row` alone is a row, `row`
#' and `column` a cell, and `column` alone a header cell. For a tip on a tag, leave both out.
#'
#' `row` is the position of the row in the data the table has now, counting from 1: after `replaceData()` or a redraw
#' with other data, row 3 is whatever is third. It is not a record's id, so do not keep it to find a record later.
#' `column` is the data column, by its name in the data frame (not the header's words) or its position among the data
#' columns, without the row-names column. A name is read from the table that [tip_table()] drew in this session, so the
#' table has to have been drawn: a name that is not a column of a table already drawn stops with an error that lists the
#' columns; a name given before the table's first draw is read when the call is sent, and if it is still not a column
#' then, the call warns (class `hovercraft_warning_table`) and sends nothing. A position needs no table. A column that
#' the widget hides has no tip. A row that is not on the table's current page has no element yet, in any paged table
#' (`DT` in client or server mode, `reactable`), so nothing happens until it is on the page.
#'
#' A call reaches only a tag that is on the page and drawn at that moment; nothing is queued, and a tag in a hidden tab
#' opens nothing. On a page without Shiny (R Markdown, Quarto, a static page) the calls stop with an error. Every call
#' is sent when the flush that follows it is done, and asks for that flush, so `tip_show()` in the same observer as a
#' `renderUI()` that draws the tag reaches the new tag, and a call from a callback of `later` is sent too.
#'
#' A tip opened by `tip_show()` stays open until `tip_hide()` closes it, or until the reader opens another tip, presses
#' Escape or clicks away. `tip_hide()` closes a hint, the tip that shows on hover or focus, and never a pin: when the
#' tip of this id is not the hint that is open (another tip's hint is, or this one is pinned or closed), it does
#' nothing. No server call pins a tip or closes a pin.
#'
#' `tip_update()` rewrites the tag's tip attributes. It takes the values of [tip_on()] that change a tip (`text`,
#' `template`, `record` and `placement`) and changes only what is named: `NULL` leaves a value as it is, `NA` takes it
#' away, and for `text`, `""` clears the words. `template` is the name of a template or a [tip_template()], which is
#' sent with the call (not one that holds a [tip_output()], which needs the tag's own [tip_on()]); `text` with
#' `{{ field }}` placeholders is filled from `record`, as in [tip_on()]. An open tip draws its new words, template or
#' record at once; a new `placement` is used the next time the tip opens. A `record` needs a `template`, or `text`
#' with placeholders, in the same call, so that only the columns it names are sent to the page; to change the
#' template alone, leave `record` out and the tag keeps the record it has. `tip_update()` leaves a `style` or `marker`
#' alone, and it leaves the tag's `aria-label` as it is, so a button named from its tip keeps the old name; give such a
#' button its own `aria-label`. The new tip lasts until the tag is drawn again, for example by `renderUI()`.
#'
#' On a control that has `when_disabled`, `tip_update()` changes the control's own tip, also while the control is
#' disabled. A disabled control still says only "Not available:" and its reason, in the new `placement`; the new `text`,
#' `template` and `record` show once the control is enabled.
#'
#' Inside a module, give the id as the module's own (`"save"`); the call adds the module's namespace, as the `update*()`
#' functions of Shiny do. In the module's UI, give [tip_on()] the id as `ns("save")`.
#'
#' @param id The tip's id: the `id` given to [tip_on()], or, with `row` or `column`, the `id` given to [tip_table()],
#'   without the module's namespace. One non-empty string with no whitespace and no `:`.
#' @param text,template,record,placement For `tip_update()`: the new values, as in [tip_on()]. `NULL` (the default)
#'   leaves a value as it is and `NA` takes it away; `""` clears the `text`. Give at least one.
#' @param ... Must be empty. It makes a misspelt argument an error.
#' @param row,column The address of a table's tip: `row` the row's position in the data, `column` a data column's name
#'   or position. Both `NULL` (the default) for a tip on a tag.
#' @param session The Shiny session; the default is the one the call runs in.
#'
#' @returns `NULL`, invisibly. These functions are called for their effect on the page.
#' @family tooltips
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'   ui <- shiny::fluidPage(
#'     tip_on(shiny::actionButton("run", "Run"), "Runs the **check**", id = "run_tip"),
#'     shiny::actionButton("show", "Show the tip"),
#'     shiny::actionButton("hide", "Hide it"),
#'     shiny::actionButton("change", "Change it")
#'   )
#'   server <- function(input, output, session) {
#'     shiny::observeEvent(input$show, tip_show("run_tip"))
#'     shiny::observeEvent(input$hide, tip_hide("run_tip"))
#'     shiny::observeEvent(input$change, tip_update("run_tip", text = "Runs the **new check**"))
#'   }
#'   shiny::shinyApp(ui, server)
#' }
#' @name tip_server
NULL

#' @rdname tip_server
#' @export
tip_show <- function(id, ..., row = NULL, column = NULL, session = shiny::getDefaultReactiveDomain()) {
  call <- rlang::current_env()
  tip_check_dots(..., args = c("id", "row", "column", "session"))
  tip_check_id(id, null_ok = FALSE)
  session <- tip_check_session(session, "tip_show")
  target <- tip_server_target(id, row, column, session, call)
  tip_send(session, "show", function() {
    id <- tip_server_id(target, session)
    if (!is.null(id)) list(id = id)
  })
  invisible(NULL)
}

#' @rdname tip_server
#' @export
tip_hide <- function(id, ..., row = NULL, column = NULL, session = shiny::getDefaultReactiveDomain()) {
  call <- rlang::current_env()
  tip_check_dots(..., args = c("id", "row", "column", "session"))
  tip_check_id(id, null_ok = FALSE)
  session <- tip_check_session(session, "tip_hide")
  target <- tip_server_target(id, row, column, session, call)
  tip_send(session, "hide", function() {
    id <- tip_server_id(target, session)
    if (!is.null(id)) list(id = id)
  })
  invisible(NULL)
}

#' @rdname tip_server
#' @export
tip_update <- function(
  id,
  text = NULL,
  ...,
  template = NULL,
  record = NULL,
  placement = NULL,
  row = NULL,
  column = NULL,
  session = shiny::getDefaultReactiveDomain()
) {
  call <- rlang::current_env()
  tip_check_dots(
    ...,
    args = c("id", "text", "template", "record", "placement", "row", "column", "session")
  )
  tip_check_id(id, null_ok = FALSE)
  values <- list(text = text, template = template, record = record, placement = placement)
  # NULL leaves a value alone; NA takes it away, and for the text "" clears the words.
  given <- names(values)[!vapply(values, is.null, NA)]
  removed <- given[vapply(values[given], function(x) is.atomic(x) && length(x) == 1L && is.na(x), NA)]
  if (!length(given)) {
    tip_abort(
      c(
        "{.fn tip_update} was given nothing to change.",
        i = "Give at least one of {.arg text}, {.arg template}, {.arg record} and {.arg placement}.",
        i = "{.code NULL} leaves a value as it is, {.code NA} takes it away."
      ),
      class = "hovercraft_error_argument"
    )
  }
  tip_arg_string(text, "text", min_chars = 0L, null_ok = TRUE, na_ok = TRUE)
  # What tip_on() takes: a template's name, or a tip_template() made here, which travels with the message; and words
  # with {{ field }} placeholders when a record is given, which are made into a template of their own.
  inline <- NULL
  if (inherits(template, "hovercraft_template")) {
    inline <- list(template = template, what = "The {.arg template}")
  } else {
    tip_arg_string(template, "template", null_ok = TRUE, na_ok = TRUE)
    holds_record <- "record" %in% given && !"record" %in% removed
    has_words <- is.character(text) && length(text) == 1L && !is.na(text) && nzchar(text)
    if (holds_record && is.null(template) && has_words && length(tip_check_text(text, "The words of `text`"))) {
      inline <- list(template = tip_template(tip_text(text)), what = "{.arg text}")
    }
  }
  if (!is.null(inline) && length(tip_output_ids(list(inline$template)))) {
    tip_abort(
      c(
        "A template with {.fn tip_output} cannot be sent from the server.",
        i = "The output's place is made when the tag is drawn: give the template to {.fn tip_on}."
      ),
      class = "hovercraft_error_template"
    )
  }
  if (!"placement" %in% removed) {
    tip_check_placement(placement)
  }
  if (
    is.null(inline) &&
      "record" %in% given &&
      !"record" %in% removed &&
      (is.null(template) || "template" %in% removed)
  ) {
    tip_abort(
      c(
        "{.arg record} needs {.arg template} in the same call.",
        i = "Give the template's name too, so only the columns it names are sent to the page.",
        i = "To change the words only, give {.arg text}."
      ),
      class = "hovercraft_error_record"
    )
  }
  session <- tip_check_session(session, "tip_update")
  target <- tip_server_target(id, row, column, session, call)
  attrs <- tip_attributes()
  ask <- c(
    text = attrs[["text"]],
    template = attrs[["template"]],
    record = attrs[["record"]],
    placement = tip_attribute("placement")
  )
  set <- list()
  define <- list()
  fields <- NULL
  if (!is.null(inline)) {
    body <- tip_blocks_plain(inline$template)
    name <- paste0("inline_", substr(rlang::hash(tip_json(body)), 1L, 12L))
    define <- list(list(name = name, blocks = body))
    fields <- unique(tip_template_fields(inline$template))
    values$template <- name
    # The words became the template: the text attribute would win over it, so it goes.
    if (is.null(template)) {
      removed <- union(removed, "text")
    }
    if (!is.null(record)) {
      tip_check_more_fields(fields, names(record), id, call, what = inline$what)
    }
  }
  for (name in setdiff(union(given, if (!is.null(inline)) "template"), removed)) {
    value <- values[[name]]
    set[[ask[[name]]]] <- if (identical(name, "record")) {
      tip_json(tip_record(value, fields %||% tip_known(session)[[template]]$fields))
    } else {
      value
    }
  }
  # A template given without a record is checked for its name only: the tag may already hold the record it reads.
  if (is.null(inline) && !is.null(template) && !"template" %in% removed) {
    if (is.null(record)) {
      tip_template_seen(template, rlang::current_env(), session)
    } else {
      tip_check_use(template, names(record), id, rlang::current_env(), session)
    }
  }
  tip_send(session, "update", function() {
    id <- tip_server_id(target, session)
    if (!is.null(id)) list(id = id, set = set, remove = as.list(unname(ask[removed])), define = define)
  })
  invisible(NULL)
}
