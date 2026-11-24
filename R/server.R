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

# `row` and `column` of a server call as the table's tip id, or the plain id for a tag. `row` is the 1-based row of the
# data the widget has now; `column` the data column's name or its 1-based position, without the row-names column.
tip_server_id <- function(id, row, column, session, call) {
  if (is.null(row) && is.null(column)) {
    return(session$ns(id))
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
  table <- tip_table_id(id, session)
  known <- tip_session_tables(session = session)[[table]]
  position <- NULL
  if (!is.null(column)) {
    by_name <- is.character(column) && length(column) == 1L && !is.na(column)
    if (!by_name) {
      tip_arg_count(column, "column")
    }
    listed <- if (is.null(known)) "none known yet" else paste0("\"", known, "\"", collapse = ", ")
    if (by_name && is.null(known)) {
      tip_check_table_error(
        c(
          "{.arg column} names a column, and no table with the id {.val {id}} has been drawn in this session.",
          i = paste(
            "A column's name is read from the table that {.fn tip_table} drew.",
            "Call this after the table is drawn, or give a position."
          )
        ),
        call = call
      )
    }
    position <- if (by_name) match(column, known) else column
    if (is.na(position) || (!is.null(known) && position > length(known))) {
      tip_check_table_error(
        c(
          "{.arg column} is not a column of the table {.val {id}}: {.val {column}}.",
          i = paste0("The data columns are ", listed, ". A position counts these, without the row names."),
          i = "A column is the data's own, not the header's words."
        ),
        call = call
      )
    }
    position <- as.integer(position)
  }
  tip_table_tip_id(table, if (!is.null(row)) as.integer(row), position)
}

# Sends one message. Called in a reactive flush, such as an observer, it waits until the flush is done: the message then
# reaches the page after the outputs of that flush, so a tag that `renderUI()` draws in the same flush is there to be
# found. Called anywhere else (a callback of `later`, outside a reactive flush) no flush follows and the message goes at
# once.
tip_send <- function(session, what, message) {
  send <- function() session$sendCustomMessage(tip_message(what), message)
  if (is.function(session$onFlushed) && tip_in_reactive_flush()) {
    session$onFlushed(send, once = TRUE)
  } else {
    send()
  }
  invisible()
}

# Whether the code runs inside a reactive context, which Shiny only gives to code that runs in a flush (an observer, a
# reactive, an output): reading a reactive value outside one is an error.
tip_in_reactive_flush <- function() {
  values <- shiny::reactiveValues(probe = NULL)
  tryCatch(
    {
      values$probe
      TRUE
    },
    error = function(e) FALSE
  )
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
#' table has to have been drawn once; a name that is not a column stops with an error that lists the columns. A column
#' that the widget hides has no tip. A row that a server-side `DT` table does not show on its current page has no
#' element yet either, so nothing happens until it is on the page.
#'
#' A call reaches only a tag that is on the page and drawn at that moment; nothing is queued. When the call runs in a
#' reactive flush, such as an observer, it is sent when the flush is over, so `tip_show()` in the same observer as a
#' `renderUI()` that draws the tag reaches the new tag. Anywhere else, such as a callback of `later`, no flush follows
#' and the call is sent at once.
#'
#' A tip opened by `tip_show()` stays open until `tip_hide()` closes it, or until the reader opens another tip, presses
#' Escape or clicks away. `tip_hide()` closes a hint, the tip that shows on hover or focus, and never a pin: when the
#' tip of this id is not the hint that is open (another tip's hint is, or this one is pinned or closed), it does
#' nothing. No server call pins a tip or closes a pin.
#'
#' `tip_update()` rewrites the tag's tip attributes. It takes the same values as [tip_on()] and changes only what is
#' named: `NULL` leaves a value as it is, `NA` takes it away, and for `text`, `""` clears the words. An open tip draws
#' its new words, template or record at once; a new `placement` is used the next time the tip opens. A `record` needs
#' the `template` in the same call, so that only the columns the template names are sent to the page; to change the
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
#'   without the module's namespace. One non-empty string with no whitespace.
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
  tip_send(session, "show", list(id = tip_server_id(id, row, column, session, call)))
  invisible(NULL)
}

#' @rdname tip_server
#' @export
tip_hide <- function(id, ..., row = NULL, column = NULL, session = shiny::getDefaultReactiveDomain()) {
  call <- rlang::current_env()
  tip_check_dots(..., args = c("id", "row", "column", "session"))
  tip_check_id(id, null_ok = FALSE)
  session <- tip_check_session(session, "tip_hide")
  tip_send(session, "hide", list(id = tip_server_id(id, row, column, session, call)))
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
  tip_arg_string(template, "template", null_ok = TRUE, na_ok = TRUE)
  if (!"placement" %in% removed) {
    tip_check_placement(placement)
  }
  if ("record" %in% given && !"record" %in% removed && (is.null(template) || "template" %in% removed)) {
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
  tip_id <- tip_server_id(id, row, column, session, call)
  attrs <- tip_attributes()
  ask <- c(
    text = attrs[["text"]],
    template = attrs[["template"]],
    record = attrs[["record"]],
    placement = tip_attribute("placement")
  )
  set <- list()
  for (name in setdiff(given, removed)) {
    value <- values[[name]]
    set[[ask[[name]]]] <- if (identical(name, "record")) {
      tip_json(tip_record(value, tip_known(session)[[template]]$fields))
    } else {
      value
    }
  }
  # A template given without a record is checked for its name only: the tag may already hold the record it reads.
  if (!is.null(template) && !"template" %in% removed) {
    if (is.null(record)) {
      tip_template_seen(template, rlang::current_env(), session)
    } else {
      tip_check_use(template, names(record), id, rlang::current_env(), session)
    }
  }
  tip_send(session, "update", list(id = tip_id, set = set, remove = as.list(unname(ask[removed]))))
  invisible(NULL)
}
