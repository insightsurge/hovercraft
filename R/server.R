# Server calls and the hint input: hovercraft's own layer for Shiny, on top of the tip engine. A server call sends one
# custom message; inst/runtime/shiny.js finds the element by its tip id and uses only the engine's public doors
# (`Hovercraft.show()` and `Hovercraft.hide()`) and its public attributes. A tag asks for an input with the attribute
# this file writes (`events =` on tip_on()); the same file names the message, so R and the script agree.

# The events a tag can ask for: "hint" sends `input$<id>_hint`, "pin" sends `input$<id>_tip` and "copy" sends
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
      open = tip_contract()$attributes$open,
      events = tip_shiny_attribute(),
      more = tip_attribute("more"),
      moreTemplate = tip_attribute("more_template")
    ),
    global = tip_names()$global,
    css = tip_names()$css,
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

#' Show, hide and change a tip from the server
#'
#' `r lifecycle::badge("experimental")`
#' `tip_show()` opens the tip of a tag, `tip_hide()` closes it, and `tip_update()` changes what the tip says, from a
#' Shiny server function. The tag is found by the `id` given to [tip_on()], or, when [tip_on()] had none, by the tag's
#' own `id`. The ids of the rows, cells and header cells of a [tip_table()] work too: they are written
#' `<id>/row/<n>`, `<id>/cell/<n>/<column>` and `<id>/header/<column>`, where `<n>` is the data row.
#'
#' A tip opened by `tip_show()` stays open until `tip_hide()` closes it, or until the reader opens another tip, presses
#' Escape or clicks away. `tip_hide()` closes a hint, the tip that shows on hover or focus, and never a pin: when the
#' tip of this id is not the hint that is open (another tip's hint is, or this one is pinned or closed), it does
#' nothing.
#'
#' `tip_update()` rewrites the tag's tip attributes. It takes the same values as [tip_on()] and changes only what is
#' named: `text`, `template`, `record` or `placement`. `NULL` takes that value away, and so do `""` and `NA` for `text`,
#' as in [tip_on()]. An open tip draws its new words, template or record at once; a new `placement` is used the next
#' time the tip opens. A `record` needs the `template` in the same call, so that only the columns the template names
#' are sent to the page; to change the template alone, leave `record` out and the tag keeps the record it has.
#' `tip_update()` leaves a `style` or `marker` alone, and it leaves the tag's `aria-label` as it is, so a button named
#' from its tip keeps the old name; give such a button its own `aria-label`. The new tip lasts until the tag is drawn
#' again, for example by `renderUI()`.
#'
#' Inside a module, give the id as the module's own (`"save"`); the call adds the module's namespace, as the `update*()`
#' functions of Shiny do. In the module's UI, give [tip_on()] the id as `ns("save")`.
#'
#' @param id The tip's id: the `id` given to [tip_on()], without the module's namespace. One non-empty string with no
#'   whitespace.
#' @param ... For `tip_update()`: the new values, all named, and at least one: `text`, `template`, `record` and
#'   `placement`, as in [tip_on()].
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
tip_show <- function(id, session = shiny::getDefaultReactiveDomain()) {
  tip_check_id(id, null_ok = FALSE)
  session <- tip_check_session(session, "tip_show")
  session$sendCustomMessage(tip_message("show"), list(id = session$ns(id)))
  invisible(NULL)
}

#' @rdname tip_server
#' @export
tip_hide <- function(id, session = shiny::getDefaultReactiveDomain()) {
  tip_check_id(id, null_ok = FALSE)
  session <- tip_check_session(session, "tip_hide")
  session$sendCustomMessage(tip_message("hide"), list(id = session$ns(id)))
  invisible(NULL)
}

#' @rdname tip_server
#' @export
tip_update <- function(id, ..., session = shiny::getDefaultReactiveDomain()) {
  tip_check_id(id, null_ok = FALSE)
  values <- list(...)
  names_given <- names(values) %||% rep("", length(values))
  known <- c("text", "template", "record", "placement")
  if (!length(values) || any(!nzchar(names_given)) || !all(names_given %in% known) || anyDuplicated(names_given)) {
    tip_abort(
      c(
        "{.arg ...} must hold new values for the tip, each with its name, at least one.",
        x = if (length(values)) "Not usable: {.val {setdiff(names_given, known)}}." else "Nothing was given.",
        i = "The names are {.arg {known}}."
      ),
      class = "hovercraft_error_argument"
    )
  }
  # As tip_on() reads them: "" and NA are no text, so the tag takes no text from this call either.
  tip_arg_string(values$text, "text", min_chars = 0L, null_ok = TRUE, na_ok = TRUE)
  if ("text" %in% names_given && (is.null(values$text) || is.na(values$text) || !nzchar(values$text))) {
    values["text"] <- list(NULL)
  }
  tip_arg_string(values$template, "template", null_ok = TRUE)
  tip_check_placement(values$placement)
  if (!is.null(values$record) && is.null(values$template)) {
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
  attrs <- tip_attributes()
  ask <- c(text = attrs[["text"]], template = attrs[["template"]], record = attrs[["record"]])
  ask[["placement"]] <- tip_attribute("placement")
  set <- list()
  remove <- character()
  for (name in names_given) {
    value <- values[[name]]
    if (is.null(value)) {
      remove <- c(remove, ask[[name]])
    } else if (identical(name, "record")) {
      set[[ask[[name]]]] <- tip_json(tip_record(value, tip_known()[[values$template]]$fields))
    } else {
      set[[ask[[name]]]] <- value
    }
  }
  # A template given without a record is checked for its name only: the tag may already hold the record it reads.
  if (!is.null(values$template)) {
    if (is.null(values$record)) {
      tip_template_seen(values$template, rlang::current_env())
    } else {
      tip_check_use(values$template, names(values$record), id, rlang::current_env())
    }
  }
  session$sendCustomMessage(
    tip_message("update"),
    list(id = session$ns(id), set = set, remove = as.list(unname(remove)))
  )
  invisible(NULL)
}
