skip_if_not_installed("withr")

# A stand-in for a Shiny session: it keeps what a server call sends, and adds a module's namespace like `session$ns`.
fake_session <- function(prefix = "") {
  sent <- list()
  list(
    sendCustomMessage = function(type, message) sent[[length(sent) + 1L]] <<- list(type = type, message = message),
    ns = function(id) if (nzchar(prefix)) paste0(prefix, "-", id) else id,
    sent = function() sent
  )
}
button <- function() htmltools::tags$button("Run", id = "run")

describe("tip_show() and tip_hide()", {
  it("send one message each, with the id", {
    session <- fake_session()
    expect_null(tip_show("run_tip", session = session))
    expect_null(tip_hide("run_tip", session = session))
    got <- session$sent()
    expect_identical(vapply(got, function(x) x$type, ""), paste0(tip_names()$prefix, c("-tip-show", "-tip-hide")))
    expect_identical(got[[1]]$message, list(id = "run_tip"))
    expect_identical(got[[2]]$message, list(id = "run_tip"))
  })

  it("add the module's namespace to the id", {
    session <- fake_session("mod")
    tip_show("run_tip", session = session)
    expect_identical(session$sent()[[1]]$message$id, "mod-run_tip")
  })

  it("stop with a classed error for an id that is not one string without spaces", {
    session <- fake_session()
    expect_error(tip_show(NULL, session = session), class = "hovercraft_error_argument")
    expect_error(tip_hide("two words", session = session), class = "hovercraft_error_argument")
    expect_length(session$sent(), 0L)
  })

  it("stop with a classed error and the fix when there is no session", {
    expect_error(tip_show("run_tip", session = NULL), class = "hovercraft_error_session")
    expect_error(tip_hide("run_tip", session = NULL), "server function")
    # Outside a server function, the default session is none.
    skip_if_not_installed("shiny")
    expect_error(tip_show("run_tip"), class = "hovercraft_error_session")
  })
})

describe("tip_update()", {
  withr::defer(tip_reset())
  tip_reset()

  it("sends the attributes to set, under the engine's names", {
    session <- fake_session()
    tip_update("run_tip", text = "New **words**", placement = "left", session = session)
    got <- session$sent()[[1]]
    expect_identical(got$type, paste0(tip_names()$prefix, "-tip-update"))
    expect_identical(got$message$id, "run_tip")
    expect_identical(
      got$message$set,
      stats::setNames(list("New **words**", "left"), c(tip_names()$attribute, tip_attribute("placement")))
    )
    expect_identical(got$message$remove, list())
  })

  it("leaves a value alone with NULL and takes it away with NA", {
    session <- fake_session()
    tip_update("run_tip", template = NA, placement = NA, session = session)
    got <- session$sent()[[1]]
    expect_identical(unlist(got$message$remove), c(tip_attributes()[["template"]], tip_attribute("placement")))
    expect_length(got$message$set, 0L)
    # The values left at NULL are in neither list.
    only <- fake_session()
    tip_update("run_tip", text = "New", template = NULL, record = NULL, placement = NULL, session = only)
    sent <- only$sent()[[1]]$message
    expect_named(sent$set, tip_names()$attribute)
    expect_identical(sent$remove, list())
  })

  it("takes the plain text off when a template comes without words, since the text would beat it", {
    tip_define(plain_swap = tip_template(tip_title("{{ label }}")))
    text <- tip_attributes()[["text"]]
    named <- fake_session()
    tip_update("run_tip", template = "plain_swap", session = named)
    got <- named$sent()[[1]]$message
    expect_identical(unlist(got$remove), text)
    expect_named(got$set, tip_attributes()[["template"]])
    inline <- fake_session()
    tip_update("run_tip", template = tip_template(tip_text("Words")), session = inline)
    expect_identical(unlist(inline$sent()[[1]]$message$remove), text)
    # Words given with it stay (they beat the template, as in tip_on()); a template taken away leaves the words alone.
    both <- fake_session()
    tip_update("run_tip", text = "Words", template = "plain_swap", session = both)
    expect_identical(both$sent()[[1]]$message$remove, list())
    gone <- fake_session()
    tip_update("run_tip", template = NA, session = gone)
    expect_identical(unlist(gone$sent()[[1]]$message$remove), tip_attributes()[["template"]])
  })

  it("warns when the record holds an engine name that the new words name", {
    expect_warning(
      tip_update(
        "run_tip",
        text = "Hello {{ who }} {{ tip_id }}",
        record = list(who = "Ada", tip_id = "9"),
        session = fake_session()
      ),
      class = "hovercraft_warning_engine_name"
    )
    expect_no_warning(
      tip_update("run_tip", text = "Hello {{ who }}", record = list(who = "Ada"), session = fake_session())
    )
  })

  it("takes the record away with NA, and needs a template to set one", {
    session <- fake_session()
    tip_update("run_tip", record = NA, session = session)
    expect_identical(unlist(session$sent()[[1]]$message$remove), tip_attributes()[["record"]])
    tip_define(diff = tip_template(tip_title("{{ label }}")))
    both <- fake_session()
    tip_update("run_tip", template = NA, record = NA, session = both)
    expect_length(both$sent()[[1]]$message$set, 0L)
    expect_error(
      tip_update("run_tip", template = NA, record = list(label = "x"), session = both),
      class = "hovercraft_error_record"
    )
  })

  it("writes a record as JSON, keeping only the columns a defined template names", {
    tip_define(diff = tip_template(tip_title("{{ label }}")))
    session <- fake_session()
    tip_update("run_tip", template = "diff", record = list(label = "ADAE", private = "x"), session = session)
    set <- session$sent()[[1]]$message$set
    expect_identical(set[[tip_attributes()[["template"]]]], "diff")
    expect_identical(jsonlite::fromJSON(set[[tip_attributes()[["record"]]]]), list(label = "ADAE"))
  })

  it("checks the template's fields against the record, as tip_on() does", {
    tip_define(diff = tip_template(tip_title("{{ label }}")))
    expect_error(
      tip_update("run_tip", template = "diff", record = list(other = 1), session = fake_session()),
      class = "hovercraft_error_field"
    )
  })

  it("refuses a record without its template, so no whole row goes to the page", {
    session <- fake_session()
    expect_error(
      tip_update("run_tip", record = list(label = "ADAE", private = "x"), session = session),
      class = "hovercraft_error_record"
    )
    expect_error(tip_update("run_tip", record = list(label = "ADAE"), session = session), "template")
    expect_length(session$sent(), 0L)
  })

  it("takes a tip_template() as tip_on() does: it travels with the message, under the name tip_on() would give it", {
    tpl <- tip_template(tip_title("{{ label }}"), tip_text("n is {{ n }}"))
    session <- fake_session()
    tip_update(
      "run_tip",
      text = NA,
      template = tpl,
      record = list(label = "ADAE", n = 3, extra = "x"),
      session = session
    )
    got <- session$sent()[[1]]$message
    name <- got$set[[tip_attributes()[["template"]]]]
    expect_match(name, "^inline_[a-f0-9]{12}$")
    expect_identical(
      name,
      htmltools::tagGetAttribute(
        tip_on(button(), template = tpl, record = list(label = "a", n = 1)),
        tip_attribute("template")
      )
    )
    expect_identical(got$define[[1]]$name, name)
    expect_identical(got$define[[1]]$blocks[[1]]$type, "title")
    expect_named(jsonlite::fromJSON(got$set[[tip_attributes()[["record"]]]]), c("label", "n"))
    expect_identical(unlist(got$remove), tip_attributes()[["text"]])
    expect_error(
      tip_update("run_tip", template = tpl, record = list(label = "ADAE"), session = fake_session()),
      class = "hovercraft_error_field"
    )
    # A template alone, with no record, changes the template only.
    alone <- fake_session()
    tip_update("run_tip", template = tpl, session = alone)
    expect_named(alone$sent()[[1]]$message$set, tip_attributes()[["template"]])
  })

  it("fills {{ field }} in new words when a record comes with them, and keeps plain words as words", {
    session <- fake_session()
    tip_update("run_tip", text = "Hello {{ who }}", record = list(who = "Ada", extra = 1), session = session)
    got <- session$sent()[[1]]$message
    expect_match(got$set[[tip_attributes()[["template"]]]], "^inline_")
    expect_false(tip_attributes()[["text"]] %in% names(got$set))
    expect_identical(unlist(got$remove), tip_attributes()[["text"]])
    expect_named(jsonlite::fromJSON(got$set[[tip_attributes()[["record"]]]]), "who")
    expect_error(
      tip_update("run_tip", text = "Hello {{ who }}", record = list(other = 1), session = fake_session()),
      class = "hovercraft_error_field"
    )
    expect_error(
      tip_update("run_tip", text = "Hello", record = list(who = "Ada"), session = fake_session()),
      class = "hovercraft_error_record"
    )
  })

  it("does not send a template that holds a tip_output(), which needs the tag's own tip_on()", {
    skip_if_not_installed("shiny")
    expect_error(
      tip_update("run_tip", template = tip_template(tip_output("plot_out")), session = fake_session()),
      class = "hovercraft_error_template"
    )
  })

  it("takes a template alone, for a tag that already holds its record", {
    tip_define(diff = tip_template(tip_title("{{ label }}")))
    session <- fake_session()
    expect_no_error(tip_update("run_tip", template = "diff", session = session))
    expect_identical(session$sent()[[1]]$message$set, stats::setNames(list("diff"), tip_attributes()[["template"]]))
  })

  it("warns about a template name that was not defined, and still sends it", {
    tip_define(diff = tip_template(tip_title("{{ label }}")))
    session <- fake_session()
    expect_warning(
      tip_update("run_tip", template = "dif_unseen", session = session),
      class = "hovercraft_warning_template"
    )
    expect_length(session$sent(), 1L)
  })

  it("takes the text away with NA and clears it with an empty string", {
    session <- fake_session()
    tip_update("run_tip", text = NA_character_, session = session)
    got <- session$sent()[[1]]$message
    expect_identical(unlist(got$remove), tip_attributes()[["text"]])
    expect_length(got$set, 0L)
    cleared <- fake_session()
    tip_update("run_tip", text = "", session = cleared)
    got <- cleared$sent()[[1]]$message
    expect_identical(got$set, stats::setNames(list(""), tip_attributes()[["text"]]))
    expect_identical(got$remove, list())
  })

  it("stops for nothing to change, an unnamed or unknown value, and a wrong value", {
    session <- fake_session()
    expect_error(tip_update("run_tip", session = session), class = "hovercraft_error_argument")
    expect_error(tip_update("run_tip", NULL, session = session), class = "hovercraft_error_argument")
    expect_error(tip_update("run_tip", text = NULL, template = NULL, session = session), "nothing to change")
    expect_error(tip_update("run_tip", "Words", extra = 1, session = session), class = "hovercraft_error_argument")
    expect_error(tip_update("run_tip", style = "sober", session = session), "style")
    expect_error(tip_update("run_tip", text = c("a", "b"), session = session), class = "hovercraft_error_argument")
    expect_error(tip_update("run_tip", placement = "above", session = session), class = "hovercraft_error_argument")
    expect_error(tip_update("run_tip", text = "a", session = NULL), class = "hovercraft_error_session")
    expect_no_error(tip_update("run_tip", "Words", session = session))
    expect_length(session$sent(), 1L)
  })

  it("adds the module's namespace to the id", {
    session <- fake_session("mod")
    tip_update("run_tip", text = "x", session = session)
    expect_identical(session$sent()[[1]]$message$id, "mod-run_tip")
  })
})

describe("tip_update() and the template registry of a session", {
  withr::defer(tip_reset())
  tip_reset()

  # What a session that a call is given looks like to tip_update(), with the registry of a real mock session.
  proxy_of <- function(mock) {
    sent <- list()
    list(
      sendCustomMessage = function(type, message) sent[[length(sent) + 1L]] <<- message,
      ns = function(id) id,
      userData = mock$userData,
      sent = function() sent
    )
  }

  it("checks and trims a record against the templates of the session it is given, outside the default domain", {
    skip_if_not_installed("shiny")
    mock <- shiny::MockShinySession$new()
    shiny::withReactiveDomain(mock, tip_define(card = tip_template(tip_text("{{ a }}"))))
    # Outside the domain, as in a later::later() callback, the process knows no "card"; the given session does.
    expect_null(tip_known()[["card"]])
    session <- proxy_of(mock)
    tip_update("x", template = "card", record = list(a = 1, secret = "S"), session = session)
    set <- session$sent()[[1]]$set
    expect_equal(jsonlite::fromJSON(set[[tip_attributes()[["record"]]]]), list(a = 1))
    expect_error(
      tip_update("x", template = "card", record = list(other = 1), session = session),
      class = "hovercraft_error_field"
    )
  })

  it("does not take another session's templates", {
    skip_if_not_installed("shiny")
    one <- shiny::MockShinySession$new()
    two <- shiny::MockShinySession$new()
    shiny::withReactiveDomain(one, tip_define(only_one = tip_template(tip_text("{{ a }}"))))
    session <- proxy_of(two)
    expect_no_error(
      suppressWarnings(tip_update("x", template = "only_one", record = list(a = 1, extra = "e"), session = session))
    )
    # Session two does not know the template of session one, so it cannot trim the record to the template's fields.
    expect_named(jsonlite::fromJSON(session$sent()[[1]]$set[[tip_attributes()[["record"]]]]), c("a", "extra"))
  })
})

describe("tip_on(events = )", {
  attrs <- function(tag) unlist(tag$attribs)

  it("writes the events attribute only when asked, and the runtime dependency carries the Shiny layer", {
    got <- tip_on(button(), "Runs", id = "run_tip", events = "hint")
    expect_identical(attrs(got)[[tip_shiny_attribute()]], "hint")
    runtime <- Filter(function(d) d$name == "hovercraft-runtime", htmltools::findDependencies(got))[[1]]
    expect_true("shiny.js" %in% runtime$script)
    expect_null(runtime$head)
    plain <- tip_on(button(), "Runs", id = "run_tip")
    expect_false(tip_shiny_attribute() %in% names(plain$attribs))
  })

  it("takes the tag's own id when there is no id", {
    expect_identical(attrs(tip_on(button(), "Runs", events = "hint"))[[tip_shiny_attribute()]], "hint")
  })

  it("replaces the events of an earlier tip_on() on the same tag", {
    first <- tip_on(button(), "Runs", events = "hint")
    rlang::reset_warning_verbosity("hovercraft-retip")
    expect_warning(again <- tip_on(first, "Runs again"), class = "hovercraft_warning_retip")
    expect_false(tip_shiny_attribute() %in% names(again$attribs))
  })

  it("needs an id", {
    expect_error(tip_on(htmltools::tags$button("Run"), "Runs", events = "hint"), class = "hovercraft_error_events")
  })

  it("refuses a name that is not an event, with the names it takes", {
    expect_error(tip_on(button(), "Runs", events = "hover"), class = "hovercraft_error_argument")
    expect_error(tip_on(button(), "Runs", events = character()), class = "hovercraft_error_argument")
    expect_error(tip_on(button(), "Runs", events = NA_character_), class = "hovercraft_error_argument")
  })

  it("takes pin and copy too, in the order given and once each", {
    got <- tip_on(button(), "Runs", events = c("hint", "pin", "copy", "pin"), pin = TRUE)
    expect_identical(attrs(got)[[tip_shiny_attribute()]], "hint pin copy")
    expect_identical(attrs(tip_on(button(), "Runs", events = "copy", pin = TRUE))[[tip_shiny_attribute()]], "copy")
  })

  it("stops on pin or copy without a pin, and says what to give", {
    for (events in list("pin", "copy", c("hint", "pin"))) {
      expect_error(tip_on(button(), "Runs", events = events), class = "hovercraft_error_events")
    }
    expect_error(tip_on(button(), "Runs", events = "pin", pin = FALSE), "no pin")
    expect_error(tip_on(button(), "Runs", events = "copy"), "TRUE")
    expect_no_error(tip_on(button(), "Runs", events = "hint"))
    expect_no_error(tip_on(htmltools::tags$span("x", id = "x", tabindex = "0"), "Runs", events = "pin", pin = "click"))
  })

  it("writes the layer's names from the engine's contract, not its own", {
    config <- jsonlite::fromJSON(jsonlite::toJSON(tip_runtime_config(), auto_unbox = TRUE))
    expect_identical(config$attributes$id, tip_attribute("id"))
    expect_identical(config$attributes$open, tip_contract()$attributes$open)
    expect_identical(config$attributes$placement, tip_attribute("placement"))
    expect_identical(config$attributes$disabled, tip_disabled_attribute())
    expect_identical(config$events$hint, tip_contract()$events$hint$name)
    expect_identical(config$messages$show, tip_message("show"))
    expect_identical(config$global, tip_names()$global)
    expect_identical(config$events$tip, tip_contract()$events$tip$name)
    expect_identical(config$events$copy, tip_contract()$events$copy$name)
    expect_identical(config$states$hint, "hint")
  })
})

describe("server calls on the tips of a table", {
  withr::defer(tip_reset())
  tip_reset()

  # A session that has drawn one table, `scores`, with these data columns (what tip_table() keeps for the session).
  table_session <- function(prefix = "", columns = c("team", "wins", "losses")) {
    session <- fake_session(prefix)
    session$userData <- new.env()
    tip_table_register(session$ns("scores"), columns, session)
    session
  }
  id_of <- function(session, i = 1L) session$sent()[[i]]$message$id

  it("address a row, a cell and a header by row and column, with the table's id", {
    session <- table_session()
    tip_show("scores", row = 3, session = session)
    tip_hide("scores", row = 3, column = "wins", session = session)
    tip_update("scores", text = "New", column = "losses", session = session)
    expect_identical(id_of(session, 1L), "scores/row/3")
    expect_identical(id_of(session, 2L), "scores/cell/3/2")
    expect_identical(id_of(session, 3L), "scores/header/3")
  })

  it("take a column by its name or by its position among the data columns", {
    session <- table_session()
    tip_show("scores", row = 2, column = 3, session = session)
    tip_show("scores", row = 2, column = "losses", session = session)
    expect_identical(id_of(session, 1L), id_of(session, 2L))
    expect_identical(id_of(session, 1L), "scores/cell/2/3")
  })

  it("add the module's namespace to the table's id, once", {
    session <- table_session("mod")
    tip_show("scores", row = 1, session = session)
    tip_show(session$ns("scores"), row = 1, session = session)
    expect_identical(id_of(session, 1L), "mod-scores/row/1")
    expect_identical(id_of(session, 2L), "mod-scores/row/1")
  })

  it("leave the id of a tag as it is when row and column are left out", {
    session <- table_session()
    tip_show("run_tip", session = session)
    expect_identical(id_of(session), "run_tip")
  })

  it("write the same ids as the table script, which the browser tests read back", {
    expect_identical(tip_table_tip_id("t", 4L, NULL), "t/row/4")
    expect_identical(tip_table_tip_id("t", 4L, 2L), "t/cell/4/2")
    expect_identical(tip_table_tip_id("t", NULL, 2L), "t/header/2")
    script <- paste(readLines(system.file("table", "table.js", package = "hovercraft")), collapse = "\n")
    expect_match(script, "`${id.base}/header/${id.column}`", fixed = TRUE)
    expect_match(script, "`${id.base}/row/${row}`", fixed = TRUE)
    expect_match(script, "`${id.base}/cell/${row}/${id.column}`", fixed = TRUE)
  })

  it("stop for a column that is not one, and list the columns", {
    session <- table_session()
    expect_error(
      tip_show("scores", row = 1, column = "points", session = session),
      class = "hovercraft_error_table"
    )
    expect_error(tip_show("scores", row = 1, column = "points", session = session), "team")
    expect_error(tip_show("scores", row = 1, column = 4, session = session), "losses")
    expect_error(tip_hide("scores", column = 0, session = session), class = "hovercraft_error_argument")
    expect_length(session$sent(), 0L)
  })

  it("wait for a column name when no table with the id has been drawn yet, and read it when sent", {
    session <- fake_session()
    session$userData <- new.env()
    # A session that cannot say when it flushes sends at once, so the name is read at the call and warns there.
    expect_warning(
      tip_show("scores", row = 1, column = "wins", session = session),
      class = "hovercraft_warning_table"
    )
    # The table is drawn after the call: the name is read at send time, from what it kept since.
    later <- fake_session()
    later$userData <- new.env()
    queue <- list()
    later$onFlushed <- function(callback, once = TRUE) queue[[length(queue) + 1L]] <<- callback
    tip_show("scores", row = 1, column = "wins", session = later)
    expect_length(later$sent(), 0L)
    tip_table_register("scores", c("team", "wins"), later)
    for (callback in queue) {
      callback()
    }
    expect_identical(later$sent()[[1]]$message$id, "scores/cell/1/2")
    # A position needs no names, and a row needs no column.
    expect_no_error(tip_show("scores", row = 1, column = 2, session = session))
    expect_no_error(tip_show("scores", row = 1, session = session))
  })

  it("warn, send nothing and never error in the flush for a name that is still not a column", {
    session <- fake_session()
    session$userData <- new.env()
    queue <- list()
    session$onFlushed <- function(callback, once = TRUE) queue[[length(queue) + 1L]] <<- callback
    tip_show("scores", row = 1, column = "wins", session = session)
    tip_update("scores", text = "x", column = "wins", session = session)
    expect_warning(queue[[1]](), class = "hovercraft_warning_table")
    expect_warning(queue[[2]](), class = "hovercraft_warning_table")
    expect_length(session$sent(), 0L)
    # A table that is drawn but has no such column warns the same way.
    queue <- list()
    tip_show("scores", column = "points", session = session)
    tip_table_register("scores", c("team", "wins"), session)
    expect_warning(
      for (callback in queue) {
        callback()
      },
      "points"
    )
    expect_length(session$sent(), 0L)
  })

  it("check everything at the call, so nothing can fail inside the flush", {
    session <- table_session()
    queue <- list()
    session$onFlushed <- function(callback, once = TRUE) queue[[length(queue) + 1L]] <<- callback
    shiny::isolate({
      expect_error(tip_show("scores", row = 1, column = "points", session = session), class = "hovercraft_error_table")
      expect_error(tip_hide("scores", row = 0, session = session), class = "hovercraft_error_argument")
      expect_error(tip_update("scores", text = "x", column = 9, session = session), class = "hovercraft_error_table")
    })
    expect_length(queue, 0L)
    shiny::isolate(tip_show("scores", row = 1, column = "wins", session = session))
    expect_no_error(
      for (callback in queue) {
        callback()
      }
    )
    expect_identical(session$sent()[[1]]$message$id, "scores/cell/1/2")
  })

  it("count the data columns only: a reactable's row names are not one", {
    skip_if_not_installed("reactable")
    skip_if_not_installed("DT")
    data <- data.frame(team = c("a", "b"), wins = c(1, 2), row.names = c("r1", "r2"))
    for (widget in list(
      reactable::reactable(data, rownames = TRUE),
      DT::datatable(data, rownames = TRUE)
    )) {
      session <- shiny::MockShinySession$new()
      shiny::withReactiveDomain(session, tip_table(widget, cells = c(wins = "{{ wins }}"), id = "scores"))
      expect_identical(get(session$ns("scores"), tip_session_tables(session = session)), c("team", "wins"))
    }
  })

  it("stop for a row that is not a whole number from 1", {
    session <- table_session()
    for (bad in list(0, -1, 1.5, NA, "2", c(1, 2))) {
      expect_error(tip_show("scores", row = bad, session = session), class = "hovercraft_error_argument")
    }
  })

  it("refuse a misspelt argument", {
    expect_error(tip_show("scores", rows = 1, session = table_session()), class = "hovercraft_error_argument")
    expect_error(tip_hide("scores", colum = 1, session = table_session()), "colum")
  })

  it("is remembered by tip_table() for the session it is drawn in", {
    skip_if_not_installed("DT")
    session <- shiny::MockShinySession$new()
    shiny::withReactiveDomain(session, {
      tip_table(DT::datatable(data.frame(team = "a", wins = 1)), rows = "{{ team }}", id = "scores")
    })
    expect_identical(get(session$ns("scores"), tip_session_tables(session = session)), c("team", "wins"))
    # A table with no id, or drawn outside a session, keeps nothing.
    tip_table(DT::datatable(data.frame(team = "a", wins = 1)), rows = "{{ team }}", id = "other")
    expect_null(tip_session_tables(session = session)[["other"]])
  })
})

describe("server calls and the flush", {
  # A session that holds onFlushed callbacks until flush() runs them, as Shiny does after a reactive flush, and counts
  # how many flushes were asked for.
  flushing_session <- function() {
    session <- fake_session()
    waiting <- list()
    asked <- 0L
    session$onFlushed <- function(callback, once = TRUE) waiting[[length(waiting) + 1L]] <<- callback
    session$requestFlush <- function() asked <<- asked + 1L
    session$asked <- function() asked
    session$flush <- function() {
      todo <- waiting
      waiting <<- list()
      for (callback in todo) {
        callback()
      }
    }
    session
  }

  it("wait for the flush, so a tag drawn in the same flush is there, and ask for one", {
    session <- flushing_session()
    shiny::isolate({
      tip_show("run_tip", session = session)
      tip_hide("run_tip", session = session)
      tip_update("run_tip", text = "New", session = session)
    })
    expect_length(session$sent(), 0L)
    expect_identical(session$asked(), 3L)
    session$flush()
    expect_identical(
      vapply(session$sent(), function(x) x$type, ""),
      paste0(tip_names()$prefix, c("-tip-show", "-tip-hide", "-tip-update"))
    )
    session$flush()
    expect_length(session$sent(), 3L)
  })

  it("wait and ask also outside a reactive context, where a `later` callback would not flush on its own", {
    session <- flushing_session()
    tip_show("run_tip", session = session)
    expect_length(session$sent(), 0L)
    expect_identical(session$asked(), 1L)
    session$flush()
    expect_length(session$sent(), 1L)
  })

  it("go on when a session says a flush cannot be requested, as shiny::testServer() does", {
    session <- flushing_session()
    session$requestFlush <- function() warning("for internal use only")
    expect_no_warning(tip_show("run_tip", session = session))
    session$requestFlush <- function() stop("for internal use only")
    expect_no_error(tip_show("run_tip", session = session))
    session$flush()
    expect_length(session$sent(), 2L)
  })

  it("wait without asking when the session has no requestFlush, and send at once with no onFlushed", {
    session <- flushing_session()
    session$requestFlush <- NULL
    expect_no_error(tip_show("run_tip", session = session))
    session$flush()
    expect_length(session$sent(), 1L)
    plain <- fake_session()
    tip_show("run_tip", session = plain)
    expect_length(plain$sent(), 1L)
  })
})
