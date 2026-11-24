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

  it("takes a value away with NULL", {
    session <- fake_session()
    tip_update("run_tip", template = NULL, placement = NULL, session = session)
    got <- session$sent()[[1]]
    expect_identical(unlist(got$message$remove), c(tip_attributes()[["template"]], tip_attribute("placement")))
    expect_length(got$message$set, 0L)
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

  it("reads an empty text or NA as no text, as tip_on() does, and takes the text attribute away", {
    for (none in list("", NA_character_, NULL)) {
      session <- fake_session()
      tip_update("run_tip", text = none, session = session)
      got <- session$sent()[[1]]$message
      expect_identical(unlist(got$remove), tip_attributes()[["text"]])
      expect_length(got$set, 0L)
    }
  })

  it("stops for nothing to change, an unnamed or unknown value, and a wrong value", {
    session <- fake_session()
    expect_error(tip_update("run_tip", session = session), class = "hovercraft_error_argument")
    expect_error(tip_update("run_tip", "Words", session = session), class = "hovercraft_error_argument")
    expect_error(tip_update("run_tip", style = "sober", session = session), "style")
    expect_error(tip_update("run_tip", text = c("a", "b"), session = session), class = "hovercraft_error_argument")
    expect_error(tip_update("run_tip", placement = "above", session = session), class = "hovercraft_error_argument")
    expect_error(tip_update("run_tip", text = "a", text = "b", session = session), class = "hovercraft_error_argument")
    expect_error(tip_update("run_tip", text = "a", session = NULL), class = "hovercraft_error_session")
    expect_length(session$sent(), 0L)
  })

  it("adds the module's namespace to the id", {
    session <- fake_session("mod")
    tip_update("run_tip", text = "x", session = session)
    expect_identical(session$sent()[[1]]$message$id, "mod-run_tip")
  })
})

describe("tip_on(events = )", {
  attrs <- function(tag) unlist(tag$attribs)

  it("writes the events attribute only when asked, and the engine's dependency carries the Shiny layer", {
    got <- tip_on(button(), "Runs", id = "run_tip", events = "hint")
    expect_identical(attrs(got)[[tip_shiny_attribute()]], "hint")
    expect_match(htmltools::findDependencies(got)[[1]]$head, "HovercraftShiny.start", fixed = TRUE)
    plain <- tip_on(button(), "Runs", id = "run_tip")
    expect_false(tip_shiny_attribute() %in% names(plain$attribs))
  })

  it("takes the tag's own id when there is no id", {
    expect_identical(attrs(tip_on(button(), "Runs", events = "hint"))[[tip_shiny_attribute()]], "hint")
  })

  it("replaces the events of an earlier tip_on() on the same tag", {
    first <- tip_on(button(), "Runs", events = "hint")
    again <- tip_on(first, "Runs again")
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
    got <- tip_on(button(), "Runs", events = c("hint", "pin", "copy", "pin"))
    expect_identical(attrs(got)[[tip_shiny_attribute()]], "hint pin copy")
    expect_identical(attrs(tip_on(button(), "Runs", events = "copy"))[[tip_shiny_attribute()]], "copy")
  })

  it("writes the layer's names from the engine's contract, not its own", {
    config <- jsonlite::fromJSON(sub("^.*Shiny\\.start\\(([^\n]*)\\);</script>.*$", "\\1", tip_shiny_head()))
    expect_identical(config$attributes$id, tip_attribute("id"))
    expect_identical(config$attributes$open, tip_contract()$attributes$open)
    expect_identical(config$events$hint, tip_contract()$events$hint$name)
    expect_identical(config$messages$show, tip_message("show"))
    expect_identical(config$global, tip_names()$global)
    expect_identical(config$events$tip, tip_contract()$events$tip$name)
    expect_identical(config$events$copy, tip_contract()$events$copy$name)
    expect_identical(config$states$hint, "hint")
  })
})
