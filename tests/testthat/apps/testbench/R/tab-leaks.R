# Leak counts: what the page's scripts make when Shiny draws or removes their host, and must let go of (#131). The
# counted checks are in test-leak-browser.R, which loads this tab alone with two pins open
# (?only=leaks&max_pins=2); `?rt=31` starts the reactable at the words of its 31st draw, for a fresh draw to compare
# with. Not behaviour scenarios, so no spots. Every id starts with `leaks_`; the checks send the inputs `*_go` and
# `*_clear` themselves, with Shiny.setInputValue().

leaks_long <- function() hovercraft::tip_more("The long form.", on = "button")

# A module with one tagged button.
leaks_card_ui <- function(id) {
  ns <- shiny::NS(id)
  hovercraft::tip_on(
    shiny::actionButton(ns("btn"), "Mod"),
    "A module's tip",
    id = ns("tip"),
    pin = TRUE,
    more = leaks_long()
  )
}

leaks_card_server <- function(id) shiny::moduleServer(id, function(input, output, session) NULL)

# The words change with every draw, so a watcher left from an earlier draw would write the old words back.
leaks_rows <- function(n = 0L) {
  hovercraft::tip_spec(
    paste0("v", n, " **{{ team }}**: {{ v }}"),
    pin = TRUE,
    more = hovercraft::tip_more("More {{ team }}.", on = "button")
  )
}

# An output that a tip outside the output's region holds (its tip_define() is not in the region), with `items` items.
leaks_held_ui <- function(name, keep_alive, items = 1L) {
  template <- paste0(name, "_t")
  shiny::tagList(
    do.call(
      hovercraft::tip_define,
      stats::setNames(
        list(
          hovercraft::tip_template(
            hovercraft::tip_text("Held {{ n }}"),
            hovercraft::tip_output(paste0(name, "_out"), keep_alive = keep_alive)
          )
        ),
        template
      )
    ),
    lapply(seq_len(items), function(i) {
      suffix <- if (i == 1L) "" else i
      hovercraft::tip_on(
        shiny::actionButton(paste0(name, "_btn", suffix), sub("^LEAKS_", "", toupper(name))),
        template = template,
        record = list(n = i),
        id = paste0(name, "_tip", suffix),
        pin = TRUE
      )
    }),
    shiny::uiOutput(name)
  )
}

tab_leaks_ui <- function() {
  shiny::tagList(
    shiny::uiOutput("leaks_ru"),
    shiny::div(id = "leaks_ins_slot"),
    shiny::div(id = "leaks_mod_slot"),
    shiny::uiOutput("leaks_pk"),
    shiny::textOutput("leaks_pk_text"),
    # Outputs that a region draws and a tip outside the region holds.
    leaks_held_ui("leaks_pkb", keep_alive = TRUE),
    leaks_held_ui("leaks_pkc", keep_alive = FALSE),
    leaks_held_ui("leaks_pkd", keep_alive = TRUE),
    # Two items that share one output, and a region that draws it and defines the template the item uses.
    leaks_held_ui("leaks_pkf", keep_alive = TRUE, items = 2L),
    # The template is defined by the region below, when it draws: tip_on() warns once that it is not defined yet.
    suppressWarnings(hovercraft::tip_on(
      shiny::actionButton("leaks_pke_btn", "PKE"),
      template = "leaks_pke_t",
      record = list(n = 1),
      id = "leaks_pke_tip",
      pin = TRUE
    )),
    shiny::uiOutput("leaks_pke"),
    DT::DTOutput("leaks_dtp"),
    DT::DTOutput("leaks_dt"),
    reactable::reactableOutput("leaks_rt")
  )
}

tab_leaks_server <- function(input, output, session) {
  version <- function(name) input[[name]] %||% 0L

  output$leaks_ru <- shiny::renderUI({
    n <- version("leaks_ru_go")
    hovercraft::tip_on(
      shiny::actionButton("leaks_ru_btn", paste0("RU v", n)),
      paste("Render tip", n),
      id = "leaks_ru_tip",
      pin = TRUE,
      more = leaks_long()
    )
  })

  shiny::observeEvent(input$leaks_ins_go, {
    shiny::insertUI(
      "#leaks_ins_slot",
      ui = hovercraft::tip_on(
        shiny::actionButton("leaks_ins_btn", "Inserted"),
        "Inserted tip",
        id = "leaks_ins_tip",
        pin = TRUE,
        more = leaks_long()
      )
    )
  })
  shiny::observeEvent(input$leaks_ins_rm, shiny::removeUI("#leaks_ins_btn"))

  shiny::observeEvent(input$leaks_mod_go, {
    id <- paste0("leaks_m", input$leaks_mod_go)
    shiny::insertUI("#leaks_mod_slot", ui = shiny::div(id = paste0("w-", id), leaks_card_ui(id)))
    leaks_card_server(id)
  })
  shiny::observeEvent(input$leaks_mod_rm, shiny::removeUI(paste0("#w-leaks_m", input$leaks_mod_rm)))

  # An output that the tip holds, inside the region that draws again: the repro of the parked-output leak.
  output$leaks_pk_text <- shiny::renderText("The output the tip holds")
  output$leaks_pk <- shiny::renderUI({
    n <- version("leaks_pk_go")
    if (isTRUE(input$leaks_pk_clear > 0)) {
      return(NULL)
    }
    shiny::tagList(
      hovercraft::tip_define(
        leaks_pk_t = hovercraft::tip_template(
          hovercraft::tip_text("Parked {{ n }}"),
          hovercraft::tip_output(shiny::textOutput("leaks_pk_out"))
        )
      ),
      hovercraft::tip_on(
        shiny::actionButton("leaks_pk_btn", paste0("PK v", n)),
        template = "leaks_pk_t",
        record = list(n = n),
        id = "leaks_pk_tip",
        pin = TRUE
      )
    )
  })
  output$leaks_pk_out <- shiny::renderText("The output the tip holds")

  # A region that draws the output a tip holds (version in `data-v`, so a draw can be told), and that clears on request.
  region <- function(name) {
    output[[name]] <- shiny::renderUI({
      n <- version(paste0(name, "_go"))
      if (isTRUE(input[[paste0(name, "_clear")]] %% 2 == 1)) {
        return(NULL)
      }
      shiny::tagAppendAttributes(shiny::textOutput(paste0(name, "_out")), `data-v` = n)
    })
    output[[paste0(name, "_out")]] <- shiny::renderText(paste("Answer", sub("^leaks_", "", name)))
  }
  for (name in c("leaks_pkb", "leaks_pkc", "leaks_pkd", "leaks_pkf")) {
    region(name)
  }

  # The template comes with the region, and the item that uses it does not: clearing the region leaves the item and its
  # open tip, and drawing the region again defines the template again.
  output$leaks_pke <- shiny::renderUI({
    n <- version("leaks_pke_go")
    if (isTRUE(input$leaks_pke_clear %% 2 == 1)) {
      return(NULL)
    }
    shiny::tagList(
      shiny::span(id = "leaks_pke_v", `data-v` = n),
      hovercraft::tip_define(
        leaks_pke_t = hovercraft::tip_template(
          hovercraft::tip_text("Held {{ n }}"),
          hovercraft::tip_output(shiny::textOutput("leaks_pke_out"))
        )
      )
    )
  })
  output$leaks_pke_out <- shiny::renderText("Answer pke")

  output$leaks_dtp <- DT::renderDT(
    {
      n <- version("leaks_dtp_go")
      hovercraft::tip_table(
        DT::datatable(data.frame(team = c("Owls", "Foxes"), v = n), rownames = FALSE),
        rows = hovercraft::tip_spec("**{{ team }}**: {{ v }}", pin = TRUE, to = "in_place"),
        id = "leaks_dtp"
      )
    },
    server = FALSE
  )
  output$leaks_dt <- DT::renderDT(
    {
      n <- version("leaks_dt_go")
      hovercraft::tip_table(
        DT::datatable(data.frame(team = c("Owls", "Foxes"), v = n), rownames = FALSE),
        rows = leaks_rows(),
        id = "leaks_dt"
      )
    },
    server = FALSE
  )
  output$leaks_rt <- reactable::renderReactable({
    # `?rt=31` starts the table at the words of the 31st draw, for a fresh draw to compare with.
    n <- input$leaks_rt_go %||% as.integer(shiny::parseQueryString(session$clientData$url_search)$rt %||% 0L)
    hovercraft::tip_table(
      reactable::reactable(data.frame(team = c("Owls", "Foxes"), v = n)),
      rows = leaks_rows(n),
      id = "leaks_rt"
    )
  })
}
