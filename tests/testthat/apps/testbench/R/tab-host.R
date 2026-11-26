# The Shiny host, HC-050 to HC-059, and the async practice: redraws, inserts, modals and hidden sections with tips open,
# the server calls, labels, settings given twice, the session ending, an iframe, module copies, bookmarks and an
# ExtendedTask.

# A module with a pinnable button and a server call, for two copies on one page.
host_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::div(
    class = "d-inline-flex gap-1",
    hovercraft::tip_on(
      htmltools::tags$button(id = ns("btn"), type = "button", class = "btn btn-outline-primary btn-sm", id),
      paste("The tip of", id),
      id = ns("tip"),
      pin = TRUE
    ),
    shiny::actionButton(ns("show"), paste("Show", id))
  )
}

host_mod_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$show, hovercraft::tip_show("tip"))
  })
}

tab_host_ui <- function() {
  htmltools::tagList(
    spot("HC-050", shiny::actionButton("host_redraw", "Redraw"), shiny::uiOutput("host_redrawn", inline = TRUE)),
    spot(
      "HC-051",
      shiny::actionButton("host_insert", "insertUI()"),
      shiny::actionButton("host_remove", "removeUI()"),
      shiny::actionButton("host_add_tab", "appendTab()"),
      shiny::actionButton("host_remove_tab", "removeTab()"),
      htmltools::div(id = "host-slot"),
      bslib::navset_tab(id = "host_tabs", bslib::nav_panel("Fixed", value = "fixed", "A fixed tab"))
    ),
    spot(
      "HC-052",
      note = "Pin the button, then open and close the modal.",
      pin_button("host-under-modal", "Pin me first", "Pinned below the modal"),
      shiny::actionButton("host_modal", "Open a modal")
    ),
    spot(
      "HC-053",
      note = "Pin the button inside, then fold the section: the pin closes as gone.",
      bslib::accordion(
        id = "host_acc",
        bslib::accordion_panel(
          "Section",
          value = "section",
          pin_button("host-in-acc", "In the section", "Pinned in a section")
        )
      )
    ),
    spot(
      "HC-054",
      shiny::actionButton("host_show", "tip_show()"),
      shiny::actionButton("host_hide", "tip_hide()"),
      shiny::actionButton("host_words", "tip_update() words"),
      shiny::actionButton("host_template", "tip_update() template"),
      hovercraft::tip_on(
        htmltools::tags$button(id = "host-called", type = "button", class = "btn btn-outline-primary btn-sm", "Called"),
        "Words from the page",
        id = "host_called_tip",
        events = "hint"
      )
    ),
    spot(
      "HC-055",
      shiny::actionButton("host_show_far", "Show the far one"),
      shiny::actionButton("host_show_hidden", "Show the hidden one"),
      shiny::actionButton("host_show_later", "Show one not drawn yet"),
      htmltools::div(style = "display: none;", tip_button("host-hidden", "Hidden", "A hidden item's tip")),
      shiny::uiOutput("host_later", inline = TRUE)
    ),
    spot(
      "HC-056",
      note = htmltools::tagList(
        bench_link("Open with custom words for every tip", only = "host", labels = "custom"),
        "."
      ),
      tip_button(
        "host-words-own",
        "Its own words",
        "Pinned with its own words",
        pin = TRUE,
        more = hovercraft::tip_more("Long form."),
        labels = c(more = "Read on", less = "Fold", close = "Dismiss")
      ),
      shiny::verbatimTextOutput("host_unknown_label")
    ),
    spot(
      "HC-057",
      note = paste(
        "Insert a second use_hovercraft() with other settings, and two tags with one tip id;",
        "the console warns once."
      ),
      shiny::actionButton("host_second_call", "Insert a second use_hovercraft()"),
      shiny::actionButton("host_same_id", "Insert two tags with one tip id"),
      htmltools::div(id = "host-second-slot")
    ),
    spot(
      "HC-058",
      note = "Pin the button, then end the session: the page greys out and nothing is left stuck.",
      pin_button("host-before-end", "Pin before the end", "Open when the session ends"),
      shiny::actionButton("host_end", "End this session", class = "btn-outline-danger")
    ),
    spot(
      "HC-059",
      note = htmltools::tagList(
        bench_link("Open this tab inside an iframe", only = "host", host = 1),
        "; pin in either module copy; bookmark with the button, reload the address it gives."
      ),
      host_mod_ui("host-mod-1"),
      host_mod_ui("host-mod-2"),
      shiny::bookmarkButton(id = "host_bookmark")
    ),
    htmltools::tags$section(
      class = "bench-spot",
      htmltools::tags$header(htmltools::tags$strong("Async (practice): an ExtendedTask")),
      htmltools::tags$p(
        class = "small",
        "Start: the output shows its loading state; the tip on the result works when it lands."
      ),
      shiny::actionButton("host_task_start", "Start a 2-second task"),
      shiny::uiOutput("host_task_result")
    ),
    htmltools::div(
      style = "height: 120vh;",
      class = "small text-body-secondary",
      "Room, so the next item is far below."
    ),
    tip_button("host-far", "The far one", "Shown from the server while out of view")
  )
}

tab_host_server <- function(input, output, session) {
  output$host_redrawn <- shiny::renderUI({
    input$host_redraw
    pin_button("host-redrawn", paste("Drawn", input$host_redraw %||% 0), "Redrawn with its tip open", more = "Long.")
  })

  shiny::observeEvent(input$host_insert, {
    shiny::insertUI("#host-slot", ui = pin_button("host-inserted", "Inserted", "Inserted, pin me"))
  })
  shiny::observeEvent(input$host_remove, shiny::removeUI("#host-inserted"))
  shiny::observeEvent(input$host_add_tab, {
    shiny::appendTab(
      "host_tabs",
      bslib::nav_panel("Added", value = "added", pin_button("host-tab-btn", "In an added tab", "Pinned in a tab")),
      select = TRUE
    )
  })
  shiny::observeEvent(input$host_remove_tab, shiny::removeTab("host_tabs", target = "added"))

  shiny::observeEvent(input$host_modal, {
    shiny::showModal(shiny::modalDialog(title = "A modal over pins", "Close me.", easyClose = TRUE))
  })

  shiny::observeEvent(input$host_show, hovercraft::tip_show("host_called_tip"))
  shiny::observeEvent(input$host_hide, hovercraft::tip_hide("host_called_tip"))
  shiny::observeEvent(input$host_words, hovercraft::tip_update("host_called_tip", "Words from the server"))
  shiny::observeEvent(input$host_template, {
    hovercraft::tip_update(
      "host_called_tip",
      template = hovercraft::tip_template(hovercraft::tip_title("{{ title }}"), hovercraft::tip_text("{{ body }}")),
      record = list(title = "A template", body = "That came with the call")
    )
  })

  shiny::observeEvent(input$host_show_far, hovercraft::tip_show("host_far_tip"))
  shiny::observeEvent(input$host_show_hidden, hovercraft::tip_show("host_hidden_tip"))
  later_on <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(input$host_show_later, {
    hovercraft::tip_show("host_later_tip")
    later_on(TRUE)
  })
  output$host_later <- shiny::renderUI({
    if (later_on()) tip_button("host-later", "Drawn later", "Drawn after the call")
  })

  output$host_unknown_label <- shiny::renderText({
    result <- tryCatch(hovercraft::use_hovercraft(labels = c(nope = "x")), error = function(e) e)
    if (inherits(result, "error")) conditionMessage(result) else "Not refused: this is a finding."
  })

  shiny::observeEvent(input$host_second_call, {
    shiny::insertUI("#host-second-slot", ui = hovercraft::use_hovercraft(max_pins = 4, keys = c(pin = FALSE)))
  })
  shiny::observeEvent(input$host_same_id, {
    shiny::insertUI(
      "#host-second-slot",
      ui = htmltools::tagList(
        hovercraft::tip_on(
          htmltools::tags$button(type = "button", class = "btn btn-sm", "Same 1"),
          "One",
          id = "host_same"
        ),
        hovercraft::tip_on(
          htmltools::tags$button(type = "button", class = "btn btn-sm", "Same 2"),
          "Two",
          id = "host_same"
        )
      )
    )
  })
  shiny::observeEvent(input$host_end, session$close())

  host_mod_server("host-mod-1")
  host_mod_server("host-mod-2")

  task <- shiny::ExtendedTask$new(function() {
    promises::promise(function(resolve, reject) later::later(function() resolve(Sys.time()), 2))
  })
  shiny::observeEvent(input$host_task_start, task$invoke())
  output$host_task_result <- shiny::renderUI({
    when <- task$result()
    tip_button("host-task-done", "Task done", paste("Finished at", format(when, "%H:%M:%S")))
  })
}
