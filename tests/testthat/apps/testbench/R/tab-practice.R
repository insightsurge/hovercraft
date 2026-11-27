# Shiny practices: renderUI() re-render, insertUI()/removeUI(), modules (two copies, nested, added at run time), hidden
# containers (tabs, accordion, conditionalPanel, collapsed sidebar), a modal and a full-screen card, input updates and a
# disabled button, dynamic tabs, bindCache() across two sessions, and (with ?host=1) the page inside an iframe.
# test-practice-browser.R drives this tab; its ids are the ones that test has always used.

# A module with a tagged button and a button that shows its tip from the server.
practice_card_ui <- function(id, label) {
  ns <- shiny::NS(id)
  tpl <- paste0(gsub("-", "_", id, fixed = TRUE), "_card")
  shiny::tagList(
    do.call(
      hovercraft::tip_define,
      stats::setNames(list(hovercraft::tip_template(hovercraft::tip_text(paste0(label, ": {{ val }}")))), tpl)
    ),
    hovercraft::tip_on(
      shiny::actionButton(ns("btn"), paste("Mod", label)),
      template = tpl,
      record = list(val = label),
      id = ns("tip")
    ),
    shiny::actionButton(ns("show"), paste("Show", label))
  )
}

practice_card_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$show, hovercraft::tip_show("tip"))
  })
}

# A module that holds the card module.
practice_outer_ui <- function(id) {
  ns <- shiny::NS(id)
  practice_card_ui(ns("inner"), "Nested")
}

practice_outer_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    practice_card_server("inner")
  })
}

tab_practice_ui <- function() {
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      id = "side",
      open = FALSE,
      hovercraft::tip_on(shiny::actionButton("sb_btn", "Sidebar button"), "Sidebar tip", id = "sb_tip")
    ),
    shiny::div(
      shiny::actionButton("rerender", "Re-render"),
      shiny::uiOutput("dyn_ui"),
      shiny::actionButton("ins_add", "Insert"),
      shiny::actionButton("ins_rm", "Remove"),
      shiny::div(id = "ins_slot"),
      shiny::verbatimTextOutput("ins_seen"),
      practice_card_ui("mod_a", "Alpha"),
      practice_card_ui("mod_b", "Beta"),
      practice_outer_ui("outer"),
      shiny::actionButton("add_dyn_mod", "Add module"),
      shiny::div(id = "dyn_mod_slot"),
      shiny::actionButton("open_modal", "Open modal"),
      shiny::actionButton("upd_inputs", "Update inputs"),
      hovercraft::tip_on(shiny::actionButton("upd_btn", "Before"), "Updated button tip", id = "upd_btn_tip"),
      hovercraft::tip_on(shiny::textInput("upd_txt", "Field", "v1"), "Updated input tip", id = "upd_txt_tip"),
      hovercraft::tip_on(
        htmltools::tagAppendAttributes(shiny::actionButton("dis_btn", "Disabled"), disabled = "disabled"),
        "Disabled tip",
        id = "dis_tip"
      ),
      shiny::checkboxInput("show_cond", "Show conditional", FALSE),
      shiny::conditionalPanel(
        "input.show_cond",
        hovercraft::tip_on(shiny::actionButton("cond_btn", "Cond"), "Conditional tip", id = "cond_tip")
      ),
      shiny::actionButton("add_tab", "Add tab"),
      shiny::actionButton("rm_tab", "Remove tab"),
      shiny::uiOutput("cached_ui"),
      shiny::actionButton("iso_show", "Show isolated"),
      hovercraft::tip_on(shiny::actionButton("iso_btn", "Iso"), "Isolated tip", id = "iso_tip")
    ),
    bslib::navset_tab(
      id = "tabs",
      bslib::nav_panel("First", shiny::p("Tab one")),
      bslib::nav_panel(
        "Second",
        hovercraft::tip_on(shiny::actionButton("tab2_btn", "Tab 2"), "Hidden tab tip", id = "tab2_tip")
      )
    ),
    bslib::accordion(
      id = "acc",
      open = FALSE,
      bslib::accordion_panel(
        "Details",
        hovercraft::tip_on(shiny::actionButton("acc_btn", "Accordion"), "Accordion tip", id = "acc_tip")
      )
    ),
    bslib::card(
      id = "fs_card",
      full_screen = TRUE,
      hovercraft::tip_on(shiny::actionButton("fs_btn", "Full screen"), "Card tip", id = "fs_tip")
    )
  )
}

tab_practice_server <- function(input, output, session) {
  ver <- shiny::reactiveVal(1L)
  shiny::observeEvent(input$rerender, ver(ver() + 1L))
  output$dyn_ui <- shiny::renderUI(
    hovercraft::tip_on(
      shiny::actionButton("dyn_btn", paste("Version", ver())),
      paste("Render tip", ver()),
      id = "dyn_tip"
    )
  )

  shiny::observeEvent(input$ins_add, {
    shiny::insertUI(
      "#ins_slot",
      ui = hovercraft::tip_on(
        shiny::actionButton("ins_btn", "Inserted"),
        "Inserted tip",
        id = "ins_tip",
        events = "hint"
      )
    )
  })
  shiny::observeEvent(input$ins_rm, shiny::removeUI("#ins_btn"))
  output$ins_seen <- shiny::renderText(paste(
    unlist(input$ins_tip_hint[c("state", "by")]),
    collapse = " "
  ))

  practice_card_server("mod_a")
  practice_card_server("mod_b")
  practice_outer_server("outer")

  shiny::observeEvent(input$add_dyn_mod, {
    shiny::insertUI("#dyn_mod_slot", ui = practice_card_ui("dyn_mod", "Dynamic"))
    practice_card_server("dyn_mod")
  })

  shiny::observeEvent(input$open_modal, {
    shiny::showModal(shiny::modalDialog(
      title = "Modal check",
      hovercraft::tip_on(shiny::actionButton("modal_btn", "In modal"), "Modal tip", id = "modal_tip"),
      easyClose = TRUE
    ))
  })

  shiny::observeEvent(input$upd_inputs, {
    shiny::updateActionButton(session, "upd_btn", label = "After")
    shiny::updateTextInput(session, "upd_txt", value = "v2")
  })

  shiny::observeEvent(input$add_tab, {
    shiny::appendTab(
      "tabs",
      shiny::tabPanel(
        "Dynamic",
        value = "dyn_tab",
        hovercraft::tip_on(shiny::actionButton("dyn_tab_btn", "Dyn tab"), "Dynamic tab tip", id = "dyn_tab_tip")
      ),
      select = TRUE
    )
  })
  shiny::observeEvent(input$rm_tab, shiny::removeTab("tabs", target = "dyn_tab"))

  output$cached_ui <- shiny::renderUI({
    hovercraft::tip_on(shiny::actionButton("cached_btn", "Cached"), "Cached tip", id = "cached_tip")
  }) |>
    shiny::bindCache("fixed_key")

  shiny::observeEvent(input$iso_show, hovercraft::tip_show("iso_tip"))
}
