skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# Shiny practices in a running bslib/Shiny app: renderUI() re-render, insertUI()/removeUI(), modules (two copies,
# nested, dynamic), hidden containers (tabs, accordion, conditionalPanel, collapsed sidebar), modals and full-screen
# cards, input updates and disabled state, dynamic tabs, bindCache() across two sessions, multi-session isolation,
# and embedding in an iframe.
describe("Shiny practices in a running app", {
  path <- find.package("hovercraft")
  port <- httpuv::randomPort()
  app <- callr::r_bg(
    function(port, path) {
      if (dir.exists(file.path(path, "inst"))) {
        pkgload::load_all(path, quiet = TRUE)
      } else {
        library(hovercraft)
      }

      card_mod_ui <- function(id, label) {
        ns <- shiny::NS(id)
        tpl <- paste0(gsub("-", "_", id, fixed = TRUE), "_card")
        shiny::tagList(
          do.call(tip_define, stats::setNames(list(tip_template(tip_text(paste0(label, ": {{ val }}")))), tpl)),
          tip_on(
            shiny::actionButton(ns("btn"), paste("Mod", label)),
            template = tpl,
            record = list(val = label),
            id = ns("tip")
          ),
          shiny::actionButton(ns("show"), paste("Show", label))
        )
      }
      card_mod_server <- function(id) {
        shiny::moduleServer(id, function(input, output, session) {
          shiny::observeEvent(input$show, tip_show("tip"))
        })
      }

      outer_mod_ui <- function(id) {
        ns <- shiny::NS(id)
        card_mod_ui(ns("inner"), "Nested")
      }
      outer_mod_server <- function(id) {
        shiny::moduleServer(id, function(input, output, session) {
          card_mod_server("inner")
        })
      }

      page_ui <- bslib::page_sidebar(
        title = "Practice checks",
        sidebar = bslib::sidebar(
          id = "side",
          open = FALSE,
          tip_on(shiny::actionButton("sb_btn", "Sidebar button"), "Sidebar tip", id = "sb_tip")
        ),
        shiny::div(
          shiny::actionButton("rerender", "Re-render"),
          shiny::uiOutput("dyn_ui"),
          shiny::actionButton("ins_add", "Insert"),
          shiny::actionButton("ins_rm", "Remove"),
          shiny::div(id = "ins_slot"),
          shiny::verbatimTextOutput("ins_seen"),
          card_mod_ui("mod_a", "Alpha"),
          card_mod_ui("mod_b", "Beta"),
          outer_mod_ui("outer"),
          shiny::actionButton("add_dyn_mod", "Add module"),
          shiny::div(id = "dyn_mod_slot"),
          shiny::actionButton("open_modal", "Open modal"),
          shiny::actionButton("upd_inputs", "Update inputs"),
          tip_on(shiny::actionButton("upd_btn", "Before"), "Updated button tip", id = "upd_btn_tip"),
          tip_on(shiny::textInput("upd_txt", "Field", "v1"), "Updated input tip", id = "upd_txt_tip"),
          tip_on(
            htmltools::tagAppendAttributes(shiny::actionButton("dis_btn", "Disabled"), disabled = "disabled"),
            "Disabled tip",
            id = "dis_tip"
          ),
          shiny::checkboxInput("show_cond", "Show conditional", FALSE),
          shiny::conditionalPanel(
            "input.show_cond",
            tip_on(shiny::actionButton("cond_btn", "Cond"), "Conditional tip", id = "cond_tip")
          ),
          shiny::actionButton("add_tab", "Add tab"),
          shiny::actionButton("rm_tab", "Remove tab"),
          shiny::uiOutput("cached_ui"),
          shiny::actionButton("iso_show", "Show isolated"),
          tip_on(shiny::actionButton("iso_btn", "Iso"), "Isolated tip", id = "iso_tip")
        ),
        bslib::navset_tab(
          id = "tabs",
          bslib::nav_panel("First", shiny::p("Tab one")),
          bslib::nav_panel(
            "Second",
            tip_on(shiny::actionButton("tab2_btn", "Tab 2"), "Hidden tab tip", id = "tab2_tip")
          )
        ),
        bslib::accordion(
          id = "acc",
          open = FALSE,
          bslib::accordion_panel(
            "Details",
            tip_on(shiny::actionButton("acc_btn", "Accordion"), "Accordion tip", id = "acc_tip")
          )
        ),
        bslib::card(
          id = "fs_card",
          full_screen = TRUE,
          tip_on(shiny::actionButton("fs_btn", "Full screen"), "Card tip", id = "fs_tip")
        )
      )

      server <- function(input, output, session) {
        ver <- shiny::reactiveVal(1L)
        shiny::observeEvent(input$rerender, ver(ver() + 1L))
        output$dyn_ui <- shiny::renderUI(
          tip_on(
            shiny::actionButton("dyn_btn", paste("Version", ver())),
            paste("Render tip", ver()),
            id = "dyn_tip"
          )
        )

        shiny::observeEvent(input$ins_add, {
          shiny::insertUI(
            "#ins_slot",
            ui = tip_on(
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

        card_mod_server("mod_a")
        card_mod_server("mod_b")
        outer_mod_server("outer")

        shiny::observeEvent(input$add_dyn_mod, {
          shiny::insertUI("#dyn_mod_slot", ui = card_mod_ui("dyn_mod", "Dynamic"))
          card_mod_server("dyn_mod")
        })

        shiny::observeEvent(input$open_modal, {
          shiny::showModal(shiny::modalDialog(
            title = "Modal check",
            tip_on(shiny::actionButton("modal_btn", "In modal"), "Modal tip", id = "modal_tip"),
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
              tip_on(shiny::actionButton("dyn_tab_btn", "Dyn tab"), "Dynamic tab tip", id = "dyn_tab_tip")
            ),
            select = TRUE
          )
        })
        shiny::observeEvent(input$rm_tab, shiny::removeTab("tabs", target = "dyn_tab"))

        output$cached_ui <- shiny::renderUI({
          tip_on(shiny::actionButton("cached_btn", "Cached"), "Cached tip", id = "cached_tip")
        }) |>
          shiny::bindCache("fixed_key")

        shiny::observeEvent(input$iso_show, tip_show("iso_tip"))
      }

      ui <- function(req) {
        if (grepl("host=1", req$QUERY_STRING %||% "", fixed = TRUE)) {
          htmltools::tags$html(
            htmltools::tags$body(
              htmltools::tags$iframe(id = "f", src = "/", width = "850", height = "650")
            )
          )
        } else {
          page_ui
        }
      }

      shiny::runApp(shiny::shinyApp(ui, server), port = port, host = "127.0.0.1", launch.browser = FALSE)
    },
    args = list(port = port, path = path),
    supervise = TRUE
  )
  withr::defer(app$kill())
  url <- paste0("http://127.0.0.1:", port)
  up <- FALSE
  for (i in 1:100) {
    up <- !is.null(tryCatch(suppressWarnings(readLines(url, n = 1L)), error = function(e) NULL))
    if (up || !app$is_alive()) {
      break
    }
    Sys.sleep(0.1)
  }
  page <- local_page(url, environment())
  press_button <- function(id) page$evaluate(sprintf("document.getElementById('%s').click()", id))
  hover_in_view <- function(selector) {
    page$evaluate(sprintf(
      "document.querySelector(%s)?.scrollIntoView({ block: 'center', behavior: 'instant' }); 0",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    ))
    Sys.sleep(0.1)
    page$hover(selector)
  }
  seen <- function(id, pattern) {
    page$wait_until(sprintf(
      "new RegExp(%s).test(document.getElementById(%s)?.textContent ?? '')",
      jsonlite::toJSON(pattern, auto_unbox = TRUE),
      jsonlite::toJSON(id, auto_unbox = TRUE)
    ))
  }

  it("starts and connects", {
    expect_true(up, info = paste(app$read_error(), collapse = "\n"))
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
  })

  it("closes an open tip on renderUI() re-render and shows the new tip without reload", {
    expect_true(page$wait_until("document.getElementById('dyn_btn') !== null"))
    expect_identical(hover_in_view("#dyn_btn"), "Render tip 1")
    press_button("rerender")
    expect_true(page$wait_until("document.getElementById('dyn_btn')?.textContent?.includes('Version 2')"))
    expect_true(page$wait_until(sprintf(
      "document.querySelectorAll('.%s:not([hidden])').length === 0",
      tip_names()$css
    )))
    expect_identical(hover_in_view("#dyn_btn"), "Render tip 2")
  })

  it("shows a tip on an insertUI() tag and closes it cleanly on removeUI()", {
    press_button("ins_add")
    expect_true(page$wait_until("document.getElementById('ins_btn') !== null"))
    expect_identical(page$hover("#ins_btn"), "Inserted tip")
    expect_true(seen("ins_seen", "^shown reader"))
    press_button("ins_rm")
    expect_true(page$wait_until("document.getElementById('ins_btn') === null"))
    expect_true(page$wait_until(sprintf(
      "document.querySelectorAll('.%s:not([hidden])').length === 0",
      tip_names()$css
    )))
    expect_true(seen("ins_seen", "^hidden data"))
  })

  it("isolates two module copies, nested modules, and a dynamically inserted module", {
    expect_identical(page$hover("#mod_a-btn"), "Alpha: Alpha")
    expect_identical(page$hover("#mod_b-btn"), "Beta: Beta")
    press_button("mod_a-show")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s:not([hidden])')?.textContent === 'Alpha: Alpha'",
      tip_names()$css
    )))
    expect_identical(page$hover("#outer-inner-btn"), "Nested: Nested")
    press_button("outer-inner-show")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s:not([hidden])')?.textContent === 'Nested: Nested'",
      tip_names()$css
    )))
    press_button("add_dyn_mod")
    expect_true(page$wait_until("document.getElementById('dyn_mod-btn') !== null"))
    expect_identical(page$hover("#dyn_mod-btn"), "Dynamic: Dynamic")
    press_button("dyn_mod-show")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s:not([hidden])')?.textContent === 'Dynamic: Dynamic'",
      tip_names()$css
    )))
  })

  it("shows tips inside containers that start hidden (tab, accordion, conditionalPanel, collapsed sidebar)", {
    # Second tab
    page$evaluate("document.querySelector('#tabs a[data-value=\"Second\"]').click()")
    expect_true(page$wait_until(
      "document.getElementById('tab2_btn')?.getBoundingClientRect().height > 0"
    ))
    expect_identical(hover_in_view("#tab2_btn"), "Hidden tab tip")

    # Accordion: wait for Bootstrap's collapse animation to finish (.show and no .collapsing).
    page$evaluate("document.querySelector('#acc .accordion-button').click()")
    expect_true(page$wait_until(
      "document.querySelector('#acc .accordion-collapse.show') !== null && document.querySelector('#acc .accordion-collapse.collapsing') === null"
    ))
    expect_identical(hover_in_view("#acc_btn"), "Accordion tip")

    # conditionalPanel
    page$evaluate("document.getElementById('show_cond').click()")
    expect_true(page$wait_until(
      "document.getElementById('cond_btn')?.getBoundingClientRect().height > 0"
    ))
    expect_identical(hover_in_view("#cond_btn"), "Conditional tip")

    # Collapsed bslib sidebar
    page$evaluate("document.querySelector('.bslib-sidebar-layout .collapse-toggle').click()")
    expect_true(page$wait_until(
      "document.getElementById('sb_btn')?.getBoundingClientRect().width > 0"
    ))
    Sys.sleep(0.4)
    expect_identical(hover_in_view("#sb_btn"), "Sidebar tip")
  })

  it("shows a tip inside showModal() above the modal backdrop", {
    press_button("open_modal")
    expect_true(page$wait_until("document.getElementById('modal_btn') !== null"))
    Sys.sleep(0.4)
    expect_identical(page$hover("#modal_btn"), "Modal tip")
    tip_above <- page$evaluate(sprintf(
      "(() => {
        const tip = document.querySelector('.%s:not([hidden])');
        const modal = document.querySelector('.modal');
        if (!tip || !modal) return false;
        const tz = Number(getComputedStyle(tip).zIndex) || 0;
        const mz = Number(getComputedStyle(modal).zIndex) || 0;
        return tz >= mz && tip.getBoundingClientRect().height > 0;
      })()",
      tip_names()$css
    ))
    expect_true(tip_above)
    page$evaluate("document.querySelector('.modal .btn-close, .modal [data-bs-dismiss=\"modal\"]')?.click()")
    expect_true(page$wait_until("document.getElementById('modal_btn') === null"))
  })

  it("shows a tip inside a bslib full-screen card", {
    page$evaluate("document.querySelector('#fs_card .bslib-full-screen-enter').click()")
    expect_true(page$wait_until("document.getElementById('fs_card')?.dataset.fullScreen === 'true'"))
    Sys.sleep(0.3)
    expect_identical(page$hover("#fs_btn"), "Card tip")
    tip_above <- page$evaluate(sprintf(
      "(() => {
        const tip = document.querySelector('.%s:not([hidden])');
        const card = document.getElementById('fs_card');
        if (!tip || !card) return false;
        const tz = Number(getComputedStyle(tip).zIndex) || 0;
        const cz = Number(getComputedStyle(card).zIndex) || 0;
        return tz >= cz && tip.getBoundingClientRect().height > 0;
      })()",
      tip_names()$css
    ))
    expect_true(tip_above)
    page$evaluate("document.querySelector('.bslib-full-screen-exit')?.click()")
    expect_true(page$wait_until("document.getElementById('fs_card')?.dataset.fullScreen !== 'true'"))
  })

  it("keeps tips working after updateActionButton() and updateTextInput()", {
    press_button("upd_inputs")
    expect_true(page$wait_until("document.getElementById('upd_btn')?.textContent?.includes('After')"))
    expect_true(page$wait_until("document.getElementById('upd_txt')?.value === 'v2'"))
    expect_identical(hover_in_view("#upd_btn"), "Updated button tip")
    expect_identical(hover_in_view("#upd_txt"), "Updated input tip")
  })

  it("handles a disabled button cleanly without console errors", {
    # Native disabled buttons suppress pointer events in browsers, so no tip opens and no error is raised.
    shown_disabled <- hover_in_view("#dis_btn")
    expect_length(shown_disabled, 0L)
    expect_identical(page$problems(), character())
  })

  it("works with appendTab() and closes an open tip on removeTab()", {
    press_button("add_tab")
    expect_true(page$wait_until("document.getElementById('dyn_tab_btn')?.getBoundingClientRect().height > 0"))
    expect_identical(hover_in_view("#dyn_tab_btn"), "Dynamic tab tip")
    press_button("rm_tab")
    expect_true(page$wait_until("document.getElementById('dyn_tab_btn') === null"))
    expect_true(page$wait_until(sprintf(
      "document.querySelectorAll('.%s:not([hidden])').length === 0",
      tip_names()$css
    )))
  })

  it("serves identical bindCache() renderUI() markup across two sessions and isolates server calls", {
    page2 <- local_page(url, environment())
    expect_true(page2$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
    expect_true(page$wait_until("document.getElementById('cached_btn') !== null"))
    expect_true(page2$wait_until("document.getElementById('cached_btn') !== null"))

    # Compare the server-rendered cached markup (strip client-side engine aria-describedby and .hc-tip-desc).
    server_html <- function(p) {
      p$evaluate(sprintf(
        "(() => {
          const clone = document.getElementById('cached_ui').cloneNode(true);
          clone.querySelectorAll('.%s-desc').forEach((n) => n.remove());
          clone.querySelectorAll('[aria-describedby]').forEach((n) => n.removeAttribute('aria-describedby'));
          return clone.innerHTML;
        })()",
        tip_names()$css
      ))
    }
    expect_identical(server_html(page), server_html(page2))
    expect_identical(hover_in_view("#cached_btn"), "Cached tip")
    page2$evaluate("document.querySelector('#cached_btn')?.scrollIntoView({ block: 'center', behavior: 'instant' }); 0")
    Sys.sleep(0.1)
    expect_identical(page2$hover("#cached_btn"), "Cached tip")

    # Clear tip in page2, trigger tip_show in page1, verify page2 stays closed.
    page2$press("Escape", wait = 0.3)
    expect_length(page2$shown(), 0L)
    press_button("iso_show")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s:not([hidden])')?.textContent === 'Isolated tip'",
      tip_names()$css
    )))
    Sys.sleep(0.3)
    expect_length(page2$shown(), 0L)
    expect_identical(page2$problems(), character())
  })

  it("works when the Shiny app is embedded inside an iframe", {
    outer <- local_page(paste0(url, "/?host=1"), environment())
    expect_true(outer$wait_until(
      "document.getElementById('f')?.contentWindow?.Shiny?.shinyapp?.isConnected?.() === true"
    ))
    expect_true(outer$wait_until(
      "document.getElementById('f')?.contentDocument?.getElementById('iso_btn') !== null"
    ))
    outer$evaluate("document.getElementById('f').contentDocument.getElementById('iso_show').click()")
    expect_true(outer$wait_until(sprintf(
      "document.getElementById('f')?.contentDocument?.querySelector('.%s:not([hidden])')?.textContent === 'Isolated tip'",
      tip_names()$css
    )))
    expect_identical(outer$problems(), character())
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})
