skip_if_no_browser()

# tip_show(), tip_hide(), tip_update() and the hint input in a running Shiny app, in the page and inside a module.
describe("server calls and the hint input in a Shiny app", {
  app <- start_app(
    function(pin_attribute) {
      module_ui <- function(id) {
        ns <- shiny::NS(id)
        shiny::tagList(
          tip_on(shiny::actionButton(ns("mod_run"), "Module run"), "Module tip", id = ns("mod_tip"), events = "hint"),
          shiny::actionButton(ns("mod_show"), "Module show"),
          shiny::verbatimTextOutput(ns("mod_seen"))
        )
      }
      module_server <- function(id) {
        shiny::moduleServer(id, function(input, output, session) {
          shiny::observeEvent(input$mod_show, tip_show("mod_tip"))
          output$mod_seen <- shiny::renderText(paste(
            unlist(input$mod_tip_hint[c("state", "by", "via")]),
            collapse = " "
          ))
        })
      }
      ui <- shiny::fluidPage(
        tip_on(shiny::actionButton("run", "Run"), "Runs the **check**", id = "run_tip", events = "hint"),
        tip_on(shiny::actionButton("quiet", "Quiet"), "A quiet tip", id = "quiet_tip"),
        shiny::actionButton("show", "Show"),
        shiny::actionButton("hide", "Hide"),
        shiny::actionButton("update", "Update"),
        shiny::actionButton("clear", "Clear"),
        shiny::actionButton("place", "Place"),
        shiny::actionButton("missing", "Missing"),
        shiny::actionButton("swap", "Swap"),
        shiny::uiOutput("swap_ui"),
        tip_on(shiny::actionButton("tplb", "Tpl"), "Old words", id = "tpl_tip"),
        shiny::actionButton("tpl_go", "Tpl go"),
        shiny::actionButton("redraw", "Redraw"),
        shiny::uiOutput("late_ui"),
        shiny::verbatimTextOutput("swap_seen"),
        # A tag the reader can pin (the engine's pin attribute, written by hand: tip_on() has no pin argument yet).
        htmltools::tagAppendAttributes(
          tip_on(shiny::actionButton("pin_a", "Pin A"), "Pinned A", id = "a_tip"),
          !!pin_attribute := ""
        ),
        tip_on(shiny::actionButton("hover_b", "Hover B"), "Hover B", id = "b_tip"),
        shiny::verbatimTextOutput("seen"),
        shiny::verbatimTextOutput("quiet_seen"),
        module_ui("m")
      )
      server <- function(input, output, session) {
        shiny::observeEvent(input$show, tip_show("run_tip"))
        shiny::observeEvent(input$hide, tip_hide("run_tip"))
        shiny::observeEvent(input$update, tip_update("run_tip", text = "Now **changed**"))
        shiny::observeEvent(input$clear, tip_update("run_tip", text = NA, template = NA))
        swapped <- shiny::reactiveVal(FALSE)
        shiny::observeEvent(input$swap, swapped(TRUE))
        output$swap_ui <- shiny::renderUI(
          if (swapped()) {
            shiny::actionButton("swap_btn", "Replaced")
          } else {
            tip_on(shiny::actionButton("swap_btn", "Swap me"), "Swap tip", id = "swap_tip", events = "hint")
          }
        )
        output$swap_seen <- shiny::renderText(paste(
          unlist(input$swap_tip_hint[c("state", "by", "reason")]),
          collapse = " "
        ))
        # A tag that renderUI() draws in the same flush as the call that shows its tip.
        ticks <- shiny::reactiveVal(0)
        output$late_ui <- shiny::renderUI(
          tip_on(shiny::actionButton("late_btn", "Late"), paste("Late tip", ticks()), id = "late_tip")
        )
        shiny::observeEvent(input$redraw, {
          ticks(ticks() + 1)
          tip_show("late_tip")
        })
        shiny::observeEvent(input$tpl_go, {
          tip_update(
            "tpl_tip",
            text = NA,
            template = tip_template(tip_title("Hi {{ who }}"), tip_text("{{ n }} items")),
            record = list(who = "Ada", n = 3)
          )
        })
        shiny::observeEvent(input$place, tip_update("run_tip", placement = "right"))
        shiny::observeEvent(input$missing, {
          tip_show("nope")
          tip_hide("nope")
          tip_update("nope", text = "x")
        })
        shiny::observeEvent(input$hide_a, tip_hide("a_tip"))
        output$seen <- shiny::renderText(paste(
          unlist(input$run_tip_hint[c("state", "by", "via", "reason")]),
          collapse = " "
        ))
        output$quiet_seen <- shiny::renderText(paste("quiet:", is.null(input$quiet_tip_hint)))
        module_server("m")
      }
      shiny::shinyApp(ui, server)
    },
    pin_attribute = tip_attribute("pin")
  )
  url <- app$url
  page <- local_page(url, environment())
  # Waits for the server to have drawn an output with this text, then reads it.

  it("starts and connects", {
    expect_true(app$proc$is_alive())
    expect_true(page$wait_connected())
  })

  it("opens a tip from the server, and says so in the hint input as the host's command", {
    page$click_id("show")
    expect_true(page$wait_until(sprintf("document.querySelector('.%s:not([hidden])') !== null", tip_names()$css)))
    expect_identical(page$shown(), "Runs the check")
    expect_true(page$wait_text("seen", "^shown host command"))
  })

  it("changes the words of the open tip from the server", {
    page$click_id("update")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s:not([hidden])')?.textContent === 'Now changed'",
      tip_names()$css
    )))
  })

  it("closes the tip from the server, with the host as the reason", {
    page$click_id("hide")
    expect_true(page$wait_text("seen", "^hidden host command host"))
    expect_length(page$shown(), 0L)
  })

  it("sends the hint input for a reader's pointer, and nothing for a tag that did not ask", {
    expect_identical(page$hover("#run"), "Now changed")
    expect_true(page$wait_text("seen", "^shown reader pointer"))
    expect_identical(page$hover("#quiet"), "A quiet tip")
    expect_true(page$wait_text("seen", "^hidden reader pointer"))
    expect_true(page$wait_text("quiet_seen", "quiet: TRUE"))
  })

  it("does nothing for a hide when that tip is not the one open", {
    page$hover("#quiet")
    page$click_id("hide")
    page$settle()
    expect_identical(page$shown(), "A quiet tip")
  })

  it("works inside a module, with the id as the module's own", {
    page$click_id("m-mod_show")
    expect_true(page$wait_until(sprintf(
      "[...document.querySelectorAll('.%s:not([hidden])')].some((t) => t.textContent === 'Module tip')",
      tip_names()$css
    )))
    expect_true(page$wait_text("m-mod_seen", "^shown host command"))
  })

  it("keeps the hint of another tag open when tip_hide() names a tag that is pinned", {
    page$click("#pin_a", wait = 0)
    expect_true(page$wait_until(sprintf("document.querySelector('.%s:not([hidden])') !== null", tip_names()$css)))
    expect_identical(
      page$evaluate(sprintf("document.getElementById('pin_a').getAttribute('%s')", tip_contract()$attributes$open)),
      "pin"
    )
    page$hover("#hover_b")
    expect_true(any(page$shown() == "Hover B"))
    # Not a click: a click away would close the pin by itself.
    page$evaluate("Shiny.setInputValue('hide_a', Date.now())")
    page$settle()
    expect_true(any(page$shown() == "Hover B"))
  })

  it("says once on the console, and not as an error, that a server call found no tag", {
    page$evaluate(
      "window.warned = []; const warn = console.warn; console.warn = (...a) => { window.warned.push(a.join(' ')); warn(...a); }"
    )
    page$click_id("missing")
    expect_true(page$wait_until("window.warned.length === 3"))
    warned <- jsonlite::fromJSON(page$evaluate("JSON.stringify(window.warned)"))
    expect_match(warned, "no tag with the tip id \"nope\"", all = TRUE)
    expect_identical(page$problems(), character())
  })

  it("sends the closing hint input when renderUI() replaces the tag that asked for it", {
    page$hover("#swap_btn")
    expect_true(page$wait_text("swap_seen", "^shown reader"))
    page$click_id("swap")
    expect_true(page$wait_text("swap_seen", "^hidden data"))
  })

  it("applies a placement given to a closed tip the next time it opens", {
    side <- function() {
      page$evaluate(sprintf(
        "(() => { const t = document.querySelector('.%s:not([hidden])'); const b = document.getElementById('run').getBoundingClientRect(); const r = t.getBoundingClientRect(); return r.left >= b.right ? 'right' : 'other'; })()",
        tip_names()$css
      ))
    }
    page$hover("#run")
    expect_identical(side(), "other")
    page$hover("#quiet")
    page$click_id("place")
    page$settle()
    page$hover("#run")
    expect_identical(side(), "right")
  })

  it("takes the words away from the server, so the tag has no tip", {
    page$click_id("clear")
    page$settle()
    page$hover("#quiet")
    expect_length(page$hover("#run"), 0L)
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })

  it("shows the tip of a tag that renderUI() draws in the same observer as the call", {
    page$click_id("redraw")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s:not([hidden])')?.textContent === 'Late tip 1'",
      tip_names()$css
    )))
    # And once more: the new tag is drawn each time, and each call reaches the new one.
    page$click_id("redraw")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s:not([hidden])')?.textContent === 'Late tip 2'",
      tip_names()$css
    )))
  })

  it("changes a tip to a template that comes with the call, drawn from the record that comes with it", {
    page$click_id("tpl_go")
    # The call is sent after its flush: wait for the tag to hold the template before the pointer rests on it.
    expect_true(page$wait_until(
      "document.getElementById('tplb').closest('[data-hovercraft-tip-template]') !== null"
    ))
    shown <- page$hover("#tplb")
    expect_match(paste(shown, collapse = " "), "Hi Ada")
    expect_match(paste(shown, collapse = " "), "3 items")
    expect_no_match(paste(shown, collapse = " "), "Old words")
    expect_identical(page$problems(), character())
  })
})
