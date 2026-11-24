skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# tip_show(), tip_hide(), tip_update() and the hint input in a running Shiny app, in the page and inside a module.
describe("server calls and the hint input in a Shiny app", {
  path <- find.package("hovercraft")
  port <- httpuv::randomPort()
  app <- callr::r_bg(
    function(port, path, pin_attribute) {
      if (dir.exists(file.path(path, "inst"))) {
        pkgload::load_all(path, quiet = TRUE)
      } else {
        library(hovercraft)
      }
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
        shiny::observeEvent(input$clear, tip_update("run_tip", text = NULL, template = NULL))
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
      shiny::runApp(shiny::shinyApp(ui, server), port = port, host = "127.0.0.1", launch.browser = FALSE)
    },
    args = list(port = port, path = path, pin_attribute = tip_attribute("pin")),
    supervise = TRUE
  )
  withr::defer(app$kill())
  url <- paste0("http://127.0.0.1:", port)
  up <- wait_for_app(app, url)
  page <- local_page(url, environment())
  # Waits for the server to have drawn an output with this text, then reads it.
  seen <- function(id, pattern) {
    page$wait_until(sprintf(
      "new RegExp(%s).test(document.getElementById(%s)?.textContent ?? '')",
      jsonlite::toJSON(pattern, auto_unbox = TRUE),
      jsonlite::toJSON(id, auto_unbox = TRUE)
    ))
  }
  press_button <- function(id) page$click_now(paste0("#", id))

  it("starts and connects", {
    expect_true(up, info = paste(app$read_error(), collapse = "\n"))
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
  })

  it("opens a tip from the server, and says so in the hint input as the host's command", {
    press_button("show")
    expect_true(page$wait_until(sprintf("document.querySelector('.%s:not([hidden])') !== null", tip_names()$css)))
    expect_identical(page$shown(), "Runs the check")
    expect_true(seen("seen", "^shown host command"))
  })

  it("changes the words of the open tip from the server", {
    press_button("update")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s:not([hidden])')?.textContent === 'Now changed'",
      tip_names()$css
    )))
  })

  it("closes the tip from the server, with the host as the reason", {
    press_button("hide")
    expect_true(seen("seen", "^hidden host command host"))
    expect_length(page$shown(), 0L)
  })

  it("sends the hint input for a reader's pointer, and nothing for a tag that did not ask", {
    expect_identical(page$hover("#run"), "Now changed")
    expect_true(seen("seen", "^shown reader pointer"))
    expect_identical(page$hover("#quiet"), "A quiet tip")
    expect_true(seen("seen", "^hidden reader pointer"))
    expect_true(seen("quiet_seen", "quiet: TRUE"))
  })

  it("does nothing for a hide when that tip is not the one open", {
    page$hover("#quiet")
    press_button("hide")
    page$settle()
    expect_identical(page$shown(), "A quiet tip")
  })

  it("works inside a module, with the id as the module's own", {
    press_button("m-mod_show")
    expect_true(page$wait_until(sprintf(
      "[...document.querySelectorAll('.%s:not([hidden])')].some((t) => t.textContent === 'Module tip')",
      tip_names()$css
    )))
    expect_true(seen("m-mod_seen", "^shown host command"))
  })

  it("keeps the hint of another tag open when tip_hide() names a tag that is pinned", {
    page$click("#pin_a")
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
    press_button("missing")
    expect_true(page$wait_until("window.warned.length === 3"))
    warned <- jsonlite::fromJSON(page$evaluate("JSON.stringify(window.warned)"))
    expect_match(warned, "no tag with the tip id \"nope\"", all = TRUE)
    expect_identical(page$problems(), character())
  })

  it("sends the closing hint input when renderUI() replaces the tag that asked for it", {
    page$hover("#swap_btn")
    expect_true(seen("swap_seen", "^shown reader"))
    press_button("swap")
    expect_true(seen("swap_seen", "^hidden data"))
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
    press_button("place")
    page$settle()
    page$hover("#run")
    expect_identical(side(), "right")
  })

  it("takes the words away from the server, so the tag has no tip", {
    press_button("clear")
    page$settle()
    page$hover("#quiet")
    expect_length(page$hover("#run"), 0L)
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})
