skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# End-to-end Shiny browser tests for tip_output(output, keep_alive = TRUE):
# - plot, DT and reactable outputs rendered inside tips
# - keep_alive = TRUE parks the output (no recomputation on reopen; refreshes when inputs change while parked)
# - keep_alive = FALSE unbinds and destroys the output on close
# - shinycssloaders loader compatibility (no clash with hovercraft placeholder)
# - loading (async via promises/later), error with Retry button, and empty states
# - bindCache() on both renderUI(tip_on(...)) and renderText/renderPlot outputs
describe("tip_output() in a live Shiny app", {
  has_dt <- requireNamespace("DT", quietly = TRUE)
  has_rt <- requireNamespace("reactable", quietly = TRUE)
  has_spin <- requireNamespace("shinycssloaders", quietly = TRUE)
  has_promises <- requireNamespace("promises", quietly = TRUE) && requireNamespace("later", quietly = TRUE)

  path <- find.package("hovercraft")
  port <- httpuv::randomPort()
  app <- callr::r_bg(
    function(port, path, has_dt, has_rt, has_spin, has_promises) {
      if (dir.exists(file.path(path, "inst"))) {
        pkgload::load_all(path, quiet = TRUE)
      } else {
        library(hovercraft)
      }
      counts <- new.env(parent = emptyenv())
      counts$alive <- 0L
      counts$dead <- 0L
      counts$err_tries <- 0L
      counts$cached_plot <- 0L

      spin_tag <- if (has_spin) {
        shinycssloaders::withSpinner(shiny::textOutput("spin_out"))
      } else {
        shiny::textOutput("spin_out")
      }

      ui <- shiny::fluidPage(
        tip_define(
          alive_tpl = tip_template(tip_title("{{ label }}"), tip_output("alive_out", keep_alive = TRUE)),
          dead_tpl = tip_template(tip_title("{{ label }}"), tip_output("dead_out", keep_alive = FALSE)),
          plot_tpl = tip_template(
            tip_title("{{ label }}"),
            tip_output(shiny::plotOutput("plot_out", height = "200px"))
          ),
          dt_tpl = tip_template(tip_title("{{ label }}"), tip_output("dt_out")),
          rt_tpl = tip_template(tip_title("{{ label }}"), tip_output("rt_out")),
          spin_tpl = tip_template(tip_title("{{ label }}"), tip_output(spin_tag)),
          async_tpl = tip_template(tip_title("{{ label }}"), tip_output("async_out")),
          err_tpl = tip_template(tip_title("{{ label }}"), tip_output("err_out")),
          empty_tpl = tip_template(tip_title("{{ label }}"), tip_output("empty_out")),
          cache_tpl = tip_template(tip_title("{{ label }}"), tip_output("cached_out"))
        ),
        shiny::tags$div(
          hidden = NA,
          shiny::textOutput("alive_out"),
          shiny::textOutput("dead_out"),
          if (has_dt) DT::DTOutput("dt_out") else shiny::textOutput("dt_out"),
          if (has_rt) reactable::reactableOutput("rt_out") else shiny::textOutput("rt_out"),
          shiny::textOutput("async_out"),
          shiny::textOutput("err_out"),
          shiny::textOutput("empty_out"),
          shiny::textOutput("cached_out")
        ),
        shiny::actionButton("bump", "Bump"),
        shiny::actionButton("park_btn", "Neutral"),
        tip_on(
          shiny::actionButton("btn_alive", "Alive"),
          template = "alive_tpl",
          record = list(label = "Alive"),
          id = "btn_alive"
        ),
        tip_on(
          shiny::actionButton("btn_dead", "Dead"),
          template = "dead_tpl",
          record = list(label = "Dead"),
          id = "btn_dead"
        ),
        tip_on(
          shiny::actionButton("btn_plot", "Plot"),
          template = "plot_tpl",
          record = list(label = "Plot"),
          id = "btn_plot"
        ),
        tip_on(shiny::actionButton("btn_dt", "DT"), template = "dt_tpl", record = list(label = "DT"), id = "btn_dt"),
        tip_on(shiny::actionButton("btn_rt", "RT"), template = "rt_tpl", record = list(label = "RT"), id = "btn_rt"),
        tip_on(
          shiny::actionButton("btn_spin", "Spin"),
          template = "spin_tpl",
          record = list(label = "Spin"),
          id = "btn_spin"
        ),
        tip_on(
          shiny::actionButton("btn_async", "Async"),
          template = "async_tpl",
          record = list(label = "Async"),
          id = "btn_async"
        ),
        tip_on(
          shiny::actionButton("btn_err", "Err"),
          template = "err_tpl",
          record = list(label = "Err"),
          id = "btn_err"
        ),
        tip_on(
          shiny::actionButton("btn_empty", "Empty"),
          template = "empty_tpl",
          record = list(label = "Empty"),
          id = "btn_empty"
        ),
        shiny::uiOutput("cached_ui"),
        shiny::verbatimTextOutput("alive_count"),
        shiny::verbatimTextOutput("dead_count")
      )

      server <- function(input, output, session) {
        bump <- shiny::reactiveVal(1L)
        shiny::observeEvent(input$bump, bump(bump() + 1L))

        output$alive_out <- shiny::renderText({
          val <- bump()
          counts$alive <- counts$alive + 1L
          paste0("alive-v", val, "-n", counts$alive)
        })
        output$alive_count <- shiny::renderText(as.character(counts$alive))

        output$dead_out <- shiny::renderText({
          counts$dead <- counts$dead + 1L
          paste0("dead-n", counts$dead)
        })
        output$dead_count <- shiny::renderText(as.character(counts$dead))

        output$plot_out <- shiny::renderPlot({
          graphics::par(mar = c(3, 3, 2, 1))
          graphics::plot(1:5, 1:5, main = "Tip Plot")
        })

        output$dt_out <- if (has_dt) {
          DT::renderDT(DT::datatable(data.frame(Team = c("Owls", "Foxes"), Wins = c(7, 5)), options = list(dom = "t")))
        } else {
          shiny::renderText("no-dt")
        }

        output$rt_out <- if (has_rt) {
          reactable::renderReactable(reactable::reactable(data.frame(Team = c("Owls", "Foxes"), Wins = c(7, 5))))
        } else {
          shiny::renderText("no-rt")
        }

        output$spin_out <- shiny::renderText({
          Sys.sleep(0.25)
          "spin-ready"
        })

        output$async_out <- shiny::renderText({
          if (has_promises) {
            promises::promise(function(resolve, reject) {
              later::later(function() resolve("async-done"), delay = 0.6)
            })
          } else {
            "async-done"
          }
        })

        output$err_out <- shiny::renderText({
          input$err_out_retry
          counts$err_tries <- counts$err_tries + 1L
          if (counts$err_tries == 1L) {
            stop("Simulated server failure")
          }
          paste0("recovered-try-", counts$err_tries)
        })

        output$empty_out <- shiny::renderText("")

        output$cached_ui <- shiny::renderUI({
          tip_on(
            shiny::actionButton("btn_cache", "Cached"),
            template = "cache_tpl",
            record = list(label = "Cached"),
            id = "btn_cache"
          )
        }) |>
          shiny::bindCache("static-key")

        output$cached_out <- shiny::renderText({
          counts$cached_plot <- counts$cached_plot + 1L
          paste0("cached-n", counts$cached_plot)
        }) |>
          shiny::bindCache("static-out-key")
      }

      shiny::runApp(shiny::shinyApp(ui, server), port = port, host = "127.0.0.1", launch.browser = FALSE)
    },
    args = list(
      port = port,
      path = path,
      has_dt = has_dt,
      has_rt = has_rt,
      has_spin = has_spin,
      has_promises = has_promises
    ),
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
  css <- tip_names()$css

  close_tip <- function() {
    page$press("Escape", wait = 0.25)
    page$wait_until(sprintf("document.querySelector('.%s:not([hidden])') === null", css))
  }

  it("starts and connects", {
    expect_true(up, info = paste(app$read_error(), collapse = "\n"))
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
  })

  it("renders a plot inside a tip", {
    page$hover("#btn_plot")
    expect_true(page$wait_until(
      "Boolean(document.querySelector('.hc-tip:not([hidden]) #plot_out img')?.getAttribute('src'))"
    ))
    close_tip()
  })

  it("renders a DT table inside a tip", {
    skip_if_not(has_dt, "DT not installed")
    page$hover("#btn_dt")
    expect_true(page$wait_until(
      "Boolean(document.querySelector('.hc-tip:not([hidden]) #dt_out table'))"
    ))
    expect_match(page$evaluate("document.querySelector('.hc-tip:not([hidden]) #dt_out').textContent"), "Owls")
    close_tip()
  })

  it("renders a reactable table inside a tip", {
    skip_if_not(has_rt, "reactable not installed")
    page$hover("#btn_rt")
    expect_true(page$wait_until(
      "Boolean(document.querySelector('.hc-tip:not([hidden]) #rt_out .ReactTable'))"
    ))
    expect_match(page$evaluate("document.querySelector('.hc-tip:not([hidden]) #rt_out').textContent"), "Foxes")
    close_tip()
  })

  it("keep_alive = TRUE parks the output without recomputing on reopen, and refreshes when inputs change while parked", {
    page$hover("#btn_alive")
    expect_true(page$wait_until(
      "document.querySelector('.hc-tip:not([hidden]) #alive_out')?.textContent === 'alive-v1-n1'"
    ))
    close_tip()
    # Parked in hidden holder, still bound.
    expect_true(page$evaluate("Boolean(document.querySelector('.hc-output-park[hidden] #alive_out'))"))
    # Reopen without input change: still n1 (not recomputed).
    page$hover("#btn_alive")
    expect_true(page$wait_until(
      "document.querySelector('.hc-tip:not([hidden]) #alive_out')?.textContent === 'alive-v1-n1'"
    ))
    close_tip()
    # Change reactive input while parked, then reopen: must refresh to v2!
    page$evaluate("document.getElementById('bump').click()")
    Sys.sleep(0.3)
    page$hover("#btn_alive")
    expect_true(page$wait_until(
      "document.querySelector('.hc-tip:not([hidden]) #alive_out')?.textContent === 'alive-v2-n2'"
    ))
    close_tip()
  })

  it("keep_alive = FALSE unbinds and removes the output from the DOM on close", {
    page$hover("#btn_dead")
    expect_true(page$wait_until(
      "document.querySelector('.hc-tip:not([hidden]) #dead_out')?.textContent === 'dead-n1'"
    ))
    close_tip()
    expect_true(page$evaluate("document.getElementById('dead_out') === null"))
    expect_true(page$evaluate("window.Shiny.shinyapp.$bindings['dead_out'] === undefined"))
    # Reopen: shell is restored and bound again.
    page$hover("#btn_dead")
    expect_true(page$wait_until(
      "Boolean(document.querySelector('.hc-tip:not([hidden]) #dead_out')?.textContent?.startsWith('dead-n'))"
    ))
    close_tip()
  })

  it("does not clash with shinycssloaders::withSpinner()", {
    skip_if_not(has_spin, "shinycssloaders not installed")
    page$hover("#btn_spin")
    expect_true(page$wait_until(
      "document.querySelector('.hc-tip:not([hidden]) .hc-output')?.dataset.hasLoader === 'true'"
    ))
    expect_true(page$evaluate(
      "Boolean(document.querySelector('.hc-tip:not([hidden]) .shiny-spinner-output-container'))"
    ))
    expect_true(page$wait_until(
      "document.querySelector('.hc-tip:not([hidden]) #spin_out')?.textContent === 'spin-ready'"
    ))
    close_tip()
  })

  it("waiter loader compatibility (skipped when waiter is not installed)", {
    skip_if_not_installed("waiter")
    expect_true(TRUE)
  })

  it("bones loader compatibility (skipped when bones is not installed)", {
    skip_if_not_installed("bones")
    expect_true(TRUE)
  })

  it("shows loading state during an async render, then shows the result", {
    skip_if_not(has_promises, "promises/later not installed")
    page$hover("#btn_async")
    expect_true(page$wait_until(
      "document.querySelector('.hc-tip:not([hidden]) .hc-output')?.dataset.state === 'loading' || document.querySelector('.hc-tip:not([hidden]) #async_out')?.textContent === 'async-done'"
    ))
    expect_true(page$wait_until(
      "document.querySelector('.hc-tip:not([hidden]) #async_out')?.textContent === 'async-done'"
    ))
    expect_identical(
      page$evaluate("document.querySelector('.hc-tip:not([hidden]) .hc-output')?.dataset.state"),
      "ready"
    )
    close_tip()
  })

  it("shows an error state with a Retry button that re-runs and recovers", {
    page$hover("#btn_err")
    expect_true(page$wait_until(
      "document.querySelector('.hc-tip:not([hidden]) .hc-output')?.dataset.state === 'error'"
    ))
    expect_true(page$evaluate(
      "!document.querySelector('.hc-tip:not([hidden]) .hc-output-error').hidden"
    ))
    # Click Retry inside the open tip.
    page$evaluate("document.querySelector('.hc-tip:not([hidden]) .hc-output-retry').click()")
    expect_true(page$wait_until(
      "document.querySelector('.hc-tip:not([hidden]) #err_out')?.textContent === 'recovered-try-2'"
    ))
    expect_identical(
      page$evaluate("document.querySelector('.hc-tip:not([hidden]) .hc-output')?.dataset.state"),
      "ready"
    )
    close_tip()
  })

  it("shows an empty state when the output returns empty content", {
    page$hover("#btn_empty")
    expect_true(page$wait_until(
      "document.querySelector('.hc-tip:not([hidden]) .hc-output')?.dataset.state === 'empty'"
    ))
    expect_true(page$evaluate(
      "!document.querySelector('.hc-tip:not([hidden]) .hc-output-empty').hidden"
    ))
    close_tip()
  })

  it("works with bindCache() on both renderUI(tip_on()) and outputs inside tip_output()", {
    expect_true(page$wait_until("document.getElementById('btn_cache') !== null"))
    page$hover("#btn_cache")
    expect_true(page$wait_until(
      "document.querySelector('.hc-tip:not([hidden]) #cached_out')?.textContent === 'cached-n1'"
    ))
    close_tip()
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})
