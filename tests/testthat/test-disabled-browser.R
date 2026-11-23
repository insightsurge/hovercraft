skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# tip_on(when_disabled = ) in a running bslib/Shiny app: a control disabled when the page is built, one switched off and
# on by Shiny's updateActionButton(), by script and by a disabled fieldset, inside a sidebar where the wrapper must not
# change a width. The reason shows by pointer and by keyboard, goes when the control is enabled, and has a lock.
describe("tip_on(when_disabled = ) in a Shiny app", {
  path <- find.package("hovercraft")
  port <- httpuv::randomPort()
  app <- callr::r_bg(
    function(port, path) {
      if (dir.exists(file.path(path, "inst"))) {
        pkgload::load_all(path, quiet = TRUE)
      } else {
        library(hovercraft)
      }
      off <- function(x) htmltools::tagAppendAttributes(x, disabled = "disabled")
      ui <- bslib::page_sidebar(
        title = "when_disabled",
        sidebar = bslib::sidebar(
          id = "side",
          open = TRUE,
          width = 300,
          shiny::actionButton("ref", "Reference", width = "100%"),
          tip_on(off(shiny::actionButton("wide", "Wide", width = "100%")), when_disabled = "Wide reason")
        ),
        shiny::actionButton("before", "Before"),
        tip_on(
          shiny::actionButton("up", "Update"),
          "Runs the **check**",
          when_disabled = "Pick a dataset first",
          id = "up_tip",
          events = "hint"
        ),
        tip_on(off(shiny::actionButton("built", "Built")), "Built tip", when_disabled = "Disabled at build"),
        tip_on(shiny::actionButton("js", "Script"), when_disabled = "Script reason"),
        htmltools::tags$fieldset(
          id = "fs",
          tip_on(shiny::actionButton("in_fs", "Fieldset"), when_disabled = "Fieldset reason")
        ),
        shiny::actionButton("after", "After"),
        shiny::actionButton("enable", "Enable"),
        shiny::actionButton("disable", "Disable"),
        shiny::verbatimTextOutput("clicks"),
        shiny::verbatimTextOutput("hint")
      )
      server <- function(input, output, session) {
        n <- shiny::reactiveVal(0L)
        shiny::observeEvent(input$up, n(n() + 1L))
        shiny::observeEvent(input$enable, shiny::updateActionButton(session, "up", disabled = FALSE))
        shiny::observeEvent(input$disable, shiny::updateActionButton(session, "up", disabled = TRUE))
        output$clicks <- shiny::renderText(paste("clicks:", n()))
        output$hint <- shiny::renderText(paste(unlist(input$up_tip_hint[c("state", "text")]), collapse = " | "))
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
  reason <- tip_attribute("reason")
  wrapper <- function(id) sprintf("document.getElementById('%s').parentElement", id)
  # The wrapper's state as the page sees it: the engine's reason attribute, the tab stop and the role.
  state <- function(id) {
    jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => { const w = %s; return JSON.stringify([w.getAttribute('%s'), w.getAttribute('tabindex'), w.getAttribute('role')]); })()",
      wrapper(id),
      reason
    )))
  }
  press_button <- function(id) page$evaluate(sprintf("document.getElementById('%s').click()", id))
  seen <- function(id, pattern) {
    page$wait_until(sprintf(
      "new RegExp(%s).test(document.getElementById(%s)?.textContent ?? '')",
      jsonlite::toJSON(pattern, auto_unbox = TRUE),
      jsonlite::toJSON(id, auto_unbox = TRUE)
    ))
  }
  # The tab stop the keyboard reaches, after pressing Tab from the button before the one under test.
  tab_to <- function(from) {
    page$evaluate(sprintf("document.getElementById('%s').focus()", from))
    page$press("Tab")
  }

  it("starts and connects", {
    expect_true(up, info = paste(app$read_error(), collapse = "\n"))
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
    expect_true(page$wait_until(sprintf("%s.getAttribute('role') === 'button'", wrapper("built"))))
  })

  it("shows the reason of a control that was disabled when the page was built, by the pointer", {
    expect_identical(page$hover("#built"), "Not available: Disabled at build")
  })

  it("reaches the reason by keyboard: the wrapper is a tab stop while the control is disabled", {
    expect_identical(state("built"), c("Disabled at build", "0", "button"))
    expect_identical(tab_to("up"), "Not available: Disabled at build")
    expect_identical(page$evaluate("document.activeElement.firstElementChild.id"), "built")
  })

  it("gives the reason a lock that adds no words, in the words' colour", {
    page$hover("#built")
    lock <- jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => { const line = document.querySelector('.%s-reason'); const s = getComputedStyle(line, '::before'); return JSON.stringify({ text: line.textContent, content: s.content, mask: s.maskImage || s.webkitMaskImage, width: parseFloat(s.width), colour: s.backgroundColor === getComputedStyle(line).color }); })()",
      tip_names()$css
    )))
    expect_identical(lock$text, "Not available: Disabled at build")
    expect_match(lock$content, "^\"?\"?$")
    expect_match(lock$mask, "^url\\(")
    expect_gt(lock$width, 4)
    expect_true(lock$colour)
  })

  it("does not change the width of a control that fills its column", {
    expect_equal(
      page$evaluate("document.getElementById('wide').getBoundingClientRect().width"),
      page$evaluate("document.getElementById('ref').getBoundingClientRect().width")
    )
    expect_equal(
      page$evaluate("document.getElementById('wide').parentElement.getBoundingClientRect().width"),
      page$evaluate("document.getElementById('ref').getBoundingClientRect().width")
    )
  })

  it("has no reason and no tab stop while the control is enabled, and its own tip shows", {
    expect_equal(state("up"), rep(NA, 3L))
    expect_identical(page$hover("#up"), "Runs the check")
    expect_identical(tab_to("before"), "Runs the check")
    expect_identical(page$evaluate("document.activeElement.id"), "up")
  })

  it("shows the reason when Shiny switches the control off, and takes it away when it is switched on", {
    press_button("disable")
    expect_true(page$wait_until(sprintf("%s.getAttribute('role') === 'button'", wrapper("up"))))
    expect_identical(state("up"), c("Pick a dataset first", "0", "button"))
    expect_identical(page$hover("#up"), "Not available: Pick a dataset first")
    expect_true(seen("hint", "^shown \\| Not available: Pick a dataset first"))
    press_button("enable")
    expect_true(page$wait_until(sprintf("%s.getAttribute('role') === null", wrapper("up"))))
    expect_equal(state("up"), rep(NA, 3L))
    expect_identical(page$hover("#up"), "Runs the check")
  })

  it("lets an enabled control be clicked again: the engine blocks the clicks of a control that still has a reason", {
    press_button("up")
    expect_true(seen("clicks", "clicks: 1"))
    page$click("#up")
    expect_true(seen("clicks", "clicks: 2"))
  })

  it("follows a control that a script switches off and on", {
    page$evaluate("document.getElementById('js').disabled = true")
    expect_true(page$wait_until(sprintf("%s.getAttribute('role') === 'button'", wrapper("js"))))
    expect_identical(page$hover("#js"), "Not available: Script reason")
    page$evaluate("document.getElementById('js').disabled = false")
    expect_true(page$wait_until(sprintf("%s.getAttribute('role') === null", wrapper("js"))))
    expect_equal(state("js"), rep(NA, 3L))
    expect_length(page$hover("#js"), 0L)
  })

  it("follows a control that a disabled fieldset switches off and on", {
    page$evaluate("document.getElementById('fs').disabled = true")
    expect_true(page$wait_until(sprintf("%s.getAttribute('role') === 'button'", wrapper("in_fs"))))
    expect_identical(page$hover("#in_fs"), "Not available: Fieldset reason")
    page$evaluate("document.getElementById('fs').disabled = false")
    expect_true(page$wait_until(sprintf("%s.getAttribute('role') === null", wrapper("in_fs"))))
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})
