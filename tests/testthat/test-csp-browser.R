skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# A page under the strictest policy a package can be asked to live with: no inline script, no inline style, nothing but
# files from the page's own origin. Tips must work, and the browser must report no violation.
csp <- "script-src 'self'; style-src 'self'"

# What the browser reports about the policy, from before the page's own scripts run: every violation, as the directive,
# what it blocked and a sample of it.
watch <- paste(
  "window.violations = [];",
  "document.addEventListener('securitypolicyviolation', (e) => {",
  "window.violations.push(e.violatedDirective + ' ' + e.blockedURI + ' ' + (e.sample || ''));",
  "});"
)
violations <- function(page) jsonlite::fromJSON(page$evaluate("JSON.stringify(window.violations ?? [])"))

# Serves a page built by `build()` (a function that returns the page's tags, run in the server process) from a folder
# with the policy as a header, and gives back the URL.
serve_page <- function(build, policy = csp, env = parent.frame()) {
  path <- find.package("hovercraft")
  port <- httpuv::randomPort()
  site <- withr::local_tempdir(.local_envir = env)
  app <- callr::r_bg(
    function(port, path, site, build, csp) {
      if (dir.exists(file.path(path, "inst"))) {
        pkgload::load_all(path, quiet = TRUE)
      } else {
        library(hovercraft)
      }
      writeLines(
        c(".page { width: 400px; } .w-50 { width: 50%; } .w-100 { width: 100%; }"),
        file.path(site, "extra.css")
      )
      htmltools::save_html(build(), file.path(site, "index.html"), background = NULL, libdir = "lib")
      server <- httpuv::startServer(
        "127.0.0.1",
        port,
        list(
          staticPaths = list(
            "/" = httpuv::staticPath(site, indexhtml = TRUE, headers = list("Content-Security-Policy" = csp))
          )
        )
      )
      on.exit(server$stop())
      while (TRUE) {
        httpuv::service(100)
      }
    },
    args = list(port = port, path = path, site = site, build = build, csp = policy),
    supervise = TRUE
  )
  withr::defer(app$kill(), envir = env)
  url <- paste0("http://127.0.0.1:", port, "/")
  up <- wait_for_app(app, url)
  list(url = url, up = up, error = function() paste(app$read_error(), collapse = "\n"))
}

describe("a page under a strict Content-Security-Policy", {
  made <- serve_page(function() {
    tip_define(
      card = tip_template(
        tip_title("{{ label }}"),
        tip_badge("{{ state }}", status = "success"),
        tip_text("Rows: {{ n }}")
      )
    ) |>
      htmltools::tagList(
        htmltools::tags$head(
          htmltools::tags$title("csp"),
          htmltools::tags$link(rel = "stylesheet", href = "extra.css")
        ),
        use_hovercraft(style = tip_style("dense", theme = list("--hovercraft-tip-radius" = "11px"))),
        htmltools::tags$div(
          class = "page",
          tip_on(htmltools::tags$button("Plain", id = "plain", tabindex = "0"), "A plain tip"),
          tip_on(
            htmltools::tags$button("Looked", id = "looked"),
            "A tip with a look of its own",
            style = tip_style(theme = list(radius = 13), motion = "none", gap = 12)
          ),
          tip_on(
            htmltools::tags$button("Card", id = "card"),
            template = "card",
            record = list(label = "Query", state = "ok", n = 3)
          ),
          tip_on(
            htmltools::tags$button("More", id = "more"),
            "Short",
            more = tip_template(tip_text("The long form")),
            more_on = "hover"
          ),
          tip_on(
            htmltools::tags$button("Pinnable", id = "pinnable"),
            "Can be pinned",
            id = "pinnable_tip",
            pin = TRUE
          ),
          tip_on(
            htmltools::tags$button("Off", id = "off", disabled = NA, class = "w-50"),
            when_disabled = "Switched off"
          ),
          # A width written in markup is a style attribute, which the policy blocks; the layer carries it as data.
          tip_on(
            htmltools::tags$button("Wide", id = "wide", disabled = NA, style = "width: 200px"),
            when_disabled = "Switched off too"
          )
        )
      )
  })
  page <- local_page(made$url, environment(), init = watch)
  css <- tip_names()$css
  hint <- sprintf(".%s:not(.%s-pin):not([hidden])", css, css)
  it("starts", {
    expect_true(made$up, info = made$error())
    expect_true(page$wait_until(sprintf("typeof %s === 'object'", tip_names()$global)))
  })

  it("catches a violation when there is one: an inline style added by hand is reported once", {
    page$evaluate("document.head.insertAdjacentHTML('beforeend', '<style>.x { color: red }</style>'); 0")
    expect_true(page$wait_until("window.violations.length === 1"))
    expect_match(violations(page), "^style-src-elem inline")
    page$evaluate("window.violations = []; document.head.lastElementChild.remove(); 0")
  })

  it("reports no violation of the policy", {
    expect_true(page$wait_until("window.HovercraftRuntime !== undefined"))
    expect_length(violations(page), 0L)
  })

  it("shows a plain tip", {
    expect_identical(page$hover("#plain"), "A plain tip")
  })

  it("applies the page's style: its preset, and a theme as CSS", {
    expect_identical(
      page$evaluate(sprintf("document.documentElement.getAttribute('%s')", tip_attribute("preset"))),
      "dense"
    )
    page$hover("#plain")
    expect_identical(
      page$evaluate(sprintf("getComputedStyle(document.querySelector('%s')).borderRadius", hint)),
      "11px"
    )
  })

  it("applies a tag's own style", {
    page$hover("#looked")
    expect_identical(
      page$evaluate(sprintf("getComputedStyle(document.querySelector('%s')).borderRadius", hint)),
      "13px"
    )
  })

  it("draws a template defined by tip_define(), with the badge's status colours", {
    expect_match(paste(page$hover("#card"), collapse = " "), "Query.*ok.*Rows: 3")
    expect_true(page$evaluate(sprintf(
      "getComputedStyle(document.querySelector('%s .%s-badge')).backgroundColor !== 'rgba(0, 0, 0, 0)'",
      hint,
      css
    )))
  })

  it("draws the long form of a tip", {
    expect_match(paste(page$hover("#more"), collapse = " "), "long form")
  })

  it("draws the pin button, which is the pin layer's own script and style", {
    page$hover("#pinnable")
    expect_true(page$wait_until("document.querySelector('.hovercraft-pin-strip-button') !== null"))
  })

  it("keeps the width a disabled control had, and shows its reason", {
    expect_identical(page$hover("#off"), "Not available: Switched off")
    width <- page$evaluate(
      "document.getElementById('off').parentElement.getBoundingClientRect().width / document.querySelector('.page').getBoundingClientRect().width"
    )
    expect_equal(width, 0.5, tolerance = 0.02)
  })

  it("applies a layout that was written in a style attribute and blocked, from data, to the wrapper and the control", {
    page$hover("#wide")
    expect_true(page$wait_until("document.getElementById('wide').parentElement.getBoundingClientRect().width === 200"))
    expect_equal(page$evaluate("document.getElementById('wide').getBoundingClientRect().width"), 200)
    expect_length(violations(page), 0L)
  })

  it("still reports no violation of the policy, and no error in the console", {
    expect_length(violations(page), 0L)
    expect_identical(page$problems(), character())
  })
})

# The same policy on a Shiny page. Shiny and its own packages may write inline markup that a strict policy blocks; what
# is asked here is that hovercraft adds nothing to what a page without a tip already reports, and that the layers that
# need Shiny (the server calls and the inputs) work.
describe("a Shiny page under a strict Content-Security-Policy", {
  skip_if_not_installed("shiny")
  path <- find.package("hovercraft")
  port <- httpuv::randomPort()
  app <- callr::r_bg(
    function(port, path, csp) {
      if (dir.exists(file.path(path, "inst"))) {
        pkgload::load_all(path, quiet = TRUE)
      } else {
        library(hovercraft)
      }
      ui <- function(req) {
        if (grepl("plain", req$QUERY_STRING, fixed = TRUE)) {
          return(shiny::fluidPage(shiny::actionButton("run", "Run")))
        }
        shiny::fluidPage(
          use_hovercraft(style = tip_style("dense")),
          tip_on(
            shiny::actionButton("run", "Run"),
            "Runs the **check**",
            id = "run_tip",
            events = c("hint", "pin"),
            pin = TRUE
          ),
          shiny::actionButton("show", "Show"),
          shiny::actionButton("late", "Late"),
          shiny::actionButton("insert", "Insert"),
          shiny::uiOutput("late_ui"),
          htmltools::tags$div(id = "slot"),
          shiny::verbatimTextOutput("seen")
        )
      }
      server <- function(input, output, session) {
        shiny::observeEvent(input$show, tip_show("run_tip"))
        output$seen <- shiny::renderText(paste(input$run_tip_hint$state, input$run_tip_hint$by))
        # A template, a tag with a theme and a long form that arrive after the page has loaded.
        late <- function(id, radius) {
          htmltools::tagList(
            tip_define(!!paste0(id, "_card") := tip_template(tip_title("{{ label }}"), tip_text("Late: {{ n }}"))),
            tip_on(
              shiny::actionButton(paste0(id, "_btn"), paste("Button", id)),
              template = paste0(id, "_card"),
              record = list(label = id, n = 7),
              style = tip_style(theme = list(radius = radius)),
              more = tip_template(tip_text(paste("The long form of", id))),
              more_on = "hover"
            )
          )
        }
        output$late_ui <- shiny::renderUI({
          shiny::req(input$late)
          late("rendered", "15px")
        })
        shiny::observeEvent(input$insert, shiny::insertUI("#slot", ui = late("inserted", "17px")))
      }
      app <- shiny::shinyApp(ui, server)
      handle <- app$httpHandler
      app$httpHandler <- function(req) {
        response <- handle(req)
        if (inherits(response, "httpResponse")) {
          response$headers[["Content-Security-Policy"]] <- csp
        }
        response
      }
      shiny::runApp(app, port = port, host = "127.0.0.1", launch.browser = FALSE)
    },
    args = list(port = port, path = path, csp = csp),
    supervise = TRUE
  )
  withr::defer(app$kill())
  url <- paste0("http://127.0.0.1:", port, "/")
  up <- wait_for_app(app, url)
  # What Shiny alone reports under the policy, to hold the page with tips to.
  plain <- local_page(paste0(url, "?plain"), environment(), init = watch)
  plain$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true")
  baseline <- violations(plain)
  page <- local_page(url, environment(), init = watch)
  css <- tip_names()$css

  it("starts and connects", {
    expect_true(up, info = paste(app$read_error(), collapse = "\n"))
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
  })

  it("adds no violation to what a page without a tip reports", {
    expect_true(page$wait_until("window.HovercraftRuntime !== undefined"))
    expect_identical(violations(page), baseline)
  })

  it("shows the hint and sends the hint input, which is the Shiny layer", {
    expect_identical(page$hover("#run"), "Runs the check")
    expect_true(page$wait_until("/shown/.test(document.getElementById('seen').textContent)"))
  })

  it("shows a tip when the server asks, and draws the pin button", {
    page$mouse(5, 5, FALSE)
    expect_true(page$wait_closed())
    page$evaluate("document.getElementById('show').click()")
    expect_true(page$wait_until(sprintf("document.querySelector('.%s:not(.%s-pin):not([hidden])') !== null", css, css)))
    expect_true(page$wait_until("document.querySelector('.hovercraft-pin-strip-button') !== null"))
    # A hint the server asked for stays until it is closed.
    page$press("Escape")
    expect_true(page$wait_until(sprintf("document.querySelector('.%s:not(.%s-pin):not([hidden])') === null", css, css)))
  })

  hint_text <- function() page$shown()
  radius <- function() {
    page$evaluate(sprintf(
      "getComputedStyle(document.querySelector('.%s:not(.%s-pin):not([hidden])')).borderRadius",
      css,
      css
    ))
  }

  it("draws a template, a theme and a long form that arrive through renderUI() after the page has loaded", {
    page$mouse(5, 5, FALSE)
    page$evaluate("document.getElementById('late').click()")
    expect_true(page$wait_until("document.getElementById('rendered_btn') !== null"))
    expect_match(paste(page$hover("#rendered_btn"), collapse = " "), "rendered.*Late: 7")
    expect_identical(radius(), "15px")
    expect_match(paste(hint_text(), collapse = " "), "long form of rendered")
    expect_identical(violations(page), baseline)
  })

  it("draws them when they arrive through insertUI() too", {
    page$mouse(5, 5, FALSE)
    expect_true(page$wait_closed())
    page$evaluate("document.getElementById('insert').click()")
    expect_true(page$wait_until("document.getElementById('inserted_btn') !== null"))
    expect_match(paste(page$hover("#inserted_btn"), collapse = " "), "inserted.*Late: 7")
    expect_identical(radius(), "17px")
    expect_match(paste(hint_text(), collapse = " "), "long form of inserted")
    expect_identical(violations(page), baseline)
  })
})

# The three small icons that the package draws from `data:` images, under a policy that has `default-src` (which covers
# `img-src` too). The help page of use_hovercraft() lists them.
describe("the icons drawn from data: images, under a policy with default-src", {
  build <- function() {
    htmltools::tagList(
      htmltools::tags$head(htmltools::tags$title("icons")),
      use_hovercraft(),
      htmltools::tags$div(
        tip_on(htmltools::tags$button("Marked", id = "marked"), "A marked tip", marker = "icon"),
        tip_on(htmltools::tags$button("Pinnable", id = "pinnable"), "Pins", id = "pin_tip", pin = TRUE),
        tip_on(htmltools::tags$button("Off", id = "off", disabled = NA), when_disabled = "Switched off")
      )
    )
  }
  strict <- "default-src 'self'"
  allowed <- "default-src 'self'; img-src 'self' data:"

  # Hovers each tag that draws an icon, and gives back what the browser reported.
  reported <- function(policy) {
    made <- serve_page(build, policy = policy, env = parent.frame())
    page <- local_page(made$url, parent.frame(), init = watch)
    page$wait_until(sprintf("typeof %s === 'object'", tip_names()$global))
    for (id in c("marked", "pinnable", "off")) {
      page$hover(paste0("#", id))
    }
    page$wait_until("document.querySelector('.hovercraft-pin-strip-button') !== null")
    page$hover("#off")
    list(violations = violations(page), page = page)
  }

  it("raise an img-src violation when the policy does not allow data: images, which is why the help names them", {
    got <- reported(strict)
    expect_true(any(grepl("^img-src", got$violations)))
  })

  it("raise none when the policy allows them, and are drawn", {
    got <- reported(allowed)
    expect_length(got$violations, 0L)
    # The marker's icon is a mask over the tag's last word; the lock is a mask on the reason line.
    expect_match(
      got$page$evaluate("getComputedStyle(document.getElementById('marked'), '::after').maskImage"),
      "^url\\(",
    )
    expect_match(
      got$page$evaluate(sprintf(
        "getComputedStyle(document.querySelector('.%s-reason'), '::before').maskImage",
        tip_names()$css
      )),
      "^url\\("
    )
  })
})
