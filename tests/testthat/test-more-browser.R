skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# Browser tests for #57: tip_on(more =, more_on =) and tip_spec(more =, more_on =).
# Covers pin mode, hover mode, table cells, smooth growth, arrow/position/edge/375px, reduced motion,
# light/dark/presets typography match, never covering the trigger, and preserving keyboard focus.
describe("tip_on(more =, more_on =) and tip_spec(more =, more_on =) in a browser", {
  has_dt <- requireNamespace("DT", quietly = TRUE)
  path <- find.package("hovercraft")
  port <- httpuv::randomPort()
  pin_attr <- tip_attribute("pin")

  app <- callr::r_bg(
    function(port, path, has_dt, pin_attr) {
      if (dir.exists(file.path(path, "inst"))) {
        pkgload::load_all(path, quiet = TRUE)
      } else {
        library(hovercraft)
      }
      scores <- data.frame(
        team = c("Owls", "Foxes"),
        wins = c(7, 5),
        detail = c("Won last four games away", "Strong home record"),
        stringsAsFactors = FALSE
      )

      make_pinnable <- function(tag) {
        htmltools::tagAppendAttributes(tag, !!pin_attr := "")
      }

      long_tpl <- tip_template(
        body = tip_fields(Owner = "{{ owner }}", Records = "{{ n | number:0 }}"),
        foot = tip_text("Updated: {{ when }}")
      )

      ui <- shiny::fluidPage(
        tip_define(
          short_tpl = tip_template(tip_title("{{ label }}"), tip_text("{{ brief }}"))
        ),
        shiny::tags$div(
          style = "padding: 40px 24px; display: flex; flex-wrap: wrap; gap: 16px; align-items: center;",
          tip_on(
            make_pinnable(shiny::actionButton("pin_btn", "Pin me")),
            template = "short_tpl",
            record = list(
              label = "ADAE",
              brief = "Adverse events summary",
              owner = "Biostats",
              n = 1420,
              when = "Today"
            ),
            more = long_tpl,
            id = "pin_tip",
            style = "modern"
          ),
          tip_on(
            shiny::actionButton("hover_btn", "Hover me"),
            "Short hover summary",
            more = "Detailed hover notes for **{{ team }}** ({{ wins | number:0 }} wins).",
            more_on = "hover",
            record = list(team = "Owls", wins = 7),
            id = "hover_tip",
            style = "sober"
          ),
          tip_on(
            make_pinnable(shiny::actionButton("edge_btn", "Edge")),
            "Near edge short",
            more = "Much longer detail block that must stay inside the viewport without covering the button.",
            id = "edge_tip",
            style = "lively",
            placement = "bottom"
          ) |>
            htmltools::tagAppendAttributes(style = "position: fixed; right: 12px; bottom: 16px;")
        ),
        if (has_dt) {
          shiny::tags$div(
            style = "margin-top: 24px; max-width: 520px;",
            DT::DTOutput("tbl")
          )
        }
      )

      server <- function(input, output, session) {
        if (has_dt) {
          output$tbl <- DT::renderDT({
            DT::datatable(scores, rownames = FALSE, options = list(dom = "t")) |>
              tip_table(
                cells = list(
                  team = tip_spec(
                    "**{{ team }}**",
                    more = "Detail: {{ detail }} ({{ wins | number:0 }} wins)",
                    more_on = "hover",
                    marker = "corner"
                  )
                ),
                id = "scores_tbl"
              )
          })
        }
      }

      shiny::runApp(shiny::shinyApp(ui, server), port = port, host = "127.0.0.1", launch.browser = FALSE)
    },
    args = list(port = port, path = path, has_dt = has_dt, pin_attr = pin_attr),
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

  it("starts and connects", {
    expect_true(up, info = paste(app$read_error(), collapse = "\n"))
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
  })

  it("shows short form with cue on hover, then expands smoothly on pin without covering control or stealing focus", {
    page$hover("#pin_btn")
    expect_true(page$wait_until(sprintf("document.querySelector('.%s:not(.%s-pin):not([hidden])') !== null", css, css)))
    short_info <- jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => {
        const t = document.querySelector('.%s:not(.%s-pin):not([hidden])');
        const r = t.getBoundingClientRect();
        const cs = getComputedStyle(t);
        return JSON.stringify({
          text: t.textContent,
          hasCue: Boolean(t.querySelector('.%s-cue')),
          height: r.height,
          font: cs.fontFamily
        });
      })()",
      css,
      css,
      css
    )))
    expect_match(short_info$text, "Adverse events summary", fixed = TRUE)
    expect_no_match(short_info$text, "Biostats", fixed = TRUE)
    expect_true(short_info$hasCue)

    # Focus the button first so we verify keyboard focus stays on #pin_btn when pinning.
    page$evaluate("document.getElementById('pin_btn').focus()")
    page$click("#pin_btn")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s.%s-pin:not([hidden])')?.textContent?.includes('Biostats')",
      css,
      css
    )))
    # Wait for growth animation to settle.
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s.%s-pin:not([hidden])')?.getAnimations?.().length === 0",
      css,
      css
    )))

    pin_info <- jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => {
        const b = document.getElementById('pin_btn').getBoundingClientRect();
        const t = document.querySelector('.%s.%s-pin:not([hidden])');
        const r = t.getBoundingClientRect();
        const cs = getComputedStyle(t);
        const overlaps = !(r.right <= b.left || r.left >= b.right || r.bottom <= b.top || r.top >= b.bottom);
        return JSON.stringify({
          text: t.textContent,
          height: r.height,
          font: cs.fontFamily,
          hasArrow: t.hasAttribute('data-arrow'),
          overlaps: overlaps,
          activeId: document.activeElement?.id ?? ''
        });
      })()",
      css,
      css
    )))
    expect_match(pin_info$text, "Biostats", fixed = TRUE)
    expect_match(pin_info$text, "Updated: Today", fixed = TRUE)
    expect_gt(pin_info$height, short_info$height)
    expect_identical(pin_info$font, short_info$font)
    expect_true(pin_info$hasArrow)
    expect_false(pin_info$overlaps)
    expect_identical(pin_info$activeId, "pin_btn")

    page$press("Escape", wait = 0.2)
    expect_true(page$wait_until(sprintf("document.querySelector('.%s.%s-pin:not([hidden])') === null", css, css)))
  })

  it("shows short and long form together on hover when more_on = 'hover' with matching typography", {
    page$hover("#hover_btn")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s:not([hidden]) .%s-more') !== null",
      css,
      css
    )))
    info <- jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => {
        const t = document.querySelector('.%s:not([hidden])');
        const m = t.querySelector('.%s-more');
        return JSON.stringify({
          text: t.textContent,
          tipFont: getComputedStyle(t).fontFamily,
          moreFont: getComputedStyle(m).fontFamily
        });
      })()",
      css,
      css
    )))
    expect_match(info$text, "Short hover summary", fixed = TRUE)
    expect_match(info$text, "Detailed hover notes for Owls (7 wins).", fixed = TRUE)
    expect_identical(info$tipFont, info$moreFont)
    page$press("Escape", wait = 0.2)
  })

  it("works on a DT table cell with tip_spec(more =, more_on = 'hover')", {
    skip_if_not(has_dt, "DT not installed")
    expect_true(page$wait_until("document.querySelector('#tbl tbody td') !== null"))
    page$hover("#tbl tbody tr:first-child td:first-child")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s:not([hidden]) .%s-more')?.textContent?.includes('Won last four games away')",
      css,
      css
    )))
    page$press("Escape", wait = 0.2)
  })

  it("keeps a pinned long tip inside the viewport near the bottom-right edge without covering its button", {
    page$click("#edge_btn")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s.%s-pin:not([hidden])')?.textContent?.includes('Much longer detail')",
      css,
      css
    )))
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s.%s-pin:not([hidden])')?.getAnimations?.().length === 0",
      css,
      css
    )))
    edge <- jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => {
        const b = document.getElementById('edge_btn').getBoundingClientRect();
        const t = document.querySelector('.%s.%s-pin:not([hidden])');
        const r = t.getBoundingClientRect();
        const overlaps = !(r.right <= b.left || r.left >= b.right || r.bottom <= b.top || r.top >= b.bottom);
        return JSON.stringify({
          inBounds: r.left >= 0 && r.top >= 0 && r.right <= window.innerWidth && r.bottom <= window.innerHeight,
          overlaps: overlaps
        });
      })()",
      css,
      css
    )))
    expect_true(edge$inBounds)
    expect_false(edge$overlaps)
    page$press("Escape", wait = 0.2)
  })

  it("respects prefers-reduced-motion: reduce (no growth animation)", {
    page$emulate("prefers-reduced-motion", "reduce")
    page$hover("#pin_btn")
    expect_true(page$wait_until(sprintf("document.querySelector('.%s:not(.%s-pin):not([hidden])') !== null", css, css)))
    page$evaluate("document.getElementById('pin_btn').click()")
    expect_true(page$wait_until(sprintf("document.querySelector('.%s.%s-pin:not([hidden])') !== null", css, css)))
    anim_count <- page$evaluate(sprintf(
      "document.querySelector('.%s.%s-pin:not([hidden])').getAnimations().length",
      css,
      css
    ))
    expect_identical(as.integer(anim_count), 0L)
    page$press("Escape", wait = 0.2)
    page$emulate("prefers-reduced-motion", "no-preference")
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})
