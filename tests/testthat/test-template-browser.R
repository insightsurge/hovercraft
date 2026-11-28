skip_if_no_browser()

# Browser tests for #65:
# - head, body and foot slots rendered in order with footer divider
# - two copies of one Shiny module keep their own templates without overwriting each other
# - tip_badge(status = "{{ col }}") sets data-tone from each item's record (and plain badge for unknown values)
describe("template slots, module-safe names and dynamic badge status in a browser", {
  app <- start_app(function() {
    card_ui <- function(id, heading, footer) {
      ns <- shiny::NS(id)
      shiny::tagList(
        tip_define(
          !!ns("card") := tip_template(
            head = list(
              tip_title(paste0(heading, ": {{ label }}")),
              tip_badge("{{ status }}", status = "{{ tone }}")
            ),
            body = tip_fields(Score = "{{ score | number:0 }}"),
            foot = tip_text(footer)
          )
        ),
        tip_on(
          shiny::actionButton(ns("btn"), heading),
          template = ns("card"),
          record = list(
            label = paste0(heading, " item"),
            status = if (id == "m1") "Pass" else "Alert",
            tone = if (id == "m1") "success" else "danger",
            score = if (id == "m1") 98 else 42
          ),
          id = ns("btn_tip")
        )
      )
    }

    ui <- shiny::fluidPage(
      tip_define(
        badge_tpl = tip_template(
          head = tip_title("{{ label }}"),
          body = tip_badge("{{ label }}", status = "{{ tone }}"),
          foot = tip_text("Source: {{ source }}")
        )
      ),
      card_ui("m1", "Module One", "Footer from copy one"),
      card_ui("m2", "Module Two", "Footer from copy two"),
      tip_on(
        shiny::actionButton("warn_btn", "Warning"),
        template = "badge_tpl",
        record = list(label = "Check", tone = "warning", source = "Audit"),
        id = "warn_tip"
      ),
      tip_on(
        shiny::actionButton("plain_btn", "Unknown"),
        template = "badge_tpl",
        record = list(label = "Neutral", tone = "custom", source = "Audit"),
        id = "plain_tip"
      )
    )

    shiny::shinyApp(ui, function(input, output, session) {})
  })
  url <- app$url
  page <- local_page(url, environment())
  css <- tip_names()$css

  it("starts and connects", {
    expect_true(app$proc$is_alive())
    expect_true(page$wait_connected())
  })

  it("keeps distinct templates for two copies of one Shiny module and renders head, body and foot slots", {
    page$hover("#m1-btn")
    expect_true(page$wait_until(sprintf("document.querySelector('.%s:not([hidden])') !== null", css)))
    m1_text <- page$evaluate(sprintf("document.querySelector('.%s:not([hidden])').textContent", css))
    expect_match(m1_text, "Module One: Module One item", fixed = TRUE)
    expect_match(m1_text, "Footer from copy one", fixed = TRUE)
    expect_identical(
      page$evaluate(sprintf("document.querySelector('.%s:not([hidden]) .hc-tip-badge')?.dataset.tone", css)),
      "success"
    )
    expect_true(page$evaluate(
      sprintf("document.querySelector('.%s:not([hidden]) .hc-slot-divider') !== null", css)
    ))

    page$hover("#m2-btn")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s:not([hidden])')?.textContent?.includes('Module Two: Module Two item')",
      css
    )))
    m2_text <- page$evaluate(sprintf("document.querySelector('.%s:not([hidden])').textContent", css))
    expect_match(m2_text, "Footer from copy two", fixed = TRUE)
    expect_identical(
      page$evaluate(sprintf("document.querySelector('.%s:not([hidden]) .hc-tip-badge')?.dataset.tone", css)),
      "danger"
    )
  })

  it("colours a badge from a record placeholder for valid statuses and leaves unknown values plain", {
    page$hover("#warn_btn")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s:not([hidden]) .hc-tip-badge')?.dataset.tone === 'warning'",
      css
    )))

    page$hover("#plain_btn")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s:not([hidden])')?.textContent?.includes('Neutral')",
      css
    )))
    expect_true(page$evaluate(
      sprintf("document.querySelector('.%s:not([hidden]) .hc-tip-badge')?.dataset.tone === undefined", css)
    ))
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})
