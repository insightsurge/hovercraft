skip_if_no_browser()

# A Shiny app whose page has no tip in it at all: the engine can only arrive with what renderUI() sends later.
describe("a Shiny app where only renderUI() brings the engine", {
  app <- start_app(function() {
    ui <- shiny::fluidPage(shiny::uiOutput("out"))
    server <- function(input, output, session) {
      output$out <- shiny::renderUI(
        tip_on(htmltools::tags$button(id = "later", "Later"), "Only from renderUI")
      )
    }
    shiny::shinyApp(ui, server)
  })
  url <- app$url
  page <- local_page(url, environment())

  it("starts", {
    expect_true(app$proc$is_alive())
  })

  it("shows the tip on a tag that renderUI() drew, with no console error", {
    expect_true(page$wait_until("document.getElementById('later') !== null"))
    expect_true(page$wait_until(sprintf("typeof %s === 'object'", tip_names()$global)))
    expect_identical(page$hover("#later"), "Only from renderUI")
    expect_identical(page$problems(), character())
  })
})
