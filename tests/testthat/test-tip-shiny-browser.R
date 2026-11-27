skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# A Shiny app whose page has no tip in it at all: the engine can only arrive with what renderUI() sends later.
describe("a Shiny app where only renderUI() brings the engine", {
  path <- find.package("hovercraft")
  port <- httpuv::randomPort()
  app <- callr::r_bg(
    function(port, path) {
      if (dir.exists(file.path(path, "inst"))) {
        pkgload::load_all(path, quiet = TRUE)
      } else {
        library(hovercraft)
      }
      ui <- shiny::fluidPage(shiny::uiOutput("out"))
      server <- function(input, output, session) {
        output$out <- shiny::renderUI(
          tip_on(htmltools::tags$button(id = "later", "Later"), "Only from renderUI")
        )
      }
      shiny::runApp(shiny::shinyApp(ui, server), port = port, host = "127.0.0.1", launch.browser = FALSE)
    },
    args = list(port = port, path = path),
    supervise = TRUE
  )
  withr::defer(app$kill())
  url <- paste0("http://127.0.0.1:", port)
  up <- wait_for_app(app, url)
  page <- local_page(url, environment())

  it("starts", {
    expect_true(up, info = paste(app$read_error(), collapse = "\n"))
  })

  it("shows the tip on a tag that renderUI() drew, with no console error", {
    expect_true(page$wait_until("document.getElementById('later') !== null"))
    expect_true(page$wait_until(sprintf("typeof %s === 'object'", tip_names()$global)))
    expect_identical(page$hover("#later"), "Only from renderUI")
    expect_identical(page$problems(), character())
  })
})
