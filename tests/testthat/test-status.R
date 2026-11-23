describe("the badge status stylesheet", {
  css <- tip_status_css()
  badge <- paste0(".", tip_names()$css, "-badge")

  it("colours each of the four statuses from Bootstrap's variables, with a fallback, light and dark", {
    expect_match(css, "^<style>.*</style>$")
    for (status in tip_statuses) {
      rule <- sprintf("%s[data-tone=\"%s\"]", badge, status)
      expect_match(css, rule, fixed = TRUE)
      expect_match(css, sprintf("var(--bs-%s-bg-subtle,#", status), fixed = TRUE)
      expect_match(css, sprintf("[data-bs-theme=\"dark\"] %s", rule), fixed = TRUE)
    }
    expect_equal(lengths(regmatches(css, gregexpr("[data-tone=", css, fixed = TRUE))), 2L * length(tip_statuses))
  })

  it("comes with the engine's dependency, on every page that has a tip", {
    expect_match(tip_dependency()$head, css, fixed = TRUE)
    expect_match(tip_dependency()$head, paste0(tip_names()$global, ".attach(document)"), fixed = TRUE)
    got <- tip_on(htmltools::tags$button("Run"), "Runs")
    expect_match(htmltools::findDependencies(got)[[1]]$head, "data-tone", fixed = TRUE)
  })
})
