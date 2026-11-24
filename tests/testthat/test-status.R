describe("the badge status stylesheet", {
  css <- tip_status_css()
  badge <- paste0(".", tip_names()$css, "-badge")

  it("colours each of the four statuses from Bootstrap's variables, with a fallback, light and dark", {
    expect_no_match(css, "<style", fixed = TRUE)
    for (status in tip_statuses) {
      rule <- sprintf("%s[data-tone=\"%s\"]", badge, status)
      expect_match(css, rule, fixed = TRUE)
      expect_match(css, sprintf("var(--bs-%s-bg-subtle,#", status), fixed = TRUE)
      expect_match(css, sprintf("[data-bs-theme=\"dark\"] %s", rule), fixed = TRUE)
    }
    expect_equal(lengths(regmatches(css, gregexpr("[data-tone=", css, fixed = TRUE))), 2L * length(tip_statuses))
  })

  it("is the file status.css of the runtime dependency, which the generator holds to", {
    file <- system.file("runtime", "status.css", package = "hovercraft")
    # The file is formatted by Biome like the package's other CSS; the rules are the same.
    squash <- function(x) gsub(";}", "}", gsub("[[:space:]]+", "", paste(x, collapse = "")), fixed = TRUE)
    expect_identical(squash(readLines(file, warn = FALSE)), squash(css))
  })

  it("comes with the runtime dependency, on every page that has a tip", {
    runtime <- use_hovercraft()[[2]]
    expect_true("status.css" %in% runtime$stylesheet)
    got <- tip_on(htmltools::tags$button("Run"), "Runs")
    deps <- htmltools::findDependencies(got)
    expect_true("status.css" %in% deps[[length(deps)]]$stylesheet)
  })
})
