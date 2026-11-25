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
    # Each status has its light rule, two dark rules (the page's `data-bs-theme` and the tip's own mode) and one in the
    # operating system's dark mode.
    expect_equal(lengths(regmatches(css, gregexpr("[data-tone=", css, fixed = TRUE))), 4L * length(tip_statuses))
  })

  it("is dark in the operating system's dark mode when the page has no theme, and in the tip's own dark mode", {
    expect_match(css, "[data-hovercraft-mode=\"dark\"] ", fixed = TRUE)
    expect_match(css, "@media (prefers-color-scheme:dark){:root:not([data-bs-theme])", fixed = TRUE)
    # A tip that is forced to light mode stays light in a dark system.
    expect_match(css, ":not([data-hovercraft-mode=\"light\"])", fixed = TRUE)
  })

  it("is dark in the operating system's dark mode for the output error, and logical for the table panel", {
    shiny <- paste(readLines(system.file("runtime", "shiny.css", package = "hovercraft")), collapse = "\n")
    expect_match(shiny, "@media (prefers-color-scheme: dark)", fixed = TRUE)
    expect_match(shiny, ":root:not([data-bs-theme]) .hc-output-error", fixed = TRUE)
    expect_match(shiny, "[data-hovercraft-mode=\"dark\"] .hc-output-error", fixed = TRUE)
    table <- paste(readLines(system.file("table", "table.css", package = "hovercraft")), collapse = "\n")
    panel <- sub("^.*?(\\.hovercraft-panel-box \\{[^}]*position: sticky[^}]*\\}).*$", "\\1", table, perl = TRUE)
    expect_match(panel, "inset-inline-start: 0", fixed = TRUE)
    expect_no_match(panel, "left:", fixed = TRUE)
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
