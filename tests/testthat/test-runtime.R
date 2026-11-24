skip_if_not_installed("withr")

# Every dependency this package makes must work in another R process than the one that made it: R Markdown and Quarto
# cache a chunk's dependencies, and Shiny's `bindCache()` can replay them from a cache that outlives the process.
describe("the dependencies of a tag", {
  build <- function() {
    list(
      tip_on(
        htmltools::tags$button("Go", id = "go"),
        "Short",
        style = tip_style(theme = list("--hovercraft-tip-radius" = "9px"), gap = 3),
        more = tip_template(tip_text("The long form")),
        more_on = "hover",
        events = "hint",
        pin = TRUE,
        marker = "icon"
      ),
      tip_on(htmltools::tags$button("Off", disabled = NA), "Off", when_disabled = "why"),
      tip_define(card = tip_template(tip_text("{{ a }}"))),
      htmltools::tags$div(use_hovercraft(style = tip_style("sober", theme = list("--hovercraft-tip-bg" = "#1b3a6b"))))
    )
  }
  all_deps <- function(x) htmltools::resolveDependencies(htmltools::findDependencies(x))

  it("are all inside the installed package: none points at a folder that goes away with the R process", {
    package <- normalizePath(system.file(package = "hovercraft"), winslash = "/")
    for (dep in all_deps(build())) {
      if (is.list(dep$src)) {
        file <- normalizePath(dep$src$file, winslash = "/", mustWork = FALSE)
        expect_true(startsWith(file, package), info = paste(dep$name, file))
      } else {
        expect_identical(dep$package, "hovercraft", info = dep$name)
      }
    }
  })

  it("render in another R process, after the one that made them has gone", {
    skip_if_not_installed("callr")
    skip_on_cran()
    path <- find.package("hovercraft")
    file <- withr::local_tempfile(fileext = ".rds")
    # The first process builds the tags and ends, taking its temporary folder with it.
    callr::r(
      function(path, file, build) {
        if (dir.exists(file.path(path, "inst"))) {
          pkgload::load_all(path, quiet = TRUE)
        } else {
          library(hovercraft)
        }
        saveRDS(build(), file)
      },
      args = list(path = path, file = file, build = build)
    )
    # The second reads them back and renders the page's head and the dependencies' files, as a cached chunk does.
    got <- callr::r(
      function(file, path) {
        if (dir.exists(file.path(path, "inst"))) {
          pkgload::load_all(path, quiet = TRUE)
        } else {
          library(hovercraft)
        }
        tags <- readRDS(file)
        dir <- tempfile("lib")
        deps <- htmltools::resolveDependencies(htmltools::findDependencies(tags))
        html <- htmltools::renderDependencies(deps)
        copied <- lapply(deps, htmltools::copyDependencyToDir, outputDir = dir)
        list(html = as.character(html), n = length(copied))
      },
      args = list(file = file, path = path)
    )
    expect_gt(got$n, 3L)
    expect_match(got$html, "runtime.js", fixed = TRUE)
  })
})
