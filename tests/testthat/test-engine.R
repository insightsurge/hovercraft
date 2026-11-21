# The checksum from the shell (shasum or sha256sum), so it works on R 4.4 too; a machine with neither skips the test.
sha256 <- function(path) {
  tool <- Sys.which(c("shasum", "sha256sum"))
  tool <- tool[nzchar(tool)][1L]
  skip_if(is.na(tool), "neither shasum nor sha256sum is on the PATH")
  args <- c(if (basename(tool) == "shasum") c("-a", "256"), shQuote(path))
  strsplit(system2(tool, args, stdout = TRUE), " ", fixed = TRUE)[[1]][[1]]
}

describe("the vendored engine", {
  it("has the files engine.json lists, with the checksums it records", {
    engine <- tip_engine()
    expect_setequal(unlist(engine$files), c("placeholders.json", "www/tip.css", "www/tip.js"))
    for (file in unlist(engine$files)) {
      path <- tip_path(file)
      expect_true(file.exists(path), info = file)
      # A file that was edited by hand, or copied from somewhere else, no longer matches what engine.json recorded.
      expect_identical(sha256(path), engine$sha256[[file]], info = file)
    }
  })

  it("records the engine's version, commit, names and rules", {
    engine <- tip_engine()
    expect_match(engine$engine, "^[0-9]+\\.[0-9]+\\.[0-9]+$")
    expect_match(engine$commit, "^[0-9a-f]{40}$")
    expect_named(engine$names, c("attribute", "css", "global"), ignore.order = TRUE)
    expect_true(all(c("placeholder", "filters", "blocks", "blocks_later", "engine_fields") %in% names(tip_rules())))
    expect_identical(tip_engine_version(), engine$engine)
  })
})

describe("tip_dependency()", {
  dep <- tip_dependency()

  it("is versioned with the engine, not the package", {
    expect_s3_class(dep, "html_dependency")
    expect_identical(dep$version, tip_engine_version())
    expect_false(identical(dep$version, as.character(utils::packageVersion("hovercraft"))))
  })

  it("serves the engine's script and stylesheet from the vendored folder", {
    expect_identical(dep$script, "tip.js")
    expect_identical(dep$stylesheet, "tip.css")
    expect_identical(dep$package, "hovercraft")
    expect_true(file.exists(tip_path("www", "tip.js")))
    expect_true(file.exists(tip_path("www", "tip.css")))
  })

  it("puts nothing else on a page: the record of where the engine came from stays in the package", {
    skip_if_not_installed("withr")
    dir <- withr::local_tempdir()
    htmltools::save_html(tip_on(htmltools::tags$button("Run"), "Runs"), file.path(dir, "page.html"))
    copied <- list.files(dir, recursive = TRUE)
    expect_setequal(basename(copied[grepl("hovercraft", copied)]), c("tip.js", "tip.css"))
    expect_false(any(grepl("engine.json|placeholders.json|tip.sha256", copied)))
  })

  it("starts the engine once the page has it, under the engine's own name", {
    expect_match(dep$head, paste0(tip_names()$global, ".attach(document)"), fixed = TRUE)
  })
})

describe("tip_reset()", {
  it("forgets the templates defined in this session", {
    skip_if_not_installed("withr")
    withr::defer(tip_reset())
    tip_define(demo = tip_template(tip_text("x")))
    expect_named(tip_known(), "demo")
    tip_reset()
    expect_identical(tip_known(), list())
  })
})
