# tools::sha256sum() came with R 4.5.0; the package itself runs on 4.4.0.
sha256 <- function(path) {
  skip_if_not(exists("sha256sum", envir = asNamespace("tools")), "tools::sha256sum() needs R 4.5.0")
  unname(get("sha256sum", envir = asNamespace("tools"))(path))
}

describe("the vendored engine", {
  it("has the files engine.json lists, with the checksums it records", {
    engine <- tip_engine()
    expect_setequal(unlist(engine$files), c("placeholders.json", "tip.css", "tip.js"))
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

  it("is read once", {
    expect_identical(tip_engine(), tip_engine())
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
    expect_true(file.exists(file.path(dep$src$file, "tip.js")))
    expect_true(file.exists(file.path(dep$src$file, "tip.css")))
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
