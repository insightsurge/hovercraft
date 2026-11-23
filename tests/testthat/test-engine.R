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
    presets <- file.path("presets", c("dense", "editorial", "lively", "modern", "sober"))
    expect_setequal(
      unlist(engine$files),
      c("placeholders.json", "tip.json", "www/tip.css", "www/tip.js", paste0(presets, ".json"))
    )
    for (file in unlist(engine$files)) {
      path <- tip_path(file)
      expect_true(file.exists(path), info = file)
      # A file that was edited by hand, or copied from somewhere else, no longer matches what engine.json recorded.
      expect_identical(sha256(path), engine$sha256[[file]], info = file)
    }
  })

  it("ships the engine's page contract under this package's names", {
    contract <- jsonlite::read_json(tip_path("tip.json"))
    names <- tip_names()
    expect_identical(contract$global, names$global)
    expect_identical(contract$attributes$tip, tip_engine()$names$attribute)
    expect_identical(contract$events$tip$name, paste0(names$prefix, ":tip"))
    expect_true("auto" %in% unlist(contract$placement$values))
    # It is the data beside the engine, never served to a page.
    expect_false(file.exists(tip_path("www", "tip.json")))
  })

  it("records the engine's version, commit, names and rules", {
    engine <- tip_engine()
    expect_match(engine$engine, "^[0-9]+\\.[0-9]+\\.[0-9]+$")
    expect_match(engine$commit, "^[0-9a-f]{40}$")
    expect_named(engine$names, c("attribute", "css", "global", "prefix", "product"), ignore.order = TRUE)
    expect_true(all(c("placeholder", "filters", "blocks", "blocks_later", "engine_fields") %in% names(tip_rules())))
    expect_identical(tip_engine_version(), engine$engine)
  })
})

# The check that runs after every copy of the engine: every name the package relies on is still there. A new copy that
# renames or drops one fails here, naming it, instead of a page quietly losing its theme or its tips. The package styles
# one engine class, the badge, by its `data-tone` attribute (R/status.R), so both are checked too.
describe("the vendored engine, after a copy", {
  css <- paste(readLines(tip_path("www", "tip.css"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  js <- paste(readLines(tip_path("www", "tip.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  names <- tip_names()

  it("still has the CSS variable of every theme role, which tip_theme() writes", {
    for (role in names(tip_roles)) {
      variable <- paste0("--", names$prefix, "-", tip_roles[[role]])
      expect_true(grepl(variable, css, fixed = TRUE) || grepl(variable, js, fixed = TRUE), info = variable)
    }
  })

  it("still has the attributes tip_on() writes", {
    contract <- jsonlite::read_json(tip_path("tip.json"))
    for (attribute in tip_attributes()) {
      expect_true(attribute %in% unlist(contract$attributes), info = attribute)
    }
  })

  it("still has the class and the global the package's tests and pages reach the engine by", {
    expect_true(grepl(paste0(".", names$css), css, fixed = TRUE), info = names$css)
    expect_true(grepl(paste0("window.", names$global), js, fixed = TRUE), info = names$global)
    # The methods the package calls on it: tip_dependency() attaches, tip_define() defines.
    for (method in c("attach", "define")) {
      expect_true(grepl(paste0("\\b", method, "\\s*[(:]"), js), info = paste0(names$global, ".", method))
    }
    expect_true(grepl(names$attribute, js, fixed = TRUE), info = names$attribute)
  })

  it("still has the badge's class and its tone, which the status colours in R/status.R style", {
    expect_true(grepl(paste0(".", names$css, "-badge"), css, fixed = TRUE), info = paste0(names$css, "-badge"))
    expect_true(grepl("dataset.tone", js, fixed = TRUE), info = "the badge's data-tone attribute")
    # The engine's own list of what a badge takes: R's `status` is written into its `tone`.
    expect_true("tone" %in% names(tip_rules()$blocks$badge$fields), info = "badge field tone")
  })

  it("still reads the page's bslib colours and its dark mode, which the assumed colours follow", {
    expect_true(grepl("--bs-tertiary-bg", css, fixed = TRUE))
    expect_true(grepl("--bs-body-color", css, fixed = TRUE))
    expect_true(grepl("data-bs-theme", js, fixed = TRUE))
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
