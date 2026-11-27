# The checksum: R's own tools::sha256sum() from R 4.5, else the shell's (shasum or sha256sum) for R 4.4; a machine
# with neither skips the test. On Windows the shell's shasum is Strawberry Perl's shasum.bat (#102's CI).
sha256 <- function(path) {
  if (exists("sha256sum", envir = asNamespace("tools"))) {
    return(unname(get("sha256sum", envir = asNamespace("tools"))(path)))
  }
  tool <- Sys.which(c("shasum", "sha256sum"))
  tool <- tool[nzchar(tool)][1L]
  skip_if(is.na(tool), "neither shasum nor sha256sum is on the PATH")
  args <- c(if (startsWith(basename(tool), "shasum")) c("-a", "256"), shQuote(path))
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
    # The methods the package calls on it: use_hovercraft() attaches, tip_define() defines.
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

  it("has the names the engine added in 0.52.0 (a part mark, a pin by hand, a reason line, who pinned and how)", {
    contract <- jsonlite::read_json(tip_path("tip.json"))
    expect_identical(contract$attributes$part, paste0(names$attribute, "-part"))
    expect_true(grepl(paste0("[${NAMES.attr}-part]"), js, fixed = TRUE), info = "the part mark")
    expect_true(grepl("=== 'manual'", js, fixed = TRUE), info = "the pin value manual")
    expect_true(grepl("${CSS}-reason", js, fixed = TRUE), info = paste0(names$css, "-reason"))
    expect_true(grepl("const { by = 'host', via = 'command' }", js, fixed = TRUE), info = "pin(el, { by, via })")
  })

  it("has the names the engine added in 0.53.0 (pin(el, { into }), a pinned tip drawn in place)", {
    expect_true(grepl("pin(el, { into })", js, fixed = TRUE), info = "the into option's message")
    expect_true(grepl("dataset.side = 'in'", js, fixed = TRUE), info = "the side value in")
    expect_true(
      grepl(paste0(".", names$css, ".", names$css, "-pin[data-side=\"in\"]"), css, fixed = TRUE),
      info = "the in-place pin rule"
    )
  })

  it("has the names the engine added in 0.55.0 (several pinned tips at once, a page cap of 10)", {
    contract <- jsonlite::read_json(tip_path("tip.json"))
    expect_identical(contract$attributes$pins, paste0("data-", names$prefix, "-tip-pins"))
    expect_true(grepl(paste0("`${NAMES.attr}-pins`"), js, fixed = TRUE), info = "the page attribute that sets the cap")
    expect_true(grepl("Math.min(n, 10)", js, fixed = TRUE), info = "the cap of ten")
    expect_true(grepl("close: (el) => {", js, fixed = TRUE), info = "close(el) closes one chosen pin")
  })

  it("has the names the engine added in 0.58.0 (a hover tip that keeps its place, a pin that closes when hidden)", {
    expect_true(grepl("function keepSpot(", js, fixed = TRUE), info = "the hover tip drawn again keeps its side")
    expect_true(grepl("box.dataset.cap = 'true'", js, fixed = TRUE), info = "the height cap of a tall hover tip")
    expect_true(grepl("const vanished = (a) =>", js, fixed = TRUE), info = "a pin whose item took no room")
    expect_true(grepl("shut({ reason: 'gone', by: 'data', via: null })", js, fixed = TRUE), info = "the close as gone")
    expect_true(grepl("attributeOldValue: true", js, fixed = TRUE), info = "the reason observer skips an equal write")
  })

  it("still has the open values the page scripts read, in their order, and the classes and variables they style", {
    # `inst/pin/pin.js` and `inst/more/more.js` read the open attribute: "hint" while a hint shows, "pin" while pinned.
    expect_identical(unlist(tip_rules()$open$values), c("hint", "pin"))
    # The part classes of a tip box that the scripts' CSS and the page scripts reach by name (the engine's own class prefix).
    for (part in c("head", "close", "cue", "more", "group")) {
      class <- paste0(".", names$css, "-", part)
      expect_true(grepl(class, css, fixed = TRUE), info = class)
    }
    # The custom properties the status, more, marker and table CSS read from the engine's tip box.
    for (variable in c("ink", "quiet", "edge")) {
      property <- paste0("--", names$css, "-", variable)
      expect_true(grepl(property, css, fixed = TRUE), info = property)
    }
  })

  it("still reads the page's bslib colours and its dark mode, which the assumed colours follow", {
    expect_true(grepl("--bs-tertiary-bg", css, fixed = TRUE))
    expect_true(grepl("--bs-body-color", css, fixed = TRUE))
    expect_true(grepl("data-bs-theme", js, fixed = TRUE))
  })
})

describe("use_hovercraft()", {
  deps <- use_hovercraft()
  dep <- deps[[1]]
  runtime <- deps[[length(deps)]]

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
    # The engine's two files, and the runtime's: the start script, the Shiny layer and the badge colours.
    expect_setequal(
      basename(copied[grepl("hovercraft", copied)]),
      c("tip.js", "tip.css", "runtime.js", "shiny.js", "shiny.css", "status.css")
    )
    expect_false(any(grepl("engine.json|placeholders.json|tip.sha256", copied)))
  })

  it("starts the engine from a file of the runtime dependency, under the engine's own name, with nothing inline", {
    expect_identical(runtime$name, "hovercraft-runtime")
    expect_identical(runtime$script, c("shiny.js", "runtime.js"))
    expect_identical(dep_config(runtime)$global, tip_names()$global)
    script <- paste(readLines(system.file("runtime", "runtime.js", package = "hovercraft")), collapse = "\n")
    expect_match(script, ".attach(document)", fixed = TRUE)
    expect_false(any(vapply(deps, function(d) !is.null(d$head), NA)))
  })
})

describe("tip_reset()", {
  it("forgets the templates defined in this session", {
    skip_if_not_installed("withr")
    # Templates are process state: another file run earlier in this process may have left some (#175).
    tip_reset()
    withr::defer(tip_reset())
    tip_define(demo = tip_template(tip_text("x")))
    expect_named(tip_known(), "demo")
    tip_reset()
    expect_identical(tip_known(), list())
  })

  it("forgets the templates of the running Shiny session as well as the process's own", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("withr")
    withr::defer(tip_reset())
    tip_reset()
    tip_define(page_wide = tip_template(tip_text("{{ x }}")))
    session <- shiny::MockShinySession$new()
    shiny::withReactiveDomain(session, {
      tip_define(own = tip_template(tip_text("{{ y }}")))
      expect_setequal(names(tip_known()), c("page_wide", "own"))
      tip_reset()
      expect_identical(tip_known(), list())
    })
    expect_identical(tip_known(), list())
  })
})

describe("use_hovercraft(max_pins =, keys =)", {
  ops_of <- function(dep) {
    meta <- dep$meta[["hovercraft-ops"]]
    jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(meta)), simplifyVector = FALSE)
  }
  named <- function(deps) vapply(deps, function(dep) dep$name, "")

  it("adds nothing for the defaults, one pin and every key on", {
    expect_identical(named(use_hovercraft()), c("hovercraft", "hovercraft-runtime"))
  })

  it("sends a value that is written, even the default one, so a later different one can be seen", {
    expect_identical(
      named(use_hovercraft(max_pins = 1)),
      c("hovercraft", "hovercraft-max-pins-1", "hovercraft-runtime")
    )
    expect_identical(
      named(use_hovercraft(keys = TRUE)),
      c("hovercraft", "hovercraft-keys-none", "hovercraft-runtime")
    )
    expect_identical(
      named(use_hovercraft(max_pins = 1, keys = c(pin = TRUE))),
      c("hovercraft", "hovercraft-max-pins-1", "hovercraft-keys-none", "hovercraft-runtime")
    )
  })

  it("sends a number of pins in a dependency of its own, named by the number", {
    deps <- use_hovercraft(max_pins = 3)
    expect_true("hovercraft-max-pins-3" %in% named(deps))
    expect_identical(
      ops_of(deps[[which(named(deps) == "hovercraft-max-pins-3")]])[[1]],
      list(op = "maxPins", value = 3L)
    )
    # Two values are two dependencies, so htmltools keeps both and the page's script can see they differ.
    both <- htmltools::resolveDependencies(c(use_hovercraft(max_pins = 3), use_hovercraft(max_pins = 5)))
    expect_true(all(c("hovercraft-max-pins-3", "hovercraft-max-pins-5") %in% named(both)))
  })

  it("takes a whole number from 1 to 10", {
    for (bad in list(0, 11, 2.5, NA, "3", c(2, 3), NULL, -1)) {
      expect_error(use_hovercraft(max_pins = bad), class = "hovercraft_error_argument")
    }
    expect_no_error(use_hovercraft(max_pins = 10))
    expect_error(use_hovercraft(max_pins = 11), "10")
  })

  it("switches the letter keys off as a whole or one by one, and leaves a key out on", {
    off <- function(keys) {
      deps <- use_hovercraft(keys = keys)
      dep <- deps[named(deps) == "hovercraft-keys-pin" | grepl("^hovercraft-keys", named(deps))][[1]]
      unlist(ops_of(dep)[[1]]$off)
    }
    expect_setequal(off(FALSE), c("pin", "more"))
    expect_identical(off(c(pin = FALSE)), "pin")
    expect_identical(off(c(more = FALSE)), "more")
    expect_identical(off(c(pin = FALSE, more = TRUE)), "pin")
  })

  it("refuses a key that is not one, a switch that is not TRUE or FALSE, and an unnamed vector", {
    expect_error(use_hovercraft(keys = c(pin = FALSE, tab = FALSE)), class = "hovercraft_error_argument")
    expect_error(use_hovercraft(keys = c(pin = NA)), class = "hovercraft_error_argument")
    expect_error(use_hovercraft(keys = c(FALSE, TRUE)), class = "hovercraft_error_argument")
    expect_error(use_hovercraft(keys = "pin"), class = "hovercraft_error_argument")
    expect_error(use_hovercraft(keys = c(pin = FALSE, pin = TRUE)), class = "hovercraft_error_argument")
    expect_error(use_hovercraft(keys = c(tab = FALSE)), "pin")
  })

  it("tells the runtime the names it needs to set the engine's page attribute and to count pins", {
    config <- jsonlite::fromJSON(jsonlite::toJSON(tip_runtime_config(), auto_unbox = TRUE), simplifyVector = FALSE)
    expect_identical(config$attributes$pins, tip_contract()$attributes$pins)
    expect_identical(config$attributes$id, tip_attribute("id"))
    expect_identical(config$events$tip, tip_contract()$events$tip$name)
  })
})
