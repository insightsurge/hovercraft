skip_if_not_installed("withr")
quiet <- function(code) suppressWarnings(code)
replace_in <- function(x, values) {
  x[names(values)] <- values
  x
}
css_of <- function(theme) tip_theme_css(theme)

describe("tip_theme()", {
  it("keeps the roles in order, as CSS, and an empty theme has none", {
    got <- tip_theme(light = list(bg = "navy", ink = "white", radius = 8, shadow = "none", font = "Georgia, serif"))
    expect_s3_class(got, "hovercraft_theme")
    expect_identical(
      got$light,
      list(bg = "#000080", ink = "#ffffff", radius = "8px", shadow = "none", font = "Georgia, serif")
    )
    expect_identical(tip_theme()$light, list())
    expect_identical(tip_theme()$mode, "auto")
  })

  it("reads a CSS variable in any role", {
    got <- tip_theme(light = list(bg = "--brand", radius = "--r", font = "var(--f)", shadow = "--s"))
    expect_identical(got$light, list(bg = "var(--brand)", radius = "var(--r)", font = "var(--f)", shadow = "var(--s)"))
  })

  it("takes nothing in ... and names the choices for a misspelt role", {
    expect_error(tip_theme(bg = "red"), "must be empty", class = "hovercraft_error_theme")
    expect_error(tip_theme(bg = "red"), class = "hovercraft_error")
    expect_error(tip_theme("red"), "unnamed", class = "hovercraft_error_theme")
    expect_error(tip_theme(light = list(bgg = "red")), "Did you mean", class = "hovercraft_error_theme")
    expect_error(tip_theme(dark = list(zzz = 1)), "not a role", class = "hovercraft_error_theme")
  })

  it("refuses a bad value with an error that says what the role takes", {
    expect_error(tip_theme(light = list(bg = "notacolour")), "colour name", class = "hovercraft_error_theme")
    expect_error(tip_theme(light = list(radius = "1;}")), "pixels", class = "hovercraft_error_theme")
    expect_error(tip_theme(dark = list(font = "a; b")), "font-family", class = "hovercraft_error_theme")
    expect_error(tip_theme(dark = list(shadow = "url(x)")), "box-shadow", class = "hovercraft_error_theme")
  })

  it("checks its other arguments", {
    expect_error(tip_theme(mode = "x"), class = "hovercraft_error_theme")
    expect_error(tip_theme(mode = 1), "mode", class = "hovercraft_error_argument")
    expect_error(tip_theme(light = 1), "light", class = "hovercraft_error_argument")
    expect_error(tip_theme(light = list("red")), "light", class = "hovercraft_error_argument")
  })
})

describe("the contrast check", {
  it("warns when the words are below 4.5:1 on the fill, and keeps the colours", {
    expect_warning(
      got <- tip_theme(light = list(bg = "#ffffff", ink = "#dddddd")),
      "1.36:1",
      class = "hovercraft_contrast"
    )
    expect_identical(got$light$ink, "#dddddd")
  })

  it("names both modes when one pair of colours is used in both", {
    expect_warning(
      tip_theme(light = list(bg = "#ffffff", ink = "#dddddd")),
      "light and dark mode",
      class = "hovercraft_contrast"
    )
  })

  it("is quiet at 4.5:1 and above, and measures the code background too", {
    expect_no_warning(tip_theme(light = list(bg = "#1b3a6b", ink = "#ffffff")))
    expect_warning(
      tip_theme(light = list(bg = "#1b3a6b", ink = "#ffffff", code_bg = "#7a9cc6")),
      "code_bg",
      class = "hovercraft_contrast"
    )
  })

  it("measures a role the theme leaves out against what Bootstrap's default gives", {
    expect_warning(tip_theme(light = list(bg = "#e0e0e0")), "ink", class = "hovercraft_contrast")
    expect_warning(tip_theme(dark = list(ink = "#e8e8e8"), mode = "dark"), class = "hovercraft_contrast")
    expect_no_warning(tip_theme(light = list(radius = 4)))
  })

  it("checks each mode the theme uses, and dark falls back to light's roles", {
    expect_warning(
      tip_theme(light = list(bg = "#1b3a6b", ink = "#ffffff"), dark = list(ink = "#444444")),
      "dark mode",
      class = "hovercraft_contrast"
    )
    expect_no_warning(tip_theme(
      light = list(bg = "#ffffff", ink = "#dddddd"),
      mode = "dark",
      dark = list(ink = "#000000")
    ))
  })

  it("lays a translucent code background over the tip's fill before it measures", {
    expect_no_warning(
      tip_theme(light = list(bg = "#1b3a6b", ink = "#ffffff", code_bg = "rgba(255, 255, 255, 0.12)"))
    )
    expect_warning(
      tip_theme(light = list(bg = "#1b3a6b", ink = "#ffffff", code_bg = "rgba(255, 255, 255, 0.9)")),
      "code_bg",
      class = "hovercraft_contrast"
    )
  })

  it("lays a translucent fill over the page of its mode before it measures", {
    expect_warning(
      tip_theme(light = list(bg = "rgba(0, 0, 0, 0.05)", ink = "#ffffff"), mode = "light"),
      class = "hovercraft_contrast"
    )
    expect_no_warning(tip_theme(light = list(bg = "rgba(0, 0, 0, 0.9)", ink = "#ffffff"), mode = "light"))
  })

  it("warns once if the option is not TRUE or FALSE, and goes on warning about contrast", {
    withr::local_options(hovercraft.contrast_warnings = "no")
    rlang::reset_warning_verbosity("hovercraft-contrast-option")
    expect_warning(
      expect_warning(
        tip_theme(light = list(bg = "#ffffff", ink = "#dddddd")),
        class = "hovercraft_warning_option"
      ),
      class = "hovercraft_contrast"
    )
  })

  it("skips a CSS variable, which only the browser can read", {
    expect_no_warning(tip_theme(light = list(bg = "--brand", ink = "#ffffff")))
    expect_no_warning(tip_theme(light = list(bg = "#ffffff", ink = "--ink")))
  })

  it("can be turned off, and silenced alone by its class", {
    withr::local_options(hovercraft.contrast_warnings = FALSE)
    expect_no_warning(tip_theme(light = list(bg = "#ffffff", ink = "#dddddd")))
    withr::local_options(hovercraft.contrast_warnings = TRUE)
    got <- withCallingHandlers(
      tip_theme(light = list(bg = "#ffffff", ink = "#dddddd")),
      hovercraft_contrast = function(w) invokeRestart("muffleWarning")
    )
    expect_s3_class(got, "hovercraft_theme")
  })
})

describe("tip_theme_roles()", {
  it("lets a dark role replace the light one, and only in dark", {
    theme <- tip_theme(light = list(bg = "#111111", radius = 4), dark = list(bg = "#eeeeee"), mode = "auto")
    expect_identical(tip_theme_roles(theme, "light"), list(bg = "#111111", radius = "4px"))
    expect_identical(tip_theme_roles(theme, "dark"), list(bg = "#eeeeee", radius = "4px"))
  })
})

describe("tip_theme_css()", {
  prefix <- tip_names()$prefix
  theme <- quiet(tip_theme(
    light = list(bg = "#1b3a6b", ink = "#ffffff", radius = 8),
    dark = list(bg = "#9ec5fe", ink = "#0b1f3a")
  ))

  it("writes light on :root, the dark roles that differ under data-bs-theme and the visitor's own setting", {
    css <- css_of(theme)
    expect_match(css, "^<style>")
    expect_match(css, "</style>$")
    expect_match(css, ":root, [data-bs-theme=\"light\"] {", fixed = TRUE)
    expect_match(css, sprintf("--%s-tip-bg: #1b3a6b;", prefix), fixed = TRUE)
    expect_match(css, "[data-bs-theme=\"dark\"] {", fixed = TRUE)
    expect_match(css, sprintf("--%s-tip-bg: #9ec5fe;", prefix), fixed = TRUE)
    expect_match(css, "@media (prefers-color-scheme: dark)", fixed = TRUE)
    expect_match(css, ":root:not([data-bs-theme])", fixed = TRUE)
  })

  it("leaves a role that is the same in dark out of the dark rules", {
    css <- css_of(theme)
    dark <- sub(".*\\[data-bs-theme=\"dark\"\\] \\{", "", css)
    expect_no_match(sub("@media.*", "", dark), "radius")
  })

  it("writes one rule for a fixed mode, whatever the page says", {
    light <- css_of(quiet(tip_theme(light = list(bg = "#111111"), dark = list(bg = "#eeeeee"), mode = "light")))
    expect_match(light, "^<style>\n:root \\{")
    expect_match(light, "#111111", fixed = TRUE)
    expect_no_match(light, "#eeeeee", fixed = TRUE)
    expect_no_match(light, "prefers-color-scheme", fixed = TRUE)
    dark <- css_of(quiet(tip_theme(
      light = list(bg = "#111111", radius = 4),
      dark = list(bg = "#eeeeee"),
      mode = "dark"
    )))
    expect_match(dark, "#eeeeee", fixed = TRUE)
    expect_match(dark, "radius: 4px", fixed = TRUE)
    expect_no_match(dark, "#111111", fixed = TRUE)
  })

  it("is empty for an empty theme", {
    expect_identical(css_of(tip_theme()), "")
  })

  it("writes every variable the engine's stylesheet reads", {
    css <- readLines(tip_path("www", "tip.css"), warn = FALSE)
    for (role in names(tip_roles)) {
      variable <- paste0("--", prefix, "-", tip_roles[[role]])
      expect_true(any(grepl(variable, css, fixed = TRUE)), info = variable)
    }
  })

  it("reads the prefix from engine.json", {
    expect_identical(prefix, tip_engine()$names$prefix)
    expect_match(tip_engine()$names$attribute, sprintf("^data-%s-tip$", prefix))
  })
})

describe("tip_as_theme()", {
  it("passes NULL and a theme through", {
    expect_null(tip_as_theme(NULL))
    theme <- tip_theme(light = list(radius = 2))
    expect_identical(tip_as_theme(theme), theme)
  })

  it("takes a list of roles as one set for both modes", {
    got <- tip_as_theme(list(bg = "#1b3a6b", ink = "#ffffff"))
    expect_identical(got$light, list(bg = "#1b3a6b", ink = "#ffffff"))
    expect_identical(got$dark, list())
    expect_warning(tip_as_theme(list(bg = "#ffffff", ink = "#dddddd")), class = "hovercraft_contrast")
  })

  it("takes a list of CSS variables as they are, and writes them for both modes", {
    prefix <- tip_names()$prefix
    got <- tip_as_theme(list("--hovercraft-tip-bg" = "#123456", "--x" = "0 1px 2px rgba(0, 0, 0, 0.2)", "--y" = "--z"))
    expect_identical(unname(unlist(got$tokens)), c("#123456", "0 1px 2px rgba(0, 0, 0, 0.2)", "var(--z)"))
    expect_identical(tip_as_theme(list("--r" = 8))$tokens, list("--r" = "8px"))
    css <- css_of(got)
    expect_match(css, "--hovercraft-tip-bg: #123456;", fixed = TRUE)
    expect_match(css, "--x: 0 1px 2px rgba(0, 0, 0, 0.2);", fixed = TRUE)
    expect_no_match(css, "dark", fixed = TRUE)
    expect_match(css_of(tip_as_theme(list("--a" = "1px"))), "[data-bs-theme=\"light\"]", fixed = TRUE)
    expect_true(nzchar(prefix))
  })

  it("refuses a bad variable name or value, a mix, and what is not a list", {
    expect_error(tip_as_theme(list("--1a" = "red")), "CSS variable name", class = "hovercraft_error_theme")
    expect_error(tip_as_theme(list("--a" = "red; x")), "not valid", class = "hovercraft_error_theme")
    expect_error(tip_as_theme(list(bg = "red", "--a" = "1")), "mixes", class = "hovercraft_error_theme")
    expect_error(tip_as_theme(3), "not a theme", class = "hovercraft_error_theme")
    expect_error(tip_as_theme(list(1, 2)), "not a theme", class = "hovercraft_error_theme")
    expect_error(tip_as_theme(list(bg = "red", bg = "blue")), "not a theme", class = "hovercraft_error_theme")
  })

  it("takes an empty list as an empty theme", {
    expect_identical(css_of(tip_as_theme(list())), "")
  })
})

describe("a bslib theme", {
  skip_if_not_installed("bslib")

  it("becomes the page inverted: its text colour is the fill and its background the words", {
    theme <- quiet(tip_as_theme(bslib::bs_theme(version = 5, bg = "#ffffff", fg = "#102a43")))
    expect_s3_class(theme, "hovercraft_theme")
    expect_identical(theme$light$ink, "#ffffff")
    expect_match(theme$light$bg, "^#[0-9a-f]{6}$")
    expect_identical(theme$mode, "auto")
    expect_named(theme$dark, c("bg", "ink", "line"))
    expect_identical(
      theme$light$radius,
      tip_length(bslib::bs_get_variables(
        bslib::bs_theme(version = 5, bg = "#ffffff", fg = "#102a43"),
        "border-radius"
      )[[1]])
    )
  })

  it("takes the page's emphasis colours, which is what the engine's own default uses", {
    local_mocked_bindings(
      bs_get_variables = function(theme, varnames) {
        stats::setNames(rep(NA_character_, length(varnames)), varnames) |>
          replace_in(c(
            "body-emphasis-color" = "#000",
            "body-color" = "#1D1F21",
            "body-bg" = "#fff",
            "body-emphasis-color-dark" = "#ffffff",
            "body-color-dark" = "#dee2e6",
            "body-bg-dark" = "#212529"
          ))
      },
      .package = "bslib"
    )
    got <- tip_as_theme(bslib::bs_theme(version = 5))
    expect_identical(got$light$bg, "#000000")
    expect_identical(got$dark$bg, "#ffffff")
  })

  it("falls back to the body colour when there is no emphasis colour", {
    local_mocked_bindings(
      bs_get_variables = function(theme, varnames) {
        stats::setNames(rep(NA_character_, length(varnames)), varnames) |>
          replace_in(c("body-color" = "#1D1F21", "body-color-dark" = "#dee2e6"))
      },
      .package = "bslib"
    )
    withr::local_options(hovercraft.contrast_warnings = FALSE)
    got <- tip_as_theme(bslib::bs_theme(version = 5))
    expect_identical(got$light$bg, "#1d1f21")
    expect_identical(got$dark$bg, "#dee2e6")
  })

  it("writes a font that is a bslib variable with a fallback, for a page that does not define it", {
    local_mocked_bindings(
      bs_get_variables = function(theme, varnames) {
        stats::setNames(rep(NA_character_, length(varnames)), varnames) |>
          replace_in(c("font-family-base" = "var(--bs-font-sans-serif)"))
      },
      .package = "bslib"
    )
    got <- tip_as_theme(bslib::bs_theme(version = 5))
    expect_identical(got$light$font, "var(--bs-font-sans-serif, system-ui, sans-serif)")
    expect_match(css_of(got), "font-body: var(--bs-font-sans-serif, system-ui, sans-serif);", fixed = TRUE)
  })

  it("warns about a font it cannot read, like every other role", {
    local_mocked_bindings(
      bs_get_variables = function(theme, varnames) {
        stats::setNames(rep(NA_character_, length(varnames)), varnames) |>
          replace_in(c("font-family-base" = "a; b"))
      },
      .package = "bslib"
    )
    expect_warning(tip_as_theme(bslib::bs_theme(version = 5)), "font", class = "hovercraft_warning_theme")
  })

  it("takes the font and the corner", {
    theme <- quiet(tip_as_theme(bslib::bs_theme(
      version = 5,
      "border-radius" = "10px",
      "font-family-base" = "Georgia, serif"
    )))
    expect_identical(theme$light$radius, "10px")
    expect_match(theme$light$font, "Georgia")
  })

  it("goes into the dependency like any other theme", {
    deps <- tip_dependency(theme = bslib::bs_theme(version = 5))
    expect_length(deps, 2L)
    expect_identical(deps[[2]]$name, "hovercraft-theme")
  })

  it("warns about a value it cannot read and keeps the role at the page's own", {
    withr::local_options(hovercraft.contrast_warnings = FALSE)
    local_mocked_bindings(
      bs_get_variables = function(theme, varnames) {
        stats::setNames(rep(NA_character_, length(varnames)), varnames) |>
          replace_in(c("body-bg" = "calc(1px + 2px)", "body-color" = "#222222"))
      },
      .package = "bslib"
    )
    expect_warning(
      got <- tip_as_theme(bslib::bs_theme(version = 5)),
      class = "hovercraft_warning_theme"
    )
    expect_null(got$light$ink)
    expect_identical(got$light$bg, "#222222")
  })

  it("needs a bslib that can read the variables", {
    local_mocked_bindings(tip_has_bslib = function() FALSE)
    expect_error(tip_as_theme(bslib::bs_theme(version = 5)), "0.6.0", class = "hovercraft_error_theme")
  })
})

describe("tip_dependency()", {
  it("is the engine alone with no theme, and for an empty theme", {
    expect_s3_class(tip_dependency(), "html_dependency")
    expect_s3_class(tip_dependency(theme = tip_theme()), "html_dependency")
    expect_s3_class(tip_dependency(theme = list()), "html_dependency")
  })

  it("is the engine and a theme that holds only CSS, with a theme", {
    deps <- tip_dependency(theme = tip_theme(light = list(radius = 6)))
    expect_length(deps, 2L)
    expect_identical(deps[[1]]$name, "hovercraft")
    expect_identical(deps[[2]]$name, "hovercraft-theme")
    expect_identical(deps[[2]]$version, deps[[1]]$version)
    expect_match(deps[[2]]$head, "radius: 6px", fixed = TRUE)
    expect_null(deps[[2]]$script)
    expect_null(deps[[2]]$stylesheet)
  })

  it("lets the engine arrive once when tip_on() adds it as well", {
    deps <- htmltools::resolveDependencies(c(
      htmltools::findDependencies(tip_on(htmltools::tags$button("x"), "t")),
      tip_dependency(theme = tip_theme(light = list(radius = 6)))
    ))
    expect_identical(vapply(deps, function(d) d$name, ""), c("hovercraft", "hovercraft-theme"))
  })

  it("takes a list of CSS variables as the theme", {
    deps <- tip_dependency(theme = list("--hovercraft-tip-radius" = "8px"))
    expect_match(deps[[2]]$head, "--hovercraft-tip-radius: 8px;", fixed = TRUE)
  })
})

describe("printing a theme", {
  it("shows a call that makes it again", {
    theme <- quiet(tip_theme(light = list(bg = "navy", radius = 8), dark = list(bg = "#9ec5fe"), mode = "auto"))
    expect_identical(
      format(theme),
      "tip_theme(light = list(bg = \"#000080\", radius = \"8px\"), dark = list(bg = \"#9ec5fe\"))"
    )
    expect_identical(format(tip_theme()), "tip_theme()")
    expect_identical(
      format(quiet(tip_theme(light = list(bg = "#111111"), mode = "dark"))),
      "tip_theme(light = list(bg = \"#111111\"), mode = \"dark\")"
    )
    expect_output(returned <- print(tip_theme()), "tip_theme()", fixed = TRUE)
    expect_s3_class(returned, "hovercraft_theme")
  })

  it("names the variables of a theme made from CSS variables", {
    expect_identical(
      format(tip_as_theme(list("--a" = "1px", "--b" = "red"))),
      "<tip theme, CSS variables: --a = 1px, --b = #ff0000>"
    )
  })
})

describe("the package's description", {
  it("asks for the bslib version a theme needs", {
    desc <- read.dcf(system.file("DESCRIPTION", package = "hovercraft"), fields = "Suggests")[[1]]
    expect_match(desc, "bslib \\(>= 0.6.0\\)")
  })
})
