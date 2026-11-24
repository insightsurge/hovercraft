skip_if_not_installed("withr")
quiet <- function(code) suppressWarnings(code)
navy <- function() tip_theme(light = list(bg = "#1b3a6b", ink = "#ffffff"))
button <- function() htmltools::tags$button("Run")
attr_of <- function(tag, name) htmltools::tagGetAttribute(tag, name)

describe("tip_presets()", {
  got <- tip_presets()

  it("lists every preset the engine's contract names, with a label and one sentence", {
    expect_s3_class(got, "data.frame")
    expect_identical(names(got), c("name", "label", "about"))
    expect_identical(got$name, unlist(jsonlite::read_json(tip_path("tip.json"))$presets))
    expect_true(all(nzchar(got$label)))
    expect_true(all(nzchar(got$about)))
  })

  it("reads the label and the sentence from each preset's own file", {
    sober <- jsonlite::read_json(tip_path("presets", "sober.json"))
    row <- got[got$name == "sober", ]
    expect_identical(row$label, sober$label)
    expect_identical(row$about, sober$about)
  })
})

describe("tip_style()", {
  it("holds nothing when it is given nothing, so it inherits everything", {
    got <- tip_style()
    expect_s3_class(got, "hovercraft_style")
    expect_null(got$preset)
    expect_null(got$theme)
    expect_null(got$class)
  })

  it("takes a preset name from the engine's list, and none", {
    expect_identical(tip_style("modern")$preset, "modern")
    expect_identical(tip_style(tip_presets()$name[[1]])$preset, tip_presets()$name[[1]])
    expect_identical(tip_style("none")$preset, "none")
  })

  it("stops on a name that is not a preset, and names the close one", {
    expect_error(tip_style("sobre"), class = "hovercraft_error_style")
    expect_error(tip_style("sobre"), "sober")
    expect_error(tip_style(3), class = "hovercraft_error_argument")
    expect_error(tip_style(c("sober", "dense")), class = "hovercraft_error_argument")
  })

  it("stops on a string that looks like CSS, with a class that is also a style error", {
    for (text in c("color: red", "color:red", "a;b", "sober; dense")) {
      expect_error(tip_style(text), class = "hovercraft_error_style_css")
    }
    err <- rlang::catch_cnd(tip_style("color: red"))
    expect_true(all(c("hovercraft_error_style_css", "hovercraft_error_style", "hovercraft_error") %in% class(err)))
  })

  it("makes a misspelt argument an error", {
    expect_error(tip_style("sober", themee = navy()), class = "hovercraft_error_style")
    expect_error(tip_style("sober", "dense"), class = "hovercraft_error_style")
  })

  it("reads a theme in every form tip_theme() accepts", {
    expect_s3_class(tip_style(theme = navy())$theme, "hovercraft_theme")
    expect_identical(quiet(tip_style(theme = list(bg = "#1b3a6b", ink = "#ffffff")))$theme$light$bg, "#1b3a6b")
    expect_identical(tip_style(theme = list("--hovercraft-tip-radius" = "8px"))$theme$tokens[[1]], "8px")
    expect_error(tip_style(theme = 3), class = "hovercraft_error_theme")
  })

  it("reads a bslib theme", {
    skip_if_not_installed("bslib")
    expect_s3_class(quiet(tip_style(theme = bslib::bs_theme(version = 5))$theme), "hovercraft_theme")
  })
})

describe("tip_style(class =)", {
  it("takes NULL, an empty string, a vector or spaced text", {
    expect_null(tip_style()$class)
    expect_identical(tip_style(class = "")$class, "")
    expect_identical(tip_style(class = "wide")$class, "wide")
    expect_identical(tip_style(class = c("wide", "audit-tip"))$class, "wide audit-tip")
    expect_identical(tip_style(class = "wide   audit-tip")$class, "wide audit-tip")
    expect_identical(tip_style(class = c("a b", "b c"))$class, "a b c")
  })

  it("treats blank text as the empty string that clears an inherited class", {
    expect_identical(tip_style(class = "   ")$class, "")
    expect_identical(tip_style(class = c("", " "))$class, "")
  })

  it("takes the engine's own pattern, from tip.json", {
    pattern <- jsonlite::read_json(tip_path("tip.json"))$class$pattern
    expect_identical(pattern, tip_rules()$class$pattern)
    for (ok in c("a", "-a", "_a", "a1", "a-b_c", "A")) {
      expect_identical(tip_style(class = ok)$class, ok)
      expect_true(grepl(pattern, ok, perl = TRUE))
    }
    for (bad in c("1a", "a.b", "a!", "-1", "--", "a#b")) {
      expect_error(tip_style(class = bad), class = "hovercraft_error_style")
    }
  })

  it("stops on a name that starts with the tip's own prefix", {
    prefix <- tip_names()$css
    expect_error(tip_style(class = paste0(prefix, "-x")), prefix, class = "hovercraft_error_style")
    expect_error(tip_style(class = c("fine", prefix)), class = "hovercraft_error_style")
    expect_identical(tip_style(class = "hc")$class, "hc") # only the reserved start is refused
  })

  it("stops on anything that is not text", {
    expect_error(tip_style(class = 1), class = "hovercraft_error_argument")
    expect_error(tip_style(class = NA_character_), class = "hovercraft_error_argument")
    expect_error(tip_style(class = character()), class = "hovercraft_error_argument")
  })
})

describe("tip_style(motion =, gap =, max_width =, pin_width =)", {
  fields <- function() jsonlite::read_json(tip_path("tip.json"))$presets_fields$fields
  with_arg <- function(arg, value) do.call(tip_style, stats::setNames(list(value), arg))

  it("holds nothing for them when they are left out, so a style inherits them", {
    got <- tip_style("sober")
    expect_null(got$motion)
    expect_null(got$gap)
    expect_null(got$max_width)
    expect_null(got$pin_width)
  })

  it("keeps what it is given, the numbers as whole numbers", {
    got <- tip_style(motion = "slide", gap = 12, max_width = 280L, pin_width = 500)
    expect_identical(got$motion, "slide")
    expect_identical(got$gap, 12L)
    expect_identical(got$max_width, 280L)
    expect_identical(got$pin_width, 500L)
  })

  it("takes both ends of each range the engine states in tip.json, and stops one step outside", {
    for (arg in c("gap", "max_width", "pin_width")) {
      field <- fields()[[arg]]
      expect_identical(with_arg(arg, field$min)[[arg]], field$min)
      expect_identical(with_arg(arg, field$max)[[arg]], field$max)
      expect_error(with_arg(arg, field$min - 1), class = "hovercraft_error_style_range")
      expect_error(with_arg(arg, field$max + 1), class = "hovercraft_error_style_range")
    }
  })

  it("names the range, and the argument, in the error", {
    err <- rlang::catch_cnd(tip_style(gap = 30))
    expect_s3_class(err, "hovercraft_error_style_range")
    expect_s3_class(err, "hovercraft_error_style")
    expect_match(conditionMessage(err), "`gap`", fixed = TRUE)
    expect_match(conditionMessage(err), "0 to 24", fixed = TRUE)
    expect_match(conditionMessage(rlang::catch_cnd(tip_style(max_width = 100))), "160 to 640", fixed = TRUE)
    expect_match(conditionMessage(rlang::catch_cnd(tip_style(pin_width = 900))), "240 to 720", fixed = TRUE)
  })

  it("stops on a fraction, and on what is not one number", {
    expect_error(tip_style(gap = 2.5), class = "hovercraft_error_style_range")
    for (bad in list("a", c(1, 2), NA_real_, Inf, TRUE, list(1))) {
      expect_error(tip_style(gap = bad), class = "hovercraft_error_argument")
    }
  })

  it("takes the engine's motion words, and names the close one for a misspelt word", {
    for (word in unlist(fields()$motion$values)) {
      expect_identical(tip_style(motion = word)$motion, word)
    }
    expect_error(tip_style(motion = "slid"), class = "hovercraft_error_style_motion")
    expect_error(tip_style(motion = "slid"), "slide")
    expect_error(tip_style(motion = "Fade"), class = "hovercraft_error_style")
    expect_error(tip_style(motion = c("fade", "none")), class = "hovercraft_error_argument")
    expect_error(tip_style(motion = 1), class = "hovercraft_error_argument")
  })

  it("has no delay: the engine reads one only from a preset's own file, so it is a misspelt argument", {
    expect_error(tip_style(delay = 300), class = "hovercraft_error_style")
    expect_error(tip_style(delay = 300), "delay")
  })
})

describe("size and motion settings merged part by part", {
  it("takes each setting from the nearer style, and the rest from the farther", {
    near <- tip_style(gap = 2, class = "near")
    far <- tip_style("sober", motion = "slide", gap = 20, max_width = 300, pin_width = 400)
    got <- tip_style_over(near, far)
    expect_identical(got$gap, 2L)
    expect_identical(got$motion, "slide")
    expect_identical(got$max_width, 300L)
    expect_identical(got$pin_width, 400L)
    expect_identical(got$preset, "sober")
    expect_identical(got$class, "near")
  })
})

describe("tip_as_style()", {
  it("passes NULL and a style through", {
    expect_null(tip_as_style(NULL))
    style <- tip_style("sober")
    expect_identical(tip_as_style(style), style)
  })

  it("reads a preset name, and none", {
    expect_identical(tip_as_style("dense")$preset, "dense")
    expect_identical(tip_as_style("none")$preset, "none")
    expect_null(tip_as_style("dense")$theme)
  })

  it("reads one part of a style as tip_style(<part>)", {
    theme <- navy()
    got <- tip_as_style(theme)
    expect_s3_class(got, "hovercraft_style")
    expect_identical(got$theme, theme)
    expect_null(got$preset)
    expect_identical(tip_as_style(theme), tip_style(theme = theme))
  })

  it("reads a bslib theme as a theme part", {
    skip_if_not_installed("bslib")
    got <- quiet(tip_as_style(bslib::bs_theme(version = 5)))
    expect_s3_class(got$theme, "hovercraft_theme")
    expect_null(got$preset)
  })

  it("stops on a bare list and points to tip_theme() and tip_style(theme =)", {
    expect_error(tip_as_style(list(bg = "red")), "tip_theme", class = "hovercraft_error_style")
    expect_error(tip_as_style(list("--a" = "1px")), "theme = list", class = "hovercraft_error_style")
    expect_error(tip_as_style(list()), class = "hovercraft_error_style")
  })

  it("stops on a string that looks like CSS, naming the argument", {
    err <- rlang::catch_cnd(tip_as_style("color: red"))
    expect_s3_class(err, "hovercraft_error_style_css")
    expect_match(conditionMessage(err), "`style`", fixed = TRUE)
    expect_error(tip_as_style("color: red", "other"), "`other`", fixed = TRUE)
    expect_error(tip_as_style("font-size: 12px; color: red"), class = "hovercraft_error_style_css")
  })

  it("stops on a name that is not a preset, and on anything else", {
    expect_error(tip_as_style("nope"), class = "hovercraft_error_style")
    expect_error(tip_as_style(c("sober", "dense")), class = "hovercraft_error_argument")
    expect_error(tip_as_style(NA_character_), class = "hovercraft_error_argument")
    expect_error(tip_as_style(3), class = "hovercraft_error_style")
    expect_error(tip_as_style(TRUE), class = "hovercraft_error_style")
    expect_error(tip_as_style(htmltools::tags$b("x")), class = "hovercraft_error_style")
  })
})

describe("printing a style", {
  it("shows a call that makes it again", {
    expect_identical(format(tip_style()), "tip_style()")
    expect_identical(format(tip_style("sober")), "tip_style(\"sober\")")
    expect_identical(format(tip_style(class = "")), "tip_style(class = \"\")")
    expect_identical(
      format(tip_style("dense", theme = tip_theme(light = list(radius = 6)), class = c("a", "b"))),
      "tip_style(\"dense\", theme = tip_theme(light = list(radius = \"6px\")), class = \"a b\")"
    )
    expect_identical(
      format(tip_style("sober", motion = "slide", gap = 12, max_width = 280, pin_width = 500)),
      "tip_style(\"sober\", motion = \"slide\", gap = 12, max_width = 280, pin_width = 500)"
    )
    expect_identical(format(tip_style(gap = 0)), "tip_style(gap = 0)")
    expect_output(returned <- print(tip_style("sober")), "tip_style(\"sober\")", fixed = TRUE)
    expect_s3_class(returned, "hovercraft_style")
  })
})

describe("the page's style, from tip_dependency(style =)", {
  it("is the engine and the runtime alone for NULL and for a style that sets nothing", {
    for (style in list(NULL, tip_style())) {
      deps <- tip_dependency(style = style)
      expect_identical(vapply(deps, function(d) d$name, ""), c("hovercraft", "hovercraft-runtime"))
    }
    expect_null(tip_style_page(tip_style()))
  })

  it("writes the page preset as the engine's attribute on the page, as data the runtime script reads", {
    deps <- tip_dependency(style = "lively")
    expect_length(deps, 3L)
    expect_identical(deps[[2]]$name, "hovercraft-style")
    expect_identical(deps[[3]]$name, "hovercraft-runtime")
    expect_match(dep_ops(deps[[2]]), "\"op\":\"page\"", fixed = TRUE)
    expect_match(dep_ops(deps[[2]]), sprintf("\"%s\":\"lively\"", tip_attribute("preset")), fixed = TRUE)
    expect_null(deps[[2]]$stylesheet)
    expect_null(deps[[2]]$head)
    expect_identical(dep_css(deps[[2]]), "")
  })

  it("writes the page class the same way, and an empty class as the empty string", {
    deps <- tip_dependency(style = tip_style(class = "wide audit-tip"))
    expect_match(dep_ops(deps[[2]]), sprintf("\"%s\":\"wide audit-tip\"", tip_attribute("class")), fixed = TRUE)
    expect_no_match(dep_ops(deps[[2]]), tip_attribute("preset"), fixed = TRUE)
    cleared <- tip_dependency(style = tip_style(class = ""))
    expect_match(dep_ops(cleared[[2]]), sprintf("\"%s\":\"\"", tip_attribute("class")), fixed = TRUE)
  })

  it("writes the theme as CSS on body and the preset as an attribute, in one dependency", {
    deps <- tip_dependency(style = tip_style("sober", theme = navy()))
    expect_length(deps, 3L)
    expect_match(dep_css(deps[[2]]), "^body \\{")
    expect_match(dep_css(deps[[2]]), "--hovercraft-tip-bg: #1b3a6b;", fixed = TRUE)
    expect_match(dep_ops(deps[[2]]), "\"sober\"", fixed = TRUE)
    expect_no_match(dep_css(deps[[2]]), ":root {", fixed = TRUE)
  })

  it("writes the size and motion settings as the engine's variables on body, with no operation", {
    deps <- tip_dependency(style = tip_style(motion = "slide", gap = 12, max_width = 280, pin_width = 500))
    expect_length(deps, 3L)
    expect_match(dep_css(deps[[2]]), "^body \\{")
    expect_match(dep_css(deps[[2]]), "--hovercraft-tip-motion: slide;", fixed = TRUE)
    expect_match(dep_css(deps[[2]]), "--hovercraft-tip-gap: 12px;", fixed = TRUE)
    expect_match(dep_css(deps[[2]]), "--hovercraft-tip-max-width: 280px;", fixed = TRUE)
    expect_match(dep_css(deps[[2]]), "--hovercraft-tip-pin-width: 500px;", fixed = TRUE)
    expect_no_match(dep_ops(deps[[2]]), "\"op\":\"page\"", fixed = TRUE)
  })

  it("writes only the settings that were given, next to a theme and a preset", {
    deps <- tip_dependency(style = tip_style("sober", theme = navy(), gap = 0))
    expect_match(dep_css(deps[[2]]), "--hovercraft-tip-bg: #1b3a6b;", fixed = TRUE)
    expect_match(dep_css(deps[[2]]), "--hovercraft-tip-gap: 0px;", fixed = TRUE)
    expect_no_match(dep_css(deps[[2]]), "motion", fixed = TRUE)
    expect_no_match(dep_css(deps[[2]]), "max-width", fixed = TRUE)
    expect_match(dep_ops(deps[[2]]), "\"sober\"", fixed = TRUE)
  })

  it("writes a theme alone with no operation", {
    deps <- tip_dependency(style = navy())
    expect_no_match(dep_ops(deps[[2]]), "\"op\":\"page\"", fixed = TRUE)
  })

  it("keeps the style when tip_on() adds the plain engine dependency, in either order", {
    page <- tip_dependency(style = "sober")
    tag <- tip_on(button(), "t")
    names_of <- function(deps) vapply(htmltools::resolveDependencies(deps), function(d) d$name, "")
    wanted <- c("hovercraft", "hovercraft-style", "hovercraft-runtime")
    expect_setequal(names_of(c(htmltools::findDependencies(tag), page)), wanted)
    expect_setequal(names_of(c(page, htmltools::findDependencies(tag))), wanted)
    expect_length(names_of(c(page, htmltools::findDependencies(tag))), 3L)
  })

  it("stops on CSS and on a bare list, as style = does everywhere", {
    expect_error(tip_dependency(style = "color: red"), class = "hovercraft_error_style_css")
    expect_error(tip_dependency(style = list(bg = "red")), class = "hovercraft_error_style")
  })
})

describe("tip_on(style =)", {
  it("writes nothing for NULL, and a style that holds nothing", {
    plain <- tip_on(button(), "t")
    expect_identical(tip_on(button(), "t", style = NULL), plain)
    expect_identical(tip_on(button(), "t", style = tip_style()), plain)
  })

  it("writes a preset name as the engine's preset attribute", {
    tag <- tip_on(button(), "t", style = "dense")
    expect_identical(attr_of(tag, tip_attribute("preset")), "dense")
    expect_null(attr_of(tag, tip_attribute("class")))
    expect_null(attr_of(tag, tip_theme_attribute()))
    expect_identical(attr_of(tip_on(button(), "t", style = "none"), tip_attribute("preset")), "none")
  })

  it("writes a class, and an empty class so a tag can clear one", {
    tag <- tip_on(button(), "t", style = tip_style(class = c("wide", "audit-tip")))
    expect_identical(attr_of(tag, tip_attribute("class")), "wide audit-tip")
    cleared <- tip_on(button(), "t", style = tip_style(class = ""))
    expect_identical(attr_of(cleared, tip_attribute("class")), "")
    expect_match(as.character(cleared), sprintf("%s=\"\"", tip_attribute("class")), fixed = TRUE)
  })

  it("gives a tag's theme a marker and a dependency of its own, named by the theme", {
    tag <- tip_on(button(), "t", style = navy())
    hash <- attr_of(tag, tip_theme_attribute())
    expect_match(hash, "^[0-9a-f]{12}$")
    deps <- htmltools::findDependencies(tag)
    expect_identical(
      vapply(deps, function(d) d$name, ""),
      c("hovercraft", paste0("hovercraft-theme-", hash), "hovercraft-runtime")
    )
    css <- dep_css(deps[[2]])
    expect_match(css, sprintf("[%s=\"%s\"] {", tip_theme_attribute(), hash), fixed = TRUE)
    expect_match(css, "--hovercraft-tip-bg: #1b3a6b;", fixed = TRUE)
    expect_no_match(css, "body", fixed = TRUE)
    expect_null(attr_of(tag, tip_attribute("preset")))
  })

  it("shares one dependency between tags with the same theme, and gives another theme its own", {
    one <- tip_on(button(), "t", style = navy())
    two <- tip_on(button(), "u", style = tip_style(theme = navy()))
    other <- tip_on(button(), "v", style = tip_theme(light = list(bg = "#222222", ink = "#ffffff")))
    expect_identical(attr_of(one, tip_theme_attribute()), attr_of(two, tip_theme_attribute()))
    expect_false(identical(attr_of(one, tip_theme_attribute()), attr_of(other, tip_theme_attribute())))
    deps <- htmltools::resolveDependencies(htmltools::findDependencies(list(one, two, other)))
    expect_length(deps, 4L)
  })

  it("writes the preset, theme and class of a tip_style() together", {
    tag <- tip_on(button(), "t", style = tip_style("editorial", theme = navy(), class = "wide"))
    expect_identical(attr_of(tag, tip_attribute("preset")), "editorial")
    expect_identical(attr_of(tag, tip_attribute("class")), "wide")
    expect_match(attr_of(tag, tip_theme_attribute()), "^[0-9a-f]{12}$")
  })

  it("writes the size and motion settings under a marker of the tag, in a dependency named by them", {
    tag <- tip_on(button(), "t", style = tip_style(motion = "none", gap = 2, max_width = 200, pin_width = 300))
    hash <- attr_of(tag, tip_theme_attribute())
    expect_match(hash, "^[0-9a-f]{12}$")
    deps <- htmltools::findDependencies(tag)
    expect_identical(
      vapply(deps, function(d) d$name, ""),
      c("hovercraft", paste0("hovercraft-theme-", hash), "hovercraft-runtime")
    )
    css <- dep_css(deps[[2]])
    expect_match(css, sprintf("[%s=\"%s\"] {", tip_theme_attribute(), hash), fixed = TRUE)
    expect_match(css, "--hovercraft-tip-motion: none;", fixed = TRUE)
    expect_match(css, "--hovercraft-tip-gap: 2px;", fixed = TRUE)
    expect_match(css, "--hovercraft-tip-max-width: 200px;", fixed = TRUE)
    expect_match(css, "--hovercraft-tip-pin-width: 300px;", fixed = TRUE)
    expect_no_match(css, "body", fixed = TRUE)
  })

  it("writes only the settings the tag sets, so the page's others still apply", {
    tag <- tip_on(button(), "t", style = tip_style(gap = 2))
    css <- dep_css(htmltools::findDependencies(tag)[[2]])
    expect_match(css, "--hovercraft-tip-gap: 2px;", fixed = TRUE)
    expect_no_match(css, "motion", fixed = TRUE)
  })

  it("shares a dependency between tags with the same settings, and not with other ones", {
    one <- tip_on(button(), "t", style = tip_style(gap = 2, motion = "fade"))
    two <- tip_on(button(), "u", style = tip_style(motion = "fade", gap = 2))
    other <- tip_on(button(), "v", style = tip_style(gap = 3, motion = "fade"))
    expect_identical(attr_of(one, tip_theme_attribute()), attr_of(two, tip_theme_attribute()))
    expect_false(identical(attr_of(one, tip_theme_attribute()), attr_of(other, tip_theme_attribute())))
  })

  it("puts a theme and the settings of one tip_style() under one marker and one dependency", {
    tag <- tip_on(button(), "t", style = tip_style(theme = navy(), gap = 2))
    deps <- htmltools::findDependencies(tag)
    expect_length(deps, 3L)
    expect_match(dep_css(deps[[2]]), "--hovercraft-tip-bg: #1b3a6b;", fixed = TRUE)
    expect_match(dep_css(deps[[2]]), "--hovercraft-tip-gap: 2px;", fixed = TRUE)
    expect_false(
      identical(
        attr_of(tag, tip_theme_attribute()),
        attr_of(tip_on(button(), "t", style = navy()), tip_theme_attribute())
      )
    )
  })

  it("replaces the settings of an earlier tip, and writes no marker for none", {
    first <- tip_on(button(), "t", style = tip_style(gap = 2))
    expect_null(attr_of(tip_on(first, "u"), tip_theme_attribute()))
    expect_identical(sum(names(tip_on(first, "u", style = tip_style(gap = 4))$attribs) == tip_theme_attribute()), 1L)
  })

  it("writes no theme marker for a theme that sets nothing", {
    tag <- tip_on(button(), "t", style = tip_style("sober", theme = tip_theme()))
    expect_null(attr_of(tag, tip_theme_attribute()))
    # The engine and the runtime only: no dependency of a theme.
    expect_identical(
      vapply(htmltools::findDependencies(tag), function(d) d$name, ""),
      c("hovercraft", "hovercraft-runtime")
    )
  })

  it("replaces the look of an earlier tip, and never joins two values in one attribute", {
    first <- tip_on(button(), "t", style = tip_style("sober", theme = navy(), class = "wide"))
    again <- tip_on(first, "u", style = "dense")
    expect_identical(attr_of(again, tip_attribute("preset")), "dense")
    expect_null(attr_of(again, tip_attribute("class")))
    expect_null(attr_of(again, tip_theme_attribute()))
    bare <- tip_on(first, "u")
    expect_null(attr_of(bare, tip_attribute("preset")))
    expect_identical(sum(names(again$attribs) == tip_attribute("preset")), 1L)
  })

  it("stops on CSS, a bare list and a bad preset, before it looks at the text", {
    expect_error(tip_on(button(), "t", style = "color: red"), class = "hovercraft_error_style_css")
    expect_error(tip_on(button(), "t", style = list(bg = "red")), class = "hovercraft_error_style")
    expect_error(tip_on(button(), "t", style = "nope"), class = "hovercraft_error_style")
    expect_error(tip_on(button(), "", style = "color: red"), class = "hovercraft_error_style_css")
  })

  it("leaves a tag with no tip as it was, whatever the style", {
    tag <- button()
    expect_identical(tip_on(tag, NA, style = "sober"), tag)
  })

  it("takes a bslib theme as a theme part", {
    skip_if_not_installed("bslib")
    tag <- quiet(tip_on(button(), "t", style = bslib::bs_theme(version = 5)))
    expect_match(attr_of(tag, tip_theme_attribute()), "^[0-9a-f]{12}$")
  })
})
