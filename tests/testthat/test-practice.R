# Non-browser checks for the Shiny-practices checklist: bindCache() markup stability, module template isolation,
# dependency structure (frameworks / reverse proxy), and scale (10,000 rows through tip_table() and many tip_on()).

describe("bindCache() markup determinism", {
  it("produces identical HTML across repeated tip_on() calls with no random or per-session ids", {
    render_once <- function() {
      tag <- tip_on(
        htmltools::tags$button(id = "save", "Save"),
        "Saves the **record**",
        style = "sober",
        marker = "underline",
        placement = "top",
        id = "save_tip",
        events = "hint"
      )
      as.character(htmltools::renderTags(tag)$html)
    }
    expect_identical(render_once(), render_once())
  })

  it("produces identical HTML across repeated tip_define() and template tip_on() calls", {
    render_once <- function() {
      tip_reset()
      withr::defer(tip_reset())
      defs <- tip_define(
        card = tip_template(tip_title("{{ name }}"), tip_text("{{ count | number:0 }} items"))
      )
      btn <- tip_on(
        htmltools::tags$button(id = "item", "Item"),
        template = "card",
        record = list(name = "Alpha", count = 4L)
      )
      as.character(htmltools::renderTags(htmltools::tagList(defs, btn))$html)
    }
    expect_identical(render_once(), render_once())
  })
})

describe("module template names and ids", {
  it("keeps templates separate when two module copies prefix their names with the module id", {
    tip_reset()
    withr::defer(tip_reset())
    expect_no_warning({
      a <- tip_define(mod_a_card = tip_template(tip_text("A: {{ label }}")))
      b <- tip_define(mod_b_card = tip_template(tip_text("B: {{ label }}")))
    })
    expect_match(as.character(a), "mod_a_card")
    expect_match(as.character(b), "mod_b_card")
  })

  it("accepts hyphenated ns() ids in tip_on() and in template names (module-safe names)", {
    skip_if_not_installed("shiny")
    ns <- shiny::NS("outer-inner")
    tag <- tip_on(shiny::actionButton(ns("go"), "Go"), "Nested tip", id = ns("go_tip"), events = "hint")
    expect_identical(htmltools::tagGetAttribute(tag, tip_attribute("id")), "outer-inner-go_tip")
    expect_no_error(
      do.call(tip_define, stats::setNames(list(tip_template(tip_text("x"))), ns("card")))
    )
  })
})

describe("app frameworks and reverse-proxy dependency structure", {
  it("ships use_hovercraft() as a package-relative htmlDependency with no absolute paths", {
    generated <- tip_on(
      htmltools::tags$button("Go"),
      "Short",
      style = tip_style(theme = list(radius = 9)),
      more = tip_more(tip_template(tip_text("Long")), on = "hover")
    )
    deps <- c(
      use_hovercraft(),
      use_hovercraft(style = tip_style("sober", theme = list("--hovercraft-tip-bg" = "#1b3a6b"))),
      htmltools::findDependencies(generated)
    )
    for (dep in deps) {
      expect_s3_class(dep, "html_dependency")
      expect_identical(dep$package, "hovercraft", info = dep$name)
      expect_false(startsWith(dep$src$file, "/"), info = dep$name)
      expect_false(any(startsWith(dep$script %||% "", "/")))
      expect_false(any(startsWith(dep$stylesheet %||% "", "/")))
    }
    expect_identical(use_hovercraft()[[1]]$src$file, "engine/www")
  })
})

describe("production checks (strict CSP and multi-session R state)", {
  it("emits no inline <script> or <style> tags under a strict Content-Security-Policy", {
    tip_reset()
    withr::defer(tip_reset())
    # No dependency carries markup in its head: files and data only. (test-csp-browser.R serves a page under the policy.)
    deps <- c(
      use_hovercraft(style = tip_style("sober", theme = list("--hovercraft-tip-radius" = "9px"), gap = 3)),
      tip_on(
        htmltools::tags$button("A"),
        "a",
        style = tip_style(gap = 2),
        more = tip_template(tip_text("b")),
        pin = TRUE,
        id = "a"
      ) |>
        htmltools::findDependencies(),
      tip_on(htmltools::tags$button("B", disabled = NA), "b", when_disabled = "why") |> htmltools::findDependencies()
    )
    for (dep in deps) {
      expect_false(grepl("<script|<style", dep$head %||% "", ignore.case = TRUE), info = dep$name)
    }
    def_html <- as.character(htmltools::renderTags(tip_define(csp_card = tip_template(tip_text("x"))))$html)
    expect_false(grepl("<style", def_html, ignore.case = TRUE))
    # The one script is a JSON data block, which a browser does not run.
    scripts <- regmatches(def_html, gregexpr("<script[^>]*>", def_html))[[1]]
    expect_true(all(grepl("type=\"application/json\"", scripts, fixed = TRUE)))
  })

  it("isolates tip_define() template registries across separate Shiny sessions in one R process", {
    skip_if_not_installed("shiny")
    tip_reset()
    withr::defer(tip_reset())
    one <- shiny::MockShinySession$new()
    two <- shiny::MockShinySession$new()
    # Session one defines "shared" with field `a`; session two defines "shared" with field `b`.
    shiny::withReactiveDomain(one, tip_define(shared = tip_template(tip_text("A: {{ a }}"))))
    shiny::withReactiveDomain(two, expect_no_warning(tip_define(shared = tip_template(tip_text("B: {{ b }}")))))
    button <- function() htmltools::tags$button("Go")
    # Each session is checked against its own definition, not the other's.
    shiny::withReactiveDomain(one, {
      expect_no_error(tip_on(button(), template = "shared", record = list(a = 1L)))
      expect_error(tip_on(button(), template = "shared", record = list(b = 1L)), class = "hovercraft_error_field")
    })
    shiny::withReactiveDomain(two, {
      expect_no_error(tip_on(button(), template = "shared", record = list(b = 1L)))
      expect_error(tip_on(button(), template = "shared", record = list(a = 1L)), class = "hovercraft_error_field")
    })
    # A template defined outside a session (the page's UI) is known to every session, which may replace it for itself.
    tip_define(page_wide = tip_template(tip_text("{{ x }}")))
    shiny::withReactiveDomain(one, expect_no_error(tip_on(button(), template = "page_wide", record = list(x = 1L))))
    shiny::withReactiveDomain(two, expect_no_error(tip_on(button(), template = "page_wide", record = list(x = 1L))))
    # A session that defines a page-wide name again for itself replaces it for itself only, and says nothing.
    shiny::withReactiveDomain(
      one,
      expect_no_warning(tip_define(page_wide = tip_template(tip_text("{{ y }}"))))
    )
    shiny::withReactiveDomain(
      one,
      expect_error(tip_on(button(), template = "page_wide", record = list(x = 1L)), class = "hovercraft_error_field")
    )
    shiny::withReactiveDomain(two, expect_no_error(tip_on(button(), template = "page_wide", record = list(x = 1L))))
    # The page-wide registry did not take the sessions' definitions.
    expect_identical(names(tip_known()), "page_wide")
  })
})

describe("scale", {
  it("handles 500 tip_on() tags on one page", {
    skip_on_cran()
    tags_list <- lapply(seq_len(500L), function(i) {
      tip_on(htmltools::tags$button(id = paste0("b", i), paste("Btn", i)), paste("Tip", i))
    })
    expect_length(tags_list, 500L)
    expect_identical(htmltools::tagGetAttribute(tags_list[[500L]], tip_names()$attribute), "Tip 500")
  })

  it("handles 10,000 rows through tip_table() with DT", {
    skip_on_cran()
    skip_if_not_installed("DT")
    n <- 10000L
    big <- data.frame(
      team = paste0("Team_", seq_len(n)),
      wins = seq_len(n) %% 50L,
      note = ifelse(seq_len(n) %% 3L == 0L, NA_character_, paste0("Note ", seq_len(n)))
    )
    elapsed <- system.time({
      tbl <- tip_table(
        DT::datatable(big, rownames = FALSE),
        rows = "**{{ team }}**: {{ wins | number:0 }} wins",
        cells = c(note = "Note: {{ note }}")
      )
    })[["elapsed"]]
    expect_s3_class(tbl, "datatables")
    expect_true(is.finite(elapsed))
  })

  it("handles 10,000 rows through tip_table() with reactable", {
    skip_on_cran()
    skip_if_not_installed("reactable")
    n <- 10000L
    big <- data.frame(
      team = paste0("Team_", seq_len(n)),
      wins = seq_len(n) %% 50L,
      note = ifelse(seq_len(n) %% 3L == 0L, NA_character_, paste0("Note ", seq_len(n)))
    )
    elapsed <- system.time({
      tbl <- tip_table(
        reactable::reactable(big),
        rows = "**{{ team }}**: {{ wins | number:0 }} wins",
        cells = c(note = "Note: {{ note }}")
      )
    })[["elapsed"]]
    expect_s3_class(tbl, "reactable")
    expect_true(is.finite(elapsed))
  })
})
