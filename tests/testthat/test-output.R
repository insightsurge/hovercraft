skip_if_not_installed("withr")

describe("tip_output()", {
  withr::defer(tip_reset())
  tip_reset()

  it("builds a classed output block with keep_alive = TRUE by default and accepts module ids", {
    got <- tip_output("detail_plot")
    expect_s3_class(got, "hovercraft_block")
    expect_identical(unclass(got), list(type = "output", output = "detail_plot", keep_alive = TRUE))

    mod <- tip_output("mod1-detail_table", keep_alive = FALSE)
    expect_identical(unclass(mod), list(type = "output", output = "mod1-detail_table", keep_alive = FALSE))
  })

  it("accepts a Shiny output tag and extracts its id while attaching the tag in tip_define()", {
    skip_if_not_installed("shiny")
    block <- tip_output(shiny::plotOutput("mod1-my_plot"))
    expect_identical(block$output, "mod1-my_plot")
    expect_true(isTRUE(block$keep_alive))
    expect_false(is.null(attr(block, "hovercraft_output_tag")))

    defined <- tip_define(card = tip_template(tip_title("{{ label }}"), block))
    html <- as.character(defined)
    expect_match(html, "\"type\":\"output\",\"output\":\"mod1-my_plot\",\"keep_alive\":true", fixed = TRUE)
    expect_match(html, "{\"op\":\"park\",\"id\":\"mod1-my_plot\"}", fixed = TRUE)
    expect_match(html, "<script type=\"application/json\" data-hovercraft-ops", fixed = TRUE)
    expect_match(html, "class=\"hc-output-park\"", fixed = TRUE)
    expect_match(html, "id=\"mod1-my_plot\"", fixed = TRUE)
  })

  it("extracts the inner output id when wrapped in shinycssloaders::withSpinner()", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("shinycssloaders")
    wrapped <- shinycssloaders::withSpinner(shiny::plotOutput("mod1-spin_plot"))
    block <- tip_output(wrapped)
    expect_identical(block$output, "mod1-spin_plot")
  })

  it("extracts a fallback id from a custom tag without Shiny classes and refuses tags with no id", {
    tag_with_id <- htmltools::tags$div(id = "custom_out")
    expect_identical(tip_output(tag_with_id)$output, "custom_out")

    no_id <- htmltools::tags$div(htmltools::tags$span("hi"))
    expect_error(tip_output(no_id), "no Shiny output id", class = "hovercraft_error_argument")

    spinner_only <- htmltools::tags$div(id = "spinner-123")
    expect_error(tip_output(spinner_only), "no Shiny output id", class = "hovercraft_error_argument")
  })

  it("checks output and keep_alive arguments with classed errors", {
    expect_error(tip_output(1), "output", class = "hovercraft_error_argument")
    expect_error(tip_output(""), "output", class = "hovercraft_error_argument")
    expect_error(tip_output("has space"), "output", class = "hovercraft_error_argument")
    expect_error(tip_output(NULL), "output", class = "hovercraft_error_argument")
    expect_error(tip_output("p", keep_alive = "yes"), "keep_alive", class = "hovercraft_error_argument")
    expect_error(tip_output("p", keep_alive = NA), "keep_alive", class = "hovercraft_error_argument")
  })

  it("works as a plain list inside tip_template() and rejects unknown fields", {
    tpl <- tip_template(list(type = "output", output = "p", keep_alive = FALSE))
    expect_identical(tpl[[1]]$output, "p")
    expect_false(tpl[[1]]$keep_alive)
    expect_error(
      tip_template(list(type = "output", output = "p", extra = 1)),
      class = "hovercraft_error_block"
    )
  })

  it("writes deterministic markup with no random or per-session ids for bindCache()", {
    tpl_a <- tip_define(live = tip_template(tip_title("{{ label }}"), tip_output("detail_plot")))
    tpl_b <- tip_define(live = tip_template(tip_title("{{ label }}"), tip_output("detail_plot")))
    expect_identical(as.character(tpl_a), as.character(tpl_b))

    btn_a <- tip_on(htmltools::tags$button("A"), template = "live", record = list(label = "Owls"), id = "item_a")
    btn_b <- tip_on(htmltools::tags$button("A"), template = "live", record = list(label = "Owls"), id = "item_a")
    expect_identical(as.character(btn_a), as.character(btn_b))
    expect_identical(tip_known()$live$fields, "label")
  })

  it("deduplicates parked output tags when two templates share one output", {
    skip_if_not_installed("shiny")
    tag <- shiny::plotOutput("shared_p")
    got <- tip_define(
      one = tip_template(tip_output(tag)),
      two = tip_template(tip_output(tag))
    )
    matches <- gregexpr("id=\"shared_p\"", as.character(got), fixed = TRUE)[[1]]
    expect_length(matches, 1L)
  })
})
