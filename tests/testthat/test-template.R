skip_if_not_installed("withr")

describe("tip_template()", {
  it("keeps the blocks in order and accepts plain lists, checked the same way", {
    got <- tip_template(
      tip_title("{{ label }}"),
      list(type = "text", text = "x"),
      list(type = "badge", text = "b", tone = "success")
    )
    expect_s3_class(got, "hovercraft_template")
    expect_identical(vapply(got, function(block) block$type, ""), c("title", "text", "badge"))
    expect_s3_class(got[[2]], "hovercraft_block")
  })

  it("reads a plain badge's engine word `tone` as the status, so the same four words apply", {
    got <- tip_template(list(type = "badge", text = "b", tone = "danger"))
    expect_identical(unclass(got[[1]])$tone, "danger")
    expect_error(tip_template(list(type = "badge", text = "b", tone = "ok")), class = "hovercraft_error_status")
  })

  it("rebuilds a fields block and a code block from plain lists", {
    got <- tip_template(
      list(type = "fields", fields = list(list(label = "A", value = "{{ a }}"))),
      list(type = "code", field = "sql", language = "sql")
    )
    expect_identical(unclass(got[[1]])$fields[[1]]$value, "{{ a }}")
    expect_identical(unclass(got[[2]])$language, "sql")
  })

  it("needs a block", {
    expect_error(tip_template(), class = "hovercraft_error_template")
  })

  it("refuses what is not a block", {
    expect_error(tip_template("text"), class = "hovercraft_error_block")
    expect_error(tip_template(list(text = "no type")), class = "hovercraft_error_block")
    expect_error(tip_template(list(type = c("a", "b"))), class = "hovercraft_error_block")
    expect_error(tip_template(list(type = "blink")), class = "hovercraft_error_block")
    expect_error(tip_template(list(type = "text", text = "x", colour = "red")), class = "hovercraft_error_block")
  })

  it("refuses a block type of a later version", {
    expect_error(tip_template(list(type = "rows")), class = "hovercraft_error_later")
    expect_error(tip_template(list(type = "visual")), class = "hovercraft_error_later")
  })

  it("refuses a block type the engine has and this package has no constructor for yet", {
    expect_error(
      tip_template(list(type = "group", id = "g")),
      "comes with a later version",
      class = "hovercraft_error_later"
    )
    expect_error(
      tip_template(list(type = "host", id = "h")),
      "comes with a later version",
      class = "hovercraft_error_later"
    )
  })

  it("refuses a field the engine takes and the constructor does not yet", {
    expect_error(
      tip_template(list(type = "text", text = "x", parent = "g")),
      "later version",
      class = "hovercraft_error_later"
    )
    expect_error(
      tip_template(list(type = "title", text = "x", kicker = "k")),
      "later version",
      class = "hovercraft_error_later"
    )
    expect_error(
      tip_template(list(type = "title", text = "x", meta = "m")),
      "later version",
      class = "hovercraft_error_later"
    )
    expect_error(
      tip_template(list(type = "fields", fields = list(list(label = "a", value = "b")), parent = "g")),
      "later version",
      class = "hovercraft_error_later"
    )
  })

  it("checks the plain list through its constructor", {
    expect_error(tip_template(list(type = "text", text = "{{ a")), class = "hovercraft_error_placeholder")
  })

  it("prints the block types", {
    expect_output(print(tip_template(tip_title("a"), tip_text("b"))), "<tip template: title, text>", fixed = TRUE)
  })
})

describe("tip_template_fields()", {
  it("lists the fields the placeholders and the code block name, once each", {
    template <- tip_template(
      tip_title("{{ label }}"),
      tip_text("{{ a | number:1 }} and {{ label }}"),
      tip_badge("{{ status }}"),
      tip_fields(Records = "{{ records }}"),
      tip_code("sql")
    )
    expect_identical(tip_template_fields(template), c("label", "a", "status", "records", "sql"))
  })

  it("reads every placeholder the engine lists, including a title's kicker and meta", {
    rules <- tip_rules()
    local_mocked_bindings(tip_rules = function() rules)
    template <- tip_template(tip_title("{{ label }}"))
    # A block carrying the title's optional fields, as a later version will make them.
    template[[1]]$kicker <- "{{ kind }}"
    template[[1]]$meta <- "{{ when | number:0 }}"
    expect_identical(tip_template_fields(template), c("label", "kind", "when"))
  })
})

describe("tip_define()", {
  withr::defer(tip_reset())

  it("returns a script that defines each template under the engine's global, with the dependency", {
    tip_reset()
    got <- tip_define(diff = tip_template(tip_title("{{ label }}")), plain = list(list(type = "text", text = "x")))
    html <- as.character(got)
    expect_match(html, paste0(tip_names()$global, ".define(\"diff\", ["), fixed = TRUE)
    expect_match(html, paste0(tip_names()$global, ".define(\"plain\", ["), fixed = TRUE)
    expect_identical(htmltools::findDependencies(got)[[1]]$name, "hovercraft")
  })

  it("remembers the templates, with their fields", {
    tip_reset()
    tip_define(diff = tip_template(tip_title("{{ label }}"), tip_text("{{ n }}")))
    expect_identical(tip_known()$diff$fields, c("label", "n"))
  })

  it("accepts one bare block as a template", {
    tip_reset()
    expect_match(as.character(tip_define(one = list(type = "text", text = "x"))), "define(\"one\"", fixed = TRUE)
  })

  it("keeps a value from closing the script", {
    got <- as.character(tip_define(a = tip_template(tip_text("</script> is text"))))
    expect_false(grepl("</script> is", got, fixed = TRUE))
  })

  it("needs names that follow the engine's rule", {
    expect_error(tip_define(tip_template(tip_text("a"))), "names")
    expect_error(tip_define(), "\\.\\.\\.")
    expect_error(tip_define(`My Template` = tip_template(tip_text("a"))), class = "hovercraft_error_name")
    expect_error(tip_define(`1st` = tip_template(tip_text("a"))), class = "hovercraft_error_name")
    expect_error(tip_define(`function` = tip_template(tip_text("a"))), class = "hovercraft_error_name")
  })

  it("checks a list of blocks the same way as tip_template()", {
    expect_error(tip_define(a = list("not a block")), class = "hovercraft_error_block")
  })

  it("accepts Shiny ns() module-namespaced template names so two module copies do not overwrite each other", {
    skip_if_not_installed("shiny")
    tip_reset()
    ns_a <- shiny::NS("mod_a")
    ns_b <- shiny::NS("mod_b")
    ns_nested <- shiny::NS("outer")("inner")
    expect_no_warning(
      a <- tip_define(!!ns_a("card") := tip_template(tip_title("Alpha"), tip_text("{{ a }}")))
    )
    expect_no_warning(
      b <- tip_define(!!ns_b("card") := tip_template(tip_title("Beta"), tip_text("{{ b }}")))
    )
    expect_no_warning(
      tip_define(!!paste0(ns_nested, "-card") := tip_template(tip_text("Nested")))
    )
    expect_match(as.character(a), "define(\"mod_a-card\"", fixed = TRUE)
    expect_match(as.character(b), "define(\"mod_b-card\"", fixed = TRUE)
    expect_identical(tip_known()[["mod_a-card"]]$fields, "a")
    expect_identical(tip_known()[["mod_b-card"]]$fields, "b")
    expect_error(tip_define(`-card` = tip_template(tip_text("a"))), class = "hovercraft_error_name")
    expect_error(tip_define(`mod-` = tip_template(tip_text("a"))), class = "hovercraft_error_name")
    expect_error(tip_define(`mod--card` = tip_template(tip_text("a"))), class = "hovercraft_error_name")
    expect_error(tip_define(`mod-1st` = tip_template(tip_text("a"))), class = "hovercraft_error_name")
    expect_error(tip_define(`mod-function` = tip_template(tip_text("a"))), class = "hovercraft_error_name")
    expect_error(tip_define(`bad name-card` = tip_template(tip_text("a"))), class = "hovercraft_error_name")
  })
})

describe("tip_template() with head, body and foot slots", {
  withr::defer(tip_reset())
  tip_reset()

  it("orders head, body and foot blocks and records their fields including dynamic badge status", {
    tpl <- tip_template(
      head = list(tip_title("{{ label }}"), tip_badge("{{ status }}", status = "{{ tone }}")),
      body = tip_fields(Count = "{{ n | number:0 }}"),
      foot = tip_text("Source: {{ source }}")
    )
    expect_s3_class(tpl, "hovercraft_template")
    expect_identical(vapply(tpl, function(b) b$type, ""), c("title", "badge", "fields", "text"))
    expect_identical(tip_template_fields(tpl), c("label", "status", "tone", "n", "source"))
    expect_output(print(tpl), "<tip template: head(title, badge), body(fields), foot(text)>", fixed = TRUE)
  })

  it("serializes foot into a footer group with a divider, and routes dynamic badge status to badge_data", {
    tpl <- tip_template(
      head = tip_title("{{ label }}"),
      body = tip_badge("{{ status }}", status = "{{ tone }}"),
      foot = tip_text("Footnote")
    )
    plain <- tip_blocks_plain(tpl)
    types <- vapply(plain, function(b) b$type, "")
    expect_identical(types, c("title", "badge_data", "slot_divider", "group", "text"))
    expect_identical(plain[[5]]$parent, "hc_foot")

    foot_only <- tip_template(foot = tip_text("Only foot"))
    expect_identical(vapply(tip_blocks_plain(foot_only), function(b) b$type, ""), c("group", "text"))
  })

  it("allows positional body blocks alongside head or foot, and rejects ambiguous or unknown slots", {
    mixed <- tip_template(tip_text("Body"), head = tip_title("Head"), foot = tip_text("Foot"))
    expect_identical(vapply(mixed, function(b) b$type, ""), c("title", "text", "text"))

    expect_error(
      tip_template(tip_text("A"), body = tip_text("B")),
      class = "hovercraft_error_template"
    )
    expect_error(
      tip_template(header = tip_title("A")),
      "head",
      class = "hovercraft_error_template"
    )
    expect_error(tip_template(head = list()), class = "hovercraft_error_template")
    expect_error(tip_template(head = "not a block"), class = "hovercraft_error_block")
  })
})
