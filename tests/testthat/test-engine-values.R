# What 0.1.0 promises about the values the engine reads: the accepted placements, preset names, marker types, block types,
# event names and filters. They come from the copied engine, so a new copy that removes or renames one fails here, and one
# that adds a value passes. A name that is on this list is part of the API: take it off only with a major version.
missing_from <- function(promised, actual) setdiff(promised, actual)

describe("the engine's value lists", {
  it("tells a value that is gone from one that is new", {
    expect_identical(missing_from(c("a", "b"), c("a", "b", "c")), character())
    expect_identical(missing_from(c("a", "b"), c("a", "c")), "b")
    expect_identical(missing_from(c("a", "b"), c("a", "bb")), "b")
  })

  it("still has every placement", {
    expect_identical(
      missing_from(c("auto", "top", "bottom", "left", "right"), unlist(tip_rules()$placement$values)),
      character()
    )
    expect_identical(tip_rules()$placement$default, "auto")
  })

  it("still has every preset, and `tip_presets()` lists them", {
    promised <- c("dense", "editorial", "lively", "modern", "sober")
    expect_identical(missing_from(promised, tip_presets()$name), character())
    expect_identical(missing_from(promised, sub("\\.json$", "", list.files(tip_path("presets")))), character())
  })

  it("still has every marker type", {
    expect_identical(missing_from(c("none", "underline", "icon", "corner"), tip_marker_types()), character())
  })

  it("still has every block type and every filter", {
    expect_identical(
      missing_from(c("title", "text", "badge", "fields", "code", "group", "host"), names(tip_rules()$blocks)),
      character()
    )
    expect_identical(missing_from(c("number", "percent"), names(tip_rules()$filters)), character())
  })

  it("still has every event, every state of an open tip and every anchor", {
    expect_identical(missing_from(c("hint", "tip", "copy"), names(tip_contract()$events)), character())
    expect_identical(missing_from(c("hint", "pin"), unlist(tip_rules()$open$values)), character())
    expect_identical(missing_from(c("item", "content"), unlist(tip_rules()$anchor$values)), character())
  })

  it("keeps the inputs a tag can ask for", {
    expect_identical(missing_from(c("hint", "pin", "copy"), tip_event_names()), character())
  })
})
