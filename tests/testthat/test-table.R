# What hovercraft's own layer (inst/table/table.js) relies on in the engine: its attribute names, read from the engine's
# own files, and its public define() call. A new engine copy that drops one fails here, naming it.
describe("the table layer's engine names", {
  it("are all in the engine", {
    attributes <- c(template = "template", record = "record", placement = "placement", kind = "kind")
    for (key in attributes) {
      expect_true(nzchar(tip_attribute(key)), info = key)
      expect_true(
        tip_attribute(key) %in% unlist(jsonlite::read_json(tip_path("tip.json"))$attributes),
        info = paste("tip.json lacks the attribute for", key)
      )
    }
    script <- paste(readLines(tip_path("www", "tip.js"), warn = FALSE), collapse = "\n")
    expect_match(script, "define,", fixed = TRUE)
    expect_match(script, "window.Hovercraft = Object.freeze({", fixed = TRUE)
  })
})

skip_if_not_installed("DT")
skip_if_not_installed("reactable")

scores <- data.frame(
  team = c("Owls", "Foxes", "Hares"),
  wins = c(7, 5, 2),
  losses = c(3, 5, 8),
  note = c("Top of the table", NA, ""),
  stringsAsFactors = FALSE
)
dt <- function(...) DT::datatable(scores, rownames = FALSE, ...)
rt <- function(...) reactable::reactable(scores, ...)
js <- function(x) as.character(x)

describe("tip_table() on a DT table", {
  it("adds a hidden column with each row's record, holding only the columns the tips name", {
    got <- tip_table(dt(), rows = "**{{ team }}**: {{ wins | number:0 }} wins")
    records <- got$x$data[[".hovercraft"]]
    expect_length(records, 3L)
    expect_identical(jsonlite::fromJSON(records[[1]]), list(team = "Owls", wins = 7L))
    expect_identical(unname(attr(got$x, "colnames")), c("team", "wins", "losses", "note", ".hovercraft"))
    expect_match(got$x$container, "<th>.hovercraft</th></tr>", fixed = TRUE)
    hidden <- Filter(function(def) isTRUE(def$visible == FALSE), got$x$options$columnDefs)[[1]]
    expect_identical(hidden$targets, 4L)
    expect_identical(hidden$name, ".hovercraft")
    expect_false(hidden$searchable)
  })

  it("marks rows through the engine's own attribute names", {
    got <- tip_table(dt(), rows = "{{ team }}")
    expect_match(js(got$x$options$createdRow), tip_attribute("template"), fixed = TRUE)
    expect_match(js(got$x$options$createdRow), "data[4]", fixed = TRUE)
    expect_match(js(got$x$options$createdRow), tip_attribute("record"), fixed = TRUE)
  })

  it("marks the cells of the named columns by their position, and the header cells", {
    got <- tip_table(dt(), cells = c(wins = "{{ wins }} wins"), headers = c(losses = "Games lost"))
    defs <- Filter(function(def) !is.null(def$createdCell), got$x$options$columnDefs)
    expect_length(defs, 1L)
    expect_identical(defs[[1]]$targets, 1L)
    expect_match(js(got$x$options$initComplete), "\"2\":{\"attrs\"", fixed = TRUE)
  })

  it("writes no record column when no tip reads the row", {
    got <- tip_table(dt(), rows = "A fixed note")
    expect_null(got$x$data[[".hovercraft"]])
    expect_match(js(got$x$options$createdRow), "null", fixed = TRUE)
  })

  it("counts the row names column in the positions when the table has one", {
    got <- tip_table(DT::datatable(scores), cells = c(wins = "{{ wins }}"))
    defs <- Filter(function(def) !is.null(def$createdCell), got$x$options$columnDefs)
    expect_identical(defs[[1]]$targets, 2L)
    expect_identical(names(got$x$data)[[1]], " ")
    expect_identical(got$x$data[[".hovercraft"]][[1]], "{\"wins\":7}")
  })

  it("writes the placement and the id as the engine's attributes", {
    got <- tip_table(dt(), rows = "{{ team }}", placement = "right", id = "league")
    expect_match(js(got$x$options$createdRow), tip_attribute("placement"), fixed = TRUE)
    expect_match(js(got$x$options$createdRow), "\"right\"", fixed = TRUE)
    expect_match(js(got$x$options$createdRow), tip_attribute("kind"), fixed = TRUE)
  })

  it("defines the templates and attaches the engine and the layer", {
    got <- tip_table(dt(), rows = "{{ team }}", headers = c(wins = "Games won"))
    deps <- vapply(got$dependencies, function(d) d$name, "")
    expect_true(all(c("hovercraft", "hovercraft-table") %in% deps))
    hook <- got$jsHooks$render[[1]]
    expect_length(hook$data$defines, 2L)
    expect_true(all(grepl("^table_[a-f0-9]{12}$", names(hook$data$defines))))
  })

  it("gives the same template the same name, so one text is defined once", {
    got <- tip_table(dt(), rows = "Same", cells = c(wins = "Same", losses = "Same"))
    expect_length(got$jsHooks$render[[1]]$data$defines, 1L)
  })

  it("accepts a tip_template()", {
    got <- tip_table(dt(), rows = tip_template(tip_title("{{ team }}"), tip_text("{{ wins | number:0 }} wins")))
    expect_identical(jsonlite::fromJSON(got$x$data[[".hovercraft"]][[2]]), list(team = "Foxes", wins = 5L))
  })

  it("leaves out a cell or a row whose words are NA or empty", {
    got <- tip_table(dt(), rows = NA_character_, cells = c(wins = "{{ wins }}", losses = NA, note = ""))
    expect_null(got$x$options$createdRow)
    expect_length(Filter(function(def) !is.null(def$createdCell), got$x$options$columnDefs), 1L)
  })

  it("refuses options it would overwrite and a header of more than one row", {
    expect_error(
      tip_table(dt(options = list(createdRow = DT::JS("function() {}"))), rows = "{{ team }}"),
      class = "hovercraft_error_widget"
    )
    two <- dt(
      container = htmltools::tags$table(
        htmltools::tags$thead(
          htmltools::tags$tr(htmltools::tags$th(colspan = 2, "A"), htmltools::tags$th(colspan = 2, "B")),
          htmltools::tags$tr(lapply(names(scores), htmltools::tags$th))
        )
      )
    )
    expect_error(tip_table(two, rows = "{{ team }}"), class = "hovercraft_error_widget")
  })
})

describe("tip_table() on a reactable table", {
  it("puts a class on the rows, the cells and the header cells, and hands the layer each row's record", {
    got <- tip_table(rt(), rows = "{{ team }}", cells = c(wins = "{{ wins }}"), headers = c(losses = "Games lost"))
    attribs <- got$x$tag$attribs
    expect_match(js(attribs$rowClassName), "hovercraft-row-", fixed = TRUE)
    expect_match(js(attribs$columns[[2]]$className), "hovercraft-cell-1-", fixed = TRUE)
    expect_identical(attribs$columns[[3]]$headerClassName, "hovercraft-head-2")
    cfg <- got$jsHooks$render[[1]]$data
    expect_length(cfg$records, 3L)
    expect_identical(jsonlite::fromJSON(cfg$records[[1]]), list(team = "Owls", wins = 7L))
    expect_named(cfg$cells, "1")
    expect_named(cfg$headers, "2")
    expect_identical(cfg$rows$record, tip_attribute("record"))
  })

  it("keeps the table's own classes, as a string or as a function", {
    got <- tip_table(
      rt(
        rowClass = "mine",
        columns = list(
          wins = reactable::colDef(class = reactable::JS("function() { return 'mine-too'; }")),
          losses = reactable::colDef(headerClass = "hdr")
        )
      ),
      rows = "{{ team }}",
      cells = c(wins = "{{ wins }}"),
      headers = c(losses = "x")
    )
    attribs <- got$x$tag$attribs
    expect_match(js(attribs$rowClassName), "\"mine\"", fixed = TRUE)
    expect_match(js(attribs$columns[[2]]$className), "mine-too", fixed = TRUE)
    expect_identical(attribs$columns[[3]]$headerClassName, "hdr hovercraft-head-2")
  })

  it("refuses a header class that is a function", {
    expect_error(
      tip_table(
        rt(columns = list(wins = reactable::colDef(headerClass = reactable::JS("function() { return 'a'; }")))),
        headers = c(wins = "x")
      ),
      class = "hovercraft_error_widget"
    )
  })

  it("hands the layer empty records when no tip reads the row", {
    got <- tip_table(rt(), rows = "A fixed note")
    expect_identical(unlist(got$jsHooks$render[[1]]$data$records), rep("", 3L))
  })

  it("attaches the engine and the layer", {
    got <- tip_table(rt(), rows = "{{ team }}")
    deps <- vapply(got$dependencies, function(d) d$name, "")
    expect_true(all(c("hovercraft", "hovercraft-table") %in% deps))
  })

  it("lists a cell tip's fields, so a blank cell can have none", {
    got <- tip_table(rt(), cells = c(note = "{{ note }}"))
    expect_identical(unlist(got$jsHooks$render[[1]]$data$cells[["3"]]$needs), "note")
  })
})

describe("tip_table() checks", {
  it("says what it takes when the widget is something else", {
    expect_error(tip_table(scores, rows = "x"), class = "hovercraft_error_widget")
    expect_error(tip_table(htmltools::tags$table(), rows = "x"), class = "hovercraft_error_widget")
  })

  it("needs a tip to put", {
    expect_error(tip_table(dt()), class = "hovercraft_error_tip")
    expect_error(tip_table(dt(), rows = "", cells = c(wins = NA)), class = "hovercraft_error_tip")
  })

  it("lists the table's columns when a placeholder names one it does not have", {
    expect_error(tip_table(dt(), rows = "{{ nope }}"), class = "hovercraft_error_field")
    expect_error(tip_table(rt(), cells = c(wins = "{{ losss }}")), "losses", class = "hovercraft_error_field")
    expect_error(tip_table(dt(), rows = tip_template(tip_text("{{ nope }}"))), class = "hovercraft_error_field")
  })

  it("keeps names that start with a dot for hovercraft", {
    expect_error(tip_table(dt(), rows = "{{ .value }}"), "dot", class = "hovercraft_error_field")
  })

  it("lists the columns when a cell or header names one that does not exist", {
    expect_error(tip_table(dt(), cells = c(winz = "x")), "wins", class = "hovercraft_error_field")
    expect_error(tip_table(dt(), headers = c(winz = "x")), "wins", class = "hovercraft_error_field")
  })

  it("gives a header tip no row to read", {
    expect_error(tip_table(dt(), headers = c(wins = "{{ wins }}")), "no row", class = "hovercraft_error_field")
  })

  it("wants cells and headers named, one entry per column", {
    expect_error(tip_table(dt(), cells = "x"), class = "hovercraft_error_argument")
    expect_error(tip_table(dt(), cells = c("x", "y")), class = "hovercraft_error_argument")
    expect_error(tip_table(dt(), rows = 1), class = "hovercraft_error_argument")
    expect_error(tip_table(dt(), cells = tip_template(tip_text("x"))), class = "hovercraft_error_argument")
  })

  it("checks the placeholders' syntax, the placement and the id", {
    expect_error(tip_table(dt(), rows = "{{ team | shout }}"), class = "hovercraft_error_placeholder")
    expect_error(tip_table(dt(), rows = "x", placement = "above"), class = "hovercraft_error_argument")
    expect_error(tip_table(dt(), rows = "x", id = "Not Allowed"), class = "hovercraft_error_argument")
  })

  it("makes a misspelt argument an error", {
    expect_error(tip_table(dt(), row = "x"), class = "hovercraft_error_argument")
  })

  it("refuses a column that holds a data frame", {
    wide <- scores
    wide$nested <- data.frame(a = 1:3)
    expect_error(tip_table(DT::datatable(wide), rows = "{{ nested }}"), class = "hovercraft_error_record")
  })
})
