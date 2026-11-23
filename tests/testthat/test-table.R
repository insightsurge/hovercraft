# What hovercraft's own layer (inst/table/table.js) relies on in the engine: its attribute names, read from the engine's
# own files, and its public define() call. A new engine copy that drops one fails here, naming it.
describe("the table layer's engine names", {
  it("are all in the engine", {
    attributes <- c(template = "template", record = "record", placement = "placement", kind = "kind", id = "id")
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
    # The layer removes the engine's hidden descriptions (a span of this class, named by aria-describedby) when a
    # widget reuses a row; a new engine copy that renames either must fail here.
    expect_match(script, "`${CSS}-desc`", fixed = TRUE)
    expect_match(script, "aria-describedby", fixed = TRUE)
    expect_identical(tip_names()$css, "hc-tip")
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

  it("writes the placement and the fixed kind as the engine's attributes", {
    got <- tip_table(dt(), rows = "{{ team }}", placement = "right")
    expect_match(js(got$x$options$createdRow), tip_attribute("placement"), fixed = TRUE)
    expect_match(js(got$x$options$createdRow), "\"right\"", fixed = TRUE)
    expect_match(js(got$x$options$createdRow), paste0("\"", tip_attribute("kind"), "\":\"table_row\""), fixed = TRUE)
  })

  it("writes a fixed kind for rows, cells and headers whether or not there is an id", {
    got <- tip_table(dt(), rows = "{{ team }}", cells = c(wins = "{{ wins }}"), headers = c(wins = "Won"))
    kind <- function(target) paste0("\"", tip_attribute("kind"), "\":\"table_", target, "\"")
    expect_match(js(got$x$options$createdRow), kind("row"), fixed = TRUE)
    cells <- Filter(function(def) !is.null(def$createdCell), got$x$options$columnDefs)
    expect_match(js(cells[[1]]$createdCell), kind("cell"), fixed = TRUE)
    expect_match(js(got$x$options$initComplete), kind("header"), fixed = TRUE)
    expect_no_match(js(got$x$options$createdRow), tip_attribute("id"), fixed = TRUE)
  })

  it("with an id adds a hidden column of 1-based data row numbers and tells the layer how to build each tip id", {
    got <- tip_table(
      dt(),
      rows = "{{ team }}",
      cells = c(wins = "{{ wins }}"),
      headers = c(losses = "Games lost"),
      id = "mod1-league_tips"
    )
    expect_identical(
      unname(attr(got$x, "colnames")),
      c("team", "wins", "losses", "note", ".hovercraft", ".hovercraft_row")
    )
    expect_identical(got$x$data[[".hovercraft_row"]], 1:3)
    expect_match(got$x$container, "<th>.hovercraft_row</th></tr>", fixed = TRUE)
    hidden <- Filter(function(def) identical(def$name, ".hovercraft_row"), got$x$options$columnDefs)[[1]]
    expect_identical(hidden$targets, 5L)
    expect_false(hidden$visible)
    expect_match(js(got$x$options$createdRow), "HovercraftTable.mark(row, ", fixed = TRUE)
    expect_match(js(got$x$options$createdRow), ", data[4], data[5]);", fixed = TRUE)
    expect_match(js(got$x$options$createdRow), "\"base\":\"mod1-league_tips\",\"target\":\"row\"}", fixed = TRUE)
    cells <- Filter(function(def) !is.null(def$createdCell), got$x$options$columnDefs)
    expect_match(js(cells[[1]]$createdCell), "\"target\":\"cell\",\"column\":2}", fixed = TRUE)
    expect_match(js(got$x$options$initComplete), "\"target\":\"header\",\"column\":3}", fixed = TRUE)
  })

  it("with an id adds the row column even when no tip reads the row, and not the record column", {
    got <- tip_table(dt(), rows = "A fixed note", id = "league")
    expect_null(got$x$data[[".hovercraft"]])
    expect_identical(got$x$data[[".hovercraft_row"]], 1:3)
    expect_match(js(got$x$options$createdRow), ", null, data[4]);", fixed = TRUE)
  })

  it("counts the column of a cell's tip id from the data, not the row names column", {
    got <- tip_table(DT::datatable(scores), cells = c(wins = "{{ wins }}"), id = "league")
    cells <- Filter(function(def) !is.null(def$createdCell), got$x$options$columnDefs)
    expect_identical(cells[[1]]$targets, 2L)
    expect_match(js(cells[[1]]$createdCell), "\"column\":2}", fixed = TRUE)
  })

  it("writes no id column when there is no id", {
    got <- tip_table(dt(), rows = "{{ team }}")
    expect_null(got$x$data[[".hovercraft_row"]])
  })

  it("defines the templates and attaches the engine and the layer", {
    got <- tip_table(dt(), rows = "{{ team }}", headers = c(wins = "Games won"))
    deps <- vapply(got$dependencies, function(d) d$name, "")
    expect_true(all(c("hovercraft", "hovercraft-table") %in% deps))
    hook <- got$jsHooks$render[[1]]
    expect_length(hook$data$defines, 2L)
    expect_true(all(grepl("^table_[a-f0-9]{12}$", names(hook$data$defines))))
  })

  it("hands the layer the engine's kind attribute and description class, for the keyboard and for stale descriptions", {
    cfg <- tip_table(dt(), rows = "{{ team }}")$jsHooks$render[[1]]$data
    expect_identical(cfg$kind, tip_attribute("kind"))
    expect_identical(cfg$desc, paste0(tip_names()$css, "-desc"))
    expect_match(
      js(tip_table(dt(), rows = "x")$jsHooks$render[[1]]$code),
      "HovercraftTable.keyboard(el, cfg.kind)",
      fixed = TRUE
    )
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

  it("hands the layer the table id and each target's data column for the derived tip ids", {
    got <- tip_table(
      rt(),
      rows = "{{ team }}",
      cells = c(wins = "{{ wins }}"),
      headers = c(losses = "Games lost"),
      id = "mod1-league_tips"
    )
    cfg <- got$jsHooks$render[[1]]$data
    expect_identical(cfg$rows$id, list(attribute = tip_attribute("id"), base = "mod1-league_tips", target = "row"))
    expect_identical(cfg$cells[["1"]]$id$column, 2L)
    expect_identical(cfg$cells[["1"]]$id$target, "cell")
    expect_identical(cfg$headers[["2"]]$id$column, 3L)
    expect_identical(cfg$headers[["2"]]$id$target, "header")
    expect_null(tip_table(rt(), rows = "{{ team }}")$jsHooks$render[[1]]$data$rows$id)
  })

  it("hands the layer the kind attribute, the description class and the keyboard call", {
    got <- tip_table(rt(), rows = "{{ team }}")
    expect_identical(got$jsHooks$render[[1]]$data$kind, tip_attribute("kind"))
    expect_identical(got$jsHooks$render[[1]]$data$desc, paste0(tip_names()$css, "-desc"))
    expect_match(js(got$jsHooks$render[[1]]$code), "HovercraftTable.keyboard(el, cfg.kind)", fixed = TRUE)
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
  })

  it("takes a Shiny id: any string without whitespace, hyphens from ns() included", {
    for (id in c("league", "mod1-league_tips", "a.b:c", "Scores")) {
      expect_no_error(tip_table(dt(), rows = "x", id = id))
    }
    for (bad in list("", "Not Allowed", " a", NA_character_, c("a", "b"), 1, "tab\tbed", character())) {
      expect_error(tip_table(dt(), rows = "x", id = bad), "id", class = "hovercraft_error_argument")
    }
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

describe("tip_spec()", {
  it("holds the words and a placement, with the class tip_table() reads", {
    got <- tip_spec("{{ team }}", placement = "right")
    expect_s3_class(got, "hovercraft_spec")
    expect_identical(got$content, "{{ team }}")
    expect_identical(got$placement, "right")
    expect_null(tip_spec("x")$placement)
  })

  it("takes words, NA, empty words or a template, and nothing else", {
    expect_no_error(tip_spec(NA))
    expect_no_error(tip_spec(NA_character_))
    expect_no_error(tip_spec(""))
    expect_no_error(tip_spec(tip_template(tip_text("x"))))
    for (bad in list(1, c("a", "b"), NULL, list("a"), mean)) {
      expect_error(tip_spec(bad), "content", class = "hovercraft_error_argument")
    }
  })

  it("checks the placement and makes a misspelt argument an error", {
    expect_error(tip_spec("x", placement = "above"), "placement", class = "hovercraft_error_argument")
    expect_error(tip_spec("x", plcement = "right"), class = "hovercraft_error_argument")
  })
})

describe("tip_spec() in tip_table()", {
  placement_key <- function() tip_attribute("placement")

  it("gives a row its own placement, over the table's", {
    got <- tip_table(dt(), rows = tip_spec("{{ team }}", placement = "left"), placement = "right")
    expect_match(js(got$x$options$createdRow), "\"left\"", fixed = TRUE)
    expect_no_match(js(got$x$options$createdRow), "\"right\"", fixed = TRUE)
  })

  it("gives a cell and a header cell their own placement, and the others the table's", {
    got <- tip_table(
      dt(),
      cells = list(wins = tip_spec("{{ wins }}", placement = "left"), losses = "{{ losses }}"),
      headers = list(wins = tip_spec("Won", placement = "bottom"), losses = "Lost"),
      placement = "right"
    )
    defs <- Filter(function(def) !is.null(def$createdCell), got$x$options$columnDefs)
    expect_match(js(defs[[1]]$createdCell), "\"left\"", fixed = TRUE)
    expect_match(js(defs[[2]]$createdCell), "\"right\"", fixed = TRUE)
    header <- jsonlite::fromJSON(
      sub("\\); \\}$", "", sub("^.*this\\.api\\(\\), ", "", js(got$x$options$initComplete))),
      simplifyVector = FALSE
    )
    expect_identical(header[["1"]]$attrs[[placement_key()]], "bottom")
    expect_identical(header[["2"]]$attrs[[placement_key()]], "right")
  })

  it("inherits the table's placement when its own is NULL, and writes none when neither has one", {
    got <- tip_table(dt(), rows = tip_spec("{{ team }}"))
    expect_no_match(js(got$x$options$createdRow), placement_key(), fixed = TRUE)
    got <- tip_table(dt(), rows = tip_spec("{{ team }}"), placement = "top")
    expect_match(js(got$x$options$createdRow), "\"top\"", fixed = TRUE)
  })

  it("lets an explicit auto beat the table's placement", {
    got <- tip_table(dt(), rows = tip_spec("{{ team }}", placement = "auto"), placement = "right")
    expect_match(js(got$x$options$createdRow), "\"auto\"", fixed = TRUE)
  })

  it("makes a cell follow the table, not its row, when the row has a placement of its own", {
    got <- tip_table(
      dt(),
      rows = tip_spec("{{ team }}", placement = "left"),
      cells = c(wins = "{{ wins }}"),
      headers = c(wins = "Won")
    )
    cells <- Filter(function(def) !is.null(def$createdCell), got$x$options$columnDefs)
    expect_match(js(cells[[1]]$createdCell), "\"auto\"", fixed = TRUE)
    got <- tip_table(
      rt(),
      rows = tip_spec("{{ team }}", placement = "left"),
      cells = list(wins = "{{ wins }}", losses = tip_spec("{{ losses }}", placement = "top")),
      placement = "right"
    )
    cfg <- got$jsHooks$render[[1]]$data
    expect_identical(cfg$cells[["1"]]$attrs[[placement_key()]], "right")
    expect_identical(cfg$cells[["2"]]$attrs[[placement_key()]], "top")
    expect_identical(cfg$rows$attrs[[placement_key()]], "left")
  })

  it("leaves a cell's placement alone when the row's is the table's", {
    got <- tip_table(rt(), rows = "{{ team }}", cells = c(wins = "{{ wins }}"))
    expect_null(got$jsHooks$render[[1]]$data$cells[["1"]]$attrs[[placement_key()]])
  })

  it("gives no tip for a spec whose words are NA or empty", {
    got <- tip_table(dt(), rows = tip_spec(NA), cells = list(wins = tip_spec("{{ wins }}"), losses = tip_spec("")))
    expect_null(got$x$options$createdRow)
    expect_length(Filter(function(def) !is.null(def$createdCell), got$x$options$columnDefs), 1L)
    expect_error(
      tip_table(dt(), rows = tip_spec(NA), cells = list(wins = tip_spec(NA))),
      class = "hovercraft_error_tip"
    )
  })

  it("checks a spec's words against the table like any other words", {
    expect_error(tip_table(dt(), rows = tip_spec("{{ nope }}")), "nope", class = "hovercraft_error_field")
    expect_error(
      tip_table(dt(), cells = list(wins = tip_spec("{{ nope }}"))),
      "does not have",
      class = "hovercraft_error_field"
    )
    expect_error(
      tip_table(dt(), headers = list(wins = tip_spec("{{ wins }}"))),
      "no row",
      class = "hovercraft_error_field"
    )
    expect_error(tip_table(dt(), cells = list(winz = tip_spec("x"))), "wins", class = "hovercraft_error_field")
  })

  it("accepts a template and a spec holding one next to plain words", {
    got <- tip_table(
      dt(),
      rows = tip_spec(tip_template(tip_title("{{ team }}"), tip_text("{{ wins }}"))),
      cells = list(wins = tip_template(tip_text("{{ wins }}")), losses = "{{ losses }}")
    )
    expect_length(got$jsHooks$render[[1]]$data$defines, 3L)
  })

  it("points to a list when cells is a bare spec", {
    expect_error(
      tip_table(dt(), cells = tip_spec("x")),
      "tip_spec",
      class = "hovercraft_error_argument"
    )
    expect_error(tip_table(dt(), headers = tip_spec("x")), class = "hovercraft_error_argument")
  })
})

describe("tip_table(marker =)", {
  marker_attr <- function(key = NULL) {
    paste0("data-", tip_names()$prefix, "-marker", if (!is.null(key)) paste0("-", key))
  }
  # What the layer is told to write on each kind of target of a DT table.
  cell_spec <- function(got) {
    cells <- Filter(function(def) !is.null(def$createdCell), got$x$options$columnDefs)
    js(cells[[1]]$createdCell)
  }
  has_marker_dep <- function(widget) "hovercraft-marker" %in% vapply(widget$dependencies, function(d) d$name, "")

  it("writes the corner and its table flag on the cells and header cells that have a tip, and adds the stylesheet", {
    got <- tip_table(dt(), cells = c(wins = "{{ wins }}"), headers = c(losses = "Games lost"), marker = "corner")
    expect_match(cell_spec(got), sprintf("\"%s\":\"corner\"", marker_attr()), fixed = TRUE)
    expect_match(cell_spec(got), sprintf("\"%s\":\"table\"", marker_attr("in")), fixed = TRUE)
    expect_match(js(got$x$options$initComplete), sprintf("\"%s\":\"corner\"", marker_attr()), fixed = TRUE)
    expect_true(has_marker_dep(got))
  })

  it("marks a reactable table's cells and header cells the same way", {
    got <- tip_table(rt(), cells = c(wins = "{{ wins }}"), headers = c(losses = "Games lost"), marker = "corner")
    cfg <- got$jsHooks$render[[1]]$data
    expect_identical(cfg$cells[["1"]]$attrs[[marker_attr()]], "corner")
    expect_identical(cfg$cells[["1"]]$attrs[[marker_attr("in")]], "table")
    expect_identical(cfg$headers[["2"]]$attrs[[marker_attr()]], "corner")
    expect_true(has_marker_dep(got))
  })

  it("writes nothing and adds no stylesheet when there is no marker, or the marker is none", {
    for (marker in list(NULL, "none", tip_marker("none"))) {
      got <- tip_table(dt(), cells = c(wins = "{{ wins }}"), marker = marker)
      expect_no_match(cell_spec(got), "marker", fixed = TRUE)
      expect_false(has_marker_dep(got))
    }
  })

  it("never marks a row, even when the table has a marker", {
    got <- tip_table(dt(), rows = "{{ team }}", cells = c(wins = "{{ wins }}"), marker = "corner")
    expect_no_match(js(got$x$options$createdRow), "marker", fixed = TRUE)
    expect_match(cell_spec(got), "marker", fixed = TRUE)
  })

  it("lets a spec's own marker beat the table's, and none take it off", {
    got <- tip_table(
      dt(),
      cells = list(wins = tip_spec("{{ wins }}", marker = "none"), losses = "{{ losses }}", note = tip_spec("x")),
      marker = "corner"
    )
    defs <- Filter(function(def) !is.null(def$createdCell), got$x$options$columnDefs)
    marked <- vapply(defs, function(def) grepl("marker", js(def$createdCell), fixed = TRUE), NA)
    expect_identical(unname(marked), c(FALSE, TRUE, TRUE))
    only <- tip_table(dt(), cells = list(wins = tip_spec("{{ wins }}", marker = "corner"), losses = "{{ losses }}"))
    marked <- vapply(
      Filter(function(def) !is.null(def$createdCell), only$x$options$columnDefs),
      function(def) grepl("marker", js(def$createdCell), fixed = TRUE),
      NA
    )
    expect_identical(marked, c(TRUE, FALSE))
  })

  it("stops on a marker a table cannot draw yet, and on a marker that has nothing to mark", {
    expect_error(
      tip_table(dt(), cells = c(wins = "x"), marker = "underline"),
      class = "hovercraft_error_marker_table"
    )
    expect_error(tip_table(dt(), cells = c(wins = "x"), marker = "icon"), class = "hovercraft_error_marker_table")
    expect_error(
      tip_table(dt(), cells = list(wins = tip_spec("x", marker = tip_marker("icon", icon = htmltools::tags$b("?"))))),
      class = "hovercraft_error_marker_table"
    )
    expect_error(
      tip_table(dt(), rows = tip_spec("{{ team }}", marker = "corner")),
      class = "hovercraft_error_marker_table"
    )
    expect_error(tip_table(dt(), rows = "{{ team }}", marker = "corner"), class = "hovercraft_error_marker_table")
    expect_error(tip_table(dt(), cells = c(wins = "x"), marker = "dotted"), class = "hovercraft_error_marker_type")
    expect_error(tip_table(dt(), cells = c(wins = "x"), marker = 1), class = "hovercraft_error_marker_type")
  })

  it("lets tip_spec() keep a marker, and refuses a misspelt one", {
    expect_identical(tip_spec("x", marker = "corner")$marker$type, "corner")
    expect_null(tip_spec("x")$marker)
    expect_error(tip_spec("x", marker = "cornr"), class = "hovercraft_error_marker_type")
  })
})

describe("tip_table(style =)", {
  look_of <- function(got, column = 1L) {
    cells <- Filter(function(def) !is.null(def$createdCell), got$x$options$columnDefs)
    js(cells[[column]]$createdCell)
  }
  preset <- function() tip_attribute("preset")
  theme_dep <- function(widget) {
    names <- vapply(widget$dependencies, function(d) d$name, "")
    names[startsWith(names, "hovercraft-theme-")]
  }
  ink <- tip_theme(light = list(bg = "#1b3a6b", ink = "#ffffff"))

  it("writes the preset and class on every row, cell and header cell that has a tip", {
    got <- tip_table(
      dt(),
      rows = "{{ team }}",
      cells = c(wins = "{{ wins }}"),
      headers = c(losses = "Games lost"),
      style = tip_style("sober", class = "league")
    )
    expect_match(js(got$x$options$createdRow), sprintf("\"%s\":\"sober\"", preset()), fixed = TRUE)
    expect_match(look_of(got), sprintf("\"%s\":\"sober\"", preset()), fixed = TRUE)
    expect_match(look_of(got), sprintf("\"%s\":\"league\"", tip_attribute("class")), fixed = TRUE)
    expect_match(js(got$x$options$initComplete), sprintf("\"%s\":\"sober\"", preset()), fixed = TRUE)
  })

  it("takes a preset name and a theme, and adds the theme's stylesheet once", {
    got <- tip_table(dt(), cells = c(wins = "{{ wins }}", losses = "{{ losses }}"), style = ink)
    expect_match(look_of(got), tip_theme_attribute(), fixed = TRUE)
    expect_length(theme_dep(got), 1L)
    named <- tip_table(dt(), cells = c(wins = "{{ wins }}"), style = "dense")
    expect_match(look_of(named), "\"dense\"", fixed = TRUE)
    expect_length(theme_dep(named), 0L)
  })

  it("marks a reactable table the same way", {
    got <- tip_table(rt(), cells = c(wins = "{{ wins }}"), headers = c(losses = "Games lost"), style = "sober")
    cfg <- got$jsHooks$render[[1]]$data
    expect_identical(cfg$cells[["1"]]$attrs[[preset()]], "sober")
    expect_identical(cfg$headers[["2"]]$attrs[[preset()]], "sober")
  })

  it("writes nothing and adds no stylesheet without a style", {
    got <- tip_table(dt(), cells = c(wins = "{{ wins }}"))
    expect_no_match(look_of(got), "preset", fixed = TRUE)
    expect_length(theme_dep(got), 0L)
  })

  it("lets a spec's style beat the table's part by part, keeping what the spec leaves out", {
    got <- tip_table(
      dt(),
      cells = list(wins = tip_spec("{{ wins }}", style = "dense"), losses = tip_spec("{{ losses }}")),
      style = tip_style("sober", class = "league")
    )
    own <- look_of(got, 1L)
    expect_match(own, sprintf("\"%s\":\"dense\"", preset()), fixed = TRUE)
    expect_match(own, sprintf("\"%s\":\"league\"", tip_attribute("class")), fixed = TRUE)
    expect_match(look_of(got, 2L), sprintf("\"%s\":\"sober\"", preset()), fixed = TRUE)
  })

  it("gives a spec a style when the table has none, and a row its own", {
    got <- tip_table(
      dt(),
      rows = tip_spec("{{ team }}", style = ink),
      cells = list(wins = tip_spec("x", style = "dense"))
    )
    expect_match(js(got$x$options$createdRow), tip_theme_attribute(), fixed = TRUE)
    expect_match(look_of(got), "\"dense\"", fixed = TRUE)
    expect_length(theme_dep(got), 1L)
  })

  it("writes a spec's size and motion settings under a marker, and adds one stylesheet for them", {
    got <- tip_table(
      dt(),
      cells = list(wins = tip_spec("{{ wins }}", style = tip_style(gap = 2)), losses = "{{ losses }}"),
      style = tip_style("sober", motion = "slide")
    )
    expect_match(look_of(got, 1L), tip_theme_attribute(), fixed = TRUE)
    expect_match(look_of(got, 1L), sprintf("\"%s\":\"sober\"", preset()), fixed = TRUE)
    expect_length(theme_dep(got), 2L)
    expect_match(
      paste(vapply(got$dependencies, function(d) paste(d$head, collapse = ""), ""), collapse = ""),
      "--hovercraft-tip-motion: slide;",
      fixed = TRUE
    )
  })

  it("stops on what style does not take, as tip_on() does", {
    expect_error(tip_table(dt(), rows = "x", style = "nope"), class = "hovercraft_error_style")
    expect_error(tip_table(dt(), rows = "x", style = list(bg = "red")), class = "hovercraft_error_style")
    expect_error(tip_spec("x", style = "color: red"), class = "hovercraft_error_style")
  })

  it("lets tip_spec() keep a style", {
    expect_identical(tip_spec("x", style = "sober")$style$preset, "sober")
    expect_null(tip_spec("x")$style)
  })
})
