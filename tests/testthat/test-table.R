# What hovercraft's own layer (inst/table/table.js) relies on in the engine: its attribute names, read from the engine's
# own files, and its public define() call. A new engine copy that drops one fails here, naming it.
describe("the table layer's engine names", {
  it("are all in the engine", {
    attributes <- c(
      template = "template",
      record = "record",
      placement = "placement",
      kind = "kind",
      id = "id",
      anchor = "anchor",
      pin = "pin"
    )
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
  it("adds no column to the data, so DT::replaceData() with the user's own data frame still fits", {
    got <- tip_table(dt(), rows = "**{{ team }}**: {{ wins | number:0 }} wins")
    expect_identical(names(got$x$data), names(scores))
    expect_identical(unname(attr(got$x, "colnames")), names(scores))
    expect_identical(got$x$container, dt()$x$container)
    expect_false(any(vapply(got$x$options$columnDefs, function(def) isFALSE(def$visible), NA)))
  })

  it("tells the layer at which position of a row's data each field the tips name sits", {
    got <- tip_table(dt(), rows = "**{{ team }}**: {{ wins | number:0 }} wins")
    expect_match(js(got$x$options$createdRow), "dtRecord(data, {\"team\":0,\"wins\":1}, true)", fixed = TRUE)
  })

  it("marks rows through the engine's own attribute names", {
    got <- tip_table(dt(), rows = "{{ team }}")
    expect_match(js(got$x$options$createdRow), tip_attribute("template"), fixed = TRUE)
    expect_match(js(got$x$options$createdRow), "HovercraftTable.mark(row, ", fixed = TRUE)
    expect_match(js(got$x$options$createdRow), tip_attribute("record"), fixed = TRUE)
  })

  it("marks the cells of the named columns by their position, and the header cells", {
    got <- tip_table(dt(), cells = c(wins = "{{ wins }} wins"), headers = c(losses = "Games lost"))
    defs <- Filter(function(def) !is.null(def$createdCell), got$x$options$columnDefs)
    expect_length(defs, 1L)
    expect_identical(defs[[1]]$targets, 1L)
    expect_match(js(got$x$options$initComplete), "\"2\":{\"attrs\"", fixed = TRUE)
  })

  it("tells the layer which columns DT escapes, so it can give a tip the value as it is in the data", {
    created <- function(widget) js(tip_table(widget, rows = "{{ team }} {{ note }}")$x$options$createdRow)
    expect_match(created(dt()), "{\"team\":0,\"note\":3}, true)", fixed = TRUE)
    expect_match(created(dt(escape = FALSE)), "{\"team\":0,\"note\":3}, false)", fixed = TRUE)
    # By column name or by number: the 0-based positions of the escaped columns (the row names column counts).
    expect_match(created(dt(escape = "team")), "{\"team\":0,\"note\":3}, [0])", fixed = TRUE)
    expect_match(created(dt(escape = c(2, 4))), "{\"team\":0,\"note\":3}, [1,3])", fixed = TRUE)
    expect_match(created(DT::datatable(scores, escape = "team")), "[1])", fixed = TRUE)
  })

  it("asks for no record when no tip reads the row", {
    got <- tip_table(dt(), rows = "A fixed note")
    expect_match(js(got$x$options$createdRow), "dtRecord(data, null, true)", fixed = TRUE)
  })

  it("counts the row names column in the positions when the table has one", {
    got <- tip_table(DT::datatable(scores), cells = c(wins = "{{ wins }}"))
    defs <- Filter(function(def) !is.null(def$createdCell), got$x$options$columnDefs)
    expect_identical(defs[[1]]$targets, 2L)
    expect_identical(names(got$x$data)[[1]], " ")
    expect_match(js(defs[[1]]$createdCell), "dtRecord(data, {\"wins\":2}, true)", fixed = TRUE)
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

  it("with an id asks the layer for the data row, and tells it how to build each tip id", {
    got <- tip_table(
      dt(),
      rows = "{{ team }}",
      cells = c(wins = "{{ wins }}"),
      headers = c(losses = "Games lost"),
      id = "mod1-league_tips"
    )
    expect_identical(names(got$x$data), names(scores))
    expect_match(js(got$x$options$createdRow), "HovercraftTable.mark(row, ", fixed = TRUE)
    expect_match(js(got$x$options$createdRow), ", HovercraftTable.dtRow(this, index));", fixed = TRUE)
    expect_match(js(got$x$options$createdRow), "\"base\":\"mod1-league_tips\",\"target\":\"row\"}", fixed = TRUE)
    cells <- Filter(function(def) !is.null(def$createdCell), got$x$options$columnDefs)
    expect_match(js(cells[[1]]$createdCell), "\"target\":\"cell\",\"column\":2}", fixed = TRUE)
    expect_match(js(got$x$options$initComplete), "\"target\":\"header\",\"column\":3}", fixed = TRUE)
  })

  it("with an id asks for the data row even when no tip reads the row, and for no record", {
    got <- tip_table(dt(), rows = "A fixed note", id = "league")
    expect_identical(names(got$x$data), names(scores))
    expect_match(
      js(got$x$options$createdRow),
      "dtRecord(data, null, true), HovercraftTable.dtRow(this, index)",
      fixed = TRUE
    )
  })

  it("counts the column of a cell's tip id from the data, not the row names column", {
    got <- tip_table(DT::datatable(scores), cells = c(wins = "{{ wins }}"), id = "league")
    cells <- Filter(function(def) !is.null(def$createdCell), got$x$options$columnDefs)
    expect_identical(cells[[1]]$targets, 2L)
    expect_match(js(cells[[1]]$createdCell), "\"column\":2}", fixed = TRUE)
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
    expect_match(js(got$x$options$createdRow), "dtRecord(data, {\"team\":0,\"wins\":1}, true)", fixed = TRUE)
  })

  it("leaves out a cell or a row whose words are NA or empty", {
    got <- tip_table(dt(), rows = NA_character_, cells = c(wins = "{{ wins }}", losses = NA, note = ""))
    expect_null(got$x$options$createdRow)
    expect_length(Filter(function(def) !is.null(def$createdCell), got$x$options$columnDefs), 1L)
  })

  it("stops on a column that holds a list, which is one value per row only in name", {
    nested <- scores
    nested$note <- list(1, 2, "x")
    expect_error(
      tip_table(DT::datatable(nested, rownames = FALSE), rows = "{{ note }}"),
      class = "hovercraft_error_record"
    )
    # A list column that no tip reads is not the tip's concern.
    expect_s3_class(tip_table(DT::datatable(nested, rownames = FALSE), rows = "{{ team }}"), "datatables")
  })

  it("refuses options it would overwrite", {
    expect_error(
      tip_table(dt(options = list(createdRow = DT::JS("function() {}"))), rows = "{{ team }}"),
      class = "hovercraft_error_widget"
    )
  })

  it("takes a header of more than one row, since it adds no header cell", {
    two <- dt(
      container = htmltools::tags$table(
        htmltools::tags$thead(
          htmltools::tags$tr(htmltools::tags$th(colspan = 2, "A"), htmltools::tags$th(colspan = 2, "B")),
          htmltools::tags$tr(lapply(names(scores), htmltools::tags$th))
        )
      )
    )
    got <- tip_table(two, rows = "{{ team }}", headers = c(wins = "Games won"))
    expect_identical(got$x$container, two$x$container)
  })
})

describe("tip_table() on a reactable table", {
  it("puts a class on the rows, the cells and the header cells, and has the row's class carry its record", {
    got <- tip_table(rt(), rows = "{{ team }}", cells = c(wins = "{{ wins }}"), headers = c(losses = "Games lost"))
    attribs <- got$x$tag$attribs
    expect_match(js(attribs$rowClassName), "HovercraftTable.rowClass(rowInfo, [\"team\",\"wins\"])", fixed = TRUE)
    expect_match(js(attribs$columns[[2]]$className), "hovercraft-cell-1-", fixed = TRUE)
    expect_identical(attribs$columns[[3]]$headerClassName, "hovercraft-head-2")
    cfg <- got$jsHooks$render[[1]]$data
    expect_null(cfg$records)
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

  it("asks for no record in the row's class when no tip reads the row", {
    got <- tip_table(rt(), rows = "A fixed note")
    expect_match(js(got$x$tag$attribs$rowClassName), "HovercraftTable.rowClass(rowInfo, [])", fixed = TRUE)
    expect_null(got$jsHooks$render[[1]]$data$records)
  })

  it("puts the class on the rows when only the cells read the row, since the cells take its record", {
    got <- tip_table(rt(), cells = c(wins = "{{ wins }} of {{ team }}"))
    expect_match(js(got$x$tag$attribs$rowClassName), "rowClass(rowInfo, [\"wins\",\"team\"])", fixed = TRUE)
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

  it("refuses a colon in an id, which Shiny would read as the type of the input", {
    expect_error(tip_table(dt(), rows = "x", id = "a:b"), "colon", class = "hovercraft_error_argument")
    expect_error(tip_on(htmltools::tags$span("x"), "y", id = "a:b"), "colon", class = "hovercraft_error_argument")
  })

  it("takes a Shiny id: any string without whitespace, hyphens from ns() included", {
    for (id in c("league", "mod1-league_tips", "a.b_c", "Scores")) {
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

describe("tip_table() anchors the tips of cells and header cells on their content", {
  anchor <- function() tip_attribute("anchor")
  it("uses a value the engine knows, under the engine's own attribute", {
    expect_true("content" %in% unlist(tip_rules()$anchor$values))
    expect_identical(tip_attribute("anchor"), paste0(tip_names()$attribute, "-anchor"))
  })

  it("writes the anchor on a DT table's cells and header cells, never on its rows", {
    got <- tip_table(dt(), rows = "{{ team }}", cells = c(wins = "{{ wins }}"), headers = c(losses = "Games lost"))
    expect_match(js(got$x$options$columnDefs[[length(got$x$options$columnDefs)]]$createdCell), anchor(), fixed = TRUE)
    expect_match(js(got$x$options$initComplete), sprintf("\"%s\":\"content\"", anchor()), fixed = TRUE)
    expect_no_match(js(got$x$options$createdRow), anchor(), fixed = TRUE)
  })

  it("writes it on a reactable table's cells and header cells, never on its rows", {
    got <- tip_table(rt(), rows = "{{ team }}", cells = c(wins = "{{ wins }}"), headers = c(losses = "Games lost"))
    cfg <- got$jsHooks$render[[1]]$data
    expect_identical(cfg$cells[["1"]]$attrs[[anchor()]], "content")
    expect_identical(cfg$headers[["2"]]$attrs[[anchor()]], "content")
    expect_null(cfg$rows$attrs[[anchor()]])
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
  ink <- tip_theme(light = list(bg = "#1b3a6b", text = "#ffffff"))

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
      paste(
        vapply(got$dependencies, function(d) dep_css(d), ""),
        collapse = ""
      ),
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

describe("tip_spec(more =) in tip_table()", {
  held_template <- function() tip_more_attribute("held-template")

  it("stores more on tip_spec(), as words, a template or a tip_more(), and validates it", {
    s <- tip_spec("{{ team }}", more = tip_more("{{ note }}", on = "hover"))
    expect_s3_class(s$more, "hovercraft_more")
    expect_identical(s$more$on, "hover")
    expect_identical(tip_spec("{{ team }}", more = "{{ note }}")$more, "{{ note }}")
    expect_error(tip_spec("x", more = 1), "more", class = "hovercraft_error_argument")
    expect_error(tip_spec("x", more = "y", more_on = "hover"), class = "hovercraft_error_argument")
  })

  it("holds the long form behind a button on DT and reactable cells, as the engine's attributes are left off", {
    got_dt <- tip_table(dt(), cells = list(team = tip_spec("{{ team }}", more = "{{ note }} ({{ wins }})")))
    defs <- Filter(function(def) !is.null(def$createdCell), got_dt$x$options$columnDefs)
    expect_match(js(defs[[1]]$createdCell), "dtRecord(data, {\"team\":0,\"note\":3,\"wins\":1}, true)", fixed = TRUE)
    expect_match(js(defs[[1]]$createdCell), held_template(), fixed = TRUE)
    # The engine's attributes are not written; they are what the button writes, and the layer takes them off a reused row.
    expect_no_match(js(defs[[1]]$createdCell), sprintf("\"%s\":", tip_attribute("more_on")), fixed = TRUE)
    expect_match(js(defs[[1]]$createdCell), sprintf("\"reset\":[\"%s\"", tip_attribute("more")), fixed = TRUE)
    expect_match(js(defs[[1]]$createdCell), "\"aria-keyshortcuts\":\"M\"", fixed = TRUE)
    expect_length(got_dt$jsHooks$render[[1]]$data$defines, 2L)
    expect_true("hovercraft-more" %in% vapply(got_dt$dependencies, function(dep) dep$name, ""))

    got_rt <- tip_table(rt(), cells = list(team = tip_spec("{{ team }}", more = tip_template(tip_text("{{ note }}")))))
    cfg <- got_rt$jsHooks$render[[1]]$data
    expect_match(cfg$cells[["0"]]$attrs[[held_template()]], "^table_[a-f0-9]{12}$")
    expect_null(cfg$cells[["0"]]$attrs[[tip_attribute("more_on")]])
    expect_null(cfg$cells[["0"]]$attrs[[tip_attribute("more_template")]])
    expect_identical(cfg$cells[["0"]]$attrs[["aria-keyshortcuts"]], "M")
    expect_true(tip_more_attribute("state") %in% unlist(cfg$cells[["0"]]$reset))
    expect_length(
      tip_table(
        rt(),
        cells = list(team = tip_spec("{{ team }}", more = tip_more("{{ note }}", on = "hover")))
      )$jsHooks$render[[1]]$data$cells[["0"]]$reset,
      0L
    )
    # A header cell has the same default.
    heads <- tip_table(rt(), headers = list(team = tip_spec("The team", more = "More about the team")))
    expect_match(
      heads$jsHooks$render[[1]]$data$headers[["0"]]$attrs[[tip_more_attribute("held-template")]],
      "^table_[a-f0-9]{12}$"
    )
  })

  it("writes the engine's attributes for on = 'hover', with no button and no key", {
    got <- tip_table(
      rt(),
      cells = list(team = tip_spec("{{ team }}", more = tip_more(tip_template(tip_text("{{ note }}")), on = "hover")))
    )
    cfg <- got$jsHooks$render[[1]]$data
    expect_match(cfg$cells[["0"]]$attrs[[tip_attribute("more_template")]], "^table_[a-f0-9]{12}$")
    expect_identical(cfg$cells[["0"]]$attrs[[tip_attribute("more_on")]], "hover")
    expect_null(cfg$cells[["0"]]$attrs[["aria-keyshortcuts"]])
    expect_false("hovercraft-more" %in% vapply(got$dependencies, function(dep) dep$name, ""))
  })

  it("holds the words of the buttons as data, and announces both keys on a row that pins", {
    got <- tip_table(
      rt(),
      rows = tip_spec("{{ team }}", more = tip_more("{{ note }}", label = "Details"), pin = TRUE),
      id = "t"
    )
    attrs <- got$jsHooks$render[[1]]$data$rows$attrs
    expect_identical(attrs[[tip_more_attribute("label")]], "{\"more\":\"Details\"}")
    expect_identical(attrs[["aria-keyshortcuts"]], "P M")
  })

  it("stops with a classed pin error when tip_more(on = 'pin') is in a target that cannot pin", {
    expect_error(
      tip_table(dt(), cells = list(team = tip_spec("{{ team }}", more = tip_more("{{ note }}", on = "pin")))),
      class = "hovercraft_error_pin"
    )
  })
})

describe("tip_output() in a tip_table() template", {
  it("hands the output ids to the table's script, so they are parked", {
    skip_if_not_installed("shiny")
    tpl <- tip_template(tip_title("{{ team }}"), tip_output("row_plot"))
    got <- tip_table(dt(), rows = tpl)
    expect_match(js(got$jsHooks$render[[1]]$code), "cfg.park", fixed = TRUE)
    expect_identical(got$jsHooks$render[[1]]$data$park, list("row_plot"))
    got <- tip_table(rt(), cells = c(wins = "{{ wins }}"), rows = tpl)
    expect_identical(got$jsHooks$render[[1]]$data$park, list("row_plot"))
    expect_identical(tip_table(dt(), rows = "{{ team }}")$jsHooks$render[[1]]$data$park, list())
  })

  it("refuses an output given as a tag, which a table cannot place in the page", {
    skip_if_not_installed("shiny")
    tpl <- tip_template(tip_title("{{ team }}"), tip_output(shiny::plotOutput("row_plot")))
    err <- expect_error(tip_table(dt(), rows = tpl), class = "hovercraft_error_output")
    expect_match(conditionMessage(err), "tip_output(\"detail\")", fixed = TRUE)
    expect_error(tip_table(rt(), cells = list(wins = tip_spec("x", more = tpl))), class = "hovercraft_error_output")
  })
})

describe("tip_spec(pin = TRUE, events = ) for the rows of tip_table()", {
  rows <- "**{{ team }}**: {{ wins | number:0 }} wins"
  pinned <- tip_spec(rows, pin = TRUE)
  deps <- function(widget) vapply(widget$dependencies, function(dep) dep$name, "")

  it("gives the rows the engine's manual pin and the pin key's announcement, and no cell or header", {
    got <- tip_table(
      rt(),
      rows = pinned,
      cells = c(wins = "{{ wins }}"),
      headers = c(wins = "Wins"),
      id = "t"
    )
    cfg <- got$jsHooks$render[[1]]$data
    expect_identical(cfg$rows$attrs[[tip_attribute("pin")]], "manual")
    expect_identical(cfg$rows$attrs[["aria-keyshortcuts"]], "P")
    expect_null(cfg$cells[["1"]]$attrs[[tip_attribute("pin")]])
    expect_null(cfg$cells[["1"]]$attrs[["aria-keyshortcuts"]])
    expect_null(cfg$headers[["1"]]$attrs[[tip_attribute("pin")]])
    got <- tip_table(dt(), rows = pinned, id = "t")
    expect_match(js(got$x$options$createdRow), sprintf("\"%s\":\"manual\"", tip_attribute("pin")), fixed = TRUE)
    expect_match(js(got$x$options$createdRow), "\"aria-keyshortcuts\":\"P\"", fixed = TRUE)
  })

  it("takes pin = NULL and pin = FALSE as no pin", {
    for (none in list(tip_spec(rows), tip_spec(rows, pin = NULL), tip_spec(rows, pin = FALSE))) {
      got <- tip_table(rt(), rows = none, id = "t")$jsHooks$render[[1]]$data
      expect_null(got$rows$attrs[[tip_attribute("pin")]])
      expect_null(got$rows$attrs[["aria-keyshortcuts"]])
    }
    expect_null(tip_spec(rows)$pin)
    expect_null(tip_spec(rows)$events)
  })

  it("adds the pin layer only with a pin, and tells the table's script how the engine marks a pin", {
    expect_true("hovercraft-pin" %in% deps(tip_table(dt(), rows = pinned, id = "t")))
    expect_true("hovercraft-pin" %in% deps(tip_table(rt(), rows = pinned, id = "t")))
    expect_false("hovercraft-pin" %in% deps(tip_table(dt(), rows = rows, id = "t")))
    expect_false("hovercraft-pin" %in% deps(tip_table(rt(), rows = tip_spec(rows, pin = FALSE), id = "t")))
    plain <- tip_table(rt(), rows = rows, id = "t")$jsHooks$render[[1]]$data
    expect_null(plain$rows$attrs[[tip_attribute("pin")]])
    open <- tip_table(rt(), rows = pinned, id = "t")$jsHooks$render[[1]]$data$open
    expect_identical(open$attribute, tip_contract()$attributes$open)
    expect_identical(open$pinned, "pin")
    open_dt <- tip_table(dt(), rows = pinned, id = "t")$jsHooks$render[[1]]$data$open
    expect_identical(open_dt, open)
  })

  it("writes the events a row sends, and only on the rows", {
    sends <- tip_spec(rows, pin = TRUE, events = c("pin", "copy"))
    got <- tip_table(rt(), rows = sends, cells = c(wins = "{{ wins }}"), id = "t")
    cfg <- got$jsHooks$render[[1]]$data
    expect_identical(cfg$rows$attrs[[tip_shiny_attribute()]], "pin copy")
    expect_null(cfg$cells[["1"]]$attrs[[tip_shiny_attribute()]])
    expect_identical(cfg$rows$attrs[[tip_attribute("pin")]], "manual")
    got <- tip_table(dt(), rows = tip_spec(rows, events = "hint"), id = "t")
    expect_match(js(got$x$options$createdRow), sprintf("\"%s\":\"hint\"", tip_shiny_attribute()), fixed = TRUE)
  })

  it("tells the layer where a row that sends events is: the table, the target and the data column's name", {
    got <- tip_table(rt(), rows = tip_spec(rows, pin = TRUE, events = "pin"), id = "t")$jsHooks$render[[1]]$data
    expect_identical(got$rows$id$address, tip_table_address_attribute())
    expect_identical(got$id, "t")
    expect_identical(tip_runtime_config()$attributes$address, tip_table_address_attribute())
    # A row with no events, and a table with no id, have no address to write.
    expect_null(tip_table(rt(), rows = rows, id = "t")$jsHooks$render[[1]]$data$rows$id$address)
    expect_null(tip_table(rt(), rows = rows)$jsHooks$render[[1]]$data$id)
  })

  it("lets a spec for the rows open its long form on the pin, and only with a pin", {
    more <- tip_spec("{{ team }}", more = tip_more("{{ note }}", on = "pin"), pin = TRUE)
    got <- tip_table(rt(), rows = more, id = "t")
    expect_identical(got$jsHooks$render[[1]]$data$rows$attrs[[tip_attribute("more_on")]], "pin")
    spec <- tip_spec("{{ team }}", more = tip_more("{{ note }}", on = "pin"))
    expect_error(tip_table(rt(), rows = spec, id = "t"), class = "hovercraft_error_pin")
    expect_error(tip_table(rt(), cells = list(team = more), rows = "x", id = "t"), class = "hovercraft_error_pin")
    expect_error(tip_table(rt(), cells = list(team = spec), rows = "x", id = "t"), class = "hovercraft_error_pin")
  })

  it("lets the More button of a row pin it, and only on a row with a pin", {
    more <- tip_spec("{{ team }}", more = tip_more("{{ note }}", pin = TRUE), pin = TRUE)
    got <- tip_table(rt(), rows = more, id = "t")
    attrs <- got$jsHooks$render[[1]]$data$rows$attrs
    expect_identical(attrs[[tip_more_attribute("pin")]], "true")
    expect_identical(attrs[["aria-keyshortcuts"]], "P M")
    plain <- tip_table(rt(), rows = tip_spec("{{ team }}", more = "{{ note }}", pin = TRUE), id = "t")
    expect_null(plain$jsHooks$render[[1]]$data$rows$attrs[[tip_more_attribute("pin")]])
    spec <- tip_spec("{{ team }}", more = tip_more("{{ note }}", pin = TRUE))
    expect_error(tip_table(rt(), rows = spec, id = "t"), "pin = TRUE", class = "hovercraft_error_pin")
    expect_error(tip_table(rt(), cells = list(team = more), rows = "x", id = "t"), class = "hovercraft_error_pin")
  })

  it("writes the look and the side of the control of a row, a cell and a header cell", {
    link <- tip_more("{{ note }}", as = "link", where = "end")
    got <- tip_table(
      rt(),
      rows = tip_spec("{{ team }}", more = tip_more("{{ note }}", as = "link"), pin = TRUE),
      cells = list(team = tip_spec("{{ team }}", more = link)),
      headers = list(team = tip_spec("The team", more = tip_more("About the team", as = "link", where = "end"))),
      id = "t"
    )
    cfg <- got$jsHooks$render[[1]]$data
    expect_identical(cfg$rows$attrs[[tip_more_attribute("as")]], "link")
    expect_null(cfg$rows$attrs[[tip_more_attribute("where")]])
    for (attrs in list(cfg$cells[["0"]]$attrs, cfg$headers[["0"]]$attrs)) {
      expect_identical(attrs[[tip_more_attribute("as")]], "link")
      expect_identical(attrs[[tip_more_attribute("where")]], "end")
    }
  })

  it("refuses a click that opens the long form, because a click on a table selects a row", {
    click <- tip_spec("{{ team }}", more = tip_more("{{ note }}", on = "click"), pin = TRUE)
    expect_error(tip_table(rt(), rows = click, id = "t"), "selects a row", class = "hovercraft_error_more")
    cell <- tip_spec("{{ team }}", more = tip_more("{{ note }}", on = "click"))
    expect_error(tip_table(rt(), cells = list(team = cell), rows = "x", id = "t"), class = "hovercraft_error_more")
  })

  it("refuses the click pin, a pin or events with no id, and a wrong value", {
    expect_error(tip_table(dt(), rows = tip_spec(rows, pin = "click"), id = "t"), class = "hovercraft_error_pin")
    expect_error(tip_table(dt(), rows = pinned), class = "hovercraft_error_pin")
    expect_error(tip_table(dt(), rows = tip_spec(rows, events = "hint")), class = "hovercraft_error_events")
    expect_error(tip_spec(rows, pin = "yes"), class = "hovercraft_error_argument")
    expect_error(tip_spec(rows, events = "click"), class = "hovercraft_error_argument")
    expect_error(tip_spec(rows, pinn = TRUE), class = "hovercraft_error_argument")
  })

  it("stops a pin or events given to cells or headers: only rows pin and send events for now", {
    for (what in c("cells", "headers")) {
      with_pin <- list(widget = dt(), rows = "x", id = "t")
      with_pin[[what]] <- list(wins = tip_spec("x", pin = TRUE))
      expect_error(do.call(tip_table, with_pin), class = "hovercraft_error_pin")
      with_events <- list(widget = dt(), rows = "x", id = "t")
      with_events[[what]] <- list(wins = tip_spec("x", events = "hint"))
      expect_error(do.call(tip_table, with_events), class = "hovercraft_error_events")
    }
    expect_error(tip_table(dt(), cells = list(wins = tip_spec("x", pin = TRUE))), "only rows pin for now")
    expect_error(
      tip_table(dt(), headers = list(wins = tip_spec("x", events = "hint"))),
      "only rows send events for now"
    )
  })

  it("stops a spec for the rows that holds no words but asks for a pin or events", {
    expect_error(tip_table(dt(), rows = tip_spec(NA, pin = TRUE), id = "t"), class = "hovercraft_error_pin")
    expect_error(tip_table(dt(), rows = tip_spec("", events = "hint"), id = "t"), class = "hovercraft_error_events")
  })

  it("stops events of the pin or the copy without a pin in a spec, as tip_on() does", {
    expect_error(tip_spec(rows, events = "pin"), "no pin", class = "hovercraft_error_events")
    expect_error(tip_spec(rows, events = c("hint", "copy")), "copy", class = "hovercraft_error_events")
    expect_error(tip_spec(rows, pin = FALSE, events = "pin"), class = "hovercraft_error_events")
    expect_s3_class(tip_spec(rows, events = "hint"), "hovercraft_spec")
    expect_s3_class(tip_spec(rows, pin = TRUE, events = c("hint", "pin", "copy")), "hovercraft_spec")
  })

  it("refuses a list of CSS variables as the style of the table or of a spec, as tip_on() does", {
    css <- tip_style(theme = list("--x" = "red"))
    expect_error(tip_table(dt(), rows = rows, style = css), "CSS variables", class = "hovercraft_error_style")
    expect_error(tip_table(dt(), rows = tip_spec(rows, style = css)), "CSS variables", class = "hovercraft_error_style")
    expect_error(
      tip_table(dt(), cells = list(wins = tip_spec("x", style = css))),
      "CSS variables",
      class = "hovercraft_error_style"
    )
  })

  it("no longer takes pin or events on the table, and says where they went", {
    expect_error(tip_table(dt(), rows = rows, id = "t", pin = TRUE), "tip_spec", class = "hovercraft_error_argument")
    expect_error(
      tip_table(dt(), rows = rows, id = "t", events = "pin"),
      "tip_spec",
      class = "hovercraft_error_argument"
    )
  })
})

describe("tip_table(id =) in a module", {
  it("adds the module's prefix to a plain id, from the session the table is drawn in", {
    expect_identical(tip_table_id("scores", list(ns = shiny::NS("mod1"))), "mod1-scores")
    expect_identical(tip_table_id("scores", list(ns = shiny::NS(c("outer", "inner")))), "outer-inner-scores")
  })

  it("leaves an id that has the prefix alone, and an id outside a session as it is", {
    expect_identical(tip_table_id("mod1-scores", list(ns = shiny::NS("mod1"))), "mod1-scores")
    expect_identical(tip_table_id("scores", NULL), "scores")
    expect_identical(tip_table_id(NULL, list(ns = shiny::NS("mod1"))), NULL)
    expect_identical(tip_table_id("scores", list(ns = function(id) id)), "scores")
    expect_identical(tip_table_id("scores", list()), "scores")
  })

  it("is the module's session inside a render function, so the id the table writes has the prefix", {
    module <- function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        output$tbl <- shiny::renderText(tip_table_id("scores"))
      })
    }
    shiny::testServer(function(input, output, session) module("mod1"), {
      expect_identical(output[["mod1-tbl"]], "mod1-scores")
    })
  })

  it("gives the table's tips ids with the prefix when it is built in a module session", {
    shiny::testServer(
      function(input, output, session) {
        shiny::moduleServer("mod1", function(input, output, session) {
          output$tbl <- shiny::renderText({
            got <- tip_table(dt(), rows = tip_spec("{{ team }}", pin = TRUE, events = "pin"), id = "league")
            js(got$x$options$createdRow)
          })
        })
      },
      {
        expect_match(output[["mod1-tbl"]], "\"base\":\"mod1-league\"", fixed = TRUE)
      }
    )
  })
})

describe("the _pinned input of a table", {
  it("has a handler that gives the list of addresses back as it was sent, however long", {
    sent <- list(
      list(target = "row", row = 2L, column = NULL, id = "t/row/2"),
      list(target = "row", row = 1L, column = NULL, id = "t/row/1")
    )
    expect_identical(tip_pinned_value(sent, NULL, "t_pinned"), sent)
    expect_identical(tip_pinned_value(list(), NULL, "t_pinned"), list())
  })

  it("is registered with Shiny when the package loads, under the type the page names", {
    expect_identical(tip_pinned_input_type(), "hovercraft.pinned")
    expect_identical(tip_runtime_config()$pinnedType, tip_pinned_input_type())
    # A type that is already registered refuses a second handler unless it is forced.
    expect_error(shiny::registerInputHandler(tip_pinned_input_type(), function(...) NULL), "already")
    expect_silent(tip_register_pinned_input())
  })

  it("is registered by the package's own load, and again when Shiny loads later", {
    hook <- packageEvent("shiny", "onLoad")
    before <- getHook(hook)
    withr::defer(setHook(hook, before, "replace"))
    expect_silent(tip_register_with_shiny())
    ours <- function() Filter(function(f) isTRUE(attr(f, "hovercraft")), getHook(hook))
    expect_length(ours(), 1L)
    # Loading again leaves one hook of ours, not two.
    expect_silent(tip_register_with_shiny())
    expect_length(ours(), 1L)
    expect_silent(ours()[[1]]())
    expect_error(shiny::registerInputHandler(tip_pinned_input_type(), function(...) NULL), "already")
  })
})

describe("tip_table() on the names it is given", {
  it("stops a column named twice in cells or headers", {
    expect_error(
      tip_table(dt(), cells = list(wins = "a", wins = "b")),
      "more than once",
      class = "hovercraft_error_field"
    )
    expect_error(
      tip_table(rt(), headers = list(team = "a", wins = "b", team = "c")),
      "team",
      class = "hovercraft_error_field"
    )
  })

  it("stops words that are exactly the name of a template made with tip_define()", {
    withr::defer(tip_reset())
    tip_define(row_tip = tip_template(tip_text("{{ team }}")))
    expect_error(tip_table(dt(), rows = "row_tip"), "tip_template", class = "hovercraft_error_template")
    expect_error(tip_table(dt(), cells = c(wins = "row_tip")), class = "hovercraft_error_template")
    expect_error(tip_table(dt(), rows = tip_spec("row_tip")), class = "hovercraft_error_template")
    # Words that only start with the name, and the template itself, are fine.
    expect_s3_class(tip_table(dt(), rows = "row_tip is"), "datatables")
    expect_s3_class(tip_table(dt(), rows = tip_template(tip_text("{{ team }}"))), "datatables")
  })
})

describe("tip_table(max_pins =)", {
  pins <- function(...) tip_spec("{{ team }}", pin = TRUE, ...)
  ops_of <- function(widget) {
    deps <- Filter(function(dep) grepl("^hovercraft-pins-", dep$name), widget$dependencies)
    lapply(deps, function(dep) {
      jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(dep$meta[["hovercraft-ops"]])), simplifyVector = FALSE)[[1]]
    })
  }

  it("gives the table its own count, carried in a dependency of its own", {
    for (widget in list(dt(), rt())) {
      got <- tip_table(widget, rows = pins(), id = "scores", max_pins = 2)
      expect_identical(ops_of(got), list(list(op = "tablePins", id = "scores", max = 2L)))
    }
  })

  it("adds nothing for NULL, which counts the table's pins in the app's limit", {
    got <- tip_table(dt(), rows = pins(), id = "scores")
    expect_length(ops_of(got), 0L)
  })

  it("names the table's id as the inputs know it, with the module's prefix", {
    session <- shiny::MockShinySession$new()
    got <- shiny::withReactiveDomain(
      session,
      tip_table(dt(), rows = pins(), id = "scores", max_pins = 3)
    )
    expect_identical(ops_of(got)[[1]]$id, session$ns("scores"))
  })

  it("takes a whole number from 1 to 10", {
    for (bad in list(0, 11, 1.5, NA, "2", c(1, 2))) {
      expect_error(
        tip_table(dt(), rows = pins(), id = "scores", max_pins = bad),
        class = "hovercraft_error_argument"
      )
    }
    expect_no_error(tip_table(dt(), rows = pins(), id = "scores", max_pins = 10))
  })

  it("needs the table's id and a rows spec that pins, and says which is missing", {
    expect_error(tip_table(dt(), rows = pins(), max_pins = 2), class = "hovercraft_error_pin")
    expect_error(tip_table(dt(), rows = pins(), max_pins = 2), "max_pins")
    expect_error(tip_table(dt(), rows = pins(), max_pins = 2), "id")
    expect_error(tip_table(dt(), rows = "{{ team }}", id = "scores", max_pins = 2), "max_pins")
    expect_error(tip_table(dt(), rows = tip_spec("{{ team }}"), id = "scores", max_pins = 2), "pin")
    expect_error(tip_table(dt(), cells = c(wins = "{{ wins }}"), id = "scores", max_pins = 2), "rows")
  })

  it("is refused as a misspelt argument no longer: it is an argument", {
    expect_true("max_pins" %in% names(formals(tip_table)))
    expect_null(formals(tip_table)$max_pins)
  })
})

describe("tip_spec(to = ) for the rows of tip_table()", {
  rows <- "**{{ team }}**: {{ wins | number:0 }} wins"
  place <- function(widget) widget$jsHooks$render[[1]]$data
  deps <- function(widget) vapply(widget$dependencies, function(dep) dep$name, "")

  it("takes NULL, \"float\" and \"in_place\", and \"float\" is NULL", {
    expect_null(tip_spec(rows)$to)
    expect_null(tip_spec(rows, to = NULL)$to)
    expect_null(tip_spec(rows, to = "float")$to)
    expect_identical(tip_spec(rows, to = "in_place")$to, "in_place")
    expect_identical(tip_spec(rows, to = "in_place")$to, tip_spec(rows, pin = NULL, to = "in_place")$to)
    for (bad in list("beside", "", NA_character_, c("float", "in_place"), TRUE, 1)) {
      expect_error(tip_spec(rows, to = bad), "to", class = "hovercraft_error_argument")
    }
  })

  it("is the same table with \"float\" as with no `to`", {
    plain <- tip_table(rt(), rows = tip_spec(rows, pin = TRUE), id = "t")
    floats <- tip_table(rt(), rows = tip_spec(rows, pin = TRUE, to = "float"), id = "t")
    expect_identical(place(plain), place(floats))
    expect_null(place(plain)$panel)
    expect_false(tip_table_panel_attribute() %in% names(place(plain)$rows$attrs))
  })

  it("marks the row and tells the page the names a panel needs, when it opens in place", {
    got <- place(tip_table(rt(), rows = tip_spec(rows, pin = TRUE, to = "in_place"), id = "t"))
    expect_identical(got$rows$attrs[[tip_table_panel_attribute()]], "in_place")
    expect_true("aria-expanded" %in% unlist(got$rows$reset))
    expect_identical(got$panel$attribute, tip_table_panel_attribute())
    expect_identical(got$panel$event, tip_contract()$events$tip$name)
    expect_identical(got$panel$idAttribute, tip_attribute("id"))
    expect_identical(got$panel$id, "t")
    expect_identical(tip_table_panel_attribute(), paste0("data-", tip_names()$prefix, "-table-panel"))
  })

  it("adds the panel's stylesheet only when a row opens in place", {
    css <- function(widget) {
      table <- Filter(function(dep) dep$name == "hovercraft-table", widget$dependencies)[[1]]
      table$stylesheet
    }
    expect_identical(css(tip_table(rt(), rows = tip_spec(rows, pin = TRUE, to = "in_place"), id = "t")), "table.css")
    expect_null(css(tip_table(rt(), rows = tip_spec(rows, pin = TRUE), id = "t")))
    expect_true(file.exists(system.file("table", "table.css", package = "hovercraft")))
  })

  it("stops without a pin or without the table's id, and says which is missing", {
    expect_error(
      tip_table(rt(), rows = tip_spec(rows, to = "in_place"), id = "t"),
      "a pin",
      class = "hovercraft_error_pin"
    )
    expect_error(
      tip_table(rt(), rows = tip_spec(rows, pin = TRUE, to = "in_place")),
      "table's id",
      class = "hovercraft_error_pin"
    )
    expect_error(
      tip_table(rt(), rows = tip_spec(rows, to = "in_place")),
      "a pin and the table's id",
      class = "hovercraft_error_pin"
    )
  })

  it("stops on cells and headers: only rows open in place for now", {
    spec <- tip_spec("x", to = "in_place")
    expect_error(
      tip_table(rt(), cells = list(wins = spec), id = "t"),
      "only rows open in place",
      class = "hovercraft_error_pin"
    )
    expect_error(
      tip_table(rt(), headers = list(wins = spec), id = "t"),
      "only rows open in place",
      class = "hovercraft_error_pin"
    )
    expect_no_error(tip_table(rt(), cells = list(wins = tip_spec("x", to = "float")), id = "t"))
  })

  it("stops on a DT table with the FixedColumns extension, which copies the rows", {
    skip_if_not_installed("DT")
    fixed <- DT::datatable(scores, extensions = "FixedColumns", options = list(scrollX = TRUE))
    expect_error(
      tip_table(fixed, rows = tip_spec(rows, pin = TRUE, to = "in_place"), id = "t"),
      "FixedColumns",
      class = "hovercraft_error_widget"
    )
    expect_no_error(tip_table(fixed, rows = tip_spec(rows, pin = TRUE), id = "t"))
    expect_no_error(tip_table(dt(), rows = tip_spec(rows, pin = TRUE, to = "in_place"), id = "t"))
  })

  it("calls the table layer to make panels, after the table is claimed", {
    got <- tip_table(dt(), rows = tip_spec(rows, pin = TRUE, to = "in_place"), id = "t")
    code <- got$jsHooks$render[[1]]$code
    expect_match(code, "HovercraftTable.panels(el, cfg)", fixed = TRUE)
    expect_lt(
      regexpr("HovercraftTable.claim(el, cfg.id)", code, fixed = TRUE),
      regexpr("HovercraftTable.panels(el, cfg)", code, fixed = TRUE)
    )
    expect_lt(
      regexpr("HovercraftTable.panels(el, cfg)", code, fixed = TRUE),
      regexpr("HovercraftTable.keyboard(el, cfg.kind)", code, fixed = TRUE)
    )
  })

  it("spells the panel's class names as the table layer writes them", {
    css <- paste(readLines(system.file("table", "table.css", package = "hovercraft")), collapse = "\n")
    js <- paste(readLines(system.file("table", "table.js", package = "hovercraft")), collapse = "\n")
    for (name in c("hovercraft-panel-box", "hovercraft-panel-cell", "hovercraft-more-strip-in")) {
      expect_true(grepl(name, css, fixed = TRUE), info = name)
    }
    expect_true(grepl("hovercraft-panel-box", js, fixed = TRUE))
    expect_true(grepl("data-hovercraft-panel", js, fixed = TRUE))
  })
})
