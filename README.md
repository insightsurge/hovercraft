# hovercraft

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Set up with launchR](https://img.shields.io/badge/set%20up%20with-launchR-f6ab85)](https://launch-r.web.app)
<!-- badges: end -->

At 0.1.0 `tip_on()` (its words, templates, `pin` one at a time and `more`), the template and block functions,
`tip_presets()` and `use_hovercraft(style = )` by a preset name are stable. Tables, server calls, `events`, `when_disabled`,
`tip_output()`, looks given as objects (`tip_style()`, `tip_theme()`, `tip_marker()`), `tip_more()`, several pins and
`keys` are experimental and may change. Each help page says which.

Tips for Shiny apps and bslib pages. Put a tip on any HTML tag: plain text, or a named template filled from the values
of one item, such as a row of a table. The tip shows when the pointer rests on the tag or keyboard focus arrives, and
hides when either leaves.

The tips are drawn by one small JavaScript file that ships with the package, so there is nothing to install in the
browser and nothing to load from the internet. It reads Bootstrap's colours where the page has them (bslib), so a tip
follows the page's light and dark theme.

## Installation

```r
# install.packages("pak")
pak::pak("insightsurge/hovercraft")
```

## Example

A tip with plain text. The text is markdown, shown as written:

```r
library(hovercraft)

tip_on(htmltools::tags$button("Run"), "Runs the **check**")
```

A tip drawn from a template, with one row of a data frame as the record for each row of a table:

```r
rows <- data.frame(label = c("ADAE", "ADSL"), differences = c(3, 0))

ui <- bslib::page_fluid(
  tip_define(
    diff = tip_template(
      tip_title("{{ label }}"),
      tip_text("{{ differences | number:0 }} differences")
    )
  ),
  htmltools::tags$table(
    lapply(seq_len(nrow(rows)), function(i) {
      tip_on(
        htmltools::tags$tr(htmltools::tags$td(rows$label[[i]]), tabindex = "0"),
        template = "diff",
        record = rows[i, ]
      )
    })
  )
)
```

A placeholder is `{{ field }}`, or `{{ field | number:2 }}` and `{{ field | percent:1 }}` to round. A value that is
missing or blank shows a dash. Mistakes in a template, such as a placeholder that never closes, stop with an error when
the template is made, not later in the browser.

A tip the reader can keep open. With `pin = TRUE` the tip gets a small pin button, and the `P` key pins it too; the tip
then stays until the reader closes it. `events = "pin"` sends what happened to the server as `input$<id>_pin`, here
`input$run_pin`:

```r
library(shiny)

ui <- fluidPage(
  tip_on(
    actionButton("run", "Run"),
    "Runs the **check** on every dataset",
    pin = TRUE,
    events = "pin",
    id = "run"
  ),
  textOutput("state")
)

server <- function(input, output, session) {
  output$state <- renderText({
    if (is.null(input$run_pin)) "Nothing pinned yet" else paste("The tip is", input$run_pin$state)
  })
}

shinyApp(ui, server)
```

One pin stays open at a time; `use_hovercraft(max_pins = 3)` in the page's UI keeps up to three (a table can have its own count with `tip_table(max_pins =)`).

`state` is `"pinned"` when the reader pins the tip, and `"closed"` when it closes, with `reason` saying why. `events =
"hint"` does the same for the tip that shows on hover or focus. `tip_show()`, `tip_hide()` and `tip_update()` change the
tip from the server; for a table's tip they take the table's `id` with `row` and `column`
(`tip_show("scores", row = 2, column = "wins")`).

A short tip with a long form. `more =` holds the long form behind a "More" button at the bottom of the tip, and the `M`
key opens it too; "Less" closes it again. The tip stays open and on its side while it grows and shrinks, and it is short
again the next time it opens. `tip_more()` says when the long form shows (`on = "button"`, `"hover"` or `"pin"`) and what
the buttons say (`label =`); it is experimental:

```r
tip_on(
  actionButton("run", "Run"),
  "Runs the **check**",
  more = "Checks every column and prints a summary. Takes about a minute on the full data."
)
```

A live Shiny output inside a tip. `tip_output()` puts a plot, a table or any other output in a template; the output is
drawn while the tip is open:

```r
library(shiny)

ui <- fluidPage(
  tip_define(
    trend = tip_template(
      tip_title("{{ label }}"),
      tip_output(plotOutput("trend_plot", width = 260, height = 140))
    )
  ),
  tip_on(
    actionButton("adae", "ADAE"),
    template = "trend",
    record = list(label = "ADAE differences"),
    id = "adae"
  )
)

server <- function(input, output, session) {
  output$trend_plot <- renderPlot({
    par(mar = c(2, 2, 1, 1))
    plot(c(3, 5, 4, 8), type = "b")
  })
}

shinyApp(ui, server)
```

The tags work in a bslib page, in a plain Shiny page and in output made later with `renderUI()`. A tag inside
`bslib::tooltip()` keeps the tip that bslib draws.

A tip shows on keyboard focus as well as on a pointer, so a tag that cannot take focus (a table row, a `div`) gives a
keyboard user no way to its tip. `tip_on()` says so once. It never adds a tab stop itself, because a table of 3,000 rows
would become 3,000 Tab stops; give the tags worth a stop `tabindex = "0"`, as above, or put the tip on a button or a
link. The rows, cells and headers of a `DT::datatable()` or `reactable::reactable()` table are the exception: `tip_table()` gives the table one tab
stop and moves between them with the arrow keys.

## Themes

The tip follows the page: on a bslib page it uses the theme's colours and turns dark with `input_dark_mode()`. To set its look
yourself, give `use_hovercraft()` a `tip_theme()` as `style` once, in the page's UI:

```r
brand <- tip_theme(
  light = list(bg = "#1b3a6b", text = "#ffffff", radius = 10),
  dark = list(bg = "#9ec5fe", text = "#0b1f3a")
)
bslib::page_fluid(use_hovercraft(style = brand), ...)
```

`style` also takes a preset name, `use_hovercraft(style = "sober")` (`tip_presets()` lists them), or a `tip_style()` that
bundles a preset, a theme, a class for the tip box, and its motion and size (`tip_style(motion = "slide", max_width = 280)`). One tag can have its own look: `tip_on(btn, "Run", style = "dense")`.

A theme can also be a `bslib::bs_theme()` or a list of CSS variables; the words are checked for contrast (4.5:1). See
`vignette("themes")`, and `vignette("hovercraft")` to get started.

## Try every feature in the studio

hovercraft studio is a Shiny app with every tip feature on one page. Rest the pointer on a figure, change its choices,
copy the R code that draws it, and follow a short list of what to test while the page shows the inputs your server
would get. It needs shiny and bslib, and `DT` or `reactable` for its Tables plate.

```r
tip_studio()
tip_studio(plate = "pins", guide = TRUE)
```

Set up with [launchR](https://launch-r.web.app).
