# hovercraft

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Set up with launchR](https://img.shields.io/badge/set%20up%20with-launchR-f6ab85)](https://launch-r.web.app)
<!-- badges: end -->

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
then stays until the reader closes it. `events = "pin"` sends what happened to the server as `input$<id>_tip`, here
`input$run_tip`:

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
    if (is.null(input$run_tip)) "Nothing pinned yet" else paste("The tip is", input$run_tip$state)
  })
}

shinyApp(ui, server)
```

`state` is `"pinned"` when the reader pins the tip, and `"closed"` when it closes, with `reason` saying why. `events =
"hint"` does the same for the tip that shows on hover or focus. `tip_show()`, `tip_hide()` and `tip_update()` change the
tip from the server.

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
yourself, give `tip_dependency()` a `tip_theme()` as `style` once, in the page's UI:

```r
brand <- tip_theme(
  light = list(bg = "#1b3a6b", ink = "#ffffff", radius = 10),
  dark = list(bg = "#9ec5fe", ink = "#0b1f3a")
)
bslib::page_fluid(tip_dependency(style = brand), ...)
```

`style` also takes a preset name, `tip_dependency(style = "sober")` (`tip_presets()` lists them), or a `tip_style()` that
bundles a preset, a theme, a class for the tip box, and its motion and size (`tip_style(motion = "slide", max_width = 280)`). One tag can have its own look: `tip_on(btn, "Run", style = "dense")`.

A theme can also be a `bslib::bs_theme()` or a list of CSS variables; the words are checked for contrast (4.5:1). See
`vignette("themes")`, and `vignette("hovercraft")` to get started.

Set up with [launchR](https://launch-r.web.app).
