# hovercraft <!-- logo: add `<img src="man/figures/logo.png" align="right" height="139" alt="" />` here when the hex exists -->

<!-- badges: start -->
[![R-CMD-check](https://github.com/insightsurge/hovercraft/actions/workflows/r-cmd-check.yaml/badge.svg)](https://github.com/insightsurge/hovercraft/actions/workflows/r-cmd-check.yaml)
[![R code coverage](https://codecov.io/gh/insightsurge/hovercraft/graph/badge.svg)](https://app.codecov.io/gh/insightsurge/hovercraft)
[![R-universe version](https://insightsurge.r-universe.dev/hovercraft/badges/version)](https://insightsurge.r-universe.dev/hovercraft)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Set up with launchR](https://img.shields.io/badge/set%20up%20with-launchR-f6ab85)](https://launch-r.web.app)
<!-- badges: end -->

Tips for Shiny apps and bslib pages. Rest the pointer on a button, a table row or a word, and a tip tells the reader what
it is, or shows the values behind it. It works with the keyboard too, and follows your page's light and dark theme.

<!-- demo: add man/figures/readme-demo.gif (or .svg) here -->

A tip is plain text, or a named template filled in from one record, such as a row of a table. Readers can pin a tip to
keep it open, open a long form behind a "More" button, and your Shiny server can see what they did and show, hide or change a tip. The tips run
on JavaScript and CSS that ship inside the package: nothing to install in the browser, nothing loaded from the internet.

[Get started](https://insightsurge.github.io/hovercraft/articles/hovercraft.html) ·
[Try every feature in the book](https://insightsurge.github.io/hovercraft/articles/the-book.html) ·
[Function reference](https://insightsurge.github.io/hovercraft/reference/index.html)

## Installation

hovercraft needs R 4.4.0 or later. It is not on CRAN yet. Install the development version, built by
[R-universe](https://insightsurge.r-universe.dev/hovercraft):

```r
install.packages(
  "hovercraft",
  repos = c("https://insightsurge.r-universe.dev", "https://cloud.r-project.org")
)
```

Or install it from GitHub:

```r
# install.packages("pak")
pak::pak("insightsurge/hovercraft")
```

## Example

Put a tip on any tag. The text is markdown:

```r
library(hovercraft)

tip_on(htmltools::tags$button("Run"), "Runs the **check**")
```

Or write the tip once as a template, and give each tag its own values:

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
`NA` or `NULL` shows a dash. A mistake in a template, such as a placeholder that never closes, stops with an error when the template is
made, not later in the browser.

## In a Shiny app

Pin a tip, and hear about it:

```r
library(shiny)
library(hovercraft)

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

The guides cover the rest:

- [Tips in a Shiny app](https://insightsurge.github.io/hovercraft/articles/shiny.html): pins, events, opening a tip from
  the server, the reason a button is off, and live plots and tables in a tip.
- [Tips on tables](https://insightsurge.github.io/hovercraft/articles/tables.html): a tip on every row, cell and header
  of a `DT` or `reactable` table.
- [A short tip with a long form](https://insightsurge.github.io/hovercraft/articles/long-form.html): a "More" button.
- [Themes and looks](https://insightsurge.github.io/hovercraft/articles/themes.html): follow the page, pick a preset, or
  set your own colours, with a contrast check.

## Try every feature

`hovercraft_book()` opens a Shiny app with every feature on one page. Rest the pointer on a figure, change its choices,
copy the R code that draws it, and follow a short checklist of what to test.

```r
hovercraft::hovercraft_book()
hovercraft::hovercraft_book(section = "pins", checklist = TRUE)
```

## Development status

Planned as stable in 0.1.0, the first CRAN release: `tip_on()` with its words, templates, `record`, `placement`, `id`,
`pin` one at a time and `more` as words or a template; the template and block functions; `tip_presets()`; and
`use_hovercraft(style = )` with a preset name.

Experimental, and may change in a later release: tables (`tip_table()`, `tip_spec()`), server calls (`tip_show()`,
`tip_hide()`, `tip_update()`), `events`, `when_disabled`, `tip_output()`, looks given as objects (`tip_style()`,
`tip_theme()`, `tip_marker()`), `marker`, `labels`, `tip_more()`, several pins at once, `keys` and `hovercraft_book()`.
Each help page says which part is which.

## What is public on the page

hovercraft puts the tip engine and a few helper scripts on a page. Your own CSS and JavaScript may rely on:

- the `Hovercraft` global of the tip engine;
- the events it sends: `hovercraft:tip`, `hovercraft:hint` and `hovercraft:copy`;
- the `data-hovercraft-tip*` attributes;
- the `hc-*` classes of the tip, and the `--hovercraft-tip-*` CSS variables that `tip_style(theme = )` sets.

Everything else on the page is internal and may change in any release: the helper scripts and their other global names,
file names and URLs, the settings tags, other classes and CSS variables, and the other `data-hovercraft-*` attributes.

## Getting help and contributing

Found a bug, or have an idea? Open an [issue](https://github.com/insightsurge/hovercraft/issues). To contribute, read
[CONTRIBUTING](https://github.com/insightsurge/hovercraft/blob/main/.github/CONTRIBUTING.md) first. Please follow the
[code of conduct](https://github.com/insightsurge/hovercraft/blob/main/.github/CODE_OF_CONDUCT.md). To report a security
problem, see the [security policy](https://github.com/insightsurge/hovercraft/blob/main/.github/SECURITY.md).

## Licence

MIT. See [LICENSE.md](LICENSE.md). The bundled tip engine is covered by the same licence.

Set up with [launchR](https://launch-r.web.app).
