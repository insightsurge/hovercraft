# Theme, HC-067 to HC-071: dark mode, forced colours, reduced motion, the page's and a tag's own look, large text.

tab_theme_ui <- function() {
  presets <- hovercraft::tip_presets()$name
  htmltools::tagList(
    spot(
      "HC-067",
      note = "Pin it, then switch dark mode with the moon in the top bar.",
      pin_button("theme-dark", "Pin, then switch", "Readable in both", more = "The long form keeps its contrast.")
    ),
    spot(
      "HC-068",
      note = "In the browser's rendering tools, emulate forced-colors: active; hover and pin the button above."
    ),
    spot(
      "HC-069",
      note = paste(
        "Emulate prefers-reduced-motion: reduce;",
        "the tip below slides when motion is allowed and does not with reduce."
      ),
      tip_button("theme-motion", "Slides in", "I slide in", style = hovercraft::tip_style(motion = "slide"))
    ),
    spot(
      "HC-070",
      note = htmltools::tagList(
        "The page's look: ",
        lapply(presets, function(p) htmltools::tagList(bench_link(p, only = "theme", style = p), " ")),
        "; each button below has its own."
      ),
      lapply(presets, function(p) tip_button(paste0("theme-preset-", p), p, paste("The", p, "look"), style = p))
    ),
    spot(
      "HC-071",
      note = htmltools::tagList(
        bench_link("Open with a 24 px root font and a serif", only = "theme", font = "large"),
        "."
      ),
      pin_button("theme-font", "Large text", "Sized by the page's font", more = "A long form in large text.")
    )
  )
}

tab_theme_server <- function(input, output, session) {
  invisible()
}
