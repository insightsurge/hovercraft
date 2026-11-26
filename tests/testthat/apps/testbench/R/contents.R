# The bench's contents: its tabs in order. Each tab `<id>` has two functions found by name, `tab_<id>_ui()` and
# `tab_<id>_server(input, output, session)`, in R/tab-<id>.R. Element ids in a tab start with its id, so every tab can
# share one page; the practice tab keeps the ids its test has always used.
bench_tabs <- data.frame(
  id = c(
    "practice",
    "js",
    "layering",
    "placement",
    "input",
    "tables",
    "updates",
    "host",
    "a11y",
    "theme",
    "scale",
    "leaks"
  ),
  title = c(
    "Shiny practices",
    "JavaScript-driven UI",
    "Layering",
    "Placement",
    "Keys, pointer, touch",
    "Tables",
    "Table updates",
    "Shiny host",
    "Accessibility",
    "Theme",
    "Scale and leaks",
    "Leak counts"
  )
)
