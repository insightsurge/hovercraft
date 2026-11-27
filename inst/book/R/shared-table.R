# Shared helpers: the table sections (Part 6), each a DT or reactable table with tips from tip_table().

table_widgets <- c("DT" = "DT", "reactable" = "reactable")

# The table package a section draws with: DT when it is installed, else reactable.
table_default <- function() if (requireNamespace("DT", quietly = TRUE)) "DT" else "reactable"

# A section's table as code: its data, the widget drawn from it, and `tipped`, the tip_table() call it is piped into.
table_code <- function(name, data, widget, tipped) {
  table <- if (identical(widget, "DT")) {
    code_call("DT::datatable", name, rownames = "FALSE", options = "list(dom = \"t\")")
  } else {
    code_call("reactable::reactable", name)
  }
  paste0(code_df(name, data), "\n\n", table, " |>\n  ", code_indent(tipped))
}
