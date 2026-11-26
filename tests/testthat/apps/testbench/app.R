# hovercraft test bench: the Shiny practices and the behaviour scenarios in one app, for a walkthrough by hand and for
# the browser tests. Run it from the package folder:
#   shiny::runApp("tests/testthat/apps/testbench", port = 8148)
# Shiny loads the files in R/ before this one (the page in R/shared-page.R, the list of tabs and scenarios in
# R/contents.R, one file per tab in R/tab-*.R) and serves www/. The tests start it with start_bench(), which loads the
# package first; by hand, it loads the package from this checkout.
if (!isNamespaceLoaded("hovercraft")) {
  root <- normalizePath(file.path("..", "..", "..", ".."), mustWork = FALSE)
  if (file.exists(file.path(root, "DESCRIPTION")) && requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(root, quiet = TRUE)
  } else {
    loadNamespace("hovercraft")
  }
}

shiny::shinyApp(ui = bench_ui, server = bench_server)
