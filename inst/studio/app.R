# hovercraft studio: every tip feature on one page, with the R code for each, what to test and the inputs it sends.
# Open it with hovercraft::tip_studio(). Shiny loads the files in R/ before this one (the shared helpers in
# R/shared-*.R, one file per plate in R/plate-*.R, the page in R/page.R) and serves www/. It needs shiny and bslib;
# the Tables plate also needs DT or reactable.
for (pkg in c("shiny", "bslib", "hovercraft")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("hovercraft studio needs the ", pkg, " package. Install it and run the studio again.", call. = FALSE)
  }
}

shiny::shinyApp(
  ui = function(request) studio_page(studio_query(request)),
  server = studio_server
)
