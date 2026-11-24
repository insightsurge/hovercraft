# hovercraft book: every tip feature on one page, with the R code for each, what to test and the inputs it sends.
# Open it with hovercraft::hovercraft_book(). Shiny loads the files in R/ before this one (the shared helpers in
# R/shared-*.R, one file per plate in R/plate-*.R, the page in R/page.R) and serves www/. It needs shiny and bslib;
# the Tables plate also needs DT or reactable.
for (pkg in c("shiny", "bslib", "hovercraft")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("hovercraft book needs the ", pkg, " package. Install it and run the book again.", call. = FALSE)
  }
}

shiny::shinyApp(
  ui = function(request) book_page(book_query(request)),
  server = book_server
)
