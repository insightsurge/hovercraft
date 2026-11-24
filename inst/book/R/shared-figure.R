# Shared helpers: the server side of a plate's figure and code. One source of truth: `ui_code()` is the text the code
# note shows, and the figure is that same text, run.

# Draws the plate's figure from `figure_code()`, the part of `ui_code()` that makes the figure (all of it, unless a
# line such as the page's look is applied another way), and sends the code note the whole code, with the revision
# of the choice it answers: shared.js counts each plate's choices in `input$book_rev`, and Copy waits until the
# code shown answers the latest one. `needs()` names a package the figure draws with, such as DT: while it is not
# installed, the figure says so instead.
plate_code <- function(
  input,
  output,
  session,
  plate,
  ui_code,
  server_code = function() NULL,
  figure_code = ui_code,
  needs = function() NULL
) {
  exports <- book_exports()
  result <- shiny::reactive({
    package <- needs()
    if (!is.null(package) && !requireNamespace(package, quietly = TRUE)) {
      return(list(ui = NULL, warnings = character(), error = paste0("This section needs the ", package, " package.")))
    }
    run_code(figure_code(), exports)
  })
  output[[paste0(plate, "_figure")]] <- shiny::renderUI(result()$ui)
  output[[paste0(plate, "_notes")]] <- shiny::renderUI(notes_ui(result()))
  shiny::observe({
    text <- code_text(ui_code(), server_code())
    rev <- shiny::isolate(input$book_rev[[plate]]) %||% 0
    session$sendCustomMessage("book-code", list(plate = plate, code = text, rev = rev))
  })
}
