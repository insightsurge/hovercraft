# Shared helpers: the server side of a plate's figure and code. One source of truth: `ui_code()` is the text the code
# note shows, and the figure is that same text, run.

# Draws the plate's figure from `figure_code()`, the part of `ui_code()` that makes the figure (all of it, unless a
# line such as the page's look is applied another way), and sends the code note the whole code, with the revision
# of the choice it answers: shared.js counts each plate's choices in `input$studio_rev`, and Copy waits until the
# code shown answers the latest one. `draw = FALSE` for a figure that is not made from code.
plate_code <- function(
  input,
  output,
  session,
  plate,
  ui_code,
  server_code = function() NULL,
  figure_code = ui_code,
  draw = TRUE
) {
  if (draw) {
    exports <- studio_exports()
    result <- shiny::reactive(run_code(figure_code(), exports))
    output[[paste0(plate, "_figure")]] <- shiny::renderUI(result()$ui)
    output[[paste0(plate, "_notes")]] <- shiny::renderUI(notes_ui(result()))
  }
  shiny::observe({
    text <- code_text(ui_code(), server_code())
    rev <- shiny::isolate(input$studio_rev[[plate]]) %||% 0
    session$sendCustomMessage("studio-code", list(plate = plate, code = text, rev = rev))
  })
}
