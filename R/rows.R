#' A data frame as one record per row
#'
#' `r lifecycle::badge("experimental")`
#' Turns a data frame into a list of records, one for each row, each a named list of that row's values. Use it to put a
#' tip on every row of a table: `Map()` over the rows and the records. A list-column keeps each cell whole, so a cell
#' that holds more than a single value is an error when it reaches [tip_on()] (keep only the columns the template
#' names).
#'
#' @param data A data frame.
#' @param columns The names of the columns to keep, or `NULL` for all of them. Name the columns a template uses, so
#'   nothing else in the data is written into the page.
#'
#' @returns An unnamed list with one named list per row.
#' @family tooltips
#' @examples
#' rows <- data.frame(label = c("ADAE", "ADSL"), differences = c(3, 0))
#' tip_rows(rows)[[1]]
#' @export
tip_rows <- function(data, columns = NULL) {
  checkmate::assert_data_frame(data)
  checkmate::assert_character(columns, any.missing = FALSE, min.len = 1L, null.ok = TRUE)
  if (!is.null(columns)) {
    unknown <- setdiff(columns, names(data))
    if (length(unknown)) {
      tip_abort(
        c(
          "{.arg columns} names {.val {unknown}}, which {.arg data} does not have.",
          i = "It has {.val {names(data)}}."
        ),
        class = "hovercraft_error_record"
      )
    }
    data <- data[columns]
  }
  tip_check_columns(data)
  lapply(seq_len(nrow(data)), function(i) lapply(data, function(column) column[[i]]))
}
