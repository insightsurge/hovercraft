#' A data frame as one record per row
#'
#' `r lifecycle::badge("experimental")`
#' Turns a data frame into a list of records, one for each row, each a named list of that row's values. Use it to put a
#' tip on every row of a table: `Map()` over the rows and the records. A list-column keeps each cell whole, so a cell
#' that holds more than a single value is an error when it reaches [tip_on()] (keep only the columns the template
#' names).
#'
#' @param data A data frame.
#'
#' @returns An unnamed list with one named list per row.
#' @family tooltips
#' @examples
#' rows <- data.frame(label = c("ADAE", "ADSL"), differences = c(3, 0))
#' tip_rows(rows)[[1]]
#' @export
tip_rows <- function(data) {
  checkmate::assert_data_frame(data)
  lapply(seq_len(nrow(data)), function(i) lapply(data, function(column) column[[i]]))
}
