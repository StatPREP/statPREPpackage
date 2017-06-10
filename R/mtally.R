#' mtally
#'
#' A function to disambiguate between dplyr::tally and mosaic::tally.
#' mtally works like mosaic::tally, producing a table-class rather than a data table.
#' The table-class output is better suited as a data presentation to people,
#' and is also suited to functions such as chisq.test() or fisher.test()
#' mtally() is slightly different from mosaic::tally() in accepting a piped
#' data frame as input
#'
#' @param .data placeholder for a piped-in data set.
#' @param ... the formula and other arguments for mosaic::tally
#'
#' @seealso tally in the mosaic package

#' @examples
#' mtally(mtcars, ~ cyl)
#' @export
mtally <- function(.data, ...) {
  mosaic::tally(..., data = .data)
}
