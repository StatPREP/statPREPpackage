#' Resample in the dplyr style
#'
#' This is a simple wrapper on `dplyr::sample_n`` that sets `replace=TRUE`
#'
#' @param tbl a data table
#' @param size optional. Defaults to the size of \code{tbl}
#' @param weight optional weighing to use when sampling.
#' @export
resample_n <- function(tbl, size = nrow(tbl), weight = NULL) {
  sample_n(tbl, size = size, replace = TRUE, weight = NULL)
}
