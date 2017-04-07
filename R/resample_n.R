#' Resample in the dplyr style
#'
#' This is a simple wrapper on `dplyr::sample_n`` that sets `replace=TRUE`
#'
#' @param tbl a data table
#' @param size optional
#' @export
resample_n <- function(tbl, size = nrow(tbl), .env = parent.frame()) {
  sample_n(tbl, size = size,
           .env = .env, replace = TRUE)
}
