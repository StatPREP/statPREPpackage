#' Intelligently set the significant digits
#'
#' Just a rough version for now#'
#'
#' @export
digits <- function(df, n = 3) {
  purrr::map_if(df, is.numeric, ~ signif(.x, digits = n)) %>%
    as.tibble()
}
