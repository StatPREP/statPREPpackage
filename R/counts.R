#' Counts and proportions

#' @export
counts <- function(formula, data, ..., tidy = FALSE) {
  Tmp <- mosaic::tally(formula, data, ...)
  if (tidy) Tmp <- as.data.frame(Tmp)

  Tmp
}

#' @export
props <- function(formula, data, as.percent = FALSE, ..., tidy = FALSE) {
  fmt <- ifelse(as.percent, "percent", "proportion")
  Tmp <- mosaic::tally(formula, data, format = fmt, ...)
  if (tidy) Tmp <- as.data.frame(Tmp)

  Tmp
}
