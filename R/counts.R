#' Counts and proportions
#' @param level set to, e.g. 0.95 for the confidence intervals
#' to be generated.

#' @export
counts <- function(formula, data, ..., tidy = TRUE) {
  Tmp <- mosaic::tally(formula, data, ...)
  if (tidy) Tmp <- as.data.frame(Tmp)

  Tmp
}

#' @export
props <- function(formula, data, as.percent = FALSE, level = NULL, ..., tidy = TRUE) {
  fmt <- ifelse(as.percent, "percent", "proportion")
  Tmp <- mosaic::tally(formula, data, format = fmt, ...)
  Tmp <- as.data.frame(Tmp)
  names(Tmp)[2] <- "proportion"
  if ( !is.null(level)) {
    # add the low and high confidence intervals
    Tmp$conf.low <- qbinom((1-level)/2, size = nrow(data),
                           prob = Tmp$proportion)/nrow(data)
    Tmp$conf.high <- qbinom(1-(1-level)/2, size = nrow(data),
                            prob = Tmp$proportion)/nrow(data)
  }

  if ( !tidy ) {
    foo <- t(Tmp[, -1])
    colnames(foo) <- Tmp[, 1]
    Tmp <- foo
  }

  Tmp
}
