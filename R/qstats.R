#' Calculate basic statistics on a quantitative variable
#'
#' @export
qstats <- function(formula, data, ..., .level = 0.95, .wide = FALSE) {
  stopifnot(.level <= 1, .level >= 0)
  others <- lazyeval::lazy_dots(...)
  others_names <-
    if (length(others) == 0) { # default set of stats
      c("min", "Q1", "median", "Q3", "max", "mean", "sd", "n", "missing")
    } else { # stats specified in ...
      unlist(lapply(others, FUN = function(x) as.character(x$expr)))
    }

  if (inherits(formula, "data.frame") && inherits(data, "formula")) {
    # switched at birth. Likely because input is piped in
    tmp <- data
    data <- formula
    formula <- tmp
  }
  if ( ! inherits(formula, "formula")) stop("first arg must be a formula")
  if ( ! inherits(data, "data.frame")) stop("second arg must be a data table")
  MF <- model.frame(formula, data)
  var_names <- sprintf("`%s`", names(MF)) # backquote to deal with odd names like log(x)

  # calculations available
  to_do <- c(
    min = "base::min(__data__, na.rm = TRUE)",
    Q1 = "stats::quantile(__data__, na.rm = TRUE, probs = 0.25)",
    median = "stats::median(__data__, na.rm = TRUE)",
    Q3 = "stats::quantile(__data__, na.rm = TRUE, probs = 0.75)",
    max = "base::max(__data__, na.rm = TRUE)",
    mean = "base::mean(__data__, na.rm = TRUE)",
    sd = "stats::sd(__data__, na.rm = TRUE)",
    n = "n()",
    missing = "sum(is.na(__data__))",
    mean.conf.low = "statPREP:::ci_mean(__data__, level = __level__, upper = FALSE)",
    mean.conf.high = "statPREP:::ci_mean(__data__, level = __level__, upper = TRUE)",
    median.conf.low = "statPREP:::ci_median(__data__, level = __level__, upper = FALSE)",
    median.conf.high = "statPREP:::ci_median(__data__, level = __level__, upper = TRUE)",
    sd.conf.low = "statPREP:::ci_sd(__data__, level = __level__, upper = FALSE)",
    sd.conf.high = "statPREP:::ci_sd(__data__, level = __level__, upper = TRUE)",
    mean.stderr = "statPREP:::stderr_mean(__data__)"
  )

  # Fill in the variables and other params of the statistics.
  to_do <- gsub("__data__", var_names[1], to_do)
  to_do <- gsub("__level__", as.character(.level), to_do)

  # make it into a list
  to_do <- as.list(to_do)

  # Check to see if the requested statistics are available
  bogus_names <- others_names[! others_names %in% names(to_do)]
  if (length(bogus_names))
    warning("No such statistic available: ", paste(bogus_names, collapse = ", "))
  others_names <- setdiff(others_names, bogus_names)

  if (length(others_names) != 0) to_do <- to_do[others_names]


  # To do ... check for quantitative grouping variables with many, many levels
  # give a warning with advice to use ntiles() on such variables

  do.call(dplyr::group_by_, c(list(MF), as.list(var_names[-1]))) -> TMP
  dplyr::ungroup(do.call(dplyr::summarise_, c(list(TMP), as.list(to_do))))
}

ci_mean <- function(vals, level = 0.95, upper = TRUE) {
  # deduce the quantile for the top and bottom of the CI
  bottom_level <- (1 - level)/2
  top_level <- 1 - bottom_level
  p_for_t <- ifelse(upper, top_level, bottom_level)
  n = length(vals)
  base::mean(vals, na.rm = TRUE) + stats::qt(p_for_t, n-1) * stats::sd(vals, na.rm = TRUE) / sqrt(n)
}
stderr_mean <- function(vals) {
  stats::sd(vals) / length(vals)
}
ci_median <- function(vals, level=0.95, upper = TRUE) {
  vals <- sort(vals)
  n <- length(vals)
  bottom_level <- (1-level)/2
  top_level <- 1 - bottom_level
  p_for_norm <- ifelse(upper, top_level, bottom_level)
  ci_index <- (n + qnorm(p_for_norm) * sqrt(n))/2 + upper
  as.numeric(vals[round(ci_index)])
}
ci_sd <- function(vals, level=0.95, upper = TRUE) {
  n <- length(vals)
  bottom_level <- (1-level)/2
  top_level <- 1 - bottom_level
  # the following seems backwards, but it's not because
  # of the form of the CI for standard deviations
  p_for_chisq <- ifelse( ! upper, top_level, bottom_level)
  result = (n-1) * stats::sd(vals, na.rm = TRUE)^2 / qchisq(p_for_chisq, df=n-1)

  sqrt(result)
}
