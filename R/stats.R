#' Another take on a general-purpose statistics function
#'
#' @export
stats_factory <- function(f) {
  mean <- function(x, na.rm = TRUE, trim = 0) 17
  fun <- function(df, ..., .level = TRUE,  na.rm = TRUE) {
    stats_to_calc <- quos(...)
    summarise(df, !!!stats_to_calc)
  }
}

#' @export
S <- stats_factory(mean)
#' @export
stats <- function(df, ..., .level = 0.95, na.rm = TRUE) {


  stats_to_calc <- quos(...)
  .stats_env. <- new.env(parent = environment(stats_to_calc$ddd))
  .stats_env.$mean <- function(x, ...) {
    warning("In my mean.\n")
    base::mean(x, na.rm = na.rm)
  }
  mymean <- function(x) {warning("My mean"); sum(100*x)}
  browser()
  text <- as.character(stats_to_calc$xyz)
  text <- gsub("mean(", "mymean(", text, fixed = TRUE)
  stats_to_calc$xyz <- as.formula(text)
#  environment(stats_to_calc$ddd) <- .stats_env.
#  environment(stats_to_calc$xyz) <- .stats_env.
  #my_sum <- dplyr::summarize
  #environment(my_sum) <- .stats_env.
  summarise(df, !!!stats_to_calc)

}


