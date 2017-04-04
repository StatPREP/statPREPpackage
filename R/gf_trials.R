#' Iterate a graphic
#'
#' @param data a data table or, more usually, an expression that produces
#' a data table, e.g. `mosaic::resample(CPS85)`
#'
#' @details It makes sense to use this only for graphics that do a
#' substantial calculation on the data.
#'
#'
#' @examples
#' gf_do(wage ~ sector + color:sex + alpha:.1, gf_boxplot,
#'   data = mosaic::resample(CPS85), ntrials = 10)
#' gf_do(~ wage + color:sex + alpha:.05, gf_freqpoly,
#'   data = mosaic::resample(CPS85), ntrials = 10)
#' gf_do(wage ~ sex + alpha:.05, gf_violin,
#'   data = mosaic::resample(CPS85), ntrials = 10)
#' gf_do(~ wage  + fill:sex + alpha:.05, gf_histogram,
#'   data = mosaic::resample(CPS85), ntrials = 50) %>%
#'   gf_facet_wrap(~sex)
#' @export
gf_do <- function(formula, what, ntrials = 3, data, start = NULL, ...) {
  data = substitute(data)
  if (is.null(start)) start <- ggplot()
  for (k in 1:ntrials) {
    this_run <- eval(data, envir = environment(formula))
    form <- as.formula(substitute(formula))
    start <- start %>% what(form, data = this_run, add = TRUE, ...)

  }

  start

}
