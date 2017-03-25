#' Hypothesis tests
#'
#' `htest` carries out a variety of hypothesis tests commonly encountered in an
#' introductory statistics course.
#'
#' @param formula a formula expression the relationship among the variable
#' @param data a data table containing the variables used in the formula
#' @param ... other arguments for the particular test
#' @param method for regression, whether to do "linear" regression (default) or logistic regression
#' @param test character string naming the particular flavor of test to conduct:
#' "t", "chisq", "exact" (Fisher exact test), "coefficients" (as in regression),
#' "anova"
#' @export
htest <- function(formula, data, ...,
                  test = c("t", "chisq", "exact", "goodness", "independence", "coefficients", "anova"),
                  method = c("linear", "logistic"),
                  prob_model = NULL) {
  test <- match.arg(test)
  method <- match.arg(method)

  if (inherits(formula, "data.frame") && inherits(data, "formula")) {
    # switched at birth. Likely because input is piped in
    tmp <- data
    data <- formula
    formula <- tmp
  }
  if ( ! inherits(formula, "formula")) stop("first arg must be a formula")
  if ( ! inherits(data, "data.frame")) stop("second arg must be a data table")
  data <- model.frame(formula, data)
  if (test == "t") {
    if (! inherits(data[[1]], c("numeric", "logical", "AsIs")))
      stop("You've given a categorical variable to the t-test. Either convert it to numerical or, perhaps, use a chi-squared type test.")
    if (length(all.vars(mosaic::rhs(formula))) > 1)
      stop("in t-test there can be only one variable on the RHS of the formula.")
    res <- if (length(all.vars(mosaic::lhs(formula))) == 1) {
      # two-sample t-test
      # done with indexing to get around a problem
      # with formula evaluation
      # would rather it be t.test(formula, data, ...)
      stats::t.test(data[[1]] ~ data[[2]], ...)
    } else {
      # one-sample t-test
      stats::t.test(data[[deparse(mosaic::rhs(formula))]])
    }
    res <- broom::tidy(res)
    res$parameter <- NULL
    res$estimate1 <- NULL
    res$estimate2 <- NULL
    res$method <- NULL
    res$alternative <- NULL
    return(res)
  } else if (test %in% c("chisq", "goodness", "independence") ) {
    res <- counts(formula, data)
    if (is.null(prob_model)) {
      res <- broom::tidy(chisq.test(res))
    } else {
      if (length(prob_model) == 1) {
        prob_model <- rep(prob_model, nrow(res))
      } else if (length(prob_model) != nrow(res)) {
        stop("prob_model has wrong number of entries. It should have a value for each group")
      }
      res <- broom::tidy(chisq.test(res, p = prob_model))
    }
    res$method <- NULL
    return(res)
  } else if (test == "exact") {
    res <- counts(formula, data)
    res <- broom::tidy(fisher.test(res))
    res$method <- NULL
    res$alternative <- NULL
    return(res)
  } else if (test == "coefficients" || test == "anova") {
    mod <- switch(method,
                  linear = lm(formula, data = data, ...),
                  logistic = glm(formula, data = data, family = "binomial", ...)
                  )
    if (test == "anova") mod <- anova(mod)
    return(broom::tidy(mod))
  }


}
