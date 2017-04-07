#' Simulate the outcome of an election based on polling data
#'
#' Takes a data table with polling information about A/B preferences and
#' covariates for each polled individual. Fit's the specified model
#' of outcome and runs 100 simulated elections with a total electorate size of N
#' (as opposed to the poll size n).

#' @param formula - the model of individual vote outcome
#' @param data - the data table containing the polling sample
#' @param N - the size of the electorate
#' @param weights - one-sided formula giving variable to be used for weights
#' @param turnout - a fraction (e.g. 0.60) or a one-sided formula giving the name
#' of the variable expressing turnout probability for each voter in the poll. NOT YET IMPLEMENTED.

#' @examples
#' D <- data.frame(response = c(0,0,1,1,1,1), weight = 1:6, sex = c("M", "F", "M", "F", "F", "F"))
#' election(response ~ 1, D, N = 1000)
#' @export
election <- function(formula, data, N = 1000,
                     weights=NULL, turnout = 1){
  wts <- eval(weights[[2]], envir = data) # get weights
  # I want to use wts in glm(), but it isn't working
  mod <- glm(formula, data, family = "binomial")
  # note that the following output is in terms of log-odds
  preds <- predict(mod, se.fit = TRUE)
  # generate 100 draws from the predicted log-odds

  # now I'm thinking that the probabilities should stay fixed at their
  # fitted values, and the uncertainty play out in the binom
  # selection of the number of votes. So I've set the sd to zero

  log_odds <- rnorm(nrow(data) * 100, mean = preds$fit, sd = preds$se.fit)
  # shape into matrix with one row for each case.
  log_odds <- matrix(log_odds, nrow = nrow(data), byrow = FALSE)
  # convert to probability scale
  probs <- exp(log_odds) / (1 + exp(log_odds))


  # population size corresponding to each individual
  pop_size <- if (is.null(weights)) {
    N / nrow(data) # ok if not an integer
    # # equal weights, round to integer.
    # round(runif(nrow(data), min = -0.5, max = 0.5) +
    #         N / nrow(data)) # equal weights, round to integer.
  } else {
    # weights given
    N * wts / sum(wts)
    # round(runif(nrow(data), min = -0.5, max = 0.5) + W)
  }
  # How many turned out for each simulated case?
  turnout <- if (inherits(turnout, "formula")) {
    eval(turnout[[2]], envir = data)
  } else {
    rep(turnout, nrow(data)) # same probability for everyone
  }
  turnout <- matrix(turnout, nrow = nrow(data), ncol = ncol(probs))

  n_v_random <-
    rbinom(length(probs[]),
           prob = turnout,
           size = round(pop_size +
             runif(length(probs[]), min = -0.5, max = 0.5)))
  # Random turnout
  n_votes <- matrix(n_v_random, nrow = nrow(data), byrow = FALSE)
  # Deterministic turnou
  # n_votes <- turnout * matrix(pop_size, nrow = nrow(data), ncol = ncol(probs))
  votes_for <- probs * n_votes
  votes_for <- apply(votes_for,2,sum)
  total_votes <- apply(n_votes, 2, sum)


  data.frame(total_votes = total_votes,
             votes_for = votes_for , frac = votes_for / total_votes)
}
