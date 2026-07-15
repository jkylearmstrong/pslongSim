#' Inverse Logit
#' @param x Numeric.
#' @export
inv_logit <- function(x) 1 / (1 + exp(-x))

#' Clamp Numeric Values
#' @param x Numeric vector.
#' @param lower Lower bound.
#' @param upper Upper bound.
#' @export
clamp <- function(x, lower, upper) pmax(lower, pmin(x, upper))

# Global variables to silence R CMD check NOTES for NSE
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("AtRisk", "PatientID", "w", "event", "tstop", ".pred_1", "p_marg"))
}

#' @importFrom stats aggregate as.formula binomial gaussian predict rbinom rnorm quantile
#' @importFrom utils head
#' @importFrom dplyr filter select mutate group_by
#' @importFrom xgboost xgboost
#' @import recipes
#' @import parsnip
#' @import workflows
#' @import dials
#' @import ranger
#' @import targets
NULL
