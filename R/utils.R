#' @keywords internal
logit <- function(x) log(x / (1 - x))

#' @keywords internal
inv_logit <- function(z) 1 / (1 + exp(-z))

#' @keywords internal
clamp <- function(x, lo, hi) pmax(lo, pmin(hi, x))
