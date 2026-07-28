#' Inverse Logit
#'
#' Computes the inverse logit (logistic) transformation.
#'
#' @param x Numeric vector.
#' @return Numeric vector of probabilities.
#' @examples
#' inv_logit(0)
#' inv_logit(c(-2, 0, 2))
#' @export
inv_logit <- function(x) 1 / (1 + exp(-x))

#' Clamp Numeric Values
#'
#' Constrains a numeric vector to lie within \code{[lower, upper]}.
#'
#' @param x Numeric vector.
#' @param lower Lower bound (must be <= upper).
#' @param upper Upper bound (must be >= lower).
#' @return Numeric vector with values clamped to \code{[lower, upper]}.
#' @examples
#' clamp(c(-1, 5, 3), 0, 4)
#' clamp(0.5, 0, 1)
#' @export
clamp <- function(x, lower, upper) {
  if (lower > upper) {
    stop("`lower` (", lower, ") must be <= `upper` (", upper, ").")
  }
  pmax(lower, pmin(x, upper))
}

#' Validate Required Columns Exist in a Data Frame
#'
#' @param df Data frame to check.
#' @param cols Character vector of required column names.
#' @param fn_name Name of the calling function (for the error message).
#' @return Invisible NULL. Called for its side effect of raising an error.
#' @keywords internal
validate_columns <- function(df, cols, fn_name = sys.call(sys.parent())[[1]]) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0L) {
    stop(fn_name, ": missing required column(s): ",
         paste(missing, collapse = ", "))
  }
  invisible(NULL)
}

#' Validate Data Frame Has Sufficient Rows
#'
#' @param df Data frame.
#' @param min_rows Minimum number of rows required.
#' @param fn_name Name of the calling function.
#' @return Invisible NULL.
#' @keywords internal
validate_min_rows <- function(df, min_rows, fn_name = sys.call(sys.parent())[[1]]) {
  if (nrow(df) < min_rows) {
    stop(fn_name, ": requires at least ", min_rows, " row(s), got ", nrow(df), ".")
  }
  invisible(NULL)
}

#' Check That a Suggested Package Is Installed
#'
#' @param pkg Character string: package name.
#' @return Invisible NULL. Called for its side effect of raising an error.
#' @keywords internal
require_suggested <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required but not installed. ",
         "Install it with install.packages('", pkg, "').")
  }
  invisible(NULL)
}

#' Sanitize Non-linear Term Column Name
#'
#' @param term_str Non-linear expression or interaction string.
#' @return Sanitized column name starting with \code{"NL_"}.
#' @keywords internal
sanitize_nl_col_name <- function(term_str) {
  parts <- strsplit(term_str, ":", fixed = TRUE)[[1L]]
  if (all(grepl("^[A-Za-z0-9_]+$", parts))) {
    return(paste0("NL_", paste(parts, collapse = "_")))
  }
  clean <- gsub("[^A-Za-z0-9]", "_", term_str)
  clean <- gsub("_+", "_", clean)
  clean <- gsub("^_|_$", "", clean)
  paste0("NL_", clean)
}

#' Parse and Evaluate Non-linear Interaction Term
#'
#' Evaluates non-linear expressions in the context of a data frame \code{df}.
#' Supports multi-way interactions (\code{"A:B:C"}), step functions
#' (\code{"step(Age, 65)"}), quadratics (\code{"I(Age^2)"}), and binary logic
#' (\code{"xor(SideEffect, Compliant)"}).
#'
#' @param term_str String expression for non-linear term.
#' @param df Data frame containing the predictor columns.
#' @return Numeric vector evaluated on \code{df}, or \code{NULL} on error.
#' @keywords internal
parse_eval_nl_term <- function(term_str, df) {
  tokens <- unlist(regmatches(term_str, gregexpr("[A-Za-z0-9_]+", term_str)))
  builtins <- c("step", "xor", "I", "c", "as", "numeric", "logical", "TRUE", "FALSE")
  cand_vars <- setdiff(tokens, builtins)
  cand_vars <- cand_vars[!grepl("^[0-9]+$", cand_vars)]
  missing_vars <- setdiff(cand_vars, names(df))
  if (length(missing_vars) > 0L) {
    warning("nonlin_coefs: variable(s) ", paste(missing_vars, collapse = ", "),
            " not in data, skipping '", term_str, "'.")
    return(NULL)
  }

  expr_text <- gsub(":", " * ", term_str, fixed = TRUE)
  eval_env <- list2env(
    list(
      step = function(x, cut) as.numeric(x > cut),
      xor  = function(x, y) as.numeric(as.logical(x) != as.logical(y))
    ),
    parent = parent.frame()
  )
  res <- tryCatch(
    eval(parse(text = expr_text), envir = df, enclos = eval_env),
    error = function(e) {
      warning("nonlin_coefs: failed to evaluate term '", term_str, "': ", e$message)
      NULL
    }
  )
  if (is.null(res)) return(NULL)
  as.numeric(res)
}


# Global variables to silence R CMD check NOTES for NSE / pipeline targets
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "AtRisk", "PatientID", "w", "event", "tstop", "p_marg",
    "demo", "trt_map", "dat_cont", "dat_bin", "dat_tte",
    "ps_cont", "ps_bin", "ps_tte",
    "dat_cont_w", "dat_bin_w", "tte_ivl_w",
    "msm_cont", "msm_bin", "msm_cox",
    "summary_cont", "summary_bin", "summary_cox",
    "Compliant", "Treatment", "VisitNumber",
    "Age", "GenderMale", "SideEffect", "Biomarker",
    ".cluster_id"
  ))
}

#' @importFrom stats aggregate as.formula binomial gaussian predict quantile rbinom rnorm ave
#' @importFrom utils head
#' @importFrom recipes recipe step_dummy step_zv step_normalize all_nominal_predictors all_numeric_predictors all_predictors
#' @importFrom parsnip logistic_reg boost_tree rand_forest set_engine set_mode set_args fit
#' @importFrom workflows workflow add_model add_recipe
#' @importFrom rsample vfold_cv
#' @importFrom tune tune_grid control_grid select_best finalize_workflow
#' @importFrom yardstick metric_set roc_auc
#' @importFrom dials grid_regular mtry
NULL
