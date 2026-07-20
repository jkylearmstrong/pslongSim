#' Build a Propensity Score Recipe
#'
#' Constructs a \pkg{recipes} preprocessing pipeline for propensity score
#' estimation.  The outcome column is converted to a factor (required for
#' binary classification), dummy-encoded, zero-variance predictors are
#' removed, and numeric predictors are normalised.
#'
#' Note: the estimation functions in \pkg{pslongSim} are intentionally simple
#' \emph{baseline/reference estimators} used to benchmark simulation output.
#' For advanced ML propensity score estimation (cross-fitting, clipping,
#' twang, diagnostics), use the \pkg{MLPScore} package.
#'
#' @param df Data frame containing the outcome and predictor columns.
#' @param outcome Character: name of the binary outcome column.
#' @param predictors Character vector of predictor column names.  When
#'   \code{NULL} (the default), a standard set is used
#'   (\code{Treatment}, \code{VisitNumber}, \code{Age}, \code{GenderMale},
#'   \code{SideEffect}, \code{Biomarker}).
#' @return A \code{recipes::recipe} object.
#' @examples
#' demo <- generate_patient_data_demographic(num_patients = 50, seed = 1)
#' trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))
#' dat <- generate_longitudinal_data(demo, num_visits = 4, trt_map = trt_map)
#' rec <- ps_recipe(dat)
#' rec
#' @export
ps_recipe <- function(df,
                      outcome = "Compliant",
                      predictors = NULL) {
  stopifnot(
    "`outcome` must be a single character string." = is.character(outcome) && length(outcome) == 1L,
    "`outcome` must be a column in `df`." = outcome %in% names(df)
  )

  if (is.null(predictors)) {
    predictors <- c("Treatment", "VisitNumber", "Age", "GenderMale",
                    "SideEffect", "Biomarker")
  }
  missing_preds <- setdiff(predictors, names(df))
  if (length(missing_preds) > 0L) {
    stop("ps_recipe: missing predictor column(s): ",
         paste(missing_preds, collapse = ", "))
  }

  if (!is.factor(df[[outcome]])) {
    df[[outcome]] <- as.factor(df[[outcome]])
  }

  formula <- stats::as.formula(
    paste(outcome, "~", paste(predictors, collapse = " + "))
  )

  recipes::recipe(formula, data = df) |>
    recipes::step_dummy(recipes::all_nominal_predictors()) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors())
}

#' Model Specification Factory for Propensity Scores
#'
#' Returns a \pkg{parsnip} model specification for one of three PS engines.
#'
#' @param model One of \code{"glm"}, \code{"xgboost"}, or \code{"ranger"}.
#' @param tune Logical; if \code{TRUE}, tuning placeholders are added for
#'   \code{mtry} (relevant for \code{"xgboost"} and \code{"ranger"}).
#' @return A \code{parsnip} model specification.
#' @examples
#' # GLM specification (no extra dependencies)
#' spec <- ps_model_spec("glm")
#'
#' # Ranger specification (requires ranger package)
#' if (requireNamespace("ranger", quietly = TRUE)) {
#'   spec_rf <- ps_model_spec("ranger")
#' }
#' @export
ps_model_spec <- function(model = c("glm", "xgboost", "ranger"), tune = FALSE) {
  model <- match.arg(model)
  if (model == "xgboost") {
    require_suggested("xgboost")
    spec <- parsnip::boost_tree(trees = 500, learn_rate = 0.05, tree_depth = 4)
    if (tune) spec <- parsnip::set_args(spec, mtry = tune::tune())
    spec |>
      parsnip::set_mode("classification") |>
      parsnip::set_engine("xgboost")
  } else if (model == "ranger") {
    require_suggested("ranger")
    spec <- parsnip::rand_forest(trees = 1000, min_n = 10)
    if (tune) spec <- parsnip::set_args(spec, mtry = tune::tune())
    spec |>
      parsnip::set_mode("classification") |>
      parsnip::set_engine("ranger", probability = TRUE)
  } else {
    parsnip::logistic_reg() |>
      parsnip::set_engine("glm")
  }
}

#' Fit a Propensity Score Model via Tidymodels
#'
#' Fits a binary classification model for the probability of compliance
#' (propensity score) using a \pkg{tidymodels} workflow.
#'
#' @param df Data frame containing the outcome and predictor columns.
#' @param model One of \code{"glm"}, \code{"xgboost"}, or \code{"ranger"}.
#' @param outcome Character: name of the binary outcome column.
#' @param predictors Character vector of predictor column names (passed to
#'   \code{ps_recipe()}).  Use \code{NULL} for the default set.
#' @param tune_ps If \code{TRUE}, performs tuning with
#'   \code{rsample::vfold_cv} and \code{tune::tune_grid}.
#' @param resamples Optional \code{rsample} rset object; defaults to 5-fold
#'   cross-validation stratified on the outcome.
#' @param grid Integer grid size or a \pkg{dials} grid object.
#' @return A list with elements \code{workflow}, \code{fit}, and
#'   \code{ps_hat} (numeric vector of propensity scores).
#' @seealso \code{\link{ps_recipe}}, \code{\link{ps_model_spec}},
#'   \code{\link{compute_stabilized_iptw}}
#' @examples
#' demo <- generate_patient_data_demographic(num_patients = 80, seed = 1)
#' trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))
#' dat <- generate_longitudinal_data(demo, num_visits = 4, trt_map = trt_map)
#' ps <- fit_ps_tidymodels(dat, model = "glm")
#' range(ps$ps_hat)
#' @export
fit_ps_tidymodels <- function(df,
                               model = c("glm", "xgboost", "ranger"),
                               outcome = "Compliant",
                               predictors = NULL,
                               tune_ps = FALSE,
                               resamples = NULL,
                               grid = 20L) {
  model <- match.arg(model)

  validate_columns(df, outcome, "fit_ps_tidymodels")
  if (!is.factor(df[[outcome]])) {
    df[[outcome]] <- as.factor(df[[outcome]])
  }
  n_levels <- nlevels(df[[outcome]])
  if (n_levels != 2L) {
    stop("fit_ps_tidymodels: `outcome` must have exactly 2 levels, got ", n_levels, ".")
  }
  validate_min_rows(df, 10L, "fit_ps_tidymodels")

  rec  <- ps_recipe(df, outcome = outcome, predictors = predictors)
  spec <- ps_model_spec(model, tune = tune_ps)
  wf   <- workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_recipe(rec)

  if (identical(model, "glm") || !tune_ps) {
    fit <- parsnip::fit(wf, data = df)
  } else {
    if (is.null(resamples)) {
      resamples <- rsample::vfold_cv(df, v = 5, strata = outcome)
    }
    
    # Pre-bake recipe to determine number of predictors for mtry finalization
    rec_prepped <- recipes::prep(rec, training = df)
    baked_df <- recipes::bake(rec_prepped, new_data = NULL)
    num_preds <- ncol(baked_df) - 1L
    mtry_param <- dials::mtry(c(1L, as.integer(num_preds)))

    tune_res <- tune::tune_grid(
      wf, resamples = resamples,
      grid = if (is.numeric(grid)) {
        dials::grid_regular(mtry_param, levels = grid)
      } else {
        grid
      },
      control = tune::control_grid(save_pred = TRUE, verbose = FALSE),
      metrics = yardstick::metric_set(yardstick::roc_auc)
    )
    best <- tune::select_best(tune_res, "roc_auc")
    fit  <- tune::finalize_workflow(wf, parameters = best)
    fit  <- parsnip::fit(fit, data = df)
  }

  preds <- predict(fit, new_data = df, type = "prob")
  # Dynamically extract the probability column for the second factor level
  # (the "event" / "compliant" class).  tidymodels names columns as
  # .pred_<level>; we find the column matching the second level name.
  level2 <- levels(df[[outcome]])[2L]
  standard_positives <- c("1", "TRUE", "yes", "Yes", "Y", "Compliant", "Success", "compliant", "true")
  if (!(level2 %in% standard_positives)) {
    warning("fit_ps_tidymodels: modeling probability of the second factor level ('", level2, 
            "'). Ensure this represents the target event.", call. = FALSE)
  }
  pred_col <- paste0(".pred_", level2)
  if (!pred_col %in% names(preds)) {
    # Fallback: take the last .pred_* column
    pred_cols <- grep("^\\.pred_", names(preds), value = TRUE)
    pred_col  <- pred_cols[length(pred_cols)]
  }
  ps_hat <- as.numeric(preds[[pred_col]])

  list(workflow = wf, fit = fit, ps_hat = ps_hat)
}
