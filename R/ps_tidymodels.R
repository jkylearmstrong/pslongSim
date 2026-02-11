#' Build a PS Recipe
#' @param df Data frame.
#' @param outcome Name of binary outcome column ("Compliant").
#' @param formula Predictors formula (default includes Treatment, time, demos, SE, Biomarker).
#' @export
ps_recipe <- function(df,
                      outcome = "Compliant",
                      formula = as.formula("Compliant ~ Treatment + VisitNumber + Age + GenderMale + SideEffect + Biomarker")) {
  stopifnot(outcome %in% names(df))
  # Classification models require the outcome to be a factor
  if (!is.factor(df[[outcome]])) {
    df[[outcome]] <- as.factor(df[[outcome]])
  }
  recipes::recipe(formula, data = df) |>
    recipes::step_dummy(recipes::all_nominal_predictors()) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors())
}

#' Model spec factory for PS
#' @param model One of "glm","xgboost","ranger".
#' @export
ps_model_spec <- function(model = c("glm","xgboost","ranger")) {
  model <- match.arg(model)
  if (model == "glm") {
    parsnip::logistic_reg() |>
      parsnip::set_engine("glm")
  } else if (model == "xgboost") {
    parsnip::boost_tree(trees = 500, learn_rate = 0.05, mtry = tune::tune(), tree_depth = 4) |>
      parsnip::set_mode("classification") |>
      parsnip::set_engine("xgboost")
  } else {
    parsnip::rand_forest(trees = 1000, mtry = tune::tune(), min_n = 10) |>
      parsnip::set_mode("classification") |>
      parsnip::set_engine("ranger", probability = TRUE)
  }
}

#' Fit PS with Tidymodels
#' @param df Data frame containing outcome and predictors.
#' @param model "glm","xgboost","ranger".
#' @param tune_ps If TRUE, performs tuning with rsample vfold_cv and tune_grid.
#' @param resamples Optional rsample object; default 5-fold vfold.
#' @param grid Optional grid size (integer) or dials grid.
#' @return A list: workflow, fitted model, predictions (ps_hat).
#' @export
fit_ps_tidymodels <- function(df,
                              model = c("glm","xgboost","ranger"),
                              tune_ps = FALSE,
                              resamples = NULL,
                              grid = 20L) {
  model <- match.arg(model)
  # Ensure outcome is a factor for classification
  outcome <- "Compliant"
  if (!is.factor(df[[outcome]])) {
    df[[outcome]] <- as.factor(df[[outcome]])
  }
  rec <- ps_recipe(df)
  spec <- ps_model_spec(model)
  wf <- workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_recipe(rec)

  if (identical(model, "glm") || !tune_ps) {
    fit <- parsnip::fit(wf, data = df)
  } else {
    if (is.null(resamples)) {
      resamples <- rsample::vfold_cv(df, v = 5, strata = "Compliant")
    }
    tune_res <- tune::tune_grid(
      wf, resamples = resamples,
      grid = if (is.numeric(grid)) tune::grid_regular(levels = grid) else grid,
      control = tune::control_grid(save_pred = TRUE, verbose = FALSE),
      metrics = yardstick::metric_set(yardstick::roc_auc)
    )
    fit <- tune::select_best(tune_res, "roc_auc") |>
      tune::finalize_workflow(wf, parameters = _)
    fit <- parsnip::fit(fit, data = df)
  }

  # Predict propensity score (P(Compliant=1))
  # Ensure the data passed to predict also has the factor outcome
  preds <- predict(fit, new_data = df, type = "prob")
  ps_hat <- as.numeric(preds$.pred_1)
  list(workflow = wf, fit = fit, ps_hat = ps_hat)
}
