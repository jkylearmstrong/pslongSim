test_that("tidymodels PS runs and returns probabilities", {
  skip_on_cran()
  set.seed(2)
  demo <- generate_patient_data_demographic(num_patients = 80, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels = levels(demo$Treatment)))
  dat <- generate_longitudinal_data(demo, num_visits = 5, trt_map = trt_map,
                                    adherence_type = "binary",
                                    outcome_type = "binary")
  fit <- fit_ps_tidymodels(dat, model = "glm")
  expect_length(fit$ps_hat, nrow(dat))
  expect_true(all(fit$ps_hat > 0 & fit$ps_hat < 1))
})

test_that("ps_recipe accepts custom outcome and predictors", {
  df <- data.frame(
    Y = factor(c(0, 1, 0, 1)),
    X1 = c(1, 2, 3, 4),
    X2 = c(0, 1, 0, 1)
  )
  rec <- ps_recipe(df, outcome = "Y", predictors = c("X1", "X2"))
  expect_s3_class(rec, "recipe")
})

test_that("ps_recipe errors on missing outcome column", {
  df <- data.frame(a = 1:3)
  expect_error(ps_recipe(df, outcome = "missing_col"),
               "must be a column in")
})

test_that("ps_recipe errors on missing predictor columns", {
  df <- data.frame(Compliant = factor(c(0, 1)), Age = c(20, 30))
  expect_error(ps_recipe(df, predictors = c("Age", "MissingCol")),
               "missing predictor")
})

test_that("fit_ps_tidymodels validates inputs", {
  df <- data.frame(a = 1:5)
  expect_error(fit_ps_tidymodels(df, outcome = "nonexistent"),
               "missing required column")
})

test_that("fit_ps_tidymodels errors on non-binary outcome", {
  df <- data.frame(
    Compliant = factor(c("A", "B", "C")),
    Treatment = factor(c("T1", "T2", "T1")),
    VisitNumber = 1:3, Age = c(30, 40, 50),
    GenderMale = c(1, 0, 1), SideEffect = c(0, 1, 0),
    Biomarker = c(1.0, 0.5, -0.3)
  )
  expect_error(fit_ps_tidymodels(df), "exactly 2 levels")
})

test_that("ps_model_spec produces valid specs for all models", {
  expect_s3_class(ps_model_spec("glm"), "logistic_reg")
  skip_if_not_installed("xgboost")
  expect_s3_class(ps_model_spec("xgboost"), "boost_tree")
  skip_if_not_installed("ranger")
  expect_s3_class(ps_model_spec("ranger"), "rand_forest")
})

test_that("ps_model_spec tune parameter works", {
  skip_if_not_installed("xgboost")
  spec_tuned <- ps_model_spec("xgboost", tune = TRUE)
  expect_s3_class(spec_tuned, "boost_tree")
})

test_that("factor conversion happens for integer outcome", {
  skip_on_cran()
  df <- data.frame(
    Compliant = c(0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L),
    Treatment = factor(c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0)),
    VisitNumber = 1:12, Age = 30:41,
    GenderMale = rep(c(1, 0), 6),
    SideEffect = rep(c(0, 1), 6),
    Biomarker = rnorm(12)
  )
  fit <- fit_ps_tidymodels(df, model = "glm")
  expect_length(fit$ps_hat, 12)
  expect_s3_class(fit$fit, "workflow")
})

test_that("dynamic .pred column extraction works for named levels", {
  skip_on_cran()
  df <- data.frame(
    Compliant = factor(c("No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes")),
    Treatment = factor(c("T1", "T2", "T1", "T2", "T1", "T2", "T1", "T2", "T1", "T2", "T1", "T2")),
    VisitNumber = 1:12, Age = 30:41,
    GenderMale = rep(c(1, 0), 6),
    SideEffect = rep(c(0, 1), 6),
    Biomarker = rnorm(12)
  )
  fit <- fit_ps_tidymodels(df, model = "glm")
  expect_length(fit$ps_hat, 12)
  expect_true(all(fit$ps_hat > 0 & fit$ps_hat < 1))
})
