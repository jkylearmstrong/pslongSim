test_that("tidymodels PS runs and returns probabilities", {
  set.seed(2)
  demo <- generate_patient_data_demographic(num_patients = 80, n_cohorts = 3)
  trt_levels <- levels(demo$Treatment)
  trt_map <- define_treatment_map(trt_levels)
  dat <- generate_longitudinal_data(demo, num_visits = 5, trt_map = trt_map,
                                    adherence_type = "binary",
                                    outcome_type = "binary")
  fit <- fit_ps_tidymodels(dat, model = "glm")
  expect_true(length(fit$ps_hat) == nrow(dat))
  expect_true(all(fit$ps_hat > 0 & fit$ps_hat < 1))
})

test_that("ps_recipe converts outcome to factor", {
  df <- data.frame(Compliant = c(0L, 1L, 0L), Treatment = c(1, 0, 1), 
                   VisitNumber = 1, Age = 50, GenderMale = 1, 
                   SideEffect = 0, Biomarker = 0.5)
  # fit_ps_tidymodels is expected to convert to factor.
  # We check if the fit object can successfully predict, 
  # which requires the factor outcome for classification.
  fit <- fit_ps_tidymodels(df, model = "glm")
  expect_true(is.factor(fit$fit$fit$data$Compliant) || 
              is.factor(fit$fit$pre$mold$outcomes$Compliant))
})

test_that("ps_model_spec and fit_ps_tidymodels work for xgboost without tuning", {
  df <- data.frame(Compliant = factor(c("0", "1", "0")), Treatment = c(1, 0, 1), 
                   VisitNumber = 1, Age = 50, GenderMale = 1, 
                   SideEffect = 0, Biomarker = 0.5)
  # This should run without the "tune() placeholder" error
  fit <- fit_ps_tidymodels(df, model = "xgboost", tune_ps = FALSE)
  expect_true(length(fit$ps_hat) == 3)
})
