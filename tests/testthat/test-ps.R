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
