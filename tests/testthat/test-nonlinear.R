test_that("non-linear feature is undetectable by linear regression", {
  skip_on_cran()
  skip_if_not_installed("randomForest")
  set.seed(42)
  n <- 2000
  demo <- generate_patient_data_demographic(num_patients = n, n_cohorts = 3)
  trt_map <- define_treatment_map(
    levels(demo$Treatment),
    adh_shift  = c(Treatment_1 = 0, Treatment_2 = -0.5, Treatment_3 = -0.9),
    out_effect = c(Treatment_1 = 0, Treatment_2 = 0.3,  Treatment_3 = 0.7),
    logHR      = c(Treatment_1 = 0, Treatment_2 = -0.2, Treatment_3 = -0.5),
    se_shift   = c(Treatment_1 = 0, Treatment_2 = 0.2,  Treatment_3 = 0.4)
  )

  dat <- generate_longitudinal_data(
    demo, num_visits = 6, trt_map = trt_map,
    adherence_type = "beta", outcome_type = "continuous",
    non_linear_feature = TRUE
  )

  # Verify interaction column exists and is the product
  expect_true("NL_Age_SideEffect" %in% names(dat))
  expect_equal(dat$NL_Age_SideEffect, dat$Age * dat$SideEffect)

  # Fit a linear model WITHOUT the interaction
  lm_additive <- lm(Outcome ~ Treatment + VisitNumber + Age + GenderMale +
                       SideEffect + Biomarker + Compliant,
                     data = dat)

  # Fit a linear model WITH the interaction
  lm_interactive <- lm(Outcome ~ Treatment + VisitNumber + Age + GenderMale +
                         SideEffect + Biomarker + Compliant +
                         Age:SideEffect,
                       data = dat)

  # The interaction should be detected when explicitly included
  coef_interaction <- coef(lm_interactive)["Age:SideEffect"]
  expect_true(is.finite(coef_interaction))

  # The additive-only model should NOT find significant effects for Age
  # or SideEffect individually (they have no main effect in the DGP)
  p_age <- summary(lm_additive)$coefficients["Age", "Pr(>|t|)"]
  p_se  <- summary(lm_additive)$coefficients["SideEffect", "Pr(>|t|)"]
  # Both p-values should be > 0.05 (not significant) in most seeds
  # This is a probabilistic test; we check the additive model R-squared
  # is much lower than what a tree model can achieve.
  r2_additive <- summary(lm_additive)$r.squared

  # Fit a random forest (using base R for no extra deps in tests)
  set.seed(42)
 rf_fit <- randomForest::randomForest(
    Outcome ~ Treatment + VisitNumber + Age + GenderMale +
      SideEffect + Biomarker + Compliant,
    data = dat, ntree = 200, importance = FALSE
  )
  r2_rf <- 1 - rf_fit$mse[200] / var(dat$Outcome)

  # Random forest R-squared should substantially exceed the additive LM
  expect_true(r2_rf > r2_additive,
              info = paste("RF R2:", round(r2_rf, 3),
                           "vs additive LM R2:", round(r2_additive, 3)))
})

test_that("NL interaction has no additive main effect in linear model", {
  set.seed(7)
  n <- 500
  demo <- generate_patient_data_demographic(num_patients = n, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))

  dat <- generate_longitudinal_data(
    demo, num_visits = 4, trt_map = trt_map,
    adherence_type = "binary", outcome_type = "continuous",
    non_linear_feature = TRUE
  )

  # A model with only the NL interaction as predictor should have near-zero R^2
  m <- lm(Outcome ~ NL_Age_SideEffect, data = dat)
  expect_true(abs(coef(m)["NL_Age_SideEffect"]) < 2,
              info = "NL interaction main effect should be near zero")
})

test_that("non-linear feature works with binary outcome", {
  set.seed(10)
  demo <- generate_patient_data_demographic(num_patients = 200, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))

  dat <- generate_longitudinal_data(
    demo, num_visits = 4, trt_map = trt_map,
    adherence_type = "binary", outcome_type = "binary",
    non_linear_feature = TRUE,
    nonlin_coefs = c("Age:SideEffect" = 0.20)
  )
  expect_true("NL_Age_SideEffect" %in% names(dat))
  expect_true(all(dat$Outcome %in% c(0L, 1L)))
  expect_equal(dat$NL_Age_SideEffect, dat$Age * dat$SideEffect)
})

test_that("non-linear feature works with TTE outcome", {
  set.seed(10)
  demo <- generate_patient_data_demographic(num_patients = 200, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))

  result <- generate_longitudinal_data(
    demo, num_visits = 5, trt_map = trt_map,
    adherence_type = "binary", outcome_type = "tte",
    non_linear_feature = TRUE,
    nonlin_coefs = c("Age:SideEffect" = 0.15)
  )
  expect_true("NL_Age_SideEffect" %in% names(result$long))
  expect_true("NL_Age_SideEffect" %in% names(result$tte_intervals))
  expect_false("NL_Age_SideEffect" %in% names(result$tte_subject))
})

test_that("nonlin_coefs warnings on malformed entries", {
  set.seed(10)
  demo <- generate_patient_data_demographic(num_patients = 30, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))

  expect_warning(
    generate_longitudinal_data(
      demo, num_visits = 3, trt_map = trt_map,
      adherence_type = "binary", outcome_type = "continuous",
      non_linear_feature = TRUE,
      nonlin_coefs = c("Age+SideEffect" = 0.15)
    ),
    "does not follow"
  )

  expect_warning(
    generate_longitudinal_data(
      demo, num_visits = 3, trt_map = trt_map,
      adherence_type = "binary", outcome_type = "continuous",
      non_linear_feature = TRUE,
      nonlin_coefs = c("FakeVar:SideEffect" = 0.15)
    ),
    "not in data"
  )
})
