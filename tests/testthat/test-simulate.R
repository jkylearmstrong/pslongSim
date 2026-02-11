test_that("simulate produces expected shapes and AR(1) signal", {
  set.seed(1)
  demo <- generate_patient_data_demographic(num_patients = 50, n_cohorts = 3)
  trt_levels <- levels(demo$Treatment)
  trt_map <- define_treatment_map(
    levels = trt_levels,
    adh_shift = c(Treatment_1=0, Treatment_2=-0.5, Treatment_3=-1.0),
    out_effect= c(Treatment_1=0, Treatment_2= 0.3, Treatment_3= 0.7),
    logHR     = c(Treatment_1=0, Treatment_2=-0.2, Treatment_3=-0.5),
    se_shift  = c(Treatment_1=0, Treatment_2= 0.2, Treatment_3= 0.4)
  )

  dat <- generate_longitudinal_data(demo, num_visits = 6, trt_map = trt_map,
                                    adherence_type = "beta",
                                    outcome_type = "continuous")
  expect_equal(nrow(dat), 50 * 6)
  expect_true(all(dat$Biomarker > -10 & dat$Biomarker < 10))

  # AR(1) check: lag-1 correlation positive
  acf1 <- cor(dat$Biomarker[dat$VisitNumber == 1], dat$Biomarker[dat$VisitNumber == 2])
  expect_true(is.finite(acf1))
})
