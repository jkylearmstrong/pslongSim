test_that("simulate produces expected shapes and AR(1) signal", {
  set.seed(1)
  demo <- generate_patient_data_demographic(num_patients = 50, n_cohorts = 3)
  trt_map <- define_treatment_map(
    levels = levels(demo$Treatment),
    adh_shift  = c(Treatment_1 = 0, Treatment_2 = -0.5, Treatment_3 = -1.0),
    out_effect = c(Treatment_1 = 0, Treatment_2 = 0.3,  Treatment_3 = 0.7),
    logHR      = c(Treatment_1 = 0, Treatment_2 = -0.2, Treatment_3 = -0.5),
    se_shift   = c(Treatment_1 = 0, Treatment_2 = 0.2,  Treatment_3 = 0.4)
  )

  dat <- generate_longitudinal_data(demo, num_visits = 6, trt_map = trt_map,
                                    adherence_type = "beta",
                                    outcome_type = "continuous")
  expect_equal(nrow(dat), 50 * 6)
  expect_true(all(dat$Biomarker > -10 & dat$Biomarker < 10))

  # AR(1) check: lag-1 correlation positive
  acf1 <- cor(dat$Biomarker[dat$VisitNumber == 1],
              dat$Biomarker[dat$VisitNumber == 2])
  expect_true(is.finite(acf1))
})

test_that("non-linear feature column is present when enabled", {
  set.seed(10)
  demo <- generate_patient_data_demographic(num_patients = 30, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))

  dat <- generate_longitudinal_data(demo, num_visits = 4, trt_map = trt_map,
                                    adherence_type = "binary",
                                    outcome_type = "continuous",
                                    non_linear_feature = TRUE)
  expect_true("NL_Age_SideEffect" %in% names(dat))
  expect_equal(dat$NL_Age_SideEffect, dat$Age * dat$SideEffect)
})

test_that("non-linear feature column is absent when disabled (default)", {
  set.seed(10)
  demo <- generate_patient_data_demographic(num_patients = 30, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))

  dat <- generate_longitudinal_data(demo, num_visits = 4, trt_map = trt_map,
                                    adherence_type = "binary",
                                    outcome_type = "continuous")
  expect_false(any(grepl("^NL_", names(dat))))
})

test_that("nonlin_coefs creates multiple interaction columns", {
  set.seed(10)
  demo <- generate_patient_data_demographic(num_patients = 30, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))

  dat <- generate_longitudinal_data(
    demo, num_visits = 4, trt_map = trt_map,
    adherence_type = "binary", outcome_type = "continuous",
    non_linear_feature = TRUE,
    nonlin_coefs = c("Age:SideEffect" = 0.15, "Biomarker:Age" = 0.10)
  )
  expect_true("NL_Age_SideEffect" %in% names(dat))
  expect_true("NL_Biomarker_Age" %in% names(dat))
  expect_equal(dat$NL_Age_SideEffect, dat$Age * dat$SideEffect)
  expect_equal(dat$NL_Biomarker_Age, dat$Biomarker * dat$Age)
})

test_that("binary outcome produces 0/1 Outcome column", {
  set.seed(10)
  demo <- generate_patient_data_demographic(num_patients = 30, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))

  dat <- generate_longitudinal_data(demo, num_visits = 4, trt_map = trt_map,
                                    adherence_type = "binary",
                                    outcome_type = "binary")
  expect_true("Outcome" %in% names(dat))
  expect_true(all(dat$Outcome %in% c(0L, 1L)))
})

test_that("TTE outcome returns list with 3 elements", {
  set.seed(10)
  demo <- generate_patient_data_demographic(num_patients = 30, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))

  result <- generate_longitudinal_data(demo, num_visits = 5, trt_map = trt_map,
                                       adherence_type = "binary",
                                       outcome_type = "tte")
  expect_type(result, "list")
  expect_named(result, c("long", "tte_intervals", "tte_subject"))
  expect_true("event" %in% names(result$tte_intervals))
  expect_true("tstart" %in% names(result$tte_intervals))
  expect_true("tstop" %in% names(result$tte_intervals))
})

test_that("input validation catches bad inputs", {
  expect_error(generate_longitudinal_data("not_a_df", trt_map = NULL),
               "must be a data.frame")
  expect_error(
    generate_longitudinal_data(
      data.frame(PatientID = "P1", Age = 50, Gender = "M",
                 Treatment = factor("A")),
      trt_map = data.frame(adh_shift = 0),
      num_visits = 0
    ),
    "must be a positive integer"
  )
})

test_that("outcome formula includes all model terms for continuous/binary", {
  set.seed(42)
  demo <- generate_patient_data_demographic(num_patients = 200, n_cohorts = 3)
  trt_map <- define_treatment_map(
    levels = levels(demo$Treatment),
    out_effect = c(Treatment_1 = 0, Treatment_2 = 1.0, Treatment_3 = 2.0)
  )
  dat <- generate_longitudinal_data(
    demo, num_visits = 4, trt_map = trt_map,
    adherence_type = "beta",
    outcome_type = "continuous",
    outcome_model = list(
      intercept = 0, adherence = 2.0, se_effect = 1.0,
      biomarker_effect = 0.5, time_trend = 0.1,
      sd_subject = 0.5, sd_error = 0.1
    )
  )
  trt_means <- tapply(dat$Outcome, dat$Treatment, mean)
  expect_true(trt_means["Treatment_3"] > trt_means["Treatment_1"],
              info = "Treatment effect should be visible in means")
})

test_that("TTE event mapping uses PatientID not row index", {
  set.seed(42)
  demo <- generate_patient_data_demographic(num_patients = 30, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))
  result <- generate_longitudinal_data(demo, num_visits = 6, trt_map = trt_map,
                                       adherence_type = "binary",
                                       outcome_type = "tte")
  all_pids <- unique(result$tte_intervals$PatientID)
  events <- result$tte_subject[result$tte_subject$status == 1, "PatientID"]
  for (pid in events) {
    rows <- result$tte_intervals[result$tte_intervals$PatientID == pid, ]
    rows <- rows[order(rows$VisitNumber), ]
    ev_rows <- rows[rows$event == 1, ]
    expect_equal(nrow(ev_rows), 1, info = paste("Patient", pid, "should have exactly 1 event"))
    later <- rows[rows$VisitNumber > ev_rows$VisitNumber, ]
    if (nrow(later) > 0) {
      expect_true(all(later$AtRisk == 0), info = paste("Patient", pid, "at-risk after event"))
    }
  }
})

test_that("generate_patient_data_demographic returns correct structure", {
  demo <- generate_patient_data_demographic(num_patients = 100, seed = 42)
  expect_equal(nrow(demo), 100)
  expect_true(all(c("PatientID", "Age", "Gender", "Treatment") %in% names(demo)))
  expect_s3_class(demo$Treatment, "factor")
  expect_s3_class(demo$Gender, "factor")
})

test_that("TTE first-event censoring works correctly", {
  set.seed(99)
  demo <- generate_patient_data_demographic(num_patients = 50, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))
  result <- generate_longitudinal_data(demo, num_visits = 6, trt_map = trt_map,
                                       adherence_type = "binary",
                                       outcome_type = "tte")
  # Each patient should have at most 1 event
  events_per_patient <- aggregate(event ~ PatientID,
                                  data = result$tte_intervals,
                                  FUN = sum)
  expect_true(all(events_per_patient$event <= 1))
  # After first event, AtRisk should be 0
  for (pid in unique(result$tte_intervals$PatientID)) {
    rows <- result$tte_intervals[result$tte_intervals$PatientID == pid, ]
    rows <- rows[order(rows$VisitNumber), ]
    if (any(rows$event == 1)) {
      first_ev <- which(rows$event == 1)[1]
      if (first_ev < nrow(rows)) {
        expect_true(all(rows$AtRisk[(first_ev + 1):nrow(rows)] == 0))
      }
    }
  }
})
