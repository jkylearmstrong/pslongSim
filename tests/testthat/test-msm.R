test_that("MSM fits (GEE and Cox) run end-to-end", {
  skip_on_cran()
  set.seed(3)
  demo <- generate_patient_data_demographic(num_patients = 60, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))

  # Continuous GEE
  dat_c <- generate_longitudinal_data(demo, num_visits = 4, trt_map = trt_map,
                                      adherence_type = "binary",
                                      outcome_type = "continuous")
  ps <- fit_ps_tidymodels(dat_c, model = "glm")
  dat_c$ps_hat <- ps$ps_hat
  dat_c_w <- compute_stabilized_iptw(dat_c)
  m1 <- fit_msm_gee_cont(dat_c_w)
  expect_s3_class(m1, "geeglm")

  # Binary GEE
  dat_b <- generate_longitudinal_data(demo, num_visits = 4, trt_map = trt_map,
                                      adherence_type = "binary",
                                      outcome_type = "binary")
  ps_b <- fit_ps_tidymodels(dat_b, model = "glm")
  dat_b$ps_hat <- ps_b$ps_hat
  dat_b_w <- compute_stabilized_iptw(dat_b)
  suppressWarnings(m_bin <- fit_msm_gee_bin(dat_b_w))
  expect_s3_class(m_bin, "geeglm")

  # Cox
  dat_tte <- generate_longitudinal_data(demo, num_visits = 5, trt_map = trt_map,
                                        adherence_type = "binary",
                                        outcome_type = "tte")
  ivl <- filter_at_risk(dat_tte$tte_intervals)
  ps2 <- fit_ps_tidymodels(ivl, model = "glm")
  ivl$ps_hat <- ps2$ps_hat
  ivl_w <- compute_stabilized_iptw(ivl)
  suppressWarnings(m2 <- fit_msm_cox(ivl_w))
  expect_s3_class(m2, "coxph")
})

test_that("compute_stabilized_iptw produces valid weights", {
  set.seed(5)
  demo <- generate_patient_data_demographic(num_patients = 40, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))
  dat <- generate_longitudinal_data(demo, num_visits = 4, trt_map = trt_map,
                                    adherence_type = "binary",
                                    outcome_type = "continuous")
  ps <- fit_ps_tidymodels(dat, model = "glm")
  dat$ps_hat <- ps$ps_hat
  dat_w <- compute_stabilized_iptw(dat)
  expect_true("w" %in% names(dat_w))
  expect_true(all(dat_w$w > 0))
  expect_true(all(is.finite(dat_w$w)))
})

test_that("compute_stabilized_iptw errors on missing columns", {
  df <- data.frame(a = 1:5, b = 1:5)
  expect_error(compute_stabilized_iptw(df), "missing required column")
})

test_that("compute_stabilized_iptw truncation can be disabled", {
  set.seed(5)
  demo <- generate_patient_data_demographic(num_patients = 40, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))
  dat <- generate_longitudinal_data(demo, num_visits = 4, trt_map = trt_map,
                                    adherence_type = "binary",
                                    outcome_type = "continuous")
  ps <- fit_ps_tidymodels(dat, model = "glm")
  dat$ps_hat <- ps$ps_hat
  dat_w <- compute_stabilized_iptw(dat, truncate = NULL)
  expect_true(all(dat_w$w > 0))
})

test_that("filter_at_risk works correctly", {
  df <- data.frame(AtRisk = c(1, 0, 1, 0), x = 1:4)
  result <- filter_at_risk(df)
  expect_equal(nrow(result), 2)
  expect_true(all(result$AtRisk == 1))
})

test_that("filter_at_risk errors on missing column", {
  expect_error(filter_at_risk(data.frame(x = 1:3)), "missing required column")
})

test_that("MSM functions error on missing columns", {
  expect_error(fit_msm_gee_cont(data.frame(a = 1)), "missing required column")
  expect_error(fit_msm_gee_bin(data.frame(a = 1)), "missing required column")
  expect_error(fit_msm_cox(data.frame(a = 1)), "missing required column")
})

test_that("GEE detects correct number of patient clusters", {
  skip_on_cran()
  set.seed(3)
  n_pat <- 60
  demo <- generate_patient_data_demographic(num_patients = n_pat, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))
  dat <- generate_longitudinal_data(demo, num_visits = 4, trt_map = trt_map,
                                    adherence_type = "binary",
                                    outcome_type = "continuous")
  ps <- fit_ps_tidymodels(dat, model = "glm")
  dat$ps_hat <- ps$ps_hat
  dat_w <- compute_stabilized_iptw(dat)
  m <- fit_msm_gee_cont(dat_w)
  expect_equal(length(m$geese$clusz), n_pat)
  expect_true(all(m$geese$clusz == 4))
})

test_that("compute_stabilized_iptw preserves row ordering", {
  set.seed(42)
  demo <- generate_patient_data_demographic(num_patients = 30, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))
  dat <- generate_longitudinal_data(demo, num_visits = 4, trt_map = trt_map,
                                    adherence_type = "binary",
                                    outcome_type = "continuous")
  original_ids <- dat$PatientID
  original_visits <- dat$VisitNumber
  ps <- fit_ps_tidymodels(dat, model = "glm")
  dat$ps_hat <- ps$ps_hat
  dat_w <- compute_stabilized_iptw(dat)
  expect_equal(dat_w$PatientID, original_ids)
  expect_equal(dat_w$VisitNumber, original_visits)
})

test_that("compute_stabilized_iptw produces cumulative weights", {
  set.seed(42)
  demo <- generate_patient_data_demographic(num_patients = 20, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))
  dat <- generate_longitudinal_data(demo, num_visits = 4, trt_map = trt_map,
                                    adherence_type = "binary",
                                    outcome_type = "continuous")
  ps <- fit_ps_tidymodels(dat, model = "glm")
  dat$ps_hat <- ps$ps_hat
  dat_w <- compute_stabilized_iptw(dat, truncate = NULL)
  for (pid in unique(dat_w$PatientID)) {
    pw <- dat_w$w[dat_w$PatientID == pid]
    pw <- pw[order(dat_w$VisitNumber[dat_w$PatientID == pid])]
    ratios <- pw[-1] / pw[-length(pw)]
    expect_true(all(ratios > 0), info = paste("Patient", pid, "weights not cumulative"))
    expect_true(all(is.finite(ratios)), info = paste("Patient", pid, "has infinite ratio"))
  }
})

test_that("compute_stabilized_iptw accepts id_var parameter", {
  set.seed(42)
  demo <- generate_patient_data_demographic(num_patients = 20, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))
  dat <- generate_longitudinal_data(demo, num_visits = 4, trt_map = trt_map,
                                    adherence_type = "binary",
                                    outcome_type = "continuous")
  ps <- fit_ps_tidymodels(dat, model = "glm")
  dat$ps_hat <- ps$ps_hat
  dat_w <- compute_stabilized_iptw(dat, id_var = "PatientID")
  expect_true("w" %in% names(dat_w))
  expect_true(all(dat_w$w > 0))
})

test_that("compute_stabilized_iptw handles factor compliance column", {
  set.seed(42)
  demo <- generate_patient_data_demographic(num_patients = 20, n_cohorts = 3)
  trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))
  dat <- generate_longitudinal_data(demo, num_visits = 4, trt_map = trt_map,
                                    adherence_type = "binary",
                                    outcome_type = "continuous")
  dat$Compliant <- factor(dat$Compliant)
  ps <- fit_ps_tidymodels(dat, model = "glm")
  dat$ps_hat <- ps$ps_hat
  dat_w <- compute_stabilized_iptw(dat)
  expect_true("w" %in% names(dat_w))
  expect_true(all(dat_w$w > 0))
})
