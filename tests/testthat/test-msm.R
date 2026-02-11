test_that("MSM fits (GEE and Cox) run", {
  skip_on_cran()
  set.seed(3)
  demo <- generate_patient_data_demographic(num_patients = 60, n_cohorts = 3)
  trt_map <- define_treatment_map(levels(demo$Treatment))
  dat_c <- generate_longitudinal_data(demo, num_visits = 4, trt_map = trt_map,
                                      adherence_type = "binary",
                                      outcome_type = "continuous")
  ps <- fit_ps_tidymodels(dat_c, model = "glm")
  dat_c$ps_hat <- ps$ps_hat
  dat_c_w <- compute_stabilized_iptw(dat_c)
  m1 <- fit_msm_gee_cont(dat_c_w)
  expect_s3_class(m1, "geeglm")

  dat_tte <- generate_longitudinal_data(demo, num_visits = 5, trt_map = trt_map,
                                        adherence_type = "binary",
                                        outcome_type = "tte")
  ivl <- subset(dat_tte$tte_intervals, AtRisk == 1)
  ps2 <- fit_ps_tidymodels(ivl, model = "glm")
  ivl$ps_hat <- ps2$ps_hat
  ivl_w <- compute_stabilized_iptw(ivl)
  m2 <- fit_msm_cox(ivl_w)
  expect_s3_class(m2, "coxph")
})
