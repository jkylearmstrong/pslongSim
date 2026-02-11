# install.packages(c("targets","tarchetypes"))
library(targets)
library(tarchetypes)
tar_option_set(packages = c(
  "pslongSim", "dplyr", "recipes", "parsnip", "workflows",
  "geepack", "survival", "rsample", "tune", "yardstick"
))

# Parameters
n_patients <- 300L
n_visits   <- 6L
seed_base  <- 10L

list(
  tar_target(demo,
             generate_patient_data_demographic(num_patients = n_patients, n_cohorts = 3, seed = seed_base)
  ),
  tar_target(trt_map, {
    levels <- levels(demo$Treatment)
    define_treatment_map(
      levels = levels,
      adh_shift = c(Treatment_1=0, Treatment_2=-0.5, Treatment_3=-0.9),
      out_effect= c(Treatment_1=0, Treatment_2= 0.3, Treatment_3= 0.7),
      logHR     = c(Treatment_1=0, Treatment_2=-0.2, Treatment_3=-0.5),
      se_shift  = c(Treatment_1=0, Treatment_2= 0.2, Treatment_3= 0.4)
    )
  }),
  # Continuous outcome dataset
  tar_target(dat_cont,
             generate_longitudinal_data(
               patient_data = demo, num_visits = n_visits, seed = seed_base + 1,
               trt_map = trt_map, adherence_type = "beta", outcome_type = "continuous"
             )
  ),
  # Binary outcome dataset
  tar_target(dat_bin,
             generate_longitudinal_data(
               patient_data = demo, num_visits = n_visits, seed = seed_base + 2,
               trt_map = trt_map, adherence_type = "binary", outcome_type = "binary"
             )
  ),
  # TTE dataset
  tar_target(dat_tte,
             generate_longitudinal_data(
               patient_data = demo, num_visits = 8, seed = seed_base + 3,
               trt_map = trt_map, adherence_type = "binary", outcome_type = "tte"
             )
  ),
  # PS fits
  tar_target(ps_cont, fit_ps_tidymodels(dat_cont, model = "glm")),
  tar_target(ps_bin,  fit_ps_tidymodels(dat_bin,  model = "ranger", tune_ps = FALSE)),
  tar_target(ps_tte,  fit_ps_tidymodels(subset(dat_tte$tte_intervals, AtRisk == 1), model = "xgboost", tune_ps = FALSE)),

  # Add PS and compute stabilized IPTW
  tar_target(dat_cont_w, {
    df <- dat_cont; df$ps_hat <- ps_cont$ps_hat
    compute_stabilized_iptw(df)
  }),
  tar_target(dat_bin_w, {
    df <- dat_bin; df$ps_hat <- ps_bin$ps_hat
    compute_stabilized_iptw(df)
  }),
  tar_target(tte_ivl_w, {
    ivl <- subset(dat_tte$tte_intervals, AtRisk == 1)
    ivl$ps_hat <- ps_tte$ps_hat
    compute_stabilized_iptw(ivl)
  }),

  # MSM fits
  tar_target(msm_cont, fit_msm_gee_cont(dat_cont_w)),
  tar_target(msm_bin,  fit_msm_gee_bin(dat_bin_w)),
  tar_target(msm_cox,  fit_msm_cox(tte_ivl_w)),

  # Summaries
  tar_target(summary_cont, summary(msm_cont)),
  tar_target(summary_bin,  summary(msm_bin)),
  tar_target(summary_cox,  summary(msm_cox))
)
