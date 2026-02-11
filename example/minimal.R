library(pslongSim)

set.seed(1)
demo <- generate_patient_data_demographic(num_patients = 400, n_cohorts = 3)
trt_map <- define_treatment_map(
  levels(demo$Treatment),
  adh_shift = c(Treatment_1=0, Treatment_2=-0.5, Treatment_3=-0.9),
  out_effect= c(Treatment_1=0, Treatment_2= 0.3, Treatment_3= 0.7),
  logHR     = c(Treatment_1=0, Treatment_2=-0.2, Treatment_3=-0.5),
  se_shift  = c(Treatment_1=0, Treatment_2= 0.2, Treatment_3= 0.4)
)

# Continuous outcome with AR(1) biomarker, Beta adherence
dat <- generate_longitudinal_data(demo, num_visits=6, trt_map=trt_map,
                                  adherence_type="beta", outcome_type="continuous")

# Tidymodels PS (GLM)
ps <- fit_ps_tidymodels(dat, model = "glm")
dat$ps_hat <- ps$ps_hat
dat_w <- compute_stabilized_iptw(dat)
m <- fit_msm_gee_cont(dat_w)
summary(m)

# Cox MSM from TTE
dat_tte <- generate_longitudinal_data(demo, num_visits=8, trt_map=trt_map,
                                      adherence_type="binary", outcome_type="tte")
ivl <- subset(dat_tte$tte_intervals, AtRisk == 1)
ps2 <- fit_ps_tidymodels(ivl, model = "ranger")
ivl$ps_hat <- ps2$ps_hat
ivl_w <- compute_stabilized_iptw(ivl)
cox_m <- fit_msm_cox(ivl_w)
summary(cox_m)
