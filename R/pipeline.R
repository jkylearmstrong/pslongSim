#' Build the Default Simulation Pipeline
#'
#' @param n_patients Integer number of patients to simulate.
#' @param n_visits Integer number of visits for continuous and binary outcomes.
#' @param tte_visits Integer number of visits for time-to-event outcomes.
#' @param seed_base Integer base seed used to derive per-target seeds.
#' @param n_cohorts Integer number of treatment cohorts.
#' @param ps_models Named list with elements \code{continuous}, \code{binary},
#'   and \code{tte}; each must be one of \code{"glm"}, \code{"ranger"}, or
#'   \code{"xgboost"}.
#' @param adh_shift Numeric vector passed to \code{define_treatment_map()}.
#' @param out_effect Numeric vector passed to \code{define_treatment_map()}.
#' @param logHR Numeric vector passed to \code{define_treatment_map()}.
#' @param se_shift Numeric vector passed to \code{define_treatment_map()}.
#' @return A list of \code{targets::tar_target()} objects for the default
#'   longitudinal simulation workflow.
#' @export
make_default_simulation_targets <- function(
    n_patients = 300L,
    n_visits = 6L,
    tte_visits = 8L,
    seed_base = 10L,
    n_cohorts = 3L,
    ps_models = list(
      continuous = "glm",
      binary = "ranger",
      tte = "xgboost"
    ),
    adh_shift = c(Treatment_1 = 0, Treatment_2 = -0.5, Treatment_3 = -0.9),
    out_effect = c(Treatment_1 = 0, Treatment_2 = 0.3, Treatment_3 = 0.7),
    logHR = c(Treatment_1 = 0, Treatment_2 = -0.2, Treatment_3 = -0.5),
    se_shift = c(Treatment_1 = 0, Treatment_2 = 0.2, Treatment_3 = 0.4)
) {
  stopifnot(
    n_patients > 0L,
    n_visits > 0L,
    tte_visits > 0L,
    n_cohorts > 0L
  )

  required_models <- c("continuous", "binary", "tte")
  valid_models <- c("glm", "xgboost", "ranger")
  stopifnot(
    is.list(ps_models),
    all(required_models %in% names(ps_models)),
    all(unlist(ps_models[required_models], use.names = FALSE) %in% valid_models)
  )

  list(
    targets::tar_target(
      demo,
      generate_patient_data_demographic(
        num_patients = n_patients,
        n_cohorts = n_cohorts,
        seed = seed_base
      )
    ),
    targets::tar_target(
      trt_map,
      {
        levels <- levels(demo$Treatment)
        define_treatment_map(
          levels = levels,
          adh_shift = adh_shift,
          out_effect = out_effect,
          logHR = logHR,
          se_shift = se_shift
        )
      }
    ),
    targets::tar_target(
      dat_cont,
      generate_longitudinal_data(
        patient_data = demo,
        num_visits = n_visits,
        seed = seed_base + 1L,
        trt_map = trt_map,
        adherence_type = "beta",
        outcome_type = "continuous"
      )
    ),
    targets::tar_target(
      dat_bin,
      generate_longitudinal_data(
        patient_data = demo,
        num_visits = n_visits,
        seed = seed_base + 2L,
        trt_map = trt_map,
        adherence_type = "binary",
        outcome_type = "binary"
      )
    ),
    targets::tar_target(
      dat_tte,
      generate_longitudinal_data(
        patient_data = demo,
        num_visits = tte_visits,
        seed = seed_base + 3L,
        trt_map = trt_map,
        adherence_type = "binary",
        outcome_type = "tte"
      )
    ),
    targets::tar_target(
      ps_cont,
      fit_ps_tidymodels(dat_cont, model = ps_models$continuous)
    ),
    targets::tar_target(
      ps_bin,
      fit_ps_tidymodels(dat_bin, model = ps_models$binary, tune_ps = FALSE)
    ),
    targets::tar_target(
      ps_tte,
      fit_ps_tidymodels(
        subset(dat_tte$tte_intervals, AtRisk == 1),
        model = ps_models$tte,
        tune_ps = FALSE
      )
    ),
    targets::tar_target(
      dat_cont_w,
      {
        df <- dat_cont
        df$ps_hat <- ps_cont$ps_hat
        compute_stabilized_iptw(df)
      }
    ),
    targets::tar_target(
      dat_bin_w,
      {
        df <- dat_bin
        df$ps_hat <- ps_bin$ps_hat
        compute_stabilized_iptw(df)
      }
    ),
    targets::tar_target(
      tte_ivl_w,
      {
        ivl <- subset(dat_tte$tte_intervals, AtRisk == 1)
        ivl$ps_hat <- ps_tte$ps_hat
        compute_stabilized_iptw(ivl)
      }
    ),
    targets::tar_target(msm_cont, fit_msm_gee_cont(dat_cont_w)),
    targets::tar_target(msm_bin, fit_msm_gee_bin(dat_bin_w)),
    targets::tar_target(msm_cox, fit_msm_cox(tte_ivl_w)),
    targets::tar_target(summary_cont, summary(msm_cont)),
    targets::tar_target(summary_bin, summary(msm_bin)),
    targets::tar_target(summary_cox, summary(msm_cox))
  )
}
