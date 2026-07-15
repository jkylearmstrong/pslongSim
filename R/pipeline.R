#' Build the Default Simulation Pipeline
#'
#' Constructs a list of \code{targets::tar_target()} objects representing
#' the full longitudinal PS simulation workflow: data generation, propensity
#' score estimation, IPTW weighting, MSM fitting, and summarisation.
#'
#' @param num_patients Integer number of patients to simulate.
#' @param num_visits Integer number of visits for continuous and binary
#'   outcomes.
#' @param tte_visits Integer number of visits for time-to-event outcomes.
#' @param seed_base Integer base seed used to derive per-target seeds.
#' @param n_cohorts Integer number of treatment cohorts.
#' @param ps_models Named list with elements \code{continuous},
#'   \code{binary}, and \code{tte}; each must be one of \code{"glm"},
#'   \code{"ranger"}, or \code{"xgboost"}.
#' @param non_linear_feature Logical; whether to include interaction terms
#'   in data generation (see \code{nonlin_coefs}).
#' @param nonlin_coefs Named numeric vector of interaction coefficients
#'   passed to \code{generate_longitudinal_data()}.
#' @param adh_shift Numeric vector passed to
#'   \code{define_treatment_map()}.
#' @param out_effect Numeric vector passed to
#'   \code{define_treatment_map()}.
#' @param logHR Numeric vector passed to
#'   \code{define_treatment_map()}.
#' @param se_shift Numeric vector passed to
#'   \code{define_treatment_map()}.
#' @return A list of \code{targets::tar_target()} objects.
#' @examples
#' \donttest{
#' # Requires the 'targets' package
#' targets <- make_default_simulation_targets(num_patients = 10)
#' length(targets)
#' }
#' @export
make_default_simulation_targets <- function(
    num_patients = 300L,
    num_visits = 6L,
    tte_visits = 8L,
    seed_base = 10L,
    n_cohorts = 3L,
    ps_models = list(
      continuous = "glm",
      binary     = "ranger",
      tte        = "xgboost"
    ),
    non_linear_feature = FALSE,
    nonlin_coefs = c("Age:SideEffect" = 0.15),
    adh_shift  = c(Treatment_1 = 0, Treatment_2 = -0.5, Treatment_3 = -0.9),
    out_effect = c(Treatment_1 = 0, Treatment_2 = 0.3, Treatment_3 = 0.7),
    logHR      = c(Treatment_1 = 0, Treatment_2 = -0.2, Treatment_3 = -0.5),
    se_shift   = c(Treatment_1 = 0, Treatment_2 = 0.2, Treatment_3 = 0.4)
) {
  stopifnot(
    "`num_patients` must be positive." = num_patients > 0L,
    "`num_visits` must be positive."   = num_visits > 0L,
    "`tte_visits` must be positive."   = tte_visits > 0L,
    "`n_cohorts` must be positive."    = n_cohorts > 0L
  )

  required_models <- c("continuous", "binary", "tte")
  valid_models    <- c("glm", "xgboost", "ranger")
  stopifnot(
    is.list(ps_models),
    all(required_models %in% names(ps_models)),
    all(unlist(ps_models[required_models], use.names = FALSE) %in% valid_models)
  )

  list(
    # -- Demographics ----------------------------------------------------------
    targets::tar_target(
      demo,
      generate_patient_data_demographic(
        num_patients = num_patients,
        n_cohorts    = n_cohorts,
        seed         = seed_base
      )
    ),
    targets::tar_target(
      trt_map,
      {
        levels <- levels(demo$Treatment)
        define_treatment_map(
          levels     = levels,
          adh_shift  = adh_shift,
          out_effect = out_effect,
          logHR      = logHR,
          se_shift   = se_shift
        )
      }
    ),
    # -- Data generation (3 outcome types) ------------------------------------
    targets::tar_target(
      dat_cont,
      generate_longitudinal_data(
        patient_data       = demo,
        num_visits         = num_visits,
        seed               = seed_base + 1L,
        trt_map            = trt_map,
        adherence_type     = "beta",
        outcome_type       = "continuous",
        non_linear_feature = non_linear_feature,
        nonlin_coefs       = nonlin_coefs
      )
    ),
    targets::tar_target(
      dat_bin,
      generate_longitudinal_data(
        patient_data       = demo,
        num_visits         = num_visits,
        seed               = seed_base + 2L,
        trt_map            = trt_map,
        adherence_type     = "binary",
        outcome_type       = "binary",
        non_linear_feature = non_linear_feature,
        nonlin_coefs       = nonlin_coefs
      )
    ),
    targets::tar_target(
      dat_tte,
      generate_longitudinal_data(
        patient_data       = demo,
        num_visits         = tte_visits,
        seed               = seed_base + 3L,
        trt_map            = trt_map,
        adherence_type     = "binary",
        outcome_type       = "tte",
        non_linear_feature = non_linear_feature,
        nonlin_coefs       = nonlin_coefs
      )
    ),
    # -- Propensity score models ----------------------------------------------
    targets::tar_target(
      ps_cont,
      fit_ps_tidymodels(dat_cont, model = ps_models$continuous)
    ),
    targets::tar_target(
      ps_bin,
      fit_ps_tidymodels(dat_bin, model = ps_models$binary,
                        tune_ps = FALSE)
    ),
    targets::tar_target(
      ps_tte,
      fit_ps_tidymodels(
        filter_at_risk(dat_tte$tte_intervals),
        model    = ps_models$tte,
        tune_ps  = FALSE
      )
    ),
    # -- IPTW weights ---------------------------------------------------------
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
        ivl <- filter_at_risk(dat_tte$tte_intervals)
        ivl$ps_hat <- ps_tte$ps_hat
        compute_stabilized_iptw(ivl)
      }
    ),
    # -- Marginal structural models -------------------------------------------
    targets::tar_target(msm_cont, fit_msm_gee_cont(dat_cont_w)),
    targets::tar_target(msm_bin,  fit_msm_gee_bin(dat_bin_w)),
    targets::tar_target(msm_cox,  fit_msm_cox(tte_ivl_w)),
    # -- Summaries ------------------------------------------------------------
    targets::tar_target(summary_cont, summary(msm_cont)),
    targets::tar_target(summary_bin,  summary(msm_bin)),
    targets::tar_target(summary_cox,  summary(msm_cox))
  )
}
