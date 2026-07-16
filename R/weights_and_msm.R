#' Compute Stabilized IPTW for Binary Compliance
#'
#' Calculates stabilised inverse probability of treatment weights using
#' marginal and conditional (propensity-score) probabilities of compliance.
#' Optionally truncates extreme weights at specified quantiles.
#'
#' @param df Data frame containing the compliance, propensity score, and
#'   visit number columns.
#' @param c_var Character: name of the binary compliance column.
#' @param ps_var Character: name of the propensity score column.
#' @param visit_var Character: name of the visit number column.
#' @param id_var Character: name of the patient identifier column used for
#'   cumulative weight calculation across visits.
#' @param truncate Numeric vector of length 2 giving the lower and upper
#'   quantiles for weight trimming.  Set to \code{NULL} to disable.
#' @return A data frame (the input with an additional \code{w} column).
#' @seealso \code{\link{fit_ps_tidymodels}}, \code{\link{fit_msm_gee_cont}}
#' @examples
#' demo <- generate_patient_data_demographic(num_patients = 50, seed = 1)
#' trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))
#' dat <- generate_longitudinal_data(demo, num_visits = 4, trt_map = trt_map)
#' ps <- fit_ps_tidymodels(dat, model = "glm")
#' dat$ps_hat <- ps$ps_hat
#' dat_w <- compute_stabilized_iptw(dat)
#' range(dat_w$w)
#' @export
compute_stabilized_iptw <- function(df,
                                    c_var = "Compliant",
                                    ps_var = "ps_hat",
                                    visit_var = "VisitNumber",
                                    id_var = "PatientID",
                                    truncate = c(0.01, 0.99)) {
  validate_columns(df, c(c_var, ps_var, visit_var, id_var), "compute_stabilized_iptw")
  validate_min_rows(df, 1L, "compute_stabilized_iptw")

  eps <- 1e-6
  C  <- df[[c_var]]
  
  if (is.factor(C)) {
    C <- as.numeric(as.character(C))
  } else {
    C <- as.numeric(C)
  }
  
  e  <- clamp(df[[ps_var]], eps, 1 - eps)
  pM <- clamp(ave(C, df[[visit_var]], FUN = mean), eps, 1 - eps)
  w_step  <- (pM^C) * ((1 - pM)^(1 - C)) / (e^C) / ((1 - e)^(1 - C))

  ord <- order(df[[id_var]], df[[visit_var]])
  w <- numeric(nrow(df))
  w[ord] <- ave(w_step[ord], df[[id_var]][ord], FUN = cumprod)

  if (!is.null(truncate)) {
    q <- stats::quantile(w, probs = clamp(truncate, 0, 1), na.rm = TRUE)
    w <- clamp(w, q[1], q[2])
  }
  df$w <- as.numeric(w)
  df
}

#' Filter Data to At-Risk Observations
#'
#' Subsets a data frame to rows where the \code{AtRisk} column equals 1.
#'
#' @param df Data frame with an \code{AtRisk} column.
#' @return Filtered data frame.
#' @seealso \code{\link{generate_longitudinal_data}},
#'   \code{\link{fit_msm_cox}}
#' @examples
#' demo <- generate_patient_data_demographic(num_patients = 50, seed = 1)
#' trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))
#' tte <- generate_longitudinal_data(demo, num_visits = 5, trt_map = trt_map,
#'                                   outcome_type = "tte")
#' nrow(tte$tte_intervals)
#' nrow(filter_at_risk(tte$tte_intervals))
#' @export
filter_at_risk <- function(df) {
  validate_columns(df, "AtRisk", "filter_at_risk")
  df[df$AtRisk == 1L, , drop = FALSE]
}

#' Fit GEE MSM for Continuous Outcomes
#'
#' Fits a generalised estimating equations marginal structural model for
#' a continuous outcome, using an independence working correlation and
#' robust (sandwich) standard errors.
#'
#' @param df_w Weighted data frame with columns \code{Outcome},
#'   \code{Treatment}, \code{VisitNumber}, \code{PatientID}, and \code{w}.
#' @return A \code{geeglm} object.
#' @seealso \code{\link{compute_stabilized_iptw}},
#'   \code{\link{fit_msm_gee_bin}}, \code{\link{fit_msm_cox}}
#' @examples
#' demo <- generate_patient_data_demographic(num_patients = 50, seed = 1)
#' trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))
#' dat <- generate_longitudinal_data(demo, num_visits = 4, trt_map = trt_map)
#' ps <- fit_ps_tidymodels(dat, model = "glm")
#' dat$ps_hat <- ps$ps_hat
#' dat_w <- compute_stabilized_iptw(dat)
#' msm <- fit_msm_gee_cont(dat_w)
#' summary(msm)
#' @export
fit_msm_gee_cont <- function(df_w) {
  .fit_msm_gee(df_w, family = gaussian(), fn_name = "fit_msm_gee_cont")
}

#' Fit GEE MSM for Binary Outcomes
#'
#' Fits a generalised estimating equations marginal structural model for
#' a binary outcome, using an independence working correlation and
#' robust (sandwich) standard errors.
#'
#' @param df_w Weighted data frame with columns \code{Outcome},
#'   \code{Treatment}, \code{VisitNumber}, \code{PatientID}, and \code{w}.
#' @return A \code{geeglm} object.
#' @seealso \code{\link{compute_stabilized_iptw}},
#'   \code{\link{fit_msm_gee_cont}}, \code{\link{fit_msm_cox}}
#' @examples
#' demo <- generate_patient_data_demographic(num_patients = 50, seed = 1)
#' trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))
#' dat <- generate_longitudinal_data(demo, num_visits = 4, trt_map = trt_map,
#'                                   outcome_type = "binary")
#' ps <- fit_ps_tidymodels(dat, model = "glm")
#' dat$ps_hat <- ps$ps_hat
#' dat_w <- compute_stabilized_iptw(dat)
#' msm <- fit_msm_gee_bin(dat_w)
#' summary(msm)
#' @export
fit_msm_gee_bin <- function(df_w) {
  .fit_msm_gee(df_w, family = binomial(), fn_name = "fit_msm_gee_bin")
}

#' @keywords internal
.fit_msm_gee <- function(df_w, family, fn_name) {
  validate_columns(df_w, c("Outcome", "Treatment", "VisitNumber",
                           "PatientID", "w"), fn_name)
  validate_min_rows(df_w, 10L, fn_name)
  df_w$.cluster_id <- as.numeric(factor(df_w$PatientID))
  tryCatch({
    geepack::geeglm(
      Outcome ~ Treatment + VisitNumber,
      id      = .cluster_id,
      weights = w,
      family  = family,
      corstr  = "independence",
      data    = df_w,
      std.err = "san.se"
    )
  }, error = function(e) {
    stop(fn_name, ": GEE model execution failed: ", e$message,
         call. = FALSE)
  })
}

#' Fit Cox MSM (Start-Stop Intervals) with Robust SEs
#'
#' Fits a Cox proportional hazards marginal structural model using
#' start-stop interval data and robust (cluster-robust) standard errors.
#'
#' @param tte_ivl_w Weighted TTE interval data frame with columns
#'   \code{tstart}, \code{tstop}, \code{event}, \code{Treatment},
#'   \code{PatientID}, and \code{w}.
#' @return A \code{coxph} object.
#' @seealso \code{\link{compute_stabilized_iptw}},
#'   \code{\link{fit_msm_gee_cont}}, \code{\link{fit_msm_gee_bin}},
#'   \code{\link{filter_at_risk}}
#' @examples
#' demo <- generate_patient_data_demographic(num_patients = 50, seed = 1)
#' trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))
#' tte <- generate_longitudinal_data(demo, num_visits = 6, trt_map = trt_map,
#'                                   outcome_type = "tte")
#' ivl <- filter_at_risk(tte$tte_intervals)
#' ps <- fit_ps_tidymodels(ivl, model = "glm")
#' ivl$ps_hat <- ps$ps_hat
#' ivl_w <- compute_stabilized_iptw(ivl)
#' cox_m <- fit_msm_cox(ivl_w)
#' summary(cox_m)
#' @export
fit_msm_cox <- function(tte_ivl_w) {
  validate_columns(tte_ivl_w, c("tstart", "tstop", "event",
                                 "Treatment", "PatientID", "w"),
                   "fit_msm_cox")
  validate_min_rows(tte_ivl_w, 10L, "fit_msm_cox")

  result <- tryCatch({
    survival::coxph(
      survival::Surv(tstart, tstop, event) ~ Treatment,
      data    = tte_ivl_w,
      weights = w,
      robust  = TRUE,
      cluster = PatientID
    )
  }, error = function(e) {
    stop("fit_msm_cox: Cox model execution failed: ", e$message,
         call. = FALSE)
  })
  result
}
