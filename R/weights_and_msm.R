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
                                    truncate = c(0.01, 0.99)) {
  validate_columns(df, c(c_var, ps_var, visit_var), "compute_stabilized_iptw")
  validate_min_rows(df, 1L, "compute_stabilized_iptw")

  marg <- stats::aggregate(df[[c_var]] ~ df[[visit_var]], data = df, FUN = mean)
  names(marg) <- c(visit_var, "p_marg")
  df <- merge(df, marg, by = visit_var, all.x = TRUE, sort = FALSE)

  eps <- 1e-6
  C  <- df[[c_var]]
  e  <- clamp(df[[ps_var]], eps, 1 - eps)
  pM <- clamp(df[["p_marg"]], eps, 1 - eps)
  w  <- (pM^C) * ((1 - pM)^(1 - C)) / (e^C) / ((1 - e)^(1 - C))

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
  require_suggested("geepack")
  validate_columns(df_w, c("Outcome", "Treatment", "VisitNumber",
                            "PatientID", "w"), "fit_msm_gee_cont")
  validate_min_rows(df_w, 10L, "fit_msm_gee_cont")

  result <- tryCatch({
    geepack::geeglm(
      Outcome ~ Treatment + VisitNumber,
      id      = PatientID,
      weights = w,
      family  = gaussian(),
      corstr  = "independence",
      data    = df_w,
      std.err = "san.se"
    )
  }, error = function(e) {
    stop("fit_msm_gee_cont: GEE model failed to converge: ", e$message,
         call. = FALSE)
  })
  result
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
#' @export
fit_msm_gee_bin <- function(df_w) {
  require_suggested("geepack")
  validate_columns(df_w, c("Outcome", "Treatment", "VisitNumber",
                            "PatientID", "w"), "fit_msm_gee_bin")
  validate_min_rows(df_w, 10L, "fit_msm_gee_bin")

  result <- tryCatch({
    geepack::geeglm(
      Outcome ~ Treatment + VisitNumber,
      id      = PatientID,
      weights = w,
      family  = binomial(),
      corstr  = "independence",
      data    = df_w,
      std.err = "san.se"
    )
  }, error = function(e) {
    stop("fit_msm_gee_bin: GEE model failed to converge: ", e$message,
         call. = FALSE)
  })
  result
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
#'   \code{\link{fit_msm_gee_cont}}, \code{\link{filter_at_risk}}
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
  require_suggested("survival")
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
    stop("fit_msm_cox: Cox model failed to fit: ", e$message,
         call. = FALSE)
  })
  result
}
