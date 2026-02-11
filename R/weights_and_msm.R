#' Compute Stabilized IPTW for Binary Compliance
#' @param df Data frame with columns: Compliant, VisitNumber, ps_hat.
#' @param truncate Trimming quantiles.
#' @export
compute_stabilized_iptw <- function(df,
                                    c_var = "Compliant",
                                    ps_var = "ps_hat",
                                    visit_var = "VisitNumber",
                                    truncate = c(0.01, 0.99)) {
  stopifnot(all(c(c_var, ps_var, visit_var) %in% names(df)))
  marg <- stats::aggregate(df[[c_var]] ~ df[[visit_var]], data = df, FUN = mean)
  names(marg) <- c(visit_var, "p_marg")
  df <- merge(df, marg, by = visit_var, all.x = TRUE, sort = FALSE)
  eps <- 1e-6
  C  <- df[[c_var]]
  e  <- clamp(df[[ps_var]], eps, 1 - eps)
  pM <- clamp(df[["p_marg"]], eps, 1 - eps)
  w <- (pM^C) * ((1 - pM)^(1 - C)) / (e^C) / ((1 - e)^(1 - C))
  if (!is.null(truncate)) {
    q <- stats::quantile(w, probs = clamp(truncate, 0, 1), na.rm = TRUE)
    w <- clamp(w, q[1], q[2])
  }
  df$w <- as.numeric(w)
  df
}

#' Fit GEE MSM for Continuous Outcomes
#' @export
fit_msm_gee_cont <- function(df_w) {
  if (!requireNamespace("geepack", quietly = TRUE)) stop("Install geepack")
  geepack::geeglm(
    Outcome ~ Treatment + VisitNumber,
    id = PatientID,
    weights = w,
    family = gaussian(),
    corstr = "independence",
    data = df_w,
    std.err = "san.se"
  )
}

#' Fit GEE MSM for Binary Outcomes
#' @export
fit_msm_gee_bin <- function(df_w) {
  if (!requireNamespace("geepack", quietly = TRUE)) stop("Install geepack")
  geepack::geeglm(
    Outcome ~ Treatment + VisitNumber,
    id = PatientID,
    weights = w,
    family = binomial(),
    corstr = "independence",
    data = df_w,
    std.err = "san.se"
  )
}

#' Fit Cox MSM (Start-Stop Intervals) with Robust SEs
#' @export
fit_msm_cox <- function(tte_ivl_w) {
  if (!requireNamespace("survival", quietly = TRUE)) stop("Install survival")
  survival::coxph(
    survival::Surv(tstart, tstop, event) ~ Treatment,
    data = tte_ivl_w,
    weights = w,
    robust = TRUE,
    cluster = PatientID
  )
}
