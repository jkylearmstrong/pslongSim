#' Generate Demographics
#' @param num_patients Total number of patients.
#' @param seed Random seed.
#' @param n_cohorts Number of treatment cohorts.
#' @param age_range Range of patient ages.
#' @param gender_levels Levels for gender variable.
#' @param treatment_prefix Prefix for treatment names.
#' @export
generate_patient_data_demographic <- function(
    num_patients = 1000,
    seed = 123,
    n_cohorts = 3,
    age_range = c(18, 90),
    gender_levels = c("Male", "Female"),
    treatment_prefix = "Treatment_"
) {
  stopifnot(length(age_range) == 2, age_range[1] < age_range[2])
  set.seed(seed)

  patient_ids <- paste0("P", sprintf("%04d", seq_len(num_patients)))
  ages <- sample(seq.int(age_range[1], age_range[2]), num_patients, replace = TRUE)
  genders <- sample(gender_levels, num_patients, replace = TRUE)
  treatments <- sample(paste0(treatment_prefix, seq_len(n_cohorts)), num_patients, replace = TRUE)

  data.frame(
    PatientID = patient_ids,
    Age = ages,
    Gender = factor(genders, levels = gender_levels),
    Treatment = factor(treatments, levels = paste0(treatment_prefix, seq_len(n_cohorts))),
    stringsAsFactors = FALSE
  )
}

#' Generate Longitudinal Data with Side-Effects, AR(1) Biomarker, and Multiple Outcome Types
#'
#' @param patient_data Data frame from generate_patient_data_demographic().
#' @param num_visits Integer.
#' @param seed Integer.
#' @param trt_map Output of define_treatment_map().
#' @param se_model List of side-effect model parameters.
#' @param adherence_type "binary" or "beta".
#' @param adherence_model List of adherence parameters (includes se_effect and biomarker_effect).
#' @param beta_concentration Numeric for beta adherence dispersion.
#' @param compliance_threshold Numeric (if adherence_type == "beta").
#' @param sd_adherence_re SD for adherence random effect v_i.
#' @param outcome_type "continuous" | "binary" | "tte".
#' @param outcome_model List of outcome parameters (includes se_effect and biomarker_effect).
#' @param tte_model List for TTE hazard model (includes biomarker_logHR).
#' @param biom_model List for AR(1) biomarker parameters.
#' @param rho_uv,rho_uw,rho_vw Correlations among outcome/adherence/SE subject effects.
#' @param start_date,visit_spacing_days,visit_jitter_days Visit scheduling.
#' @return For continuous/binary outcomes: a long data.frame. For TTE: a list with long, tte_intervals, tte_subject.
#' @export
generate_longitudinal_data <- function(
    patient_data,
    num_visits = 6,
    seed = 2026,
    trt_map,
    # Side-effects model
    se_model = list(
      intercept  = -1.2,
      age        =  0.00,
      male       =  0.05,
      time_trend =  0.05,
      lag_se     =  1.00,
      sd_subject =  0.6
    ),
    # Adherence model
    adherence_type = c("binary", "beta"),
    adherence_model = list(
      intercept       = -0.3,
      age             = -0.005,
      male            = -0.10,
      time_trend      =  0.04,
      lag_comp        =  0.80,
      se_effect       = -0.7,
      biomarker_effect=  0.25   # higher biomarker improves adherence if positive
    ),
    beta_concentration = 25,
    compliance_threshold = 0.80,
    sd_adherence_re = 1.0,
    # Outcome
    outcome_type = c("continuous", "binary", "tte"),
    outcome_model = list(
      intercept        = 0.0,    # for continuous: mean; for binary: logit
      adherence        = 0.9,
      se_effect        = -0.5,
      biomarker_effect = 0.40,
      time_trend       = 0.05,
      sd_subject       = 0.7,
      sd_error         = 1.0     # only used for continuous
    ),
    # TTE hazard (discrete-time)
    tte_model = list(
      baseline_logit_h = -3.2,
      adherence_logHR  = -0.5,
      se_logHR         =  0.4,
      biomarker_logHR  = -0.25,  # protective if negative
      time_trend_logHR =  0.00,
      sd_frailty       =  0.6
    ),
    # AR(1) biomarker
    biom_model = list(
      intercept = 0.0,     # mean level
      phi       = 0.75,    # AR(1) coefficient
      sd_innov  = 0.5,     # innovation SD
      sd_b0     = 0.75     # subject-specific baseline SD
    ),
    # RE correlations
    rho_uv = 0.4,
    rho_uw = 0.2,
    rho_vw = 0.2,
    # Visit timing
    start_date = as.Date("2024-01-01"),
    visit_spacing_days = 28,
    visit_jitter_days  = 5
) {
  set.seed(seed)
  stopifnot(is.factor(patient_data$Treatment))
  trt_levels <- levels(patient_data$Treatment)
  req_cols <- c("adh_shift","out_effect","logHR","se_shift")
  stopifnot(all(trt_levels %in% rownames(trt_map)), all(req_cols %in% colnames(trt_map)))

  adherence_type <- match.arg(adherence_type)
  outcome_type   <- match.arg(outcome_type)

  n     <- nrow(patient_data)
  T_vis <- num_visits

  # Long index
  long_index <- expand.grid(PatientRow = seq_len(n), VisitNumber = seq_len(T_vis))
  long_index <- long_index[order(long_index$PatientRow, long_index$VisitNumber), ]

  id     <- patient_data$PatientID[long_index$PatientRow]
  trt    <- patient_data$Treatment[long_index$PatientRow]
  age    <- patient_data$Age[long_index$PatientRow]
  male   <- as.integer(patient_data$Gender[long_index$PatientRow] == "Male")
  visit  <- long_index$VisitNumber

  nominal_days <- (visit - 1L) * visit_spacing_days
  jitter <- sample.int(2 * visit_jitter_days + 1L, length(nominal_days), replace = TRUE) - (visit_jitter_days + 1L)
  visit_date <- start_date + nominal_days + jitter

  # Subject-level RE (u, v, w) with correlation
  sd_u <- outcome_model$sd_subject
  sd_v <- sd_adherence_re
  sd_w <- se_model$sd_subject

  R <- matrix(c(1,      rho_uv, rho_uw,
                rho_uv, 1,      rho_vw,
                rho_uw, rho_vw, 1), 3, 3, byrow = TRUE)
  S <- diag(c(sd_u, sd_v, sd_w))
  Sigma <- S %*% R %*% S
  if (!requireNamespace("MASS", quietly = TRUE)) stop("Install MASS")
  uvw <- MASS::mvrnorm(n = n, mu = c(0, 0, 0), Sigma = Sigma)
  u_i <- uvw[, 1][long_index$PatientRow]
  v_i <- uvw[, 2][long_index$PatientRow]
  w_i <- uvw[, 3][long_index$PatientRow]

  # Subject-specific biomarker baseline
  b0_subj <- rnorm(n, mean = biom_model$intercept, sd = biom_model$sd_b0)
  b0 <- b0_subj[long_index$PatientRow]

  # Treatment shifts
  trt_names <- as.character(trt)
  trt_adh_shift <- trt_map[trt_names, "adh_shift"]
  trt_out_effect <- trt_map[trt_names, "out_effect"]
  trt_se_shift <- trt_map[trt_names, "se_shift"]
  trt_logHR <- trt_map[trt_names, "logHR"]

  # Precompute base LPs
  lp_se_base <- se_model$intercept +
    trt_se_shift + se_model$age * age + se_model$male * male +
    se_model$time_trend * (visit - 1) + w_i

  lp_adh_base <- adherence_model$intercept +
    trt_adh_shift + adherence_model$age * age + adherence_model$male * male +
    adherence_model$time_trend * (visit - 1) + v_i

  # Containers
  SE  <- integer(length(visit))
  Adh <- numeric(length(visit))
  Comp <- integer(length(visit))
  Biom <- numeric(length(visit))
  Event <- integer(length(visit))

  # Helper index
  idx_fun <- function(j, t) (j - 1L) * T_vis + t

  # Generate AR(1) biomarker and lagged processes
  for (j in seq_len(n)) {
    prev_se   <- 0L
    prev_comp <- 0L
    # Biomarker AR(1) initialization
    # Stationary variance: sd = sd_innov / sqrt(1 - phi^2), but we start around b0
    b_prev <- b0_subj[j] + rnorm(1, 0, biom_model$sd_innov / sqrt(1 - biom_model$phi^2 + 1e-8))

    for (t in seq_len(T_vis)) {
      idx <- idx_fun(j, t)

      # AR(1) biomarker at visit t
      b_t <- biom_model$intercept + biom_model$phi * (b_prev - biom_model$intercept) +
        rnorm(1, 0, biom_model$sd_innov)
      Biom[idx] <- b_t
      b_prev <- b_t

      # Side-effect
      lp_se <- lp_se_base[idx] + se_model$lag_se * prev_se
      p_se  <- inv_logit(lp_se)
      se_t  <- rbinom(1L, 1L, clamp(p_se, 1e-6, 1 - 1e-6))

      # Adherence (depends on SE and biomarker)
      lp_adh <- lp_adh_base[idx] +
        adherence_model$lag_comp * prev_comp +
        adherence_model$se_effect * se_t +
        adherence_model$biomarker_effect * b_t
      p_adh <- inv_logit(lp_adh)

      if (adherence_type == "binary") {
        comp_t <- rbinom(1L, 1L, clamp(p_adh, 1e-6, 1 - 1e-6))
        adh_t  <- as.numeric(comp_t)
      } else {
        k  <- beta_concentration
        a  <- max(p_adh * k, 1e-6)
        b  <- max((1 - p_adh) * k, 1e-6)
        adh_t <- stats::rbeta(1L, a, b)
        comp_t <- as.integer(adh_t >= compliance_threshold)
      }

      SE[idx]   <- se_t
      Adh[idx]  <- adh_t
      Comp[idx] <- comp_t

      prev_se   <- se_t
      prev_comp <- comp_t
    }
  }

  # Base data frame
  out_df <- data.frame(
    PatientID   = id,
    Treatment   = trt,
    Age         = age,
    GenderMale  = male,
    VisitNumber = visit,
    VisitDate   = visit_date,
    SideEffect  = SE,
    Adherence   = Adh,
    Compliant   = Comp,
    Biomarker   = Biom,
    stringsAsFactors = FALSE
  )

  if (outcome_type %in% c("continuous","binary")) {
    lp_out <- outcome_model$intercept +
      trt_out_effect +
      outcome_model$adherence * if (adherence_type == "binary") Comp else Adh +
      outcome_model$se_effect * SE +
      outcome_model$biomarker_effect * Biom +
      outcome_model$time_trend * (visit - 1) +
      u_i

    if (outcome_type == "continuous") {
      Y <- lp_out + rnorm(nrow(out_df), 0, outcome_model$sd_error)
    } else {
      pY <- inv_logit(lp_out)
      Y  <- rbinom(nrow(out_df), 1L, clamp(pY, 1e-6, 1 - 1e-6))
    }
    out_df$Outcome <- Y
    return(out_df)
  }

  # TTE discrete-time hazard
  lp_h <- tte_model$baseline_logit_h +
    trt_logHR +
    tte_model$adherence_logHR * Comp +
    tte_model$se_logHR * SE +
    tte_model$biomarker_logHR * Biom +
    tte_model$time_trend_logHR * (visit - 1) +
    u_i

  h <- inv_logit(lp_h)
  Event <- rbinom(nrow(out_df), 1L, clamp(h, 1e-6, 1 - 1e-6))

  out_df$Event <- 0L
  out_df$AtRisk <- 1L

  first_event_row <- tapply(seq_len(nrow(out_df)), out_df$PatientID, function(idx_rows) {
    ev_rows <- idx_rows[Event[idx_rows] == 1]
    if (length(ev_rows) == 0) NA_integer_ else ev_rows[1]
  })
  first_event_row <- unlist(first_event_row, use.names = FALSE)

  for (j in seq_len(n)) {
    pid <- patient_data$PatientID[j]
    rows <- which(out_df$PatientID == pid)
    fe_row <- first_event_row[j]
    if (!is.na(fe_row)) {
      out_df$Event[fe_row] <- 1L
      later <- rows[rows > fe_row]
      if (length(later)) out_df$AtRisk[later] <- 0L
    }
  }

  # Start-stop intervals
  by_pid <- split(out_df, out_df$PatientID)
  tte_intervals <- do.call(rbind, lapply(by_pid, function(df_i) {
    df_i <- df_i[order(df_i$VisitNumber), ]
    t_abs <- as.numeric(df_i$VisitDate - min(df_i$VisitDate))
    tstart <- c(0, head(t_abs, -1))
    tstop  <- t_abs
    data.frame(
      PatientID   = df_i$PatientID,
      Treatment   = df_i$Treatment,
      Age         = df_i$Age,
      GenderMale  = df_i$GenderMale,
      VisitNumber = df_i$VisitNumber,
      SideEffect  = df_i$SideEffect,
      Compliant   = df_i$Compliant,
      Adherence   = df_i$Adherence,
      Biomarker   = df_i$Biomarker,
      tstart      = tstart,
      tstop       = tstop,
      event       = df_i$Event,
      AtRisk      = df_i$AtRisk,
      stringsAsFactors = FALSE
    )
  }))

  subj_event <- aggregate(event ~ PatientID, data = tte_intervals, FUN = function(x) any(x == 1))
  names(subj_event)[2] <- "status"
  subj_time  <- aggregate(tstop ~ PatientID, data = subset(tte_intervals, AtRisk == 1),
                          FUN = function(x) max(x, na.rm = TRUE))
  names(subj_time)[2] <- "time"
  tte_subject <- merge(subj_time, subj_event, by = "PatientID", all.x = TRUE)
  tte_subject$status[is.na(tte_subject$status)] <- 0L
  tte_subject$status <- as.integer(tte_subject$status)

  list(
    long = out_df,
    tte_intervals = tte_intervals,
    tte_subject = tte_subject
  )
}
