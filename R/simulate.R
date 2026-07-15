#' Generate Demographics
#'
#' Creates a baseline patient data frame with IDs, ages, genders, and
#' randomised treatment assignments.
#'
#' @param num_patients Total number of patients.
#' @param seed Random seed (note: sets the global RNG state).
#' @param n_cohorts Number of treatment cohorts.
#' @param age_range Integer vector of length 2 giving the age range.
#' @param gender_levels Character vector of gender levels.
#' @param treatment_prefix Character prefix for treatment level names.
#' @return A data.frame with columns \code{PatientID}, \code{Age},
#'   \code{Gender}, and \code{Treatment}.
#' @examples
#' demo <- generate_patient_data_demographic(num_patients = 100, seed = 1)
#' head(demo)
#' @export
generate_patient_data_demographic <- function(
    num_patients = 1000,
    seed = 123,
    n_cohorts = 3,
    age_range = c(18, 90),
    gender_levels = c("Male", "Female"),
    treatment_prefix = "Treatment_"
) {
  stopifnot(
    "`num_patients` must be a positive integer." = is.numeric(num_patients) && num_patients >= 1L,
    "`age_range` must be length 2 with age_range[1] < age_range[2]." =
      length(age_range) == 2L && age_range[1] < age_range[2]
  )
  set.seed(seed)

  patient_ids <- paste0("P", sprintf("%04d", seq_len(num_patients)))
  ages <- sample(seq.int(age_range[1], age_range[2]), num_patients, replace = TRUE)
  genders <- sample(gender_levels, num_patients, replace = TRUE)
  treatments <- sample(paste0(treatment_prefix, seq_len(n_cohorts)),
                       num_patients, replace = TRUE)

  data.frame(
    PatientID = patient_ids,
    Age       = ages,
    Gender    = factor(genders, levels = gender_levels),
    Treatment = factor(treatments,
                       levels = paste0(treatment_prefix, seq_len(n_cohorts)))
  )
}

#' Generate Longitudinal Data with Side-Effects, AR(1) Biomarker,
#' and Multiple Outcome Types
#'
#' Simulates a longitudinal clinical trial dataset with treatment-driven
#' side effects, an AR(1) biomarker process, time-varying adherence,
#' and one of three outcome types (continuous, binary, or time-to-event).
#'
#' @section Non-linear feature:
#' When \code{non_linear_feature = TRUE}, one or more multiplicative
#' interactions are added to the outcome with no additive main effects.
#' The default interaction is \code{Age * SideEffect}.  These signals are
#' \strong{undetectable} by linear regression (which can only model additive
#' effects) but \strong{readily detectable} by tree-based models such as
#' random forests or XGBoost, which naturally partition on feature
#' interactions.  Use \code{nonlin_coefs} to control which interactions are
#' included and their coefficients.
#'
#' @param patient_data Data frame from \code{generate_patient_data_demographic()}.
#' @param num_visits Integer number of study visits.
#' @param seed Integer random seed (note: sets the global RNG state).
#' @param trt_map Output of \code{define_treatment_map()}.
#' @param se_model List of side-effect model parameters.
#' @param adherence_type \code{"binary"} or \code{"beta"}.
#' @param adherence_model List of adherence parameters.
#' @param beta_concentration Numeric concentration parameter for beta
#'   adherence.
#' @param compliance_threshold Numeric threshold for beta adherence
#'   compliance.
#' @param sd_adherence_re SD for the adherence random effect.
#' @param outcome_type \code{"continuous"}, \code{"binary"}, or
#'   \code{"tte"}.
#' @param outcome_model List of outcome model parameters.
#' @param non_linear_feature Logical; if \code{TRUE}, adds interaction terms
#'   to the outcome as specified by \code{nonlin_coefs}.  Default
#'   \code{FALSE} for backward compatibility.
#' @param nonlin_coefs Named numeric vector of interaction coefficients.
#'   Names must be in the form \code{"VarA:VarB"} (e.g.,
#'   \code{c("Age:SideEffect" = 0.15)}).  Only used when
#'   \code{non_linear_feature = TRUE}.
#' @param tte_model List for the TTE hazard model.
#' @param biom_model List for the AR(1) biomarker parameters.
#' @param rho_uv Correlation between outcome and adherence subject effects.
#' @param rho_uw Correlation between outcome and side-effect subject effects.
#' @param rho_vw Correlation between adherence and side-effect subject effects.
#' @param start_date Date of the first visit.
#' @param visit_spacing_days Nominal number of days between visits.
#' @param visit_jitter_days Maximum random jitter (in days) added to each
#'   visit date.
#' @return For continuous/binary outcomes: a \code{data.frame}. For TTE:
#'   a list with elements \code{long}, \code{tte_intervals}, and
#'   \code{tte_subject}.
#' @seealso \code{\link{generate_patient_data_demographic}},
#'   \code{\link{define_treatment_map}}, \code{\link{filter_at_risk}}
#' @examples
#' demo <- generate_patient_data_demographic(num_patients = 50, seed = 1)
#' trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))
#' dat <- generate_longitudinal_data(demo, num_visits = 4, trt_map = trt_map)
#' head(dat)
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
      intercept        = -0.3,
      age              = -0.005,
      male             = -0.10,
      time_trend       =  0.04,
      lag_comp         =  0.80,
      se_effect        = -0.7,
      biomarker_effect =  0.25
    ),
    beta_concentration = 25,
    compliance_threshold = 0.80,
    sd_adherence_re = 1.0,
    # Outcome
    outcome_type = c("continuous", "binary", "tte"),
    outcome_model = list(
      intercept        = 0.0,
      adherence        = 0.9,
      se_effect        = -0.5,
      biomarker_effect = 0.40,
      time_trend       = 0.05,
      sd_subject       = 0.7,
      sd_error         = 1.0
    ),
    # Non-linear feature toggle
    non_linear_feature = FALSE,
    nonlin_coefs = c("Age:SideEffect" = 0.15),
    # TTE hazard (discrete-time)
    tte_model = list(
      baseline_logit_h = -3.2,
      adherence_logHR  = -0.5,
      se_logHR         =  0.4,
      biomarker_logHR  = -0.25,
      time_trend_logHR =  0.00,
      sd_frailty       =  0.6
    ),
    # AR(1) biomarker
    biom_model = list(
      intercept = 0.0,
      phi       = 0.75,
      sd_innov  = 0.5,
      sd_b0     = 0.75
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
  # -- Input validation -------------------------------------------------------
  if (!is.data.frame(patient_data)) {
    stop("generate_longitudinal_data: `patient_data` must be a data.frame.")
  }
  required_demo <- c("PatientID", "Age", "Gender", "Treatment")
  validate_columns(patient_data, required_demo, "generate_longitudinal_data")
  if (!is.factor(patient_data$Treatment)) {
    stop("generate_longitudinal_data: `patient_data$Treatment` must be a factor.")
  }
  stopifnot(
    "`num_visits` must be a positive integer." =
      is.numeric(num_visits) && num_visits >= 1L,
    "`trt_map` must be a data.frame." = is.data.frame(trt_map)
  )

  adherence_type <- match.arg(adherence_type)
  outcome_type   <- match.arg(outcome_type)

  trt_levels <- levels(patient_data$Treatment)
  req_trt_cols <- c("adh_shift", "out_effect", "logHR", "se_shift")
  if (!all(trt_levels %in% rownames(trt_map))) {
    stop("generate_longitudinal_data: trt_map is missing rows for treatment levels: ",
         paste(setdiff(trt_levels, rownames(trt_map)), collapse = ", "))
  }
  if (!all(req_trt_cols %in% colnames(trt_map))) {
    stop("generate_longitudinal_data: trt_map is missing columns: ",
         paste(setdiff(req_trt_cols, colnames(trt_map)), collapse = ", "))
  }

  set.seed(seed)

  n     <- nrow(patient_data)
  T_vis <- num_visits

  # Long index
  long_index <- expand.grid(PatientRow = seq_len(n),
                            VisitNumber = seq_len(T_vis))
  long_index <- long_index[order(long_index$PatientRow,
                                  long_index$VisitNumber), ]

  id    <- patient_data$PatientID[long_index$PatientRow]
  trt   <- patient_data$Treatment[long_index$PatientRow]
  age   <- patient_data$Age[long_index$PatientRow]
  male  <- as.integer(patient_data$Gender[long_index$PatientRow] == "Male")
  visit <- long_index$VisitNumber

  nominal_days <- (visit - 1L) * visit_spacing_days
  jitter <- sample.int(2 * visit_jitter_days + 1L,
                       length(nominal_days), replace = TRUE) -
    (visit_jitter_days + 1L)
  visit_date <- start_date + nominal_days + jitter

  # Subject-level RE (u, v, w) with correlation
  # u_i is the outcome frailty; for TTE use sd_frailty, otherwise sd_subject
  sd_u <- if (identical(outcome_type, "tte")) {
    tte_model$sd_frailty
  } else {
    outcome_model$sd_subject
  }
  sd_v <- sd_adherence_re
  sd_w <- se_model$sd_subject

  R <- matrix(c(1,      rho_uv, rho_uw,
                rho_uv, 1,      rho_vw,
                rho_uw, rho_vw, 1), 3, 3, byrow = TRUE)
  S <- diag(c(sd_u, sd_v, sd_w))
  Sigma <- S %*% R %*% S
  require_suggested("MASS")
  uvw <- MASS::mvrnorm(n = n, mu = c(0, 0, 0), Sigma = Sigma)
  u_i <- uvw[, 1][long_index$PatientRow]
  v_i <- uvw[, 2][long_index$PatientRow]
  w_i <- uvw[, 3][long_index$PatientRow]

  # Subject-specific biomarker baseline
  b0_subj <- rnorm(n, mean = biom_model$intercept, sd = biom_model$sd_b0)
  b0 <- b0_subj[long_index$PatientRow]

  # Treatment shifts
  trt_names      <- as.character(trt)
  trt_adh_shift  <- trt_map[trt_names, "adh_shift"]
  trt_out_effect <- trt_map[trt_names, "out_effect"]
  trt_se_shift   <- trt_map[trt_names, "se_shift"]
  trt_logHR      <- trt_map[trt_names, "logHR"]

  # Precompute base LPs
  lp_se_base <- se_model$intercept +
    trt_se_shift + se_model$age * age + se_model$male * male +
    se_model$time_trend * (visit - 1) + w_i

  lp_adh_base <- adherence_model$intercept +
    trt_adh_shift + adherence_model$age * age +
    adherence_model$male * male +
    adherence_model$time_trend * (visit - 1) + v_i

  # Containers
  n_long <- length(visit)
  SE    <- integer(n_long)
  Adh   <- numeric(n_long)
  Comp  <- integer(n_long)
  Biom  <- numeric(n_long)
  Event <- integer(n_long)

  # Helper index
  idx_fun <- function(j, t) (j - 1L) * T_vis + t

  # Generate AR(1) biomarker and lagged processes
  for (j in seq_len(n)) {
    prev_se   <- 0L
    prev_comp <- 0L
    b_prev <- b0_subj[j] +
      rnorm(1, 0, biom_model$sd_innov /
              sqrt(1 - biom_model$phi^2 + 1e-8))

    for (t in seq_len(T_vis)) {
      idx <- idx_fun(j, t)

      # AR(1) biomarker
      b_t <- biom_model$intercept +
        biom_model$phi * (b_prev - biom_model$intercept) +
        rnorm(1, 0, biom_model$sd_innov)
      Biom[idx] <- b_t
      b_prev <- b_t

      # Side-effect
      lp_se <- lp_se_base[idx] + se_model$lag_se * prev_se
      p_se  <- inv_logit(lp_se)
      se_t  <- rbinom(1L, 1L, clamp(p_se, 1e-6, 1 - 1e-6))

      # Adherence
      lp_adh <- lp_adh_base[idx] +
        adherence_model$lag_comp * prev_comp +
        adherence_model$se_effect * se_t +
        adherence_model$biomarker_effect * b_t
      p_adh <- inv_logit(lp_adh)

      if (adherence_type == "binary") {
        comp_t <- rbinom(1L, 1L, clamp(p_adh, 1e-6, 1 - 1e-6))
        adh_t  <- as.numeric(comp_t)
      } else {
        k <- beta_concentration
        a <- max(p_adh * k, 1e-6)
        b <- max((1 - p_adh) * k, 1e-6)
        adh_t  <- stats::rbeta(1L, a, b)
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
    Biomarker   = Biom
  )

  # Non-linear interaction features
  # Interactions are included ONLY as products; no additive main effects.
  # Linear models cannot detect these signals; tree-based models can.
  if (non_linear_feature) {
    out_df <- build_nl_columns(out_df, nonlin_coefs)
  }

  # -- Outcome generation -----------------------------------------------------
  if (outcome_type %in% c("continuous", "binary")) {
    lp_out <- outcome_model$intercept +
      trt_out_effect +
      outcome_model$adherence *
        if (adherence_type == "binary") Comp else Adh +
      outcome_model$se_effect * SE +
      outcome_model$biomarker_effect * Biom +
      outcome_model$time_trend * (visit - 1) +
      u_i

    if (non_linear_feature) {
      lp_out <- add_nl_to_lp(lp_out, out_df, nonlin_coefs)
    }

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

  if (non_linear_feature) {
    lp_h <- add_nl_to_lp(lp_h, out_df, nonlin_coefs)
  }

  h <- inv_logit(lp_h)
  Event <- rbinom(nrow(out_df), 1L, clamp(h, 1e-6, 1 - 1e-6))

  out_df$Event  <- 0L
  out_df$AtRisk <- 1L

  # Vectorised first-event identification (avoids O(n^2) loop)
  first_event_row <- tapply(
    seq_len(nrow(out_df)), out_df$PatientID,
    function(idx_rows) {
      ev_rows <- idx_rows[Event[idx_rows] == 1]
      if (length(ev_rows) == 0L) NA_integer_ else ev_rows[1L]
    }
  )
  first_event_row <- unlist(first_event_row, use.names = FALSE)

  # Assign events and censoring using vectorised row indexing
  non_na <- which(!is.na(first_event_row))
  for (j in non_na) {
    pid    <- patient_data$PatientID[j]
    fe_row <- first_event_row[j]
    rows   <- which(out_df$PatientID == pid)
    out_df$Event[fe_row] <- 1L
    later <- rows[rows > fe_row]
    if (length(later)) out_df$AtRisk[later] <- 0L
  }

  # Start-stop intervals
  nl_cols <- grep("^NL_", names(out_df), value = TRUE)
  by_pid <- split(out_df, out_df$PatientID)
  tte_intervals <- do.call(rbind, lapply(by_pid, function(df_i) {
    df_i <- df_i[order(df_i$VisitNumber), ]
    t_abs <- as.numeric(df_i$VisitDate - min(df_i$VisitDate))
    tstart <- c(0, head(t_abs, -1))
    tstop  <- t_abs
    base <- data.frame(
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
      AtRisk      = df_i$AtRisk
    )
    for (nc in nl_cols) base[[nc]] <- df_i[[nc]]
    base
  }))

  subj_event <- aggregate(
    event ~ PatientID, data = tte_intervals,
    FUN = function(x) any(x == 1)
  )
  names(subj_event)[2] <- "status"

  at_risk_df <- filter_at_risk(tte_intervals)
  subj_time <- aggregate(
    tstop ~ PatientID, data = at_risk_df,
    FUN = function(x) max(x, na.rm = TRUE)
  )
  names(subj_time)[2] <- "time"

  tte_subject <- merge(subj_time, subj_event, by = "PatientID", all.x = TRUE)
  tte_subject$status[is.na(tte_subject$status)] <- 0L
  tte_subject$status <- as.integer(tte_subject$status)

  list(
    long          = out_df,
    tte_intervals = tte_intervals,
    tte_subject   = tte_subject
  )
}

# --- Internal helpers for non-linear interactions ----------------------------

#' Build non-linear interaction columns
#'
#' Parses \code{nonlin_coefs} names in \code{"VarA:VarB"} form, validates
#' that both variables exist in \code{df}, and creates product columns named
#' \code{NL_<VarA>_<VarB}.
#'
#' @param df Data frame containing the predictor variables.
#' @param nonlin_coefs Named numeric vector; names must be \code{"VarA:VarB"}.
#' @return \code{df} with additional \code{NL_*} columns (invisibly).
#' @keywords internal
build_nl_columns <- function(df, nonlin_coefs) {
  if (length(nonlin_coefs) == 0L) return(df)
  nm_names <- names(nonlin_coefs)
  for (i in seq_along(nonlin_coefs)) {
    nm <- nm_names[i]
    parts <- strsplit(nm, ":", fixed = TRUE)[[1L]]
    if (length(parts) != 2L) {
      warning("nonlin_coefs: '", nm, "' does not follow 'VarA:VarB' format, skipping.")
      next
    }
    if (!all(parts %in% names(df))) {
      missing <- setdiff(parts, names(df))
      warning("nonlin_coefs: variable(s) ", paste(missing, collapse = ", "),
              " not in data, skipping '", nm, "'.")
      next
    }
    col_name <- paste0("NL_", parts[1L], "_", parts[2L])
    if (!col_name %in% names(df)) {
      df[[col_name]] <- df[[parts[1L]]] * df[[parts[2L]]]
    }
  }
  df
}

#' Add non-linear terms to a linear predictor vector
#'
#' For each entry in \code{nonlin_coefs}, looks up the corresponding
#' \code{NL_<VarA>_<VarB>} column in \code{df} and adds the weighted
#' interaction to \code{lp}.
#'
#' @param lp Numeric vector (linear predictor, modified in place).
#' @param df Data frame containing the \code{NL_*} columns.
#' @param nonlin_coefs Named numeric vector of interaction coefficients.
#' @return Updated linear predictor \code{lp}.
#' @keywords internal
add_nl_to_lp <- function(lp, df, nonlin_coefs) {
  if (length(nonlin_coefs) == 0L) return(lp)
  nm_names <- names(nonlin_coefs)
  for (i in seq_along(nonlin_coefs)) {
    nm <- nm_names[i]
    parts <- strsplit(nm, ":", fixed = TRUE)[[1L]]
    if (length(parts) == 2L) {
      col_name <- paste0("NL_", parts[1L], "_", parts[2L])
      if (col_name %in% names(df)) {
        lp <- lp + nonlin_coefs[[i]] * df[[col_name]]
      }
    }
  }
  lp
}
