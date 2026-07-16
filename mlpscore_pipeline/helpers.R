# Custom pipeline helpers to avoid dependency on MLPScore simulation code

`%||%` <- function(a, b) if (!is.null(a)) a else b

read_sim_params <- function(path, coerce_types = TRUE,
                            numeric_params = c("num_patients", "num_visits",
                              "n_cohorts", "rho_uv", "adherence_intercept",
                              "compliance_threshold", "trt\\d+_adherence_shift",
                              "trt\\d+_outcome_effect", "adherence_\\w+",
                              "outcome_\\w+", "allocation_ratio", "beta_concentration",
                              "expected_compliance_\\w+", "visit_spacing_days",
                              "visit_jitter_days", "days_per_visit"),
                            sheet = 1,
                            value_col = getOption("MLPScore.value_col", "Value")) {
  params_df <- readxl::read_excel(path, sheet = sheet)
  required_cols <- c("Parameter", value_col)
  missing_cols <- setdiff(required_cols, names(params_df))
  if (length(missing_cols) > 0) {
    stop("Simulation parameter sheet must contain columns: ", paste(required_cols, collapse = ", "),
         ". Missing: ", paste(missing_cols, collapse = ", "))
  }
  params_df <- params_df[!is.na(params_df$Parameter), ]
  params_df$Parameter <- trimws(as.character(params_df$Parameter))
  params_df <- params_df[params_df$Parameter != "", ]
  vals <- params_df[[value_col]]
  if (is.list(vals)) vals <- unlist(vals)
  params_list <- setNames(as.list(vals), params_df$Parameter)
  params_names <- names(params_list)
  params_list <- lapply(seq_along(params_list), function(i) {
    nm <- params_names[i]
    v <- params_list[[i]]
    if (is.null(v)) return(NULL)
    v_str <- trimws(as.character(v))
    if (length(v) == 1 && (is.na(v) || v_str == "")) return(NULL)
    if (!coerce_types) return(v_str)
    if (nm == "allocation_ratio") {
      parts <- strsplit(v_str, ",")[[1]]
      v_num <- suppressWarnings(as.numeric(trimws(parts)))
      if (!any(is.na(v_num))) return(v_num)
    }
    if (nm == "gender_levels") {
      parts <- strsplit(v_str, ",")[[1]]
      return(trimws(parts))
    }
    bool_params <- c("empirical", "adherence_override")
    if (nm %in% bool_params) {
      v_clean <- tolower(trimws(v_str))
      if (v_clean %in% c("true", "yes", "1", "t")) {
        return(TRUE)
      } else if (v_clean %in% c("false", "no", "0", "f")) {
        return(FALSE)
      } else {
        v_bool <- suppressWarnings(as.logical(v_str))
        if (length(v_bool) == 1 && !is.na(v_bool)) return(v_bool)
      }
    }
    is_numeric_param <- any(vapply(numeric_params, function(p) grepl(p, nm), logical(1)))
    if (is_numeric_param) {
      v_str_clean <- gsub(",", "", v_str)
      if (v_str_clean %in% c("NA", "nan", "NaN")) return(NULL)
      v_num <- suppressWarnings(as.numeric(v_str_clean))
      if (length(v_num) == 1 && !is.na(v_num)) return(v_num)
    }
    v_str
  })
  names(params_list) <- params_names
  params_list <- params_list[!vapply(params_list, is.null, logical(1))]
  params_list
}

get_true_effects <- function(p) {
  n_coh <- as.integer(p$n_cohorts %||% 3)
  trt_pref <- p$treatment_prefix %||% "Treatment_"
  trt_levels <- paste0(trt_pref, seq_len(n_coh))
  direct_effects <- sapply(seq_len(n_coh), function(i) {
    as.numeric(p[[paste0("trt", i, "_outcome_effect")]] %||% 0)
  })
  names(direct_effects) <- trt_levels
  rel_effects <- direct_effects - direct_effects[1]
  names(rel_effects) <- trt_levels
  return(rel_effects)
}
