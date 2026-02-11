#' Define Treatment Relationship Map
#'
#' @param levels Character vector of treatment levels.
#' @param adh_shift Numeric vector: logit shift for adherence.
#' @param out_effect Numeric vector: main effect for outcome (continuous/binary).
#' @param logHR Numeric vector: log-hazard ratio for TTE.
#' @param se_shift Numeric vector: logit shift for side-effect risk.
#' @return A data.frame with one row per treatment level.
#' @export
define_treatment_map <- function(levels,
                                 adh_shift = 0,
                                 out_effect = 0,
                                 logHR = 0,
                                 se_shift = 0) {
  stopifnot(length(levels) > 0)
  L <- length(levels)
  df <- data.frame(
    Treatment = factor(levels, levels = levels),
    adh_shift = rep(adh_shift, length.out = L),
    out_effect = rep(out_effect, length.out = L),
    logHR = rep(logHR, length.out = L),
    se_shift = rep(se_shift, length.out = L),
    stringsAsFactors = FALSE
  )
  rownames(df) <- as.character(df$Treatment)
  df
}
