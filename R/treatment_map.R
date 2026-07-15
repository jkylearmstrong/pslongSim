#' Define Treatment Relationship Map
#'
#' Creates a data frame mapping treatment levels to their effect parameters
#' (adherence shift, outcome effect, log-hazard ratio, and side-effect shift).
#'
#' @param levels Character vector of treatment levels (must be non-empty).
#' @param adh_shift Numeric vector: logit shift for adherence. Recycled to
#'   match \code{length(levels)}; a length mismatch generates a warning.
#' @param out_effect Numeric vector: main effect for outcome (continuous/binary).
#'   Recycled with warning on length mismatch.
#' @param logHR Numeric vector: log-hazard ratio for TTE.
#'   Recycled with warning on length mismatch.
#' @param se_shift Numeric vector: logit shift for side-effect risk.
#'   Recycled with warning on length mismatch.
#' @return A data.frame with one row per treatment level and rownames equal to
#'   the treatment level strings.
#' @export
define_treatment_map <- function(levels,
                                 adh_shift = 0,
                                 out_effect = 0,
                                 logHR = 0,
                                 se_shift = 0) {
  stopifnot(
    "`levels` must be a non-empty character vector." = is.character(levels) && length(levels) > 0L
  )

  L <- length(levels)
  params <- list(adh_shift = adh_shift, out_effect = out_effect,
                 logHR = logHR, se_shift = se_shift)
  for (nm in names(params)) {
    if (length(params[[nm]]) != L) {
      warning(nm, " has length ", length(params[[nm]]),
              " but ", L, " treatment levels were specified; values will be recycled.",
              call. = FALSE)
    }
  }

  df <- data.frame(
    Treatment  = factor(levels, levels = levels),
    adh_shift  = rep(adh_shift, length.out = L),
    out_effect = rep(out_effect, length.out = L),
    logHR      = rep(logHR, length.out = L),
    se_shift   = rep(se_shift, length.out = L),
    stringsAsFactors = FALSE
  )
  rownames(df) <- as.character(df$Treatment)
  df
}
