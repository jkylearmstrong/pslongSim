# tests/testthat/test-separation.R
# ---------------------------------------------------------------------------
# Guards for the pslongSim/MLPScore separation (v0.3.7):
#  - MLPScore may only ever be a Suggests dependency (soft), never
#    Depends/Imports — this keeps the dependency graph acyclic.
#  - The intentional baseline estimators remain exported so simulation
#    sweeps stay self-contained without MLPScore installed.
# ---------------------------------------------------------------------------

pkg_root <- function() {
    for (p in c(".", "../..", "../../..")) {
        if (file.exists(file.path(p, "DESCRIPTION"))) return(normalizePath(p))
    }
    testthat::skip("Package root not accessible")
}

test_that("MLPScore is at most a soft (Suggests) dependency", {
    d <- read.dcf(file.path(pkg_root(), "DESCRIPTION"))
    for (field in c("Depends", "Imports", "LinkingTo")) {
        if (field %in% colnames(d)) {
            expect_false(grepl("MLPScore", d[1, field]),
                         info = paste0("MLPScore must not be in ", field))
        }
    }
})

test_that("baseline estimators remain exported", {
    ns <- readLines(file.path(pkg_root(), "NAMESPACE"), warn = FALSE)
    baseline <- c("fit_ps_tidymodels", "compute_stabilized_iptw",
                  "fit_msm_gee_cont", "fit_msm_gee_bin", "fit_msm_cox")
    for (fn in baseline) {
        expect_true(any(grepl(paste0("export\\(", fn, "\\)"), ns)),
                    info = paste0(fn, " must stay exported (baseline estimator)"))
    }
})
