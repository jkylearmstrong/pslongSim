
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pslongSim

<!-- badges: start -->

[![R-CMD-check](https://github.com/jkylearmstrong/pslongSim/actions/workflows/r.yml/badge.svg)](https://github.com/jkylearmstrong/pslongSim/actions/workflows/r.yml)
[![CRAN
status](https://www.r-pkg.org/badges/version/pslongSim)](https://CRAN.R-project.org/package=pslongSim)
<!-- badges: end -->

The goal of **pslongSim** is to provide a robust simulation framework
for evaluating propensity score methods for adjustment of time-varying
non-adherence in longitudinal randomized clinical trials.

For details on the architecture and division of labor between
`pslongSim` and `MLPScore`, see
[SEPARATION_NOTES.md](SEPARATION_NOTES.md).

## Installation

You can install the development version of pslongSim from
[GitHub](https://github.com/jkylearmstrong/pslongSim) with:

``` r
# install.packages("devtools")
devtools::install_github("jkylearmstrong/pslongSim")
```

## Dependencies

This package has a development dependency on **MLPScore** for running
the methods comparison pipeline. It is specified in `Suggests` and will
be resolved if you install with development tools:

``` r
# Install MLPScore from GitHub first
devtools::install_github("jkylearmstrong-temple/ML-PScore")
```

Dependency contract: `pslongSim` → `MLPScore` is *Suggests-only*;
`MLPScore` has no dependency on `pslongSim` in any DESCRIPTION field. The
graph is acyclic by construction and guarded by
`tests/testthat/test-separation.R`.

### Baseline Estimators

`pslongSim` intentionally ships a small set of simple reference
estimators (`fit_ps_tidymodels()`, `compute_stabilized_iptw()`,
`fit_msm_gee_cont()`, `fit_msm_gee_bin()`, `fit_msm_cox()`) so simulation
sweeps are self-contained without `MLPScore` installed. These are
benchmarking baselines — for advanced estimation (cross-fitting, TWANG,
AIPW, diagnostics) use `MLPScore`.

## Running the Simulation

This project uses the `targets` R package for pipeline management. The
default pipeline lives in the package. To run the full simulation suite:

``` r
targets::tar_make()
```

This will:

- Generate synthetic patient demographics and longitudinal data (binary,
  continuous, and time-to-event).
- Fit propensity score models using GLM, Random Forest, and XGBoost.
- Compute stabilized IPTW weights.
- Fit Marginal Structural Models (MSM) and summarise results.

## Modifying the Experiment

You can modify the simulation parameters by editing the
`make_default_simulation_targets()` call in `_targets.R`:

- **Sample size**: Change `num_patients`, `num_visits`, and
  `tte_visits`.
- **Treatment effects**: Adjust `adh_shift`, `out_effect`, `logHR`, and
  `se_shift`.
- **Models**: Switch between different ML models with the `ps_models`
  argument.
- **Adherence model**: Set `adherence_type = "beta"` for continuous
  \[0,1\] adherence or `"binary"` for 0/1 compliance. Beta adherence
  produces more realistic dose-response patterns.

## Non-Linear Feature

When `non_linear_feature = TRUE`, interaction terms (e.g.,
`Age * SideEffect`) are added to the outcome with no additive main
effects. Linear models cannot detect this signal, while tree-based
models (random forest, XGBoost) capture it naturally:

``` r
library(pslongSim)
demo <- generate_patient_data_demographic(num_patients = 1000, seed = 1)
trt_map <- suppressWarnings(define_treatment_map(levels(demo$Treatment)))
dat <- generate_longitudinal_data(
  demo, num_visits = 6, trt_map = trt_map,
  adherence_type = "beta", outcome_type = "continuous",
  non_linear_feature = TRUE,
  nonlin_coefs = c("Age:SideEffect" = 0.20)
)

lm_fit <- lm(Outcome ~ Treatment + VisitNumber + Age + SideEffect, data = dat)
rf_fit <- randomForest::randomForest(Outcome ~ Treatment + VisitNumber + Age + SideEffect, data = dat)

summary(lm_fit)$r.squared   # low
1 - rf_fit$mse[200] / var(dat$Outcome)  # substantially higher
```

Multiple interactions can be specified via `nonlin_coefs`:

``` r
nonlin_coefs = c("Age:SideEffect" = 0.15, "Biomarker:Age" = 0.10)
```

See `vignette("getting-started")` for a full walkthrough.

## R Package Structure

- `R/`: Core simulation and analysis functions.
- `tests/`: Unit tests for ensuring reliability.
- `vignettes/`: Package vignettes.
- `_targets.R`: Loads the packaged default simulation pipeline.

## Results Summary

After running `tar_make()`, you can inspect results with:

``` r
targets::tar_read(summary_cont)
targets::tar_read(summary_bin)
targets::tar_read(summary_cox)
```
