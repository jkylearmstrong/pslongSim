# pslongSim 0.3.4

## Bug Fixes

* Added `.Rprofile` to `.Rbuildignore` — prevents `R CMD build` from
  including the renv activation script (which would fail on install
  because `renv/activate.R` is excluded).

## Documentation

* Added `@examples` to `inv_logit()`, `clamp()`, and
  `make_default_simulation_targets()` (all remaining exported functions
  without examples).
* Fixed `fit_msm_cox()` `@seealso` to include `fit_msm_gee_bin()`
  (was missing from the cross-reference triangle).

# pslongSim 0.3.3

## Improvements

* **Recycling warning silenced for scalar defaults**: `define_treatment_map()`
  no longer warns when a scalar default (e.g., `adh_shift = 0`) is recycled
  to match multiple treatment levels.  Only length > 1 mismatches trigger
  warnings.
* **Extracted `.fit_msm_gee()` internal helper**: `fit_msm_gee_cont()` and
  `fit_msm_gee_bin()` are now thin wrappers around a shared function,
  eliminating the duplicated GEE/tryCatch/validation code.
* **Removed redundant `require_suggested()` calls**: `geepack` and `survival`
  are in `Imports` (always installed), so the guards were unnecessary.
* **`@examples` added** to `fit_msm_gee_bin()` and `filter_at_risk()`.
* **`@seealso` cross-references** added to `fit_msm_gee_bin()` and
  `filter_at_risk()`.
* **Test skip guards**: `skip_if_not_installed("xgboost")`,
  `skip_if_not_installed("ranger")`, and
  `skip_if_not_installed("randomForest")` added to relevant tests so
  the suite passes without those optional packages.

## Documentation

* Vignette expanded with binary outcome, TTE/Cox MSM, and Ranger PS
  model sections.
* README updated with adherence model description (beta vs binary).

# pslongSim 0.3.2

## Improvements

* **`nonlin_coefs` validation**: malformed entries (e.g., `"Age+SideEffect"`
  instead of `"Age:SideEffect"`) now emit warnings instead of being silently
  skipped.  Missing variable names also generate informative warnings.
* **`sd_frailty` now used**: the `tte_model$sd_frailty` parameter (previously
  declared but unused) now controls the TTE frailty standard deviation.
  Previously `outcome_model$sd_subject` was used for both continuous/binary
  and TTE outcomes.
* **`xgboost` and `ranger` moved to Suggests**: these packages are only
  needed when `model = "xgboost"` or `model = "ranger"` is requested.
  The GLM path now works without them.
* **Reduced code duplication**: non-linear interaction column creation and
  linear-predictor addition extracted into `build_nl_columns()` and
  `add_nl_to_lp()` internal helpers.
* **`@examples` added** to all key exported functions: `generate_patient_data_demographic`,
  `generate_longitudinal_data`, `define_treatment_map`, `ps_recipe`,
  `ps_model_spec`, `fit_ps_tidymodels`, `compute_stabilized_iptw`,
  `fit_msm_gee_cont`, and `fit_msm_cox`.
* **`@seealso` cross-references** added to documentation for related functions.
* Removed stale `.pred_1` from `globalVariables()`.
* Removed redundant `stringsAsFactors = FALSE` from `define_treatment_map()`.
* Removed unnecessary `LazyData: true` from DESCRIPTION.
* Fixed stale `NL_Feature` reference in `example/minimal.R`.

## Testing

* Added tests for non-linear features in binary and TTE outcomes.
* Added tests for `nonlin_coefs` validation (malformed names, missing variables).
* Added edge-case tests for `clamp()` (NA, NaN, Inf, lower==upper, empty input).

# pslongSim 0.3.1

## Changes

* **`non_linear_feature` default changed to `FALSE`** for backward
  compatibility.  Explicitly set to `TRUE` to enable interaction generation.
* **New `nonlin_coefs` parameter** on `generate_longitudinal_data()` and
  `make_default_simulation_targets()`.  A named numeric vector of interaction
  coefficients (e.g., `c("Age:SideEffect" = 0.15)`).  Multiple interactions
  are supported; each creates an `NL_<VarA>_<VarB>` column.
* Removed hardcoded `NL_Feature` column; interactions are now generated
  dynamically from `nonlin_coefs` names.

## Infrastructure

* Modernised CI workflow: multi-OS matrix (macOS, Windows, Ubuntu),
  updated `actions/checkout` to v4, `r-lib/actions` to v2, RSPM enabled.
* Added `VignetteBuilder: knitr` and `pkgdown` site configuration
  (`_pkgdown.yml`).
* Added vignette `getting-started` demonstrating non-linear feature
  detection (linear regression vs random forest).
* Added `*.tar.gz` to `.gitignore` to exclude built tarballs.

# pslongSim 0.3.0

## New Features

* **Non-linear interaction feature** (`non_linear_feature` parameter): adds a
  multiplicative `Age * SideEffect` interaction to the outcome with no additive
  main effects.  This signal is undetectable by linear regression but readily
  detectable by tree-based models (random forest, XGBoost).  Enabled by default
  in `generate_longitudinal_data()` and `make_default_simulation_targets()`.
* `filter_at_risk()` utility: extracts at-risk observations from TTE interval
  data, replacing repeated `subset(df, AtRisk == 1)` calls.

## Improvements

* **Input validation**: all exported functions now validate inputs and raise
  informative errors on missing columns, wrong types, or insufficient rows.
* **Error handling**: MSM fitting functions (`fit_msm_gee_cont`,
  `fit_msm_gee_bin`, `fit_msm_cox`) now wrap model calls in `tryCatch()` and
  report the underlying error message.
* **Dynamic PS column extraction**: `fit_ps_tidymodels()` no longer hardcodes
  `.pred_1`; it dynamically identifies the positive-class probability column,
  supporting arbitrary factor level names.
* **Functional `outcome` parameter**: `ps_recipe()` and `fit_ps_tidymodels()`
  now accept a custom `outcome` column name and `predictors` vector, replacing
  the previously hardcoded formula.
* **Consistent parameter naming**: `make_default_simulation_targets()` now uses
  `num_patients` and `num_visits` (matching the simulation functions) instead of
  `n_patients` / `n_visits`.
* Removed unused `dplyr` imports (`filter`, `select`, `mutate`, `group_by`).
* Switched from `import` to `importFrom` for all tidymodels dependencies,
  reducing namespace pollution.
* Moved `targets` and `MASS` from `Imports` to `Suggests` (they are only
  needed for the pipeline entry point and the multivariate normal draw,
  respectively).
* Added `clamp()` validation: errors when `lower > upper` instead of silently
  returning incorrect values.
* Added recycling warning in `define_treatment_map()` when parameter vectors
  have mismatched lengths.
* Removed redundant `stringsAsFactors = FALSE` (default since R 4.0).
* Removed commented-out `install.packages("targets")` from `_targets.R`.

## Testing

* Comprehensive test suite: 40+ tests covering utilities, treatment maps,
  simulation (all 3 outcome types, non-linear feature), PS fitting (GLM,
  XGBoost, named factor levels), IPTW weights, MSM models, pipeline structure,
  and input validation edge cases.
* Added `skip_on_cran()` to slow ML-dependent tests.
* Added `test-nonlinear.R` verifying that the interaction feature is
  undetectable by additive linear models but captured by random forest.

## Documentation

* Updated roxygen2 documentation for all exported functions.
* Updated `example/minimal.R` to demonstrate the non-linear feature.
* Updated README.Rmd with non-linear feature description.

# pslongSim 0.2.0

* Packaged the default longitudinal propensity score simulation pipeline as a
  reusable function `make_default_simulation_targets()`.
* Streamlined the repository-level `_targets.R` configuration file to invoke
  the packaged pipeline directly.
* Configured GitHub Actions CI workflow to verify package integrity and run
  unit tests on push and pull requests on modern R releases.
* Updated `.Rbuildignore` to ignore top-level development and pipeline
  directories (like `_targets/` and `mlpscore_pipeline/`) to ensure clean
  packages.
* Updated unit tests to verify the pipeline builder structure.

# pslongSim 0.1.2

* Updated maintainer email to `j.kyle.armstrong@temple.edu` in `DESCRIPTION`.

# pslongSim 0.1.1

* Fixed classification outcome error in `fit_ps_tidymodels()` by ensuring
  `Compliant` is converted to a factor.
* Updated `ps_model_spec()` to correctly handle optional tuning, allowing for
  non-tuned `xgboost` and `ranger` models.
* Improved `.gitignore` to exclude `_targets/` and simulation log files.
* Added unit tests for factor conversion and tuning logic.
