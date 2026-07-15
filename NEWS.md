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
