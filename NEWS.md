# pslongSim 0.2.0

* Packaged the default longitudinal propensity score simulation pipeline as a reusable function `make_default_simulation_targets()`.
* Streamlined the repository-level `_targets.R` configuration file to invoke the packaged pipeline directly.
* Configured GitHub Actions CI workflow to verify package integrity and run unit tests on push and pull requests on modern R releases.
* Updated `.Rbuildignore` to ignore top-level development and pipeline directories (like `_targets/` and `mlpscore_pipeline/`) to ensure clean packages.
* Updated unit tests to verify the pipeline builder structure.

# pslongSim 0.1.2

* Updated maintainer email to `j.kyle.armstrong@temple.edu` in `DESCRIPTION`.

# pslongSim 0.1.1

* Fixed classification outcome error in `fit_ps_tidymodels()` by ensuring `Compliant` is converted to a factor.
* Updated `ps_model_spec()` to correctly handle optional tuning, allowing for non-tuned `xgboost` and `ranger` models.
* Improved `.gitignore` to exclude `_targets/` and simulation log files.
* Added unit tests for factor conversion and tuning logic.
