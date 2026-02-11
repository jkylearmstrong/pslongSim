# pslongSim 0.1.2

* Updated maintainer email to `j.kyle.armstrong@temple.edu` in `DESCRIPTION`.

# pslongSim 0.1.1

* Fixed classification outcome error in `fit_ps_tidymodels()` by ensuring `Compliant` is converted to a factor.
* Updated `ps_model_spec()` to correctly handle optional tuning, allowing for non-tuned `xgboost` and `ranger` models.
* Improved `.gitignore` to exclude `_targets/` and simulation log files.
* Added unit tests for factor conversion and tuning logic.
