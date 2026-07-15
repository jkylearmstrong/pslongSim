
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pslongSim

<!-- badges: start -->

<!-- badges: end -->

The goal of **pslongSim** is to provide a robust simulation framework
for evaluating propensity score methods for adjustment of time-varying
non-adherence in longitudinal randomized clinical trials.

## Installation

You can install the development version of pslongSim from
[GitHub](https://github.com/jkylearmstrong/pslongSim) with:

``` r
# install.packages("devtools")
devtools::install_github("jkylearmstrong/pslongSim")
```

## Running the Simulation

This project uses the `targets` R package for pipeline management. The
default pipeline now lives in the package, so this repository can act as
the home for the end-to-end simulation workflow. To run the full
simulation suite:

1.  Ensure all dependencies are installed (managed via `renv`).
2.  Run the following command in the R console:

``` r
targets::tar_make()
```

This will: - Generate synthetic patient demographics and longitudinal
data (binary, continuous, and time-to-event). - Fit propensity score
models using GLM, Random Forest, and XGBoost. - Compute stabilized IPTW
weights. - Fit Marginal Structural Models (MSM) and summarize results.

## Modifying the Experiment

You can modify the simulation parameters by editing the
`make_default_simulation_targets()` call in `_targets.R`:

- **Sample Size**: Change `n_patients`, `n_visits`, and `tte_visits`.
- **Treatment Effects**: Adjust the `adh_shift`, `out_effect`, `logHR`,
  and `se_shift` arguments.
- **Models**: Switch between different machine learning models with the
  `ps_models` argument.

## R Package Structure

- `R/`: Contains the core simulation and analysis functions.
- `tests/`: Unit tests for ensuring reliability.
- `_targets.R`: Loads the packaged default simulation pipeline.

## Results Summary

After running `tar_make()`, you can inspect the results using:

``` r
targets::tar_read(summary_cont)
targets::tar_read(summary_bin)
targets::tar_read(summary_cox)
```
