# Separation of Concerns: pslongSim vs. MLPScore

This repository (**pslongSim**) serves as the single source of truth for simulating clinical trial datasets with longitudinal propensity scores, side-effects, biomarkers, and various outcome types.

## Division of Concerns

* **pslongSim Package**: Solely responsible for simulating patient cohorts, demographics, longitudinal trial visits, adherence/compliance metrics, and structural models.
* **MLPScore Package**: Solely responsible for advanced machine learning propensity score estimation (cross-fitting, clipping, twang, etc.), diagnostics, and comparative assessment.
* **Orchestration**: The `mlpscore_pipeline/` subdirectory inside `pslongsim` acts as a consumer. It generates datasets using the `pslongSim` simulation engine, feeds them into the `MLPScore` estimation library, and compiles comparative figures and Quarto reports.

## Architecture

```
                       ┌─────────────────────────┐
                       │   pslongSim (Package)   │
                       │  - Data Generator       │
                       └────────────┬────────────┘
                                    │
                                    ▼ (generates data)
  ┌───────────────────┐    ┌─────────────────┐
  │ MLPScore (Package)│───>│  mlpscore_pipeline│
  │ - PS Estimations  │    │  (Orchestration)│
  └───────────────────┘    └─────────────────┘
```

## Baseline Estimators (Intentional Overlap)

`pslongSim` deliberately retains a small set of **simple reference estimators** —
`fit_ps_tidymodels()`, `compute_stabilized_iptw()`, `fit_msm_gee_cont()`,
`fit_msm_gee_bin()`, and `fit_msm_cox()` — so that simulation sweeps are
self-contained and can benchmark generated data without requiring `MLPScore`.
These are not duplicates of `MLPScore` functionality: `compute_stabilized_iptw()`
is a longitudinal cumulative-product IPTW, whereas `MLPScore::compute_stabilized_weights()`
is cross-sectional with clipping diagnostics. All advanced estimation
(cross-fitting, TWANG, AIPW, diagnostics) lives exclusively in `MLPScore`.

## Dependency Direction

* `pslongSim` → `MLPScore`: **Suggests only** (soft; used by `mlpscore_pipeline/`).
* `MLPScore` → `pslongSim`: **none** (no DESCRIPTION reference in either direction that could cycle).
* `mlpscore_pipeline/` (this repo) is the **single** orchestration point; the
  legacy pipeline scripts in the ML-PScore repo have been moved to its
  `analysis/` folder and are excluded from that package's build.

## Migration Checklist & Status

- [x] **Add comparative targets pipeline**: Created `mlpscore_pipeline/` subdirectory to contain the orchestration logic.
- [x] **Migrate Excel simulation parameters**: Relocated `simulation_params.xlsx` to `mlpscore_pipeline/`.
- [x] **Verification**: Ran the entire targets pipeline (`_targets_mlpscore.R` and `run_simulation.R`) successfully, rendering the comparative reports/figures in PDF.
- [x] **Unit Testing**: Verified that 100% of the 175 tests in `pslongsim` pass with zero failures.
