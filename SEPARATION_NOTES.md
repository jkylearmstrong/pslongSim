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
