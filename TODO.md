# TODO — pslongSim

Observations from the v0.3.7 separation review, roughly prioritized.

## Hygiene

- [ ] **Stop CRLF churn**: add `.gitattributes` with `* text=auto` and run
      `git add --renormalize .` once (same issue as ML-PScore).
- [ ] **Regenerate `man/`** with `devtools::document()` after the v0.3.7 roxygen
      additions (baseline-estimator notes in `ps_tidymodels.R` / `weights_and_msm.R`).
- [ ] **`Remotes:` field must be removed before any CRAN submission** — CRAN rejects
      packages with `Remotes:`. Fine for GitHub-only installs; revisit if submitting.
- [ ] **Repo name vs package name**: folder/repo is `pslongsim`, package is `pslongSim`,
      DESCRIPTION URL says `jkylearmstrong/pslongSim` while MLPScore's Remotes points at
      `jkylearmstrong-temple/ML-PScore`. Unify owner + casing to avoid install confusion.

## Dependencies

- [ ] **Heavy tidymodels Imports** (`tune`, `dials`, `workflows`, `rsample`, `yardstick`)
      exist only to support `fit_ps_tidymodels()`. If the baseline estimators ever move
      to MLPScore, Imports could shrink to ~4 packages. Decided to keep for now
      (self-contained sims) — revisit if install weight becomes a problem.
- [ ] **renv.lock maintenance**: lockfile is committed; schedule periodic
      `renv::update()` + `renv::snapshot()` so the Docker image and lockfile don't drift.

## Pipeline

- [ ] `mlpscore_pipeline/` requires MLPScore at runtime but this is only documented in
      README — consider a startup check in `_targets_mlpscore.R` with a clear
      install message when MLPScore is absent.
- [ ] Consider promoting `mlpscore_pipeline/` to its own repo if a third consumer of
      either package appears (not warranted at two packages).

## Testing

- [ ] Add CI via `usethis::use_github_action("check-standard")`.
- [ ] Property-based checks for `generate_longitudinal_data()` invariants
      (monotone visit numbers, weight positivity, adherence in [0,1]).
