# TODO — pslongSim

Observations from the v0.3.7 separation review, roughly prioritized.

## Feature requests (Dr. Zhao) — design notes

All three belong in this package (DGP properties). Sharpened after code review of
`R/simulate.R` (v0.3.7):

- [x] **Non-linearity in the *adherence* model** (highest value). Added `nonlin_coefs_adherence` argument that injects non-linear terms into `lp_adh`.
- [x] **Basis-breaking non-linear forms.** Extended `nonlin_coefs` and `nonlin_coefs_adherence` with thresholds (`"step(Age,65):SideEffect"`), quadratics/U-shapes (`"I(Age^2)"`), and XOR-style region effects (`"xor(SideEffect, Compliant)"`).
- [x] **Treatment-confounder feedback** (the defining feature of longitudinal confounding). Added `biom_model$comp_feedback` and `se_model$comp_feedback` coefficients so prior adherence shifts future covariates.
- [x] **Higher-order interactions**: extended `nonlin_coefs` parsing to support 3-way interactions (`"A:B:C"`).
- [x] Implementation rules per README Development section: off by default, named `NL_*` columns, invariant tests, scenario knob in `mlpscore_pipeline/_targets_mlpscore.R`.

## Hygiene

- [x] **Stop CRLF churn**: added `.gitattributes` with `* text=auto`.
- [x] **Regenerate `man/`** with `devtools::document()`.
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

- [x] `mlpscore_pipeline/` startup check added in `_targets_mlpscore.R` with a clear install message when MLPScore is absent.
- [ ] Consider promoting `mlpscore_pipeline/` to its own repo if a third consumer of
      either package appears (not warranted at two packages).

## Testing

- [ ] Add CI via `usethis::use_github_action("check-standard")`.
- [x] Property-based checks for `generate_longitudinal_data()` invariants (monotone visit numbers, weight positivity, adherence in [0,1]).
