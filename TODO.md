# TODO — pslongSim

Observations from the v0.3.7 separation review, roughly prioritized.

## Feature requests (Dr. Zhao) — design notes

All three belong in this package (DGP properties). Sharpened after code review of
`R/simulate.R` (v0.3.7):

- [ ] **Non-linearity in the *adherence* model** (highest value). `nonlin_coefs`
      currently injects interactions only into the outcome (line ~361) and hazard
      (line ~384) linear predictors; `lp_adh` (line ~263) is purely linear-logistic,
      so the simulated propensity truth is a correctly-specified GLM and ML PS methods
      cannot demonstrate an advantage on the PS itself. Add a
      `nonlin_coefs_adherence` argument that injects terms into `lp_adh`.
- [ ] **Basis-breaking non-linear forms.** Pairwise products (`"Age:SideEffect"`)
      are detectable by logistic regression whenever the analyst includes the term.
      For genuinely ML-only signal, extend the `nonlin_coefs` grammar with:
      thresholds (`"step(Age,65):SideEffect"`), quadratics/U-shapes (`"I(Age^2)"`),
      and XOR-style region effects — tree-friendly, linear-hostile.
- [ ] **Treatment-confounder feedback** (the defining feature of longitudinal
      confounding). In the visit loop (~line 280), prior compliance affects only
      future *adherence* — never future biomarker or side-effect. Add e.g.
      `biom_model$comp_feedback` and `se_model$comp_feedback` coefficients so prior
      adherence shifts future covariates. Without this, the simulation cannot show
      why sequential weighting (MSM) beats naive adjustment.
- [ ] **Higher-order interactions**: extend `nonlin_coefs` parsing from `"A:B"` to
      `"A:B:C"` (3-way).
- [ ] Implementation rules per README Development section: off by default, named
      `NL_*` columns, invariant tests, scenario knob in
      `mlpscore_pipeline/_targets_mlpscore.R`.

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
