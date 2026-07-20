#!/usr/bin/env Rscript
# pslongSim Release Workflow
# Run: Rscript release_workflow.R

cat("\n========== PSLONGSIM 10-STEP RELEASE WORKFLOW ==========\n\n")

# Get current version
desc <- read.dcf("DESCRIPTION")
current_version <- desc[1, "Version"]
cat("Current version:", current_version, "\n\n")

# Step 1: Code changes & manual verification
cat("STEP 1: Code changes + manual verification\n")
cat("------\n")
cat("Actions:\n")
cat("  [ ] Make code changes\n")
cat("  [ ] Manual testing: run targets::tar_make() for the default pipeline\n")
cat("  [ ] Verify simulation results align with expectations\n")
cat("  [ ] Check git status: git status\n\n")

# Step 2: Add/update tests
cat("STEP 2: Add/update tests\n")
cat("------\n")
cat("Actions:\n")
cat("  [ ] Review test/ directory\n")
cat("  [ ] Add tests for new functions\n")
cat("  [ ] Update snapshots if needed: testthat::snapshot_update()\n")
cat("  [ ] Run: devtools::test()\n")
cat("  [ ] Check coverage: covr::package_coverage()\n\n")

# Step 3: Run full test suite
cat("STEP 3: Run full test suite\n")
cat("------\n")
cat("  [ ] Run: devtools::check()\n")
cat("  [ ] Fix any ERRORS, WARNINGS, or NOTES\n")
cat("  [ ] Confirm all tests pass\n\n")

# Step 4: Update NEWS & README.Rmd
cat("STEP 4: Update NEWS & README.Rmd (quarto::quarto_render)\n")
cat("------\n")
cat("Actions:\n")
cat("  [ ] Edit NEWS.md with major changes/fixes in this release\n")
cat("  [ ] Edit README.Rmd with updates to examples/documentation\n")
cat("  [ ] Render README: quarto::quarto_render('README.Rmd')\n")
cat("      (or: rmarkdown::render('README.Rmd') for standard Rmd)\n")
cat("  [ ] Commit: git add NEWS.md README.* && git commit -m 'Update NEWS and README'\n\n")

# Step 5: Update version
cat("STEP 5: Update version\n")
cat("------\n")
cat("Suggested new version (MAJOR.MINOR.PATCH):\n")
cat("  Current: ", current_version, "\n")
cat("  Next candidates:\n")
cat("    - Patch (bug fixes):    0.3.7\n")
cat("    - Minor (features):     0.4.0\n")
cat("    - Major (breaking):     1.0.0\n\n")
cat("Actions:\n")
cat("  [ ] Edit DESCRIPTION: change Version = 'X.Y.Z'\n")
cat("  [ ] Commit: git add DESCRIPTION && git commit -m 'Bump version to X.Y.Z'\n\n")

# Step 6: Document + pkgdown
cat("STEP 6: Document + pkgdown\n")
cat("------\n")
cat("Actions:\n")
cat("  [ ] Update roxygen: devtools::document()\n")
cat("  [ ] Build site: pkgdown::build_site()\n")
cat("  [ ] Review docs/ directory\n")
cat("  [ ] Commit: git add man/ NAMESPACE docs/ && git commit -m 'Update documentation'\n\n")

# Step 7: Build + run checks
cat("STEP 7: Build + run checks\n")
cat("------\n")
cat("Actions:\n")
cat("  [ ] devtools::check(remote = TRUE, manual = TRUE)\n")
cat("  [ ] Confirm no ERRORS or WARNINGS\n")
cat("  [ ] Build tarball: devtools::build()\n")
cat("  [ ] Verify tarball can be installed: R CMD INSTALL <tarball>\n\n")

# Step 8: Commit
cat("STEP 8: Commit\n")
cat("------\n")
cat("  [ ] Review all changes: git log --oneline (last 5 commits)\n")
cat("  [ ] Confirm all staged: git add -A && git status\n")
cat("  [ ] Final commit (if any unstaged changes): git commit -m 'Release prep X.Y.Z'\n\n")

# Step 9: Tag (if releasing)
cat("STEP 9: Tag (if releasing)\n")
cat("------\n")
cat("Actions:\n")
cat("  [ ] Create annotated tag: git tag -a v<VERSION> -m 'Release version <VERSION>'\n")
cat("  [ ] Example: git tag -a v0.3.7 -m 'Release version 0.3.7'\n")
cat("  [ ] View: git tag -l\n\n")

# Step 10: Push + push tags
cat("STEP 10: Push + push tags\n")
cat("------\n")
cat("Actions:\n")
cat("  [ ] Push commits: git push origin main\n")
cat("  [ ] Push tags: git push origin --tags\n")
cat("  [ ] Verify on GitHub: browse to https://github.com/jkylearmstrong/pslongSim/releases\n")
cat("  [ ] (Optional) Submit to CRAN if appropriate\n\n")

cat("========== RELEASE WORKFLOW COMPLETE ==========\n\n")
