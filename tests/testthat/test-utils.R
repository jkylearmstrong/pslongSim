test_that("inv_logit returns valid probabilities", {
  expect_equal(inv_logit(0), 0.5)
  expect_true(all(inv_logit(-10:10) >= 0 & inv_logit(-10:10) <= 1))
  expect_equal(inv_logit(-Inf), 0)
  expect_equal(inv_logit(Inf), 1)
})

test_that("clamp constrains values to [lower, upper]", {
  expect_equal(clamp(-1:5, 0, 3), c(0, 0, 1, 2, 3, 3, 3))
  expect_equal(clamp(5, 0, 10), 5)
  expect_error(clamp(1, lower = 5, upper = 2), "must be <=")
})

test_that("validate_columns errors on missing columns", {
  df <- data.frame(a = 1, b = 2)
  expect_error(pslongSim:::validate_columns(df, c("a", "c"), "test_fn"), "missing required column")
})

test_that("validate_columns passes on present columns", {
  df <- data.frame(a = 1, b = 2)
  expect_invisible(pslongSim:::validate_columns(df, c("a", "b"), "test_fn"))
})

test_that("validate_min_rows errors when too few rows", {
  df <- data.frame(a = 1:3)
  expect_error(pslongSim:::validate_min_rows(df, 10, "test_fn"), "requires at least 10")
})

test_that("require_suggested errors for missing package", {
  expect_error(pslongSim:::require_suggested("nonexistent_package_xyz"),
               "required but not installed")
})
