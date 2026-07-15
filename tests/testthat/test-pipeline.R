test_that("default simulation pipeline helper returns the full target list", {
  skip_if_not_installed("targets")
  targets <- make_default_simulation_targets()
  expect_type(targets, "list")
  expect_length(targets, 17L)
})

test_that("pipeline parameter naming is consistent", {
  f <- formals(make_default_simulation_targets)
  expect_true("num_patients" %in% names(f))
  expect_true("num_visits" %in% names(f))
  expect_false("n_patients" %in% names(f))
  expect_false("n_visits" %in% names(f))
})

test_that("pipeline validates inputs", {
  expect_error(make_default_simulation_targets(num_patients = 0),
               "must be positive")
  expect_error(make_default_simulation_targets(num_visits = -1),
               "must be positive")
})

test_that("pipeline accepts non_linear_feature and nonlin_coefs parameters", {
  skip_if_not_installed("targets")
  targets <- make_default_simulation_targets(non_linear_feature = FALSE)
  expect_length(targets, 17L)

  targets2 <- make_default_simulation_targets(
    non_linear_feature = TRUE,
    nonlin_coefs = c("Age:SideEffect" = 0.20)
  )
  expect_length(targets2, 17L)
})
