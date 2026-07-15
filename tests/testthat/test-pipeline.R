test_that("default simulation pipeline helper returns the full target list", {
  targets <- make_default_simulation_targets()

  expect_type(targets, "list")
  expect_length(targets, 17L)
})
