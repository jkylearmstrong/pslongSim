test_that("define_treatment_map creates correct structure", {
  map <- define_treatment_map(
    levels = c("A", "B", "C"),
    adh_shift  = c(0, -0.5, -1.0),
    out_effect = c(0, 0.3, 0.7),
    logHR      = c(0, -0.2, -0.5),
    se_shift   = c(0, 0.2, 0.4)
  )
  expect_s3_class(map, "data.frame")
  expect_equal(nrow(map), 3)
  expect_equal(rownames(map), c("A", "B", "C"))
  expect_true("Treatment" %in% names(map))
  expect_equal(map["B", "adh_shift"], -0.5)
})

test_that("define_treatment_map errors on empty levels", {
  expect_error(define_treatment_map(character(0)), "non-empty")
})

test_that("define_treatment_map warns on recycling", {
  expect_warning(
    define_treatment_map(levels = c("A", "B", "C"), adh_shift = c(0, -1)),
    "will be recycled"
  )
})

test_that("define_treatment_map recycles scalar defaults without warning", {
  expect_warning(
    map <- define_treatment_map(levels = c("A", "B")),
    NA
  )
  expect_equal(map$adh_shift, c(0, 0))
  expect_equal(map$out_effect, c(0, 0))
})
