test_that(".mosaic_active_subset_cols returns tier columns when flag is off", {
  r <- data.frame(is_best_subset = c(TRUE, FALSE), weight_best = c(0.5, 0))
  ctrl <- list(predictions = list(optimize_subset = FALSE))
  res <- MOSAIC:::.mosaic_active_subset_cols(r, ctrl)
  expect_equal(res$subset_col, "is_best_subset")
  expect_equal(res$weight_col, "weight_best")
  expect_equal(res$source, "tier")
})

test_that(".mosaic_active_subset_cols returns tier columns when flag is on but _opt missing", {
  r <- data.frame(is_best_subset = c(TRUE, FALSE), weight_best = c(0.5, 0))
  ctrl <- list(predictions = list(optimize_subset = TRUE))
  res <- MOSAIC:::.mosaic_active_subset_cols(r, ctrl)
  expect_equal(res$source, "tier")
  expect_equal(res$subset_col, "is_best_subset")
})

test_that(".mosaic_active_subset_cols returns optimized columns when flag is on and _opt present", {
  r <- data.frame(
    is_best_subset     = c(TRUE, TRUE, FALSE),
    weight_best        = c(0.5, 0.5, 0),
    is_best_subset_opt = c(TRUE, FALSE, FALSE),
    weight_best_opt    = c(1.0, 0, 0)
  )
  ctrl <- list(predictions = list(optimize_subset = TRUE))
  res <- MOSAIC:::.mosaic_active_subset_cols(r, ctrl)
  expect_equal(res$subset_col, "is_best_subset_opt")
  expect_equal(res$weight_col, "weight_best_opt")
  expect_equal(res$source, "optimized")
})

test_that(".mosaic_active_subset_cols falls back to tier when _opt columns all FALSE", {
  r <- data.frame(
    is_best_subset     = c(TRUE, FALSE),
    weight_best        = c(0.5, 0),
    is_best_subset_opt = c(FALSE, FALSE),
    weight_best_opt    = c(0, 0)
  )
  ctrl <- list(predictions = list(optimize_subset = TRUE))
  res <- MOSAIC:::.mosaic_active_subset_cols(r, ctrl)
  expect_equal(res$source, "tier")
})

test_that(".mosaic_active_subset_cols treats NULL control$predictions as flag-off", {
  r <- data.frame(is_best_subset = c(TRUE, FALSE), weight_best = c(0.5, 0))
  ctrl <- list()
  res <- MOSAIC:::.mosaic_active_subset_cols(r, ctrl)
  expect_equal(res$source, "tier")
})

test_that(".mosaic_active_subset_cols handles NA values in is_best_subset_opt", {
  # NA mixed with TRUE -> any() with na.rm = TRUE returns TRUE -> optimized path
  r_mixed <- data.frame(
    is_best_subset     = c(TRUE, TRUE, FALSE),
    weight_best        = c(0.5, 0.5, 0),
    is_best_subset_opt = c(NA, TRUE, FALSE),
    weight_best_opt    = c(0, 1.0, 0)
  )
  ctrl <- list(predictions = list(optimize_subset = TRUE))
  expect_equal(MOSAIC:::.mosaic_active_subset_cols(r_mixed, ctrl)$source, "optimized")

  # All NA -> any() with na.rm = TRUE returns FALSE -> tier fallback
  r_allna <- data.frame(
    is_best_subset     = c(TRUE, FALSE),
    weight_best        = c(0.5, 0),
    is_best_subset_opt = c(NA, NA),
    weight_best_opt    = c(0, 0)
  )
  expect_equal(MOSAIC:::.mosaic_active_subset_cols(r_allna, ctrl)$source, "tier")
})
