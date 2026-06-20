# Tests for the metrics-only engine-artifact mask used by run_MOSAIC scoring.
# .mosaic_mask_central_for_scoring() sets artifact time-positions (columns) to NA
# on the EST central matrix so calc_model_R2()/calc_bias_ratio() drop them
# pairwise (both na_rm = TRUE by default). Obs stays unmasked.

mask_fn <- MOSAIC:::.mosaic_mask_central_for_scoring

test_that("cases mask blanks the first N columns", {
  m <- matrix(1:12, nrow = 2, byrow = TRUE)  # 2 x 6, columns = time
  out <- mask_fn(m, "cases", list(cases_warmup = 2L, deaths_final = TRUE))
  expect_true(all(is.na(out[, 1:2])))
  expect_false(any(is.na(out[, 3:6])))
  # untouched columns preserve values exactly
  expect_equal(out[, 3:6], m[, 3:6])
})

test_that("deaths mask blanks only the last column", {
  m <- matrix(1:12, nrow = 2, byrow = TRUE)  # 2 x 6
  out <- mask_fn(m, "deaths", list(cases_warmup = 2L, deaths_final = TRUE))
  expect_true(all(is.na(out[, 6])))
  expect_false(any(is.na(out[, 1:5])))
  expect_equal(out[, 1:5], m[, 1:5])
})

test_that("NULL spec falls back to default (cases_warmup=2, deaths_final=TRUE)", {
  m <- matrix(1:12, nrow = 2, byrow = TRUE)
  out_c <- mask_fn(m, "cases", NULL)
  expect_true(all(is.na(out_c[, 1:2])))
  expect_false(any(is.na(out_c[, 3:6])))

  out_d <- mask_fn(m, "deaths", NULL)
  expect_true(all(is.na(out_d[, 6])))
  expect_false(any(is.na(out_d[, 1:5])))
})

test_that("cases_warmup = 0 disables the cases mask", {
  m <- matrix(1:12, nrow = 2, byrow = TRUE)
  out <- mask_fn(m, "cases", list(cases_warmup = 0L, deaths_final = TRUE))
  expect_false(any(is.na(out)))
  expect_equal(out, m)
})

test_that("deaths_final = FALSE disables the deaths mask", {
  m <- matrix(1:12, nrow = 2, byrow = TRUE)
  out <- mask_fn(m, "deaths", list(cases_warmup = 2L, deaths_final = FALSE))
  expect_false(any(is.na(out)))
  expect_equal(out, m)
})

test_that("cases_warmup clamps to ncol when too large", {
  m <- matrix(1:6, nrow = 1)  # 1 x 6
  out <- mask_fn(m, "cases", list(cases_warmup = 100L, deaths_final = TRUE))
  expect_true(all(is.na(out)))
})

test_that("works for n_loc = 1 (single-row matrix)", {
  m <- matrix(c(5, 6, 7, 8), nrow = 1)  # 1 x 4
  out_c <- mask_fn(m, "cases", list(cases_warmup = 1L, deaths_final = TRUE))
  expect_true(is.na(out_c[1, 1]))
  expect_equal(out_c[1, 2:4], m[1, 2:4])

  out_d <- mask_fn(m, "deaths", list(cases_warmup = 1L, deaths_final = TRUE))
  expect_true(is.na(out_d[1, 4]))
  expect_equal(out_d[1, 1:3], m[1, 1:3])
})

test_that("works for n_loc > 1 (masks the same columns across all rows)", {
  m <- matrix(1:15, nrow = 3, byrow = TRUE)  # 3 x 5
  out <- mask_fn(m, "cases", list(cases_warmup = 2L, deaths_final = TRUE))
  expect_true(all(is.na(out[, 1:2])))
  expect_equal(dim(out), c(3L, 5L))
})

test_that("vector input is coerced to a single-row matrix", {
  v <- c(10, 20, 30, 40)
  out <- mask_fn(v, "cases", list(cases_warmup = 1L, deaths_final = TRUE))
  expect_true(is.matrix(out))
  expect_equal(nrow(out), 1L)
  expect_true(is.na(out[1, 1]))
})

test_that("scoring regression: masked est differs from unmasked and equals drop-columns", {
  # Build a tiny 1 x 6 obs/est where the artifact columns (cases: cols 1-2;
  # deaths: col 6) hold deliberately WRONG est values, and the interior matches
  # obs closely. Masking should improve R2 / move bias toward 1 and exactly
  # equal the metric computed with those columns physically dropped.
  spec <- list(cases_warmup = 2L, deaths_final = TRUE)

  # ---- cases ----
  obs_c <- matrix(c(100, 110, 50, 52, 48, 51), nrow = 1)
  # interior (cols 3-6) tracks obs; cols 1-2 are wildly wrong warm-up spikes
  est_c <- matrix(c(900, 880, 49, 53, 47, 50), nrow = 1)

  cen_c_masked   <- as.numeric(mask_fn(est_c, "cases", spec))
  cen_c_unmasked <- as.numeric(est_c)
  obs_c_flat     <- as.numeric(obs_c)

  r2_masked   <- calc_model_R2(obs_c_flat, cen_c_masked)
  r2_unmasked <- calc_model_R2(obs_c_flat, cen_c_unmasked)
  # Reference: physically drop the artifact columns from BOTH series
  r2_dropped  <- calc_model_R2(obs_c_flat[3:6], cen_c_unmasked[3:6])

  expect_false(isTRUE(all.equal(r2_masked, r2_unmasked)))
  expect_equal(r2_masked, r2_dropped)

  bias_masked   <- calc_bias_ratio(obs_c_flat, cen_c_masked)
  bias_unmasked <- calc_bias_ratio(obs_c_flat, cen_c_unmasked)
  bias_dropped  <- calc_bias_ratio(obs_c_flat[3:6], cen_c_unmasked[3:6])
  expect_false(isTRUE(all.equal(bias_masked, bias_unmasked)))
  expect_equal(bias_masked, bias_dropped)

  # ---- deaths (last column is the artifact) ----
  obs_d <- matrix(c(5, 6, 7, 6, 5, 4), nrow = 1)
  est_d <- matrix(c(5, 6, 7, 6, 5, 999), nrow = 1)  # col 6 structural-zero stand-in

  cen_d_masked   <- as.numeric(mask_fn(est_d, "deaths", spec))
  cen_d_unmasked <- as.numeric(est_d)
  obs_d_flat     <- as.numeric(obs_d)

  bias_d_masked   <- calc_bias_ratio(obs_d_flat, cen_d_masked)
  bias_d_unmasked <- calc_bias_ratio(obs_d_flat, cen_d_unmasked)
  bias_d_dropped  <- calc_bias_ratio(obs_d_flat[1:5], cen_d_unmasked[1:5])

  expect_false(isTRUE(all.equal(bias_d_masked, bias_d_unmasked)))
  expect_equal(bias_d_masked, bias_d_dropped)
})
