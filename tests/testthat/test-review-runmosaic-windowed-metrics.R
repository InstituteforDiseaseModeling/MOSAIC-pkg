# =============================================================================
# test-review-runmosaic-windowed-metrics.R
#
# model_fit_windows.csv was computed on location-interleaved flattened vectors
# indexed against an n_time date vector: multi-location runs got NA dates and
# "last_<w>obs" windows spanning ~w/n_loc days. Windows are now over time.
# =============================================================================

.wm_series <- function(n_loc, n_t, seed = 1) {
  set.seed(seed)
  obs <- matrix(stats::rpois(n_loc * n_t, 50), n_loc, n_t)
  est <- obs * stats::runif(n_loc * n_t, 0.8, 1.2)
  list(obs = obs, est = est,
       dates = seq.Date(as.Date("2023-01-01"), by = "day", length.out = n_t))
}

test_that("multi-location windows are dated and span w time steps", {
  s <- .wm_series(3L, 400L)
  wm <- MOSAIC:::.mosaic_compute_windowed_metrics(s$obs, s$est, s$obs, s$est, s$dates,
                                                  windows = c(365, 30))
  expect_identical(wm$window, c("full", "last_365obs", "last_30obs"))
  expect_false(anyNA(wm$date_start)); expect_false(anyNA(wm$date_end))
  expect_identical(wm$date_end, rep("2024-02-04", 3))
  expect_identical(wm$date_start[3], as.character(s$dates[371]))
  expect_identical(wm$n_obs, c(1200L, 1095L, 90L))
  # The 30-step window pools all three locations over the last 30 days.
  sel <- 371:400
  expect_equal(wm$r2_cases[3],
               round(calc_model_R2(as.numeric(s$obs[, sel]), as.numeric(s$est[, sel])), 4))
})

test_that("a single location matches the last-w-observation definition", {
  s <- .wm_series(1L, 200L, seed = 2)
  obs <- s$obs; obs[1, c(190, 195)] <- NA
  wm <- MOSAIC:::.mosaic_compute_windowed_metrics(as.numeric(obs), as.numeric(s$est),
                                                  as.numeric(obs), as.numeric(s$est),
                                                  s$dates, windows = 30)
  idx <- utils::tail(which(is.finite(obs[1, ])), 30)
  expect_identical(wm$n_obs[2], 30L)
  expect_identical(wm$date_start[2], as.character(s$dates[min(idx)]))
  expect_equal(wm$r2_cases[2], round(calc_model_R2(obs[1, idx], s$est[1, idx]), 4))
})

test_that("cells whose estimate is masked are not counted as scored", {
  s <- .wm_series(2L, 100L, seed = 3)
  est <- s$est; est[, 1:10] <- NA   # scoring-masked head
  wm <- MOSAIC:::.mosaic_compute_windowed_metrics(s$obs, est, s$obs, est, s$dates,
                                                  windows = integer(0))
  expect_identical(wm$n_obs, 180L)
  expect_identical(wm$date_start, as.character(s$dates[11]))
})
