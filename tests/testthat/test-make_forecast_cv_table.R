test_that("make_forecast_cv_table builds detail + summary and flags/pools correctly", {
  set.seed(1)
  isos    <- c("NGA", "MOZ", "COD")
  cutoffs <- as.Date(c("2024-01-01", "2024-07-01", "2025-01-01"))
  hz      <- c(1, 2, 3)
  grid <- expand.grid(iso_code = isos, cutoff_date = cutoffs, metric = c("cases", "deaths"),
                      h = hz, stringsAsFactors = FALSE)
  cells <- data.frame(
    run_id      = "r", model = "ensemble_opt",
    iso_code    = grid$iso_code, cutoff_date = grid$cutoff_date, metric = grid$metric,
    window      = sprintf("OOS<=%dmo", grid$h),
    n           = 30L,
    R2_corr     = runif(nrow(grid)), R2_sse = -runif(nrow(grid)),
    bias_ratio  = runif(nrow(grid), 0.7, 1.3),
    cov50 = runif(nrow(grid)), cov95 = runif(nrow(grid)),
    mae_skill_seasonal = runif(nrow(grid), -0.2, 0.5),
    wis_skill_seasonal = runif(nrow(grid), -0.2, 0.5),
    ess = 100, ess_ok = TRUE, stringsAsFactors = FALSE)

  out <- make_forecast_cv_table(cells, primary_horizon = 3)
  expect_named(out, c("detail", "summary"))
  # detail: one row per iso x cutoff x horizon x metric
  expect_equal(nrow(out$detail), 3L * 3L * 3L * 2L)
  expect_true(all(c("train_years", "exploratory", "near_cast", "wis_skill") %in% names(out$detail)))
  # NGA flagged exploratory; deaths flagged near-cast
  expect_true(all(out$detail$exploratory[out$detail$country == "NGA"]))
  expect_false(any(out$detail$exploratory[out$detail$country == "MOZ"]))
  expect_true(all(out$detail$near_cast[out$detail$metric == "deaths"]))
  # summary at primary horizon: per-country + pooled, pooled excludes NGA
  expect_true(all(out$summary$horizon_mo == 3))
  pooled <- out$summary[out$summary$scope == "pooled" & out$summary$metric == "cases", ]
  expect_equal(pooled$n_origins, 6L)      # MOZ(3) + COD(3), NGA excluded
  expect_true("wis_skill_median" %in% names(out$summary))
})

test_that("make_forecast_cv_table writes files and reads a path", {
  skip_if_not_installed("arrow")
  cells <- data.frame(run_id = "r", model = "ensemble_opt", iso_code = "MOZ",
    cutoff_date = as.Date("2024-01-01"), metric = "cases", window = "OOS<=3mo",
    n = 30L, R2_corr = 0.5, R2_sse = -1, bias_ratio = 1, cov50 = 0.4, cov95 = 0.8,
    mae_skill_seasonal = 0.3, wis_skill_seasonal = 0.4, ess = 100, ess_ok = TRUE,
    stringsAsFactors = FALSE)
  dir <- withr::local_tempdir()
  out <- make_forecast_cv_table(cells, dir_output = dir)
  expect_true(file.exists(file.path(dir, "forecast_cv_table_detail.parquet")))
  expect_true(file.exists(file.path(dir, "forecast_cv_table_summary.parquet")))
})
