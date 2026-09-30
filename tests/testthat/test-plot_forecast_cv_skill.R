make_cells <- function(ess_ok = TRUE) {
  do.call(rbind, lapply(c("MOZ", "ETH", "NGA"), function(u)
    do.call(rbind, lapply(c("2024-06-01", "2024-12-01", "2025-06-01"), function(ct)
      data.frame(model = "ensemble", iso_code = u,
                 metric = c("cases", "deaths"), cutoff_date = ct, window = "OOS<=6mo",
                 R2_corr = runif(2), R2_sse = rnorm(2),
                 bias_ratio = runif(2, 0.2, 3),
                 wis_skill_seasonal = rnorm(2, 0.1, 0.3),
                 mae_skill_seasonal = rnorm(2, 0, 0.2),
                 ess = runif(2, 30, 150), ess_ok = ess_ok,
                 unit = u, exploratory = (u == "NGA"), stringsAsFactors = FALSE))))) }

test_that("plot_forecast_cv_skill renders R2 / bias / skill and builds a ggplot", {
  skip_if_not_installed("ggplot2")
  set.seed(1); cells <- make_cells()
  for (v in c("R2_corr", "R2_sse", "bias_ratio", "wis_skill", "mae_skill")) {
    r <- plot_forecast_cv_skill(cells, value = v, horizon_months = 6, verbose = FALSE)
    expect_s3_class(r$plot, "ggplot")
    expect_equal(nrow(r$summary), 6L)           # 3 units x 2 metrics
    expect_true(all(r$summary$n_origins == 3L))
  }
})

test_that("skill tally counts origins beating the baseline (>0); bias counts [0.5,2]", {
  skip_if_not_installed("ggplot2")
  set.seed(2); cells <- make_cells()
  rs <- plot_forecast_cv_skill(cells, value = "wis_skill", verbose = FALSE)
  chk <- aggregate(wis_skill_seasonal ~ unit + metric, data = cells, FUN = function(v) sum(v > 0))
  m <- merge(rs$summary, chk, by = c("unit", "metric"))
  expect_equal(m$n_good, m$wis_skill_seasonal)
  rb <- plot_forecast_cv_skill(cells, value = "bias_ratio", verbose = FALSE)
  chkb <- aggregate(bias_ratio ~ unit + metric, data = cells, FUN = function(v) sum(v >= 0.5 & v <= 2))
  mb <- merge(rb$summary, chkb, by = c("unit", "metric"))
  expect_equal(mb$n_good, mb$bias_ratio)
})

test_that("R2 uses a median annotation (no win/loss tally)", {
  skip_if_not_installed("ggplot2")
  set.seed(3); cells <- make_cells()
  r <- plot_forecast_cv_skill(cells, value = "R2_corr", verbose = FALSE)
  expect_true(all(is.na(r$summary$n_good)))
  expect_true(all(grepl("^med ", r$summary$lab)))
})

test_that("errors on a missing window or column", {
  skip_if_not_installed("ggplot2")
  cells <- make_cells()
  expect_error(plot_forecast_cv_skill(cells, horizon_months = 99), "window")
  expect_error(plot_forecast_cv_skill(cells, value = "wis_skill", baseline = "nope"), "not found")
})
