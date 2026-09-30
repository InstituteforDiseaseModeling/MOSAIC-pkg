# Regression tests for the forecast-CV scoring / reporting fixes from the
# production-readiness review (group cfr-cv): the embargo week and horizon
# origin in evaluate_rolling_cv(), partial named embargo vectors, the
# make_forecast_cv_table() training anchor / ESS gate / horizon parsing, and
# the plot_forecast_cv_skill() / plot_forecast_cv_grid() defaults and labels.

# Harness-shaped daily predictions for one cutoff: segments come from the real
# labeller, so the embargo rows are exactly what run_rolling_cv() writes.
.sc_pred <- function(cutoff = as.Date("2024-06-01"), embargo_days = 7L,
                     model = "ensemble_opt", ess = 100, anchor = "2023-01-01") {
  dates <- seq(as.Date("2022-01-01"), cutoff + 200, by = "day")
  lab <- MOSAIC:::.rolling_cv_label(dates, cutoff, embargo_days, c(1, 3, 5))
  obs <- 20 + 10 * sin(seq_along(dates) / 9)
  mk <- function(metric, scale) data.frame(
    run_id = paste0("cutoff_", cutoff), model = model, iso_code = "MOZ",
    anchor_date = anchor, cutoff_date = as.character(cutoff), date = dates,
    metric = metric, segment = lab$segment, weeks_ahead = lab$weeks_ahead,
    horizon_bucket = lab$horizon_bucket, observed = obs * scale,
    observed_source = "config_reported", pred_central = 1.1 * obs * scale,
    pred_median = 1.1 * obs * scale,
    pi50_lo = 0.8 * obs * scale, pi50_hi = 1.4 * obs * scale,
    pi95_lo = 0.3 * obs * scale, pi95_hi = 2.5 * obs * scale,
    ess = ess, stringsAsFactors = FALSE)
  rbind(mk("cases", 1), mk("deaths", 0.05))
}

test_that("default evaluation never scores the harness embargo week", {
  p <- .sc_pred()
  cutoff <- as.Date("2024-06-01")
  ev <- evaluate_rolling_cv(p, n_boot = 10L)
  c1 <- ev$cells[ev$cells$metric == "cases" & ev$cells$window == "OOS<=1mo", ]
  first_oos <- cutoff + 8L
  expect_equal(c1$n, as.integer(ceiling(30.4375)))           # (T+7, T+7+31], no embargo day
  # same count as the harness's own h1mo bucket
  expect_equal(c1$n, sum(p$metric == "cases" & p$horizon_bucket %in% "h1mo"))
  expect_equal(min(p$date[p$segment == "OOS"]), first_oos)
})

test_that("a post-cutoff reporting gap shrinks the window instead of shifting lead time", {
  p <- .sc_pred()
  cutoff <- as.Date("2024-06-01")
  gap <- p$date >= cutoff + 8L & p$date <= cutoff + 35L
  p$observed[gap] <- NA
  ev <- evaluate_rolling_cv(p, n_boot = 10L)
  c1 <- ev$cells[ev$cells$metric == "cases" & ev$cells$window == "OOS<=1mo", ]
  expect_equal(c1$n, 3L)                                      # only T+36..T+38 remain
})

test_that("a partial named embargo vector leaves omitted metrics at 0", {
  p <- .sc_pred()
  expect_no_error(ev <- evaluate_rolling_cv(p, embargo_weeks = c(cases = 3), n_boot = 10L))
  n_d <- ev$cells$n[ev$cells$metric == "deaths" & ev$cells$window == "OOS<=1mo"]
  n_c <- ev$cells$n[ev$cells$metric == "cases"  & ev$cells$window == "OOS<=1mo"]
  expect_equal(n_d, 31L)                                      # harness embargo only
  expect_equal(n_c, 31L)                                      # (T+21, T+52]
  expect_equal(MOSAIC:::.rcv_embargo_lookup(c(cases = 3), c("cases", "deaths")),
               list(cases = 3, deaths = 0))
})

test_that("an explicit embargo shorter than the harness one cannot score embargo rows", {
  p <- .sc_pred(embargo_days = 14L)
  ev <- evaluate_rolling_cv(p, embargo_weeks = 1, n_boot = 10L)
  c1 <- ev$cells[ev$cells$metric == "cases" & ev$cells$window == "OOS<=1mo", ]
  expect_equal(c1$n, 31L)                                     # origin T+14, not T+7
})

test_that("the scoring origin survives AI-sourced embargo rows being filtered out", {
  p <- .sc_pred(embargo_days = 14L)
  p$observed_source[p$segment == "embargo"] <- "AI"
  ev <- evaluate_rolling_cv(p, n_boot = 10L)
  c1 <- ev$cells[ev$cells$metric == "cases" & ev$cells$window == "OOS<=1mo", ]
  expect_equal(c1$n, 31L)                                     # (T+14, T+45], not (T, T+31]
})

test_that("make_forecast_cv_table: anchor-based train_years, ESS-gated summary, horizon parsing", {
  p <- rbind(.sc_pred(as.Date("2024-06-01"), ess = 100),
             .sc_pred(as.Date("2024-07-01"), ess = 5))
  cells <- evaluate_rolling_cv(p, horizons_months = c(1, 1.5, 3), ess_min = 50, n_boot = 10L)$cells
  expect_true("anchor_date" %in% names(cells))

  tb <- make_forecast_cv_table(cells, horizons_months = c(1, 1.5, 3), primary_horizon = 3)
  # train_years measured from the run's config start (2023-01-01), not 2018
  r <- tb$detail[tb$detail$cutoff == as.Date("2024-06-01"), ]
  expect_equal(unique(r$train_years), round(as.numeric(as.Date("2024-06-01") - as.Date("2023-01-01")) / 365.25, 2))
  # fractional horizon parsed
  expect_true(1.5 %in% tb$detail$horizon_mo)
  expect_false(anyNA(tb$detail$horizon_mo))
  # the low-ESS origin is left out of the summary, as in evaluate_rolling_cv()
  s <- tb$summary[tb$summary$scope == "country" & tb$summary$metric == "cases", ]
  expect_equal(s$n_origins, 1L)
  expect_equal(s$n_gated, 1L)
  ok <- cells[cells$metric == "cases" & cells$window == "OOS<=3mo" & cells$ess_ok, ]
  expect_equal(s$wis_skill_median, stats::median(ok$wis_skill_seasonal, na.rm = TRUE))

  expect_error(make_forecast_cv_table(cells, horizons_months = c(1, 1.5), primary_horizon = 3),
               "must be one of horizons_months")
})

test_that("plot_forecast_cv_skill works on evaluate_rolling_cv() defaults", {
  skip_if_not_installed("ggplot2")
  cells <- evaluate_rolling_cv(.sc_pred(), n_boot = 10L)$cells
  expect_no_error(res <- plot_forecast_cv_skill(cells, verbose = FALSE))
  expect_true(all(res$data$window == "OOS<=3mo"))
  expect_true(all(res$data$model == "ensemble_opt"))
})

test_that("plot_forecast_cv_grid labels daily counts per day and names the drawn interval", {
  skip_if_not_installed("patchwork")
  skip_if_not_installed("ggplot2")
  fig <- plot_forecast_cv_grid(.sc_pred(), metric = "cases", ci = "pi50", verbose = FALSE)
  ann <- fig$patches$annotation
  expect_match(ann$caption, "per day")
  expect_match(ann$subtitle, "50% CI")
  expect_false(grepl("95% CI", ann$subtitle))
})
