# Synthetic predictions with known relationships:
#   IS:  pred = obs            -> R2_corr=1, R2_sse=1, bias=1
#   OOS: pred = 2*obs          -> R2_corr=1, R2_sse<0, bias=2
make_pred <- function() {
  cutoff <- as.Date("2025-06-01")
  is_dates  <- seq(as.Date("2024-06-03"), cutoff, by = "week")
  oos_dates <- seq(cutoff + 14, by = "week", length.out = 24)   # ~5.5 months
  obs_is  <- 50 + 30 * sin(seq_along(is_dates))  + 10
  obs_oos <- 40 + 25 * sin(seq_along(oos_dates)) + 10
  mk <- function(dates, seg, obs, pred) data.frame(
    run_id = "cutoff_2025-06-01", iso_code = "A", anchor_date = "2023-02-01",
    cutoff_date = as.character(cutoff), date = dates, metric = "cases",
    segment = seg, weeks_ahead = NA_integer_, horizon_bucket = NA_character_,
    observed = obs, observed_source = "config_reported", pred_median = pred,
    pi50_lo = pred * 0.5, pi50_hi = pred * 1.5,
    pi95_lo = pred * 0.05, pi95_hi = pred * 3, stringsAsFactors = FALSE)
  rbind(mk(is_dates, "IS", obs_is, obs_is),         # perfect IS fit
        mk(oos_dates, "OOS", obs_oos, 2 * obs_oos))  # OOS over-predicts 2x
}

test_that("evaluate_rolling_cv computes IS + cumulative OOS metrics with known values", {
  ev <- evaluate_rolling_cv(make_pred(), horizons_months = c(1, 3, 5), n_boot = 50L)
  cells <- ev$cells

  expect_true(all(c("IS","OOS<=1mo","OOS<=3mo","OOS<=5mo") %in% cells$window))

  is_row <- cells[cells$window == "IS", ]
  expect_equal(is_row$R2_corr, 1, tolerance = 1e-6)   # pred==obs
  expect_equal(is_row$R2_sse,  1, tolerance = 1e-6)
  expect_equal(is_row$bias_ratio, 1, tolerance = 1e-6)

  o5 <- cells[cells$window == "OOS<=5mo", ]
  expect_equal(o5$R2_corr, 1, tolerance = 1e-6)        # pred = 2*obs -> still perfectly correlated
  expect_lt(o5$R2_sse, 0)                              # but worse than the mean
  expect_equal(o5$bias_ratio, 2, tolerance = 1e-6)     # pred = 2*obs
  expect_equal(o5$cov95, 1, tolerance = 1e-6)          # obs within the wide 95% PI
})

test_that("evaluate_rolling_cv horizons are cumulative (nested, n increasing)", {
  ev <- evaluate_rolling_cv(make_pred(), horizons_months = c(1, 3, 5), n_boot = 10L)
  oc <- ev$cells[grepl("^OOS", ev$cells$window), ]
  n1 <- oc$n[oc$window == "OOS<=1mo"]; n3 <- oc$n[oc$window == "OOS<=3mo"]; n5 <- oc$n[oc$window == "OOS<=5mo"]
  expect_true(n1 < n3 && n3 <= n5)
})

test_that("evaluate_rolling_cv reports baseline skill columns + summary", {
  ev <- evaluate_rolling_cv(make_pred(), baselines = c("seasonal","persistence"), n_boot = 50L)
  expect_true(all(c("skill_seasonal","skill_persistence") %in% names(ev$cells)))
  # OOS skill values are finite (single cell -> CI is NA, mean present)
  o5 <- ev$cells[ev$cells$window == "OOS<=5mo", ]
  expect_true(is.finite(o5$skill_seasonal) && is.finite(o5$skill_persistence))
  expect_true(all(c("metric","window","n_cells","wis_mean") %in% names(ev$summary)))
})

test_that("evaluate_rolling_cv breaks out metrics per model when a model column is present", {
  # two models: a perfect ensemble (pred==obs) and a 2x over-predicting best
  p <- make_pred(); p$model <- "ensemble"
  best <- p
  best$model <- "best"
  best$pred_median <- 2 * best$observed                      # OOS+IS both 2x
  best$pi50_lo <- best$pred_median * 0.5; best$pi50_hi <- best$pred_median * 1.5
  best$pi95_lo <- best$pred_median * 0.05; best$pi95_hi <- best$pred_median * 3
  ev <- evaluate_rolling_cv(rbind(p, best), n_boot = 20L)

  expect_true("model" %in% names(ev$cells))
  expect_setequal(unique(ev$cells$model), c("ensemble", "best"))
  expect_true("model" %in% names(ev$summary))

  is_ens  <- ev$cells[ev$cells$model == "ensemble" & ev$cells$window == "IS", ]
  is_best <- ev$cells[ev$cells$model == "best"     & ev$cells$window == "IS", ]
  expect_equal(is_ens$bias_ratio, 1, tolerance = 1e-6)       # ensemble perfect
  expect_equal(is_best$bias_ratio, 2, tolerance = 1e-6)      # best 2x high
})

test_that("evaluate_rolling_cv drops AI-sourced observations when trusted_only", {
  p <- make_pred()
  # inject an AI-sourced OOS row with absurd observed; trusted_only should drop it
  ai <- p[p$segment == "OOS", ][1, ]; ai$observed_source <- "AI"; ai$observed <- 1e6
  p2 <- rbind(p, ai)
  ev_keep <- evaluate_rolling_cv(p2, trusted_only = FALSE, n_boot = 10L)
  ev_drop <- evaluate_rolling_cv(p2, trusted_only = TRUE,  n_boot = 10L)
  n_keep <- ev_keep$cells$n[ev_keep$cells$window == "OOS<=5mo"]
  n_drop <- ev_drop$cells$n[ev_drop$cells$window == "OOS<=5mo"]
  expect_equal(n_drop, n_keep - 1L)
})
