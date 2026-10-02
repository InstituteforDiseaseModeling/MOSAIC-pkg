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

test_that("evaluate_rolling_cv reports mae/wis skill columns + summary", {
  # Column names changed (v0.55.x Phase-3): skill_<b> -> mae_skill_<b> + wis_skill_<b>;
  # summary n_cells -> n_cells_used/n_cells_total. Update assertions accordingly.
  ev <- evaluate_rolling_cv(make_pred(), baselines = c("seasonal","persistence"), n_boot = 50L)
  expect_true(all(c("mae_skill_seasonal","mae_skill_persistence",
                    "wis_skill_seasonal","wis_skill_persistence") %in% names(ev$cells)))
  # persistence skill is finite (~1 yr IS); seasonal is NA (needs >= 2 yr IS).
  o5 <- ev$cells[ev$cells$window == "OOS<=5mo", ]
  expect_true(is.finite(o5$mae_skill_persistence))
  expect_true(is.na(o5$mae_skill_seasonal))   # < 2 yr IS -> seasonal NA-gated
  expect_true(all(c("metric","window","n_cells_used","n_cells_total","wis_mean") %in% names(ev$summary)))
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

# Predictions with >= 2 years of IS history (enables the seasonal baseline) and
# an optional per-cutoff ess column.
make_pred_long <- function(ess = NA_real_) {
  cutoff <- as.Date("2025-06-01")
  is_dates  <- seq(as.Date("2022-06-05"), cutoff, by = "week")   # ~3 yr IS
  oos_dates <- seq(cutoff + 14, by = "week", length.out = 24)
  obs_is  <- 50 + 30 * sin(seq_along(is_dates))  + 10
  obs_oos <- 40 + 25 * sin(seq_along(oos_dates)) + 10
  mk <- function(dates, seg, obs, pred) data.frame(
    run_id = "cutoff_2025-06-01", iso_code = "A", anchor_date = "2022-02-01",
    cutoff_date = as.character(cutoff), date = dates, metric = "cases",
    segment = seg, weeks_ahead = NA_integer_, horizon_bucket = NA_character_,
    observed = obs, observed_source = "config_reported", pred_median = pred,
    pi50_lo = pred * 0.5, pi50_hi = pred * 1.5,
    pi95_lo = pred * 0.05, pi95_hi = pred * 3,
    ess = ess, stringsAsFactors = FALSE)
  out <- rbind(mk(is_dates, "IS", obs_is, obs_is),
               mk(oos_dates, "OOS", obs_oos, 1.2 * obs_oos))   # mild over-predict
  if (all(is.na(ess))) out$ess <- NULL                          # drop if not requested
  out
}

test_that("ess gate: cells flagged, summary uses only ess_ok, reports n_used/n_total", {
  # Two cutoffs: one high-ess (passes), one low-ess (fails the gate).
  p_hi <- make_pred_long(ess = 100); p_hi$run_id <- "cutoff_hi"; p_hi$cutoff_date <- as.character(as.Date("2025-06-01"))
  p_lo <- make_pred_long(ess = 5);   p_lo$run_id <- "cutoff_lo"; p_lo$cutoff_date <- as.character(as.Date("2025-06-08"))
  p_lo$date <- p_lo$date + 7
  ev <- evaluate_rolling_cv(rbind(p_hi, p_lo), ess_min = 50, n_boot = 20L)

  expect_true(all(c("ess","ess_ok") %in% names(ev$cells)))
  expect_true(all(ev$cells$ess_ok[ev$cells$run_id == "cutoff_hi"]))
  expect_false(any(ev$cells$ess_ok[ev$cells$run_id == "cutoff_lo"]))

  o5 <- ev$summary[ev$summary$window == "OOS<=5mo", ]
  expect_equal(o5$n_cells_total, 2L)   # both cutoffs present as cells
  expect_equal(o5$n_cells_used, 1L)    # only the high-ess cutoff aggregated
  expect_true(o5$ess_gated)

  # NA ess -> ess_ok FALSE
  p_na <- make_pred_long(ess = NA_real_); p_na$ess <- NA_real_
  ev_na <- evaluate_rolling_cv(p_na, ess_min = 50, n_boot = 10L)
  expect_false(any(ev_na$cells$ess_ok))
})

test_that("ess gate is back-compat off when ess column absent (ess_ok TRUE)", {
  ev <- evaluate_rolling_cv(make_pred_long(), ess_min = 1e9, n_boot = 10L)
  expect_true(all(ev$cells$ess_ok))                 # no ess col -> always TRUE
  expect_false(ev$summary$ess_gated[1])
  expect_equal(ev$summary$n_cells_used[ev$summary$window == "OOS<=5mo"],
               ev$summary$n_cells_total[ev$summary$window == "OOS<=5mo"])
})

test_that("seasonal baseline NA-gates below 2 yr IS (no grand-mean fallback)", {
  short <- make_pred()         # ~1 yr IS -> seasonal must be NA
  long  <- make_pred_long()    # ~3 yr IS -> seasonal finite
  ev_s <- evaluate_rolling_cv(short, baselines = "seasonal", n_boot = 10L)
  ev_l <- evaluate_rolling_cv(long,  baselines = "seasonal", n_boot = 10L)
  expect_true(all(is.na(ev_s$cells$mae_skill_seasonal[grepl("^OOS", ev_s$cells$window)])))
  expect_true(any(is.finite(ev_l$cells$mae_skill_seasonal[grepl("^OOS", ev_l$cells$window)])))
})

test_that("wis_skill computed against a known persistence baseline", {
  # Build a slice where we can hand-check the WIS-ratio sign.
  p <- make_pred_long()
  ev <- evaluate_rolling_cv(p, baselines = "persistence", n_boot = 10L)
  o5 <- ev$cells[ev$cells$window == "OOS<=5mo", ]
  expect_true(is.finite(o5$wis_skill_persistence))

  # Independent recomputation of the baseline WIS for the OOS<=5mo slice.
  cutoff <- as.Date("2025-06-01")
  is_df  <- p[p$segment == "IS", ]
  oos    <- p[p$segment == "OOS", ]
  # Horizon windows are measured from the scoring origin (the cutoff here: no
  # embargo rows), not from the first observed OOS date.
  oos0   <- cutoff; wend <- oos0 + ceiling(5 * 30.4375)
  sl     <- oos[oos$date > oos0 & oos$date <= wend, ]
  io     <- is_df[order(is_df$date), ]
  mu     <- mean(utils::tail(io$observed, 4), na.rm = TRUE)
  resid  <- io$observed - mu
  q      <- stats::quantile(resid, c(0.025, 0.25, 0.75, 0.975), names = FALSE, type = 7)
  wis_fun <- function(y, m, l50, u50, l95, u95) {
    is_s <- function(l, u, y, a) (u - l) + (2/a)*(l - y)*(y < l) + (2/a)*(y - u)*(y > u)
    (0.5*abs(y - m) + 0.25*is_s(l50, u50, y, 0.5) + 0.025*is_s(l95, u95, y, 0.05)) / 2.5
  }
  wis_b <- mean(wis_fun(sl$observed, mu, mu + q[2], mu + q[3], mu + q[1], mu + q[4]))
  wis_m <- mean(wis_fun(sl$observed, sl$pred_median, sl$pi50_lo, sl$pi50_hi, sl$pi95_lo, sl$pi95_hi))
  expect_equal(o5$wis_skill_persistence, round(1 - wis_m / wis_b, 3), tolerance = 1e-6)
})

test_that("small-n CI suppression: < min_cells_ci finite cells -> lo/hi NA", {
  # Single cutoff/country/metric -> 1 cell per window -> CI must be suppressed.
  ev <- evaluate_rolling_cv(make_pred_long(), min_cells_ci = 5L,
                            baselines = "persistence", n_boot = 50L)
  o5 <- ev$summary[ev$summary$window == "OOS<=5mo", ]
  expect_true(is.na(o5$mae_skill_persistence_lo))
  expect_true(is.na(o5$mae_skill_persistence_hi))
  expect_true(is.finite(o5$mae_skill_persistence_mean))
  expect_true(o5$ci_suppressed)

  # With min_cells_ci = 1 a single cell still has length 1 -> CI NA by construction,
  # but the suppression flag is FALSE (not gated by the threshold).
  ev2 <- evaluate_rolling_cv(make_pred_long(), min_cells_ci = 1L,
                             baselines = "persistence", n_boot = 50L)
  o5b <- ev2$summary[ev2$summary$window == "OOS<=5mo", ]
  expect_false(o5b$ci_suppressed)
})

test_that("per-metric embargo shifts the OOS scoring boundary", {
  # Build cases + deaths; deaths embargo 6 wk drops more early OOS rows than cases 0 wk.
  p_cases <- make_pred_long()
  p_deaths <- p_cases; p_deaths$metric <- "deaths"
  p <- rbind(p_cases, p_deaths)

  ev0 <- evaluate_rolling_cv(p, embargo_weeks = 0, n_boot = 10L)
  ev  <- evaluate_rolling_cv(p, embargo_weeks = c(cases = 0, deaths = 6), n_boot = 10L)

  n_cases_0  <- ev0$cells$n[ev0$cells$metric == "cases"  & ev0$cells$window == "OOS<=5mo"]
  n_cases    <- ev$cells$n[ev$cells$metric == "cases"    & ev$cells$window == "OOS<=5mo"]
  n_deaths_0 <- ev0$cells$n[ev0$cells$metric == "deaths" & ev0$cells$window == "OOS<=5mo"]
  n_deaths   <- ev$cells$n[ev$cells$metric == "deaths"   & ev$cells$window == "OOS<=5mo"]

  expect_equal(n_cases, n_cases_0)        # cases embargo unchanged
  expect_lt(n_deaths, n_deaths_0)         # deaths embargo drops early OOS rows

  # The deaths scoring origin is exactly cutoff + 6*7 days: scored dates are
  # strictly after it and the horizon window is measured from it.
  cutoff <- as.Date("2025-06-01")
  oos    <- p[p$segment == "OOS" & p$metric == "deaths", ]
  oos0   <- cutoff + 6 * 7
  wend   <- oos0 + ceiling(5 * 30.4375)
  expect_equal(n_deaths, sum(oos$date > oos0 & oos$date <= wend))
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

test_that("WIS pairs the intervals with pred_median_obs (hand-computed), falling back to pred_median", {
  # Release red team TA-04/OBS-4: the consumers of pred_median_obs were untested.
  # WIS = (0.5|y - m| + 0.25 IS_50 + 0.025 IS_95) / 2.5 (Bracher et al. 2021).
  # Rows with m = pred_median_obs:
  #   y=10, m=6:     (2      + 0.25*7          + 0.025*18) / 2.5 = 1.68
  #   y=20, m=15:    (2.5    + 0.25*12         + 0.025*35) / 2.5 = 2.55
  #   y=0,  m=0:     (0      + 0.25*2          + 0.025*6)  / 2.5 = 0.26
  #   y=50, m=31.95: (9.025  + 0.25*(20 + 4*5) + 0.025*70) / 2.5 = 8.31   -> mean 3.2
  # and with m = pred_median (9, 18, 2, 39.95): 1.08, 1.95, 0.66, 6.71   -> mean 2.6
  df <- data.frame(date = as.Date("2025-07-01") + 7 * (0:3),
                   observed        = c(10, 20, 0, 50),
                   pred_central    = c(12, 22, 3, 44),
                   pred_median     = c(9, 18, 2, 39.95),
                   pred_median_obs = c(6, 15, 0, 31.95),
                   pi50_lo = c(5, 12, 0, 25), pi50_hi = c(12, 24, 2, 45),
                   pi95_lo = c(2, 5, 0, 10),  pi95_hi = c(20, 40, 6, 80))
  w <- with(df, MOSAIC:::.rcv_wis(observed, pred_median_obs, pi50_lo, pi50_hi, pi95_lo, pi95_hi))
  expect_equal(w, c(1.68, 2.55, 0.26, 8.31), tolerance = 1e-12)
  expect_equal(MOSAIC:::.rcv_window_metrics(df)$wis, 3.2)
  expect_equal(MOSAIC:::.rcv_window_metrics(df)$mae, 3.25)          # point metrics stay on pred_central
  no_obs <- df; no_obs$pred_median_obs <- NULL
  expect_equal(MOSAIC:::.rcv_window_metrics(no_obs)$wis, 2.6)

  # Skill against a persistence baseline whose in-sample residuals are all zero:
  # its intervals collapse onto its point mu = 10, so its WIS is |y - 10|, mean 15.
  is_df <- data.frame(date = as.Date("2025-05-01") + 7 * (0:5), observed = 10)
  sk <- MOSAIC:::.rcv_skill(df, is_df, "persistence")
  expect_equal(sk$wis_skill_persistence, round(1 - 3.2 / 15, 3))    # 0.787
  expect_equal(sk$mae_skill_persistence, round(1 - 3.25 / 15, 3))   # 0.783
  expect_equal(MOSAIC:::.rcv_skill(no_obs, is_df, "persistence")$wis_skill_persistence,
               round(1 - 2.6 / 15, 3))                              # 0.827
})
