# observed_tier: the config's surveillance trust tier carried into the rolling-CV
# predictions table and honoured by evaluate_rolling_cv(observed_tiers =).

tier_ensemble <- function(n_t, ds) {
  mk <- function(v) matrix(v, nrow = 1)
  ci_pair <- function(lo, hi) list(lower = mk(lo), upper = mk(hi))
  list(
    n_time_points = n_t, date_start = ds, date_stop = ds + (n_t - 1L),
    location_names = "MOZ", envelope_quantiles = c(0.025, 0.25, 0.75, 0.975),
    cases_median  = mk(seq_len(n_t)), deaths_median = mk(seq_len(n_t) / 10),
    ci_bounds = list(
      cases  = list(ci_pair(seq_len(n_t) - 1, seq_len(n_t) + 1),
                    ci_pair(seq_len(n_t) - 0.5, seq_len(n_t) + 0.5)),
      deaths = list(ci_pair(seq_len(n_t) / 10 - 1, seq_len(n_t) / 10 + 1),
                    ci_pair(seq_len(n_t) / 10 - 0.5, seq_len(n_t) / 10 + 0.5))))
}

test_that(".rolling_cv_compile_run carries observed_tier aligned by date", {
  n_t <- 60L
  ds  <- as.Date("2025-04-01")
  ens <- tier_ensemble(n_t, ds)
  obs_dates <- seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day")
  oc <- matrix(100, 1, length(obs_dates)); od <- oc
  # tier = 1, 2, 3 repeating over the whole observed calendar
  tier <- matrix(rep_len(1:3, length(obs_dates)), 1)
  args <- list(ensemble = ens, run_id = "cutoff_2025-04-15", cutoff = as.Date("2025-04-15"),
               anchor = as.Date("2023-02-01"), embargo_days = 7L, horizons_months = c(1, 3),
               obs_cases = oc, obs_deaths = od, obs_dates = obs_dates, location_names = "MOZ")

  out <- do.call(MOSAIC:::.rolling_cv_compile_run, c(args, list(obs_tier = tier)))
  expect_type(out$observed_tier, "integer")
  want <- tier[1, match(as.character(out$date), as.character(obs_dates))]
  expect_identical(out$observed_tier, as.integer(want))
  expect_setequal(unique(out$observed_tier), 1:3)

  # a config without reported_tier: the column exists and is NA
  out0 <- do.call(MOSAIC:::.rolling_cv_compile_run, args)
  expect_true("observed_tier" %in% names(out0))
  expect_true(all(is.na(out0$observed_tier)))
})

test_that(".rcv_tier_matrix returns NULL without reported_tier and a matrix with it", {
  n_t <- 10L
  expect_null(MOSAIC:::.rcv_tier_matrix(list(location_name = "MOZ"), n_t))
  m <- MOSAIC:::.rcv_tier_matrix(list(location_name = "MOZ", reported_tier = rep(2L, n_t)), n_t)
  expect_equal(dim(m), c(1L, n_t))
  m2 <- MOSAIC:::.rcv_tier_matrix(
    list(location_name = c("A", "B"), reported_tier = matrix(1:20, 2)), n_t)
  expect_equal(dim(m2), c(2L, n_t))
})

tier_preds <- function() {
  cutoff <- as.Date("2025-06-01")
  is_dates  <- seq(as.Date("2023-06-05"), cutoff, by = "week")   # 2 yr: climatology defined
  oos_dates <- seq(cutoff + 7, by = "week", length.out = 20)
  mk <- function(dates, seg, obs, tier) data.frame(
    run_id = "cutoff_2025-06-01", iso_code = "A", cutoff_date = as.character(cutoff),
    date = dates, metric = "cases", segment = seg, observed = obs,
    observed_source = "config_reported", observed_tier = tier,
    pred_median = obs * 1.2, pi50_lo = obs * 0.8, pi50_hi = obs * 1.5,
    pi95_lo = obs * 0.3, pi95_hi = obs * 3, stringsAsFactors = FALSE)
  is_obs  <- 40 + 20 * sin(seq_along(is_dates) / 4)
  oos_obs <- 40 + 20 * sin(seq_along(oos_dates) / 4)
  # IS: every 4th week imputed (tier 3); OOS: the last 8 weeks imputed
  is_tier  <- ifelse(seq_along(is_dates) %% 4L == 0L, 3L, 1L)
  oos_tier <- c(rep(1L, 6), rep(2L, 6), rep(3L, 8))
  rbind(mk(is_dates, "IS", is_obs, is_tier), mk(oos_dates, "OOS", oos_obs, oos_tier))
}

test_that("evaluate_rolling_cv scores only observed_tiers out of sample, every tier in sample", {
  p <- tier_preds()
  ev_12  <- evaluate_rolling_cv(p, horizons_months = 5, metrics = "cases", n_boot = 10L)
  ev_all <- evaluate_rolling_cv(p, horizons_months = 5, metrics = "cases", n_boot = 10L,
                                observed_tiers = NULL)
  n_of <- function(ev, w) ev$cells$n[ev$cells$window == w]
  oos  <- p[p$segment == "OOS", ]
  expect_equal(n_of(ev_all, "OOS<=5mo"), nrow(oos))
  expect_equal(n_of(ev_12,  "OOS<=5mo"), sum(oos$observed_tier %in% 1:2))
  # in-sample rows (the baselines' training data) keep their tier-3 weeks
  expect_equal(n_of(ev_12, "IS"), n_of(ev_all, "IS"))
  expect_equal(n_of(ev_12, "IS"), sum(p$segment == "IS"))
  # tier 1 only drops the reconstructed weeks too
  ev_1 <- evaluate_rolling_cv(p, horizons_months = 5, metrics = "cases", n_boot = 10L,
                              observed_tiers = 1)
  expect_equal(n_of(ev_1, "OOS<=5mo"), sum(oos$observed_tier == 1L))
})

test_that("evaluate_rolling_cv scores predictions without observed_tier unchanged", {
  p  <- tier_preds()
  p0 <- p[, setdiff(names(p), "observed_tier")]
  ev0   <- evaluate_rolling_cv(p0, horizons_months = 5, metrics = "cases", n_boot = 10L)
  evall <- evaluate_rolling_cv(p,  horizons_months = 5, metrics = "cases", n_boot = 10L,
                               observed_tiers = NULL)
  expect_equal(ev0$cells$n, evall$cells$n)
  expect_equal(ev0$cells$wis, evall$cells$wis)
})
