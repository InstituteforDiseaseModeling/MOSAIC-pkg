# Unit tests for run_rolling_cv() pure-logic helpers. The heavy end-to-end path
# (est_suitability -> run_MOSAIC -> ensemble) is validated by a live smoke run,
# not here.

test_that(".rolling_cv_cutoffs builds a monthly schedule back from the latest", {
  cut <- MOSAIC:::.rolling_cv_cutoffs("2025-12-07", n_cutoffs = 12L, step_months = 1L)
  expect_length(cut, 12L)
  expect_equal(max(cut), as.Date("2025-12-07"))
  expect_equal(min(cut), as.Date("2025-01-07"))      # 11 months back
  expect_true(all(diff(cut) >= 28 & diff(cut) <= 31)) # monthly steps
  # 6-cutoff case
  cut6 <- MOSAIC:::.rolling_cv_cutoffs("2025-12-07", 6L, 1L)
  expect_length(cut6, 6L)
  expect_equal(min(cut6), as.Date("2025-07-07"))
})

test_that(".rolling_cv_label assigns IS / embargo / OOS + horizon buckets", {
  cutoff <- as.Date("2025-06-01"); embargo <- 7L
  dates  <- seq(as.Date("2025-05-01"), as.Date("2025-12-01"), by = "day")
  lab    <- MOSAIC:::.rolling_cv_label(dates, cutoff, embargo, c(1, 3, 5))

  expect_true(all(lab$segment[dates <= cutoff] == "IS"))
  expect_true(all(lab$segment[dates > cutoff & dates <= cutoff + embargo] == "embargo"))
  expect_true(all(lab$segment[dates > cutoff + embargo] == "OOS"))
  # IS / embargo rows have NA weeks_ahead + NA horizon
  expect_true(all(is.na(lab$weeks_ahead[lab$segment != "OOS"])))
  expect_true(all(is.na(lab$horizon_bucket[lab$segment != "OOS"])))
  # smallest containing horizon wins
  oos0 <- cutoff + embargo
  expect_equal(lab$horizon_bucket[dates == oos0 + 10], "h1mo")  # ~1.4 wk in
  expect_equal(lab$horizon_bucket[dates == oos0 + 60], "h3mo")  # ~2 mo in
  expect_equal(lab$horizon_bucket[dates == oos0 + 130], "h5mo") # ~4.3 mo in
  expect_true(is.na(lab$horizon_bucket[dates == oos0 + 175]))   # beyond 5 mo
  expect_true(all(lab$weeks_ahead[lab$segment == "OOS"] >= 1L))
})

test_that(".rcv_add_months clamps month arithmetic", {
  expect_equal(MOSAIC:::.rcv_add_months("2025-12-07", -11), as.Date("2025-01-07"))
  expect_equal(MOSAIC:::.rcv_add_months("2025-03-31", -1),  as.Date("2025-02-28") + 0) # Feb clamp
})

test_that(".rolling_cv_psi_matrix builds locations x dates and fills gaps", {
  dates <- seq(as.Date("2025-01-01"), as.Date("2025-01-31"), by = "day")
  csv <- tempfile(fileext = ".csv")
  # Option A (v0.34): the matrix builder consumes the canonical `psi` column.
  df <- rbind(
    data.frame(iso_code = "MOZ", date = as.character(dates[c(1, 10, 20, 31)]),
               psi = c(0.1, 0.4, 0.6, 0.9)),
    data.frame(iso_code = "KEN", date = as.character(dates[c(1, 31)]),
               psi = c(0.2, 0.3)))
  write.csv(df, csv, row.names = FALSE)

  m <- MOSAIC:::.rolling_cv_psi_matrix(csv, c("MOZ", "KEN"), dates)
  expect_equal(dim(m), c(2L, length(dates)))
  expect_equal(rownames(m), c("MOZ", "KEN"))
  expect_false(any(is.na(m)))                         # gaps filled by locf/nocb
  expect_equal(unname(m["MOZ", 1]), 0.1)
  expect_equal(unname(m["MOZ", length(dates)]), 0.9)
  expect_equal(unname(m["MOZ", 5]), 0.1)              # carried forward from day 1
})

test_that(".rcv_merge_est_args drops harness-owned date keys with a warning", {
  spec  <- list(n_splits = 0L, fit_date_stop = "2025-01-01", exclude_covariates = "x")
  owned <- list(PATHS = "P", fit_date_stop = as.Date("2025-06-01"),
                pred_date_start = as.Date("2023-02-01"))
  expect_warning(merged <- MOSAIC:::.rcv_merge_est_args(spec, owned), "harness-owned")
  expect_equal(merged$fit_date_stop, as.Date("2025-06-01"))  # harness wins
  expect_equal(merged$n_splits, 0L)                          # modeling arg kept
  expect_equal(merged$exclude_covariates, "x")
})

test_that(".rolling_cv_compile_run assembles a labeled long table from an ensemble", {
  n_t <- 60L
  ds <- as.Date("2025-04-01"); de <- ds + (n_t - 1L)
  edates <- seq(ds, de, length.out = n_t)
  mk <- function(v) matrix(v, nrow = 1)
  ci_pair <- function(lo, hi) list(lower = mk(lo), upper = mk(hi))
  ens <- list(
    n_time_points = n_t, date_start = ds, date_stop = de,
    location_names = "MOZ", envelope_quantiles = c(0.025, 0.25, 0.75, 0.975),
    cases_median  = mk(seq_len(n_t)), deaths_median = mk(seq_len(n_t) / 10),
    ci_bounds = list(
      cases  = list(ci_pair(seq_len(n_t) - 1, seq_len(n_t) + 1),
                    ci_pair(seq_len(n_t) - 0.5, seq_len(n_t) + 0.5)),
      deaths = list(ci_pair(seq_len(n_t)/10 - 1, seq_len(n_t)/10 + 1),
                    ci_pair(seq_len(n_t)/10 - 0.5, seq_len(n_t)/10 + 0.5))))

  obs_dates <- seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day")
  oc <- matrix(NA_real_, 1, length(obs_dates)); od <- oc
  oc[1, match(as.character(edates), as.character(obs_dates))] <- 100  # observed present in window

  out <- MOSAIC:::.rolling_cv_compile_run(
    ensemble = ens, run_id = "cutoff_2025-04-15", cutoff = as.Date("2025-04-15"),
    anchor = as.Date("2023-02-01"), embargo_days = 7L, horizons_months = c(1, 3, 5),
    obs_cases = oc, obs_deaths = od, obs_dates = obs_dates, location_names = "MOZ",
    model = "best")

  # two metrics x n_t rows
  expect_equal(nrow(out), 2L * n_t)
  expect_setequal(unique(out$metric), c("cases", "deaths"))
  expect_true("model" %in% names(out))
  expect_setequal(unique(out$model), "best")          # model tag carried through
  expect_true(all(c("run_id","model","iso_code","date","segment","weeks_ahead",
                    "horizon_bucket","observed","pred_median",
                    "pi95_lo","pi95_hi","pi50_lo","pi50_hi") %in% names(out)))
  # segments present and observed carried from held-out matrix
  expect_true(all(c("IS","embargo","OOS") %in% unique(out$segment)))
  expect_true(all(out$observed[!is.na(out$observed)] == 100))
  # CI ordering sane
  expect_true(all(out$pi95_lo <= out$pi50_lo, na.rm = TRUE))
  expect_true(all(out$pi95_hi >= out$pi50_hi, na.rm = TRUE))
})
