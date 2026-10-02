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

test_that(".rolling_cv_compile_run writes pred_median_obs from each location's predictive median", {
  # Release red team TA-04/OBS-4: the producer was untested; only the fallback ran.
  n_t <- 30L; ds <- as.Date("2025-04-01")
  ci <- function(m) list(list(lower = m - 2, upper = m + 2), list(lower = m - 1, upper = m + 1))
  cm <- rbind(seq_len(n_t), 100 + seq_len(n_t)); dm <- cm / 10
  ens <- list(n_time_points = n_t, date_start = ds, date_stop = ds + n_t - 1L,
              location_names = c("AAA", "BBB"), envelope_quantiles = c(0.025, 0.25, 0.75, 0.975),
              cases_median = cm, cases_mean = cm + 0.5, deaths_median = dm, deaths_mean = dm + 0.05,
              ci_bounds = list(cases = ci(cm), deaths = ci(dm)),
              predictive_median = list(cases = cm - 0.25 * (1:2), deaths = dm - 0.125 * (1:2)))
  obs_dates <- seq(ds, by = "day", length.out = n_t)
  oc <- matrix(1, 2L, n_t)
  args <- list(run_id = "cutoff_2025-04-15", cutoff = as.Date("2025-04-15"),
               anchor = as.Date("2025-01-01"), embargo_days = 7L, horizons_months = 1,
               obs_cases = oc, obs_deaths = oc, obs_dates = obs_dates,
               location_names = c("AAA", "BBB"), model = "ensemble")
  out <- do.call(MOSAIC:::.rolling_cv_compile_run, c(list(ensemble = ens), args))
  for (i in 1:2) for (m in c("cases", "deaths")) {
    r <- out$iso_code == ens$location_names[i] & out$metric == m
    expect_identical(out$pred_median_obs[r], as.numeric(ens$predictive_median[[m]][i, ]), info = paste(i, m))
    expect_identical(out$pred_median[r], as.numeric(ens[[paste0(m, "_median")]][i, ]), info = paste(i, m))
  }
  # An ensemble without predictive_median (engine-level, before v0.101.0): the median.
  ens$predictive_median <- NULL
  out0 <- do.call(MOSAIC:::.rolling_cv_compile_run, c(list(ensemble = ens), args))
  expect_identical(out0$pred_median_obs, out0$pred_median)
})

test_that("medoid rows carry the cutoff run's observation model and deaths integration, like the ensemble rows", {
  # Release red team TA-01/OBS-3/CM-05: the medoid (and best) rows were plain
  # engine reruns with engine-level intervals and no deaths redraw, while the
  # ensemble rows of the same compile were observation-level.
  n_t <- 35L; ds <- as.Date("2023-01-02")
  cfg <- list(location_name = "MOZ", reported_cases = rep(10, n_t), reported_deaths = rep(1, n_t),
              date_start = as.character(ds), date_stop = as.character(ds + n_t - 1L))
  P <- 6L; S <- 8L
  set.seed(4)
  base <- 40 * exp(sin(seq_len(n_t) / 4)); lev <- exp(stats::rnorm(P, 0, 0.4))
  recs <- list()
  for (p in seq_len(P)) for (s in seq_len(S)) {
    cs <- stats::rpois(n_t, base * lev[p])
    recs[[length(recs) + 1L]] <- list(param_idx = p, stoch_idx = s, success = TRUE,
      reported_cases = matrix(cs, 1L), reported_deaths = matrix(stats::rbinom(n_t, cs, 0.03), 1L),
      expected_deaths = matrix(0.03 * cs, 1L))
  }
  local_mocked_ensemble_sims(recs)
  di <- list(setup = list(nL = 1L), n_time = n_t, dispersion = 2.5, years = 2023L,
             base_logit_full = matrix(stats::qlogis(0.03), 1L, n_t), year_full = rep(2023L, n_t))
  k <- 0.14
  eq <- c(0.025, 0.25, 0.75, 0.975)
  ens_of <- function(n_p, om, d) calc_model_ensemble(
    config = cfg, configs = rep(list(cfg), n_p), n_simulations_per_config = S,
    envelope_quantiles = eq, deaths_integration = d, observation_model = om, verbose = FALSE)

  root <- withr::local_tempdir()
  mk_run <- function(cand, with_di = TRUE) {
    run_dir <- tempfile("run_", tmpdir = root)
    dir.create(file.path(run_dir, "2_calibration", "best_model"), recursive = TRUE)
    saveRDS(MOSAIC:::.mosaic_ensemble_drop_arrays(cand),
            file.path(run_dir, "2_calibration", "ensemble_candidate.rds"))
    if (with_di) saveRDS(di, file.path(run_dir, "2_calibration", "deaths_integration.rds"))
    MOSAIC:::.mosaic_write_json(cfg, file.path(run_dir, "2_calibration", "best_model", "config_medoid.json"))
    run_dir
  }
  compile <- function(run_dir) MOSAIC:::.rcv_compile_all_models(
    run_dir = run_dir, run_id = "cutoff_2023-01-20", cutoff = ds + 18L, anchor = ds,
    embargo_days = 7L, horizons_months = 1, obs_cases = matrix(NA_real_, 1L, n_t),
    obs_deaths = matrix(NA_real_, 1L, n_t), obs_dates = seq(ds, by = "day", length.out = n_t),
    location_names = "MOZ", models = c("ensemble", "medoid"), n_reps = S)
  rows <- function(out, model, metric) out[out$model == model & out$metric == metric, ]
  same_draws <- function(r, e, ch) {
    expect_identical(r$pred_median_obs, as.numeric(e$predictive_median[[ch]][1, ]), info = ch)
    expect_identical(r$pi95_lo, as.numeric(e$ci_bounds[[ch]][[1]]$lower[1, ]), info = ch)
    expect_identical(r$pi95_hi, as.numeric(e$ci_bounds[[ch]][[1]]$upper[1, ]), info = ch)
    expect_identical(r$pi50_lo, as.numeric(e$ci_bounds[[ch]][[2]]$lower[1, ]), info = ch)
    expect_identical(r$pi50_hi, as.numeric(e$ci_bounds[[ch]][[2]]$upper[1, ]), info = ch)
    expect_identical(r$pred_median, as.numeric(e[[paste0(ch, "_median")]][1, ]), info = ch)
  }

  # An observation-level run: every model's rows are observation-level, the
  # medoid's with the run's k and deaths phi.
  cand <- ens_of(P, list(k_cases = k), di)
  out  <- compile(mk_run(cand))
  expect_setequal(unique(out$model), c("ensemble", "medoid"))
  medoid_obs <- ens_of(1L, list(k_cases = k), di)
  medoid_eng <- ens_of(1L, NULL, NULL)
  for (ch in c("cases", "deaths")) {
    same_draws(rows(out, "ensemble", ch), cand, ch)
    same_draws(rows(out, "medoid", ch), medoid_obs, ch)
    expect_false(identical(rows(out, "medoid", ch)$pi95_hi,
                           as.numeric(medoid_eng$ci_bounds[[ch]][[1]]$upper[1, ])), info = ch)
  }
  med <- rows(out, "medoid", "cases")
  expect_lt(sum(med$pred_median_obs), sum(med$pred_median))   # the small-k collapse
  expect_false(identical(rows(out, "medoid", "cases")$pi95_hi,
                         as.numeric(ens_of(1L, list(k_cases = 5), di)$ci_bounds$cases[[1]]$upper[1, ])))

  # An engine-level run (no observation model, no deaths redraw): engine-level medoid rows.
  cand0 <- ens_of(P, NULL, NULL)
  out0  <- compile(mk_run(cand0, with_di = FALSE))
  for (ch in c("cases", "deaths")) same_draws(rows(out0, "medoid", ch), medoid_eng, ch)

  # A run whose deaths were redrawn but whose deaths_integration.rds is missing warns.
  expect_warning(compile(mk_run(cand, with_di = FALSE)), "deaths_integration.rds not found")
})
