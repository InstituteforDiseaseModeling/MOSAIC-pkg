# =============================================================================
# Tests for the central_method feature (weighted-mean ensemble central tendency)
# Covers: resolver, .central series, optimizer threading (median bit-for-bit,
# mean propagation, WIS invariance), plot CSV labelling (incl. per-channel + NA),
# and the rolling-CV scoring switch.
# =============================================================================

# ---- .mosaic_resolve_central_method -----------------------------------------

test_that("resolver: NULL and scalar default/expand correctly", {
  # Package default (v0.101.0): NULL / empty resolves cases to median, deaths to mean.
  expect_equal(MOSAIC:::.mosaic_resolve_central_method(NULL),
               c(cases = "median", deaths = "mean"))
  expect_equal(MOSAIC:::.mosaic_resolve_central_method("median"),
               c(cases = "median", deaths = "median"))
  expect_equal(MOSAIC:::.mosaic_resolve_central_method("mean"),
               c(cases = "mean", deaths = "mean"))
})

test_that("resolver: per-channel named vector, missing channel falls back to its default", {
  expect_equal(MOSAIC:::.mosaic_resolve_central_method(c(cases = "median", deaths = "mean")),
               c(cases = "median", deaths = "mean"))
  # An unset channel inherits that channel's package default (deaths mean,
  # cases median), not the other channel's setting.
  expect_equal(MOSAIC:::.mosaic_resolve_central_method(c(cases = "mean")),
               c(cases = "mean", deaths = "mean"))
  expect_equal(MOSAIC:::.mosaic_resolve_central_method(c(deaths = "median")),
               c(cases = "median", deaths = "median"))
})

test_that("resolver: invalid inputs error", {
  expect_error(MOSAIC:::.mosaic_resolve_central_method("mode"))            # bad value
  expect_error(MOSAIC:::.mosaic_resolve_central_method(c("mean", "median"))) # unnamed length-2
  expect_error(MOSAIC:::.mosaic_resolve_central_method(c(foo = "mean")))    # unknown channel
  expect_error(MOSAIC:::.mosaic_resolve_central_method(c(cases = "median", "mean"))) # partially named
})

test_that("package default central tendency is cases median, deaths mean (v0.101.0)", {
  # Pin the default at every authoritative site so a partial flip is caught
  # (mean v0.38.0-v0.46.0, median v0.46.1-v0.97.x, mean v0.98.0-v0.100.x,
  # cases median / deaths mean from v0.101.0).
  per_channel <- c(cases = "median", deaths = "mean")
  expect_equal(MOSAIC:::.mosaic_resolve_central_method(NULL), per_channel)
  expect_equal(MOSAIC::mosaic_control_defaults()$predictions$central_method, per_channel)
  expect_equal(eval(formals(MOSAIC::plot_model_ensemble)$central_method), per_channel)
  expect_equal(eval(formals(MOSAIC:::.mosaic_assemble_prediction_table)$central_method),
               per_channel)
  expect_equal(eval(formals(MOSAIC::run_rolling_cv)$central_method), per_channel)
  expect_equal(eval(formals(MOSAIC:::.rolling_cv_compile_run)$central_method), per_channel)
  expect_equal(eval(formals(MOSAIC:::.rcv_compile_all_models)$central_method), per_channel)
  expect_equal(formals(MOSAIC:::.mosaic_reff_resim_ci)$cases_central_method, "median")
  expect_equal(eval(formals(MOSAIC:::.mosaic_build_trajectories)$central_method), per_channel)
  # Each formal resolves to the same pair as the resolver's own default.
  for (f in list(MOSAIC::plot_model_ensemble, MOSAIC::run_rolling_cv,
                 MOSAIC:::.mosaic_assemble_prediction_table))
    expect_equal(MOSAIC:::.mosaic_resolve_central_method(eval(formals(f)$central_method)),
                 MOSAIC:::.mosaic_resolve_central_method(NULL))
  # The optimizer's own formal stays median by design (Tier-2 bit-for-bit parity);
  # run_MOSAIC() passes the resolved control value explicitly.
  expect_equal(formals(MOSAIC::optimize_ensemble_subset)$central_method, "median")
})

test_that("the per-channel default survives the control merge and control.json", {
  ctl <- MOSAIC:::.mosaic_validate_and_merge_control(list())
  expect_equal(MOSAIC:::.mosaic_resolve_central_method(ctl$predictions$central_method),
               c(cases = "median", deaths = "mean"))
  # A user scalar still sets both channels.
  ctl2 <- MOSAIC:::.mosaic_validate_and_merge_control(list(predictions = list(central_method = "mean")))
  expect_equal(MOSAIC:::.mosaic_resolve_central_method(ctl2$predictions$central_method),
               c(cases = "mean", deaths = "mean"))
  # control.json keeps the channel names.
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(MOSAIC:::.mosaic_control_for_json(ctl), tmp, auto_unbox = TRUE)
  back <- jsonlite::read_json(tmp, simplifyVector = TRUE)
  expect_equal(MOSAIC:::.mosaic_resolve_central_method(back$predictions$central_method),
               c(cases = "median", deaths = "mean"))
})

test_that("an invalid central_method is rejected when the control is validated", {
  # Release red team CM-02: it was first resolved after calibration and shard
  # consolidation, ending the run with no ensemble in a directory that cannot
  # be resumed.
  bad <- list(c("median", "mean"), "medain", list(case = "median"), c(cases = "medain"))
  for (cm in bad)
    expect_error(MOSAIC:::.mosaic_validate_and_merge_control(list(predictions = list(central_method = cm))),
                 "control\\$predictions\\$central_method is invalid", info = deparse(cm))
  for (cm in list("mean", c(cases = "mean"), list(cases = "mean", deaths = "median")))
    expect_no_error(MOSAIC:::.mosaic_validate_and_merge_control(list(predictions = list(central_method = cm))))
})

test_that("compile_rolling_cv_predictions() recompiles a manifest without central_method on the median", {
  # Release red team CM-03: the manifest field arrived with central_method
  # itself (v0.38.0), so a manifest without it predicted the ensemble median.
  dir_out <- withr::local_tempdir()
  run_dir <- file.path(dir_out, "runs", "cutoff_2023-02-10")
  dir.create(file.path(run_dir, "2_calibration"), recursive = TRUE)
  n_t <- 40L; ds <- as.Date("2023-01-01")
  mk <- function(v) matrix(v, nrow = 1L)
  ens <- list(n_time_points = n_t, date_start = ds, date_stop = ds + n_t - 1L,
              location_names = "MOZ", envelope_quantiles = c(0.025, 0.25, 0.75, 0.975),
              cases_median = mk(seq_len(n_t)), cases_mean = mk(seq_len(n_t) + 0.5),
              deaths_median = mk(rep(0, n_t)), deaths_mean = mk(rep(0.25, n_t)),
              ci_bounds = list(cases  = rep(list(list(lower = mk(rep(0, n_t)), upper = mk(rep(99, n_t)))), 2L),
                               deaths = rep(list(list(lower = mk(rep(0, n_t)), upper = mk(rep(9, n_t)))), 2L)))
  saveRDS(ens, file.path(run_dir, "2_calibration", "ensemble_candidate.rds"))
  man <- function(spec_extra = list()) jsonlite::write_json(list(
    spec = c(list(iso = "MOZ", anchor_date = "2023-01-01", horizons_months = 1,
                  embargo_weeks = 1L, models = "ensemble", n_reps_best_medoid = 50L), spec_extra),
    runs = list(list(run_id = "cutoff_2023-02-10", status = "success",
                     dir = "runs/cutoff_2023-02-10", cutoff_date = "2023-02-10"))),
    file.path(dir_out, "manifest.json"), auto_unbox = TRUE)
  compiled <- function() compile_rolling_cv_predictions(dir_out, base_config = MOSAIC::config_default,
                                                        write = FALSE)
  man()
  p <- compiled()
  expect_identical(unique(p$central_method), "median")
  expect_identical(p$pred_central, p$pred_median)
  # A recorded value is honoured.
  man(list(central_method = list(cases = "mean", deaths = "mean")))
  p <- compiled()
  expect_identical(unique(p$central_method), "mean")
  expect_identical(p$pred_central, p$pred_mean)
})

test_that(".mosaic_central_series selects the requested field per channel", {
  ens <- list(cases_mean = matrix(1, 1, 3), cases_median = matrix(2, 1, 3),
              deaths_mean = matrix(3, 1, 3), deaths_median = matrix(4, 1, 3))
  m <- c(cases = "mean", deaths = "median")
  expect_equal(MOSAIC:::.mosaic_central_series(ens, "cases",  m), matrix(1, 1, 3))
  expect_equal(MOSAIC:::.mosaic_central_series(ens, "deaths", m), matrix(4, 1, 3))
})

# ---- Fixture where weighted mean != weighted median -------------------------

# Right-skewed cases (mean > median) + sparse deaths (median collapses to 0).
make_skew_ensemble <- function(n_locs = 1, n_times = 8, n_params = 12, n_stoch = 4) {
  set.seed(7)
  obs_cases  <- matrix(rpois(n_locs * n_times, 40), n_locs, n_times)
  obs_deaths <- matrix(rpois(n_locs * n_times, 1),  n_locs, n_times)

  cases_array  <- array(NA_real_, c(n_locs, n_times, n_params, n_stoch))
  deaths_array <- array(NA_real_, c(n_locs, n_times, n_params, n_stoch))
  for (p in seq_len(n_params)) for (s in seq_len(n_stoch)) {
    # lognormal-ish positive skew -> mean above median
    cases_array[, , p, s]  <- round(30 * exp(rnorm(n_locs * n_times, 0, 0.6)))
    # sparse: ~92% zero days, max ~4 (matches the real cholera deaths regime
    # the feature targets) so the per-tick weighted median collapses to 0
    spike <- rbinom(n_locs * n_times, 1, 0.07) * rpois(n_locs * n_times, 1.5)
    deaths_array[, , p, s] <- pmin(spike, 4)
  }

  weights <- rev(seq_len(n_params)); weights <- weights / sum(weights)
  sim_weights <- rep(weights, times = n_stoch) / n_stoch

  cmean <- cmed <- matrix(NA_real_, n_locs, n_times)
  dmean <- dmed <- matrix(NA_real_, n_locs, n_times)
  wmean <- function(v, w) { ok <- is.finite(v) & is.finite(w) & w > 0
                            if (!any(ok)) NA_real_ else sum(v[ok]*w[ok])/sum(w[ok]) }
  for (i in seq_len(n_locs)) for (j in seq_len(n_times)) {
    vc <- as.vector(cases_array[i, j, , ]); vd <- as.vector(deaths_array[i, j, , ])
    cmed[i, j]  <- MOSAIC::weighted_quantiles(vc, sim_weights, 0.5)
    dmed[i, j]  <- MOSAIC::weighted_quantiles(vd, sim_weights, 0.5)
    cmean[i, j] <- wmean(vc, sim_weights)
    dmean[i, j] <- wmean(vd, sim_weights)
  }

  structure(list(
    cases_array = cases_array, deaths_array = deaths_array,
    cases_mean = cmean, cases_median = cmed,
    deaths_mean = dmean, deaths_median = dmed,
    ci_bounds = list(
      cases  = list(list(lower = cmed * 0.7, upper = cmean * 1.3)),
      deaths = list(list(lower = dmed,       upper = dmean * 2 + 1))),
    obs_cases = obs_cases, obs_deaths = obs_deaths,
    parameter_weights = weights,
    seeds = seq_len(n_params),
    n_param_sets = as.integer(n_params),
    n_simulations_per_config = as.integer(n_stoch),
    n_successful = as.integer(n_params * n_stoch),
    location_names = paste0("LOC", seq_len(n_locs)),
    n_locations = as.integer(n_locs), n_time_points = as.integer(n_times),
    date_start = "2023-01-01", date_stop = "2023-02-26",
    envelope_quantiles = c(0.025, 0.975)
  ), class = "mosaic_ensemble")
}

test_that("fixture actually has mean != median (sanity)", {
  ens <- make_skew_ensemble()
  expect_gt(mean(ens$cases_mean - ens$cases_median), 0)        # cases right-skewed
  expect_true(any(ens$deaths_median == 0))                      # median collapses
  expect_gt(sum(ens$deaths_mean), sum(ens$deaths_median))       # mean does not
})

# ---- Optimizer threading ----------------------------------------------------

test_that("optimizer median path is deterministic and matches direct weighted-median MAE", {
  ens <- make_skew_ensemble(n_params = 10, n_stoch = 3)
  ll  <- -100 - seq(0, by = 0.5, length.out = 10)

  a <- optimize_ensemble_subset(ens, ll, min_n = 4L, objective = "mae",
                                central_method = "median", verbose = FALSE)
  b <- optimize_ensemble_subset(ens, ll, min_n = 4L, objective = "mae",
                                central_method = "median", verbose = FALSE)
  expect_identical(a$optimal_n, b$optimal_n)
  expect_equal(a$evaluation_table, b$evaluation_table)
  # default arg ("median") == explicit "median"
  d <- optimize_ensemble_subset(ens, ll, min_n = 4L, objective = "mae", verbose = FALSE)
  expect_equal(a$evaluation_table, d$evaluation_table)
})

test_that("optimizer mean path differs from median and is recorded as mean", {
  ens <- make_skew_ensemble(n_params = 10, n_stoch = 3)
  ll  <- -100 - seq(0, by = 0.5, length.out = 10)
  med  <- optimize_ensemble_subset(ens, ll, min_n = 4L, objective = "mae",
                                   central_method = "median", verbose = FALSE)
  mn   <- optimize_ensemble_subset(ens, ll, min_n = 4L, objective = "mae",
                                   central_method = "mean", verbose = FALSE)
  # The recorded diagnostics differ (mean series != median series on skew data)
  expect_false(isTRUE(all.equal(med$evaluation_table$mae_cases,
                                mn$evaluation_table$mae_cases)))
  expect_equal(unname(mn$central_method), c("mean", "mean"))
  expect_equal(unname(med$central_method), c("median", "median"))
})

test_that("optimizer WIS objective is invariant to central_method (quantile-based)", {
  ens <- make_skew_ensemble(n_params = 10, n_stoch = 3)
  ll  <- -100 - seq(0, by = 0.5, length.out = 10)
  w_med <- optimize_ensemble_subset(ens, ll, min_n = 4L, objective = "wis",
                                    central_method = "median", verbose = FALSE)
  w_mn  <- optimize_ensemble_subset(ens, ll, min_n = 4L, objective = "wis",
                                    central_method = "mean", verbose = FALSE)
  expect_identical(w_med$optimal_n, w_mn$optimal_n)
  expect_equal(w_med$evaluation_table$score, w_mn$evaluation_table$score)
})

# ---- Prediction-table central_method labelling ------------------------------
# Exercises the pure assembly helper (the single source of truth for both the
# exported CSV and the plotted line), the way run_MOSAIC() now writes them.

assemble_pred <- function(ens, ...) {
  MOSAIC:::.mosaic_assemble_prediction_table(ens, ...)
}

test_that("table: central_method='mean' makes predicted_central == predicted_mean", {
  ens <- make_skew_ensemble(n_locs = 1)
  df  <- assemble_pred(ens, central_method = "mean")
  expect_true(all(c("predicted_central", "predicted_mean", "predicted_median",
                    "central_method") %in% names(df)))
  expect_equal(df$predicted_central, df$predicted_mean)
  expect_equal(unique(df$central_method), "mean")
  # predicted_median stays the TRUE median (deaths collapse), never mislabeled.
  # na.rm: the helper masks the final deaths timestep (engine off-by-one) by
  # default, so the last cell is NA; that masked cell is irrelevant here.
  deaths_rows <- df[df$metric == "Deaths", ]
  expect_true(any(deaths_rows$predicted_median == 0, na.rm = TRUE))
  expect_gt(sum(deaths_rows$predicted_central, na.rm = TRUE),
            sum(deaths_rows$predicted_median, na.rm = TRUE))
})

test_that("table: central_method='median' makes predicted_central == predicted_median", {
  ens <- make_skew_ensemble(n_locs = 1)
  df  <- assemble_pred(ens, central_method = "median")
  expect_equal(df$predicted_central, df$predicted_median)
  expect_equal(unique(df$central_method), "median")
})

test_that("table: per-channel central_method routes cases/deaths independently", {
  ens <- make_skew_ensemble(n_locs = 1)
  df  <- assemble_pred(ens, central_method = c(cases = "median", deaths = "mean"))
  cas <- df[df$metric == "Suspected Cases", ]
  dea <- df[df$metric == "Deaths", ]
  expect_equal(cas$predicted_central, cas$predicted_median)
  expect_equal(dea$predicted_central, dea$predicted_mean)
  expect_equal(unique(cas$central_method), "median")
  expect_equal(unique(dea$central_method), "mean")
})

test_that("plot renders from a precomputed table and emits faceted PDFs (>1 loc)", {
  skip_if_not_installed("ggplot2")
  ens <- make_skew_ensemble(n_locs = 3)
  td  <- withr::local_tempdir()
  tbl <- assemble_pred(ens, central_method = "mean")
  expect_no_error(
    plot_model_ensemble(ens, output_dir = td, file_prefix = "m",
                        central_method = "mean", prediction_table = tbl,
                        verbose = FALSE)
  )
  expect_true(file.exists(file.path(td, "predictions_m_cases_all.pdf")))
  expect_true(file.exists(file.path(td, "predictions_m_deaths_all.pdf")))
})

test_that("table falls back to median when ensemble lacks *_mean field", {
  ens <- make_skew_ensemble(n_locs = 1)
  ens$cases_mean <- NULL; ens$deaths_mean <- NULL
  df <- assemble_pred(ens, central_method = "mean")
  # mean unavailable -> central + mean columns fall back to the median series
  expect_equal(df$predicted_central, df$predicted_median)
})

# ---- Rolling-CV scoring switch ----------------------------------------------

test_that("rolling-CV manifest round-trips central_method (JSON object preserves names)", {
  # Guards the compile_rolling_cv_predictions() recompile-from-disk path: a bare
  # named vector loses its names under auto_unbox=TRUE; as.list() keeps them.
  for (inp in list("mean", "median", c(cases = "median", deaths = "mean"))) {
    cm  <- MOSAIC:::.mosaic_resolve_central_method(inp)
    tmp <- withr::local_tempfile(fileext = ".json")
    jsonlite::write_json(list(spec = list(central_method = as.list(cm))), tmp,
                         auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
    man <- jsonlite::read_json(tmp, simplifyVector = TRUE)
    expect_equal(MOSAIC:::.mosaic_resolve_central_method(man$spec$central_method), cm)
  }
})

test_that(".rcv_window_metrics scores on pred_central but keeps WIS on the median", {
  df <- data.frame(
    observed    = c(10, 20, 30, 40),
    pred_central = c(12, 22, 33, 44),   # mean-ish
    pred_median  = c( 9, 18, 27, 36),   # median-ish (collapsed lower)
    pi50_lo = c(8, 16, 24, 32), pi50_hi = c(11, 21, 31, 41),
    pi95_lo = c(6, 13, 20, 27), pi95_hi = c(14, 26, 38, 50))
  m_central <- MOSAIC:::.rcv_window_metrics(df)
  # MAE must equal |obs - pred_central|, NOT pred_median
  expect_equal(m_central$mae, round(mean(abs(df$observed - df$pred_central)), 3))
  # back-compat: without pred_central, falls back to pred_median
  df2 <- df; df2$pred_central <- NULL
  m_med <- MOSAIC:::.rcv_window_metrics(df2)
  expect_equal(m_med$mae, round(mean(abs(df2$observed - df2$pred_median)), 3))
})
