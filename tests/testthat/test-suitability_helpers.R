# Tier-1 (CI, pure-R, no keras) unit tests for the lstm_v2 suitability helpers.
# Internal (dot-prefixed) functions are reached via getFromNamespace so the
# tests pass under both devtools::test() and R CMD check (installed package).

ns <- "MOSAIC"
build_sequences   <- getFromNamespace(".psi_build_sequences", ns)
make_rw_cv_steps  <- getFromNamespace(".psi_make_rw_cv_steps", ns)
configure_loss    <- getFromNamespace(".psi_configure_loss", ns)
run_seed_ensemble <- getFromNamespace(".psi_run_seed_ensemble", ns)
resolve_region_map<- getFromNamespace(".psi_resolve_region_map", ns)

# ----------------------------------------------------------------------------
# .psi_make_rw_cv_steps — geometry, embargo, subsample, empty grid
# ----------------------------------------------------------------------------
test_that("make_rw_cv_steps respects the embargo and stays inside IS", {
  steps <- make_rw_cv_steps(fit_date_start = "2015-01-01",
                            cutoff_date = "2024-10-01",
                            step_months = 1L, test_months = 5L,
                            gap_weeks = 4L, subsample = 1L, timesteps = 13L)
  expect_gt(length(steps), 0L)
  for (s in steps) {
    # embargo: test_start is exactly gap_weeks*7 days after train_end
    expect_equal(as.numeric(s$test_start - s$train_end), 4L * 7L)
    # strictly inside IS
    expect_true(s$test_start < as.Date("2024-10-01"))
    expect_true(s$test_end   < as.Date("2024-10-01"))
    # test window long enough for at least one 13-week sequence (>= 98 days)
    expect_gte(as.numeric(s$test_end - s$test_start), 13L * 7L + 7L)
  }
})

test_that("make_rw_cv_steps subsample takes every Nth step and renumbers", {
  full <- make_rw_cv_steps("2015-01-01", "2024-10-01", subsample = 1L)
  sub  <- make_rw_cv_steps("2015-01-01", "2024-10-01", subsample = 6L)
  expect_lte(length(sub), length(full))
  expect_equal(vapply(sub, `[[`, integer(1), "step"), seq_along(sub))
  # subsampled train_ends are a subset of the full grid's train_ends
  expect_true(all(vapply(sub, function(s) s$train_end, as.Date(NA)) %in%
                  vapply(full, function(s) s$train_end, as.Date(NA))))
})

test_that("make_rw_cv_steps returns an empty grid when the window is too short", {
  steps <- make_rw_cv_steps("2024-08-01", "2024-10-01", test_months = 5L,
                            gap_weeks = 4L, timesteps = 13L)
  expect_equal(length(steps), 0L)
})

# ----------------------------------------------------------------------------
# .psi_build_sequences — country boundaries, gap skipping, cw/region anchoring
# ----------------------------------------------------------------------------
test_that("build_sequences respects country boundaries and gap skipping", {
  # Two countries, weekly dates; country B has a 4-week gap mid-series.
  dA <- seq(as.Date("2020-01-01"), by = "week", length.out = 6)
  dB <- c(seq(as.Date("2020-01-01"), by = "week", length.out = 3),
          seq(as.Date("2020-03-01"), by = "week", length.out = 3))  # 4-wk gap
  X  <- matrix(rnorm((length(dA) + length(dB)) * 2), ncol = 2)
  countries <- c(rep("AAA", length(dA)), rep("BBB", length(dB)))
  dates <- c(dA, dB)
  ts <- 3L
  out <- build_sequences(X, y = seq_len(nrow(X)), countries = countries,
                         dates = dates, timesteps = ts, max_gap_days = 14L,
                         country_id_lookup = c(AAA = 0L, BBB = 1L),
                         region_for_country = c(AAA = 0L, BBB = 0L))
  # No sequence may mix countries; output countries are valid labels
  expect_true(all(out$countries %in% c("AAA", "BBB")))
  # AAA: 6 points, ts=3 -> 4 sequences, all kept (no gap)
  expect_equal(sum(out$countries == "AAA"), 4L)
  # BBB: the sequences spanning the >14d gap are skipped
  expect_lt(sum(out$countries == "BBB"), 4L)
  # region_ids propagate from the lookup
  expect_true(all(out$region_ids == 0L))
})

test_that("build_sequences anchors cw and y to the LAST (target) timestep", {
  d <- seq(as.Date("2020-01-01"), by = "week", length.out = 5)
  X <- matrix(0, nrow = 5, ncol = 2)
  y <- c(10, 20, 30, 40, 50)
  cw <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  out <- build_sequences(X, y = y, countries = rep("AAA", 5), dates = d,
                         timesteps = 3L, cw = cw)
  # 5 points, ts=3 -> 3 sequences ending at indices 3,4,5
  expect_equal(out$y,  c(30, 40, 50))
  expect_equal(out$cw, c(0.3, 0.4, 0.5))
})

# ----------------------------------------------------------------------------
# .psi_configure_loss — balanced_uniform ratio, CW total-weight preservation
# ----------------------------------------------------------------------------
test_that("balanced_uniform equalizes aggregate zero vs nonzero loss (balance_R)", {
  y <- c(rep(0, 88), runif(12, 0.1, 1))   # ~88% zeros
  lc1 <- configure_loss(y, sample_weights = "balanced_uniform", balance_R = 1.0)
  w   <- lc1$sample_weight_train
  tot_zero <- sum(w[y == 0]); tot_nz <- sum(w[y > 0])
  expect_equal(tot_zero, tot_nz, tolerance = 1e-8)        # 1:1 at balance_R=1
  lc2 <- configure_loss(y, sample_weights = "balanced_uniform", balance_R = 2.0)
  w2  <- lc2$sample_weight_train
  expect_equal(sum(w2[y > 0]) / sum(w2[y == 0]), 2.0, tolerance = 1e-8)
})

test_that("confidence-weight overlay preserves total weight", {
  y  <- c(rep(0, 50), runif(50, 0.1, 1))
  cw <- runif(100, 0.33, 1.0)
  base <- configure_loss(y, sample_weights = "balanced_uniform")
  over <- configure_loss(y, sample_weights = "balanced_uniform",
                         confidence_weight_train = cw)
  expect_equal(sum(over$sample_weight_train), sum(base$sample_weight_train),
               tolerance = 1e-8)
  expect_true(over$use_confidence_weight)
})

test_that("configure_loss errors on a confidence-weight length mismatch", {
  y <- c(rep(0, 10), runif(10, 0.1, 1))
  expect_error(configure_loss(y, sample_weights = "balanced_uniform",
                              confidence_weight_train = runif(5)),
               "length mismatch")
})

# ----------------------------------------------------------------------------
# .psi_run_seed_ensemble — aggregation schema + quantile ordering (mock arch)
# ----------------------------------------------------------------------------
test_that("run_seed_ensemble aggregates a mock arch into the Option-A schema", {
  isos <- c("AAA", "BBB")
  dts  <- seq(as.Date("2021-01-07"), by = "week", length.out = 120)
  countries_pred <- rep(isos, each = length(dts))
  dates_pred     <- rep(dts, times = 2)
  obs_all <- data.frame(
    date = dates_pred, iso_code = countries_pred,
    cases = 0, intensity = 0, stringsAsFactors = FALSE)
  bundle <- list(
    dates_pred = dates_pred, countries_pred = countries_pred,
    pred_date_stop = max(dts), obs_all = obs_all, target_iso = "AAA",
    X_pred = array(0, dim = c(length(dates_pred), 3, 2)))
  # Mock arch: seed-dependent smooth ramp in (0,1), distinct per seed so
  # cross-seed quantiles are non-degenerate and ordered.
  mock_fit <- function(data_bundle, seed, hyperparams) {
    n <- length(data_bundle$dates_pred)
    base <- seq(0.2, 0.6, length.out = n)
    list(pred = pmin(0.95, pmax(0.05, base + (seed %% 7) * 0.01)),
         val_loss = 0.1, val_metric = 0.1, n_epochs = 10L, loss_type = "bce")
  }
  ens <- run_seed_ensemble(mock_fit, bundle, seeds = c(11L, 22L, 33L),
                           verbose = FALSE)
  el <- ens$ensemble_long
  expect_true(all(c("iso", "date", "pred_raw", "pred_smooth",
                    "q025", "q25", "q75", "q975", "cases", "intensity") %in% names(el)))
  expect_setequal(unique(el$iso), isos)
  # all predictions in [0,1]
  expect_true(all(el$pred_raw >= 0 & el$pred_raw <= 1, na.rm = TRUE))
  expect_true(all(el$pred_smooth >= 0 & el$pred_smooth <= 1, na.rm = TRUE))
  # seed-dispersion quantiles are ordered
  expect_true(all(el$q025 <= el$q25 + 1e-9, na.rm = TRUE))
  expect_true(all(el$q25  <= el$pred_smooth + 1e-9, na.rm = TRUE))
  expect_true(all(el$pred_smooth <= el$q75 + 1e-9, na.rm = TRUE))
  expect_true(all(el$q75  <= el$q975 + 1e-9, na.rm = TRUE))
  # fit_info records one ok row per seed
  expect_equal(sum(ens$fit_info$status == "ok"), 3L)
})

test_that("run_seed_ensemble errors when all seeds fail", {
  isos <- "AAA"; dts <- seq(as.Date("2023-01-05"), by = "week", length.out = 20)
  bundle <- list(
    dates_pred = dts, countries_pred = rep(isos, length(dts)),
    pred_date_stop = max(dts),
    obs_all = data.frame(date = dts, iso_code = isos, cases = 0, intensity = 0),
    target_iso = "AAA", X_pred = array(0, dim = c(length(dts), 3, 2)))
  boom <- function(data_bundle, seed, hyperparams) stop("boom")
  expect_error(run_seed_ensemble(boom, bundle, seeds = c(11L, 22L), verbose = FALSE),
               "ALL seeds failed")
})

# ----------------------------------------------------------------------------
# .psi_resolve_region_map — schema, exhaustiveness over the MOSAIC-40
# ----------------------------------------------------------------------------
test_that("resolve_region_map returns exhaustive maps (or NULL for csv)", {
  m40 <- MOSAIC::iso_codes_mosaic
  expect_null(resolve_region_map("csv"))
  for (nm in c("snf_k5", "snf_k4", "seasonal_v1", "hydro_v1")) {
    rm <- resolve_region_map(nm)
    expect_true(is.character(rm) && !is.null(names(rm)))
    # every MOSAIC-40 country is mapped (no _unmapped_ would arise)
    expect_true(all(m40 %in% names(rm)), info = nm)
    expect_false(any(is.na(rm[m40])), info = nm)
  }
  # expected cluster counts
  expect_equal(length(unique(resolve_region_map("snf_k5")[m40])), 5L)
  expect_equal(length(unique(resolve_region_map("snf_k4")[m40])), 4L)
  expect_equal(length(unique(resolve_region_map("seasonal_v1")[m40])), 4L)
})

test_that("resolve_region_map rejects an unknown map name", {
  expect_error(resolve_region_map("not_a_map"), "should be one of")
})
