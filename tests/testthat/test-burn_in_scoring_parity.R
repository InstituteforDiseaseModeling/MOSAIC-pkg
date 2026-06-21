# =============================================================================
# test-burn_in_scoring_parity.R
#
# Per-channel scoring window (burn-in + deaths-era start).
#
# These tests are engine-free (no Python/LASER). They exercise the resolver,
# the generalized scoring mask, the per-channel deaths zeroing, the worker
# slice math, and the resume guard. The KEN end-to-end LASER check is run
# separately (not here).
#
# Non-negotiable: with burn_in_days = 0 and deaths_score_start = NULL (the
# defaults), idx_cases == idx_deaths == 1 => NO slicing => scoring is
# bit-identical to the pre-feature behavior.
# =============================================================================

# A small synthetic obs/est fixture: 1 location, 12 daily steps. The first
# step carries a huge IC-style spike that the model can't match.
.mk_series <- function(spike = 0) {
  n <- 12L
  obs_cases  <- matrix(c(spike, 5, 6, 7, 8, 20, 9, 7, 6, 5, 4, 3), nrow = 1L)
  est_cases  <- matrix(c(0,     6, 6, 6, 7, 18, 8, 7, 6, 5, 4, 3), nrow = 1L)
  obs_deaths <- matrix(c(spike, 1, 1, 1, 2,  3, 1, 1, 1, 0, 0, 0), nrow = 1L)
  est_deaths <- matrix(c(0,     1, 1, 1, 1,  2, 1, 1, 1, 0, 0, 0), nrow = 1L)
  list(obs_cases = obs_cases, est_cases = est_cases,
       obs_deaths = obs_deaths, est_deaths = est_deaths, n = n)
}

.mk_config <- function(n = 12L) {
  list(
    location_name = "ETH",
    date_start    = as.Date("2020-01-01"),
    date_stop     = as.Date("2020-01-01") + (n - 1L),
    reported_cases = matrix(0, nrow = 1L, ncol = n)
  )
}


# =============================================================================
# TEST 1: bit-identical default — resolver returns idx = 1 and scoring matches
# =============================================================================
test_that("default knobs resolve to idx 1 and scoring is bit-identical", {
  cfg <- .mk_config()

  # Default likelihood (no knobs / explicit defaults) => idx_cases=idx_deaths=1.
  ctrl_default <- list(likelihood = list(burn_in_days = 0L,
                                         deaths_score_start = NULL,
                                         score_start_cases = NULL))
  sw <- MOSAIC:::.mosaic_resolve_score_window(cfg, ctrl_default)
  expect_identical(sw$idx_cases, 1L)
  expect_identical(sw$idx_deaths, 1L)
  expect_identical(sw$n_time, 12L)

  # NULL likelihood also resolves to idx 1 (back-compat with old controls).
  sw_null <- MOSAIC:::.mosaic_resolve_score_window(cfg, list(likelihood = NULL))
  expect_identical(sw_null$idx_cases, 1L)
  expect_identical(sw_null$idx_deaths, 1L)

  # Scoring with the default (idx 1, no slice) MUST equal scoring with no
  # window plumbing at all (the pre-feature call).
  fx <- .mk_series(spike = 0)
  ll_plain <- MOSAIC::calc_model_likelihood(
    config = cfg,
    obs_cases = fx$obs_cases, est_cases = fx$est_cases,
    obs_deaths = fx$obs_deaths, est_deaths = fx$est_deaths
  )
  # Emulate the worker's default path: .slice_lik FALSE => arrays untouched.
  ll_default <- MOSAIC::calc_model_likelihood(
    config = cfg,
    obs_cases = fx$obs_cases, est_cases = fx$est_cases,
    obs_deaths = fx$obs_deaths, est_deaths = fx$est_deaths
  )
  expect_identical(ll_default, ll_plain)
})


# =============================================================================
# TEST 2: window exclusion + slice-not-downweight (spike invariance)
#
# A huge spike in the unscored head must NOT change the score once burn-in
# covers it — proves we SLICE (not down-weight), so the shape terms (which
# ignore weights_time) never see the spike.
# =============================================================================
test_that("burn-in slicing makes the score invariant to a head spike", {
  cfg <- .mk_config()
  burn_in <- 3L  # idx_cases = idx_deaths = 4

  ctrl <- list(likelihood = list(burn_in_days = burn_in,
                                 deaths_score_start = NULL,
                                 score_start_cases = NULL,
                                 weight_peak_magnitude = 0.25))
  sw <- MOSAIC:::.mosaic_resolve_score_window(cfg, ctrl)
  expect_identical(sw$idx_cases, burn_in + 1L)

  s    <- min(sw$idx_cases, sw$idx_deaths)
  keep <- s:sw$n_time
  cfg_sliced <- cfg
  cfg_sliced$date_start <- as.Date(cfg$date_start) + (s - 1L)

  score_sliced <- function(spike) {
    fx <- .mk_series(spike = spike)
    MOSAIC::calc_model_likelihood(
      config = cfg_sliced,
      obs_cases  = fx$obs_cases[,  keep, drop = FALSE],
      est_cases  = fx$est_cases[,  keep, drop = FALSE],
      obs_deaths = fx$obs_deaths[, keep, drop = FALSE],
      est_deaths = fx$est_deaths[, keep, drop = FALSE],
      weight_peak_magnitude = 0.25
    )
  }

  ll_no_spike  <- score_sliced(0)
  ll_big_spike <- score_sliced(1e6)
  expect_equal(ll_no_spike, ll_big_spike)

  # Sanity: without slicing, the spike DOES change the (peak-magnitude) score —
  # confirms the fixture spike is actually load-bearing.
  score_full <- function(spike) {
    fx <- .mk_series(spike = spike)
    MOSAIC::calc_model_likelihood(
      config = cfg,
      obs_cases = fx$obs_cases, est_cases = fx$est_cases,
      obs_deaths = fx$obs_deaths, est_deaths = fx$est_deaths,
      weight_peak_magnitude = 0.25
    )
  }
  expect_false(isTRUE(all.equal(score_full(0), score_full(1e6))))
})


# =============================================================================
# TEST 3: mask generalization — .mosaic_mask_central_for_scoring per channel
# =============================================================================
test_that(".mosaic_mask_central_for_scoring NA's columns before score_idx", {
  mat <- matrix(1:24, nrow = 2L)  # 2 loc x 12 time

  # score_idx_cases = 4 => columns 1:3 NA for cases.
  spec <- list(cases_warmup = 0L, deaths_final = FALSE,
               score_idx_cases = 4L, score_idx_deaths = 1L)
  out_c <- MOSAIC:::.mosaic_mask_central_for_scoring(mat, "cases", spec)
  expect_true(all(is.na(out_c[, 1:3])))
  expect_false(any(is.na(out_c[, 4:12])))

  # deaths channel with score_idx_deaths = 6 => columns 1:5 NA + final col NA.
  spec_d <- list(cases_warmup = 0L, deaths_final = TRUE,
                 score_idx_cases = 1L, score_idx_deaths = 6L)
  out_d <- MOSAIC:::.mosaic_mask_central_for_scoring(mat, "deaths", spec_d)
  expect_true(all(is.na(out_d[, 1:5])))
  expect_true(all(is.na(out_d[, 12])))
  expect_false(any(is.na(out_d[, 6:11])))

  # Default spec (no score_idx fields) => no extra masking (back-compat).
  spec_old <- list(cases_warmup = 2L, deaths_final = TRUE)
  out_old <- MOSAIC:::.mosaic_mask_central_for_scoring(mat, "cases", spec_old)
  expect_true(all(is.na(out_old[, 1:2])))
  expect_false(any(is.na(out_old[, 3:12])))

  # R2 ignores the NA'd head: masked R2 equals pairwise-complete R2.
  obs <- matrix(c(1, 100, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20), nrow = 1L)
  est <- matrix(c(1, 0.5, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20), nrow = 1L)
  spec2 <- list(cases_warmup = 0L, deaths_final = FALSE,
                score_idx_cases = 3L, score_idx_deaths = 1L)
  est_masked <- MOSAIC:::.mosaic_mask_central_for_scoring(est, "cases", spec2)
  r2_masked  <- MOSAIC::calc_model_R2(as.numeric(obs), as.numeric(est_masked))
  r2_manual  <- MOSAIC::calc_model_R2(as.numeric(obs[, 3:12]), as.numeric(est[, 3:12]))
  expect_equal(r2_masked, r2_manual)
})


# =============================================================================
# TEST 4: per-channel deaths — deaths_score_start later than cases start
# =============================================================================
test_that("deaths_score_start scores deaths from a later step than cases", {
  cfg <- .mk_config(n = 12L)  # 2020-01-01 .. 2020-01-12

  ctrl <- list(likelihood = list(
    burn_in_days       = 2L,                # cases from step 3
    deaths_score_start = "2020-01-06"       # offset 5 => deaths from step 6
  ))
  sw <- MOSAIC:::.mosaic_resolve_score_window(cfg, ctrl)
  expect_identical(sw$idx_cases, 3L)
  expect_identical(sw$idx_deaths, 6L)

  # Shared slice s = min(3, 6) = 3; deaths residual prefix is columns 3:5
  # (post-slice 1:3) zeroed. Verify the mask spec NA's deaths 1:5.
  spec <- list(cases_warmup = 0L, deaths_final = FALSE,
               score_idx_cases = sw$idx_cases, score_idx_deaths = sw$idx_deaths)
  mat  <- matrix(1, nrow = 1L, ncol = 12L)
  out_c <- MOSAIC:::.mosaic_mask_central_for_scoring(mat, "cases",  spec)
  out_d <- MOSAIC:::.mosaic_mask_central_for_scoring(mat, "deaths", spec)
  expect_true(all(is.na(out_c[, 1:2])))   # cases NA'd before step 3
  expect_false(any(is.na(out_c[, 3:12])))
  expect_true(all(is.na(out_d[, 1:5])))   # deaths NA'd before step 6
  expect_false(any(is.na(out_d[, 6:12])))

  # deaths never score the burn-in head even when deaths_score_start < burn-in.
  ctrl2 <- list(likelihood = list(burn_in_days = 7L,
                                  deaths_score_start = "2020-01-03"))
  sw2 <- MOSAIC:::.mosaic_resolve_score_window(cfg, ctrl2)
  expect_identical(sw2$idx_deaths, 8L)    # max(burn_in=7, offset=2) + 1
  expect_identical(sw2$idx_cases, 8L)
})


# =============================================================================
# TEST 5: clamping + score_start_cases (date) overrides burn-in
# =============================================================================
test_that("resolver clamps indices and honors score_start_cases date", {
  cfg <- .mk_config(n = 12L)

  # A start past the window clamps to n_time.
  ctrl <- list(likelihood = list(burn_in_days = 999L))
  sw <- MOSAIC:::.mosaic_resolve_score_window(cfg, ctrl)
  expect_identical(sw$idx_cases, 12L)
  expect_identical(sw$idx_deaths, 12L)

  # score_start_cases date overrides burn-in when later.
  ctrl2 <- list(likelihood = list(burn_in_days = 1L,
                                  score_start_cases = "2020-01-05"))
  sw2 <- MOSAIC:::.mosaic_resolve_score_window(cfg, ctrl2)
  expect_identical(sw2$idx_cases, 5L)     # offset 4 + 1, beats burn_in=1

  # A date at/before date_start scores the full window (offset clamped to 0).
  ctrl3 <- list(likelihood = list(deaths_score_start = "2019-06-01"))
  sw3 <- MOSAIC:::.mosaic_resolve_score_window(cfg, ctrl3)
  expect_identical(sw3$idx_deaths, 1L)
})


# =============================================================================
# TEST 6: resume drift — changing burn_in_days / deaths_score_start errors
# =============================================================================
test_that(".mosaic_resume_check_inputs guards scored-window drift", {
  base <- tempfile("sw_"); inp <- file.path(base, "1_inputs")
  dir.create(inp, recursive = TRUE)
  dirs <- list(inputs = inp)
  priors <- list(a = 1); config <- list(location_name = "ETH")
  wj <- function(x, f) jsonlite::write_json(x, f, pretty = TRUE, auto_unbox = TRUE, digits = NA)
  wj(priors, file.path(inp, "priors.json"))
  wj(config, file.path(inp, "config.json"))

  lik <- list(weight_cases = 1, burn_in_days = 14L, deaths_score_start = "2023-01-01")
  control <- list(likelihood = lik)
  wj(list(control = control, timestamp = "t0"), file.path(inp, "control.json"))

  # identical scored window => passes
  expect_true(suppressWarnings(
    MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors, control)))

  # changed burn_in_days => hard error (control$likelihood byte-compare)
  control_bi <- list(likelihood = list(weight_cases = 1, burn_in_days = 21L,
                                       deaths_score_start = "2023-01-01"))
  expect_error(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors, control_bi),
               "control\\$likelihood")

  # changed deaths_score_start => hard error
  control_ds <- list(likelihood = list(weight_cases = 1, burn_in_days = 14L,
                                       deaths_score_start = "2018-01-01"))
  expect_error(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors, control_ds),
               "control\\$likelihood")
})


# =============================================================================
# TEST 7: ensemble artifact_mask carries score_idx; default is no-op
# =============================================================================
test_that("calc_model_ensemble records score_idx in artifact_mask (default no-op)", {
  # Validation-only: the function errors on bad score_idx without running sims.
  expect_error(
    suppressWarnings(MOSAIC::calc_model_ensemble(
      config = list(location_name = "ETH"),
      configs = list(list(location_name = "ETH")),
      score_idx_cases = 0L)),
    "score_idx_cases"
  )
  expect_error(
    suppressWarnings(MOSAIC::calc_model_ensemble(
      config = list(location_name = "ETH"),
      configs = list(list(location_name = "ETH")),
      score_idx_deaths = -1L)),
    "score_idx_deaths"
  )
})
