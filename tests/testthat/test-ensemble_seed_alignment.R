# Regression coverage for the ensemble member <-> seed alignment that drives
# best/medoid selection in run_MOSAIC().
#
# BUG (observed in a 60k MOZ run, MOSAIC v0.36.1): the medoid metric correctly
# selected the central ensemble member (cases_array slice), but the seed it was
# mapped to came from a SEPARATE vector that had drifted out of positional
# alignment with cases_array. config_medoid was then sampled from the wrong
# (low-transmission) seed and the medoid prediction collapsed to ~zero while the
# selected member's own trajectory was perfectly healthy.
#
# FIX: calc_model_ensemble() now carries a per-member `seeds` vector aligned with
# cases_array (member i <-> seeds[i]), sourced from the parameter set that
# PRODUCED each member: the member's own config $seed, else positional
# parameter_seeds. optimize_ensemble_subset() carries it through the sort/slice,
# and run_MOSAIC()'s medoid uses ensemble$seeds[medoid_idx].
#
# A third source used to sit ahead of those two: a `param_seed` field on the
# result record, which existed because a Dask worker held the config the master
# did not. Both that field and the precomputed_results argument that delivered it
# went with the Dask backend (v0.67.0). The invariant under test is unchanged --
# a member's seed must describe that member's own trajectory -- and is now
# exercised through the config-$seed tier, which is what the local path uses.
#
# Engine output is injected by mocking the per-task worker; see
# helper-ensemble-mock.R. These tests are pure construction (no Python, no
# packaged data) and always run.

library(testthat)
library(MOSAIC)

# Minimal single-location config supplying just the metadata calc_model_ensemble
# reads (observed matrices, location, dates).
n_t        <- 5L
base_config <- list(
  location_name   = "TEST",
  date_start      = "2020-01-01",
  date_stop       = "2020-01-05",
  reported_cases  = matrix(c(10, 20, 30, 20, 10), nrow = 1),
  reported_deaths = matrix(c(1, 2, 3, 2, 1),       nrow = 1)
)
np <- 4L

# Result records: member p has a constant trajectory of p*10, so a member's seed
# is recoverable from its own trajectory whenever the seed is 100 + p.
make_records <- function() {
  lapply(seq_len(np), function(p) list(
    param_idx       = p,
    stoch_idx       = 1L,
    reported_cases  = matrix(p * 10, nrow = 1, ncol = n_t),
    reported_deaths = matrix(p,      nrow = 1, ncol = n_t),
    success         = TRUE
  ))
}

# Configs whose $seed is the GROUND TRUTH binding for member p: 100 + p.
make_seeded_configs <- function() {
  lapply(seq_len(np), function(p) { cc <- base_config; cc$seed <- 100L + p; cc })
}

test_that("calc_model_ensemble binds member seeds to the producing param set, not a positional vector", {
  local_mocked_ensemble_sims(make_records())
  ens <- calc_model_ensemble(
    config                   = base_config,
    configs                  = make_seeded_configs(),
    parameter_seeds          = c(901L, 902L, 903L, 904L),  # deliberately WRONG order/values
    parameter_weights        = rep(1, np),
    n_simulations_per_config = 1L,
    verbose                  = FALSE
  )

  # seeds come from each member's own config $seed (ground truth), NOT the
  # positional parameter_seeds vector.
  expect_equal(ens$seeds, c(101L, 102L, 103L, 104L))
  expect_false(any(ens$seeds %in% c(901L, 902L, 903L, 904L)))

  # the seed attached to member p genuinely describes member p's trajectory.
  for (p in seq_len(np)) {
    traj_level <- ens$cases_array[1, 1, p, 1]
    expect_equal(traj_level, p * 10)
    expect_equal(ens$seeds[p], 100L + as.integer(traj_level / 10))
  }
})

test_that("seed<->member alignment survives optimize_ensemble_subset sort/slice", {
  local_mocked_ensemble_sims(make_records())
  ens <- calc_model_ensemble(
    config                   = base_config,
    configs                  = make_seeded_configs(),
    parameter_seeds          = c(901L, 902L, 903L, 904L),
    parameter_weights        = rep(1, np),
    n_simulations_per_config = 1L,
    verbose                  = FALSE
  )

  opt <- optimize_ensemble_subset(
    ens,
    likelihoods = c(10, 40, 20, 30),          # reorders members under the hood
    seeds       = c(901L, 902L, 903L, 904L),  # WRONG; ensemble$seeds must take precedence
    min_n       = 2L,
    objective   = "mae",
    verbose     = FALSE
  )
  eo <- opt$ensemble_optimized

  # The optimized ensemble carries member-aligned seeds, and every member's seed
  # still matches its OWN trajectory after the internal sort + top-N slice. This
  # is the exact invariant the medoid relies on (and that the bug violated).
  expect_false(is.null(eo$seeds))
  for (k in seq_len(eo$n_param_sets)) {
    expect_equal(eo$seeds[k], 100L + as.integer(eo$cases_array[1, 1, k, 1] / 10))
  }
  # ensemble_optimized$seeds (the medoid's source) follows cases_array, NOT the
  # supplied `seeds` arg -- so the deliberately-misaligned arg cannot leak in.
  expect_false(any(eo$seeds %in% c(901L, 902L, 903L, 904L)))
  # optimal_seeds stays aligned with the `seeds` arg / optimal_weights for the
  # downstream seed->weight mapping -- deliberately independent of eo$seeds.
  expect_true(all(opt$optimal_seeds %in% c(901L, 902L, 903L, 904L)))
})

test_that("calc_model_ensemble seed fallbacks: config $seed, then positional parameter_seeds", {
  local_mocked_ensemble_sims(make_records())

  # (a) direct-config path: take the seed from each member's config.
  cfgs <- lapply(seq_len(np), function(p) { cc <- base_config; cc$seed <- 200L + p; cc })
  ens_cfg <- calc_model_ensemble(
    config = base_config, configs = cfgs, parameter_weights = rep(1, np),
    n_simulations_per_config = 1L, verbose = FALSE)
  expect_equal(ens_cfg$seeds, c(201L, 202L, 203L, 204L))

  # (b) last-resort positional fallback when no config carries a $seed.
  cfgs_noseed <- rep(list(base_config), np)   # base_config has no $seed
  ens_pos <- calc_model_ensemble(
    config = base_config, configs = cfgs_noseed,
    parameter_seeds = c(301L, 302L, 303L, 304L),
    parameter_weights = rep(1, np), n_simulations_per_config = 1L, verbose = FALSE)
  expect_equal(ens_pos$seeds, c(301L, 302L, 303L, 304L))
})
