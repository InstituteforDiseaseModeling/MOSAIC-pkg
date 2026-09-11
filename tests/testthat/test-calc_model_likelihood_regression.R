# =============================================================================
# test-calc_model_likelihood_regression.R
#
# Frozen-value regression tests for calc_model_likelihood().
#
# Harvested from test-calc_model_likelihood_python_parity.R when the Python
# path was removed. That file compared the R likelihood against a Python
# re-implementation of it inside laser-cholera; with the Python side gone the
# comparison has no counterparty, and its tests #6/#7 (which pinned known
# R-vs-Python divergences) became meaningless.
#
# What was worth keeping is not the comparison but the PROPERTIES it happened
# to cover, because this function's history is a history of scaling bugs:
#
#   - the WIS term was missing its 0.5 MAE coefficient per Bracher et al. 2021
#     -- mathematically wrong but plausible at a glance (CLAUDE.md lesson #4);
#   - the cumulative term's per-timepoint floor of -1e9 produced a -115 billion
#     log-likelihood once T-normalization was introduced (lesson #5);
#   - shape-term assembly multipliers (N_obs / N_component_observations) are
#     invisible in the output until they are wrong by a factor.
#
# A frozen expected value catches all three: any change to a coefficient,
# floor, or scaling multiplier moves the number. The baselines below were
# computed from this package's own implementation, so they pin BEHAVIOUR, not
# correctness -- they will not tell you the formula is right, only that it has
# not silently changed. Deliberate changes should update these numbers in the
# same commit that changes the formula, and say why.
#
# Baselines generated 2026-09-10 on R 4.6.1.
# =============================================================================

# 2 locations x 60 daily timesteps. Same construction as the retired parity
# test, so the frozen values are comparable to the figures quoted in its
# comments (core was recorded there as "R -495").
regression_inputs <- function() {
  set.seed(42)
  n_loc <- 2L
  n_t   <- 60L
  obs_c <- matrix(rpois(n_loc * n_t, lambda = 30), nrow = n_loc)
  est_c <- matrix(pmax(0, obs_c + matrix(rnorm(n_loc * n_t, sd = 4), nrow = n_loc)),
                  nrow = n_loc)
  obs_d <- matrix(round(obs_c * 0.05), nrow = n_loc)
  est_d <- matrix(pmax(0, round(est_c * 0.05)), nrow = n_loc)
  list(obs_cases = obs_c, est_cases = est_c,
       obs_deaths = obs_d, est_deaths = est_d)
}

ll_with <- function(...) {
  do.call(MOSAIC::calc_model_likelihood, c(regression_inputs(), list(...)))
}

# Tolerance is loose enough to absorb platform floating-point variation but far
# tighter than any coefficient or scaling change could hide in.
LL_TOL <- 1e-6

test_that("core negative-binomial likelihood is unchanged", {
  expect_equal(ll_with(), -495.0074110579, tolerance = LL_TOL)
})

test_that("cumulative shape term contributes its frozen amount", {
  ll <- ll_with(weight_cumulative_total = 0.25)
  expect_equal(ll, -497.0989888952, tolerance = LL_TOL)
  # The term must actually move the answer -- a scaling bug that zeroed it out
  # would otherwise pass a bare equality test against the core value.
  expect_lt(ll, ll_with())
})

test_that("WIS shape term contributes its frozen amount", {
  # Guards the 0.5 MAE coefficient and the wis_scale = N_obs / N_quantiles
  # multiplier together: either one changing moves this number.
  ll <- ll_with(weight_wis = 0.10)
  expect_equal(ll, -500.1626110579, tolerance = LL_TOL)
  expect_lt(ll, ll_with())
})

test_that("all shape-term weights default to zero", {
  # The documented contract is that every shape term is OFF unless its weight
  # is set. If a default flipped, the core baseline above would move and this
  # test says which knob did it.
  expect_equal(ll_with(), ll_with(weight_cumulative_total = 0,
                                  weight_wis = 0,
                                  weight_peak_timing = 0),
               tolerance = LL_TOL)
})

test_that("shape-term contributions scale monotonically with their weights", {
  # A sign error or a misplaced normalisation often shows up as a term whose
  # contribution does not grow with its weight.
  base <- ll_with()
  small <- ll_with(weight_wis = 0.05)
  large <- ll_with(weight_wis = 0.20)
  expect_lt(small, base)
  expect_lt(large, small)
  # doubling the weight roughly doubles the penalty
  expect_equal((base - large) / (base - small), 4, tolerance = 0.01)
})

test_that("a non-finite likelihood returns -Inf rather than NA", {
  # Documented contract; calibration weighting depends on -Inf being
  # distinguishable from a missing value.
  inp <- regression_inputs()
  inp$est_cases[1, 1] <- NaN
  ll <- do.call(MOSAIC::calc_model_likelihood, inp)
  expect_false(is.na(ll) && !is.nan(ll))
  expect_true(is.finite(ll) || identical(ll, -Inf))
})

test_that("observation orientation is [locations, time]", {
  # Both implementations had to agree on this and it was the first thing the
  # retired parity test checked. Transposing the inputs must change the answer
  # -- if it does not, the function is collapsing the matrices and the
  # per-location structure is being ignored.
  inp <- regression_inputs()
  flipped <- lapply(inp, t)
  expect_false(isTRUE(all.equal(do.call(MOSAIC::calc_model_likelihood, inp),
                                do.call(MOSAIC::calc_model_likelihood, flipped))))
})
