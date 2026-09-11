# =============================================================================
# test-laser_precompute.R
#
# Tier A parity: the four deterministic precomputed matrices against frozen
# recordings of the Python laser-cholera oracle.
#
# These have no RNG in them, which makes them the sharpest part of the parity
# story -- they can be compared exactly with no draw sequence to align first.
# Between them they validate the gravity model, the two-harmonic seasonality,
# the psi-normalisation and the beta-CDF decay map.
#
# Requires no Python; see tests/testthat/fixtures/ORACLE.md.
# =============================================================================

# float32 carries ~7 significant digits (eps 5.96e-8) and the oracle stores
# these matrices as float32 while the engine computes in double. Combined
# absolute AND relative tolerance: a purely relative test is wrong wherever a
# value is legitimately zero, which pi_ij's diagonal always is.
TIER_A_RTOL <- 1e-6
TIER_A_ATOL <- 1e-12

# pi_ij gets a looser tolerance -- deliberately per-matrix, not a global
# loosening. The oracle's haversine runs in float32: params.py coerces
# lat/lon to float32, and np.radians of a float32 array stays float32, so the
# whole `a` term is single-precision before the arcsin widens it. Its distance
# matrix therefore carries ~1e-6 relative error and THIS implementation is the
# more accurate of the two. Established by mimicking each float32 truncation in
# turn, which monotonically closed the gap (1.09e-6 -> 8.5e-7 -> 5.2e-7),
# confirming cumulative precision loss rather than a difference of formula.
# Observed worst case 1.17e-6; 1e-5 leaves ~10x headroom.
TIER_A_RTOL_PI_IJ <- 1e-5

tier_a_fixture <- function(name) {
  path <- testthat::test_path("fixtures", paste0(name, ".rds"))
  if (!file.exists(path)) testthat::skip(sprintf("Tier A fixture '%s' not committed", name))
  readRDS(path)
}

expect_matches_oracle <- function(got, want, nm) {
  rtol <- if (identical(nm, "pi_ij")) TIER_A_RTOL_PI_IJ else TIER_A_RTOL
  expect_false(is.null(got), info = sprintf("%s missing from params", nm))
  expect_identical(dim(got), dim(want), info = sprintf("%s dims", nm))
  d <- abs(got - want)
  tol <- TIER_A_ATOL + rtol * abs(want)
  bad <- which(d > tol)
  expect_length(bad, 0L)
  if (length(bad)) {
    i <- bad[1]
    fail(sprintf("%s: %d/%d elements exceed tolerance; first at [%d,%d]: %.12g vs oracle %.12g",
                 nm, length(bad), length(want),
                 (i - 1) %% nrow(want) + 1, (i - 1) %/% nrow(want) + 1,
                 got[i], want[i]))
  }
}

# -----------------------------------------------------------------------------
# Oracle parity
# -----------------------------------------------------------------------------

for (fx_name in c("tier_a_default", "tier_a_single_location")) {

  test_that(sprintf("deterministic precomputation matches the oracle (%s)", fx_name), {
    fx <- tier_a_fixture(fx_name)
    par <- laser_params(fx$meta$config_list, components = LASER_PIPELINE)

    expect_gt(length(fx$matrices), 0L)
    for (nm in names(fx$matrices)) {
      expect_matches_oracle(par[[nm]], fx$matrices[[nm]], nm)
    }
  })
}

# -----------------------------------------------------------------------------
# Structural properties the oracle comparison cannot catch
#
# These are the checks that survive once the oracle is gone, and they catch the
# class of bug where R and Python agree because both are wrong.
# -----------------------------------------------------------------------------

test_that("pi_ij is row-stochastic with a zero diagonal", {
  fx <- tier_a_fixture("tier_a_default")
  par <- laser_params(fx$meta$config_list, components = LASER_PIPELINE)

  expect_true(all(diag(par$pi_ij) == 0))
  expect_equal(rowSums(par$pi_ij), rep(1, par$npatches), tolerance = 1e-10)
  expect_true(all(par$pi_ij >= 0))
})

test_that("pi_ij is [[0]] for a single location rather than NaN", {
  # The only cell is the zeroed diagonal, so the row sum is 0. Dividing would
  # give NaN; the Python loop skipped the normalisation via its `continue`, and
  # laser_pi_ij() mirrors that with a `row_sum != 0` guard. Getting this wrong
  # poisons every downstream hazard with NaN for exactly the configuration
  # (one country) that is most often used for quick tests.
  fx <- tier_a_fixture("tier_a_single_location")
  par <- laser_params(fx$meta$config_list, components = LASER_PIPELINE)

  expect_identical(dim(par$pi_ij), c(1L, 1L))
  expect_identical(par$pi_ij[1, 1], 0)
  expect_false(anyNA(par$pi_ij))
})

test_that("delta_jt is bounded by the two decay times", {
  # psi = 0 gives 1/decay_days_short (fast decay), psi = 1 gives
  # 1/decay_days_long (slow decay). A sign error or a swapped fast/slow would
  # invert the relationship while keeping the values plausible.
  fx <- tier_a_fixture("tier_a_default")
  par <- laser_params(fx$meta$config_list, components = LASER_PIPELINE)

  expect_true(all(par$delta_jt >= 1 / par$decay_days_long - 1e-12))
  expect_true(all(par$delta_jt <= 1 / par$decay_days_short + 1e-12))
  expect_true(all(is.finite(par$delta_jt)))
})

test_that("delta_jt decreases monotonically in suitability", {
  par <- list(psi_jt = matrix(c(0, 0.25, 0.5, 0.75, 1), ncol = 1),
              decay_days_short = 3, decay_days_long = 90,
              decay_shape_1 = 2, decay_shape_2 = 5)
  d <- laser_delta_jt(par)
  expect_true(all(diff(as.vector(d)) < 0))
  expect_equal(d[1, 1], 1 / 3)
  expect_equal(d[5, 1], 1 / 90)
})

test_that("beta_jt_env has each patch's time-mean equal to its baseline", {
  # The normalisation is beta_j0_env * (1 + (psi - psi_bar)/psi_bar) with
  # psi_bar the mean over TIME within a patch, so averaging the result back
  # over time must return beta_j0_env exactly. Taking the mean over PATCHES
  # instead would produce a plausible-looking matrix that fails this identity --
  # which is the whole point of asserting it.
  fx <- tier_a_fixture("tier_a_default")
  par <- laser_params(fx$meta$config_list, components = LASER_PIPELINE)

  expect_equal(colMeans(par$beta_jt_env), par$beta_j0_env, tolerance = 1e-10)
})

test_that("beta_jt_human seasonality uses 1-indexed time", {
  # The Python implementation writes np.arange(0, nticks) + 1 with the comment
  # "R is 1-indexed, so we start at 1". Using 0:(nticks-1) here would
  # phase-shift the entire seasonal envelope by one day -- a shift far too
  # small to notice in a plot and large enough to matter to a peak-timing
  # likelihood term.
  par <- list(nticks = 4L, npatches = 1L, p = 365,
              beta_j0_hum = 1, a_1_j = 1, b_1_j = 0, a_2_j = 0, b_2_j = 0)
  got <- laser_beta_jt_human(par)
  expect_equal(as.vector(got), 1 + cos(2 * pi * (1:4) / 365))
  # and explicitly NOT the 0-indexed form
  expect_false(isTRUE(all.equal(as.vector(got), 1 + cos(2 * pi * (0:3) / 365))))
})

test_that("a flat seasonality collapses to the baseline", {
  par <- list(nticks = 10L, npatches = 2L, p = 365,
              beta_j0_hum = c(0.5, 2), a_1_j = c(0, 0), b_1_j = c(0, 0),
              a_2_j = c(0, 0), b_2_j = c(0, 0))
  got <- laser_beta_jt_human(par)
  expect_identical(dim(got), c(10L, 2L))
  expect_true(all(got[, 1] == 0.5))
  expect_true(all(got[, 2] == 2))
})

# -----------------------------------------------------------------------------
# Haversine
# -----------------------------------------------------------------------------

test_that("the distance matrix is symmetric with a zero diagonal", {
  d <- laser_distance_matrix(c(0, 10, -33.9), c(0, 10, 18.4))
  expect_equal(d, t(d))
  expect_true(all(diag(d) == 0))
})

test_that("known great-circle distances are reproduced", {
  # A quarter of the way round the equator is pi/2 * 6371 km.
  expect_equal(laser_distance_matrix(c(0, 0), c(0, 90))[1, 2],
               pi / 2 * 6371, tolerance = 1e-9)
  # Pole to pole along a meridian is pi * 6371 km.
  expect_equal(laser_distance_matrix(c(-90, 90), c(0, 0))[1, 2],
               pi * 6371, tolerance = 1e-9)
  # One degree of latitude at the equator.
  expect_equal(laser_distance_matrix(c(0, 1), c(0, 0))[1, 2],
               pi / 180 * 6371, tolerance = 1e-9)
})

test_that("antipodal points do not produce NaN", {
  # The haversine `a` term reaches exactly 1 for antipodal points, and rounding
  # can push it a hair above -- asin() of which is NaN. Clamped in
  # laser_distance_matrix() precisely so a pathological config cannot silently
  # poison the whole gravity matrix.
  d <- laser_distance_matrix(c(0, 0), c(0, 180))
  expect_false(anyNA(d))
  expect_equal(d[1, 2], pi * 6371, tolerance = 1e-6)
})

test_that("out-of-range coordinates are rejected", {
  expect_error(laser_distance_matrix(c(0, 91), c(0, 0)), "\\[-90, 90\\]")
  expect_error(laser_distance_matrix(c(0, 0), c(0, 181)), "\\[-180, 180\\]")
  expect_error(laser_distance_matrix(c(0, 1), 0), "same length")
})

# -----------------------------------------------------------------------------
# Gravity model
# -----------------------------------------------------------------------------

test_that("gravity flow rises with destination population and falls with distance", {
  d <- rbind(c(0, 100, 100), c(100, 0, 100), c(100, 100, 0))
  pi_ij <- laser_pi_ij(c(1, 1, 10), d, omega = 1, gamma = 1)
  # From patch 1: patch 3 is 10x more populous at equal distance, so it takes
  # 10x the flow.
  expect_equal(pi_ij[1, 3] / pi_ij[1, 2], 10, tolerance = 1e-12)

  d2 <- rbind(c(0, 100, 200), c(100, 0, 100), c(200, 100, 0))
  pi2 <- laser_pi_ij(c(1, 1, 1), d2, omega = 1, gamma = 1)
  # equal populations, patch 3 twice as far -> half the flow
  expect_equal(pi2[1, 2] / pi2[1, 3], 2, tolerance = 1e-12)
})

test_that("tau_i is NOT folded into pi_ij", {
  # The migrating fraction is applied at runtime inside the HumanToHuman phase
  # so it can vary per patch without rebuilding pi_ij. Folding it in here would
  # double-count it, and the rows would no longer sum to 1.
  fx <- tier_a_fixture("tier_a_default")
  par <- laser_params(fx$meta$config_list, components = LASER_PIPELINE)
  expect_equal(rowSums(par$pi_ij), rep(1, par$npatches), tolerance = 1e-10)
  expect_false(any(abs(rowSums(par$pi_ij) - par$tau_i) < 1e-12 & par$tau_i < 0.99))
})

test_that("a mis-shaped distance matrix is rejected", {
  expect_error(laser_pi_ij(c(1, 2, 3), matrix(1, 2, 2), 1, 1), "expected 3 x 3")
})
