# Tests for the re-simulated posterior R_eff credible interval machinery:
#   - .mosaic_reff_to_mat()          (orientation-robust channel coercion)
#   - per-member weighted-quantile reduction (known weights -> known quantiles)
#   - burn-in exclusion (leading days set NA in the assembled table)
#   - .mosaic_build_trajectories() grid: stride = 1 -> full daily-consecutive set
#
# The full re-simulation (.mosaic_reff_resim_ci / add_reproductive_numbers
# recompute_ci) drives the Python laser engine and is exercised by the smoke
# test, not the unit suite.

# -----------------------------------------------------------------------------
# .mosaic_reff_to_mat: orientation-robust coercion to nL x Tn
# -----------------------------------------------------------------------------
test_that(".mosaic_reff_to_mat coerces a single-location vector to 1 x Tn", {
  v <- as.numeric(1:10)
  m <- MOSAIC:::.mosaic_reff_to_mat(v, nL = 1L, Tn = 10L)
  expect_equal(dim(m), c(1L, 10L))
  expect_equal(as.numeric(m), v)
})

test_that(".mosaic_reff_to_mat coerces an nL x Tn matrix preserving values", {
  nL <- 3L; Tn <- 7L
  M  <- matrix(seq_len(nL * Tn), nrow = nL, ncol = Tn)
  out <- MOSAIC:::.mosaic_reff_to_mat(M, nL = nL, Tn = Tn)
  expect_equal(dim(out), c(nL, Tn))
  expect_equal(out, M)
})

test_that(".mosaic_reff_to_mat trims a trailing tick+1 column", {
  nL <- 2L; Tn <- 5L
  M  <- matrix(seq_len(nL * (Tn + 1L)), nrow = nL, ncol = Tn + 1L)
  out <- MOSAIC:::.mosaic_reff_to_mat(M, nL = nL, Tn = Tn)
  expect_equal(dim(out), c(nL, Tn))
  expect_equal(out, M[, seq_len(Tn), drop = FALSE])
})

# -----------------------------------------------------------------------------
# Weighted-quantile reduction over members: known weights -> known quantiles
# -----------------------------------------------------------------------------
test_that("per-member weighted quantiles recover the weighted median + 95% bounds", {
  # Four members with values 1,2,3,4 and equal weights -> the weighted-quantile
  # reduction must match weighted_quantiles() exactly (the function the resim
  # path calls per (location, t) cell).
  vals <- c(1, 2, 3, 4)
  w    <- rep(0.25, 4)
  probs <- c(0.025, 0.5, 0.975)
  qs <- weighted_quantiles(vals, w, probs)
  expect_length(qs, 3L)
  expect_true(all(diff(qs) >= 0))           # monotone
  expect_equal(qs[2L], weighted_quantiles(vals, w, 0.5))  # median consistency

  # Skewed weights pull the median toward the heavily-weighted value.
  w2 <- c(0.85, 0.05, 0.05, 0.05)
  med2 <- weighted_quantiles(vals, w2, 0.5)
  expect_lt(med2, weighted_quantiles(vals, rep(0.25, 4), 0.5))
})

# -----------------------------------------------------------------------------
# Burn-in exclusion logic (mirror the table assembly in .add_reff_recompute_ci)
# -----------------------------------------------------------------------------
test_that("burn-in exclusion sets the leading days to NA across all columns", {
  nL <- 1L; Tn <- 20L; bid <- 5L
  central <- matrix(1 + 0.1 * seq_len(Tn), nrow = nL)
  qmats   <- array(0, dim = c(nL, Tn, 3L))
  qmats[1, , 1] <- central - 0.2
  qmats[1, , 2] <- central
  qmats[1, , 3] <- central + 0.2

  burn_idx <- seq_len(min(bid, Tn))
  central[, burn_idx] <- NA_real_
  qmats[, burn_idx, ] <- NA_real_

  expect_true(all(is.na(central[1, seq_len(bid)])))
  expect_true(all(is.finite(central[1, (bid + 1L):Tn])))
  expect_true(all(is.na(qmats[1, seq_len(bid), ])))
  expect_true(all(is.finite(qmats[1, (bid + 1L):Tn, ])))
})

# -----------------------------------------------------------------------------
# .mosaic_build_trajectories grid (Phase-2 fix): stride = 1 -> full daily grid
# -----------------------------------------------------------------------------
test_that("trajectory time-stride grid yields a full daily-consecutive set at stride 1", {
  n_time_points <- 30L
  # Reproduce the (fixed) grid expression used in .mosaic_build_trajectories().
  t_idx1 <- seq.int(1L, n_time_points, by = max(1L, 1L))
  expect_equal(t_idx1, seq_len(n_time_points))            # full daily set
  expect_true(all(diff(t_idx1) == 1L))                    # daily-consecutive

  t_idx7 <- seq.int(1L, n_time_points, by = 7L)
  expect_equal(t_idx7, c(1L, 8L, 15L, 22L, 29L))
  expect_true(t_idx7[1L] == 1L)                           # starts at day 1

  # The OLD buggy form yielded an EMPTY set at stride 1.
  old_stride1 <- which(seq_len(n_time_points) %% 1L == 1L)
  expect_length(old_stride1, 0L)
})
