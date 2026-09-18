# `best_subset_weighting = "tempered"` was DOCUMENTED as the softer alternative
# and is in fact ~86x sharper: .mosaic_calc_adaptive_gibbs_weights() derives eta
# from max(delta) WITHIN the set it is handed. Across all draws max(delta) ~2.9e6
# gives a near-flat weighting; inside the ~115-member best subset it is ~4.2e3,
# so eta is ~680x larger. Measured on ETH 25k: ESS 107.48 -> 1.25, 96.08% of the
# mass on one draw. These tests pin the real behaviour so the description and the
# code cannot drift apart again.

test_that("adaptive eta depends on the RANGE of the set it is given", {
  set.seed(9)
  ll_wide   <- -c(0, cumsum(abs(rnorm(999, sd = 3000))))   # max delta ~ 1e6
  ll_narrow <- ll_wide[1:115]                              # a much tighter subset

  w_wide   <- MOSAIC:::.mosaic_calc_adaptive_gibbs_weights(ll_wide,   verbose = FALSE)
  w_narrow <- MOSAIC:::.mosaic_calc_adaptive_gibbs_weights(ll_narrow, verbose = FALSE)

  # Same scheme, same data, narrower SUBSET -> larger eta, i.e. sharper per unit
  # of delta. eta = 1/(2*temperature) in this helper, so temperature falls.
  expect_gt(diff(range(-2 * ll_wide)), diff(range(-2 * ll_narrow)))
  eta <- function(x) 1 / (2 * x$temperature)
  expect_gt(eta(w_narrow), eta(w_wide))
})

test_that("tempered is SHARPER than saturated on a realistic best subset", {
  set.seed(12)
  # A subset whose internal delta range is large but far below the all-draws range.
  ll <- -c(0, cumsum(abs(rnorm(114, sd = 18))))
  delta <- -2 * ll - min(-2 * ll)

  sat <- calc_model_weights_gibbs(pmin(delta, 4.0), eta = 0.5, verbose = FALSE)
  tmp <- MOSAIC:::.mosaic_calc_adaptive_gibbs_weights(ll, verbose = FALSE)$weights
  sat <- sat / sum(sat); tmp <- tmp / sum(tmp)

  # The documented claim was "softer". It is not.
  expect_gt(max(tmp), max(sat))
  expect_lt(calc_model_ess(tmp, method = "perplexity"),
            calc_model_ess(sat, method = "perplexity"))
})

test_that("saturated weighting is not sample-coherent", {
  # Two fixed draws' RELATIVE weight must not depend on what else is sampled.
  # Under pmin(delta, 4) it does, because the sample max sits inside a
  # non-linear function so the additive shift stops cancelling.
  ratio <- function(ll) {
    d <- -2 * ll; d <- d - min(d)
    w <- exp(-0.5 * pmin(d, 4))
    w[1] / w[2]
  }
  expect_equal(ratio(c(-100, -102)), exp(2), tolerance = 1e-8)
  # Add a BETTER third draw; draws 1 and 2 are untouched.
  expect_equal(ratio(c(-100, -102, -90)), 1, tolerance = 1e-8)
})
