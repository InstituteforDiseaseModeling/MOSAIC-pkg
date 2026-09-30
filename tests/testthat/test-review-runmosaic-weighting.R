# =============================================================================
# test-review-runmosaic-weighting.R
#
# best_subset_weighting = "tempered" used to change only the gate metrics:
# results$weight_best (posterior quantiles, posteriors.json, ensemble weights,
# optimizer) was hard-coded to the saturated weights. Both now come from
# .mosaic_best_subset_weights().
# =============================================================================

test_that("saturated best-subset weights are the historical exp(-0.5 * min(delta, 4))", {
  ll <- c(-100, -101, -103, -120, -500)
  d  <- -2 * ll - min(-2 * ll)
  ref <- calc_model_weights_gibbs(pmin(d, 4), eta = 0.5, verbose = FALSE)
  got <- MOSAIC:::.mosaic_best_subset_weights(ll, "saturated")
  expect_identical(got$weights, ref)
  expect_identical(got$temperature, 0.5)
  expect_identical(got$effective_range, 4.0)
})

test_that("tempered best-subset weights follow the adaptive scheme and differ from saturated", {
  set.seed(12)
  ll <- -c(0, cumsum(abs(rnorm(114, sd = 18))))
  tmp <- MOSAIC:::.mosaic_best_subset_weights(ll, "tempered")
  ref <- MOSAIC:::.mosaic_calc_adaptive_gibbs_weights(ll, verbose = FALSE)
  expect_identical(tmp$weights, ref$weights)
  sat <- MOSAIC:::.mosaic_best_subset_weights(ll, "saturated")
  expect_false(isTRUE(all.equal(tmp$weights, sat$weights)))
})

test_that("an unknown scheme is rejected", {
  expect_error(MOSAIC:::.mosaic_best_subset_weights(c(-1, -2), "flat"), "saturated' or 'tempered")
})

test_that("run_MOSAIC derives weight_best and the gate weights from the same helper", {
  body_txt <- paste(deparse(body(MOSAIC::run_MOSAIC)), collapse = "\n")
  expect_identical(lengths(regmatches(body_txt, gregexpr(".mosaic_best_subset_weights(",
                                                          body_txt, fixed = TRUE))), 2L)
  expect_false(grepl("pmin(delta_aic_best, 4", body_txt, fixed = TRUE))
})
