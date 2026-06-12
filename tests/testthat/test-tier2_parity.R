# Parity tests for the Tier-2 performance refactor (weighted_quantiles_presorted,
# merge-0.5, sort-once-per-cell) and the R-7 presort-memory refactor.
#
# Provenance & what each test actually guards (review B2-9):
#  * Tests #2a and #1 are INDEPENDENT correctness oracles: they recompute the
#    presorted weighted quantiles from scratch via the naive per-N
#    weighted_quantiles() and assert agreement (tolerance = 0). They validate the
#    sort-once / presorted math WITHOUT any stored snapshot, so correctness does
#    not rest on the fixture.
#  * Tests #1+#2b and #2b compare optimize_ensemble_subset() / calc_model_ensemble()
#    against the golden fixture (fixtures/parity_tier2.rds). The fixture was captured
#    via devtools::load_all() of the package (claude/capture_parity_tier2.R), so it
#    reflects the code AT CAPTURE TIME and cannot independently prove a pre-refactor
#    snapshot. It is therefore a REGRESSION guard against drift from a
#    continuously-verified baseline, not proof of the original bit-identical claim.
#  * That baseline is kept verified: the optimized-ensemble MEAN fields were
#    surgically refreshed with all other fields asserted bit-identical in v0.36.2
#    (R-5, claude/update_parity_tier2_means.R) when optimize_ensemble_subset() began
#    renormalizing the mean over surviving sims (a ~1e-14 divide-by-sum(w) change on
#    this no-failed-sim fixture; the NA behavior is covered by
#    test-optimize_ensemble_subset.R), and the optimize path was re-confirmed
#    bit-identical at tolerance = 0 after the v0.36.9 (R-7) presort-memory refactor.
#  * Cross-platform tolerance (v0.36.13): the #1+#2b and #2b fixture comparisons
#    use testthat::testthat_tolerance() (~1.5e-8), not tolerance = 0. The
#    fixture was baked on the author's local machine; the docker CI image
#    (idmmosaicacr.azurecr.io/mosaic-worker:latest, Linux x86_64 + OpenBLAS)
#    diverges by O(10^3) ULPs (~1e-13) — a length-N float-reduction noise floor
#    that depends on SIMD lane width and BLAS reduction order, not on code
#    correctness. The bit-identical guarantee is preserved by the INDEPENDENT
#    oracles (#2a and #1, which recompute the math from scratch with no fixture
#    and stay at tolerance = 0). Integer-typed fields (optimal_n, optimal_seeds)
#    also remain expect_identical.

test_that("#2a: weighted_quantiles_presorted == weighted_quantiles on sorted input", {
  set.seed(99)
  probs <- c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1)
  for (trial in seq_len(60)) {
    n <- sample(2:40, 1)
    x <- rnorm(n, 10, 5); w <- runif(n, 1e-6, 1)
    ord <- order(x)
    expect_equal(weighted_quantiles_presorted(x[ord], w[ord], probs),
                 weighted_quantiles(x, w, probs), tolerance = 0)
  }
  # edge cases
  expect_equal(weighted_quantiles_presorted(5, 1, c(0.25, 0.5)), c(5, 5))
  expect_true(all(is.na(weighted_quantiles_presorted(numeric(0), numeric(0), c(0.5)))))
  expect_equal(weighted_quantiles_presorted(c(3, 3, 3), c(.2, .3, .5), 0.5), 3)
})

test_that("#1: sort-once-per-cell reproduces naive per-N weighted medians (incl. NAs)", {
  # Directly compares the sort-once/mask/presorted transform against the naive
  # per-N weighted_quantiles (param-fastest values, `times` weights), including
  # scattered failed sims (NA) and several subset sizes.
  set.seed(11)
  max_n <- 8L; n_stoch <- 3L
  vals <- rnorm(max_n * n_stoch, 20, 6)
  vals[c(2L, 9L, 15L)] <- NA_real_                 # failed sims
  pid  <- rep(seq_len(max_n), times = n_stoch)     # param id, param-fastest
  fin  <- is.finite(vals); o <- order(vals[fin])
  xs <- vals[fin][o]; pids <- pid[fin][o]          # sort-once table
  probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
  for (n in 3:max_n) {
    w_n <- runif(n, 0.1, 1)
    # naive (fixed alignment): top-n param-fastest block + `times` weights
    vals_n  <- as.vector(matrix(vals, nrow = max_n)[1:n, ])
    naive   <- weighted_quantiles(vals_n, rep(w_n, times = n_stoch) / n_stoch, probs)
    # sort-once: mask to param<=n, weight each value by its param
    kc      <- pids <= n
    fast    <- weighted_quantiles_presorted(xs[kc], (w_n / n_stoch)[pids[kc]], probs)
    expect_equal(fast, naive, tolerance = 0, info = paste("n =", n))
  }
})

test_that("#1+#2b: optimize_ensemble_subset is bit-identical to the fixed reference", {
  fx <- readRDS(test_path("fixtures", "parity_tier2.rds"))
  for (o in c("mae", "r2_bias", "wis")) {
    new <- optimize_ensemble_subset(fx$ens, fx$lls, seeds = fx$seeds,
                                    min_n = 4L, objective = o, verbose = FALSE)
    ref <- fx$opt[[o]]
    expect_identical(new$optimal_n, ref$optimal_n, info = o)
    expect_identical(new$optimal_seeds, ref$optimal_seeds, info = o)
    expect_equal(new$optimal_weights, ref$optimal_weights, tolerance = testthat::testthat_tolerance(), info = o)
    expect_equal(new$evaluation_table, ref$evaluation_table, tolerance = testthat::testthat_tolerance(), info = o)
    eo <- new$ensemble_optimized; er <- ref$ensemble_optimized
    expect_equal(eo$cases_median,  er$cases_median,  tolerance = testthat::testthat_tolerance(), info = o)
    expect_equal(eo$deaths_median, er$deaths_median, tolerance = testthat::testthat_tolerance(), info = o)
    expect_equal(eo$cases_mean,    er$cases_mean,    tolerance = testthat::testthat_tolerance(), info = o)
    expect_equal(eo$deaths_mean,   er$deaths_mean,   tolerance = testthat::testthat_tolerance(), info = o)
    expect_equal(eo$ci_bounds,     er$ci_bounds,     tolerance = testthat::testthat_tolerance(), info = o)
  }
})

test_that("#2b: calc_model_ensemble is bit-identical to the fixed reference", {
  fx <- readRDS(test_path("fixtures", "parity_tier2.rds"))
  ci <- fx$ce_inputs
  new <- calc_model_ensemble(config = ci$config, configs = ci$configs,
            parameter_weights = ci$parameter_weights, n_simulations_per_config = ci$n_stoch,
            envelope_quantiles = ci$envelope, precomputed_results = ci$precomputed,
            verbose = FALSE)
  expect_equal(new$cases_median,  fx$ce$cases_median,  tolerance = testthat::testthat_tolerance())
  expect_equal(new$deaths_median, fx$ce$deaths_median, tolerance = testthat::testthat_tolerance())
  expect_equal(new$cases_mean,    fx$ce$cases_mean,    tolerance = testthat::testthat_tolerance())
  expect_equal(new$deaths_mean,   fx$ce$deaths_mean,   tolerance = testthat::testthat_tolerance())
  expect_equal(new$ci_bounds,     fx$ce$ci_bounds,     tolerance = testthat::testthat_tolerance())
})
