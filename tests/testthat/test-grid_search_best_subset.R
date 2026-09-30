# Tests for grid_search_best_subset: agreement with an independent naive
# reference of the documented algorithm (the fixture now supplies inputs only)
# + baseline coverage.
#
# Cross-platform tolerance (v0.36.13): $metrics is compared at
# testthat::testthat_tolerance() (~1.5e-8) rather than tolerance = 0. The
# fixture (fixtures/parity_grid_search.rds) was baked on the author's local
# machine; a Linux x86_64 + OpenBLAS CI runner (measured on the since-retired
# docker worker image) re-derives ESS via sum(w^2) over hundreds of weights,
# and that accumulation's bit-result is build-dependent (SIMD lane width, BLAS
# reduction strategy). Measured drift in CI: ESS up to ~768 ULP (~1.7e-13);
# A and CVw <= 1 ULP. Everything that does NOT depend on a length-N float
# reduction is still asserted bit-identical: $n, $converged, $evaluations,
# rownames($subset), $subset$sim, $subset$likelihood (integer slices of the
# input frame). $subset itself stays at tolerance = 0 for the same reason —
# it is just re-ordered input rows, with no float arithmetic.

# --- helper: small valid results frame ---------------------------------------
mk_results <- function(n = 100, seed = 1) {
  set.seed(seed)
  data.frame(sim = seq_len(n),
             likelihood = sort(-100 - rexp(n, 0.4), decreasing = TRUE),
             extra = rnorm(n), stringsAsFactors = FALSE)
}

# ============================================================================
# Parity: hoist-sort refactor is bit-identical to the captured reference
# ============================================================================
# Independent reference: the naive full-frame algorithm (re-sort per n, weight
# the top n with the production saturated scheme w ~ exp(-0.5 * min(delta, 4))).
# Until the weighting fix the fixture's reference outputs were produced by the
# range-scaled weighting exp(-2 * delta / range(delta)), which differs from the
# weights run_MOSAIC() gates and uses; only the fixture's INPUTS are reused here.
ref_grid_search <- function(results, target_ESS, target_A, target_CVw,
                            min_size, max_size, step_size, ess_method) {
  max_size <- min(max_size, nrow(results))
  ranked <- results[order(results$likelihood, decreasing = TRUE), ]
  metrics_at <- function(n) {
    d <- -2 * ranked$likelihood[1:n]; d <- d - min(d)
    w <- exp(-0.5 * pmin(d, 4)); w <- w / sum(w)
    list(ESS = calc_model_ess(w, method = ess_method),
         A = calc_model_agreement_index(w * n)$A,
         CVw = calc_model_cvw(w * n))
  }
  k <- 0
  for (n in seq(min_size, max_size, by = step_size)) {
    k <- k + 1
    m <- metrics_at(n)
    if (m$ESS >= target_ESS && m$A >= target_A && m$CVw <= target_CVw)
      return(list(n = n, subset = ranked[1:n, ], metrics = m,
                  converged = TRUE, evaluations = k))
  }
  list(n = max_size, subset = ranked[1:max_size, ], metrics = metrics_at(max_size),
       converged = FALSE, evaluations = k)
}

test_that("#3 grid_search_best_subset matches the naive saturated-weight reference", {
  fx <- readRDS(test_path("fixtures", "parity_grid_search.rds"))
  for (k in names(fx$cases)) {
    a   <- fx$cases[[k]]
    ref <- ref_grid_search(fx$results, a$target_ESS, a$target_A, a$target_CVw,
                           a$min_size, a$max_size, a$step_size, a$ess_method)
    new <- grid_search_best_subset(fx$results,
             target_ESS = a$target_ESS, target_A = a$target_A, target_CVw = a$target_CVw,
             min_size = a$min_size, max_size = a$max_size, step_size = a$step_size,
             ess_method = a$ess_method, verbose = FALSE)
    expect_identical(new$n,           ref$n,           info = k)
    expect_identical(new$converged,   ref$converged,   info = k)
    expect_identical(new$evaluations, ref$evaluations, info = k)
    expect_equal(new$metrics, ref$metrics, tolerance = testthat::testthat_tolerance(), info = k)
    # Returned subset must be the same rows, order, columns AND row.names
    expect_equal(new$subset, ref$subset, tolerance = 0, info = k)
    expect_identical(rownames(new$subset), rownames(ref$subset), info = k)
  }
})

# ============================================================================
# Baseline coverage
# ============================================================================
test_that("grid_search_best_subset validates inputs", {
  r <- mk_results(50)
  expect_error(grid_search_best_subset(list(), 10, 0.5, 2, 5, 40), "data frame")
  expect_error(grid_search_best_subset(r[, "extra", drop = FALSE], 10, 0.5, 2, 5, 40),
               "sim, likelihood")
  expect_error(grid_search_best_subset(r[0, ], 10, 0.5, 2, 5, 40), "empty")
  expect_error(grid_search_best_subset(r, -1, 0.5, 2, 5, 40), "target_ESS")
  expect_error(grid_search_best_subset(r, 10, 1.5, 2, 5, 40), "target_A")
  expect_error(grid_search_best_subset(r, 10, 0.5, -2, 5, 40), "target_CVw")
  expect_error(grid_search_best_subset(r, 10, 0.5, 2, 50, 40), "max_size must be >= min_size")
})

test_that("grid_search_best_subset clamps max_size > nrow with a warning", {
  r <- mk_results(30)
  expect_warning(
    res <- grid_search_best_subset(r, target_ESS = 5, target_A = 0.4, target_CVw = 3,
                                   min_size = 5, max_size = 100, verbose = FALSE),
    "exceeds available simulations"
  )
  expect_true(res$n <= 30)
})

test_that("grid_search_best_subset returns the documented structure and stops early", {
  r <- mk_results(120)
  res <- grid_search_best_subset(r, target_ESS = 15, target_A = 0.4, target_CVw = 3,
                                 min_size = 10, max_size = 100, verbose = FALSE)
  expect_named(res, c("n", "subset", "metrics", "converged", "evaluations"))
  expect_true(is.data.frame(res$subset))
  expect_equal(nrow(res$subset), res$n)
  expect_named(res$metrics, c("ESS", "A", "CVw"))
  # subset is the top-n by likelihood (descending) — first row is the max
  expect_equal(res$subset$likelihood, sort(res$subset$likelihood, decreasing = TRUE))
  expect_equal(res$subset$likelihood[1], max(r$likelihood))
  if (res$converged) expect_true(res$metrics$ESS >= 15 && res$metrics$A >= 0.4 && res$metrics$CVw <= 3)
})
