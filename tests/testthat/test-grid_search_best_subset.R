# Tests for grid_search_best_subset: bit-identical parity for the hoist-sort
# refactor (vs a golden fixture captured from the pre-refactor code) + baseline
# coverage (the function previously had no tests).

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
test_that("#3 grid_search_best_subset is bit-identical to the fixed reference", {
  fx <- readRDS(test_path("fixtures", "parity_grid_search.rds"))
  for (k in names(fx$cases)) {
    a   <- fx$cases[[k]]
    ref <- fx$out[[k]]
    new <- grid_search_best_subset(fx$results,
             target_ESS = a$target_ESS, target_A = a$target_A, target_CVw = a$target_CVw,
             min_size = a$min_size, max_size = a$max_size, step_size = a$step_size,
             ess_method = a$ess_method, verbose = FALSE)
    expect_identical(new$n,           ref$n,           info = k)
    expect_identical(new$converged,   ref$converged,   info = k)
    expect_identical(new$evaluations, ref$evaluations, info = k)
    expect_equal(new$metrics, ref$metrics, tolerance = 0, info = k)
    # Returned subset must be the same rows, order, columns AND row.names
    expect_equal(new$subset, ref$subset, tolerance = 0, info = k)
    expect_identical(rownames(new$subset), rownames(ref$subset), info = k)
    expect_identical(new$subset$sim,        ref$subset$sim,        info = k)
    expect_identical(new$subset$likelihood, ref$subset$likelihood, info = k)
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
