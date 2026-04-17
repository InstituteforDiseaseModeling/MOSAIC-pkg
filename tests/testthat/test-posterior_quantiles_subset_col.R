# Regression test: ensure the new subset_col / weight_col arguments default to
# the legacy column names so pre-refactor callers see identical output.

make_mock_results_for_quantiles <- function(n = 40, seed = 42) {
  set.seed(seed)
  # Two estimated params with some variation
  df <- data.frame(
    sim           = seq_len(n),
    iter          = rep(1L, n),
    seed_sim      = seq.int(1000L, length.out = n),
    seed_iter     = rep(1L, n),
    likelihood    = rnorm(n, -100, 2),
    tau_i         = runif(n, 0.1, 1.0),
    gamma_1       = runif(n, 0.01, 0.1),
    is_finite     = rep(TRUE, n),
    is_retained   = rep(TRUE, n),
    is_best_subset = c(rep(TRUE, 10), rep(FALSE, n - 10)),
    weight_best   = c(runif(10, 0.05, 0.2), rep(0, n - 10))
  )
  df$weight_best[df$is_best_subset] <-
    df$weight_best[df$is_best_subset] / sum(df$weight_best[df$is_best_subset])
  df
}

test_that("calc_model_posterior_quantiles with default args matches explicit tier cols", {
  results <- make_mock_results_for_quantiles()

  out_default  <- tempfile("pq_default_");  dir.create(out_default)
  out_explicit <- tempfile("pq_explicit_"); dir.create(out_explicit)
  on.exit({ unlink(out_default, recursive = TRUE); unlink(out_explicit, recursive = TRUE) }, add = TRUE)

  q_default <- calc_model_posterior_quantiles(
    results    = results,
    output_dir = out_default,
    verbose    = FALSE
  )
  q_explicit <- calc_model_posterior_quantiles(
    results    = results,
    output_dir = out_explicit,
    subset_col = "is_best_subset",
    weight_col = "weight_best",
    verbose    = FALSE
  )

  # Same row count, same parameters, same quantiles
  expect_equal(nrow(q_default), nrow(q_explicit))
  numeric_cols <- sapply(q_default, is.numeric)
  expect_equal(q_default[, numeric_cols], q_explicit[, numeric_cols])
})

test_that("calc_model_posterior_quantiles with subset_col='is_best_subset_opt' differs from default", {
  results <- make_mock_results_for_quantiles()
  # Add a proper _opt subset (strict subset of is_best_subset) with renormalized weights
  results$is_best_subset_opt <- c(rep(TRUE, 4), rep(FALSE, nrow(results) - 4))
  results$weight_best_opt    <- 0
  opt_idx <- which(results$is_best_subset_opt)
  raw <- results$weight_best[opt_idx]
  results$weight_best_opt[opt_idx] <- raw / sum(raw)

  out_tier <- tempfile("pq_tier_"); dir.create(out_tier)
  out_opt  <- tempfile("pq_opt_");  dir.create(out_opt)
  on.exit({ unlink(out_tier, recursive = TRUE); unlink(out_opt, recursive = TRUE) }, add = TRUE)

  q_tier <- calc_model_posterior_quantiles(
    results = results, output_dir = out_tier, verbose = FALSE
  )
  q_opt <- calc_model_posterior_quantiles(
    results    = results,
    output_dir = out_opt,
    subset_col = "is_best_subset_opt",
    weight_col = "weight_best_opt",
    verbose    = FALSE
  )

  # Posterior rows differ (different subset selected)
  post_tier <- q_tier[q_tier$type == "posterior", ]
  post_opt  <- q_opt[q_opt$type == "posterior", ]
  expect_equal(nrow(post_tier), nrow(post_opt))

  # At least one parameter's posterior quantiles should differ
  q50_tier <- post_tier$q0.5
  q50_opt  <- post_opt$q0.5
  expect_false(isTRUE(all.equal(q50_tier, q50_opt, tolerance = 1e-10)))
})
