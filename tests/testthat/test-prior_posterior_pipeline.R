# =============================================================================
# Tests for issue #64 fixes: prior-aware posterior distribution fitting
# =============================================================================

# ---------------------------------------------------------------------------
# 1. Shared .mosaic_dist_core_fields()
# ---------------------------------------------------------------------------

test_that(".mosaic_dist_core_fields returns complete canonical field list", {
  fields <- MOSAIC:::.mosaic_dist_core_fields()
  expect_type(fields, "list")
  expect_true(all(c("beta", "gamma", "lognormal", "normal", "truncnorm",
                     "uniform", "gompertz", "fixed", "frozen") %in% names(fields)))
  expect_equal(fields$beta, c("shape1", "shape2"))
  expect_equal(fields$gamma, c("shape", "rate"))
  expect_equal(fields$lognormal, c("meanlog", "sdlog"))
  expect_equal(fields$truncnorm, c("mean", "sd", "a", "b"))
  expect_equal(fields$gompertz, c("b", "eta"))
  expect_equal(fields$fixed, c("value"))
  expect_equal(fields$frozen, c("value"))
})


# ---------------------------------------------------------------------------
# 2. calc_model_posterior_quantiles: prior-aware inventory building
# ---------------------------------------------------------------------------

test_that("calc_model_posterior_quantiles uses actual prior family when provided", {

  # Create a minimal results data frame with a few parameters
  set.seed(42)
  n <- 200
  results <- data.frame(
    alpha_1 = rbeta(n, 2, 5),
    beta_j0_tot_MOZ = rlnorm(n, -10, 1),
    is_finite = TRUE,
    is_retained = TRUE,
    is_best_subset = c(rep(TRUE, 50), rep(FALSE, n - 50)),
    weight_best = c(rep(1/50, 50), rep(0, n - 50)),
    likelihood = rnorm(n, -100, 10)
  )

  # Create priors where beta_j0_tot is lognormal (not gompertz as in static lookup)
  priors <- list(
    metadata = list(version = "test"),
    parameters_global = list(
      alpha_1 = list(distribution = "beta", parameters = list(shape1 = 2, shape2 = 5))
    ),
    parameters_location = list(
      beta_j0_tot = list(
        description = "Transmission rate",
        location = list(
          MOZ = list(distribution = "lognormal", parameters = list(meanlog = -10, sdlog = 1))
        )
      )
    )
  )

  out_dir <- tempfile("test_pq_")
  dir.create(out_dir, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  # With priors: should use actual family (lognormal for beta_j0_tot)
  result_with <- calc_model_posterior_quantiles(
    results = results,
    output_dir = out_dir,
    priors = priors,
    verbose = FALSE
  )

  # Check beta_j0_tot_MOZ uses lognormal (from priors), not gompertz (from static lookup)
  btm <- result_with[result_with$parameter == "beta_j0_tot_MOZ", ]
  expect_true(nrow(btm) > 0, info = "beta_j0_tot_MOZ should be in output")

  # The prior_distribution column should reflect actual prior family
  expect_equal(unique(btm$prior_distribution), "lognormal",
               info = "Prior family should come from priors object, not static lookup")

  # The posterior_distribution column should also be lognormal (non-uniform prior)
  expect_equal(unique(btm$posterior_distribution), "lognormal",
               info = "Posterior fitting family should match actual prior for non-uniform priors")
})


test_that("calc_model_posterior_quantiles preserves uniform→upgraded family", {

  # Exercises the uniform -> truncnorm domain inference path (v0.28.1). After
  # Commits 1-3, no default prior in the package is uniform, so we inject one
  # on decay_days_spread (an inventory parameter) via the priors argument. The
  # actual_family branch at calc_model_posterior_quantiles.R:336-355 reads the
  # passed priors, not the inventory default, so domain inference kicks in.

  set.seed(42)
  n <- 200
  results <- data.frame(
    decay_days_spread = runif(n, 30, 365),
    is_finite = TRUE,
    is_retained = TRUE,
    is_best_subset = c(rep(TRUE, 50), rep(FALSE, n - 50)),
    weight_best = c(rep(1/50, 50), rep(0, n - 50)),
    likelihood = rnorm(n, -100, 10)
  )

  # Prior is uniform — posterior family inferred from domain (not [0,1] → truncnorm)
  priors <- list(
    metadata = list(version = "test"),
    parameters_global = list(
      decay_days_spread = list(distribution = "uniform", parameters = list(min = 30, max = 365))
    ),
    parameters_location = list()
  )

  out_dir <- tempfile("test_pq_uniform_")
  dir.create(out_dir, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  result <- calc_model_posterior_quantiles(
    results = results,
    output_dir = out_dir,
    priors = priors,
    verbose = FALSE
  )

  dds <- result[result$parameter == "decay_days_spread", ]
  expect_true(nrow(dds) > 0)

  # Prior distribution should be "uniform" (the actual prior)
  expect_equal(unique(dds$prior_distribution), "uniform")

  # Posterior distribution should be truncnorm (v0.28.1: uniform → truncnorm
  # preserves the uniform's [min, max] support through staged posteriors).
  expect_equal(unique(dds$posterior_distribution), "truncnorm",
               info = "Uniform priors outside [0,1] should infer truncnorm posterior (v0.28.1)")
})


test_that("domain inference from uniform bounds: proportion → beta, anything else → truncnorm", {

  # Test the internal domain inference helper directly.
  # v0.28.1: The non-[0,1] branch returns "truncnorm" instead of lognormal/normal.
  # Truncnorm preserves the uniform's [min, max] support through the staged-
  # posterior family-match guard (see update_priors_from_posteriors.R).
  infer <- MOSAIC:::.infer_posterior_family_uniform

  # [0, 1] → beta (proportion domain — Beta honors [0,1] natively)
  expect_equal(
    infer(list(distribution = "uniform", parameters = list(min = 0, max = 1))),
    "beta"
  )
  expect_equal(
    infer(list(distribution = "uniform", parameters = list(min = 0.0, max = 0.5))),
    "beta"
  )

  # [0, inf) or [a, b] with a >= 0 and b > 1 → truncnorm (positive domain, not proportion)
  expect_equal(
    infer(list(distribution = "uniform", parameters = list(min = 30, max = 365))),
    "truncnorm"
  )
  expect_equal(
    infer(list(distribution = "uniform", parameters = list(min = 0.1, max = 10))),
    "truncnorm"
  )
  expect_equal(
    infer(list(distribution = "uniform", parameters = list(min = 0, max = 100))),
    "truncnorm"
  )

  # min < 0 → truncnorm (unconstrained domain; bounds still preserved via truncation)
  expect_equal(
    infer(list(distribution = "uniform", parameters = list(min = -10, max = 10))),
    "truncnorm"
  )
  expect_equal(
    infer(list(distribution = "uniform", parameters = list(min = -1, max = 1))),
    "truncnorm"
  )
})


test_that("uniform prior decay_shape_1 gets truncnorm posterior via domain inference (v0.28.1)", {

  set.seed(42)
  n <- 200
  results <- data.frame(
    decay_shape_1 = runif(n, 0.1, 10),
    is_finite = TRUE,
    is_retained = TRUE,
    is_best_subset = c(rep(TRUE, 50), rep(FALSE, n - 50)),
    weight_best = c(rep(1/50, 50), rep(0, n - 50)),
    likelihood = rnorm(n, -100, 10)
  )

  # Actual prior: Uniform(0.1, 10) → domain inference: non-[0,1] → truncnorm
  priors <- list(
    metadata = list(version = "test"),
    parameters_global = list(
      decay_shape_1 = list(distribution = "uniform", parameters = list(min = 0.1, max = 10))
    ),
    parameters_location = list()
  )

  out_dir <- tempfile("test_pq_shape_")
  dir.create(out_dir, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  result <- calc_model_posterior_quantiles(
    results = results,
    output_dir = out_dir,
    priors = priors,
    verbose = FALSE
  )

  ds1 <- result[result$parameter == "decay_shape_1", ]
  expect_true(nrow(ds1) > 0)
  expect_equal(unique(ds1$prior_distribution), "uniform")
  expect_equal(unique(ds1$posterior_distribution), "truncnorm",
               info = "Uniform(0.1, 10) should infer truncnorm preserving [0.1, 10] support (v0.28.1)")
})


test_that("uniform prior end-to-end: truncnorm posterior preserves original bounds (v0.28.1)", {

  # Backstop regression test. Confirms the full pipeline (quantiles → fit →
  # sample from fitted) preserves the uniform prior's support. Before v0.28.1
  # a uniform prior was fit as unbounded lognormal/normal at stage 2+, so
  # stage-2 samples could drift past the original min/max.

  set.seed(42)
  n <- 2000
  results <- data.frame(
    decay_shape_1 = runif(n, 0.1, 10),
    is_finite = TRUE,
    is_retained = TRUE,
    is_best_subset = c(rep(TRUE, 500), rep(FALSE, n - 500)),
    weight_best = c(rep(1/500, 500), rep(0, n - 500)),
    likelihood = rnorm(n, -100, 10)
  )

  priors <- list(
    metadata = list(version = "test"),
    parameters_global = list(
      decay_shape_1 = list(distribution = "uniform", parameters = list(min = 0.1, max = 10))
    ),
    parameters_location = list()
  )

  out_dir <- tempfile("test_backstop_")
  dir.create(out_dir, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  # Stage 1: compute quantiles
  calc_model_posterior_quantiles(
    results = results,
    output_dir = out_dir,
    priors = priors,
    verbose = FALSE
  )

  # Stage 2: fit posterior distribution (should read min/max as truncnorm a/b)
  priors_path <- file.path(out_dir, "priors.json")
  jsonlite::write_json(priors, priors_path, auto_unbox = TRUE, pretty = TRUE, digits = NA)

  calc_model_posterior_distributions(
    quantiles_file = file.path(out_dir, "posterior_quantiles.csv"),
    priors_file = priors_path,
    output_dir = out_dir,
    verbose = FALSE
  )

  fitted <- jsonlite::fromJSON(
    file.path(out_dir, "posteriors.json"),
    simplifyVector = FALSE
  )$parameters_global$decay_shape_1

  # Fitted posterior should be truncnorm with bounds inherited from uniform prior
  expect_equal(fitted$distribution, "truncnorm")
  expect_equal(as.numeric(fitted$parameters$a), 0.1)
  expect_equal(as.numeric(fitted$parameters$b), 10)

  # Sample from the fitted posterior: every sample must respect the bounds
  post_samples <- sample_from_prior(n = 5000, prior = fitted)
  expect_true(all(is.finite(post_samples)))
  expect_true(all(post_samples >= 0.1))
  expect_true(all(post_samples <= 10))
})


test_that("calc_model_posterior_quantiles falls back to static lookup without priors", {

  set.seed(42)
  n <- 200
  results <- data.frame(
    alpha_1 = rbeta(n, 2, 5),
    is_finite = TRUE,
    is_retained = TRUE,
    is_best_subset = c(rep(TRUE, 50), rep(FALSE, n - 50)),
    weight_best = c(rep(1/50, 50), rep(0, n - 50)),
    likelihood = rnorm(n, -100, 10)
  )

  out_dir <- tempfile("test_pq_fallback_")
  dir.create(out_dir, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  # Without priors (NULL): should use static lookup as before
  result <- calc_model_posterior_quantiles(
    results = results,
    output_dir = out_dir,
    priors = NULL,
    verbose = FALSE
  )

  a1 <- result[result$parameter == "alpha_1", ]
  expect_true(nrow(a1) > 0)
  # Should still work and produce valid output
  expect_true("prior_distribution" %in% names(result))
  expect_true("posterior_distribution" %in% names(result))
})


# ---------------------------------------------------------------------------
# 3. inflate_priors: Gompertz log-space inflation
# ---------------------------------------------------------------------------

test_that("inflate_priors handles Gompertz near zero without negative samples", {

  # Create a Gompertz prior concentrated near zero
  priors <- list(
    metadata = list(version = "test", date = "2026-01-01", description = "test"),
    parameters_global = list(
      test_param = list(
        distribution = "gompertz",
        parameters = list(b = 100, eta = 100)
      )
    ),
    parameters_location = list()
  )

  # Inflate by factor 2 — previously caused negative samples with pmax clipping
  result <- inflate_priors(priors, inflation_factor = 2.0, verbose = FALSE)

  expect_equal(result$parameters_global$test_param$distribution, "gompertz")
  # Should still have valid gompertz parameters
  expect_true(result$parameters_global$test_param$parameters$b > 0)
  expect_true(result$parameters_global$test_param$parameters$eta > 0)
})


test_that("inflate_priors log-space inflation preserves positivity for gamma", {

  priors <- list(
    metadata = list(version = "test", date = "2026-01-01", description = "test"),
    parameters_global = list(),
    parameters_location = list()
  )

  # When empirical fallback is used for gamma, should inflate in log-space
  # (Gamma has analytic path, but test that the empirical path handles it)
  # We can't directly force empirical, but we can verify the analytic result
  priors$parameters_global$test_gamma <- list(
    distribution = "gamma",
    parameters = list(shape = 2, rate = 1000)  # concentrated near zero
  )

  result <- inflate_priors(priors, inflation_factor = 2.0, verbose = FALSE)
  expect_true(result$parameters_global$test_gamma$parameters$shape > 0)
  expect_true(result$parameters_global$test_gamma$parameters$rate > 0)
})


# ---------------------------------------------------------------------------
# 4. End-to-end: posterior fitting uses correct family
# ---------------------------------------------------------------------------

test_that("calc_model_posterior_distributions respects prior family from quantiles CSV", {

  # Create a posterior_quantiles.csv where posterior_distribution = "lognormal"
  # (as it would be after the fix, for a lognormal prior on beta_j0_tot)
  quantiles_df <- data.frame(
    parameter = c("alpha_1", "alpha_1"),
    display_name = c("Population Mixing", "Population Mixing"),
    category = c("transmission", "transmission"),
    param_type = c("global", "global"),
    location = c("", ""),
    prior_distribution = c("beta", "beta"),
    posterior_distribution = c("beta", "beta"),
    posterior_lower = c(NA, NA),
    posterior_upper = c(NA, NA),
    type = c("prior", "posterior"),
    mean = c(0.3, 0.28),
    sd = c(0.1, 0.08),
    mode = c(0.25, 0.24),
    kl = c(NA, 0.5),
    q0.025 = c(0.12, 0.14),
    q0.25 = c(0.22, 0.21),
    q0.5 = c(0.30, 0.27),
    q0.75 = c(0.38, 0.34),
    q0.975 = c(0.50, 0.45),
    stringsAsFactors = FALSE
  )

  out_dir <- tempfile("test_pd_")
  dir.create(out_dir, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  csv_path <- file.path(out_dir, "posterior_quantiles.csv")
  write.csv(quantiles_df, csv_path, row.names = FALSE)

  # Minimal priors template
  priors <- list(
    metadata = list(version = "test"),
    parameters_global = list(
      alpha_1 = list(distribution = "beta", parameters = list(shape1 = 2, shape2 = 5))
    ),
    parameters_location = list()
  )

  priors_path <- file.path(out_dir, "priors.json")
  jsonlite::write_json(priors, priors_path, auto_unbox = TRUE, pretty = TRUE, digits = NA)

  # Run posterior distribution fitting
  calc_model_posterior_distributions(
    quantiles_file = csv_path,
    priors_file = priors_path,
    output_dir = out_dir,
    verbose = FALSE
  )

  # Read output
  posteriors_path <- file.path(out_dir, "posteriors.json")
  expect_true(file.exists(posteriors_path))

  posteriors <- jsonlite::fromJSON(posteriors_path, simplifyVector = FALSE)
  expect_equal(posteriors$parameters_global$alpha_1$distribution, "beta")
  expect_true(!is.null(posteriors$parameters_global$alpha_1$parameters$shape1))
  expect_true(!is.null(posteriors$parameters_global$alpha_1$parameters$shape2))
})
