# =============================================================================
# Round-trip integration tests: prior → sample → quantiles → fit → verify
#
# These tests confirm that for each supported distribution family, the
# pipeline correctly recovers the distribution from its own samples.
# =============================================================================


# ---------------------------------------------------------------------------
# Helper: generate a synthetic calibration results data frame from a prior
# ---------------------------------------------------------------------------
make_synthetic_results <- function(param_name, prior_entry, n = 2000, n_best = 200, seed = 42) {
  set.seed(seed)
  samples <- sample_from_prior(n = n, prior = prior_entry)

  results <- data.frame(
    x = samples,
    is_finite = is.finite(samples),
    is_retained = TRUE,
    is_best_subset = c(rep(TRUE, n_best), rep(FALSE, n - n_best)),
    weight_best = c(rep(1/n_best, n_best), rep(0, n - n_best)),
    likelihood = rnorm(n, -100, 10)
  )
  names(results)[1] <- param_name
  results
}


# ---------------------------------------------------------------------------
# Helper: run the full pipeline for one parameter and return the fitted posterior
# ---------------------------------------------------------------------------
run_roundtrip <- function(param_name, prior_entry, scale = "global", iso = NULL) {

  results <- make_synthetic_results(param_name, prior_entry)

  # Build priors object
  priors <- list(
    metadata = list(version = "test"),
    parameters_global = list(),
    parameters_location = list()
  )
  if (scale == "global") {
    priors$parameters_global[[param_name]] <- prior_entry
  } else {
    base_name <- sub("_[A-Z]{3}$", "", param_name)
    priors$parameters_location[[base_name]] <- list(
      description = "test",
      location = setNames(list(prior_entry), iso)
    )
  }

  # Step 1: Posterior quantiles
  out_dir <- tempfile("rt_")
  dir.create(out_dir, recursive = TRUE)

  quantiles <- calc_model_posterior_quantiles(
    results = results,
    output_dir = out_dir,
    priors = priors,
    verbose = FALSE
  )

  # Step 2: Posterior distribution fitting
  priors_path <- file.path(out_dir, "priors.json")
  jsonlite::write_json(priors, priors_path, auto_unbox = TRUE, pretty = TRUE)

  calc_model_posterior_distributions(
    quantiles_file = file.path(out_dir, "posterior_quantiles.csv"),
    priors_file = priors_path,
    output_dir = out_dir,
    verbose = FALSE
  )

  # Read fitted posterior
  posteriors <- jsonlite::fromJSON(
    file.path(out_dir, "posteriors.json"),
    simplifyVector = FALSE
  )

  unlink(out_dir, recursive = TRUE)

  # Extract the fitted entry
  if (scale == "global") {
    posteriors$parameters_global[[param_name]]
  } else {
    posteriors$parameters_location[[base_name]]$location[[iso]]
  }
}


# ---------------------------------------------------------------------------
# 1. Beta round-trip
# ---------------------------------------------------------------------------
test_that("round-trip: beta prior recovers beta posterior", {
  prior <- list(distribution = "beta", parameters = list(shape1 = 5, shape2 = 12))
  fitted <- run_roundtrip("alpha_1", prior)

  expect_equal(fitted$distribution, "beta")
  expect_true(fitted$parameters$shape1 > 0)
  expect_true(fitted$parameters$shape2 > 0)

  # The fitted beta should be in the same ballpark as the prior
  # Prior mean = 5/(5+12) = 0.294
  fitted_mean <- fitted$parameters$shape1 / (fitted$parameters$shape1 + fitted$parameters$shape2)
  expect_true(abs(fitted_mean - 0.294) < 0.1,
              info = sprintf("Beta mean %.3f should be near 0.294", fitted_mean))

  # Verify round-trip: sample from fitted posterior
  post_samples <- sample_from_prior(n = 1000, prior = fitted)
  expect_true(all(is.finite(post_samples)))
  expect_true(all(post_samples > 0 & post_samples < 1))
})


# ---------------------------------------------------------------------------
# 2. Beta near zero (prop_E_initial edge case)
# ---------------------------------------------------------------------------
test_that("round-trip: beta prior near zero preserves small values", {
  # prop_E_initial: Beta(0.01, 9999.99) — mean ~0.0001%
  prior <- list(distribution = "beta", parameters = list(shape1 = 0.5, shape2 = 499.5))
  # Mean = 0.5/500 = 0.001
  fitted <- run_roundtrip("sigma", prior)  # use sigma (beta prior in inventory)

  expect_equal(fitted$distribution, "beta")
  expect_true(fitted$parameters$shape1 > 0)
  expect_true(fitted$parameters$shape2 > 0)

  # The fitted mean should be small (same order of magnitude)
  fitted_mean <- fitted$parameters$shape1 / (fitted$parameters$shape1 + fitted$parameters$shape2)
  expect_true(fitted_mean < 0.05,
              info = sprintf("Near-zero beta mean %.6f should stay small", fitted_mean))

  # Must be sampleable
  post_samples <- sample_from_prior(n = 1000, prior = fitted)
  expect_true(all(is.finite(post_samples)))
})


# ---------------------------------------------------------------------------
# 3. Gamma round-trip
# ---------------------------------------------------------------------------
test_that("round-trip: gamma prior recovers gamma posterior", {
  prior <- list(distribution = "gamma", parameters = list(shape = 4, rate = 2))
  fitted <- run_roundtrip("omega_1", prior)

  expect_equal(fitted$distribution, "gamma")
  expect_true(fitted$parameters$shape > 0)
  expect_true(fitted$parameters$rate > 0)

  # Prior mean = 4/2 = 2.0
  fitted_mean <- fitted$parameters$shape / fitted$parameters$rate
  expect_true(abs(fitted_mean - 2.0) < 1.0,
              info = sprintf("Gamma mean %.3f should be near 2.0", fitted_mean))

  post_samples <- sample_from_prior(n = 1000, prior = fitted)
  expect_true(all(is.finite(post_samples)))
  expect_true(all(post_samples > 0))
})


# ---------------------------------------------------------------------------
# 4. Lognormal round-trip
# ---------------------------------------------------------------------------
test_that("round-trip: lognormal prior recovers lognormal posterior", {
  prior <- list(distribution = "lognormal", parameters = list(meanlog = -10, sdlog = 1.2))
  fitted <- run_roundtrip("iota", prior)

  expect_equal(fitted$distribution, "lognormal")
  expect_true(is.finite(fitted$parameters$meanlog))
  expect_true(fitted$parameters$sdlog > 0)

  # Prior median = exp(-10) ≈ 4.5e-5
  fitted_median <- exp(fitted$parameters$meanlog)
  prior_median <- exp(-10)
  expect_true(fitted_median > prior_median * 0.1 && fitted_median < prior_median * 10,
              info = sprintf("Lognormal median %.2e should be within 10x of %.2e",
                             fitted_median, prior_median))

  post_samples <- sample_from_prior(n = 1000, prior = fitted)
  expect_true(all(is.finite(post_samples)))
  expect_true(all(post_samples > 0))
})


# ---------------------------------------------------------------------------
# 5. Normal round-trip
# ---------------------------------------------------------------------------
test_that("round-trip: normal prior recovers normal posterior", {
  prior <- list(distribution = "normal", parameters = list(mean = 0.5, sd = 0.3))
  fitted <- run_roundtrip("a_1_j_ETH", prior, scale = "location", iso = "ETH")

  expect_equal(fitted$distribution, "normal")
  expect_true(is.finite(fitted$parameters$mean))
  expect_true(fitted$parameters$sd > 0)

  expect_true(abs(fitted$parameters$mean - 0.5) < 0.2,
              info = sprintf("Normal mean %.3f should be near 0.5", fitted$parameters$mean))

  post_samples <- sample_from_prior(n = 1000, prior = fitted)
  expect_true(all(is.finite(post_samples)))
})


# ---------------------------------------------------------------------------
# 6. Truncnorm round-trip
# ---------------------------------------------------------------------------
test_that("round-trip: truncnorm prior recovers truncnorm posterior", {
  prior <- list(distribution = "truncnorm",
                parameters = list(mean = 16, sd = 7, a = 0.01, b = 29))
  fitted <- run_roundtrip("decay_days_short", prior)

  expect_equal(fitted$distribution, "truncnorm")
  expect_true(is.finite(fitted$parameters$mean))
  expect_true(fitted$parameters$sd > 0)

  post_samples <- sample_from_prior(n = 1000, prior = fitted)
  expect_true(all(is.finite(post_samples)))
  # Samples should respect bounds
  expect_true(all(post_samples >= fitted$parameters$a))
  expect_true(all(post_samples <= fitted$parameters$b))
})


# ---------------------------------------------------------------------------
# 7. Uniform → lognormal (domain inference for positive)
# ---------------------------------------------------------------------------
test_that("round-trip: uniform positive prior fits lognormal posterior", {
  prior <- list(distribution = "uniform", parameters = list(min = 30, max = 365))
  fitted <- run_roundtrip("decay_days_long", prior)

  # Uniform with min >= 0 should infer lognormal
  expect_equal(fitted$distribution, "lognormal")
  expect_true(is.finite(fitted$parameters$meanlog))
  expect_true(fitted$parameters$sdlog > 0)

  post_samples <- sample_from_prior(n = 1000, prior = fitted)
  expect_true(all(is.finite(post_samples)))
  expect_true(all(post_samples > 0))
})


# ---------------------------------------------------------------------------
# 8. Uniform → gamma (decay_shape via domain inference)
# ---------------------------------------------------------------------------
test_that("round-trip: uniform shape prior fits lognormal posterior", {
  prior <- list(distribution = "uniform", parameters = list(min = 0.1, max = 10))
  fitted <- run_roundtrip("decay_shape_1", prior)

  # Uniform with min >= 0 should infer lognormal via domain inference
  expect_equal(fitted$distribution, "lognormal")
  expect_true(fitted$parameters$sdlog > 0)

  post_samples <- sample_from_prior(n = 1000, prior = fitted)
  expect_true(all(is.finite(post_samples)))
  expect_true(all(post_samples > 0))
})


# ---------------------------------------------------------------------------
# 9. Location-specific lognormal round-trip (beta_j0_tot — the #64 fix)
# ---------------------------------------------------------------------------
test_that("round-trip: location-specific lognormal prior (beta_j0_tot issue #64)", {
  prior <- list(distribution = "lognormal",
                parameters = list(meanlog = -10.82, sdlog = 1.17))
  fitted <- run_roundtrip("beta_j0_tot_MOZ", prior, scale = "location", iso = "MOZ")

  # Critical: must be lognormal (not gompertz from old static lookup)
  expect_equal(fitted$distribution, "lognormal",
               info = "beta_j0_tot_MOZ must use lognormal (issue #64 fix)")
  expect_true(is.finite(fitted$parameters$meanlog))
  expect_true(fitted$parameters$sdlog > 0)

  post_samples <- sample_from_prior(n = 1000, prior = fitted)
  expect_true(all(is.finite(post_samples)))
  expect_true(all(post_samples > 0))
})


# ---------------------------------------------------------------------------
# 10. Update priors round-trip: posteriors replace priors correctly
# ---------------------------------------------------------------------------
test_that("round-trip: update_priors_from_posteriors merges correctly", {

  # Original priors
  priors <- list(
    metadata = list(version = "1.0", date = "2026-01-01", description = "test"),
    parameters_global = list(
      alpha_1 = list(distribution = "beta", parameters = list(shape1 = 2, shape2 = 5)),
      iota = list(distribution = "lognormal", parameters = list(meanlog = -0.3, sdlog = 0.4))
    ),
    parameters_location = list(
      beta_j0_tot = list(
        description = "Transmission rate",
        location = list(
          MOZ = list(distribution = "lognormal", parameters = list(meanlog = -10.8, sdlog = 1.2))
        )
      )
    )
  )

  # Simulated posteriors (as calc_model_posterior_distributions would produce)
  posteriors <- list(
    metadata = list(version = "1.0", date = "2026-01-02", description = "posterior"),
    parameters_global = list(
      alpha_1 = list(distribution = "beta", parameters = list(shape1 = 8, shape2 = 15)),
      iota = list(distribution = "lognormal", parameters = list(meanlog = -0.25, sdlog = 0.3))
    ),
    parameters_location = list(
      beta_j0_tot = list(
        description = "Transmission rate",
        location = list(
          MOZ = list(distribution = "lognormal", parameters = list(meanlog = -11.0, sdlog = 0.8))
        )
      )
    )
  )

  updated <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  # alpha_1: beta→beta, should be replaced
  expect_equal(updated$parameters_global$alpha_1$distribution, "beta")
  expect_equal(updated$parameters_global$alpha_1$parameters$shape1, 8)
  expect_equal(updated$parameters_global$alpha_1$parameters$shape2, 15)

  # iota: lognormal→lognormal, should be replaced
  expect_equal(updated$parameters_global$iota$distribution, "lognormal")
  expect_equal(updated$parameters_global$iota$parameters$meanlog, -0.25)

  # beta_j0_tot_MOZ: lognormal→lognormal, should be replaced (issue #64)
  expect_equal(updated$parameters_location$beta_j0_tot$location$MOZ$distribution, "lognormal")
  expect_equal(updated$parameters_location$beta_j0_tot$location$MOZ$parameters$meanlog, -11.0)
  expect_equal(updated$parameters_location$beta_j0_tot$location$MOZ$parameters$sdlog, 0.8)

  # All updated priors must be sampleable
  for (name in names(updated$parameters_global)) {
    entry <- updated$parameters_global[[name]]
    samples <- sample_from_prior(n = 10, prior = entry)
    expect_true(all(is.finite(samples)),
                info = sprintf("Global %s must be sampleable after update", name))
  }
  for (name in names(updated$parameters_location)) {
    for (iso in names(updated$parameters_location[[name]]$location)) {
      entry <- updated$parameters_location[[name]]$location[[iso]]
      samples <- sample_from_prior(n = 10, prior = entry)
      expect_true(all(is.finite(samples)),
                  info = sprintf("Location %s_%s must be sampleable after update", name, iso))
    }
  }
})


# ---------------------------------------------------------------------------
# 11. Inflate + sample round-trip
# ---------------------------------------------------------------------------
test_that("round-trip: inflate_priors produces sampleable priors for all families", {

  priors <- list(
    metadata = list(version = "1.0", date = "2026-01-01", description = "test"),
    parameters_global = list(
      test_beta = list(distribution = "beta", parameters = list(shape1 = 5, shape2 = 12)),
      test_gamma = list(distribution = "gamma", parameters = list(shape = 4, rate = 2)),
      test_lnorm = list(distribution = "lognormal", parameters = list(meanlog = -10, sdlog = 1)),
      test_norm = list(distribution = "normal", parameters = list(mean = 5, sd = 2)),
      test_tnorm = list(distribution = "truncnorm", parameters = list(mean = 16, sd = 7, a = 0.01, b = 29)),
      test_unif = list(distribution = "uniform", parameters = list(min = 30, max = 365))
    ),
    parameters_location = list()
  )

  inflated <- inflate_priors(priors, inflation_factor = 2.0, verbose = FALSE)

  for (name in names(inflated$parameters_global)) {
    entry <- inflated$parameters_global[[name]]
    expect_true(!is.null(entry$distribution),
                info = sprintf("%s should have distribution after inflation", name))

    samples <- sample_from_prior(n = 100, prior = entry)
    expect_true(all(is.finite(samples)),
                info = sprintf("%s (%s) must be sampleable after inflation",
                               name, entry$distribution))
  }
})
