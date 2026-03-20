# =============================================================================
# Tests for update_priors_from_posteriors()
# =============================================================================

# ---------------------------------------------------------------------------
# Helper: minimal valid priors object
# ---------------------------------------------------------------------------
make_test_priors <- function() {
  list(
    metadata = list(version = "1.0.0", date = "2026-01-01", description = "test priors"),
    parameters_global = list(
      alpha_1 = list(
        distribution = "beta",
        parameters = list(shape1 = 2, shape2 = 5)
      ),
      gamma_1 = list(
        distribution = "uniform",
        parameters = list(min = 0.05, max = 0.5)
      ),
      iota = list(
        distribution = "lognormal",
        parameters = list(meanlog = 0.5, sdlog = 0.3)
      ),
      kappa = list(
        distribution = "lognormal",
        parameters = list(meanlog = 10, sdlog = 1)
      )
    ),
    parameters_location = list(
      beta_j0_tot = list(
        description = "Transmission rate",
        location = list(
          ETH = list(distribution = "gompertz", parameters = list(b = 100, eta = 100)),
          MOZ = list(distribution = "gompertz", parameters = list(b = 50, eta = 50))
        )
      ),
      theta_j = list(
        description = "WASH coverage",
        location = list(
          ETH = list(distribution = "beta", parameters = list(shape1 = 3, shape2 = 7)),
          MOZ = list(distribution = "beta", parameters = list(shape1 = 4, shape2 = 6))
        )
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Helper: posteriors object (pruned, as calc_model_posterior_distributions produces)
# ---------------------------------------------------------------------------
make_test_posteriors <- function() {
  list(
    metadata = list(
      version = "1.0.0",
      date = "2026-01-02",
      description = "Posterior distributions fitted from calibration quantiles",
      source_quantiles = "posterior_quantiles.csv"
    ),
    parameters_global = list(
      alpha_1 = list(
        distribution = "beta",
        parameters = list(shape1 = 4.5, shape2 = 11.9)
      ),
      gamma_1 = list(
        distribution = "lognormal",
        parameters = list(meanlog = -1.2, sdlog = 0.4)
      )
      # iota and kappa absent = not sampled in this stage
    ),
    parameters_location = list(
      beta_j0_tot = list(
        location = list(
          ETH = list(distribution = "gompertz", parameters = list(b = 120, eta = 125)),
          MOZ = list(distribution = "gompertz", parameters = list(b = 60, eta = 65))
        )
      )
      # theta_j absent = not sampled in this stage
    )
  )
}

# =============================================================================
# Basic merge behaviour
# =============================================================================

test_that("posteriors replace corresponding priors", {
  priors <- make_test_priors()
  posteriors <- make_test_posteriors()

  result <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  # alpha_1 should be updated

  expect_equal(result$parameters_global$alpha_1$parameters$shape1, 4.5)
  expect_equal(result$parameters_global$alpha_1$parameters$shape2, 11.9)

  # gamma_1 should be updated (uniform → lognormal)
  expect_equal(result$parameters_global$gamma_1$distribution, "lognormal")
  expect_equal(result$parameters_global$gamma_1$parameters$meanlog, -1.2)

  # beta_j0_tot ETH should be updated
  expect_equal(result$parameters_location$beta_j0_tot$location$ETH$parameters$b, 120)
})

test_that("unsampled parameters keep original prior", {
  priors <- make_test_priors()
  posteriors <- make_test_posteriors()

  result <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  # iota not in posteriors → stays at original prior
  expect_equal(result$parameters_global$iota$distribution, "lognormal")
  expect_equal(result$parameters_global$iota$parameters$meanlog, 0.5)

  # theta_j not in posteriors → stays at original prior
  expect_equal(result$parameters_location$theta_j$location$ETH$parameters$shape1, 3)
  expect_equal(result$parameters_location$theta_j$location$MOZ$parameters$shape2, 6)
})

test_that("output has same structure as input priors", {
  priors <- make_test_priors()
  posteriors <- make_test_posteriors()

  result <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  # Same top-level slots
  expect_true(all(c("metadata", "parameters_global", "parameters_location") %in% names(result)))

  # Same global parameter names
  expect_equal(sort(names(result$parameters_global)),
               sort(names(priors$parameters_global)))

  # Same location parameter groups
  expect_equal(sort(names(result$parameters_location)),
               sort(names(priors$parameters_location)))

  # Same locations per group
  expect_equal(sort(names(result$parameters_location$beta_j0_tot$location)),
               sort(names(priors$parameters_location$beta_j0_tot$location)))
})


# =============================================================================
# Fixed posteriors
# =============================================================================

test_that("fixed posteriors are propagated", {
  priors <- make_test_priors()
  posteriors <- make_test_posteriors()

  # kappa was detected as near-zero variance → fixed
  posteriors$parameters_global$kappa <- list(
    distribution = "fixed",
    parameters = list(value = 1000000)
  )

  result <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  expect_equal(result$parameters_global$kappa$distribution, "fixed")
  expect_equal(result$parameters_global$kappa$parameters$value, 1000000)
})


# =============================================================================
# Failed posteriors
# =============================================================================

test_that("failed posteriors revert to original prior", {
  priors <- make_test_priors()
  posteriors <- make_test_posteriors()

  # alpha_1 fitting failed
  posteriors$parameters_global$alpha_1 <- list(
    distribution = "failed",
    parameters = list(),
    metadata = list(error = "Distribution fitting failed")
  )

  result <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  # alpha_1 should keep original prior, not the failed marker
  expect_equal(result$parameters_global$alpha_1$distribution, "beta")
  expect_equal(result$parameters_global$alpha_1$parameters$shape1, 2)
  expect_equal(result$parameters_global$alpha_1$parameters$shape2, 5)
})

test_that("failed location posteriors revert to original prior", {
  priors <- make_test_priors()
  posteriors <- make_test_posteriors()

  # ETH beta_j0_tot fitting failed
  posteriors$parameters_location$beta_j0_tot$location$ETH <- list(
    distribution = "failed",
    parameters = list()
  )

  result <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  # ETH should keep original prior
  expect_equal(result$parameters_location$beta_j0_tot$location$ETH$distribution, "gompertz")
  expect_equal(result$parameters_location$beta_j0_tot$location$ETH$parameters$b, 100)

  # MOZ should still be updated
  expect_equal(result$parameters_location$beta_j0_tot$location$MOZ$parameters$b, 60)
})


# =============================================================================
# Metadata stripping
# =============================================================================

test_that("fitted diagnostic metadata is stripped from output", {
  priors <- make_test_priors()
  posteriors <- make_test_posteriors()

  # Add fitted metadata like fit_beta_from_ci returns
  posteriors$parameters_global$alpha_1$parameters$fitted_mode <- 0.246
  posteriors$parameters_global$alpha_1$parameters$fitted_mean <- 0.277
  posteriors$parameters_global$alpha_1$parameters$fitted_var <- 0.011
  posteriors$parameters_global$alpha_1$parameters$fitted_sd <- 0.107
  posteriors$parameters_global$alpha_1$parameters$fitted_ci <- list(0.097, 0.508)
  posteriors$parameters_global$alpha_1$parameters$input_mode <- 0.246
  posteriors$parameters_global$alpha_1$parameters$input_ci <- list(0.079, 0.504)

  result <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  # Only canonical beta fields should remain
  param_names <- names(result$parameters_global$alpha_1$parameters)
  expect_equal(sort(param_names), c("shape1", "shape2"))
})


# =============================================================================
# Parameter entries all have distribution + parameters
# =============================================================================

test_that("every entry in output has distribution and parameters fields", {
  priors <- make_test_priors()
  posteriors <- make_test_posteriors()

  result <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  # Global
  for (param_name in names(result$parameters_global)) {
    entry <- result$parameters_global[[param_name]]
    expect_true(!is.null(entry$distribution),
                info = paste("Missing distribution:", param_name))
    expect_true(!is.null(entry$parameters),
                info = paste("Missing parameters:", param_name))
  }

  # Location
  for (param_base in names(result$parameters_location)) {
    locs <- result$parameters_location[[param_base]]$location
    for (iso in names(locs)) {
      entry <- locs[[iso]]
      expect_true(!is.null(entry$distribution),
                  info = paste("Missing distribution:", param_base, iso))
      expect_true(!is.null(entry$parameters),
                  info = paste("Missing parameters:", param_base, iso))
    }
  }
})


# =============================================================================
# Input validation
# =============================================================================

test_that("errors on invalid priors structure", {
  posteriors <- make_test_posteriors()

  expect_error(update_priors_from_posteriors("nonexistent.json", posteriors),
               "not found")
  expect_error(update_priors_from_posteriors(list(foo = 1), posteriors),
               "missing required top-level slots")
})

test_that("errors on empty posteriors", {
  priors <- make_test_priors()

  expect_error(
    update_priors_from_posteriors(priors, list(parameters_global = list(), parameters_location = list())),
    "no parameters"
  )
})


# =============================================================================
# File path convenience
# =============================================================================

test_that("accepts file paths and loads JSON", {
  priors <- make_test_priors()
  posteriors <- make_test_posteriors()

  tmp_dir <- tempdir()
  priors_path <- file.path(tmp_dir, "test_priors.json")
  post_path <- file.path(tmp_dir, "test_posteriors.json")

  writeLines(jsonlite::toJSON(priors, auto_unbox = TRUE, pretty = TRUE), priors_path)
  writeLines(jsonlite::toJSON(posteriors, auto_unbox = TRUE, pretty = TRUE), post_path)

  result <- update_priors_from_posteriors(priors_path, post_path, verbose = FALSE)

  expect_equal(result$parameters_global$alpha_1$parameters$shape1, 4.5)
  expect_equal(result$parameters_global$iota$parameters$meanlog, 0.5)

  unlink(c(priors_path, post_path))
})


# =============================================================================
# Metadata update
# =============================================================================

test_that("output metadata is updated", {
  priors <- make_test_priors()
  posteriors <- make_test_posteriors()

  result <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  expect_true(grepl("staged estimation", result$metadata$description, ignore.case = TRUE))
  expect_equal(result$metadata$date, as.character(Sys.Date()))
  expect_true(!is.null(result$metadata$source_posteriors))
})


# =============================================================================
# Posterior with extra location not in priors (should skip, not crash)
# =============================================================================

test_that("extra locations in posteriors are skipped gracefully", {
  priors <- make_test_priors()
  posteriors <- make_test_posteriors()

  # Add a location that doesn't exist in priors
  posteriors$parameters_location$beta_j0_tot$location$ZZZ <- list(
    distribution = "gompertz",
    parameters = list(b = 99, eta = 99)
  )

  result <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  # ZZZ should not appear in output (not in original priors)
  expect_null(result$parameters_location$beta_j0_tot$location$ZZZ)

  # Original locations should still be correct
  expect_equal(result$parameters_location$beta_j0_tot$location$ETH$parameters$b, 120)
})


# =============================================================================
# Round-trip: output is valid input for sample_from_prior
# =============================================================================

test_that("merged priors can be sampled by sample_from_prior", {
  priors <- make_test_priors()
  posteriors <- make_test_posteriors()

  # Add a fixed entry
  posteriors$parameters_global$kappa <- list(
    distribution = "fixed",
    parameters = list(value = 1000000)
  )

  result <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  # Sample from each global prior — should not error
  for (param_name in names(result$parameters_global)) {
    entry <- result$parameters_global[[param_name]]
    val <- sample_from_prior(n = 1, prior = entry)
    expect_true(is.numeric(val) && length(val) == 1,
                info = paste("sample_from_prior failed for:", param_name))
  }

  # Sample from each location prior
  for (param_base in names(result$parameters_location)) {
    locs <- result$parameters_location[[param_base]]$location
    for (iso in names(locs)) {
      entry <- locs[[iso]]
      val <- sample_from_prior(n = 1, prior = entry)
      expect_true(is.numeric(val) && length(val) == 1,
                  info = paste("sample_from_prior failed for:", param_base, iso))
    }
  }
})
