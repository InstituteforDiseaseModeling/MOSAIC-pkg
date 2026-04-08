# =============================================================================
# Thorough tests for update_priors_from_posteriors() and inflate_priors()
# across all distribution families and edge cases
# =============================================================================


# ---------------------------------------------------------------------------
# Helper: create a full priors object with all 7 distribution families
# ---------------------------------------------------------------------------
make_all_family_priors <- function() {
  list(
    metadata = list(version = "1.0", date = "2026-01-01", description = "test priors"),
    parameters_global = list(
      p_beta      = list(distribution = "beta",      parameters = list(shape1 = 5, shape2 = 12)),
      p_gamma     = list(distribution = "gamma",     parameters = list(shape = 4, rate = 2)),
      p_lognormal = list(distribution = "lognormal", parameters = list(meanlog = -10, sdlog = 1.2)),
      p_normal    = list(distribution = "normal",    parameters = list(mean = 5, sd = 2)),
      p_truncnorm = list(distribution = "truncnorm", parameters = list(mean = 16, sd = 7, a = 0.01, b = 29)),
      p_uniform   = list(distribution = "uniform",   parameters = list(min = 30, max = 365)),
      p_gompertz  = list(distribution = "gompertz",  parameters = list(b = 50, eta = 50))
    ),
    parameters_location = list(
      loc_beta = list(
        description = "test location beta",
        location = list(
          ETH = list(distribution = "beta", parameters = list(shape1 = 3, shape2 = 7)),
          MOZ = list(distribution = "beta", parameters = list(shape1 = 4, shape2 = 6))
        )
      ),
      loc_lnorm = list(
        description = "test location lognormal",
        location = list(
          ETH = list(distribution = "lognormal", parameters = list(meanlog = -8, sdlog = 0.9)),
          MOZ = list(distribution = "lognormal", parameters = list(meanlog = -10, sdlog = 1.1))
        )
      )
    )
  )
}


# ---------------------------------------------------------------------------
# Helper: create matching posteriors with same families
# ---------------------------------------------------------------------------
make_matching_posteriors <- function() {
  list(
    metadata = list(version = "1.0", date = "2026-01-02", description = "posteriors"),
    parameters_global = list(
      p_beta      = list(distribution = "beta",      parameters = list(shape1 = 8, shape2 = 20)),
      p_gamma     = list(distribution = "gamma",     parameters = list(shape = 6, rate = 3)),
      p_lognormal = list(distribution = "lognormal", parameters = list(meanlog = -9.5, sdlog = 0.8)),
      p_normal    = list(distribution = "normal",    parameters = list(mean = 4.5, sd = 1.5)),
      p_truncnorm = list(distribution = "truncnorm", parameters = list(mean = 14, sd = 5, a = 0.01, b = 29)),
      p_uniform   = list(distribution = "lognormal", parameters = list(meanlog = 5.0, sdlog = 0.5)),
      p_gompertz  = list(distribution = "gompertz",  parameters = list(b = 80, eta = 80))
    ),
    parameters_location = list(
      loc_beta = list(
        description = "test location beta",
        location = list(
          ETH = list(distribution = "beta", parameters = list(shape1 = 5, shape2 = 10)),
          MOZ = list(distribution = "beta", parameters = list(shape1 = 6, shape2 = 9))
        )
      ),
      loc_lnorm = list(
        description = "test location lognormal",
        location = list(
          ETH = list(distribution = "lognormal", parameters = list(meanlog = -7.5, sdlog = 0.7)),
          MOZ = list(distribution = "lognormal", parameters = list(meanlog = -9.5, sdlog = 0.9))
        )
      )
    )
  )
}


# ===========================================================================
# UPDATE_PRIORS_FROM_POSTERIORS: comprehensive tests
# ===========================================================================

# ---------------------------------------------------------------------------
# 1. All matching families are replaced
# ---------------------------------------------------------------------------
test_that("update_priors: all 7 families replaced when families match", {
  priors <- make_all_family_priors()
  posteriors <- make_matching_posteriors()

  updated <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  # Global: all non-uniform matching families should be replaced
  expect_equal(updated$parameters_global$p_beta$parameters$shape1, 8)
  expect_equal(updated$parameters_global$p_gamma$parameters$shape, 6)
  expect_equal(updated$parameters_global$p_lognormal$parameters$meanlog, -9.5)
  expect_equal(updated$parameters_global$p_normal$parameters$mean, 4.5)
  expect_equal(updated$parameters_global$p_truncnorm$parameters$mean, 14)
  expect_equal(updated$parameters_global$p_gompertz$parameters$b, 80)

  # Uniform prior → lognormal posterior: should be accepted (uniform allows family change)
  expect_equal(updated$parameters_global$p_uniform$distribution, "lognormal")
  expect_equal(updated$parameters_global$p_uniform$parameters$meanlog, 5.0)

  # Location params: both countries replaced
  expect_equal(updated$parameters_location$loc_beta$location$ETH$parameters$shape1, 5)
  expect_equal(updated$parameters_location$loc_beta$location$MOZ$parameters$shape1, 6)
  expect_equal(updated$parameters_location$loc_lnorm$location$ETH$parameters$meanlog, -7.5)
  expect_equal(updated$parameters_location$loc_lnorm$location$MOZ$parameters$meanlog, -9.5)
})


# ---------------------------------------------------------------------------
# 2. All updated priors are sampleable
# ---------------------------------------------------------------------------
test_that("update_priors: all updated params produce finite samples", {
  priors <- make_all_family_priors()
  posteriors <- make_matching_posteriors()
  updated <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  for (name in names(updated$parameters_global)) {
    entry <- updated$parameters_global[[name]]
    samples <- sample_from_prior(n = 100, prior = entry)
    expect_true(all(is.finite(samples)),
                info = sprintf("Global %s (%s) must produce finite samples",
                               name, entry$distribution))
  }

  for (pname in names(updated$parameters_location)) {
    for (iso in names(updated$parameters_location[[pname]]$location)) {
      entry <- updated$parameters_location[[pname]]$location[[iso]]
      samples <- sample_from_prior(n = 100, prior = entry)
      expect_true(all(is.finite(samples)),
                  info = sprintf("Location %s_%s (%s) must produce finite samples",
                                 pname, iso, entry$distribution))
    }
  }
})


# ---------------------------------------------------------------------------
# 3. Family mismatch guard: rejects non-uniform family changes
# ---------------------------------------------------------------------------
test_that("update_priors: family mismatch guard keeps original prior", {
  priors <- make_all_family_priors()

  # Create posteriors with WRONG families for non-uniform priors
  posteriors <- list(
    metadata = list(version = "1.0", date = "2026-01-02", description = "posteriors"),
    parameters_global = list(
      # beta prior → gompertz posterior: should be REJECTED
      p_beta = list(distribution = "gompertz", parameters = list(b = 10, eta = 10)),
      # lognormal prior → gamma posterior: should be REJECTED
      p_lognormal = list(distribution = "gamma", parameters = list(shape = 2, rate = 100)),
      # gamma prior → normal posterior: should be REJECTED
      p_gamma = list(distribution = "normal", parameters = list(mean = 2, sd = 1))
    ),
    parameters_location = list()
  )

  updated <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  # All should keep ORIGINAL prior (family mismatch guard)
  expect_equal(updated$parameters_global$p_beta$distribution, "beta")
  expect_equal(updated$parameters_global$p_beta$parameters$shape1, 5,
               info = "Beta prior should be unchanged after family mismatch")

  expect_equal(updated$parameters_global$p_lognormal$distribution, "lognormal")
  expect_equal(updated$parameters_global$p_lognormal$parameters$meanlog, -10,
               info = "Lognormal prior should be unchanged after family mismatch")

  expect_equal(updated$parameters_global$p_gamma$distribution, "gamma")
  expect_equal(updated$parameters_global$p_gamma$parameters$shape, 4,
               info = "Gamma prior should be unchanged after family mismatch")
})


# ---------------------------------------------------------------------------
# 4. "failed" posteriors keep original prior
# ---------------------------------------------------------------------------
test_that("update_priors: failed posteriors preserve original prior", {
  priors <- make_all_family_priors()

  posteriors <- list(
    metadata = list(version = "1.0", date = "2026-01-02", description = "posteriors"),
    parameters_global = list(
      p_beta = list(distribution = "failed", parameters = list()),
      p_gamma = list(distribution = "failed", parameters = list())
    ),
    parameters_location = list()
  )

  updated <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  expect_equal(updated$parameters_global$p_beta$distribution, "beta")
  expect_equal(updated$parameters_global$p_beta$parameters$shape1, 5)
  expect_equal(updated$parameters_global$p_gamma$distribution, "gamma")
  expect_equal(updated$parameters_global$p_gamma$parameters$shape, 4)
})


# ---------------------------------------------------------------------------
# 5. "frozen" posteriors keep original prior
# ---------------------------------------------------------------------------
test_that("update_priors: frozen posteriors preserve original prior", {
  priors <- make_all_family_priors()

  posteriors <- list(
    metadata = list(version = "1.0", date = "2026-01-02", description = "posteriors"),
    parameters_global = list(
      p_normal = list(distribution = "frozen", parameters = list(value = 5.0)),
      p_truncnorm = list(distribution = "frozen", parameters = list(value = 16.0))
    ),
    parameters_location = list()
  )

  updated <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  expect_equal(updated$parameters_global$p_normal$distribution, "normal")
  expect_equal(updated$parameters_global$p_normal$parameters$mean, 5)
  expect_equal(updated$parameters_global$p_truncnorm$distribution, "truncnorm")
  expect_equal(updated$parameters_global$p_truncnorm$parameters$mean, 16)
})


# ---------------------------------------------------------------------------
# 6. Uniform prior accepts any posterior family
# ---------------------------------------------------------------------------
test_that("update_priors: uniform prior accepts different posterior family", {
  priors <- make_all_family_priors()

  posteriors <- list(
    metadata = list(version = "1.0", date = "2026-01-02", description = "posteriors"),
    parameters_global = list(
      p_uniform = list(distribution = "gamma", parameters = list(shape = 3, rate = 0.02))
    ),
    parameters_location = list()
  )

  updated <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  # Uniform → gamma should be accepted
  expect_equal(updated$parameters_global$p_uniform$distribution, "gamma")
  expect_equal(updated$parameters_global$p_uniform$parameters$shape, 3)

  # And it must be sampleable
  samples <- sample_from_prior(n = 100, prior = updated$parameters_global$p_uniform)
  expect_true(all(is.finite(samples)))
  expect_true(all(samples > 0))
})


# ---------------------------------------------------------------------------
# 7. Location-specific family mismatch
# ---------------------------------------------------------------------------
test_that("update_priors: location family mismatch keeps original", {
  priors <- make_all_family_priors()

  posteriors <- list(
    metadata = list(version = "1.0", date = "2026-01-02", description = "posteriors"),
    parameters_global = list(),
    parameters_location = list(
      loc_beta = list(
        description = "test",
        location = list(
          # beta → gompertz: should be REJECTED
          ETH = list(distribution = "gompertz", parameters = list(b = 10, eta = 10)),
          # beta → beta: should be ACCEPTED
          MOZ = list(distribution = "beta", parameters = list(shape1 = 10, shape2 = 15))
        )
      )
    )
  )

  updated <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  # ETH: mismatch, keeps original
  expect_equal(updated$parameters_location$loc_beta$location$ETH$distribution, "beta")
  expect_equal(updated$parameters_location$loc_beta$location$ETH$parameters$shape1, 3)

  # MOZ: match, replaced
  expect_equal(updated$parameters_location$loc_beta$location$MOZ$distribution, "beta")
  expect_equal(updated$parameters_location$loc_beta$location$MOZ$parameters$shape1, 10)
})


# ---------------------------------------------------------------------------
# 8. Posteriors have no extra diagnostic fields after cleaning
# ---------------------------------------------------------------------------
test_that("update_priors: cleaned posteriors have only canonical fields", {
  priors <- make_all_family_priors()

  # Posteriors with extra diagnostic fields (as fit_*_from_ci would return)
  posteriors <- list(
    metadata = list(version = "1.0", date = "2026-01-02", description = "posteriors"),
    parameters_global = list(
      p_beta = list(distribution = "beta",
                    parameters = list(shape1 = 8, shape2 = 20,
                                      fitted_mode = 0.3, fitted_ci = c(0.1, 0.5),
                                      input_mode = 0.25)),
      p_gamma = list(distribution = "gamma",
                     parameters = list(shape = 6, rate = 3, scale = 0.333,
                                       fitted_mean = 2.0))
    ),
    parameters_location = list()
  )

  updated <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  # Beta: only shape1, shape2
  expect_equal(sort(names(updated$parameters_global$p_beta$parameters)),
               c("shape1", "shape2"))

  # Gamma: only shape, rate (not scale or fitted_mean)
  expect_equal(sort(names(updated$parameters_global$p_gamma$parameters)),
               c("rate", "shape"))
})


# ===========================================================================
# INFLATE_PRIORS: comprehensive tests
# ===========================================================================

# ---------------------------------------------------------------------------
# 9. Inflation preserves distribution family for all types
# ---------------------------------------------------------------------------
test_that("inflate_priors: preserves distribution family for all types", {
  priors <- make_all_family_priors()
  inflated <- inflate_priors(priors, inflation_factor = 2.0, verbose = FALSE)

  for (name in names(inflated$parameters_global)) {
    orig_dist <- priors$parameters_global[[name]]$distribution
    new_dist  <- inflated$parameters_global[[name]]$distribution
    expect_equal(new_dist, orig_dist,
                 info = sprintf("%s: family should be preserved (%s)", name, orig_dist))
  }
})


# ---------------------------------------------------------------------------
# 10. Inflation increases variance for each family
# ---------------------------------------------------------------------------
test_that("inflate_priors: variance increases for all non-uniform families", {
  priors <- make_all_family_priors()
  inflated <- inflate_priors(priors, inflation_factor = 2.0, verbose = FALSE)

  set.seed(123)
  n <- 10000

  for (name in names(priors$parameters_global)) {
    orig <- priors$parameters_global[[name]]
    infl <- inflated$parameters_global[[name]]

    # Skip uniform (may be skipped by inflate_priors if bounds go negative)
    if (tolower(orig$distribution) == "uniform") next

    orig_samples <- sample_from_prior(n = n, prior = orig)
    infl_samples <- sample_from_prior(n = n, prior = infl)

    orig_var <- var(orig_samples)
    infl_var <- var(infl_samples)

    expect_true(infl_var > orig_var,
                info = sprintf("%s (%s): inflated var %.4g should exceed original %.4g",
                               name, orig$distribution, infl_var, orig_var))
  }
})


# ---------------------------------------------------------------------------
# 11. Inflation approximately preserves mean for each family
# ---------------------------------------------------------------------------
test_that("inflate_priors: mean approximately preserved for all families", {
  priors <- make_all_family_priors()
  inflated <- inflate_priors(priors, inflation_factor = 2.0, verbose = FALSE)

  set.seed(456)
  n <- 10000

  for (name in names(priors$parameters_global)) {
    orig <- priors$parameters_global[[name]]
    infl <- inflated$parameters_global[[name]]

    if (tolower(orig$distribution) == "uniform") next

    orig_samples <- sample_from_prior(n = n, prior = orig)
    infl_samples <- sample_from_prior(n = n, prior = infl)

    orig_mean <- mean(orig_samples)
    infl_mean <- mean(infl_samples)

    # Allow tolerance for mean shift. Gompertz mean is very sensitive to
    # shape changes (exponential tail) so gets a wider tolerance — the
    # geometric mean is preserved, but the arithmetic mean shifts.
    tol <- if (tolower(orig$distribution) == "gompertz") 2.0 else 0.20
    if (abs(orig_mean) > 1e-10) {
      rel_diff <- abs(infl_mean - orig_mean) / abs(orig_mean)
      expect_true(rel_diff < tol,
                  info = sprintf("%s (%s): mean shifted from %.4g to %.4g (%.1f%%)",
                                 name, orig$distribution, orig_mean, infl_mean, rel_diff * 100))
    }
  }
})


# ---------------------------------------------------------------------------
# 12. Inflated priors are all sampleable
# ---------------------------------------------------------------------------
test_that("inflate_priors: all inflated params produce finite samples", {
  priors <- make_all_family_priors()
  inflated <- inflate_priors(priors, inflation_factor = 2.0, verbose = FALSE)

  for (name in names(inflated$parameters_global)) {
    entry <- inflated$parameters_global[[name]]
    samples <- sample_from_prior(n = 100, prior = entry)
    expect_true(all(is.finite(samples)),
                info = sprintf("Global %s (%s) must produce finite samples after inflation",
                               name, entry$distribution))
  }
})


# ---------------------------------------------------------------------------
# 13. Gompertz inflation uses log-space (no negative samples)
# ---------------------------------------------------------------------------
test_that("inflate_priors: gompertz inflation produces valid positive samples", {
  priors <- list(
    metadata = list(version = "1.0", date = "2026-01-01", description = "test"),
    parameters_global = list(
      g_small = list(distribution = "gompertz", parameters = list(b = 200, eta = 200)),
      g_large = list(distribution = "gompertz", parameters = list(b = 10, eta = 10))
    ),
    parameters_location = list()
  )

  inflated <- inflate_priors(priors, inflation_factor = 3.0, verbose = FALSE)

  for (name in c("g_small", "g_large")) {
    entry <- inflated$parameters_global[[name]]
    expect_equal(entry$distribution, "gompertz")
    expect_true(entry$parameters$b > 0,
                info = sprintf("%s: b=%.4g should be positive", name, entry$parameters$b))
    expect_true(entry$parameters$eta > 0,
                info = sprintf("%s: eta=%.4g should be positive", name, entry$parameters$eta))

    samples <- sample_from_prior(n = 1000, prior = entry)
    expect_true(all(is.finite(samples)),
                info = sprintf("%s: all samples should be finite", name))
    expect_true(all(samples >= 0),
                info = sprintf("%s: all samples should be non-negative", name))
  }
})


# ---------------------------------------------------------------------------
# 14. Location params inflate correctly
# ---------------------------------------------------------------------------
test_that("inflate_priors: location params inflate and remain sampleable", {
  priors <- make_all_family_priors()
  inflated <- inflate_priors(priors, inflation_factor = 2.0, verbose = FALSE)

  for (pname in names(inflated$parameters_location)) {
    for (iso in names(inflated$parameters_location[[pname]]$location)) {
      orig <- priors$parameters_location[[pname]]$location[[iso]]
      infl <- inflated$parameters_location[[pname]]$location[[iso]]

      # Family preserved
      expect_equal(infl$distribution, orig$distribution,
                   info = sprintf("%s_%s: family should be preserved", pname, iso))

      # Sampleable
      samples <- sample_from_prior(n = 100, prior = infl)
      expect_true(all(is.finite(samples)),
                  info = sprintf("%s_%s: must be sampleable after inflation", pname, iso))

      # Variance increased
      set.seed(789)
      orig_var <- var(sample_from_prior(n = 5000, prior = orig))
      infl_var <- var(sample_from_prior(n = 5000, prior = infl))
      expect_true(infl_var > orig_var,
                  info = sprintf("%s_%s: inflated var should exceed original", pname, iso))
    }
  }
})


# ---------------------------------------------------------------------------
# 15. Deflation (factor < 1) reduces variance
# ---------------------------------------------------------------------------
test_that("inflate_priors: factor < 1 deflates variance", {
  priors <- list(
    metadata = list(version = "1.0", date = "2026-01-01", description = "test"),
    parameters_global = list(
      p = list(distribution = "normal", parameters = list(mean = 5, sd = 2))
    ),
    parameters_location = list()
  )

  deflated <- inflate_priors(priors, inflation_factor = 0.5, verbose = FALSE)

  expect_equal(deflated$parameters_global$p$distribution, "normal")
  # sd should be sqrt(0.5) * 2 ≈ 1.41
  expect_true(deflated$parameters_global$p$parameters$sd < 2.0)
  expect_true(deflated$parameters_global$p$parameters$sd > 1.0)
})


# ---------------------------------------------------------------------------
# 16. Full pipeline: update → inflate → sample
# ---------------------------------------------------------------------------
test_that("full pipeline: update priors → inflate → sample works for all families", {
  priors <- make_all_family_priors()
  posteriors <- make_matching_posteriors()

  # Step 1: Update
  updated <- update_priors_from_posteriors(priors, posteriors, verbose = FALSE)

  # Step 2: Inflate
  inflated <- inflate_priors(updated, inflation_factor = 1.5, verbose = FALSE)

  # Step 3: Sample from every inflated parameter
  for (name in names(inflated$parameters_global)) {
    entry <- inflated$parameters_global[[name]]
    samples <- sample_from_prior(n = 100, prior = entry)
    expect_true(all(is.finite(samples)),
                info = sprintf("Pipeline: global %s (%s) must be sampleable",
                               name, entry$distribution))
  }

  for (pname in names(inflated$parameters_location)) {
    for (iso in names(inflated$parameters_location[[pname]]$location)) {
      entry <- inflated$parameters_location[[pname]]$location[[iso]]
      samples <- sample_from_prior(n = 100, prior = entry)
      expect_true(all(is.finite(samples)),
                  info = sprintf("Pipeline: %s_%s (%s) must be sampleable",
                                 pname, iso, entry$distribution))
    }
  }
})
