# =============================================================================
# test-cfr-pipeline-consistency.R
#
# Guards the invariant that config_default and priors_default agree per country
# on the CFR -> mu_j_baseline pipeline. The two data objects are built by
# independent scripts (data-raw/make_config_default.R and
# data-raw/make_priors_default.R) and silent drift between them is the bug class
# that caused 38 of 40 countries to ship raw-CFR values as daily hazards before
# v0.32.1 (see NEWS / CLAUDE.md Lesson #12).
#
# B2.1 (priors_default v15.15 / config_default v4.6): the per-country mu_j_baseline
# GAMMA location prior was REPLACED by a per-country CFR_target LOGNORMAL location
# prior. mu_j_baseline is now DERIVED at sample time by sample_parameters() with the
# ENGINE-CORRECT chain factor (v0.50.0 B2.1; statistician memory
# b2-cfr-chain-factor-diagnosis):
#   mu_j_baseline = CFR_target * (1 - exp(-gamma_1)) * rho / (rho_deaths * chi_epidemic)
# config_default ships both a numeric CFR_target (the prior median) and a numeric
# mu_j_baseline built as CFR_target * the STATIC chain anchor
#   cfr_to_mu_adjustment = (1 - exp(-gamma_1_anchor)) * rho_mean / (rho_deaths_mean * chi_epidemic_anchor)
#   (gamma_1_anchor = 0.10, chi_epidemic_anchor = 0.75)
# so the two artifacts must agree by construction:
#   config_default$CFR_target[iso]    == exp(meanlog) of the priors CFR_target lognormal
#   config_default$mu_j_baseline[iso] == config_default$CFR_target[iso] * cfr_to_mu_adjustment
# A mismatch means one of the .rda files has gone stale.
# =============================================================================

library(testthat)

test_that("config_default$CFR_target matches priors_default CFR_target lognormal median per country", {
  skip_if_not_installed("MOSAIC")
  cfg <- tryCatch(MOSAIC::config_default, error = function(e) NULL)
  pri <- tryCatch(MOSAIC::priors_default, error = function(e) NULL)
  if (is.null(cfg) || is.null(pri)) skip("config_default or priors_default not loadable")
  if (is.null(cfg$location_name) || is.null(cfg$CFR_target)) {
    skip("config_default missing location_name or CFR_target (pre-B2 object?)")
  }
  if (is.null(pri$parameters_location$CFR_target$location)) {
    skip("priors_default missing per-location CFR_target (pre-B2 object?)")
  }

  tol <- 1e-6  # exact equality expected since config sources from prior
  for (i in seq_along(cfg$location_name)) {
    iso <- cfg$location_name[i]
    prior_loc <- pri$parameters_location$CFR_target$location[[iso]]
    if (is.null(prior_loc)) {
      fail(sprintf("priors_default has no CFR_target for iso '%s'", iso))
      next
    }
    expect_equal(prior_loc$distribution, "lognormal",
                 info = sprintf("iso %s: expected lognormal CFR_target prior", iso))
    prior_median <- exp(prior_loc$parameters$meanlog)
    expect_equal(cfg$CFR_target[i], unname(prior_median), tolerance = tol,
                 info = sprintf("iso %s: config CFR_target=%.8f vs prior median=%.8f",
                                iso, cfg$CFR_target[i], prior_median))
  }
})

test_that("config_default$mu_j_baseline == CFR_target * static chain anchor (B2.1)", {
  skip_if_not_installed("MOSAIC")
  cfg <- tryCatch(MOSAIC::config_default, error = function(e) NULL)
  pri <- tryCatch(MOSAIC::priors_default, error = function(e) NULL)
  if (is.null(cfg) || is.null(pri)) skip("config_default or priors_default not loadable")
  if (is.null(cfg$CFR_target) || is.null(cfg$mu_j_baseline)) {
    skip("config_default missing CFR_target or mu_j_baseline (pre-B2 object?)")
  }

  # Recompute the B2.1 static chain anchor from the SHIPPED rho/rho_deaths Beta
  # priors (the same formula make_config_default.R uses): dwell = (1-exp(-0.10)),
  # chi_epidemic = 0.75.
  beta_mean <- function(p) p$shape1 / (p$shape1 + p$shape2)
  rho_mean        <- beta_mean(pri$parameters_global$rho$parameters)
  rho_deaths_mean <- beta_mean(pri$parameters_global$rho_deaths$parameters)
  adj <- (1 - exp(-0.10)) * rho_mean / (rho_deaths_mean * 0.75)

  expected_mu <- cfg$CFR_target * adj
  expect_equal(unname(cfg$mu_j_baseline), unname(expected_mu), tolerance = 1e-8,
               info = "config_default mu_j_baseline must equal CFR_target * cfr_to_mu_adjustment")

  # And the [0,1] engine bound (make_LASER_config L686) must hold for the defaults.
  expect_true(all(cfg$mu_j_baseline >= 0 & cfg$mu_j_baseline <= 1),
              info = "config_default mu_j_baseline must lie in [0, 1]")
})

test_that("priors_default CFR_target derivation describes the B2 chain-coupled identity", {
  skip_if_not_installed("MOSAIC")
  pri <- tryCatch(MOSAIC::priors_default, error = function(e) NULL)
  if (is.null(pri)) skip("priors_default not loadable")
  if (is.null(pri$parameters_location$CFR_target)) skip("priors_default missing CFR_target (pre-B2 object?)")

  # The description should mention the v0.13+/B2 identity components.
  desc <- pri$parameters_location$CFR_target$description
  expect_true(
    grepl("rho_deaths", desc, fixed = TRUE),
    info = "CFR_target description should mention rho_deaths (chain factor)"
  )
  expect_true(
    grepl("gamma_1", desc, fixed = TRUE),
    info = "CFR_target description should mention gamma_1 (B2 coupling)"
  )

  # The legacy mu_j_baseline Gamma LOCATION PRIOR must be GONE under B2 (it is
  # now derived at sample time, not a sampled prior).
  expect_null(pri$parameters_location$mu_j_baseline,
              info = "B2: mu_j_baseline location prior must be removed (replaced by CFR_target)")
})

test_that("rho_deaths uses the informative Beta(36.95, 51.02) prior", {
  skip_if_not_installed("MOSAIC")
  pri <- tryCatch(MOSAIC::priors_default, error = function(e) NULL)
  if (is.null(pri)) skip("priors_default not loadable")

  # v15.7+: production uses the narrow informative variant (pooled-mean CI fit)
  # to pin rho_deaths near 0.42 during sampling so mu_j_baseline posteriors
  # carry the cross-country CFR signal cleanly. The wider prediction-interval
  # variant Beta(6.30, 8.52) is retained for sensitivity (SYNTHESIS_REPORT 3.4).
  rd <- pri$parameters_global$rho_deaths
  expect_equal(rd$distribution, "beta")
  expect_equal(rd$parameters$shape1, 36.95, tolerance = 1e-6)
  expect_equal(rd$parameters$shape2, 51.02, tolerance = 1e-6)
})

test_that("config_default mu_j_baseline implies plausible per-episode CFR for high-N countries", {
  skip_if_not_installed("MOSAIC")
  cfg <- tryCatch(MOSAIC::config_default, error = function(e) NULL)
  if (is.null(cfg)) skip("config_default not loadable")

  # Per-episode CFR ~ 1 - exp(-mu_baseline / gamma_1), evaluated at config_default's
  # ACTUAL gamma_1 (config_default uses 0.1). v0.14.0+: the gamma_1-factor CFR->mu
  # identity lowers mu (hence per-episode CFR). High-N stable countries should be in
  # [0.5%, 15%]. Under B2 ETH is no longer x0.497-residual'd, so its config-default
  # mu rises ~2x vs v4.4 but the per-episode CFR remains well inside the envelope.
  high_n_isos <- c("MOZ", "KEN", "ETH", "COD")
  for (iso in high_n_isos) {
    idx <- match(iso, cfg$location_name)
    if (is.na(idx)) next
    mu_baseline <- cfg$mu_j_baseline[idx]
    per_episode_cfr <- 1 - exp(-mu_baseline / cfg$gamma_1)
    expect_gt(per_episode_cfr, 0.005,
              label = sprintf("%s per-episode CFR (%.3f%%) too low", iso, 100*per_episode_cfr))
    expect_lt(per_episode_cfr, 0.15,
              label = sprintf("%s per-episode CFR (%.3f%%) too high", iso, 100*per_episode_cfr))
  }
})
