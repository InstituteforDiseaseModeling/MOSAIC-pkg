# =============================================================================
# test-cfr-pipeline-consistency.R
#
# Guards the invariant that config_default$mu_j_baseline and priors_default
# agree per country. The two data objects are built by independent scripts
# (data-raw/make_config_default.R and data-raw/make_priors_default.R) and
# silent drift between them is the bug class that caused 38 of 40 countries
# to ship raw-CFR values as daily hazards before v0.32.1 (see NEWS / CLAUDE.md
# Lesson #12).
#
# The shared invariant under the v0.13+ schema:
#
#   config_default$mu_j_baseline[iso] == Gamma_mean(priors_default$mu_j_baseline$location[[iso]])
#
# Both pipelines apply the steady-state identity
#   mu_j_baseline = reported_CFR * rho / (rho_deaths * chi)
# to derive the per-country mean. config_default reads the Gamma mean directly
# from priors_default at build time, so a mismatch means one of the .rda files
# has gone stale.
# =============================================================================

library(testthat)

test_that("config_default$mu_j_baseline matches priors_default Gamma mean per country", {
  skip_if_not_installed("MOSAIC")
  cfg <- tryCatch(MOSAIC::config_default, error = function(e) NULL)
  pri <- tryCatch(MOSAIC::priors_default, error = function(e) NULL)
  if (is.null(cfg) || is.null(pri)) skip("config_default or priors_default not loadable")
  if (is.null(cfg$location_name) || is.null(cfg$mu_j_baseline)) {
    skip("config_default missing location_name or mu_j_baseline")
  }
  if (is.null(pri$parameters_location$mu_j_baseline$location)) {
    skip("priors_default missing per-location mu_j_baseline")
  }

  tol <- 1e-6  # exact equality expected since config sources from prior
  for (i in seq_along(cfg$location_name)) {
    iso <- cfg$location_name[i]
    prior_loc <- pri$parameters_location$mu_j_baseline$location[[iso]]
    if (is.null(prior_loc)) {
      fail(sprintf("priors_default has no mu_j_baseline for iso '%s'", iso))
      next
    }
    expect_equal(prior_loc$distribution, "gamma",
                 info = sprintf("iso %s: expected gamma prior", iso))
    p <- prior_loc$parameters
    prior_mean <- p$shape / p$rate
    expect_equal(cfg$mu_j_baseline[i], unname(prior_mean), tolerance = tol,
                 info = sprintf("iso %s: config=%.8f vs prior_mean=%.8f",
                                iso, cfg$mu_j_baseline[i], prior_mean))
  }
})

test_that("priors_default mu_j_baseline derivation uses v0.13+ adjustment factor", {
  skip_if_not_installed("MOSAIC")
  pri <- tryCatch(MOSAIC::priors_default, error = function(e) NULL)
  if (is.null(pri)) skip("priors_default not loadable")

  # The description should mention the v0.13+ identity
  desc <- pri$parameters_location$mu_j_baseline$description
  expect_true(
    grepl("rho_deaths", desc, fixed = TRUE),
    info = "mu_j_baseline description should mention rho_deaths (v0.13+ identity)"
  )
  expect_true(
    grepl("CFR.*rho.*rho_deaths.*chi", desc),
    info = "mu_j_baseline description should describe the CFR -> mu identity"
  )
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
  # ACTUAL gamma_1 (not a hardcoded 0.2 — config_default uses 0.1; the old hardcode
  # was a latent 2x error that only surfaced once v0.14.0 lowered mu). v0.14.0: the
  # gamma_1-factor CFR->mu identity lowers mu (hence per-episode CFR) ~8.8x vs the
  # pre-v0.14 prevalence form. High-N stable countries should be in [0.5%, 15%].
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
