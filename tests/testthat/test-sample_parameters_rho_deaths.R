library(testthat)
library(MOSAIC)

# Set root directory (required for sample_parameters to load defaults).
# Tests skip cleanly if not set so this file is safe to ship before the
# data-raw rebuild has populated priors_default with rho_deaths.
tryCatch(set_root_directory("~/MOSAIC"), error = function(e) NULL)

skip_if_no_rho_deaths_prior <- function() {
  skip_if(is.null(getOption("root_directory")), "MOSAIC root directory not set")
  pri <- tryCatch(MOSAIC::priors_default, error = function(e) NULL)
  if (is.null(pri) || is.null(pri$parameters_global$rho_deaths)) {
    skip("priors_default$parameters_global$rho_deaths not yet populated (data-raw rebuild pending)")
  }
}

test_that("rho_deaths is present in default config", {
  skip_if(is.null(getOption("root_directory")), "MOSAIC root directory not set")
  cfg <- tryCatch(MOSAIC::config_default, error = function(e) NULL)
  if (is.null(cfg)) skip("config_default not loadable")
  expect_true("rho_deaths" %in% names(cfg))
  expect_true(is.numeric(cfg$rho_deaths))
  expect_gte(cfg$rho_deaths, 0)
  expect_lte(cfg$rho_deaths, 1)
})

test_that("priors_default carries the Beta(36.95, 51.02) informative prior for rho_deaths", {
  skip_if_no_rho_deaths_prior()
  # Beta(36.95, 51.02): mean 0.420, 95% CI [0.319, 0.524]. Derived from
  # random-effects meta-analysis of three SSA studies (Routh 2017, Shikanga 2009,
  # Bwire 2013); informative variant fit to the pooled-mean CI for cleaner
  # mu_j_baseline identifiability (SYNTHESIS_REPORT sec 3.3 + 3.4). The wider
  # prediction-interval variant Beta(6.30, 8.52) is retained for sensitivity.
  prior <- MOSAIC::priors_default$parameters_global$rho_deaths
  expect_equal(prior$distribution, "beta")
  expect_equal(prior$parameters$shape1, 36.95)
  expect_equal(prior$parameters$shape2, 51.02)
})

test_that("sample_parameters draws rho_deaths in (0, 1) when sample_rho_deaths = TRUE", {
  skip_if_no_rho_deaths_prior()
  cfg <- sample_parameters(seed = 1L, verbose = FALSE)
  expect_true(is.finite(cfg$rho_deaths))
  expect_gt(cfg$rho_deaths, 0)
  expect_lt(cfg$rho_deaths, 1)
})

test_that("sample_parameters holds rho_deaths fixed when sample_rho_deaths = FALSE", {
  skip_if_no_rho_deaths_prior()
  cfg_default <- MOSAIC::config_default
  if (is.null(cfg_default) || !("rho_deaths" %in% names(cfg_default))) {
    skip("config_default$rho_deaths not loadable")
  }
  fixed_value <- cfg_default$rho_deaths
  cfg <- sample_parameters(
    seed = 2L,
    verbose = FALSE,
    sample_args = list(sample_rho_deaths = FALSE)
  )
  expect_equal(cfg$rho_deaths, fixed_value, tolerance = 1e-10)
})

test_that("rho_deaths empirical draws match Beta(36.95, 51.02)", {
  skip_if_no_rho_deaths_prior()
  draws <- vapply(1:200, function(s) {
    sample_parameters(seed = s, verbose = FALSE)$rho_deaths
  }, numeric(1))
  expect_true(all(is.finite(draws)))
  # Beta(36.95, 51.02): mean 0.420, 95% CI [0.319, 0.524]. The informative
  # variant is tight (sd ~0.052), so use a fairly narrow inner band.
  expect_gte(mean(draws > 0.20 & draws < 0.60), 0.95)
  expect_gt(mean(draws), 0.37)
  expect_lt(mean(draws), 0.47)
})
