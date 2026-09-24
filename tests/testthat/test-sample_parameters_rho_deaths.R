library(testthat)
library(MOSAIC)

# Set root directory (required for sample_parameters to load defaults).
# Tests skip cleanly if not set so this file is safe to ship before the
# data-raw rebuild has populated priors_default with rho_deaths.
tryCatch(set_root_directory("~/MOSAIC"), error = function(e) NULL)

# skip_if_no_rho_deaths_prior() is centralized in helper-skips.R.

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
  # sample_rho_deaths defaults FALSE (pinned), so the draw must be requested
  # explicitly here -- otherwise this fixture would pass vacuously on the
  # constant 0.42 and stop testing the prior at all.
  cfg <- sample_parameters(seed = 1L, verbose = FALSE,
                           sample_args = list(sample_rho_deaths = TRUE))
  expect_true(is.finite(cfg$rho_deaths))
  expect_gt(cfg$rho_deaths, 0)
  expect_lt(cfg$rho_deaths, 1)
  # ... and it must actually differ from the pinned constant, i.e. the flag works.
  expect_false(isTRUE(all.equal(cfg$rho_deaths, MOSAIC::config_default$rho_deaths)))
})

test_that("rho_deaths is PINNED by default (R2: cancels identically under B2)", {
  skip_if_no_rho_deaths_prior()
  # Under B2 sample_parameters() derives mu_j_baseline proportional to
  # 1/rho_deaths while the engine thins disease_deaths by rho_deaths, so the
  # parameter cancels from the deaths mean and carries no likelihood
  # information. It is therefore held at config_default$rho_deaths = 0.42.
  pinned <- MOSAIC::config_default$rho_deaths
  expect_equal(pinned, 0.42, tolerance = 1e-12)
  for (s in c(1L, 7L, 4242L)) {
    cfg <- sample_parameters(seed = s, verbose = FALSE)
    expect_equal(cfg$rho_deaths, pinned, tolerance = 1e-12,
                 info = sprintf("seed %d", s))
  }
})

test_that("the B2 product mu_j_baseline * rho_deaths is invariant to the pin", {
  # The algebraic invariant the pin rests on: for the SAME CFR_target and chain
  # draws, mu_j_baseline * rho_deaths is unchanged when rho_deaths moves, because
  # mu = CFR_target * (1 - exp(-gamma_1)) * rho / (rho_deaths * chi_epidemic).
  # Self-contained (no MOSAIC root needed).
  priors <- list(
    parameters_global = list(
      gamma_1      = list(distribution = "lognormal", parameters = list(meanlog = log(0.10), sdlog = 0.5)),
      rho          = list(distribution = "beta", parameters = list(shape1 = 5.38, shape2 = 7.10)),
      rho_deaths   = list(distribution = "beta", parameters = list(shape1 = 36.95, shape2 = 51.02)),
      chi_endemic  = list(distribution = "beta", parameters = list(shape1 = 5.43, shape2 = 5.01)),
      chi_epidemic = list(distribution = "beta", parameters = list(shape1 = 4.79, shape2 = 1.53))
    ),
    parameters_location = list(
      CFR_target = list(description = "test", location = list(
        AAA = list(distribution = "lognormal", parameters = list(meanlog = log(0.02), sdlog = 0.787))
      ))
    )
  )
  base_cfg <- list(
    location_name = "AAA", gamma_1 = 0.10, rho = 0.423, rho_deaths = 0.42,
    chi_endemic = 0.50, chi_epidemic = 0.75,
    CFR_target = 0.02, mu_j_baseline = 0.003, seed = 1L
  )
  frozen <- list(
    sample_gamma_1 = FALSE, sample_rho = FALSE, sample_rho_deaths = FALSE,
    sample_chi_endemic = FALSE, sample_chi_epidemic = FALSE,
    sample_CFR_target = FALSE, sample_mu_j_baseline = TRUE,
    sample_initial_conditions = FALSE
  )
  ref <- NULL
  for (rd in c(0.25, 0.32, 0.42, 0.52, 0.65)) {
    cfg <- base_cfg; cfg$rho_deaths <- rd
    out <- MOSAIC::sample_parameters(PATHS = list(), priors = priors, config = cfg,
                                     seed = 11L, verbose = FALSE, validate = FALSE,
                                     sample_args = frozen)
    prod <- out$mu_j_baseline * out$rho_deaths
    if (is.null(ref)) ref <- prod
    expect_equal(prod, ref, tolerance = 1e-12,
                 info = sprintf("rho_deaths = %.2f", rd))
  }
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
  # 40 draws: Beta(36.95,51.02) has sd ~0.052, so SE(mean) ~0.008 -- the [0.37,
  # 0.47] mean band sits ~6 SE from the true mean (0.420), comfortably robust at
  # this draw count while keeping the loop cheap.
  # sample_rho_deaths defaults FALSE (pinned); request the draw explicitly so
  # this fixture tests the PRIOR rather than the pinned constant.
  draws <- vapply(1:40, function(s) {
    sample_parameters(seed = s, verbose = FALSE,
                      sample_args = list(sample_rho_deaths = TRUE))$rho_deaths
  }, numeric(1))
  expect_gt(stats::sd(draws), 0)   # guard: a pinned default would give sd == 0
  expect_true(all(is.finite(draws)))
  # Beta(36.95, 51.02): mean 0.420, 95% CI [0.319, 0.524]. The informative
  # variant is tight (sd ~0.052), so use a fairly narrow inner band.
  expect_gte(mean(draws > 0.20 & draws < 0.60), 0.95)
  expect_gt(mean(draws), 0.37)
  expect_lt(mean(draws), 0.47)
})
