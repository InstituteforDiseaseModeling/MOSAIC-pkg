# Regression tests for the config-group review fixes in R/sample_parameters.R:
# create_sampling_args(), the shared default flag list, unknown `...` flags,
# the pinned-psi_star calibration, and the NA guards in both samplers.

cfg_eth <- function() get_location_config(iso = "ETH")

sample_quiet <- function(...) {
  sample_parameters(PATHS = list(), verbose = FALSE, validate = FALSE, ...)
}

test_that("sample_parameters() and mosaic_control_defaults() share one set of sampling defaults", {
  flags_sp   <- MOSAIC:::.mosaic_default_sample_args()
  flags_ctrl <- mosaic_control_defaults()$sampling
  expect_setequal(names(flags_sp), names(flags_ctrl))
  expect_identical(flags_sp[sort(names(flags_sp))], flags_ctrl[sort(names(flags_ctrl))])
  # kappa has been pinned at config_default$kappa since v0.89.0
  expect_false(flags_sp$sample_kappa)
})

test_that("sample_parameters(sample_args = NULL) keeps kappa at the config value", {
  cfg <- cfg_eth()
  out <- sample_quiet(config = cfg, seed = 3)
  expect_identical(out$kappa, cfg$kappa)
})

test_that("create_sampling_args('none') leaves every sampled parameter at its config value", {
  cfg  <- cfg_eth()
  args <- create_sampling_args("none", seed = 11, config = cfg, PATHS = list())
  expect_true(is.list(args$sample_args))
  expect_true(all(!unlist(args$sample_args)))
  out <- do.call(sample_parameters, c(args, verbose = FALSE))
  for (p in c("kappa", "gamma_1", "iota", "sigma", "rho", "beta_j0_tot", "p_beta",
              "tau_i", "theta_j", "a_1_j", "epidemic_threshold", "zeta_1",
              "psi_star_a", "psi_star_b", "S_j_initial", "E_j_initial", "I_j_initial")) {
    expect_identical(out[[p]], cfg[[p]], info = p)
  }
})

test_that("create_sampling_args('disease_only') samples disease parameters and nothing else", {
  cfg  <- cfg_eth()
  args <- create_sampling_args("disease_only", seed = 5, config = cfg, PATHS = list())
  expect_true(args$sample_args$sample_gamma_1)
  expect_false(args$sample_args$sample_beta_j0_tot)
  expect_false(args$sample_args$sample_initial_conditions)
  out <- do.call(sample_parameters, c(args, verbose = FALSE))
  expect_false(identical(out$gamma_1, cfg$gamma_1))
  expect_false(identical(out$iota, cfg$iota))
  expect_identical(out$beta_j0_tot, cfg$beta_j0_tot)
  expect_identical(out$S_j_initial, cfg$S_j_initial)
  expect_identical(out$kappa, cfg$kappa)
})

test_that("create_sampling_args() honours custom overrides with real flag names", {
  expect_silent(args <- create_sampling_args("none", seed = 1,
                                             custom = list(sample_kappa = TRUE)))
  expect_true(args$sample_args$sample_kappa)
  expect_false(args$sample_args$sample_gamma_1)
  expect_warning(create_sampling_args("none", seed = 1, custom = list(sample_kapa = TRUE)),
                 "Unknown parameter in custom overrides: sample_kapa")
  expect_error(create_sampling_args("bogus", seed = 1), "Unknown pattern")
})

test_that("create_sampling_args('all') reproduces the package defaults", {
  args <- create_sampling_args("all", seed = 1)
  expect_identical(args$sample_args, MOSAIC:::.mosaic_default_sample_args())
  expect_identical(args$seed, 1)
})

test_that("a misspelled sample_* flag passed through ... warns instead of being dropped", {
  cfg <- cfg_eth()
  expect_warning(sample_quiet(config = cfg, seed = 1, sample_gama_1 = FALSE),
                 "Unknown sampling parameter: sample_gama_1")
})

test_that("pinned psi_star values are applied even when every psi_star flag is FALSE", {
  cfg <- cfg_eth()
  expect_true(any(cfg$psi_star_b != 0))
  pin_all <- list(sample_psi_star_a = FALSE, sample_psi_star_b = FALSE,
                  sample_psi_star_z = FALSE, sample_psi_star_k = FALSE)
  out_pinned <- sample_quiet(config = cfg, seed = 2, sample_args = pin_all)
  expected <- calc_psi_star(cfg$psi_jt[1, ], a = cfg$psi_star_a[1], b = cfg$psi_star_b[1],
                            z = cfg$psi_star_z[1], k = cfg$psi_star_k[1],
                            fill_method = "locf", warn_k_rounding = FALSE)
  expect_equal(unname(out_pinned$psi_jt[1, ]), unname(expected))
  expect_false(isTRUE(all.equal(out_pinned$psi_jt, cfg$psi_jt)))

  # Turning on an unrelated sibling flag applies the same pinned b
  out_k <- sample_quiet(config = cfg, seed = 2,
                        sample_args = modifyList(pin_all, list(sample_psi_star_k = TRUE)))
  expect_identical(out_k$psi_star_b, out_pinned$psi_star_b)
})

test_that("psi_jt is left untouched when all psi_star values are pinned at the identity", {
  cfg <- cfg_eth()
  cfg$psi_star_a[] <- 1; cfg$psi_star_b[] <- 0; cfg$psi_star_z[] <- 1; cfg$psi_star_k[] <- 0
  pin_all <- list(sample_psi_star_a = FALSE, sample_psi_star_b = FALSE,
                  sample_psi_star_z = FALSE, sample_psi_star_k = FALSE)
  out <- sample_quiet(config = cfg, seed = 2, sample_args = pin_all)
  expect_identical(out$psi_jt, cfg$psi_jt)
})

test_that("an NA draw for a global parameter falls back to the config value", {
  cfg <- cfg_eth()
  pri <- priors_default
  pri$parameters_global$gamma_1 <- list(distribution = "failed", parameters = list())
  rm(list = ls(MOSAIC:::.mosaic_once, pattern = "^global_prior_na_"),
     envir = MOSAIC:::.mosaic_once)
  expect_warning(out <- sample_quiet(config = cfg, priors = pri, seed = 4),
                 "gamma_1")
  expect_identical(out$gamma_1, cfg$gamma_1)

  cfg_missing <- cfg
  cfg_missing$gamma_1 <- NULL
  expect_error(sample_quiet(config = cfg_missing, priors = pri, seed = 4),
               "gamma_1")
})

test_that("a location NA draw with no config fallback stops and leaves the global env clean", {
  cfg <- cfg_eth()
  pri <- priors_default
  pri$parameters_location$new_param <- list(
    location = list(ETH = list(distribution = "beta",
                               parameters = list(shape1 = NA_real_, shape2 = 2))))
  if (exists("failed_locations", envir = globalenv())) rm("failed_locations", envir = globalenv())
  expect_error(suppressWarnings(sample_quiet(config = cfg, priors = pri, seed = 4)),
               "Failed to sample parameter 'new_param'")
  expect_false(exists("failed_locations", envir = globalenv()))
})
