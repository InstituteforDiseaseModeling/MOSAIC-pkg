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

test_that("create_sampling_args('none') draws nothing; only the pinned psi_star transform touches psi_jt", {
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
  # psi_jt is not drawn, but config's pinned psi_star values (psi_star_b = 1) are applied
  expected <- calc_psi_star(cfg$psi_jt[1, ], a = cfg$psi_star_a[1], b = cfg$psi_star_b[1],
                            z = cfg$psi_star_z[1], k = cfg$psi_star_k[1],
                            fill_method = "locf", warn_k_rounding = FALSE)
  expect_identical(unname(out$psi_jt[1, ]), unname(expected))
})

test_that("no create_sampling_args() pattern un-pins a parameter the defaults pin", {
  defaults <- MOSAIC:::.mosaic_default_sample_args()
  pinned   <- names(defaults)[!unlist(defaults)]
  expect_true(all(c("sample_alpha_1", "sample_alpha_2", "sample_kappa",
                    "sample_rho_deaths") %in% pinned))
  for (pat in c("none", "disease_only", "transmission_only", "mobility_only",
                "spatial_only", "environmental_only", "initial_conditions_only")) {
    flags <- create_sampling_args(pat, seed = 1)$sample_args
    expect_false(any(unlist(flags[pinned])), info = pat)
    expect_true(pat == "none" || any(unlist(flags)), info = pat)
  }
})

test_that("create_sampling_args('spatial_only') keeps kappa at its pinned config value", {
  cfg  <- cfg_eth()
  args <- create_sampling_args("spatial_only", seed = 1, config = cfg, PATHS = list())
  out  <- do.call(sample_parameters, c(args, verbose = FALSE))
  expect_identical(out$kappa, cfg$kappa)
  expect_false(identical(out$mobility_omega, cfg$mobility_omega))
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

test_that("re-sampling a sampled config never applies psi_star twice", {
  cfg <- cfg_eth()
  s1  <- sample_quiet(config = cfg, seed = 7)
  expect_true(isTRUE(attr(s1, "psi_star_applied")))
  expect_null(attr(cfg, "psi_star_applied"))
  none <- create_sampling_args("none", seed = 8)$sample_args
  s2   <- sample_quiet(config = s1, seed = 8, sample_args = none)
  expect_identical(s2$psi_jt, s1$psi_jt)
  expect_identical(s2$psi_star_b, s1$psi_star_b)
  # Redrawing psi_star needs the raw psi_jt, which a sampled config no longer has
  expect_error(sample_quiet(config = s1, seed = 8), "already carries a psi_star calibration")
  # Other flags can still be re-sampled around the calibrated psi
  s3 <- sample_quiet(config = s1, seed = 9,
                     sample_args = modifyList(none, list(sample_gamma_1 = TRUE)))
  expect_identical(s3$psi_jt, s1$psi_jt)
  expect_false(identical(s3$gamma_1, s1$gamma_1))
})

test_that("the psi_star mark survives a JSON round trip (config_medoid.json)", {
  cfg <- cfg_eth()
  s1  <- sample_quiet(config = cfg, seed = 7)
  expect_true(isTRUE(s1$psi_star_applied))
  expect_null(cfg$psi_star_applied)
  f <- withr::local_tempfile(fileext = ".json")
  MOSAIC:::.mosaic_write_config_medoid(s1, NULL, NULL, f)
  back <- read_json_to_list(f)
  expect_null(attr(back, "psi_star_applied"))
  expect_true(isTRUE(back$psi_star_applied))
  # 17 significant digits: the medoid's parameters come back exactly
  expect_identical(back$gamma_1, s1$gamma_1)
  expect_equal(unname(as.matrix(back$psi_jt)), unname(s1$psi_jt), tolerance = 0)
  # Default flags would redraw psi_star on an already-calibrated psi -> error
  expect_error(sample_quiet(config = back, seed = 8), "already carries a psi_star calibration")
  # All psi_star flags FALSE: psi_jt kept as is, not transformed a second time
  pin_all <- list(sample_psi_star_a = FALSE, sample_psi_star_b = FALSE,
                  sample_psi_star_z = FALSE, sample_psi_star_k = FALSE)
  s2 <- sample_quiet(config = back, seed = 8, sample_args = pin_all)
  expect_equal(unname(as.matrix(s2$psi_jt)), unname(as.matrix(back$psi_jt)), tolerance = 0)
})

test_that("psi_jt is left untouched when all psi_star values are pinned at the identity", {
  cfg <- cfg_eth()
  cfg$psi_star_a[] <- 1; cfg$psi_star_b[] <- 0; cfg$psi_star_z[] <- 1; cfg$psi_star_k[] <- 0
  pin_all <- list(sample_psi_star_a = FALSE, sample_psi_star_b = FALSE,
                  sample_psi_star_z = FALSE, sample_psi_star_k = FALSE)
  out <- sample_quiet(config = cfg, seed = 2, sample_args = pin_all)
  expect_identical(out$psi_jt, cfg$psi_jt)
  expect_null(attr(out, "psi_star_applied"))
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
