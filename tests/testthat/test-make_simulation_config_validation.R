# Validation coverage for make_simulation_config() (review Batch 4 / B2-10: the
# function had only one incidental test). Confirms (1) the shipped config_default
# builds cleanly through the full validator, and (2) each per-parameter guard
# rejects malformed input. Complements test-config_default.R (epidemic_peaks).
# Pure-R / CPU-only (no Python).

# Valid args = the shipped default minus the tracking fields not in the signature.
.valid_sim_args <- function() {
  args <- MOSAIC::config_default
  args$metadata          <- NULL
  args$zeta_ratio        <- NULL
  args$decay_days_spread <- NULL
  args$reported_cases_weight  <- NULL
  args$reported_deaths_weight <- NULL
  args$reported_tier          <- NULL
  args$output_file_path  <- NULL
  args
}

test_that("make_simulation_config accepts the shipped config_default (valid baseline)", {
  cfg <- do.call(MOSAIC::make_simulation_config, .valid_sim_args())
  expect_type(cfg, "list")
  expect_equal(length(cfg$location_name), 40L)
})

test_that("make_simulation_config rejects a transmission param (tau_i) outside [0,1]", {
  args <- .valid_sim_args(); args$tau_i[1] <- 2
  expect_error(do.call(MOSAIC::make_simulation_config, args),
               "tau_i must be a numeric vector")
})

test_that("make_simulation_config rejects a non-character location_name", {
  args <- .valid_sim_args(); args$location_name <- seq_along(args$location_name)
  expect_error(do.call(MOSAIC::make_simulation_config, args),
               "location_name must be a character vector")
})

test_that("make_simulation_config rejects a fractional integer-count field", {
  args <- .valid_sim_args(); args$N_j_initial[1] <- args$N_j_initial[1] + 0.5
  expect_error(do.call(MOSAIC::make_simulation_config, args),
               "integer-valued")
})

test_that("make_simulation_config rejects a proportion outside [0,1]", {
  args <- .valid_sim_args(); args$prop_S_initial[1] <- 1.5
  expect_error(do.call(MOSAIC::make_simulation_config, args),
               "values in \\[0,1\\]")
})

test_that("make_simulation_config rejects a wrong-length per-location vector", {
  args <- .valid_sim_args(); args$S_j_initial <- args$S_j_initial[-1]
  expect_error(do.call(MOSAIC::make_simulation_config, args),
               "length equal to number of locations")
})

test_that("make_simulation_config rejects out-of-range scalar params", {
  a1 <- .valid_sim_args(); a1$phi_1 <- 1.5
  expect_error(do.call(MOSAIC::make_simulation_config, a1),
               "phi_1 must be numeric and within the range")
  a2 <- .valid_sim_args(); a2$omega_1 <- -1
  expect_error(do.call(MOSAIC::make_simulation_config, a2),
               "omega_1 must be a numeric scalar greater than or equal to zero")
  a3 <- .valid_sim_args(); a3$iota <- 0
  expect_error(do.call(MOSAIC::make_simulation_config, a3),
               "iota must be a numeric scalar greater than zero")
})

# ---------------------------------------------------------------------------
# alpha_1 is DUAL-MODE (priors_default v15.16 / config_default v4.7): the engine
# accepts a global scalar (broadcast across patches) OR a length-nL per-location
# vector. make_simulation_config must validate BOTH forms and reject a wrong-length
# vector and out-of-range values. Scalar back-compat protects national/legacy
# configs.
# ---------------------------------------------------------------------------
test_that("make_simulation_config accepts a SCALAR alpha_1 (back-compat broadcast)", {
  args <- .valid_sim_args(); args$alpha_1 <- 0.27
  cfg <- do.call(MOSAIC::make_simulation_config, args)
  expect_equal(length(cfg$alpha_1), 1L)
  expect_equal(cfg$alpha_1, 0.27)
})

test_that("make_simulation_config accepts a length-nL per-location alpha_1", {
  args <- .valid_sim_args()
  nL <- length(args$location_name)
  args$alpha_1 <- seq(0.2, 0.4, length.out = nL)
  cfg <- do.call(MOSAIC::make_simulation_config, args)
  expect_equal(length(cfg$alpha_1), nL)
  expect_equal(cfg$alpha_1, seq(0.2, 0.4, length.out = nL))
})

test_that("make_simulation_config rejects a wrong-length alpha_1 vector", {
  args <- .valid_sim_args(); args$alpha_1 <- args$alpha_1[-1]
  expect_error(do.call(MOSAIC::make_simulation_config, args),
               "alpha_1 must be numeric in \\(0, 1\\]")
})

test_that("make_simulation_config rejects an out-of-range alpha_1 (scalar and vector)", {
  # 0 is invalid (engine requires strict > 0)
  a0 <- .valid_sim_args(); a0$alpha_1 <- 0
  expect_error(do.call(MOSAIC::make_simulation_config, a0),
               "alpha_1 must be numeric in \\(0, 1\\]")
  # > 1 is invalid
  a1 <- .valid_sim_args(); a1$alpha_1 <- 1.5
  expect_error(do.call(MOSAIC::make_simulation_config, a1),
               "alpha_1 must be numeric in \\(0, 1\\]")
  # one out-of-range element in an otherwise-valid vector
  av <- .valid_sim_args(); av$alpha_1[1] <- 1.2
  expect_error(do.call(MOSAIC::make_simulation_config, av),
               "alpha_1 must be numeric in \\(0, 1\\]")
})

# ---------------------------------------------------------------------------
# mu_jt (v0.96.0): the reported CFR by location and day. The validator accepts
# a scalar, a per-location vector, a [nL x nT] matrix, or (one location) a daily
# vector -- how JSON returns a [1 x nT] matrix -- and always returns the full
# matrix, so every config carries one schema.
# ---------------------------------------------------------------------------
.nT_of <- function(args) length(seq.Date(as.Date(args$date_start), as.Date(args$date_stop), by = "day"))

test_that("make_simulation_config expands a scalar or per-location mu_jt to the full matrix", {
  args <- .valid_sim_args(); nL <- length(args$location_name); nT <- .nT_of(args)
  args$mu_jt <- 0.02
  cfg <- suppressMessages(do.call(MOSAIC::make_simulation_config, args))
  expect_identical(dim(cfg$mu_jt), c(nL, nT))
  expect_true(all(cfg$mu_jt == 0.02))

  args$mu_jt <- seq(0.01, 0.05, length.out = nL)
  cfg <- suppressMessages(do.call(MOSAIC::make_simulation_config, args))
  expect_identical(dim(cfg$mu_jt), c(nL, nT))
  expect_equal(cfg$mu_jt[, nT], seq(0.01, 0.05, length.out = nL))
})

test_that("make_simulation_config keeps a full mu_jt matrix and accepts a one-location daily vector", {
  args <- .valid_sim_args()
  cfg <- suppressMessages(do.call(MOSAIC::make_simulation_config, args))
  expect_identical(cfg$mu_jt, args$mu_jt)

  one <- MOSAIC::get_location_config(MOSAIC::config_default, iso = "MOZ")
  a1 <- one[intersect(names(one), names(args))]
  a1$mu_jt <- as.numeric(one$mu_jt)              # JSON's form of a [1 x nT] matrix
  cfg1 <- suppressMessages(do.call(MOSAIC::make_simulation_config, a1))
  expect_identical(dim(cfg1$mu_jt), c(1L, .nT_of(a1)))
  expect_equal(as.numeric(cfg1$mu_jt), as.numeric(one$mu_jt))
})

test_that("make_simulation_config rejects a missing, misshapen or out-of-range mu_jt", {
  args <- .valid_sim_args(); nL <- length(args$location_name); nT <- .nT_of(args)
  a <- args; a$mu_jt <- NULL
  expect_error(suppressMessages(do.call(MOSAIC::make_simulation_config, a)), "must be provided: mu_jt")
  a <- args; a$mu_jt <- matrix(0.02, nL, nT - 1L)
  expect_error(suppressMessages(do.call(MOSAIC::make_simulation_config, a)), "mu_jt must be a matrix")
  a <- args; a$mu_jt <- rep(0.02, nL + 1L)
  expect_error(suppressMessages(do.call(MOSAIC::make_simulation_config, a)), "mu_jt must be a matrix")
  a <- args; a$mu_jt[1, 1] <- 1
  expect_error(suppressMessages(do.call(MOSAIC::make_simulation_config, a)), "finite and in \\[0, 1\\)")
  a <- args; a$mu_jt[2, 3] <- -0.01
  expect_error(suppressMessages(do.call(MOSAIC::make_simulation_config, a)), "finite and in \\[0, 1\\)")
  a <- args; a$mu_jt[1, 2] <- NA
  expect_error(suppressMessages(do.call(MOSAIC::make_simulation_config, a)), "finite and in \\[0, 1\\)")
})

test_that("make_simulation_config rejects a mu_jt no per-onset probability can produce", {
  # p = mu_jt * rho / (rho_deaths * chi_epidemic) must stay below 1.
  args <- .valid_sim_args()
  args$mu_jt <- 0.8
  expect_error(suppressMessages(do.call(MOSAIC::make_simulation_config, args)),
               "no per-onset fatality probability")
  args$mu_jt <- 0.02; args$rho_deaths <- 0
  expect_error(suppressMessages(do.call(MOSAIC::make_simulation_config, args)),
               "rho_deaths is 0")
})

test_that("make_simulation_config refuses a config written for the retired mortality model", {
  # Such a config's `mu_jt` was never read by any engine, so it must not be
  # silently promoted to the reported CFR.
  a <- .valid_sim_args(); a$mu_j_baseline <- rep(0.002, length(a$location_name))
  expect_error(suppressMessages(do.call(MOSAIC::make_simulation_config, a)),
               "pre-v0.96.0 mortality model.*mu_j_baseline")
  a <- .valid_sim_args(); a$mu_j_epidemic_factor <- 0.5
  expect_error(suppressMessages(do.call(MOSAIC::make_simulation_config, a)),
               "pre-v0.96.0 mortality model.*mu_j_epidemic_factor")
})

test_that("delta_reporting_deaths is accepted silently and dropped (deaths use the case lag)", {
  a <- .valid_sim_args(); a$delta_reporting_deaths <- 5
  expect_silent(cfg <- suppressMessages(do.call(MOSAIC::make_simulation_config, a)))
  expect_null(cfg$delta_reporting_deaths)
  expect_identical(cfg, suppressMessages(do.call(MOSAIC::make_simulation_config, .valid_sim_args())))
})

# ---------------------------------------------------------------------------
# Legacy-field tolerance. mu_j_slope has been removed from the config schema
# but kept as a deprecated, ignored formal, because every config saved before
# v0.95.0 carries it and is replayed via do.call(make_simulation_config, config).
# Replay must neither error nor warn, and the field must not reappear.
# ---------------------------------------------------------------------------
test_that("a legacy config carrying mu_j_slope is accepted silently and it is dropped", {
  args <- .valid_sim_args()
  args$mu_j_slope <- rep(0.05, length(args$location_name))

  expect_silent(cfg <- suppressMessages(do.call(MOSAIC::make_simulation_config, args)))
  expect_null(cfg$mu_j_slope)

  # No length or range check survives: the value is never read, so a
  # wrong-length or non-numeric mu_j_slope must be ignored, not rejected.
  bad <- .valid_sim_args(); bad$mu_j_slope <- "not numeric"
  expect_silent(cfg_bad <- suppressMessages(do.call(MOSAIC::make_simulation_config, bad)))
  expect_null(cfg_bad$mu_j_slope)

  short <- .valid_sim_args(); short$mu_j_slope <- rep(0, 2L)
  expect_silent(cfg_short <- suppressMessages(do.call(MOSAIC::make_simulation_config, short)))
  expect_null(cfg_short$mu_j_slope)

  cfg_none <- suppressMessages(do.call(MOSAIC::make_simulation_config, .valid_sim_args()))
  expect_identical(cfg, cfg_none)
})

test_that("the engine ignores mu_j_slope entirely (R3: the trend term is gone)", {
  # The falsifiable form of "the term was removed": a config with a large
  # mu_j_slope must produce bit-identical results to one with none. Before
  # MOSAIC v0.95.0 the hazard was multiplied by (1 + mu_j_slope * tick/nticks),
  # so this comparison DIFFERED in 22 of 28 result channels.
  cfg0 <- MOSAIC::config_simulation_epidemic
  expect_null(cfg0$mu_j_slope)          # no longer shipped

  cfgS <- cfg0
  cfgS$mu_j_slope <- rep(0.5, length(cfg0$location_name))

  expect_identical(
    MOSAIC::run_simulation(cfgS, seed = 11L)$results,
    MOSAIC::run_simulation(cfg0, seed = 11L)$results
  )
})
