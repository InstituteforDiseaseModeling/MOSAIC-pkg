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
  args$CFR_target        <- NULL   # B2 (v4.5): injected tracking field, not a signature arg
  args$reported_cases_weight  <- NULL
  args$reported_deaths_weight <- NULL
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
# mu_j_* are DUAL-MODE for the same reason alpha_1 is: .sim_patch_vector()
# (R/sim_params.R) broadcasts a length-1 mu_j_baseline / mu_j_slope /
# mu_j_epidemic_factor across every patch. Before the v4.8 mu_jt removal these
# length checks lived inside the (dead-on-the-shipped-path) mu_jt generation
# block and demanded length-nL; re-homing them must not make the validator
# stricter than the engine it validates, or a saved scalar config stops loading.
# ---------------------------------------------------------------------------
test_that("make_simulation_config accepts SCALAR mu_j_* (engine broadcasts them)", {
  args <- .valid_sim_args()
  args$mu_j_baseline        <- 0.002
  args$mu_j_slope           <- 0
  args$mu_j_epidemic_factor <- 0.5
  cfg <- do.call(MOSAIC::make_simulation_config, args)
  expect_equal(cfg$mu_j_baseline, 0.002)
  expect_equal(cfg$mu_j_slope, 0)
  expect_equal(cfg$mu_j_epidemic_factor, 0.5)
})

test_that("make_simulation_config accepts length-nL mu_j_* (per-location form)", {
  args <- .valid_sim_args()
  nL <- length(args$location_name)
  expect_equal(length(args$mu_j_baseline), nL)  # the shipped default is per-location
  cfg <- do.call(MOSAIC::make_simulation_config, args)
  expect_equal(length(cfg$mu_j_baseline), nL)
  expect_equal(length(cfg$mu_j_slope), nL)
  expect_equal(length(cfg$mu_j_epidemic_factor), nL)
})

test_that("a SCALAR mu_j_baseline broadcasts to the same simulation as its length-nL form", {
  # The broadcast is the engine's job; assert it end-to-end so relaxing the
  # validator cannot quietly diverge from what run_simulation() actually does.
  cfg_vec <- MOSAIC::config_simulation_epidemic
  nL <- length(cfg_vec$location_name)
  cfg_vec$mu_j_baseline        <- rep(0.01, nL)
  cfg_vec$mu_j_slope           <- rep(0,    nL)
  cfg_vec$mu_j_epidemic_factor <- rep(0.5,  nL)

  cfg_sca <- cfg_vec
  cfg_sca$mu_j_baseline        <- 0.01
  cfg_sca$mu_j_slope           <- 0
  cfg_sca$mu_j_epidemic_factor <- 0.5

  expect_identical(
    MOSAIC::run_simulation(cfg_sca, seed = 1L)$results,
    MOSAIC::run_simulation(cfg_vec, seed = 1L)$results
  )
})

test_that("make_simulation_config rejects a wrong-length mu_j_* vector", {
  nL <- length(.valid_sim_args()$location_name)

  a1 <- .valid_sim_args(); a1$mu_j_baseline <- rep(0.002, nL - 1L)
  expect_error(do.call(MOSAIC::make_simulation_config, a1),
               "mu_j_baseline must be a numeric scalar or a vector")

  a2 <- .valid_sim_args(); a2$mu_j_slope <- rep(0, 2L)
  expect_error(do.call(MOSAIC::make_simulation_config, a2),
               "mu_j_slope must be a numeric scalar or a vector")

  a3 <- .valid_sim_args(); a3$mu_j_epidemic_factor <- rep(0.5, nL + 1L)
  expect_error(do.call(MOSAIC::make_simulation_config, a3),
               "mu_j_epidemic_factor must be a numeric scalar or a vector")
})

test_that("make_simulation_config still range-checks mu_j_* (scalar and vector)", {
  a1 <- .valid_sim_args(); a1$mu_j_baseline <- 1.5
  expect_error(do.call(MOSAIC::make_simulation_config, a1),
               "mu_j_baseline must be between 0 and 1")

  a2 <- .valid_sim_args(); a2$mu_j_baseline[1] <- -0.1
  expect_error(do.call(MOSAIC::make_simulation_config, a2),
               "mu_j_baseline must be between 0 and 1")

  a3 <- .valid_sim_args(); a3$mu_j_epidemic_factor <- -2
  expect_error(do.call(MOSAIC::make_simulation_config, a3),
               "mu_j_epidemic_factor must be greater than or equal to -1")
})

# ---------------------------------------------------------------------------
# mu_jt legacy tolerance (v4.8). The [nL x nT] mu_jt matrix is no longer built,
# validated or returned, but every config saved before the removal carries one
# and is replayed via do.call(make_simulation_config, config). The formal is
# retained, deprecated and ignored, so that replay neither errors nor warns.
# ---------------------------------------------------------------------------
test_that("a legacy config carrying mu_jt is accepted silently and mu_jt is dropped", {
  args <- .valid_sim_args()
  nT <- length(seq.Date(as.Date(args$date_start), as.Date(args$date_stop), by = "day"))
  args$mu_jt <- matrix(0.01, nrow = length(args$location_name), ncol = nT)

  expect_silent(cfg <- suppressMessages(do.call(MOSAIC::make_simulation_config, args)))
  expect_null(cfg$mu_jt)

  # A garbage mu_jt must be ignored just as thoroughly -- it is never read.
  bad <- .valid_sim_args(); bad$mu_jt <- "not a matrix"
  expect_silent(cfg_bad <- suppressMessages(do.call(MOSAIC::make_simulation_config, bad)))
  expect_null(cfg_bad$mu_jt)

  # And the result must not depend on whether mu_jt was supplied at all.
  cfg_none <- suppressMessages(do.call(MOSAIC::make_simulation_config, .valid_sim_args()))
  expect_identical(cfg, cfg_none)
})
