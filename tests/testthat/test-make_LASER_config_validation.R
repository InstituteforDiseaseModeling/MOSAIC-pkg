# Validation coverage for make_LASER_config() (review Batch 4 / B2-10: the
# function had only one incidental test). Confirms (1) the shipped config_default
# builds cleanly through the full validator, and (2) each per-parameter guard
# rejects malformed input. Complements test-config_default.R (epidemic_peaks).
# Pure-R / CPU-only (no Python).

# Valid args = the shipped default minus the tracking fields not in the signature.
.valid_LASER_args <- function() {
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

test_that("make_LASER_config accepts the shipped config_default (valid baseline)", {
  cfg <- do.call(MOSAIC::make_LASER_config, .valid_LASER_args())
  expect_type(cfg, "list")
  expect_equal(length(cfg$location_name), 40L)
})

test_that("make_LASER_config rejects a transmission param (tau_i) outside [0,1]", {
  args <- .valid_LASER_args(); args$tau_i[1] <- 2
  expect_error(do.call(MOSAIC::make_LASER_config, args),
               "tau_i must be a numeric vector")
})

test_that("make_LASER_config rejects a non-character location_name", {
  args <- .valid_LASER_args(); args$location_name <- seq_along(args$location_name)
  expect_error(do.call(MOSAIC::make_LASER_config, args),
               "location_name must be a character vector")
})

test_that("make_LASER_config rejects a fractional integer-count field", {
  args <- .valid_LASER_args(); args$N_j_initial[1] <- args$N_j_initial[1] + 0.5
  expect_error(do.call(MOSAIC::make_LASER_config, args),
               "integer-valued")
})

test_that("make_LASER_config rejects a proportion outside [0,1]", {
  args <- .valid_LASER_args(); args$prop_S_initial[1] <- 1.5
  expect_error(do.call(MOSAIC::make_LASER_config, args),
               "values in \\[0,1\\]")
})

test_that("make_LASER_config rejects a wrong-length per-location vector", {
  args <- .valid_LASER_args(); args$S_j_initial <- args$S_j_initial[-1]
  expect_error(do.call(MOSAIC::make_LASER_config, args),
               "length equal to number of locations")
})

test_that("make_LASER_config rejects out-of-range scalar params", {
  a1 <- .valid_LASER_args(); a1$phi_1 <- 1.5
  expect_error(do.call(MOSAIC::make_LASER_config, a1),
               "phi_1 must be numeric and within the range")
  a2 <- .valid_LASER_args(); a2$omega_1 <- -1
  expect_error(do.call(MOSAIC::make_LASER_config, a2),
               "omega_1 must be a numeric scalar greater than or equal to zero")
  a3 <- .valid_LASER_args(); a3$iota <- 0
  expect_error(do.call(MOSAIC::make_LASER_config, a3),
               "iota must be a numeric scalar greater than zero")
})

# ---------------------------------------------------------------------------
# alpha_1 is DUAL-MODE (priors_default v15.16 / config_default v4.7): the engine
# accepts a global scalar (broadcast across patches) OR a length-nL per-location
# vector. make_LASER_config must validate BOTH forms and reject a wrong-length
# vector and out-of-range values. Scalar back-compat protects national/legacy
# configs.
# ---------------------------------------------------------------------------
test_that("make_LASER_config accepts a SCALAR alpha_1 (back-compat broadcast)", {
  args <- .valid_LASER_args(); args$alpha_1 <- 0.27
  cfg <- do.call(MOSAIC::make_LASER_config, args)
  expect_equal(length(cfg$alpha_1), 1L)
  expect_equal(cfg$alpha_1, 0.27)
})

test_that("make_LASER_config accepts a length-nL per-location alpha_1", {
  args <- .valid_LASER_args()
  nL <- length(args$location_name)
  args$alpha_1 <- seq(0.2, 0.4, length.out = nL)
  cfg <- do.call(MOSAIC::make_LASER_config, args)
  expect_equal(length(cfg$alpha_1), nL)
  expect_equal(cfg$alpha_1, seq(0.2, 0.4, length.out = nL))
})

test_that("make_LASER_config rejects a wrong-length alpha_1 vector", {
  args <- .valid_LASER_args(); args$alpha_1 <- args$alpha_1[-1]
  expect_error(do.call(MOSAIC::make_LASER_config, args),
               "alpha_1 must be numeric in \\(0, 1\\]")
})

test_that("make_LASER_config rejects an out-of-range alpha_1 (scalar and vector)", {
  # 0 is invalid (engine requires strict > 0)
  a0 <- .valid_LASER_args(); a0$alpha_1 <- 0
  expect_error(do.call(MOSAIC::make_LASER_config, a0),
               "alpha_1 must be numeric in \\(0, 1\\]")
  # > 1 is invalid
  a1 <- .valid_LASER_args(); a1$alpha_1 <- 1.5
  expect_error(do.call(MOSAIC::make_LASER_config, a1),
               "alpha_1 must be numeric in \\(0, 1\\]")
  # one out-of-range element in an otherwise-valid vector
  av <- .valid_LASER_args(); av$alpha_1[1] <- 1.2
  expect_error(do.call(MOSAIC::make_LASER_config, av),
               "alpha_1 must be numeric in \\(0, 1\\]")
})
