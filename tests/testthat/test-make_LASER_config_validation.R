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
