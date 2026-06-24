# Coverage for validate_sampled_config() dual-mode alpha_1 handling.
#
# alpha_1 was relocated from a global scalar to a PER-LOCATION prior
# (priors_default v15.16 / config_default v4.7). The validation schema in
# validate_sampled_config() previously listed alpha_1 under the rigid
# `global`/scalar group, which HARD-ERRORED on a length-nL value
# ("condition has length > 1"). alpha_1 is now validated as DUAL-MODE
# (scalar OR length-nL), so both forms must pass and a wrong-length value
# must fail. alpha_2 stays a strict global scalar.
library(testthat)
library(MOSAIC)

# Minimal config that satisfies the full validate_sampled_config schema for a
# 2-location case. Values are placeholders chosen to pass every schema check;
# the test focuses on the alpha_1 dual-mode branch.
.minimal_valid_config <- function(nL = 2L) {
  iso <- head(c("ETH", "KEN", "MOZ", "COD"), nL)
  list(
    location_name = iso,
    # global scalars
    phi_1 = 0.5, phi_2 = 0.3, omega_1 = 0.1, omega_2 = 0.05, iota = 0.01,
    gamma_1 = 0.1, gamma_2 = 0.1, epsilon = 1e-4,
    chi_endemic = 0.5, chi_epidemic = 0.7,
    rho = 0.42, rho_deaths = 0.42, sigma = 0.2,
    mobility_omega = 1, mobility_gamma = 1,
    zeta_1 = 1e10, zeta_ratio = 70, zeta_2 = 1e9, kappa = 1e5,
    alpha_2 = 0.5,
    decay_days_short = 16, decay_days_spread = 180, decay_days_long = 196,
    decay_shape_1 = 3, decay_shape_2 = 3,
    delta_reporting_cases = 7, delta_reporting_deaths = 7,
    # location vectors (length nL)
    beta_j0_env = rep(1e-5, nL), beta_j0_hum = rep(1e-5, nL),
    tau_i = rep(0.5, nL), theta_j = rep(0.5, nL),
    a_1_j = rep(0.5, nL), a_2_j = rep(0.5, nL),
    b_1_j = rep(0.5, nL), b_2_j = rep(0.5, nL),
    mu_j_baseline = rep(1e-3, nL), mu_j_slope = rep(0, nL),
    mu_j_epidemic_factor = rep(2, nL), epidemic_threshold = rep(1e-5, nL),
    # dual-mode alpha_1 — default to per-location vector
    alpha_1 = rep(0.27, nL)
  )
}

test_that("validate_sampled_config accepts a PER-LOCATION (length-nL) alpha_1", {
  cfg <- .minimal_valid_config(2L)
  expect_true(validate_sampled_config(cfg, verbose = FALSE))
})

test_that("validate_sampled_config accepts a SCALAR alpha_1 (back-compat broadcast)", {
  cfg <- .minimal_valid_config(2L)
  cfg$alpha_1 <- 0.27          # scalar in a multi-location config must still pass
  expect_true(validate_sampled_config(cfg, verbose = FALSE))
})

test_that("validate_sampled_config rejects a wrong-length alpha_1", {
  cfg <- .minimal_valid_config(2L)
  cfg$alpha_1 <- c(0.27, 0.27, 0.27)   # length 3 in a 2-location config
  expect_false(validate_sampled_config(cfg, verbose = FALSE))
})

test_that("validate_sampled_config rejects a non-finite alpha_1 value", {
  cfg <- .minimal_valid_config(2L)
  cfg$alpha_1 <- c(0.27, NA_real_)
  expect_false(validate_sampled_config(cfg, verbose = FALSE))
})
