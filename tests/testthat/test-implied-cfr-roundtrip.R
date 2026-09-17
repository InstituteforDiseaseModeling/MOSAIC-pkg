# =============================================================================
# MOSAIC:::.mosaic_add_implied_cfr_columns() must be the exact inverse of sample_parameters()'s
# mu_j_baseline derivation.
#
# sample_parameters.R builds mu from a target CFR as
#     mu = CFR_target * (1 - exp(-gamma_1)) * rho / (rho_deaths * chi)
# so recovering the CFR requires dividing by the incidence-dwell factor as well
# as the reporting chain. Until v0.88.0 the dwell factor was missing here -- the
# pre-v0.14.0 identity, from before laser-cholera #67 made reported_cases an
# incidence flow. It understated the surveillance CFR by 1/(1 - exp(-gamma_1)):
# 10.5x at the shipped gamma_1 = 0.1.
#
# A round trip is the right test because it cannot drift: it pins the two
# functions to each other rather than to a constant either could be wrong about.
# =============================================================================

test_that("deriving mu from a CFR and inverting it recovers the CFR", {
     skip_if_not_installed("testthat")

     CFR <- 0.02; rho <- 0.10; rhod <- 0.30; chi <- 0.25; g1 <- 0.10
     dwell <- 1 - exp(-g1)

     # Forward: exactly sample_parameters.R's chain.
     mu <- CFR * dwell * rho / (rhod * chi)

     res <- data.frame(
          rho = rho, rho_deaths = rhod,
          chi_endemic = chi, chi_epidemic = chi,
          gamma_1 = g1,
          mu_j_baseline_TST = mu,
          mu_j_epidemic_factor_TST = 0
     )
     out <- MOSAIC:::.mosaic_add_implied_cfr_columns(res, iso_codes = "TST", verbose = FALSE)

     expect_true("cfr_baseline_TST" %in% names(out))
     expect_equal(out$cfr_baseline_TST, CFR, tolerance = 1e-12)
})

test_that("the inverse holds across gamma_1, where the old bug scaled worst", {
     for (g1 in c(0.05, 0.10, 0.20, 0.50)) {
          CFR <- 0.015; rho <- 0.2; rhod <- 0.4; chi <- 0.3
          dwell <- 1 - exp(-g1)
          mu <- CFR * dwell * rho / (rhod * chi)
          res <- data.frame(
               rho = rho, rho_deaths = rhod, chi_endemic = chi, chi_epidemic = chi,
               gamma_1 = g1, mu_j_baseline_TST = mu, mu_j_epidemic_factor_TST = 0
          )
          out <- MOSAIC:::.mosaic_add_implied_cfr_columns(res, iso_codes = "TST", verbose = FALSE)
          expect_equal(out$cfr_baseline_TST, CFR, tolerance = 1e-12,
                       info = sprintf("gamma_1 = %.2f", g1))
     }
})

test_that("the epidemic CFR carries the (1 + eps) escalation", {
     CFR <- 0.02; rho <- 0.1; rhod <- 0.3; chi <- 0.25; g1 <- 0.1; eps <- 0.5
     dwell <- 1 - exp(-g1)
     mu <- CFR * dwell * rho / (rhod * chi)
     res <- data.frame(
          rho = rho, rho_deaths = rhod, chi_endemic = chi, chi_epidemic = chi,
          gamma_1 = g1, mu_j_baseline_TST = mu, mu_j_epidemic_factor_TST = eps
     )
     out <- MOSAIC:::.mosaic_add_implied_cfr_columns(res, iso_codes = "TST", verbose = FALSE)
     expect_equal(out$cfr_epidemic_TST, CFR * (1 + eps), tolerance = 1e-12)
     expect_gt(out$cfr_epidemic_TST, out$cfr_baseline_TST)
})

test_that("surveillance CFR is omitted, not wrong, when gamma_1 is absent", {
     # It cannot be computed without gamma_1. Emitting the dwell-free value
     # would reintroduce the v0.88.0 bug silently.
     res <- data.frame(
          rho = 0.1, rho_deaths = 0.3, chi_endemic = 0.25, chi_epidemic = 0.25,
          mu_j_baseline_TST = 1e-3, mu_j_epidemic_factor_TST = 0
     )
     out <- MOSAIC:::.mosaic_add_implied_cfr_columns(res, iso_codes = "TST", verbose = FALSE)
     expect_false("cfr_baseline_TST" %in% names(out))
     expect_false("cfr_epidemic_TST" %in% names(out))
})
