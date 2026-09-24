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
#
# CAVEAT, and the reason for the chi-asymmetric block at the bottom of this file:
# every fixture above sets chi_endemic == chi_epidemic, which collapses the only
# axis on which the two functions currently DISAGREE. Production ships
# chi_endemic = 0.50 and chi_epidemic = 0.75 (inst/extdata/config_default.json),
# so the round trip does NOT close on the endemic column in production. See the
# "chi_endemic != chi_epidemic" tests below, which pin the discrepancy so it
# cannot be changed silently.
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

# =============================================================================
# chi_endemic != chi_epidemic -- the axis every fixture above collapses.
#
# !! THESE TESTS DOCUMENT A KNOWN DEFECT. THEY ARE EXPECTED TO FAIL AT R4/R5 !!
#
# The sample-time derivation (R/sample_parameters.R:670, "B2.1") hardcodes
# chi_epidemic:
#     mu = CFR_target * (1 - exp(-gamma_1)) * rho / (rho_deaths * chi_epidemic)
# while the engine switches per tick between chi_endemic and chi_epidemic
# (R/sim_components.R). Inverting with chi_endemic therefore does NOT return
# CFR_target; it returns CFR_target * (chi_endemic / chi_epidemic).
#
# At the shipped config (chi_endemic = 0.50, chi_epidemic = 0.75) that factor is
# 2/3 = 0.6667, i.e. the endemic-regime implied CFR sits 33.3% BELOW the target
# the prior was centred on. The CFR review concluded the derivation should use
# the EFFECTIVE (per-tick) chi instead, which is plan step R4/R5 of
# claude/cfr_review/PLAN.md.
#
# The assertions below pin the CURRENT, WRONG ratio on purpose. When R4/R5
# switches the derivation to the effective chi, `cfr_baseline_TST` will become
# equal to CFR_target and the 0.6667 expectation will FAIL LOUDLY. That failure
# is the signal that R4/R5 landed -- update the expectation to `CFR` (and delete
# this block's caveat) at that point, do not widen the tolerance.
# =============================================================================

test_that("chi_endemic != chi_epidemic: the endemic column is off by chi_end/chi_epi (R4/R5 will change this)", {
     # Shipped production values, inst/extdata/config_default.json.
     CFR <- 0.02; rho <- 0.423; rhod <- 0.42; g1 <- 0.10
     chi_end <- 0.50; chi_epi <- 0.75
     dwell <- 1 - exp(-g1)

     # Forward: exactly sample_parameters.R B2.1 -- note it uses chi_epidemic ONLY.
     mu <- CFR * dwell * rho / (rhod * chi_epi)

     res <- data.frame(
          rho = rho, rho_deaths = rhod,
          chi_endemic = chi_end, chi_epidemic = chi_epi,
          gamma_1 = g1,
          mu_j_baseline_TST = mu,
          mu_j_epidemic_factor_TST = 0
     )
     out <- MOSAIC:::.mosaic_add_implied_cfr_columns(res, iso_codes = "TST", verbose = FALSE)

     # (a) The epidemic column DOES close the round trip, because the derivation
     #     and the inverse both use chi_epidemic. This half must never break.
     expect_equal(out$cfr_epidemic_TST, CFR, tolerance = 1e-12,
                  info = "epidemic column is the one the B2.1 derivation is the inverse of")

     # (b) The endemic column does NOT. It is low by exactly chi_end/chi_epi.
     #     CURRENT BEHAVIOUR, KNOWN WRONG -- see the block comment. R4/R5 fixes it.
     expect_equal(out$cfr_baseline_TST, CFR * (chi_end / chi_epi), tolerance = 1e-12,
                  info = "PLAN.md R4/R5 will make this equal CFR; this failing is the intended signal")
     expect_equal(out$cfr_baseline_TST / CFR, 2 / 3, tolerance = 1e-9,
                  info = "measured discrepancy at the shipped chi pair: 0.6667x on endemic ticks")

     # (c) And therefore the two columns are NOT equal even at eps = 0, which is
     #     the fact the chi_endemic == chi_epidemic fixtures above cannot see.
     expect_false(isTRUE(all.equal(out$cfr_baseline_TST, out$cfr_epidemic_TST)))
})

test_that("chi_endemic != chi_epidemic: the chi_end/chi_epi factor scales as advertised", {
     # The discrepancy is a pure ratio, so it must track chi_endemic exactly.
     # If a future change makes the inverse use an effective/blended chi, the
     # ratio stops being chi_end/chi_epi and this loop fails at every point.
     CFR <- 0.015; rho <- 0.2; rhod <- 0.4; g1 <- 0.10; chi_epi <- 0.75
     dwell <- 1 - exp(-g1)
     mu <- CFR * dwell * rho / (rhod * chi_epi)

     for (chi_end in c(0.25, 0.50, 0.60, 0.75)) {
          res <- data.frame(
               rho = rho, rho_deaths = rhod,
               chi_endemic = chi_end, chi_epidemic = chi_epi,
               gamma_1 = g1, mu_j_baseline_TST = mu, mu_j_epidemic_factor_TST = 0
          )
          out <- MOSAIC:::.mosaic_add_implied_cfr_columns(res, iso_codes = "TST", verbose = FALSE)
          expect_equal(out$cfr_baseline_TST / CFR, chi_end / chi_epi, tolerance = 1e-9,
                       info = sprintf("chi_endemic = %.2f (CURRENT behaviour; R4/R5 makes this 1.0)", chi_end))
     }
})
