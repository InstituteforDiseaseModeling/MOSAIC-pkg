# =============================================================================
# The environmental dose-response is PER-CAPITA (v0.89.0).
#
# The reservoir update accumulates ABSOLUTE cells summed over everyone shedding,
# so W is extensive. kappa is a CONCENTRATION -- 04-model-description.Rmd
# defines it as "the V. cholerae concentration at which the per-contact
# probability of infection is 50%", fitted to volunteer studies reporting CFU
# concentrations. Comparing an extensive stock to an intensive constant pins the
# dose-response at 1 and makes kappa, zeta_1, zeta_2, zeta_ratio and the decay
# parameters flat directions.
#
# The spec flags the mismatch itself ("the W-vs-kappa scale matching is an open
# methodological question"), so dividing by N resolves a documented question
# rather than contradicting the model description.
#
# rng mode divides by N; replay keeps the oracle's raw W so the Tier B fixtures
# stay a valid port-parity harness. This file is therefore the ONLY coverage of
# the production form -- CLAUDE.md lesson #18(v).
# =============================================================================

test_that("the correction is registered where replay's blind spots are listed", {
  expect_true("envtohuman/dose_percapita" %in% MOSAIC:::.SIM_RNG_ONLY_CORRECTIONS)
  expect_true("infectious/sigma_split"    %in% MOSAIC:::.SIM_RNG_ONLY_CORRECTIONS)
})

test_that("kappa is FIXED by default, not sampled", {
  # It is not identifiable even after the fix -- the fix makes it MEANINGFUL,
  # not estimable from these data -- so the default holds it at its
  # literature-anchored value.
  ctrl <- mosaic_control_defaults()
  expect_false(ctrl$sampling$sample_kappa)
  expect_identical(unique(MOSAIC::config_default$kappa), 1e6)
})

test_that("the per-capita dose-response is density dependent", {
  # The property the raw-W form lacked: the response must MOVE with prevalence.
  kappa <- 1e6; z1 <- 3.29e8; theta <- 0.5; dec <- 1/16; N <- 2e7
  resp <- function(Isym, percap) {
    W <- (1 - theta) * z1 * Isym / dec
    D <- if (percap) W / N else W
    D / (kappa + D)
  }
  isym <- c(1, 10, 100, 1e3, 1e4, 1e5)

  raw <- vapply(isym, resp, numeric(1), percap = FALSE)
  pc  <- vapply(isym, resp, numeric(1), percap = TRUE)

  # Raw W: saturated everywhere, a 100,000x change in prevalence moves it <1%.
  expect_true(all(raw > 0.99))
  expect_lt(max(raw) - min(raw), 0.01)

  # Per capita: spans most of (0, 1) and is strictly increasing.
  expect_lt(min(pc), 0.05)
  expect_gt(max(pc), 0.85)
  expect_true(all(diff(pc) > 0))
})

test_that("kappa = 1e6 half-saturates near a plausible outbreak prevalence", {
  # What kappa now MEANS: the symptomatic prevalence at which environmental
  # transmission is half-saturated. If this lands somewhere implausible the
  # per-capita scaling would be trading one bad anchor for another.
  kappa <- 1e6; z1 <- 3.29e8; theta <- 0.5; dec <- 1/16
  # W/N = kappa  =>  (1-theta)*z1*prev/dec = kappa
  prev <- kappa * dec / ((1 - theta) * z1)
  expect_gt(prev, 1e-4)     # not absurdly low
  expect_lt(prev, 1e-2)     # not absurdly high
})

test_that("a production run stays finite and bounded after the change", {
  skip_if_not(exists("config_simulation_epidemic", asNamespace("MOSAIC")),
              "no packaged simulation config")
  out <- run_simulation(config = MOSAIC::config_simulation_epidemic,
                        seed = 7L, quiet = TRUE)
  expect_true(all(is.finite(out$results$Psi)))
  expect_true(all(out$results$Psi >= 0))
  # Psi is a hazard fed to -expm1(-Psi); a value that pins at 1 every tick is
  # the pathology this change removes.
  expect_true(any(out$results$Psi < 1))
})
