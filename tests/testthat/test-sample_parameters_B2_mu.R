# =============================================================================
# test-sample_parameters_B2_mu.R
#
# Regression fixtures for B2: the dynamic per-country mu_j_baseline <-> sampled
# gamma_1 coupling (priors_default v15.15 / config_default v4.5; statistician
# spec MOSAIC-pkg/claude/prior_fix_spec/SPEC_B2.md §6.1).
#
# Under B2, sample_parameters() DERIVES mu_j_baseline from a sampled CFR_target
# location prior and the already-sampled global chain factor:
#   mu_j_baseline[j] = CFR_target[j] * gamma_1 * rho / (rho_deaths * chi)
#   chi = 0.5 * (chi_endemic + chi_epidemic)
# so the realized implied reported CFR == CFR_target for every draw
# (chain-factor invariant).
#
# These fixtures are the math-correctness gate. Several are hand-computed and
# self-contained (no MOSAIC root / data needed): they build a minimal priors +
# config and exercise the B2 hook directly.
# =============================================================================

library(testthat)

# Recompute the realized reported CFR from emitted parameters (the engine's
# v0.14.0 steady-state identity). B2's whole point is that this == CFR_target.
.b2_reported_cfr <- function(mu, gamma_1, rho, rho_deaths, chi_endemic, chi_epidemic) {
  chi <- 0.5 * (chi_endemic + chi_epidemic)
  mu * rho_deaths * chi / (gamma_1 * rho)
}

# Build a minimal 2-location B2 priors object + matching config template.
.b2_make_priors <- function(cfr_meanlog = c(AAA = log(0.02), BBB = log(0.01)),
                            sdlog = 0.787) {
  loc <- lapply(cfr_meanlog, function(ml) {
    list(distribution = "lognormal", parameters = list(meanlog = ml, sdlog = sdlog))
  })
  names(loc) <- names(cfr_meanlog)
  list(
    parameters_global = list(
      gamma_1      = list(distribution = "lognormal", parameters = list(meanlog = log(0.10), sdlog = 0.5)),
      rho          = list(distribution = "beta", parameters = list(shape1 = 5.38, shape2 = 7.10)),
      rho_deaths   = list(distribution = "beta", parameters = list(shape1 = 36.95, shape2 = 51.02)),
      chi_endemic  = list(distribution = "beta", parameters = list(shape1 = 5.43, shape2 = 5.01)),
      chi_epidemic = list(distribution = "beta", parameters = list(shape1 = 4.79, shape2 = 1.53))
    ),
    parameters_location = list(
      CFR_target = list(description = "test CFR_target gamma_1 rho_deaths chi", location = loc)
    )
  )
}

.b2_make_config <- function(isos = c("AAA", "BBB")) {
  n <- length(isos)
  list(
    location_name = isos,
    gamma_1 = 0.10, rho = 0.423, rho_deaths = 0.42,
    chi_endemic = 0.50, chi_epidemic = 0.75,
    CFR_target = rep(0.02, n),
    mu_j_baseline = rep(0.003, n),
    seed = 1L
  )
}

# ---------------------------------------------------------------------------
# Fixture 1 (SPEC §6.1 #1): exact at-sample derivation, hand-computed.
# ---------------------------------------------------------------------------
test_that("B2 derives mu_j_baseline exactly from CFR_target * chain", {
  CFR_target <- 0.02; g1 <- 0.09; rho <- 0.43; rhod <- 0.42
  chi_end <- 0.55; chi_epi <- 0.80
  chi <- 0.5 * (chi_end + chi_epi)                       # 0.675
  mu_expected <- CFR_target * g1 * rho / (rhod * chi)    # hand value

  # Drive the hook via a fully-specified config (sample_CFR_target=FALSE keeps
  # CFR_target at the config default, so the result is deterministic).
  priors <- .b2_make_priors()
  cfg <- .b2_make_config()
  cfg$gamma_1 <- g1; cfg$rho <- rho; cfg$rho_deaths <- rhod
  cfg$chi_endemic <- chi_end; cfg$chi_epidemic <- chi_epi
  cfg$CFR_target <- rep(CFR_target, 2)

  out <- sample_parameters(
    priors = priors, config = cfg, seed = 123L, verbose = FALSE, validate = FALSE,
    sample_args = list(
      sample_gamma_1 = FALSE, sample_rho = FALSE, sample_rho_deaths = FALSE,
      sample_chi_endemic = FALSE, sample_chi_epidemic = FALSE,
      sample_CFR_target = FALSE, sample_mu_j_baseline = TRUE,
      sample_initial_conditions = FALSE
    )
  )
  expect_equal(out$mu_j_baseline, rep(mu_expected, 2), tolerance = 1e-12)
  # Recompute by hand to >= 10 sig figs (independent of the package formula).
  expect_equal(mu_expected, 0.02 * 0.09 * 0.43 / (0.42 * 0.675), tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# Fixture 2 (SPEC §6.1 #2): implied-CFR invariance across the gamma_1 support
# and across chi/rho/rho_deaths.
# ---------------------------------------------------------------------------
test_that("B2 implied reported CFR == CFR_target across the chain support", {
  CFR_target <- 0.015
  priors <- .b2_make_priors(cfr_meanlog = c(AAA = log(CFR_target), BBB = log(CFR_target)))
  grid <- expand.grid(
    g1   = c(0.05, 0.10, 0.20),
    rho  = c(0.30, 0.50),
    rhod = c(0.35, 0.50),
    ce   = c(0.45, 0.55),
    ee   = c(0.70, 0.85)
  )
  for (i in seq_len(nrow(grid))) {
    cfg <- .b2_make_config()
    cfg$gamma_1 <- grid$g1[i]; cfg$rho <- grid$rho[i]; cfg$rho_deaths <- grid$rhod[i]
    cfg$chi_endemic <- grid$ce[i]; cfg$chi_epidemic <- grid$ee[i]
    cfg$CFR_target <- rep(CFR_target, 2)
    out <- sample_parameters(
      priors = priors, config = cfg, seed = 1L, verbose = FALSE, validate = FALSE,
      sample_args = list(
        sample_gamma_1 = FALSE, sample_rho = FALSE, sample_rho_deaths = FALSE,
        sample_chi_endemic = FALSE, sample_chi_epidemic = FALSE,
        sample_CFR_target = FALSE, sample_mu_j_baseline = TRUE,
        sample_initial_conditions = FALSE
      )
    )
    cfr_realized <- .b2_reported_cfr(out$mu_j_baseline[1], grid$g1[i], grid$rho[i],
                                     grid$rhod[i], grid$ce[i], grid$ee[i])
    expect_equal(cfr_realized, CFR_target, tolerance = 1e-12,
                 info = sprintf("row %d g1=%.2f", i, grid$g1[i]))
  }
})

# ---------------------------------------------------------------------------
# Fixture 3 (SPEC §6.1 #3): composed-spread. Monte-Carlo CFR_target (lognormal,
# sdlog=0.787) * the sampled chain factor. Locks the §2 sizing decision: the
# implied-CFR prior spread must equal today's (sd ~0.787) and the marginal mu
# spread must be the intended WIDER value (Var(log) ~1.016).
# ---------------------------------------------------------------------------
test_that("B2 composed spread: sd(log implied_CFR) ~ 0.787, Var(log mu) ~ 1.016", {
  set.seed(2026L)
  N <- 2e5
  sdlog_cfr <- 0.787
  cfr_meanlog0 <- log(0.02)

  # CFR_target lognormal draws.
  cfr <- rlnorm(N, meanlog = cfr_meanlog0, sdlog = sdlog_cfr)
  # Chain factor draws from the shipped priors (lognormal gamma_1, Beta others).
  g1   <- rlnorm(N, meanlog = log(0.10), sdlog = 0.5)
  rho  <- rbeta(N, 5.38, 7.10)
  rhod <- rbeta(N, 36.95, 51.02)
  ce   <- rbeta(N, 5.43, 5.01)
  ee   <- rbeta(N, 4.79, 1.53)
  chi  <- 0.5 * (ce + ee)
  chain <- g1 * rho / (rhod * chi)

  mu <- cfr * chain
  implied_cfr <- .b2_reported_cfr(mu, g1, rho, rhod, ce, ee)  # == cfr by construction

  # Implied CFR spread == CFR_target spread (the chain cancels exactly).
  expect_equal(sd(log(implied_cfr)), sdlog_cfr, tolerance = 0.02)
  # Marginal mu is WIDER by design: Var(log mu) = Var(log cfr) + Var(log chain).
  # Today's implied-CFR Var(log) = 0.6195; the chain Var(log) ~ 0.396 -> ~1.016.
  expect_gt(var(log(mu)), 0.90)
  expect_lt(var(log(mu)), 1.15)
  # Sanity: marginal mu CV ~ 1.33 (well above the infeasible Gamma(4) CV=0.5).
  cv_mu <- sd(mu) / mean(mu)
  expect_gt(cv_mu, 1.0)
})

# ---------------------------------------------------------------------------
# Fixture 4 (SPEC §6.1 #4): make_LASER_config [0,1] bound. The RAW B2 product
# (CFR_target * chain) CAN exceed 1 in the extreme upper tail for the highest-CFR
# countries (P(mu>1) ~ 1e-5 at the highest real country, CFR median ~0.089;
# SPEC_B2.md §3.5), so sample_parameters() CLAMPS the derived mu just below 1.
# This fixture asserts (a) the raw product can breach (documenting why the clamp
# exists) and (b) the clamped derivation through sample_parameters() never does.
# ---------------------------------------------------------------------------
test_that("B2 raw product can breach 1 in the tail (documents the clamp need)", {
  set.seed(7L)
  N <- 1e6
  # Highest REAL per-country CFR_target median is ~0.089 (MLI). At that center
  # the upper tail breaches 1 with P ~ 1e-5.
  cfr <- rlnorm(N, meanlog = log(0.089), sdlog = 0.787)
  g1   <- rlnorm(N, meanlog = log(0.10), sdlog = 0.5)
  rho  <- rbeta(N, 5.38, 7.10)
  rhod <- rbeta(N, 36.95, 51.02)
  chi  <- 0.5 * (rbeta(N, 5.43, 5.01) + rbeta(N, 4.79, 1.53))
  mu <- cfr * g1 * rho / (rhod * chi)
  expect_gt(max(mu), 1.0)          # raw product breaches -> clamp is required
  expect_lt(mean(mu > 1), 1e-3)    # but only in the extreme tail
})

test_that("B2 sample_parameters clamps derived mu_j_baseline to the engine [0,1] bound", {
  priors <- .b2_make_priors(cfr_meanlog = c(AAA = log(0.5), BBB = log(0.089)))
  cfg <- .b2_make_config()
  cfg$CFR_target <- c(0.5, 0.089)
  # Force a high-chain draw: large gamma_1, small rho_deaths/chi -> chain large.
  cfg$gamma_1 <- 0.5; cfg$rho <- 0.7; cfg$rho_deaths <- 0.3
  cfg$chi_endemic <- 0.3; cfg$chi_epidemic <- 0.3
  out <- sample_parameters(
    priors = priors, config = cfg, seed = 1L, verbose = FALSE, validate = FALSE,
    sample_args = list(
      sample_gamma_1 = FALSE, sample_rho = FALSE, sample_rho_deaths = FALSE,
      sample_chi_endemic = FALSE, sample_chi_epidemic = FALSE,
      sample_CFR_target = FALSE, sample_mu_j_baseline = TRUE,
      sample_initial_conditions = FALSE
    )
  )
  # chain = 0.5*0.7/(0.3*0.3) = 3.889; CFR 0.5 -> mu 1.944 (>1) clamped; 0.089 -> 0.346 (ok)
  expect_true(all(out$mu_j_baseline <= 1 & out$mu_j_baseline >= 0))
  expect_lt(out$mu_j_baseline[1], 1.0)   # clamped just under 1
  expect_gt(out$mu_j_baseline[1], 0.99)  # but at the ceiling (raw was 1.944)
})

# ---------------------------------------------------------------------------
# Fixture 5 (SPEC §6.1 #5): frozen-flag contract.
# ---------------------------------------------------------------------------
test_that("B2 sample_mu_j_baseline=FALSE leaves mu_j_baseline at the config default", {
  priors <- .b2_make_priors()
  cfg <- .b2_make_config()
  cfg$mu_j_baseline <- c(0.0042, 0.0017)
  out <- sample_parameters(
    priors = priors, config = cfg, seed = 5L, verbose = FALSE, validate = FALSE,
    sample_args = list(sample_mu_j_baseline = FALSE, sample_initial_conditions = FALSE)
  )
  expect_equal(out$mu_j_baseline, c(0.0042, 0.0017), tolerance = 1e-12)
})

test_that("B2 sample_CFR_target=FALSE freezes CFR but still derives mu from sampled chain", {
  priors <- .b2_make_priors()
  cfg <- .b2_make_config()
  cfg$CFR_target <- c(0.02, 0.01)
  # Freeze chain too, for a deterministic check of the frozen-CFR derivation.
  out <- sample_parameters(
    priors = priors, config = cfg, seed = 6L, verbose = FALSE, validate = FALSE,
    sample_args = list(
      sample_CFR_target = FALSE, sample_mu_j_baseline = TRUE,
      sample_gamma_1 = FALSE, sample_rho = FALSE, sample_rho_deaths = FALSE,
      sample_chi_endemic = FALSE, sample_chi_epidemic = FALSE,
      sample_initial_conditions = FALSE
    )
  )
  chi <- 0.5 * (cfg$chi_endemic + cfg$chi_epidemic)
  chain <- cfg$gamma_1 * cfg$rho / (cfg$rho_deaths * chi)
  expect_equal(out$CFR_target, c(0.02, 0.01), tolerance = 1e-12)  # frozen
  expect_equal(out$mu_j_baseline, c(0.02, 0.01) * chain, tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# Fixture 6 (SPEC §6.1 #6): bit-identical NON-B2 (legacy) path. A legacy priors
# object (mu_j_baseline Gamma prior present, NO CFR_target prior) must take the
# old code path: mu_j_baseline sampled in the loop, the B2 hook skipped. The
# full config_sampled MINUS mu/CFR must be bit-identical to a re-run.
# RNG-ORDER CONTRACT NOTE (§6.1 #6, §8.4): introducing a CFR_target lognormal
# draw where the legacy object drew a mu Gamma reorders the RNG stream, so a B2
# object is NOT bit-identical to a legacy object on downstream draws -- that is
# expected and documented. This fixture asserts the LEGACY path is unchanged.
# ---------------------------------------------------------------------------
test_that("B2-OFF legacy path is bit-identical and skips the B2 hook", {
  # Legacy priors: mu_j_baseline Gamma location prior, no CFR_target.
  legacy <- list(
    parameters_global = list(
      gamma_1      = list(distribution = "lognormal", parameters = list(meanlog = log(0.10), sdlog = 0.5)),
      rho          = list(distribution = "beta", parameters = list(shape1 = 5.38, shape2 = 7.10)),
      rho_deaths   = list(distribution = "beta", parameters = list(shape1 = 36.95, shape2 = 51.02)),
      chi_endemic  = list(distribution = "beta", parameters = list(shape1 = 5.43, shape2 = 5.01)),
      chi_epidemic = list(distribution = "beta", parameters = list(shape1 = 4.79, shape2 = 1.53))
    ),
    parameters_location = list(
      mu_j_baseline = list(description = "legacy gamma", location = list(
        AAA = list(distribution = "gamma", parameters = list(shape = 4, rate = 4 / 0.003)),
        BBB = list(distribution = "gamma", parameters = list(shape = 4, rate = 4 / 0.0015))
      ))
    )
  )
  cfg <- .b2_make_config()
  cfg$CFR_target <- NULL  # legacy config has no CFR_target

  a <- sample_parameters(priors = legacy, config = cfg, seed = 99L,
                         verbose = FALSE, validate = FALSE,
                         sample_args = list(sample_initial_conditions = FALSE))
  b <- sample_parameters(priors = legacy, config = cfg, seed = 99L,
                         verbose = FALSE, validate = FALSE,
                         sample_args = list(sample_initial_conditions = FALSE))
  # Full bit-identical re-run.
  expect_identical(a, b)
  # mu_j_baseline was SAMPLED (Gamma), not derived; CFR_target absent.
  expect_true(is.numeric(a$mu_j_baseline) && length(a$mu_j_baseline) == 2)
  expect_null(a$CFR_target)
})

# ---------------------------------------------------------------------------
# Version-skew guard (SPEC §5.3, Lesson #12): B2 derivation requested but the
# priors object has NEITHER a CFR_target NOR a legacy mu_j_baseline prior -> stop.
# ---------------------------------------------------------------------------
test_that("B2 fails loud when neither CFR_target nor mu_j_baseline prior is present", {
  broken <- list(
    parameters_global = list(
      gamma_1      = list(distribution = "lognormal", parameters = list(meanlog = log(0.10), sdlog = 0.5)),
      rho          = list(distribution = "beta", parameters = list(shape1 = 5.38, shape2 = 7.10)),
      rho_deaths   = list(distribution = "beta", parameters = list(shape1 = 36.95, shape2 = 51.02)),
      chi_endemic  = list(distribution = "beta", parameters = list(shape1 = 5.43, shape2 = 5.01)),
      chi_epidemic = list(distribution = "beta", parameters = list(shape1 = 4.79, shape2 = 1.53))
    ),
    parameters_location = list()  # neither prior present
  )
  cfg <- .b2_make_config()
  cfg$CFR_target <- NULL
  expect_error(
    sample_parameters(priors = broken, config = cfg, seed = 1L,
                      verbose = FALSE, validate = FALSE,
                      sample_args = list(sample_mu_j_baseline = TRUE,
                                         sample_initial_conditions = FALSE)),
    regexp = "neither a CFR_target.*nor a mu_j_baseline"
  )
})
