# =============================================================================
# test-sim-mortality-onset.R
#
# The v0.96.0 mortality model in the engine ("rng" mode): each symptomatic onset
# is fatal with probability p = mu_jt * rho / (rho_deaths * chi_epidemic), drawn
# at onset (draw site infectious/fatal_onsets), and fatal onsets never enter
# Isym. Fatal onsets are recorded in the row of the onsets that produced them,
# and reported deaths are read from that row on the case lag, so a death is
# reported in the same tick as its case.
#
# Result-column bookkeeping this file pins (new_symptomatic is TRIM_FIRST,
# disease_deaths / reported_deaths / births are TRIM_LAST, see sim_results.R):
#   disease_deaths[, c + 1] <= new_symptomatic[, c]          (fatal onsets of column c)
#   reported_deaths[, c]  == disease_deaths[, c - lc]         (rho_deaths = 1)
#   N[, c] - N[, c - 1]   == births[, c] - ndd[, c] - disease_deaths[, c + 1]
# Replay mode keeps the oracle's hazard and is covered by test-sim_engine_replay.R.
# =============================================================================

.onset_cfg <- function(mu = 0.2, ...) {
  cfg <- MOSAIC::config_simulation_epidemic
  cfg$mu_jt[] <- mu
  mods <- list(...)
  for (nm in names(mods)) cfg[[nm]] <- mods[[nm]]
  cfg
}
.run <- function(cfg, seed = 3L) MOSAIC::run_simulation(cfg, seed = seed, quiet = TRUE)$results

test_that("mu_jt = 0 gives no deaths and every onset enters Isym", {
  r <- .run(.onset_cfg(mu = 0))
  expect_gt(sum(r$new_symptomatic), 0)
  expect_true(all(r$disease_deaths == 0))
  expect_true(all(r$reported_deaths == 0))
})

test_that("fatal onsets come from the previous column's onsets and never exceed them", {
  r <- .run(.onset_cfg(mu = 0.3))
  nT <- ncol(r$new_symptomatic)
  expect_gt(sum(r$disease_deaths), 0)
  expect_true(all(r$disease_deaths[, 1] == 0))
  expect_true(all(r$disease_deaths[, 2:nT] <= r$new_symptomatic[, 1:(nT - 1)]))
})

test_that("with rho_deaths = 1, reported deaths are the fatal onsets shifted by the case lag exactly", {
  for (lc in c(0L, 3L)) {
    r <- .run(.onset_cfg(mu = 0.2, rho_deaths = 1, delta_reporting_cases = lc))
    nT <- ncol(r$reported_deaths)
    cols <- (lc + 1L):nT
    expect_identical(r$reported_deaths[, cols], r$disease_deaths[, cols - lc], info = paste("lc", lc))
    if (lc > 0L) expect_true(all(r$reported_deaths[, seq_len(lc)] == 0), info = paste("lc", lc))
  }
})

test_that("the population balances with disease deaths one column after births", {
  r <- .run(.onset_cfg(mu = 0.2))
  nT <- ncol(r$N)
  cc <- 3:(nT - 1L)
  expect_gt(sum(r$disease_deaths), 0)
  expect_identical(r$N[, cc] - r$N[, cc - 1L],
                   r$births[, cc] - r$non_disease_deaths[, cc] - r$disease_deaths[, cc + 1L])
})

test_that("an initial symptomatic stock with no new onsets produces no disease deaths", {
  # Fate is decided at onset, so people already symptomatic at t0 have survived
  # it (evaluation F5). iota = 0 stops E -> I, so there are no onsets at all.
  cfg <- .onset_cfg(mu = 0.2, iota = 0)
  cfg$I_j_initial <- pmax(cfg$I_j_initial, 50000L)
  r <- .run(cfg)
  expect_identical(sum(r$new_symptomatic), 0L)
  expect_identical(sum(r$disease_deaths), 0L)
  expect_identical(sum(r$reported_deaths), 0L)
})

test_that("realized reported CFR matches mu_jt when the case PPV does not switch", {
  # chi_endemic = chi_epidemic removes the regime switch, so E[reported deaths] /
  # E[reported cases] = mu_jt exactly. round(Binom(O, rho) / chi) is biased low
  # at 1-2 onsets per patch-day (evaluation F6), so pool several seeds at a high
  # mu_jt and allow 8%.
  cfg <- .onset_cfg(mu = 0.15, chi_endemic = 0.75, chi_epidemic = 0.75)
  d <- 0; cs <- 0
  for (s in 1:4) { r <- .run(cfg, seed = s); d <- d + sum(r$reported_deaths); cs <- cs + sum(r$reported_cases) }
  expect_gt(d, 500)
  expect_equal(d / cs, 0.15, tolerance = 0.08)
})

test_that("reported CFR is mu_jt at forced epidemic PPV and mu_jt*chi_end/chi_epi at forced endemic PPV", {
  # The conversion p = mu_jt * rho / (rho_deaths * chi_epidemic) must use the
  # EPIDEMIC PPV: fixtures with chi_endemic == chi_epidemic cannot tell (the
  # fixture loosening that hid this axis twice). Threshold 0 forces every tick
  # epidemic; threshold 1 forces every tick endemic.
  base <- .onset_cfg(mu = 0.15, chi_endemic = 0.5, chi_epidemic = 0.75)
  ratio <- function(thr) {
    cfg <- base; cfg$epidemic_threshold <- thr
    d <- 0; cs <- 0
    for (s in 1:4) { r <- .run(cfg, seed = s); d <- d + sum(r$reported_deaths); cs <- cs + sum(r$reported_cases) }
    d / cs
  }
  expect_equal(ratio(0), 0.15, tolerance = 0.08)
  expect_equal(ratio(1), 0.15 * 0.5 / 0.75, tolerance = 0.08)
})

test_that("a step in mu_jt takes effect at the onset tick it is indexed by", {
  cfg <- .onset_cfg(mu = 0)
  nT <- ncol(cfg$mu_jt); D <- 150L
  cfg$mu_jt[, (D + 1L):nT] <- 0.3
  r <- .run(cfg)
  # Onsets in column c use mu_jt day c and are recorded in disease_deaths column c + 1.
  expect_true(all(r$disease_deaths[, seq_len(D + 1L)] == 0))
  expect_gt(sum(r$disease_deaths[, (D + 2L):nT]), 0)
})

test_that("the engine refuses a mu_jt no per-onset probability can produce", {
  cfg <- .onset_cfg(mu = 0.02)
  cfg$mu_jt[1, 10] <- 0.9      # 0.9 * 0.52 / (0.42 * 0.75) > 1
  expect_error(.run(cfg), "no per-onset fatality probability")
  cfg <- .onset_cfg(mu = 0.02, rho_deaths = 0)
  expect_error(.run(cfg), "rho_deaths` is 0")
  cfg <- .onset_cfg(mu = 0.02); cfg$mu_jt[2, 5] <- -0.1
  expect_error(.run(cfg), "must lie in \\[0, 1\\)")
})

test_that("mu_jt accepts a scalar, a per-location vector and a one-location daily vector", {
  base <- .onset_cfg(mu = 0.05)
  a <- base; a$mu_jt <- 0.05
  expect_identical(.run(a), .run(base))
  b <- base; b$mu_jt <- rep(0.05, length(base$location_name))
  expect_identical(.run(b), .run(base))

  one <- MOSAIC::get_location_config(base, iso = base$location_name[1])
  v <- one; v$mu_jt <- as.numeric(one$mu_jt)     # JSON's form of a [1 x nT] matrix
  expect_identical(.run(v), .run(one))
})

test_that("a legacy config uses its CFR_target as a constant mu_jt and ignores its dead mu_jt", {
  new <- .onset_cfg(mu = 0.03)
  leg <- new
  leg$mu_jt <- matrix(0.5, nrow(new$mu_jt), ncol(new$mu_jt))   # never read by any engine
  leg$mu_j_baseline <- rep(0.004, 3); leg$mu_j_epidemic_factor <- rep(0.5, 3)
  leg$CFR_target <- rep(0.03, 3); leg$delta_reporting_deaths <- 5
  rm(list = intersect("legacy_mortality_config", ls(MOSAIC:::.mosaic_once)), envir = MOSAIC:::.mosaic_once)
  expect_warning(r_leg <- .run(leg), "predates the v0.96.0 mortality model")
  expect_identical(r_leg, .run(new))

  bad <- leg; bad$CFR_target <- NULL
  expect_error(.run(bad), "no `CFR_target` to convert")
  pre_ifr <- new; pre_ifr$mu_j <- rep(0.01, 3)
  expect_error(.run(pre_ifr), "`mu_j`.*no `CFR_target`")
})

test_that("the likelihood resolves mu_jt exactly as the engine does", {
  cfg <- .onset_cfg(mu = 0.02)
  cfg$mu_jt[2, ] <- seq(0.01, 0.04, length.out = ncol(cfg$mu_jt))
  nL <- length(cfg$location_name); nT <- ncol(cfg$mu_jt)
  par <- MOSAIC:::sim_params(cfg)
  expect_identical(MOSAIC:::.mosaic_config_mu_jt(cfg, nL, nT), t(par$mu_jt))

  leg <- cfg; leg$mu_j_baseline <- rep(0.004, nL); leg$CFR_target <- c(0.01, 0.02, 0.03)
  suppressWarnings({
    m_eng <- MOSAIC:::sim_params(leg)$mu_jt
    m_lik <- MOSAIC:::.mosaic_config_mu_jt(leg, nL, nT)
  })
  expect_identical(m_lik, t(m_eng))
  expect_equal(m_lik[, 1], c(0.01, 0.02, 0.03))
})

test_that("fatal_onsets is an rng-only draw site and disease_deaths a replay-only one", {
  expect_true("infectious/fatal_onsets" %in% MOSAIC:::.SIM_RNG_ONLY_SITES)
  expect_true("infectious/fatal_onsets" %in% MOSAIC:::.SIM_RNG_ONLY_CORRECTIONS)
  expect_identical(MOSAIC:::.SIM_REPLAY_ONLY_SITES, "infectious/disease_deaths")
  expect_false("infectious/fatal_onsets" %in% names(MOSAIC:::.SIM_ORACLE_SITE_MAP))
})
