# =============================================================================
# p_fatal indexing across engine, integrated deaths likelihood and redraw
# (deep review tracker item).
#
# The engine reads p_fatal_jt at state row `here` (tick t) and writes the fatal
# onsets it draws to row t + 1. The question was whether that is a one-day
# offset relative to the likelihood / post-hoc redraw, which use mu_jt at the
# recorded onset column. It is not: TRIM_FIRST channels hold end-of-tick state,
# so row t + 1 is results column t + 1 of new_symptomatic, and row `here` of
# p_fatal_jt is mu_jt column t + 1. This file pins that with an on/off mu_jt
# pattern, where a one-column misalignment anywhere would put deaths on a
# column the other side says has zero CFR.
# =============================================================================

.alt_cfg <- function(lc) {
  cfg <- MOSAIC::config_simulation_epidemic
  nT <- ncol(cfg$mu_jt)
  cfg$mu_jt[] <- 0
  cfg$mu_jt[, seq(1L, nT, by = 2L)] <- 0.3     # CFR only on odd days
  cfg$rho_deaths <- 1
  cfg$delta_reporting_cases <- lc
  cfg
}

test_that("engine fatal onsets use mu_jt at their own new_symptomatic column", {
  cfg <- .alt_cfg(0L)
  r <- MOSAIC::run_simulation(cfg, seed = 5L, quiet = TRUE)$results
  nT <- ncol(r$new_symptomatic)
  c_on <- seq_len(nT - 1L)
  # Onsets of column c are recorded in disease_deaths column c + 1.
  fated <- r$disease_deaths[, c_on + 1L, drop = FALSE]
  mu <- cfg$mu_jt[, c_on, drop = FALSE]
  expect_gt(sum(fated), 0)
  expect_true(all(fated[mu == 0] == 0))
  expect_true(all(fated <= r$new_symptomatic[, c_on, drop = FALSE]))
})

test_that("the likelihood's exposure puts every reported death on a positive-CFR onset column", {
  for (lc in c(0L, 2L)) {
    cfg <- .alt_cfg(lc)
    r <- MOSAIC::run_simulation(cfg, seed = 5L, quiet = TRUE)$results
    nT <- ncol(r$new_symptomatic)
    di <- list(n_time = nT,
               base_logit_full = stats::qlogis(pmin(pmax(cfg$mu_jt, 1e-12), 1 - 1e-12)),
               dates_full = as.Date(cfg$date_start) + seq_len(nT) - 1L)
    ex <- MOSAIC:::.mosaic_deaths_exposure(di, r$new_symptomatic, cfg)
    has_d <- r$reported_deaths > 0
    expect_gt(sum(has_d), 0, label = paste("reported deaths, lc", lc))
    expect_true(all(stats::plogis(ex$eta[has_d]) > 0.1), info = paste("lc", lc))
    expect_true(all(ex$X[has_d] > 0), info = paste("lc", lc))
    # With rho_deaths = 1 the reported deaths equal the fatal onsets the exposure
    # column points at, so they never exceed the onsets it scales.
    expect_true(all(r$reported_deaths <= ex$X / (cfg$rho / cfg$chi_epidemic) + 1e-9),
                info = paste("lc", lc))
  }
})
