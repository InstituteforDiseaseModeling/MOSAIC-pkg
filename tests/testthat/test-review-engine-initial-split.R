# =============================================================================
# The t=0 symptomatic split of I_j_initial (deep review, engine-04).
#
# v0.89.0 replaced the deterministic round(sigma * progressing) per-tick split
# with a binomial in "rng" mode because round() is wrong in the mean at small
# counts, but left the t=0 split of I_j_initial as round(sigma * I). At the
# default sigma = 0.25 a patch seeded with 1 or 2 infections therefore started
# with no symptomatic at all. Production now draws Binom(I_j_initial, sigma) at
# the rng-only site infectious/sigma_split_t0; replay keeps the oracle's round().
# =============================================================================

.split_cfg <- function(I = rep(2L, 3L), sigma = 0.25) {
  cfg <- MOSAIC::config_simulation_epidemic
  cfg$I_j_initial <- as.integer(I)
  cfg$sigma <- sigma
  cfg
}

.seed_row <- function(par, ctl) {
  st <- MOSAIC:::sim_alloc_state(par$nticks, par$npatches)
  MOSAIC:::sim_seed_state(st, par, ctl)$rows[[1L]]
}

test_that("sigma_split_t0 is a registered rng-only correction, never an oracle site", {
  expect_true("infectious/sigma_split_t0" %in% MOSAIC:::.SIM_DRAW_SITES)
  expect_true("infectious/sigma_split_t0" %in% MOSAIC:::.SIM_RNG_ONLY_SITES)
  expect_true("infectious/sigma_split_t0" %in% MOSAIC:::.SIM_RNG_ONLY_CORRECTIONS)
  expect_false("infectious/sigma_split_t0" %in% unname(MOSAIC:::.SIM_ORACLE_SITE_MAP))
})

test_that("rng mode seeds a symptomatic arm at low counts, unbiased and conserving I", {
  par <- MOSAIC:::sim_params(.split_cfg(I = rep(2L, 3L)))
  isym <- vapply(1:4000, function(s) {
    ctl <- MOSAIC:::sim_draws(mode = "rng", seed = s)
    withr::with_seed(s, {
      r1 <- .seed_row(par, ctl)
      stopifnot(identical(r1$Isym + r1$Iasym, par$I_j_initial))
      sum(r1$Isym)
    })
  }, numeric(1))
  # The deterministic form gives 0 on every run: round(0.25 * 2) = 0.
  expect_identical(as.integer(round(par$sigma * 2L)), 0L)
  # E[Isym] = 3 patches * 2 * 0.25 = 1.5; SE of the mean ~ 0.013.
  expect_equal(mean(isym), 1.5, tolerance = 0.05)
  expect_gt(mean(isym > 0), 0.5)
})

test_that("replay mode and a NULL controller keep the oracle's round()", {
  par <- MOSAIC:::sim_params(.split_cfg(I = c(101L, 7L, 2L), sigma = 0.5))
  r1 <- .seed_row(par, NULL)
  expect_identical(r1$Isym, c(50L, 4L, 1L))        # round-half-to-even
  ctl <- list(mode = "replay")
  expect_identical(.seed_row(par, ctl)$Isym, c(50L, 4L, 1L))
})

test_that("a full rng run draws the t=0 split exactly once", {
  out <- MOSAIC::run_simulation(.split_cfg(), seed = 7L, quiet = TRUE)
  cov <- attr(out, "sim_coverage")
  expect_identical(cov$n_calls[cov$site == "infectious/sigma_split_t0"], 1L)
})
