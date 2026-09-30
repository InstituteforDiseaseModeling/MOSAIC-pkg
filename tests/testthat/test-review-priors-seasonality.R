# Regression tests (deep review, priors group): seasonal envelope positivity
# and calendar phase.

test_that(".seasonal_envelope_scale keeps min(1 + f) at the floor and preserves shape", {
     # NAM-like coefficients from priors_default v16.1: min(1 + f) = -8.5
     cf <- c(a_1 = 2.32, b_1 = 5.98, a_2 = 2.35, b_2 = 4.45)
     s <- MOSAIC:::.seasonal_envelope_scale(cf, floor = 0.1)
     expect_lt(s, 1)
     expect_equal(1 + MOSAIC:::.seasonal_envelope_min(cf * s), 0.1, tolerance = 1e-12)
     # Phase unchanged: the day of the peak is the same after scaling
     t <- 1:365
     f <- function(c) c[["a_1"]] * cos(2 * pi * t / 365) + c[["b_1"]] * sin(2 * pi * t / 365) +
          c[["a_2"]] * cos(4 * pi * t / 365) + c[["b_2"]] * sin(4 * pi * t / 365)
     expect_equal(which.max(f(cf)), which.max(f(cf * s)))
     # An envelope that already clears the floor is left alone
     expect_equal(MOSAIC:::.seasonal_envelope_scale(c(a_1 = 0.3, b_1 = 0, a_2 = 0, b_2 = 0)), 1)
})

test_that("the human-transmission envelope is evaluated at calendar day-of-year", {
     # Before v0.99.11 tick 1 was always t = 1 (1 January) whatever date_start
     # was, so a 1 July start forced the season 181 days out of phase.
     par <- list(nticks = 3L, npatches = 1L, p = 365, beta_j0_hum = 1,
                 a_1_j = 1, b_1_j = 0, a_2_j = 0, b_2_j = 0, season_t0 = 181L)
     expect_equal(as.vector(sim_beta_jt_human(par)), 1 + cos(2 * pi * (182:184) / 365))
     # A 1 January start (season_t0 = 0) is unchanged
     par$season_t0 <- 0L
     expect_equal(as.vector(sim_beta_jt_human(par)), 1 + cos(2 * pi * (1:3) / 365))
})

test_that("sim_params sets season_t0 from date_start", {
     cfg <- MOSAIC::config_default
     par <- sim_params(cfg, components = SIM_PIPELINE)
     expect_identical(par$season_t0, as.integer(format(as.Date(cfg$date_start), "%j")) - 1L)
})
