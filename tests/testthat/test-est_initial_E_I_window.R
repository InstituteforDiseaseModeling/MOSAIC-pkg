# The E/I surveillance window may straddle t0 (lookahead_days): an outbreak
# under way at the simulation start must seed E/I even when the days just
# before t0 report nothing.

.ei_args <- list(population = 1e6, sigma = 0.35, rho = 0.42, chi = 0.52,
                 tau_r = 1, iota = 0.714, gamma_1 = 0.1, gamma_2 = 0.5)
.ei <- function(cases, dates, t0, lookback, lookahead) {
     do.call(est_initial_E_I_location,
             c(list(cases = cases, dates = dates, t0 = t0, lookback_days = lookback,
                    lookahead_days = lookahead), .ei_args))
}

test_that("cases only after t0 seed E/I when the window looks ahead", {
     t0 <- as.Date("2023-01-01")
     dates <- seq(t0 - 14, t0 + 13, by = "day")
     cases <- ifelse(dates >= t0, 25, 0)
     behind <- .ei(cases, dates, t0, 14, 0)
     expect_equal(c(behind$E, behind$I), c(0, 0))
     ahead <- .ei(cases, dates, t0, 14, 14)
     expect_gt(ahead$E, 0)
     expect_gt(ahead$I, 0)
})

test_that("under constant incidence the straddling window gives the same E and I", {
     t0 <- as.Date("2023-01-01")
     dates <- seq(t0 - 14, t0 + 13, by = "day")
     cases <- rep(10, length(dates))
     a <- .ei(cases, dates, t0, 14, 0)
     b <- .ei(cases, dates, t0, 14, 14)
     expect_equal(a, b)   # lambda is the window mean; post-t0 reports do not enter I directly
})

test_that("est_initial_E_I uses the template only when the whole window is empty", {
     root <- withr::local_tempdir()
     t0 <- as.Date("2023-01-01")
     dates <- seq(t0 - 30, t0 + 30, by = "day")
     utils::write.csv(data.frame(date = dates, iso_code = "TZA", cases = ifelse(dates >= t0, 25, 0)),
                      file.path(root, "cholera_surveillance_daily_combined.csv"), row.names = FALSE)
     utils::write.csv(data.frame(date = t0, iso_code = "TZA", total_population = 6e7),
                      file.path(root, "UN_world_population_prospects_daily.csv"), row.names = FALSE)
     PATHS <- list(DATA_CHOLERA_DAILY = root, DATA_DEMOGRAPHICS = root)
     run <- function(lookahead) suppressWarnings(est_initial_E_I(
          PATHS, MOSAIC::priors_default, list(location_name = "TZA", date_start = t0),
          n_samples = 50, t0 = t0, lookback_days = 14, lookahead_days = lookahead,
          verbose = FALSE, variance_inflation = 10, seed = 1
     ))$parameters_location$prop_E_initial$parameters$location$TZA
     expect_identical(run(0)$method, "observed_zero")
     fwd <- run(14)
     expect_identical(fwd$method, "variance_inflation")
     expect_gt(fwd$shape1 / (fwd$shape1 + fwd$shape2), 1e-6)
     expect_error(est_initial_E_I(PATHS, MOSAIC::priors_default,
                                  list(location_name = "TZA", date_start = t0),
                                  lookahead_days = -1, verbose = FALSE), "lookahead_days")
})
