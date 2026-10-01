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

test_that("quiet_start = 'seed' floors quiet-start locations only", {
     root <- withr::local_tempdir()
     t0 <- as.Date("2023-01-01")
     dates <- seq(t0 - 30, t0 + 400, by = "day")
     surv <- rbind(
          # GHA: nothing around t0, an outbreak a year later
          data.frame(date = dates, iso_code = "GHA", cases = ifelse(dates >= t0 + 365, 20, 0)),
          # ERI: silent throughout
          data.frame(date = dates, iso_code = "ERI", cases = 0),
          # TZA: cases around t0 (estimated from data either way)
          data.frame(date = dates, iso_code = "TZA", cases = 10))
     utils::write.csv(surv, file.path(root, "cholera_surveillance_daily_combined.csv"), row.names = FALSE)
     utils::write.csv(data.frame(date = t0, iso_code = c("GHA", "ERI", "TZA"), total_population = c(3.4e7, 3.7e6, 6.5e7)),
                      file.path(root, "UN_world_population_prospects_daily.csv"), row.names = FALSE)
     PATHS <- list(DATA_CHOLERA_DAILY = root, DATA_DEMOGRAPHICS = root)
     cfg <- list(location_name = c("GHA", "ERI", "TZA"), date_start = t0, date_stop = t0 + 400)
     run <- function(...) suppressWarnings(est_initial_E_I(
          PATHS, MOSAIC::priors_default, cfg, n_samples = 50, t0 = t0, lookback_days = 14,
          lookahead_days = 14, verbose = FALSE, variance_inflation = 10, seed = 1, ...))
     tmpl <- run()
     expect_identical(tmpl$parameters_location$prop_E_initial$parameters$location$GHA$method, "observed_zero")
     expect_identical(tmpl$metadata$quiet_start_seeded, character(0))

     seeded <- run(quiet_start = "seed")
     loc_E <- seeded$parameters_location$prop_E_initial$parameters$location
     loc_I <- seeded$parameters_location$prop_I_initial$parameters$location
     for (fit in list(loc_E$GHA, loc_I$GHA)) {
          expect_identical(fit$method, "quiet_start_seed")
          expect_equal(c(fit$shape1, fit$shape2), c(1, 1e5))
     }
     expect_identical(loc_E$ERI$method, "observed_zero")         # silent everywhere: template
     expect_equal(c(loc_E$ERI$shape1, loc_E$ERI$shape2), c(0.01, 99999.99))
     expect_identical(loc_E$TZA, tmpl$parameters_location$prop_E_initial$parameters$location$TZA)
     expect_identical(seeded$metadata$quiet_start_seeded, "GHA")

     # Cases after date_stop do not count.
     cfg$date_stop <- t0 + 300
     expect_identical(run(quiet_start = "seed")$metadata$quiet_start_seeded, character(0))

     expect_error(run(quiet_start = "seed", quiet_seed_shape2 = -1), "quiet_seed_shape")
     expect_error(run(quiet_start = "bogus"))
})
