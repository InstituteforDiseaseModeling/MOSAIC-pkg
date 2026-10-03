# Imputed (tier-3) surveillance rows -- AI Fourier reconstructions, a year's
# total spread along a seasonal shape -- are not dated reports. They do not seed
# E/I where observed or reconstructed counts exist in the window, a window
# without such counts falls back on country-level (never regional)
# reconstructions, and imputed later cases never trigger the quiet-start seed.

.ei_args_tier <- list(population = 1e6, sigma = 0.35, rho = 0.42, chi = 0.52,
                      tau_r = 1, iota = 0.714, gamma_1 = 0.1, gamma_2 = 0.5)

test_that("est_initial_E_I_location treats a day without a count as unobserved, not zero", {
     t0 <- as.Date("2018-01-01")
     dates <- seq(t0 - 14, t0 + 13, by = "day")
     full <- rep(10, length(dates))
     run <- function(cases, d = dates) {
          do.call(est_initial_E_I_location,
                  c(list(cases = cases, dates = d, t0 = t0, lookback_days = 14,
                         lookahead_days = 14), .ei_args_tier))
     }
     ref <- run(full)
     expect_gt(ref$E, 0)
     expect_gt(ref$I, 0)
     # Under constant incidence a missing reporting week before t0 changes
     # nothing, whether it is NA or absent; so does a window observed only from t0.
     gap <- dates >= t0 - 14 & dates < t0 - 7
     expect_equal(run(replace(full, gap, NA)), ref)
     expect_equal(run(full[!gap], dates[!gap]), ref)
     expect_equal(run(replace(full, dates < t0, NA)), ref)
     # An all-NA window is still a window without cases.
     expect_equal(run(rep(NA_real_, length(dates))), list(E = 0, I = 0))
})

test_that("est_initial_E_I: tier 1-2 counts take precedence over imputed rows", {
     test_env <- environment()
     t0 <- as.Date("2018-01-01")
     dates <- seq(t0 - 30, t0 + 400, by = "day")
     win_pre <- dates >= t0 - 14 & dates < t0
     mk <- function(iso, cases, method) {
          data.frame(date = dates, iso_code = iso, cases = cases, disaggregation_method = method)
     }
     surv <- rbind(
          # KEN: observed 10/day from t0, the two weeks before t0 reconstructed at 50/day
          mk("KEN", ifelse(win_pre, 50, 10), ifelse(win_pre, "fourier_country_k2", NA)),
          # ETH: no tier 1-2 count near t0; country reconstructions at 20/day, observed later
          mk("ETH", 20, ifelse(dates < t0 + 60, "fourier_country_k2", NA)),
          # RWA: a regional reconstruction only (one case in the window), observed cases a year on
          mk("RWA", ifelse(dates >= t0 + 365, 15, ifelse(dates == t0 + 3, 1, 0)),
             ifelse(dates >= t0 + 365, NA, "fourier_regional_East Africa_k5")),
          # BDI: observed zeros from t0, reconstructed cases before it, observed cases later
          mk("BDI", ifelse(win_pre, 2, ifelse(dates >= t0 + 300, 15, 0)),
             ifelse(dates < t0, "fourier_country_k2", NA)),
          # MLI: observed zeros in the window; its only later cases are reconstructions
          mk("MLI", ifelse(dates >= t0 + 200, 3, 0),
             ifelse(dates >= t0 + 200, "fourier_regional_West Africa_k5", NA)))
     pops <- c(KEN = 5e7, ETH = 1.1e8, RWA = 1.2e7, BDI = 1.2e7, MLI = 2e7)
     write_inputs <- function(s, iso) {
          root <- withr::local_tempdir(.local_envir = test_env)
          utils::write.csv(s, file.path(root, "cholera_surveillance_daily_combined.csv"), row.names = FALSE)
          utils::write.csv(data.frame(date = t0, iso_code = iso, total_population = pops[iso]),
                           file.path(root, "UN_world_population_prospects_daily.csv"), row.names = FALSE)
          list(DATA_CHOLERA_DAILY = root, DATA_DEMOGRAPHICS = root)
     }
     run <- function(PATHS, iso, ...) {
          suppressWarnings(est_initial_E_I(
               PATHS, MOSAIC::priors_default,
               list(location_name = iso, date_start = t0, date_stop = t0 + 400),
               n_samples = 50, t0 = t0, lookback_days = 14, lookahead_days = 14,
               verbose = FALSE, variance_inflation = 10, seed = 1, ...))
     }
     paths_all <- write_inputs(surv, names(pops))
     res <- run(paths_all, names(pops), quiet_start = "seed")
     loc_E <- res$parameters_location$prop_E_initial$parameters$location
     loc_I <- res$parameters_location$prop_I_initial$parameters$location

     # KEN: exactly the estimate from its observed days alone
     ken_obs <- surv[surv$iso_code == "KEN", c("date", "iso_code", "cases")]
     ken_obs$cases[win_pre] <- NA
     paths_ken <- write_inputs(ken_obs, "KEN")
     ref <- run(paths_ken, "KEN", quiet_start = "seed")
     expect_identical(loc_E$KEN, ref$parameters_location$prop_E_initial$parameters$location$KEN)
     expect_identical(loc_I$KEN, ref$parameters_location$prop_I_initial$parameters$location$KEN)

     # ETH: window read from its country reconstructions, not a quiet start
     expect_identical(loc_E$ETH$method, "variance_inflation")
     expect_identical(res$metadata$imputed_window_fallback, "ETH")

     # RWA (regional reconstruction only) and BDI (observed zeros once its
     # reconstructions are set aside) are quiet starts; MLI's later cases are
     # all imputed, so it keeps the near-zero template.
     for (iso in c("RWA", "BDI")) {
          for (fit in list(loc_E[[iso]], loc_I[[iso]])) {
               expect_identical(fit$method, "quiet_start_seed")
               expect_equal(c(fit$shape1, fit$shape2), c(1, 1e5))
          }
     }
     expect_identical(loc_E$MLI$method, "observed_zero")
     expect_equal(c(loc_E$MLI$shape1, loc_E$MLI$shape2), c(0.01, 99999.99))
     expect_setequal(res$metadata$quiet_start_seeded, c("RWA", "BDI"))

     # A file without disaggregation_method: every row is observed (KEN then
     # reads its 50/day pre-t0 rows as reports and gets a larger E).
     paths_plain <- write_inputs(surv[surv$iso_code == "KEN", c("date", "iso_code", "cases")], "KEN")
     plain <- run(paths_plain, "KEN")
     expect_identical(plain$metadata$imputed_window_fallback, character(0))
     m <- function(x) x$shape1 / (x$shape1 + x$shape2)
     expect_gt(m(plain$parameters_location$prop_E_initial$parameters$location$KEN), m(loc_E$KEN))
})
