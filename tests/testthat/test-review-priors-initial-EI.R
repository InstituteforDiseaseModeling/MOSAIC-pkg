# Regression tests (deep review, priors group) for est_initial_E_I().

.ei_fixture <- function(cases_per_day = 50, n_days = 21, t0 = as.Date("2024-03-01"),
                        iso = c("TCD", "NER")) {
     dir <- withr::local_tempdir(.local_envir = parent.frame())
     dates <- seq(t0 - n_days, t0 - 1, by = "day")
     surv <- do.call(rbind, lapply(iso, function(i)
          data.frame(date = dates, iso_code = i, cases = cases_per_day)))
     utils::write.csv(surv, file.path(dir, "cholera_surveillance_daily_combined.csv"),
                      row.names = FALSE)
     utils::write.csv(data.frame(date = t0, iso_code = iso, total_population = 1e7),
                      file.path(dir, "UN_world_population_prospects_daily.csv"),
                      row.names = FALSE)
     list(PATHS = list(DATA_CHOLERA_DAILY = dir, DATA_DEMOGRAPHICS = dir),
          config = list(location_name = iso, date_start = as.character(t0)), t0 = t0)
}

.point <- function(x) list(distribution = "beta", parameters = list(shape1 = 1e6 * x, shape2 = 1e6 * (1 - x)))

.ei_priors <- function(rho) {
     pr <- MOSAIC::priors_default
     pr$parameters_global$rho <- .point(rho)
     pr
}

.beta_mean <- function(x) x$shape1 / (x$shape1 + x$shape2)

test_that("est_initial_E_I samples the model's rho prior, not a hardcoded U(a, b)", {
     # Before v0.99.11 both MC branches hardcoded rho ~ U(0.2, 0.7) (parallel)
     # or U(0.05, 0.30) (sequential) and ignored priors$parameters_global$rho.
     fx <- .ei_fixture()
     run <- function(rho) {
          set.seed(3)
          out <- est_initial_E_I(fx$PATHS, .ei_priors(rho), fx$config, n_samples = 40,
                                 t0 = fx$t0, lookback_days = 14, verbose = FALSE,
                                 variance_inflation = 2)
          .beta_mean(out$parameters_location$prop_E_initial$parameters$location$TCD)
     }
     ratio <- run(0.1) / run(0.5)
     expect_gt(ratio, 4)
     expect_lt(ratio, 6)
})

test_that("E is the stock in balance with the onset rate; reported cases are not put in E", {
     # Before v0.99.11 E was exp(-iota * (t0 - report - tau_r)) of each report,
     # i.e. it counted already-symptomatic (reported) people as exposed and
     # was zero whenever tau_r >= the lookback.
     dates <- as.Date("2024-01-01") + 0:6
     t0 <- as.Date("2024-01-08")
     r <- est_initial_E_I_location(cases = rep(10, 7), dates = dates, population = 1e7,
                                   t0 = t0, lookback_days = 7, sigma = 1, rho = 1,
                                   chi = 1, tau_r = 10, iota = 0.714,
                                   gamma_1 = 0.1, gamma_2 = 0.67)
     expect_equal(r$E, round(10 / (-expm1(-0.714))))
     # I: observed onsets (ages 11..17) + unobserved recent onsets (ages 1..10)
     # + onsets older than the window (ages >= 18), all at 10/day
     expect_equal(r$I, round(10 / (-expm1(-0.1)) * exp(-0.1)))
})

test_that("a location whose estimation errors still gets the fallback prior", {
     fx <- .ei_fixture()
     testthat::local_mocked_bindings(.est_initial_E_I_one = function(...) stop("boom"))
     msgs <- character(0)
     out <- withCallingHandlers(
          est_initial_E_I(fx$PATHS, MOSAIC::priors_default, fx$config,
                          n_samples = 5, t0 = fx$t0, verbose = FALSE),
          warning = function(w) {
               msgs <<- c(msgs, conditionMessage(w))
               invokeRestart("muffleWarning")
          })
     expect_equal(sum(grepl("boom", msgs)), 2L)
     E <- out$parameters_location$prop_E_initial$parameters$location
     expect_setequal(names(E), c("TCD", "NER"))
     expect_equal(c(E$TCD$shape1, E$TCD$shape2), c(1, 9999))
})

test_that("per-location variance inflation does not leak between locations", {
     fx <- .ei_fixture()
     set.seed(1)
     out <- est_initial_E_I(fx$PATHS, MOSAIC::priors_default, fx$config, n_samples = 30,
                            t0 = fx$t0, lookback_days = 14, verbose = FALSE,
                            variance_inflation = c(TCD = 1.5, NER = 50))
     cv <- function(x) {
          a <- x$shape1; b <- x$shape2
          sqrt(a * b / ((a + b)^2 * (a + b + 1))) / (a / (a + b))
     }
     loc <- out$parameters_location
     # NER's I prior is much wider than TCD's: each location used its own VI
     expect_gt(cv(loc$prop_I_initial$parameters$location$NER),
               3 * cv(loc$prop_I_initial$parameters$location$TCD))
})

test_that("too few Monte Carlo draws fall back to the no-data prior, not Beta(1, 999)", {
     fx <- .ei_fixture()
     out <- est_initial_E_I(fx$PATHS, MOSAIC::priors_default, fx$config, n_samples = 1,
                            t0 = fx$t0, verbose = FALSE)
     E <- out$parameters_location$prop_E_initial$parameters$location$TCD
     I <- out$parameters_location$prop_I_initial$parameters$location$TCD
     expect_equal(c(E$shape1, E$shape2), c(1, 9999))
     expect_equal(c(I$shape1, I$shape2), c(0.5, 9999.5))
})

test_that("the verbose summary prints one row per data-driven location", {
     fx <- .ei_fixture()
     txt <- capture.output(suppressWarnings(
          est_initial_E_I(fx$PATHS, MOSAIC::priors_default, fx$config, n_samples = 10,
                          t0 = fx$t0, lookback_days = 14, verbose = TRUE)))
     rows <- grep("^(TCD|NER) +\\(", txt, value = TRUE)
     expect_length(rows, 2L)
     expect_false(any(grepl("\\(-\\)", rows)))
})

test_that("zero reported cases in the window give the near-zero prior, not the no-data one", {
     fx <- .ei_fixture(cases_per_day = 0)
     out <- suppressWarnings(est_initial_E_I(fx$PATHS, MOSAIC::priors_default, fx$config,
                                             n_samples = 10, t0 = fx$t0, verbose = FALSE))
     E <- out$parameters_location$prop_E_initial$parameters$location$TCD
     expect_equal(c(E$shape1, E$shape2), c(0.01, 99999.99))
     expect_equal(E$method, "observed_zero")
})

test_that("the E/I Beta keeps the Monte Carlo mean for wide variance inflation", {
     # The Monte Carlo mean used to be passed to fit_beta_from_ci() as the MODE;
     # with both shapes held above 1 that put the prior mean ~6x above it at the
     # shipped VI = 65-160.
     set.seed(5)
     counts <- stats::rpois(200, 10)
     N <- 1e7
     for (vi in c(2, 65, 120, 160)) {
          fit <- MOSAIC:::.est_initial_E_I_fit(counts, N, "E", "TCD", vi, 200L,
                                              total_cases = 100, verbose = FALSE)
          expect_rel_equal(.beta_mean(fit), mean(counts / N), rel = 1e-9)
     }
})

test_that("each location's E and I fits receive that location's variance inflation", {
     # The pre-v0.99.11 loop looked up the I-compartment factor with
     # exists("loc_variance_inflation"), which could pick up the previous
     # location's value. The factor is now resolved once per location and passed
     # explicitly; pin that contract by recording what the fit helper receives.
     fx <- .ei_fixture()
     seen <- list()
     real_fit <- MOSAIC:::.est_initial_E_I_fit
     local_mocked_bindings(
          .est_initial_E_I_fit = function(counts, population_t0, compartment, loc,
                                          loc_variance_inflation, ...) {
               seen[[paste(loc, compartment)]] <<- loc_variance_inflation
               real_fit(counts, population_t0, compartment, loc, loc_variance_inflation, ...)
          },
          .package = "MOSAIC"
     )
     set.seed(1)
     est_initial_E_I(fx$PATHS, MOSAIC::priors_default, fx$config, n_samples = 20,
                     t0 = fx$t0, lookback_days = 14, verbose = FALSE,
                     variance_inflation = c(TCD = 1.5, NER = 50))
     expect_equal(unlist(seen[c("TCD E", "TCD I", "NER E", "NER I")], use.names = FALSE),
                  c(1.5, 1.5, 50, 50))
})
