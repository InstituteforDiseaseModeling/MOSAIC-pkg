# Regression tests: zeta_ratio is a lognormal truncated below at 1 so the
# derived zeta_2 = zeta_1 / zeta_ratio never exceeds zeta_1 (about 16% of
# untruncated draws did).

.zr_prior <- list(distribution = "lognormal",
                  parameters = list(meanlog = 4.31, sdlog = 4.39, lower = 1))

test_that("sample_from_prior honours lognormal truncation bounds", {
     set.seed(1)
     x <- sample_from_prior(20000, .zr_prior)
     expect_true(all(x >= 1))
     p_lo <- plnorm(1, 4.31, 4.39)
     expect_equal(stats::median(x), qlnorm(p_lo + 0.5 * (1 - p_lo), 4.31, 4.39),
                  tolerance = 0.05)
     y <- sample_from_prior(5000, list(distribution = "lognormal",
                                       parameters = list(meanlog = 0, sdlog = 1,
                                                         lower = 0.5, upper = 2)))
     expect_true(all(y >= 0.5 & y <= 2))
     # mean/sd parameterisation takes the same bounds
     z <- sample_from_prior(2000, list(distribution = "lognormal",
                                       parameters = list(mean = 1, sd = 2, lower = 1)))
     expect_true(all(z >= 1))
})

test_that("untruncated lognormal draws are unchanged (same RNG stream)", {
     set.seed(7); a <- sample_from_prior(50, list(distribution = "lognormal",
                                                  parameters = list(meanlog = 1, sdlog = 2)))
     set.seed(7); b <- rlnorm(50, 1, 2)
     expect_identical(a, b)
})

test_that("invalid truncation bounds return NA", {
     bad <- list(distribution = "lognormal",
                 parameters = list(meanlog = 0, sdlog = 1, lower = 2, upper = 1))
     expect_true(all(is.na(sample_from_prior(3, bad))))
})

test_that("sample_parameters never gives zeta_2 > zeta_1 under the truncated prior", {
     fx <- skip_if_no_data()
     pri <- fx$priors
     pri$parameters_global$zeta_ratio <- .zr_prior
     ok <- vapply(1:40, function(s) {
          out <- suppressWarnings(suppressMessages(sample_parameters(
               PATHS = get_paths(), priors = pri, config = fx$config, seed = s,
               verbose = FALSE)))
          out$zeta_2 <= out$zeta_1
     }, logical(1))
     expect_true(all(ok))
})

test_that("update_priors_from_posteriors and inflate_priors keep the truncation bound", {
     pri <- list(parameters_global = list(zeta_ratio = .zr_prior), parameters_location = list())
     post <- list(parameters_global = list(zeta_ratio = list(
          distribution = "lognormal", parameters = list(meanlog = 5, sdlog = 2, ess = 100))),
          parameters_location = list())
     upd <- suppressMessages(update_priors_from_posteriors(pri, post, verbose = FALSE))
     expect_equal(upd$parameters_global$zeta_ratio$parameters$lower, 1)
     expect_equal(upd$parameters_global$zeta_ratio$parameters$meanlog, 5)
     infl <- inflate_priors(pri, inflation_factor = 2, verbose = FALSE)
     expect_equal(infl$parameters_global$zeta_ratio$parameters$lower, 1)
})
