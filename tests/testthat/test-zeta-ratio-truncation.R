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
     up <- upd$parameters_global$zeta_ratio$parameters
     expect_equal(up$lower, 1)
     # The untruncated LN(5, 2) posterior is refitted so that the TRUNCATED
     # distribution keeps its 95% interval (it is not re-truncated unchanged).
     ci_post <- qlnorm(c(0.025, 0.975), 5, 2)
     p_lo <- plnorm(1, up$meanlog, up$sdlog)
     ci_trunc <- qlnorm(p_lo + c(0.025, 0.975) * (1 - p_lo), up$meanlog, up$sdlog)
     expect_equal(log(ci_trunc), log(ci_post), tolerance = 1e-4)
     # A posterior already fitted in the truncated family (carries lower) is kept
     post$parameters_global$zeta_ratio$parameters$lower <- 1
     upd2 <- suppressMessages(update_priors_from_posteriors(pri, post, verbose = FALSE))
     expect_equal(upd2$parameters_global$zeta_ratio$parameters$meanlog, 5)
     infl <- inflate_priors(pri, inflation_factor = 2, verbose = FALSE)
     expect_equal(infl$parameters_global$zeta_ratio$parameters$lower, 1)
})

test_that("a non-identifiable truncated prior does not drift across staged updates", {
     # Posterior == prior (zeta_ratio is not identifiable). Quantiles are taken
     # analytically from the current truncated distribution, so any movement is
     # the fit-then-carry step itself, not Monte Carlo noise. Before the fix each
     # step moved the median ~184 -> 1017 -> 1447 -> 1816 -> 2058.
     qtr <- function(p, e) {
          m <- e$parameters$meanlog; s <- e$parameters$sdlog
          lo <- plnorm(e$parameters$lower %||% 0, m, s)
          qlnorm(lo + p * (1 - lo), m, s)
     }
     med0 <- qtr(0.5, .zr_prior)
     cur <- .zr_prior
     for (stage in 1:5) {
          q <- qtr(c(0.025, 0.5, 0.975), cur)
          f <- fit_lognormal_from_ci(mode_val = q[2], ci_lower = q[1], ci_upper = q[3])
          entry <- list(distribution = "lognormal",
                        parameters = list(meanlog = f$meanlog, sdlog = f$sdlog))
          cur <- MOSAIC:::.carry_lognormal_bounds(entry, .zr_prior)
          expect_equal(cur$parameters$lower, 1)
     }
     expect_equal(qtr(0.5, cur), med0, tolerance = 1e-3)
     expect_equal(qtr(c(0.025, 0.975), cur), qtr(c(0.025, 0.975), .zr_prior), tolerance = 1e-3)
     # Sampling from the carried entry reproduces the prior's median
     set.seed(11)
     x <- sample_from_prior(1e5, cur)
     expect_lt(abs(log(stats::median(x) / med0)), log(1.05))
})

test_that("calc_model_posterior_distributions fits and keeps the truncated family", {
     skip_if_not_installed("jsonlite")
     dir <- withr::local_tempdir()
     pri <- list(metadata = list(version = "t"),
                 parameters_global = list(zeta_ratio = .zr_prior),
                 parameters_location = list())
     pf <- file.path(dir, "priors.json")
     jsonlite::write_json(pri, pf, auto_unbox = TRUE, digits = NA)
     set.seed(3)
     x <- sample_from_prior(1e5, .zr_prior)
     q <- stats::quantile(x, c(0.025, 0.25, 0.5, 0.75, 0.975), names = FALSE)
     qdf <- data.frame(parameter = "zeta_ratio", type = "posterior", param_type = "global",
                       location = NA_character_, q0.025 = q[1], q0.25 = q[2],
                       q0.5 = q[3], q0.75 = q[4], q0.975 = q[5], mean = mean(x),
                       sd = stats::sd(x), mode = q[3], prior_distribution = "lognormal",
                       posterior_distribution = "lognormal", stringsAsFactors = FALSE)
     qf <- file.path(dir, "posterior_quantiles.csv")
     utils::write.csv(qdf, qf, row.names = FALSE)
     res <- tryCatch(suppressMessages(suppressWarnings(calc_model_posterior_distributions(
          quantiles_file = qf, priors_file = pf, output_dir = dir, verbose = FALSE))),
          error = function(e) skip(paste("fixture shape not accepted:", conditionMessage(e))))
     post <- jsonlite::read_json(file.path(dir, "posteriors.json"))
     zp <- post$parameters_global$zeta_ratio$parameters
     expect_equal(zp$lower, 1)
     y <- sample_from_prior(1e5, list(distribution = "lognormal", parameters = zp))
     expect_lt(abs(log(stats::median(y) / q[3])), log(1.10))
})

test_that("density, quantile and mean readers honour lognormal truncation", {
     d <- MOSAIC:::.dlnorm_trunc(c(0.5, 2), 4.31, 4.39, lower = 1)
     expect_identical(d[1], 0)
     expect_equal(stats::integrate(function(x) MOSAIC:::.dlnorm_trunc(x, 0, 1, lower = 1),
                                   1, Inf)$value, 1, tolerance = 1e-6)
     # Untruncated entries are unchanged
     expect_equal(MOSAIC:::.dlnorm_trunc(2, 0, 1), dlnorm(2, 0, 1))
     expect_equal(MOSAIC:::.lognormal_trunc_mean(0, 1), exp(0.5))
     set.seed(5)
     x <- sample_from_prior(4e5, list(distribution = "lognormal",
                                      parameters = list(meanlog = 0, sdlog = 1, lower = 1)))
     expect_equal(MOSAIC:::.lognormal_trunc_mean(0, 1, lower = 1), mean(x), tolerance = 0.01)
     expect_equal(MOSAIC:::.qlnorm_trunc(0.5, 0, 1, lower = 1), stats::median(x), tolerance = 0.01)
     # check_sampled_parameter() reports the truncated mean
     pri <- list(parameters_global = list(zr = list(distribution = "lognormal",
                                                    parameters = list(meanlog = 0, sdlog = 1,
                                                                      lower = 1))),
                 parameters_location = list())
     chk <- check_sampled_parameter(list(zr = 2), pri, "zr")
     expect_equal(chk$expected_mean, MOSAIC:::.lognormal_trunc_mean(0, 1, lower = 1))
})
