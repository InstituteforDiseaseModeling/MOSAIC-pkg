# Regression tests (deep review, priors group) for the CI fitters.

test_that("fit_beta_from_ci matches the CI for small proportions (no absolute clamp)", {
     # Before v0.100.0 the mean was clamped to [ci_lower + 0.01, ci_upper - 0.01],
     # so any CI below ~0.02 was discarded: mode 1e-6 with CI [1e-7, 1e-5]
     # came back roughly 1.6x wide instead of 100x.
     rel_ok <- function(fit, lo, hi, tol) {
          all(abs(log(fit$fitted_ci / c(lo, hi))) < log(tol))
     }
     f1 <- fit_beta_from_ci(1e-6, 1e-7, 1e-5)
     expect_rel_equal(f1$fitted_mode, 1e-6, rel = 1e-9)
     expect_true(rel_ok(f1, 1e-7, 1e-5, 2.5))
     f2 <- fit_beta_from_ci(1e-3, 5e-4, 2e-3)
     expect_true(rel_ok(f2, 5e-4, 2e-3, 1.1))
     expect_lt(f2$fitted_mean, 2e-3)
     f3 <- fit_beta_from_ci(0.05, 0.02, 0.1)
     expect_true(rel_ok(f3, 0.02, 0.1, 1.2))
     # Well-behaved interior case still matches closely
     f4 <- fit_beta_from_ci(0.5, 0.25, 0.75)
     expect_true(rel_ok(f4, 0.25, 0.75, 1.02))
     # The optimisation method is usable for small values too
     f5 <- fit_beta_from_ci(1e-3, 5e-4, 2e-3, method = "optimization")
     expect_true(rel_ok(f5, 5e-4, 2e-3, 1.1))
})

test_that("fit_lognormal_from_ci reproduces a wide CI instead of shifting it up", {
     # Before v0.100.0 meanlog = log(mode) + sdlog^2 with sdlog from the CI
     # width, so CI [0.1, 10] with mode 1 came back as [0.42, 66.9].
     f <- fit_lognormal_from_ci(1, 0.1, 10)
     expect_equal(f$fitted_ci, c(0.1, 10), tolerance = 1e-10)
     g <- fit_lognormal_from_ci(1.3e8, 1e6, 1e12)
     expect_equal(g$fitted_ci / c(1e6, 1e12), c(1, 1), tolerance = 1e-10)
     # A genuinely lognormal target is recovered exactly
     ml <- -1.5; sl <- 1.5
     h <- fit_lognormal_from_ci(exp(ml - sl^2), qlnorm(0.025, ml, sl), qlnorm(0.975, ml, sl))
     expect_equal(c(h$meanlog, h$sdlog), c(ml, sl), tolerance = 1e-10)
     o <- fit_lognormal_from_ci(exp(ml - sl^2), qlnorm(0.025, ml, sl), qlnorm(0.975, ml, sl),
                                method = "optimization")
     expect_equal(c(o$meanlog, o$sdlog), c(ml, sl), tolerance = 1e-4)
})

test_that("fit_gompertz_from_ci reports the true mode and matches the interval", {
     # Before v0.100.0 eta = b * exp(b * mode) was enforced, which makes the
     # density monotone decreasing (argmax 0) while reporting fitted_mode = mode,
     # and the lower quantile missed its target (8.9e-4 vs 0.005).
     r <- fit_gompertz_from_ci(0.02, 0.005, 0.1)
     expect_equal(unname(r$fitted_ci), c(0.005, 0.1), tolerance = 1e-8)
     x <- seq(0, 0.2, length.out = 200001)
     expect_equal(r$fitted_mode, x[which.max(dgompertz(x, r$b, r$eta))], tolerance = 1e-4)
     # A monotone-decreasing target (eta >= 1) reports mode 0
     q <- qgompertz(c(0.025, 0.975), b = 50, eta = 50)
     m <- fit_gompertz_from_ci(1e-4, q[1], q[2])
     expect_equal(c(m$b, m$eta), c(50, 50), tolerance = 1e-6)
     expect_equal(m$fitted_mode, 0)
})

test_that("fit_gompertz_from_ci accepts a reference mode outside the interval", {
     # A monotone-decreasing posterior (eta >= 1) has its KDE mode near 0, often
     # below the 2.5% quantile. mode_val does not enter the fit, so it must not
     # abort it (inflate_priors / calc_model_posterior_distributions used to skip
     # such parameters on the resulting error).
     q <- qgompertz(c(0.025, 0.975), b = 50, eta = 50)
     m <- fit_gompertz_from_ci(q[1] / 10, q[1], q[2])
     expect_equal(c(m$b, m$eta), c(50, 50), tolerance = 1e-6)
})

test_that(".fit_beta_mean_ci keeps the mean exact and spans decades below it", {
     # est_initial_E_I passes the Monte Carlo MEAN. Anchoring it as the mode of
     # a Beta with both shapes > 1 put the prior mean ~6x above it for VI = 120
     # (Beta(1.21, 2.08e5)-like fits) and could not reach m / VI.
     m <- 1e-6
     for (vi in c(2, 65, 120)) {
          sh <- MOSAIC:::.fit_beta_mean_ci(m, m / vi, m * vi)
          expect_rel_equal(sh[1] / sum(sh), m, rel = 1e-9)
          q <- stats::qbeta(c(0.025, 0.975), sh[1], sh[2])
          expect_lt(q[1], m)
          expect_gt(q[2], m)
     }
     sh <- MOSAIC:::.fit_beta_mean_ci(m, m / 120, m * 120)
     q <- stats::qbeta(c(0.025, 0.975), sh[1], sh[2])
     expect_lt(sh[1], 1)                    # J-shaped: the only way to span the lower target
     expect_lt(abs(log(q[1] / (m / 120))), log(2))
     # A moderate CI is matched on both sides
     sh <- MOSAIC:::.fit_beta_mean_ci(m, m / 2, m * 2)
     q <- stats::qbeta(c(0.025, 0.975), sh[1], sh[2])
     expect_true(all(abs(log(q / c(m / 2, m * 2))) < log(1.3)))
})

test_that(".fit_beta_inflated_samples keeps the sample mean when the SD is inflated", {
     # est_initial_R / est_initial_S used to widen the sample CI linearly and floor
     # the lower bound (1e-10 / 0.001). The logit-scale mode-exact fitter then
     # chased the unreachable floor: mode 0.06, VI = 3 gave mean 0.155 and an
     # upper 97.5% of 0.43. The refit is now method of moments on (mean, VI * sd).
     set.seed(11)
     x <- stats::rbeta(4000, 0.06 * 500, 0.94 * 500)
     for (vi in c(0, 1, 3, 6, 14)) {
          sh <- MOSAIC:::.fit_beta_inflated_samples(x, vi)
          expect_rel_equal(sh[1] / sum(sh), mean(x), rel = 1e-9)
     }
     sd_beta <- function(sh) sqrt(prod(sh) / (sum(sh)^2 * (sum(sh) + 1)))
     expect_rel_equal(sd_beta(MOSAIC:::.fit_beta_inflated_samples(x, 1)), stats::sd(x), rel = 1e-9)
     expect_rel_equal(sd_beta(MOSAIC:::.fit_beta_inflated_samples(x, 3)), 3 * stats::sd(x), rel = 1e-9)
     sh3 <- MOSAIC:::.fit_beta_inflated_samples(x, 3)
     expect_lt(stats::qbeta(0.975, sh3[1], sh3[2]), 0.16)
     # Huge factors are capped at concentration 2, mean still kept
     sh50 <- MOSAIC:::.fit_beta_inflated_samples(x, 50)
     expect_equal(sum(sh50), 2)
     expect_rel_equal(sh50[1] / 2, mean(x), rel = 1e-9)
     expect_null(MOSAIC:::.fit_beta_inflated_samples(c(0.1), 2))
     expect_null(MOSAIC:::.fit_beta_inflated_samples(rep(0.1, 5), 2))
})

test_that("fit_beta_with_variance_inflation_R keeps the prop_R sample mean", {
     set.seed(3)
     x <- stats::rnorm(2000, 0.06, 0.01)
     for (vi in c(3, 6)) {
          f <- fit_beta_with_variance_inflation_R(x, vi)
          expect_rel_equal(f$shape1 / (f$shape1 + f$shape2), mean(x[x > 0 & x < 1]), rel = 1e-9)
     }
})
