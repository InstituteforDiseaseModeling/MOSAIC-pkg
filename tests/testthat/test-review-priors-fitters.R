# Regression tests (deep review, priors group) for the CI fitters.

test_that("fit_beta_from_ci matches the CI for small proportions (no absolute clamp)", {
     # Before v0.99.11 the mean was clamped to [ci_lower + 0.01, ci_upper - 0.01],
     # so any CI below ~0.02 was discarded: mode 1e-6 with CI [1e-7, 1e-5]
     # came back roughly 1.6x wide instead of 100x.
     rel_ok <- function(fit, lo, hi, tol) {
          all(abs(log(fit$fitted_ci / c(lo, hi))) < log(tol))
     }
     f1 <- fit_beta_from_ci(1e-6, 1e-7, 1e-5)
     expect_equal(f1$fitted_mode, 1e-6, tolerance = 1e-6)
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
     # Before v0.99.11 meanlog = log(mode) + sdlog^2 with sdlog from the CI
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
     # Before v0.99.11 eta = b * exp(b * mode) was enforced, which makes the
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
