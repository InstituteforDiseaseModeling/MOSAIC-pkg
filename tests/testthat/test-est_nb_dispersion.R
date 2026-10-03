# Tests for est_nb_dispersion() -- conditional weekly NB dispersion.
#
# The estimator replaced a marginal method-of-moments form that was mis-specified
# for a non-stationary mean. The decisive regression test is therefore recovery
# of a KNOWN k from data built the way MOSAIC surveillance data are built:
# weekly totals divided by 7 and rounded onto a daily grid.

# Build a daily grid from weekly NB draws around a seasonal mean.
.mk_daily <- function(k_true, n_weeks = 200L, seed = 1L, amp = 1.2, base = 30, floor_ = 5) {
     set.seed(seed)
     wk <- seq_len(n_weeks)
     mu <- base * exp(amp * sin(2 * pi * wk / 52)) + floor_
     W  <- stats::rnbinom(n_weeks, mu = mu, size = k_true)
     rep(round(W / 7), each = 7)
}

test_that("recovers a known dispersion across two orders of magnitude", {
     for (k_true in c(0.5, 4, 20)) {
          y <- .mk_daily(k_true, seed = 42L)
          r <- est_nb_dispersion(matrix(y, nrow = 1L), date_start = "2018-01-01",
                                 shrink = FALSE)
          expect_true(is.finite(r$k))
          # generous factor-of-two band: the /7-and-round roundtrip and the
          # spline mean model both perturb the estimate
          expect_gt(r$k, k_true / 2.5)
          expect_lt(r$k, k_true * 2.5)
          expect_identical(r$status, "ok")
     }
})

test_that("weekly cadence is detected, not assumed", {
     y <- .mk_daily(4, seed = 7L)
     r <- est_nb_dispersion(matrix(y, nrow = 1L), date_start = "2018-01-01", shrink = FALSE)
     expect_equal(r$weekly_share, 1, tolerance = 1e-8)

     # a genuinely daily series must NOT look weekly
     set.seed(7); yd <- stats::rnbinom(1400, mu = 20, size = 3)
     rd <- est_nb_dispersion(matrix(yd, nrow = 1L), date_start = "2018-01-01", shrink = FALSE)
     expect_lt(rd$weekly_share, 0.5)
})

test_that("the estimate is invariant to the series start weekday", {
     # The reporting-week boundary is DETECTED, not assumed to be Monday, so a
     # config starting mid-week (config_default v6.x started on Sunday
     # 2023-01-01) must not split every reporting week across two blocks --
     # which would inflate the apparent noise and bias k downward.
     y <- .mk_daily(4, seed = 11L)
     r_mon <- est_nb_dispersion(matrix(y, nrow = 1L), date_start = "2023-01-02", shrink = FALSE)
     r_sun <- est_nb_dispersion(matrix(y, nrow = 1L), date_start = "2023-01-01", shrink = FALSE)
     expect_equal(r_sun$weekly_share, 1, tolerance = 1e-8)
     expect_equal(r_mon$weekly_share, 1, tolerance = 1e-8)
     expect_equal(r_sun$k, r_mon$k, tolerance = 1e-8)
     expect_equal(r_sun$week_offset, 6L)   # Sunday start -> boundary 6 days off Monday
     expect_equal(r_mon$week_offset, 0L)
})

test_that("real config observations are detected as Monday-aligned weekly", {
     skip_if_not(exists("config_default", where = asNamespace("MOSAIC")))
     cfg <- MOSAIC::config_default
     r <- est_nb_dispersion(cfg$reported_cases[1:5, , drop = FALSE],
                            cfg$reported_cases_weight[1:5, , drop = FALSE],
                            date_start = cfg$date_start,
                            location_name = cfg$location_name[1:5], shrink = FALSE)
     ok <- is.finite(r$weekly_share)
     expect_true(all(r$weekly_share[ok] > 0.99))
})

test_that("uninformative series route to the Poisson limit, not to NA", {
     n <- 1400L
     all_zero <- rep(0, n)
     expect_identical(
          est_nb_dispersion(matrix(all_zero, nrow = 1L), date_start = "2018-01-01",
                            shrink = FALSE)$status, "poisson_insufficient_data")

     # below edgeR's min.total.count = 15
     sparse <- c(rep(0, n - 14L), rep(1, 14L))
     rs <- est_nb_dispersion(matrix(sparse, nrow = 1L), date_start = "2018-01-01", shrink = FALSE)
     expect_true(is.infinite(rs$k))

     # fewer than 5 non-zero weeks
     few <- rep(0, n); few[1:21] <- 7
     rf <- est_nb_dispersion(matrix(few, nrow = 1L), date_start = "2018-01-01", shrink = FALSE)
     expect_true(is.infinite(rf$k))
})

test_that("k is always bounded, finite-or-Inf, never NA after shrinkage", {
     rows <- rbind(.mk_daily(4, seed = 1L), .mk_daily(0.5, seed = 2L),
                   rep(0, 1400), .mk_daily(20, seed = 3L))
     r <- est_nb_dispersion(rows, date_start = "2018-01-01",
                            location_name = c("A", "B", "C", "D"))
     expect_equal(nrow(r), 4L)
     expect_false(any(is.na(r$k)))
     fin <- is.finite(r$k)
     expect_true(all(r$k[fin] >= 0.1 & r$k[fin] <= 1e5))
})

test_that("observation weights are honoured", {
     y <- .mk_daily(4, seed = 5L)
     n <- length(y)
     w_all <- matrix(1, nrow = 1L, ncol = n)
     # zero-weight the second half: the estimate must change
     w_half <- w_all; w_half[1, (n %/% 2):n] <- 0
     k_all  <- est_nb_dispersion(matrix(y, nrow = 1L), w_all,  date_start = "2018-01-01", shrink = FALSE)$k
     k_half <- est_nb_dispersion(matrix(y, nrow = 1L), w_half, date_start = "2018-01-01", shrink = FALSE)$k
     expect_true(is.finite(k_all))
     expect_false(isTRUE(all.equal(k_all, k_half)))
})

test_that("calc_model_likelihood accepts scalar and per-location dispersion", {
     set.seed(3)
     n_loc <- 3L; n_t <- 120L
     obs_c <- matrix(stats::rpois(n_loc * n_t, 20), n_loc, n_t)
     est_c <- matrix(20, n_loc, n_t)
     obs_d <- matrix(stats::rpois(n_loc * n_t, 2), n_loc, n_t)
     est_d <- matrix(2, n_loc, n_t)

     ll_scalar <- calc_model_likelihood(obs_cases = obs_c, est_cases = est_c,
                                        obs_deaths = obs_d, est_deaths = est_d,
                                        nb_k_cases = 5, nb_k_deaths = 5)
     ll_vector <- calc_model_likelihood(obs_cases = obs_c, est_cases = est_c,
                                        obs_deaths = obs_d, est_deaths = est_d,
                                        nb_k_cases = rep(5, n_loc),
                                        nb_k_deaths = rep(5, n_loc))
     expect_equal(ll_scalar, ll_vector)

     # a wrong-length vector must ERROR, not silently collapse to max()
     expect_error(
          calc_model_likelihood(obs_cases = obs_c, est_cases = est_c,
                                obs_deaths = obs_d, est_deaths = est_d,
                                nb_k_cases = c(5, 10), nb_k_deaths = 5),
          "length 1 or n_locations")
})

test_that("per-location dispersion actually differentiates locations", {
     set.seed(4)
     n_loc <- 2L; n_t <- 120L
     obs_c <- matrix(stats::rpois(n_loc * n_t, 20), n_loc, n_t)
     est_c <- matrix(20, n_loc, n_t)
     obs_d <- matrix(0, n_loc, n_t); est_d <- matrix(0, n_loc, n_t)
     a <- calc_model_likelihood(obs_cases = obs_c, est_cases = est_c,
                                obs_deaths = obs_d, est_deaths = est_d,
                                nb_k_cases = c(1, 1), nb_k_deaths = Inf)
     b <- calc_model_likelihood(obs_cases = obs_c, est_cases = est_c,
                                obs_deaths = obs_d, est_deaths = est_d,
                                nb_k_cases = c(1, 50), nb_k_deaths = Inf)
     expect_false(isTRUE(all.equal(a, b)))
})

test_that("retired nb_k_min_* settings warn and are dropped", {
     ctrl <- mosaic_control_defaults()
     ctrl$likelihood$nb_k_min_cases <- 20
     expect_warning(out <- MOSAIC:::.mosaic_validate_and_merge_control(ctrl),
                    "RETIRED")
     expect_null(out$likelihood$nb_k_min_cases)
})

test_that("k never resolves to NA regardless of panel size", {
     # REGRESSION (red team, v0.92.0): the NA -> value rescue used to live only
     # in the cross-location shrinkage step, which returns early below 5
     # fittable locations. Every single-country run therefore leaked NA, and a
     # single NA makes the TOTAL log-likelihood -Inf for every simulation.
     skip_if_not(exists("config_default", where = asNamespace("MOSAIC")))
     cfg <- MOSAIC::config_default

     for (n in c(1L, 2L, 4L)) {
          r <- est_nb_dispersion(cfg$reported_deaths[seq_len(n), , drop = FALSE],
                                 cfg$reported_deaths_weight[seq_len(n), , drop = FALSE],
                                 date_start = cfg$date_start,
                                 location_name = cfg$location_name[seq_len(n)])
          expect_false(any(is.na(r$k)), info = paste("n_loc =", n))
     }

     # and with shrinkage disabled on the full panel
     rf <- est_nb_dispersion(cfg$reported_deaths, cfg$reported_deaths_weight,
                             date_start = cfg$date_start,
                             location_name = cfg$location_name, shrink = FALSE)
     expect_false(any(is.na(rf$k)))

     # a location that cannot be fitted at all, alone, must fall to Poisson
     i <- match("BFA", cfg$location_name)
     if (!is.na(i)) {
          r1 <- est_nb_dispersion(cfg$reported_deaths[i, , drop = FALSE],
                                  cfg$reported_deaths_weight[i, , drop = FALSE],
                                  date_start = cfg$date_start, location_name = "BFA")
          expect_false(is.na(r1$k))
     }
})

test_that("the resolver rejects an unusable dispersion loudly", {
     skip_if_not(exists("config_default", where = asNamespace("MOSAIC")))
     cfg <- MOSAIC::config_default
     ctrl <- mosaic_control_defaults()
     ctrl$likelihood$nb_k_cases <- c(NA_real_, rep(1, length(cfg$location_name) - 1L))
     expect_error(MOSAIC:::.mosaic_resolve_nb_dispersion(cfg, ctrl), "unusable dispersion")
})
