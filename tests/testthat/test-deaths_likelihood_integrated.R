# =============================================================================
# test-deaths_likelihood_integrated.R
#
# The deaths likelihood with the reported CFR integrated out (v0.96.0; score
# revised v0.97.0, yearly levels v0.97.2, forecast years v0.98.0/v0.99.0):
#   logit mu_jt = logit mu0_jt + a_j + sum_y B_y(t) delta_{j,y}
#   a ~ N(0, s^2), delta ~ N(0, sd_year^2) up to the latest observed year and
#   N(forecast shift, sd_year^2) after it (run_MOSAIC sets the shift to the
#   ensemble's mean latest-year deviation); B_y = yearly levels blended at 1 January
# scored weekly with a quasi-Poisson likelihood (Poisson / phi) plus an additive
# background, and marginalised by a Laplace step per location
# (calc_log_likelihood_deaths_integrated() and its run_MOSAIC() adapters), plus
# the post-hoc death redraw and the posterior shift of a config's mu_jt.
# =============================================================================

.d7_data <- function(n = 140, cfr = 0.02, seed = 1, start = "2024-01-01") {
  set.seed(seed)
  dates <- seq(as.Date(start), by = "day", length.out = n)
  expo <- rpois(n, 400) * 0.423 / 0.75
  list(dates = dates, expo = expo, obs = rpois(n, cfr * expo))
}

# Independently coded log joint density of (data, a, d) for one year of data:
# weekly Poisson / phi on expected + background, and the two normal priors.
.d7_logjoint <- function(a, d, obs, expo, dates, base, phi, bg, sa, sy) {
  blk <- as.integer(dates - dates[1]) %/% 7L
  D <- tapply(obs, blk, sum)
  m <- tapply(plogis(base + a + d) * expo, blk, sum) + bg
  sum(D * log(m) - m - lgamma(D + 1)) / phi +
    dnorm(a, 0, sa, log = TRUE) + dnorm(d, 0, sy, log = TRUE)
}

# A 20-day, one-location config for mocked-engine ensemble tests.
.d7_ens_cfg <- function() {
  list(location_name = "AAA", reported_cases = 10 + seq_len(20), reported_deaths = rep(1, 20),
       date_start = "2020-01-01", date_stop = "2020-01-20")
}

test_that("the Laplace marginal matches brute-force quadrature (Poisson and quasi-Poisson)", {
  x <- .d7_data()
  base <- qlogis(0.015)
  g <- seq(-3, 3, length.out = 241); h <- diff(g)[1]
  for (phi in c(1, 3)) {
    fit <- calc_log_likelihood_deaths_integrated(
      x$obs, x$expo, rep(base, 140), dates = x$dates, sd_shift = 0.5, sd_year = 0.7,
      dispersion = phi, background_rel = 0.02, week_offset = 0L)
    bg <- fit$background
    expect_equal(bg, max(1e-4, 0.02 * mean(tapply(x$obs, (0:139) %/% 7, sum))))
    M <- outer(g, g, Vectorize(function(a, d) .d7_logjoint(a, d, x$obs, x$expo, x$dates, base, phi, bg, 0.5, 0.7)))
    brute <- max(M) + log(sum(exp(M - max(M))) * h * h)
    expect_lt(abs(fit$ll - brute), 0.02, label = paste("|Laplace - quadrature|, phi =", phi))
    expect_true(fit$converged)
    # The mode is a stationary point of the independently coded density.
    th <- fit$theta[1, ]; e <- 1e-4
    f <- function(a, d) .d7_logjoint(a, d, x$obs, x$expo, x$dates, base, phi, bg, 0.5, 0.7)
    ga <- (f(th[1] + e, th[2]) - f(th[1] - e, th[2])) / (2 * e)
    gd <- (f(th[1], th[2] + e) - f(th[1], th[2] - e)) / (2 * e)
    expect_lt(max(abs(c(ga, gd))), 1e-4)
  }
})

test_that("the fitted CFR reproduces the observed deaths total whatever the weekly shape", {
  # The P1 regression: a negative-binomial score weights low-count weeks far
  # above the peak, so when the weekly ratio varies (it always does) the level
  # tracks the low weeks. The quasi-Poisson score is the Poisson score: with a
  # flat prior the fitted expected total equals the observed total.
  set.seed(4)
  n <- 280
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = n)
  expo <- c(rep(5, 140), rep(300, 140)) * 0.6
  cfr_true <- c(rep(0.01, 140), rep(0.04, 140))       # higher CFR in the wave
  obs <- rpois(n, cfr_true * expo)
  for (phi in c(1, 4)) {
    fit <- calc_log_likelihood_deaths_integrated(
      obs, expo, rep(qlogis(0.02), n), dates = dates, sd_shift = 50, sd_year = 1e-3,
      dispersion = phi, background_rel = 0, week_offset = 0L)
    cfr_hat <- plogis(qlogis(0.02) + fit$theta[1, 1] + fit$theta[1, 2])
    expect_equal(sum(cfr_hat * expo), sum(obs), tolerance = 1e-3, info = paste("phi", phi))
  }
})

test_that("the dispersion scales the likelihood and widens the posterior by sqrt(phi)", {
  x <- .d7_data(n = 364, seed = 9)
  args <- list(obs_deaths = x$obs, exposure = x$expo, base_logit = rep(qlogis(0.015), 364),
               dates = x$dates, sd_shift = 100, sd_year = 1e-3, background_rel = 0, week_offset = 0L)
  f1 <- do.call(calc_log_likelihood_deaths_integrated, c(args, dispersion = 1))
  f4 <- do.call(calc_log_likelihood_deaths_integrated, c(args, dispersion = 4))
  expect_equal(f4$theta[1, 1], f1$theta[1, 1], tolerance = 1e-6)        # same point estimate
  expect_equal(unname(f4$theta_sd[1, 1] / f1$theta_sd[1, 1]), 2, tolerance = 1e-3)
})

test_that("the year deviations are yearly levels, blended over 60 days at each 1 January", {
  years <- 2023:2025
  d <- as.numeric(seq(as.Date("2022-11-01"), as.Date("2026-02-28"), by = "day"))
  B <- MOSAIC:::.d7_basis(d, years)
  expect_equal(rowSums(B), rep(1, length(d)))
  mid24 <- d >= as.numeric(as.Date("2024-02-15")) & d <= as.numeric(as.Date("2024-11-15"))
  expect_true(all(B[mid24, 2] == 1))                              # a level inside the year
  jan <- which(d == as.numeric(as.Date("2024-01-01")))
  expect_equal(unname(B[jan, 1:2]), c(0.5, 0.5))
  expect_lt(max(abs(diff(B))), 1 / 60 + 1e-12)                    # continuous: no step
  expect_true(all(B[d < as.numeric(as.Date("2023-01-01")), 1] == 1))   # before: first year
  expect_true(all(B[d > as.numeric(as.Date("2026-01-01")), 3] == 1))   # after: last year
  # Years with a gap do not blend across it; an unlisted year takes the earlier level.
  G <- MOSAIC:::.d7_basis(d, c(2023L, 2025L))
  in24 <- d >= as.numeric(as.Date("2024-01-01")) & d <= as.numeric(as.Date("2024-12-31"))
  expect_true(all(G[in24, 1] == 1))
  expect_equal(MOSAIC:::.d7_basis(d, 2024L), matrix(1, length(d), 1))
})

test_that("a year observed only in part is forecast at its observed level, not an extrapolated trend", {
  # The v0.97.0 interpolated basis extrapolated the within-year trend: with the
  # true CFR 3% through 2024 and 1.5% in Jan-May 2025, it forecast Jun-Dec 2025 at
  # 1.3%, BELOW the fitted Jan-May level. A yearly level carries Jan-May forward.
  d_all <- seq(as.Date("2023-01-01"), as.Date("2025-12-31"), by = "day")
  X <- rep(200, length(d_all)); cfr_true <- ifelse(d_all < as.Date("2025-01-01"), 0.03, 0.015)
  set.seed(3); D <- rpois(length(d_all), cfr_true * X); D[d_all > as.Date("2025-05-31")] <- NA
  base <- rep(qlogis(0.03), length(d_all))
  fit <- calc_log_likelihood_deaths_integrated(D, X, base, d_all, sd_shift = 0.5, sd_year = 0.7,
                                               week_offset = 0L, years = 2023:2025)
  th <- fit$theta[1, ]
  cfr_fit <- plogis(base + th[1] + as.numeric(MOSAIC:::.d7_basis(as.numeric(d_all), 2023:2025) %*% th[-1]))
  obs_part <- mean(cfr_fit[d_all >= as.Date("2025-02-01") & d_all <= as.Date("2025-05-31")])
  rest     <- mean(cfr_fit[d_all >= as.Date("2025-06-01")])
  expect_equal(rest, obs_part, tolerance = 1e-8)
  expect_rel_equal(obs_part, 0.015, 0.15)
})

test_that("the level is recovered and the posterior SD is honest", {
  x <- .d7_data(n = 364, cfr = 0.03, seed = 7)
  fit <- calc_log_likelihood_deaths_integrated(
    x$obs, x$expo, rep(qlogis(0.015), 364), dates = x$dates, sd_shift = 1, sd_year = 0.7,
    week_offset = 0L)
  cfr_hat <- plogis(qlogis(0.015) + sum(fit$theta[1, ]))
  expect_rel_equal(cfr_hat, 0.03, 0.06)
  expect_true(all(fit$theta_sd[1, ] > 0 & fit$theta_sd[1, ] < c(1, 0.7)))
})

test_that("a forecast year is centred on the forecast shift; no scored weeks at all returns the prior", {
  x <- .d7_data()
  fit <- calc_log_likelihood_deaths_integrated(
    x$obs, x$expo, rep(qlogis(0.02), 140), dates = x$dates, sd_shift = 0.5, sd_year = 0.7,
    week_offset = 0L, years = c(2024L, 2026L))
  expect_identical(colnames(fit$theta), c("a", "y2024", "y2026"))
  # No blending across the missing 2025, so no data reach 2026: by default it
  # keeps N(0, sd_year^2), and a shift moves only its centre.
  expect_equal(unname(fit$theta[1, "y2026"]), 0)
  expect_equal(unname(fit$theta_sd[1, "y2026"]), 0.7, tolerance = 1e-8)
  sh <- calc_log_likelihood_deaths_integrated(
    x$obs, x$expo, rep(qlogis(0.02), 140), dates = x$dates, sd_shift = 0.5, sd_year = 0.7,
    week_offset = 0L, years = c(2024L, 2026L), forecast_shift = 0.4)
  expect_equal(unname(sh$theta[1, "y2026"]), 0.4, tolerance = 1e-8)
  expect_equal(unname(sh$theta_sd[1, "y2026"]), 0.7, tolerance = 1e-8)
  expect_equal(sh$ll, fit$ll, tolerance = 1e-10)
  expect_error(calc_log_likelihood_deaths_integrated(
    x$obs, x$expo, rep(qlogis(0.02), 140), dates = x$dates, sd_shift = 0.5, sd_year = 0.7,
    forecast_shift = c(0.1, 0.2)), "forecast_shift")

  none <- calc_log_likelihood_deaths_integrated(
    rep(NA_real_, 140), x$expo, rep(qlogis(0.02), 140), dates = x$dates, sd_shift = 0.5,
    sd_year = 0.7, week_offset = 0L)
  expect_identical(none$ll, 0)
  expect_identical(none$n_weeks, 0L)
  expect_equal(unname(none$theta_sd[1, ]), c(0.5, 0.7))
})

test_that("forecast years leave the marginal likelihood unchanged and move only their posterior", {
  x <- .d7_data()                                             # Jan-May 2024
  base <- rep(qlogis(0.01), 140)                              # the data run at 2x the prior
  one <- calc_log_likelihood_deaths_integrated(x$obs, x$expo, base, x$dates, sd_shift = 0.5,
                                               sd_year = 0.7, week_offset = 0L, years = 2024L)
  fc <- calc_log_likelihood_deaths_integrated(x$obs, x$expo, base, x$dates, sd_shift = 0.5,
                                              sd_year = 0.7, week_offset = 0L, years = 2024:2026,
                                              forecast_shift = 0.3)
  # Unscored years add a factor that integrates to 1, whatever its centre.
  expect_equal(fc$ll, one$ll, tolerance = 1e-9)
  expect_equal(unname(fc$theta[1, c("a", "y2024")]), unname(one$theta[1, ]), tolerance = 1e-7)
  # Each forecast year sits on the shift, one year-to-year SD wide, independent of
  # the observed years.
  expect_equal(unname(fc$theta[1, c("y2025", "y2026")]), c(0.3, 0.3), tolerance = 1e-8)
  V <- fc$vcov[[1]]
  expect_equal(c(V[3, 3], V[4, 4]), c(0.49, 0.49), tolerance = 1e-8)
  expect_equal(c(V[2, 3], V[2, 4], V[3, 4]), c(0, 0, 0), tolerance = 1e-10)
})

test_that("the forecast anchor is the latest year observed past its New Year blend", {
  yrs <- 2023:2027
  run_to <- function(stop) seq(as.Date("2023-03-01"), as.Date(stop), by = "day")
  expect_identical(MOSAIC:::.d7_forecast_years(run_to("2025-05-31"), yrs), list(anchor = 3L, forecast_years = 4:5))
  # Data reaching only into the blend of a new year leave the year before as anchor.
  expect_identical(MOSAIC:::.d7_forecast_years(run_to("2026-01-20"), yrs), list(anchor = 3L, forecast_years = 4:5))
  expect_identical(MOSAIC:::.d7_forecast_years(run_to("2026-01-31"), yrs), list(anchor = 4L, forecast_years = 5L))
  expect_identical(MOSAIC:::.d7_forecast_years(run_to("2027-06-30"), yrs), list(anchor = 5L, forecast_years = integer(0)))
  expect_identical(MOSAIC:::.d7_forecast_years(as.Date(character(0)), yrs),
                   list(anchor = NA_integer_, forecast_years = integer(0)))
  expect_identical(MOSAIC:::.d7_forecast_years(seq(as.Date("2023-01-01"), as.Date("2023-01-15"), by = "day"), yrs),
                   list(anchor = NA_integer_, forecast_years = integer(0)))
  # An unlisted anchor year maps to the listed year its level comes from.
  expect_identical(MOSAIC:::.d7_forecast_years(run_to("2024-06-30"), c(2023L, 2025L, 2026L)),
                   list(anchor = 1L, forecast_years = 2:3))
})

test_that("a year observed only inside its New Year blend is a forecast year fitted to those days", {
  # Data through 2025-01-20 at 3% against a 1.5% prior: 2025 has only blend days,
  # so 2024 stays the anchor and 2025 is a forecast year centred on the shift yet
  # still fitted to those days -- the one case where the shift enters the
  # marginal likelihood. Centred on 2024's own deviation, 2025 sits nearer
  # 2024's level than when it reverts to the prior.
  d_all <- seq(as.Date("2024-01-01"), as.Date("2025-12-31"), by = "day")
  X <- rep(10, length(d_all))
  set.seed(5); D <- rpois(length(d_all), 0.03 * X); D[d_all > as.Date("2025-01-20")] <- NA
  base <- rep(qlogis(0.015), length(d_all))
  setup <- MOSAIC:::.d7_setup(matrix(D, 1), NULL, d_all, 2024:2025, sd_shift = 0.3,
                              sd_year = 0.7, phi = 1, week_offset = 0L)
  expect_identical(setup$locs[[1]][c("anchor", "forecast_years", "forecast_shift")],
                   list(anchor = 1L, forecast_years = 2L, forecast_shift = 0))
  fit0 <- MOSAIC:::.d7_fit_all(setup, matrix(X, 1), matrix(base, 1), as.numeric(d_all))
  di <- MOSAIC:::.mosaic_set_forecast_shift(list(setup = setup), fit0$theta[1, "y2024"])
  fit_s <- MOSAIC:::.d7_fit_all(di$setup, matrix(X, 1), matrix(base, 1), as.numeric(d_all))
  lvl <- function(f, k) plogis(qlogis(0.015) + f$theta[1, "a"] + f$theta[1, k])
  expect_lt(abs(lvl(fit_s, "y2025") - lvl(fit_s, "y2024")),
            abs(lvl(fit0, "y2025") - lvl(fit0, "y2024")))
  expect_false(isTRUE(all.equal(fit_s$ll, fit0$ll)))
  expect_true(all(fit_s$converged))
})

test_that("weeks cut by the data edges are scored on their own days; interior gaps drop the week", {
  x <- .d7_data()
  args <- list(exposure = x$expo, base_logit = rep(qlogis(0.02), 140), dates = x$dates,
               sd_shift = 0.5, sd_year = 0.7, dispersion = 2, week_offset = 3L)
  full <- do.call(calc_log_likelihood_deaths_integrated, c(list(obs_deaths = x$obs), args))
  # 2024-01-01 is a Monday; with the week boundary on Thursday the data start and
  # end mid-week, and both edge weeks are scored on the days they have.
  expect_identical(full$n_weeks, 21L)
  # One missing day removes its whole week, exactly as zero weight on that week does.
  o <- x$obs; o[40] <- NA
  miss <- do.call(calc_log_likelihood_deaths_integrated, c(list(obs_deaths = o), args))
  blk <- (as.integer(x$dates - as.Date("2024-01-04")) %/% 7L)
  w <- rep(1, 140); w[blk == blk[40]] <- 0
  zero <- do.call(calc_log_likelihood_deaths_integrated, c(list(obs_deaths = x$obs, weights = w), args))
  expect_identical(miss$n_weeks, 20L)
  expect_equal(miss$ll, zero$ll, tolerance = 1e-10)
  expect_equal(miss$theta, zero$theta, tolerance = 1e-8)
})

test_that("a week with observed deaths but no onsets costs a bounded, data-scaled amount", {
  x <- .d7_data()
  expo <- x$expo; expo[1:14] <- 0
  obs <- x$obs; obs[1:14] <- 3
  args <- list(obs_deaths = obs, exposure = expo, base_logit = rep(qlogis(0.02), 140),
               dates = x$dates, sd_shift = 0.5, sd_year = 0.7, week_offset = 0L)
  f <- do.call(calc_log_likelihood_deaths_integrated, args)
  expect_true(is.finite(f$ll) && f$converged)
  ref <- do.call(calc_log_likelihood_deaths_integrated,
                 modifyList(args, list(obs_deaths = replace(obs, 1:14, NA))))
  # 42 deaths in two zero-onset weeks: well under the ~23 LL per death the old
  # 1e-10 floor charged (roughly -log(background) per death instead).
  expect_gt(f$ll - ref$ll, -42 * 8)
})

test_that("the deaths core replaces the floored NB: eps_rel_deaths and nb_k_deaths no longer matter", {
  # Evaluation F4: an eps floor must not survive into the integrated score.
  set.seed(2)
  nL <- 2; nT <- 70
  oc <- matrix(rpois(nL * nT, 50), nL); ec <- oc + 1
  od <- matrix(rpois(nL * nT, 0.3), nL); ed <- matrix(0.3, nL, nT)
  core <- c(-41.5, -37.25)
  ll <- function(eps, k) calc_model_likelihood(oc, ec, od, ed, weight_cases = 1, weight_deaths = 1,
                                               nb_k_cases = 10, eps_rel_deaths = eps, nb_k_deaths = k,
                                               ll_deaths_core = core)
  ref <- ll(0.25, 3)
  expect_identical(ll(0.001, 3), ref)
  expect_identical(ll(0.25, 0.5), ref)
  cases_only <- calc_model_likelihood(oc, ec, od, ed, weight_cases = 1, weight_deaths = 0,
                                      nb_k_cases = 10, nb_k_deaths = 3, ll_deaths_core = core)
  expect_equal(ref - cases_only, sum(core), tolerance = 1e-8)
  expect_error(calc_model_likelihood(oc, ec, od, ed, nb_k_cases = 10, nb_k_deaths = 3, ll_deaths_core = 1),
               "one value per location")
})

test_that("level-dependent deaths shape terms are dropped when the CFR is integrated out", {
  set.seed(3)
  nL <- 1; nT <- 120
  oc <- matrix(rpois(nT, 50), 1); ec <- oc
  od <- matrix(rpois(nT, 2), 1)
  ll <- function(ed) suppressWarnings(calc_model_likelihood(
    oc, ec, od, ed, weight_cases = 1, weight_deaths = 1, nb_k_cases = 10, nb_k_deaths = 3,
    ll_deaths_core = -50, weight_cumulative_total = 0.1, weight_wis = 0.1))
  # Engine deaths drawn at the prior CFR must not move the score.
  expect_identical(ll(matrix(1, 1, nT)), ll(matrix(20, 1, nT)))
  rm(list = intersect("deaths_shape_terms_integrated", ls(MOSAIC:::.mosaic_once)), envir = MOSAIC:::.mosaic_once)
  expect_warning(calc_model_likelihood(oc, ec, od, matrix(2, 1, nT), weight_cases = 1, weight_deaths = 1,
                                       nb_k_cases = 10, nb_k_deaths = 3, ll_deaths_core = -50,
                                       weight_wis = 0.1),
                 "shape terms are dropped")
})

test_that("the exported function refuses bad exposure and base_logit", {
  x <- .d7_data()
  bad <- x$expo; bad[5] <- -1
  expect_error(calc_log_likelihood_deaths_integrated(x$obs, bad, rep(-4, 140), x$dates, 0.5, 0.7),
               "finite and non-negative")
  bl <- rep(-4, 140); bl[3] <- NaN
  expect_error(calc_log_likelihood_deaths_integrated(x$obs, x$expo, bl, x$dates, 0.5, 0.7),
               "base_logit must be finite")
  expect_error(calc_log_likelihood_deaths_integrated(x$obs, x$expo, rep(-4, 140), x$dates, 0.5, 0.7,
                                                     dispersion = 0), "dispersion must be positive")
})

test_that("the dispersion estimate reads deaths against the observed cases and is clamped at 1", {
  set.seed(5)
  C <- rpois(104, 400); yr <- rep(2023:2024, each = 52)
  expect_lt(MOSAIC:::.d7_dispersion(rpois(104, 0.02 * C), C, yr), 1.4)   # Poisson data: ~1
  over <- rnbinom(104, mu = 0.02 * C, size = 2)
  expect_gt(MOSAIC:::.d7_dispersion(over, C, yr), 2)
  expect_equal(MOSAIC:::.d7_dispersion(c(1, 2, rep(0, 102)), C, yr), 1)       # too few deaths
})

test_that("the exposure alignment matches the engine: no death is reported without its onset", {
  cfg <- MOSAIC::config_simulation_epidemic
  cfg$mu_jt[] <- 0.3; cfg$rho_deaths <- 1; cfg$delta_reporting_cases <- 2L
  r <- MOSAIC::run_simulation(cfg, seed = 5L, quiet = TRUE)$results
  nL <- nrow(r$reported_deaths); nT <- ncol(r$reported_deaths)
  di <- list(n_time = nT, base_logit_full = matrix(qlogis(0.3), nL, nT),
             dates_full = as.Date(cfg$date_start) + seq_len(nT) - 1L)
  ex <- MOSAIC:::.mosaic_deaths_exposure(di, r$new_symptomatic, cfg)
  expect_gt(sum(r$reported_deaths), 100)
  # Reported deaths in column c are thinned from onsets in column c - s, s = lc + 1.
  expect_true(all(r$reported_deaths <= ex$X * cfg$chi_epidemic / cfg$rho + 1e-9))
  expect_true(all(ex$X[r$reported_deaths > 0] > 0))
  expect_equal(ex$onset_day, as.numeric(di$dates_full) - 3)
  # ... and the realized total matches the expectation the likelihood uses.
  expect_equal(sum(r$reported_deaths) / sum(0.3 * ex$X), 1, tolerance = 0.1)
  # The same bound fails one column either side, so the check has power.
  shift <- function(M, s) { out <- matrix(0, nrow(M), ncol(M)); out[, (s + 1):ncol(M)] <- M[, 1:(ncol(M) - s)]; out }
  O <- r$new_symptomatic
  expect_false(all(r$reported_deaths <= shift(O, 2L)))
  expect_false(all(r$reported_deaths <= shift(O, 4L)))
  expect_true(all(r$reported_deaths <= shift(O, 3L)))
  expect_error(MOSAIC:::.mosaic_deaths_exposure(di, O[, -1], cfg), "deaths integration expects")
})

test_that("the resolver reads the priors' mu_jt widths, estimates dispersion, and falls back with a warning", {
  cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = c("MOZ", "MWI"))
  pri <- MOSAIC::get_location_priors(c("MOZ", "MWI"), MOSAIC::priors_default)
  ctl <- list(likelihood = list())
  di <- MOSAIC:::.mosaic_resolve_deaths_integration(cfg, ctl, pri, score_window = NULL)
  se <- vapply(1:2, function(j) {
    obs_years <- unique(di$year_full[is.finite(cfg$reported_deaths[j, ])])
    L <- pri$mu_jt$location[[c("MOZ", "MWI")[j]]]; sqrt(mean(L$logit_se[L$year %in% obs_years]^2))
  }, numeric(1))
  expect_equal(di$sd_shift, unname(sqrt(pri$mu_jt$sd_product^2 + se^2)))
  expect_equal(di$sd_year, pri$mu_jt$sd_year)
  expect_equal(di$base_logit_full, qlogis(cfg$mu_jt))
  expect_true(all(di$dispersion >= 1))
  # Background = the cases channel's relative floor (eps_rel_cases, default 0.02).
  D1 <- di$setup$locs[[1]]$D
  expect_equal(di$background[1], max(1e-4, 0.02 * mean(D1)))
  di5 <- MOSAIC:::.mosaic_resolve_deaths_integration(cfg, list(likelihood = list(eps_rel_cases = 0.05)),
                                                     pri, score_window = NULL)
  expect_equal(di5$background[1], max(1e-4, 0.05 * mean(D1)))

  pri_old <- pri; pri_old$mu_jt <- NULL
  rm(list = intersect("mu_jt_prior_missing", ls(MOSAIC:::.mosaic_once)), envir = MOSAIC:::.mosaic_once)
  expect_warning(MOSAIC:::.mosaic_resolve_deaths_integration(cfg, ctl, pri_old, score_window = NULL),
                 "carries no `mu_jt` entry")
})

test_that("the resolver's confidence weights are mass-preserving and the deaths prefix is unscored", {
  cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = "MOZ")
  pri <- MOSAIC::get_location_priors("MOZ", MOSAIC::priors_default)
  nT <- ncol(cfg$reported_deaths)
  plain <- cfg; plain$reported_deaths_weight <- NULL
  a <- MOSAIC:::.mosaic_resolve_deaths_integration(plain, list(likelihood = list()), pri, NULL)
  down <- cfg; down$reported_deaths_weight <- matrix(0.5, 1, nT)
  b <- MOSAIC:::.mosaic_resolve_deaths_integration(down, list(likelihood = list()), pri, NULL)
  expect_equal(sum(b$setup$locs[[1]]$W), sum(a$setup$locs[[1]]$W), tolerance = 1e-12)
  late <- MOSAIC:::.mosaic_resolve_deaths_integration(
    cfg, list(likelihood = list()), pri,
    score_window = list(idx_cases = 31L, idx_deaths = 400L, n_time = nT))
  expect_identical(late$keep, 31:nT)
  expect_gte(min(late$keep[late$setup$locs[[1]]$day]), 400L)
})

test_that("the post-hoc redraw is reproducible, continuous, respects the engine bounds and leaves the caller's RNG alone", {
  cfg <- MOSAIC::config_simulation_epidemic
  cfg$mu_jt[] <- 0.05; cfg$delta_reporting_cases <- 1L
  r <- MOSAIC::run_simulation(cfg, seed = 4L, quiet = TRUE)$results
  nL <- nrow(r$new_symptomatic); nT <- ncol(r$new_symptomatic)
  cfg$reported_deaths <- r$reported_deaths
  pri <- list(mu_jt = list(sd_year = 0.7, sd_product = 0.3,
                           location = setNames(lapply(cfg$location_name, function(i)
                             list(year = 2020L, logit_mean = qlogis(0.05), logit_se = 0.2)), cfg$location_name)))
  di <- MOSAIC:::.mosaic_resolve_deaths_integration(cfg, list(likelihood = list()), pri, NULL)

  set.seed(99); before <- .Random.seed
  a <- MOSAIC:::.mosaic_posthoc_deaths(di, r, cfg, seed = 123L)
  expect_identical(.Random.seed, before)
  b <- MOSAIC:::.mosaic_posthoc_deaths(di, r, cfg, seed = 123L)
  expect_identical(a, b)
  c2 <- MOSAIC:::.mosaic_posthoc_deaths(di, r, cfg, seed = 124L)
  expect_false(identical(a$reported_deaths, c2$reported_deaths))

  expect_true(all(a$disease_deaths[, 1] == 0))
  expect_true(all(a$disease_deaths[, 2:nT] <= r$new_symptomatic[, 1:(nT - 1)]))
  expect_true(all(a$reported_deaths[, 2:nT] <= a$disease_deaths[, 1:(nT - 1)]))
  expect_identical(dim(a$cfr_year), c(nL, 1L))
  # Fitted to deaths simulated at 5%, the redrawn CFR stays near it.
  expect_true(all(abs(a$cfr_year - 0.05) < 0.03))
})

test_that("the redrawn deaths reproduce the observed total when cases are misfit", {
  # The ensemble-level symptom of P1: the old NB fit under-predicted deaths
  # totals for a path whose weekly shape differed from the data.
  cfg <- MOSAIC::config_simulation_epidemic
  cfg$mu_jt[] <- 0.15
  r <- MOSAIC::run_simulation(cfg, seed = 8L, quiet = TRUE)$results
  obs <- r$reported_deaths
  nT <- ncol(obs)
  obs[, 1:(nT %/% 2)] <- round(obs[, 1:(nT %/% 2)] * 0.3)   # early season under-reported
  cfg2 <- cfg; cfg2$reported_deaths <- obs
  pri <- list(mu_jt = list(sd_year = 0.7, sd_product = 5,
                           location = setNames(lapply(cfg$location_name, function(i)
                             list(year = 2020L, logit_mean = qlogis(0.15), logit_se = 0.1)), cfg$location_name)))
  di <- MOSAIC:::.mosaic_resolve_deaths_integration(cfg2, list(likelihood = list()), pri, NULL)
  expect_gt(sum(obs), 100)
  tot <- vapply(1:40, function(s) sum(MOSAIC:::.mosaic_posthoc_deaths(di, r, cfg2, seed = s)$reported_deaths), numeric(1))
  expect_equal(mean(tot) / sum(obs), 1, tolerance = 0.05)
})

test_that("the post-hoc redraw centres forecast years on the set shift, not on each path's own year", {
  # Steady onsets; deaths at 2.5% for onsets in 2020-2021 and 5% in 2022, observed
  # to 2022-09-30 against a 2.5% prior (so no scored day's blend reaches 2023).
  # Unset, the forecast years 2023-2024 revert toward the location's multi-year
  # level; with the shift set from the paths' 2022 deviation, they continue 2022's
  # 5%, and the observed years are untouched.
  cfg <- MOSAIC::config_simulation_endemic
  d <- seq(as.Date(cfg$date_start), as.Date(cfg$date_stop), by = "day")
  nL <- length(cfg$location_name); nT <- length(d)
  r <- list(new_symptomatic = matrix(300, nL, nT))
  src <- seq_len(nT) - (as.integer(cfg$delta_reporting_cases) + 1L); ok <- src >= 1L
  X <- matrix(0, nL, nT); X[, ok] <- cfg$rho / cfg$chi_epidemic * r$new_symptomatic[, src[ok]]
  cfr_true <- ifelse(format(d[pmax(src, 1L)], "%Y") == "2022", 0.05, 0.025)
  set.seed(21); obs <- matrix(rpois(nL * nT, sweep(X, 2L, cfr_true, `*`)), nL, nT)
  obs[, d > as.Date("2022-09-30")] <- NA
  cfg$reported_deaths <- obs; cfg$mu_jt[] <- 0.025
  pri <- list(mu_jt = list(sd_year = 0.7, sd_product = 0.3,
                           location = setNames(lapply(cfg$location_name, function(i)
                             list(year = 2020:2024, logit_mean = rep(qlogis(0.025), 5),
                                  logit_se = rep(0.2, 5))), cfg$location_name)))
  di <- MOSAIC:::.mosaic_resolve_deaths_integration(cfg, list(likelihood = list()), pri, NULL)
  expect_true(MOSAIC:::.mosaic_has_forecast_years(di))
  expect_true(all(vapply(di$setup$locs, function(L) L$anchor, integer(1)) == 3L))
  med_cfr <- function(di) {
    cy <- sapply(1:30, function(s) MOSAIC:::.mosaic_posthoc_deaths(di, r, cfg, seed = s)$cfr_year,
                 simplify = "array")                               # [loc x year x draw]
    apply(cy, c(1, 2), stats::median)
  }
  # Medians over 30 draws scatter by ~15% per cell (year-to-year SD 0.7), so the
  # checks average over locations and years.
  unset <- med_cfr(di)
  expect_lt(mean(unset[, c("2023", "2024")]), 0.035)              # reverts toward the multi-year level
  fit <- MOSAIC:::.mosaic_deaths_ll_integrated(di, r, cfg)
  shift <- MOSAIC:::.mosaic_anchor_deviation(di, fit$theta)
  expect_equal(shift, unname(fit$theta[, "y2022"]))
  carried <- med_cfr(MOSAIC:::.mosaic_set_forecast_shift(di, shift))
  expect_rel_equal(mean(carried[, "2022"]), 0.05, 0.1)
  expect_rel_equal(mean(carried[, c("2023", "2024")]), mean(carried[, "2022"]), 0.2)
  # Observed years are untouched: exactly for the first location, and up to
  # resampling noise for later ones, whose draws follow on the same random
  # stream after the earlier locations' (changed) forecast-year draws.
  expect_identical(carried[1, "2021"], unset[1, "2021"])
  expect_rel_equal(unname(carried[, "2021"]), unname(unset[, "2021"]), 0.1)
  expect_error(MOSAIC:::.mosaic_set_forecast_shift(di, 0.1), "location")
})

test_that("the ensemble's forecast shift is the weighted mean of its paths' latest-year deviations", {
  cfg <- .d7_ens_cfg()
  recs <- list(); k <- 0L
  dev <- c(0.2, -0.4, 1.0)                                        # per param set
  for (s in 1:2) for (p in 1:3) {
    k <- k + 1L
    recs[[k]] <- list(param_idx = p, stoch_idx = s, success = TRUE,
                      reported_cases = 10 + seq_len(20), reported_deaths = rep(1, 20),
                      cfr_year = matrix(0.02, 1, 1), cfr_infeasible = 0L,
                      anchor_dev = dev[p] + 0.1 * s)
  }
  local_mocked_ensemble_sims(recs)
  di <- list(setup = list(nL = 1L, locs = list(list(anchor = 1L, forecast_years = integer(0)))),
             n_time = 20L, years = 2020L, base_logit_full = matrix(qlogis(0.02), 1, 20),
             year_full = rep(2020L, 20))
  w <- c(0.6, 0.3, 0.1)
  ens <- calc_model_ensemble(config = cfg, configs = lapply(1:3, function(p) { x <- cfg; x$seed <- p; x }),
                             parameter_weights = w, n_simulations_per_config = 2L,
                             deaths_integration = di, verbose = FALSE)
  want <- sum(rep(w, 2) / 2 * c(dev + 0.1, dev + 0.2))
  expect_equal(ens$forecast_shift, want, tolerance = 1e-12)
})

test_that("the posterior shift puts each year's mean CFR on target and is continuous", {
  cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = c("MOZ", "MWI"))
  d <- seq(as.Date(cfg$date_start), as.Date(cfg$date_stop), by = "day")
  yrs <- sort(unique(as.integer(format(d, "%Y"))))
  post <- expand.grid(year = yrs, location = cfg$location_name, stringsAsFactors = FALSE)
  set.seed(1); post$cfr_median <- runif(nrow(post), 0.005, 0.04)
  out <- MOSAIC:::.mosaic_apply_cfr_posterior(cfg, post)
  for (i in 1:2) {
    got <- tapply(out$mu_jt[i, ], format(d, "%Y"), mean)
    want <- post$cfr_median[post$location == cfg$location_name[i]]
    expect_equal(unname(as.numeric(got)), want, tolerance = 1e-8)
    jumps <- abs(diff(qlogis(out$mu_jt[i, ])))
    expect_lt(max(jumps), 0.06)                        # blended: no step at a year boundary
  }
  # A location the posterior does not cover keeps its mu_jt exactly.
  out2 <- MOSAIC:::.mosaic_apply_cfr_posterior(cfg, post[post$location == "MOZ", ])
  expect_identical(out2$mu_jt[2, ], cfg$mu_jt[2, ])
  expect_error(MOSAIC:::.mosaic_apply_cfr_posterior(cfg, post[, c("location", "year")]),
               "must be a data frame")
})
