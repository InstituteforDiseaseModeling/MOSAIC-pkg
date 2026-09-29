# =============================================================================
# test-deaths_likelihood_integrated.R
#
# The deaths likelihood with the reported CFR integrated out (v0.96.0; score
# revised v0.97.0):
#   logit mu_jt = logit mu0_jt + a_j + sum_y B_y(t) delta_{j,y}
#   a ~ N(0, s^2), delta ~ N(0, sd_year^2), B_y = linear between 1 July anchors
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

test_that("the year deviations are continuous and flat beyond the first and last anchors", {
  anchors <- as.numeric(as.Date(c("2023-07-01", "2024-07-01", "2025-07-01")))
  d <- as.numeric(seq(as.Date("2023-01-01"), as.Date("2025-12-31"), by = "day"))
  B <- MOSAIC:::.d7_basis(d, anchors)
  expect_equal(rowSums(B), rep(1, length(d)))
  expect_true(all(B[d <= anchors[1], 1] == 1))
  expect_true(all(B[d >= anchors[3], 3] == 1))
  expect_lt(max(abs(diff(B))), 1 / 360)                # no step anywhere, incl. 1 January
  mid <- which(d == as.numeric(as.Date("2024-01-01")))
  expect_equal(unname(B[mid, 1:2]), c(182, 184) / 366, tolerance = 1e-12)
  expect_equal(MOSAIC:::.d7_basis(d, anchors[2]), matrix(1, length(d), 1))
})

test_that("the level is recovered and the posterior SD is honest", {
  x <- .d7_data(n = 364, cfr = 0.03, seed = 7)
  fit <- calc_log_likelihood_deaths_integrated(
    x$obs, x$expo, rep(qlogis(0.015), 364), dates = x$dates, sd_shift = 1, sd_year = 0.7,
    week_offset = 0L)
  cfr_hat <- plogis(qlogis(0.015) + sum(fit$theta[1, ]))
  expect_equal(cfr_hat, 0.03, tolerance = 0.06)
  expect_true(all(fit$theta_sd[1, ] > 0 & fit$theta_sd[1, ] < c(1, 0.7)))
})

test_that("a year with no scored weeks keeps its prior; no scored weeks at all returns the prior", {
  x <- .d7_data()
  fit <- calc_log_likelihood_deaths_integrated(
    x$obs, x$expo, rep(qlogis(0.02), 140), dates = x$dates, sd_shift = 0.5, sd_year = 0.7,
    week_offset = 0L, years = c(2024L, 2026L))
  expect_identical(colnames(fit$theta), c("a", "y2024", "y2026"))
  # 2024 data lie before the 2024 anchor, so the 2026 deviation is untouched.
  expect_equal(unname(fit$theta[1, "y2026"]), 0)
  expect_equal(unname(fit$theta_sd[1, "y2026"]), 0.7, tolerance = 1e-8)

  none <- calc_log_likelihood_deaths_integrated(
    rep(NA_real_, 140), x$expo, rep(qlogis(0.02), 140), dates = x$dates, sd_shift = 0.5,
    sd_year = 0.7, week_offset = 0L)
  expect_identical(none$ll, 0)
  expect_identical(none$n_weeks, 0L)
  expect_equal(unname(none$theta_sd[1, ]), c(0.5, 0.7))
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
  obs_years <- unique(di$year_full[colSums(is.finite(cfg$reported_deaths)) > 0])
  se <- vapply(c("MOZ", "MWI"), function(iso) {
    L <- pri$mu_jt$location[[iso]]; sqrt(mean(L$logit_se[L$year %in% obs_years]^2))
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
    expect_lt(max(jumps), 0.02)                        # no step at a year boundary
  }
  # A location the posterior does not cover keeps its mu_jt exactly.
  out2 <- MOSAIC:::.mosaic_apply_cfr_posterior(cfg, post[post$location == "MOZ", ])
  expect_identical(out2$mu_jt[2, ], cfg$mu_jt[2, ])
  expect_error(MOSAIC:::.mosaic_apply_cfr_posterior(cfg, post[, c("location", "year")]),
               "must be a data frame")
})
