# =============================================================================
# test-deaths_likelihood_integrated.R
#
# The deaths likelihood with the reported CFR integrated out (v0.96.0):
#   logit mu_jt = logit mu0_jt + a_j + delta_{j,y(t)},  a ~ N(0, s^2), delta ~ N(0, sd_year^2)
# scored weekly with a negative binomial and marginalised by a Laplace step per
# location (calc_log_likelihood_deaths_integrated() and its run_MOSAIC()
# adapters), plus the post-hoc death redraw the ensemble uses.
# =============================================================================

.d7_data <- function(n = 140, cfr = 0.02, seed = 1, start = "2024-01-01") {
  set.seed(seed)
  dates <- seq(as.Date(start), by = "day", length.out = n)
  expo <- rpois(n, 400) * 0.423 / 0.75
  list(dates = dates, expo = expo, obs = rpois(n, cfr * expo))
}

# Independently coded log joint density of (data, a, d) for one year of data.
.d7_logjoint <- function(a, d, obs, expo, dates, base, k, sa, sy) {
  blk <- as.integer(dates - dates[1]) %/% 7L
  D <- tapply(obs, blk, sum)
  m <- tapply(plogis(base + a + d) * expo, blk, sum)
  ll <- if (is.infinite(k)) sum(dpois(D, m, log = TRUE)) else sum(dnbinom(D, size = k, mu = m, log = TRUE))
  ll + dnorm(a, 0, sa, log = TRUE) + dnorm(d, 0, sy, log = TRUE)
}

test_that("the Laplace marginal matches brute-force quadrature (Poisson and NB)", {
  x <- .d7_data()
  base <- qlogis(0.015)
  g <- seq(-3, 3, length.out = 241); h <- diff(g)[1]
  for (k in c(Inf, 5)) {
    fit <- calc_log_likelihood_deaths_integrated(
      x$obs, x$expo, rep(base, 140), rep(2024L, 140), x$dates,
      sd_shift = 0.5, sd_year = 0.7, k = k, week_offset = 0L)
    M <- outer(g, g, Vectorize(function(a, d) .d7_logjoint(a, d, x$obs, x$expo, x$dates, base, k, 0.5, 0.7)))
    brute <- max(M) + log(sum(exp(M - max(M))) * h * h)
    expect_lt(abs(fit$ll - brute), 0.02, label = paste("|Laplace - quadrature|, k =", k))
    expect_true(fit$converged)
    # The mode is a stationary point of the independently coded density.
    th <- fit$theta[1, ]; e <- 1e-4
    ga <- (.d7_logjoint(th[1] + e, th[2], x$obs, x$expo, x$dates, base, k, 0.5, 0.7) -
           .d7_logjoint(th[1] - e, th[2], x$obs, x$expo, x$dates, base, k, 0.5, 0.7)) / (2 * e)
    gd <- (.d7_logjoint(th[1], th[2] + e, x$obs, x$expo, x$dates, base, k, 0.5, 0.7) -
           .d7_logjoint(th[1], th[2] - e, x$obs, x$expo, x$dates, base, k, 0.5, 0.7)) / (2 * e)
    expect_lt(max(abs(c(ga, gd))), 1e-4)
  }
})

test_that("the level is recovered and the posterior SD is honest", {
  x <- .d7_data(n = 364, cfr = 0.03, seed = 7)
  fit <- calc_log_likelihood_deaths_integrated(
    x$obs, x$expo, rep(qlogis(0.015), 364), rep(2024L, 364), x$dates,
    sd_shift = 1, sd_year = 0.7, k = Inf, week_offset = 0L)
  cfr_hat <- plogis(qlogis(0.015) + sum(fit$theta[1, ]))
  expect_equal(cfr_hat, 0.03, tolerance = 0.06)
  expect_true(all(fit$theta_sd[1, ] > 0 & fit$theta_sd[1, ] < c(1, 0.7)))
})

test_that("a year with no scored weeks keeps its prior; no scored weeks at all returns the prior", {
  x <- .d7_data()
  fit <- calc_log_likelihood_deaths_integrated(
    x$obs, x$expo, rep(qlogis(0.02), 140), rep(2024L, 140), x$dates,
    sd_shift = 0.5, sd_year = 0.7, k = Inf, week_offset = 0L, years = c(2024L, 2025L))
  expect_identical(colnames(fit$theta), c("a", "y2024", "y2025"))
  expect_equal(unname(fit$theta[1, "y2025"]), 0)
  expect_equal(unname(fit$theta_sd[1, "y2025"]), 0.7, tolerance = 1e-8)

  none <- calc_log_likelihood_deaths_integrated(
    rep(NA_real_, 140), x$expo, rep(qlogis(0.02), 140), rep(2024L, 140), x$dates,
    sd_shift = 0.5, sd_year = 0.7, k = Inf, week_offset = 0L)
  expect_identical(none$ll, 0)
  expect_identical(none$n_weeks, 0L)
  expect_equal(unname(none$theta_sd[1, ]), c(0.5, 0.7))
})

test_that("only complete, positively weighted weeks are scored", {
  x <- .d7_data()
  args <- list(exposure = x$expo, base_logit = rep(qlogis(0.02), 140), year = rep(2024L, 140),
               dates = x$dates, sd_shift = 0.5, sd_year = 0.7, k = 4, week_offset = 0L)
  full <- do.call(calc_log_likelihood_deaths_integrated, c(list(obs_deaths = x$obs), args))
  expect_identical(full$n_weeks, 20L)
  # One missing day removes its whole week, exactly as a zero weight on that week does.
  o <- x$obs; o[10] <- NA
  miss <- do.call(calc_log_likelihood_deaths_integrated, c(list(obs_deaths = o), args))
  w <- rep(1, 140); w[8:14] <- 0
  zero <- do.call(calc_log_likelihood_deaths_integrated, c(list(obs_deaths = x$obs, weights = w), args))
  expect_identical(miss$n_weeks, 19L)
  expect_equal(miss$ll, zero$ll, tolerance = 1e-10)
  expect_equal(miss$theta, zero$theta, tolerance = 1e-8)
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

test_that("the exposure alignment matches the engine: no death is reported without its onset", {
  cfg <- MOSAIC::config_simulation_epidemic
  cfg$mu_jt[] <- 0.3; cfg$rho_deaths <- 1; cfg$delta_reporting_cases <- 2L
  r <- MOSAIC::run_simulation(cfg, seed = 5L, quiet = TRUE)$results
  nL <- nrow(r$reported_deaths); nT <- ncol(r$reported_deaths)
  di <- list(n_time = nT, base_logit_full = matrix(qlogis(0.3), nL, nT),
             year_full = rep(2020L, nT))
  ex <- MOSAIC:::.mosaic_deaths_exposure(di, r$new_symptomatic, cfg)
  expect_gt(sum(r$reported_deaths), 100)
  # Reported deaths in column c are thinned from onsets in column c - s, s = lc + 1.
  expect_true(all(r$reported_deaths <= ex$X * cfg$chi_epidemic / cfg$rho + 1e-9))
  expect_true(all(ex$X[r$reported_deaths > 0] > 0))
  # ... and the realized total matches the expectation the likelihood uses.
  expect_equal(sum(r$reported_deaths) / sum(0.3 * ex$X), 1, tolerance = 0.1)
  # The same bound fails one column either side, so the check has power.
  shift <- function(M, s) { out <- matrix(0, nrow(M), ncol(M)); out[, (s + 1):ncol(M)] <- M[, 1:(ncol(M) - s)]; out }
  O <- r$new_symptomatic
  expect_false(all(r$reported_deaths <= shift(O, 2L)))
  expect_false(all(r$reported_deaths <= shift(O, 4L)))
  expect_true(all(r$reported_deaths <= shift(O, 3L)))
})

test_that("the resolver reads the priors' mu_jt widths and falls back with a warning", {
  cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = c("MOZ", "MWI"))
  pri <- MOSAIC::get_location_priors(c("MOZ", "MWI"), MOSAIC::priors_default)
  ctl <- list(likelihood = list())
  di <- MOSAIC:::.mosaic_resolve_deaths_integration(cfg, ctl, pri, score_window = NULL)
  yrs <- di$years
  se <- vapply(c("MOZ", "MWI"), function(iso) {
    L <- pri$mu_jt$location[[iso]]; sqrt(mean(L$logit_se[L$year %in% yrs]^2))
  }, numeric(1))
  expect_equal(di$sd_shift, unname(sqrt(pri$mu_jt$sd_product^2 + se^2)))
  expect_equal(di$sd_year, pri$mu_jt$sd_year)
  expect_equal(di$base_logit_full, qlogis(cfg$mu_jt))

  pri_old <- pri; pri_old$mu_jt <- NULL
  rm(list = intersect("mu_jt_prior_missing", ls(MOSAIC:::.mosaic_once)), envir = MOSAIC:::.mosaic_once)
  expect_warning(MOSAIC:::.mosaic_resolve_deaths_integration(cfg, ctl, pri_old, score_window = NULL),
                 "carries no `mu_jt` entry")
})

test_that("the resolver zeroes deaths before the deaths scoring start", {
  cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = "MOZ")
  pri <- MOSAIC::get_location_priors("MOZ", MOSAIC::priors_default)
  nT <- ncol(cfg$reported_deaths)
  all_w <- MOSAIC:::.mosaic_resolve_deaths_integration(cfg, list(likelihood = list()), pri, NULL)
  late <- MOSAIC:::.mosaic_resolve_deaths_integration(
    cfg, list(likelihood = list()), pri,
    score_window = list(idx_cases = 31L, idx_deaths = 400L, n_time = nT))
  expect_identical(late$keep, 31:nT)
  expect_lt(length(late$setup$locs[[1]]$D), length(all_w$setup$locs[[1]]$D))
  first_scored <- min(late$keep[late$setup$locs[[1]]$day])
  expect_gte(first_scored, 400L)
})

test_that("the post-hoc redraw is reproducible, respects the engine bounds and leaves the caller's RNG alone", {
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
