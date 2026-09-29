# =============================================================================
# test-cfr_calibrated_and_asof.R
#
# Two ways the reported CFR mu_jt reaches a simulation outside the calibration
# loop:
#  1. .mosaic_apply_cfr_posterior(): run_MOSAIC() moves config_medoid.json's
#     mu_jt from the prior to the run's posterior (cfr_posterior), so a later
#     re-simulation of that config draws deaths at the calibrated level.
#  2. .rcv_cfr_asof() / .rcv_apply_cfr_asof(): run_rolling_cv() rebuilds mu_jt
#     and its prior per cutoff from WHO annual years <= year(T) - 1 only.
# =============================================================================

.post_for <- function(cfg, shift = log(2)) {
  mu <- MOSAIC:::.mosaic_config_mu_jt(cfg, length(cfg$location_name), ncol(cfg$mu_jt))
  d <- seq(as.Date(cfg$date_start), as.Date(cfg$date_stop), by = "day")
  yrs <- sort(unique(as.integer(format(d, "%Y"))))
  do.call(rbind, lapply(seq_along(cfg$location_name), function(i) do.call(rbind, lapply(yrs, function(y) {
    pm <- plogis(mean(qlogis(mu[i, format(d, "%Y") == y])))
    data.frame(location = cfg$location_name[i], year = y, prior_median = pm,
               cfr_median = plogis(qlogis(pm) + shift * i), cfr_lower = NA, cfr_upper = NA)
  }))))
}

test_that("the posterior shift moves each location-year on the logit scale and keeps the shape", {
  cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = c("MOZ", "MWI"))
  post <- .post_for(cfg, shift = 0.4)
  out <- MOSAIC:::.mosaic_apply_cfr_posterior(cfg, post)
  expect_identical(dim(out$mu_jt), dim(cfg$mu_jt))
  expect_equal(qlogis(out$mu_jt[1, ]) - qlogis(cfg$mu_jt[1, ]), rep(0.4, ncol(cfg$mu_jt)), tolerance = 1e-10)
  expect_equal(qlogis(out$mu_jt[2, ]) - qlogis(cfg$mu_jt[2, ]), rep(0.8, ncol(cfg$mu_jt)), tolerance = 1e-10)
  # A year-specific shift applies to that year only.
  post2 <- post; post2$cfr_median[post2$location == "MOZ" & post2$year == 2024] <-
    plogis(qlogis(post2$prior_median[post2$location == "MOZ" & post2$year == 2024]) - 1)
  out2 <- MOSAIC:::.mosaic_apply_cfr_posterior(cfg, post2)
  yr <- format(seq(as.Date(cfg$date_start), as.Date(cfg$date_stop), by = "day"), "%Y")
  expect_equal(unique(round(qlogis(out2$mu_jt[1, yr == "2024"]) - qlogis(cfg$mu_jt[1, yr == "2024"]), 10)), -1)
  expect_equal(unique(round(qlogis(out2$mu_jt[1, yr == "2025"]) - qlogis(cfg$mu_jt[1, yr == "2025"]), 10)), 0.4)
  # A location or year the posterior does not cover keeps the prior.
  out3 <- MOSAIC:::.mosaic_apply_cfr_posterior(cfg, post[post$location == "MOZ", ])
  expect_identical(out3$mu_jt[2, ], cfg$mu_jt[2, ])
})

test_that("a re-simulation of the shifted config realizes the posterior CFR", {
  cfg <- MOSAIC::config_simulation_epidemic
  cfg$mu_jt[] <- 0.02; cfg$chi_endemic <- cfg$chi_epidemic
  post <- .post_for(cfg, shift = 0)
  post$cfr_median <- 0.08
  out <- MOSAIC:::.mosaic_apply_cfr_posterior(cfg, post)
  expect_true(all(abs(out$mu_jt - 0.08) < 1e-12))
  d <- 0; cs <- 0
  for (s in 1:3) {
    r <- MOSAIC::run_simulation(out, seed = s, quiet = TRUE)$results
    d <- d + sum(r$reported_deaths); cs <- cs + sum(r$reported_cases)
  }
  expect_equal(d / cs, 0.08, tolerance = 0.12)
})

test_that("a legacy config comes back in the v0.96.0 form", {
  cfg <- MOSAIC::config_simulation_epidemic
  leg <- cfg; leg$mu_jt <- matrix(0.5, nrow(cfg$mu_jt), ncol(cfg$mu_jt))
  leg$mu_j_baseline <- rep(0.004, 3); leg$CFR_target <- c(0.01, 0.02, 0.03); leg$delta_reporting_deaths <- 5
  post <- suppressWarnings(.post_for(leg, shift = 0))
  out <- suppressWarnings(MOSAIC:::.mosaic_apply_cfr_posterior(leg, post))
  for (f in c("mu_j_baseline", "CFR_target", "delta_reporting_deaths")) expect_null(out[[f]], info = f)
  expect_equal(out$mu_jt[, 1], c(0.01, 0.02, 0.03), tolerance = 1e-12)
  expect_silent(MOSAIC::run_simulation(out, seed = 1L, quiet = TRUE))
})

test_that("a posterior CFR the reporting parameters cannot produce is refused, not clamped", {
  cfg <- MOSAIC::config_simulation_epidemic
  post <- .post_for(cfg, shift = 0); post$cfr_median <- 0.9
  expect_error(MOSAIC:::.mosaic_apply_cfr_posterior(cfg, post), "per-onset fatality probability >= 1")
  expect_error(MOSAIC:::.mosaic_apply_cfr_posterior(cfg, post[, c("location", "year")]), "must be a data frame")
})

# ---- rolling-CV as-of estimates -------------------------------------------

skip_if_not_installed("mgcv")

.who_synth <- function(seed = 1, years = 2000:2025, bump_after = NULL, bump = 0) {
  set.seed(seed)
  isos <- MOSAIC::iso_codes_mosaic[1:10]
  base <- stats::setNames(qlogis(0.02) + rnorm(length(isos), 0, 0.5), isos)
  rows <- expand.grid(iso_code = isos, year = years, stringsAsFactors = FALSE)
  rows$cases_total <- rpois(nrow(rows), 3000)
  eta <- base[rows$iso_code] + rnorm(nrow(rows), 0, 0.6)
  if (!is.null(bump_after)) eta[rows$year > bump_after] <- eta[rows$year > bump_after] + bump
  rows$deaths_total <- rbinom(nrow(rows), rows$cases_total, plogis(eta))
  rows$country <- rows$iso_code
  rows
}

test_that("as-of estimates ignore every year after the cutoff's last usable year", {
  a <- MOSAIC:::.rcv_cfr_asof(.who_synth(), last_year = 2022L, cfg_stop = "2026-06-30")
  b <- MOSAIC:::.rcv_cfr_asof(.who_synth(bump_after = 2022L, bump = 2.5),
                              last_year = 2022L, cfg_stop = "2026-06-30")
  expect_identical(a$last_data_year, 2022L)
  expect_equal(a$predictions, b$predictions)
  expect_identical(a$sigma, b$sigma)
  p <- a$predictions[a$predictions$iso_code == MOSAIC::iso_codes_mosaic[1], ]
  expect_identical(max(p$year), 2026L)
  expect_true(all(p$logit_mean[p$year > 2022] == p$logit_mean[p$year == 2022]))
})

test_that("the as-of mu_jt is flat after the last usable year's 1 July and drops legacy fields", {
  asof <- MOSAIC:::.rcv_cfr_asof(.who_synth(), last_year = 2023L, cfg_stop = "2025-12-31")
  cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = MOSAIC::iso_codes_mosaic[1])
  cfg$CFR_target <- 0.02
  dates <- seq(as.Date("2023-01-01"), as.Date("2025-12-31"), by = "day")
  out <- MOSAIC:::.rcv_apply_cfr_asof(cfg, asof, dates)
  expect_null(out$CFR_target)
  expect_identical(dim(out$mu_jt), c(1L, length(dates)))
  after <- dates >= as.Date("2023-07-01")
  expect_true(all(out$mu_jt[1, after] == out$mu_jt[1, which(after)[1]]))
  pri <- MOSAIC:::.mosaic_mu_jt_prior(asof$predictions, cfg$location_name, asof$sigma, asof$tau)
  L <- pri$location[[cfg$location_name]]
  expect_equal(qlogis(out$mu_jt[1, match(as.Date("2023-07-01"), dates)]),
               L$logit_mean[L$year == 2023], tolerance = 1e-10)
})
