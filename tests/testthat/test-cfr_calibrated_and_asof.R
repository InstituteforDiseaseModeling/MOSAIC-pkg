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

# A posterior whose yearly targets are the yearly mean CFR of the prior shifted by
# `shift * i` on the logit scale for location i (so the exact answer is known).
.post_for <- function(cfg, shift = log(2)) {
  nT <- as.integer(as.Date(cfg$date_stop) - as.Date(cfg$date_start)) + 1L
  mu <- MOSAIC:::.mosaic_config_mu_jt(cfg, length(cfg$location_name), nT)
  d <- seq(as.Date(cfg$date_start), as.Date(cfg$date_stop), by = "day")
  yr <- format(d, "%Y"); yrs <- sort(unique(as.integer(yr)))
  do.call(rbind, lapply(seq_along(cfg$location_name), function(i) do.call(rbind, lapply(yrs, function(y) {
    sel <- yr == y
    data.frame(location = cfg$location_name[i], year = y,
               prior_cfr = mean(mu[i, sel]),
               cfr_median = mean(plogis(qlogis(mu[i, sel]) + shift * i)), cfr_lower = NA, cfr_upper = NA)
  }))))
}

test_that("a uniform posterior shift is recovered exactly and keeps the prior's shape", {
  cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = c("MOZ", "MWI"))
  out <- MOSAIC:::.mosaic_apply_cfr_posterior(cfg, .post_for(cfg, shift = 0.4))
  expect_identical(dim(out$mu_jt), dim(cfg$mu_jt))
  expect_equal(qlogis(out$mu_jt[1, ]) - qlogis(cfg$mu_jt[1, ]), rep(0.4, ncol(cfg$mu_jt)), tolerance = 1e-6)
  expect_equal(qlogis(out$mu_jt[2, ]) - qlogis(cfg$mu_jt[2, ]), rep(0.8, ncol(cfg$mu_jt)), tolerance = 1e-6)
})

test_that("a change in one year's posterior moves that year's mean and stays continuous", {
  cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = "MOZ")
  post <- .post_for(cfg, shift = 0)
  post$cfr_median[post$year == 2024] <- post$cfr_median[post$year == 2024] * 2
  out <- MOSAIC:::.mosaic_apply_cfr_posterior(cfg, post)
  d <- seq(as.Date(cfg$date_start), as.Date(cfg$date_stop), by = "day")
  got <- tapply(out$mu_jt[1, ], format(d, "%Y"), mean)
  expect_equal(unname(as.numeric(got)), post$cfr_median, tolerance = 1e-8)
  expect_lt(max(abs(diff(qlogis(out$mu_jt[1, ])))), 0.02)
  # The shift peaks inside 2024 and fades towards the neighbouring anchors.
  sh <- qlogis(out$mu_jt[1, ]) - qlogis(cfg$mu_jt[1, ])
  expect_gt(max(sh[format(d, "%Y") == "2024"]), max(abs(sh[format(d, "%Y") == "2026"])))
})

test_that("a re-simulation of the shifted config realizes the posterior CFR", {
  cfg <- MOSAIC::config_simulation_epidemic
  cfg$mu_jt[] <- 0.02; cfg$chi_endemic <- cfg$chi_epidemic
  post <- .post_for(cfg, shift = 0)
  post$cfr_median <- 0.08
  out <- MOSAIC:::.mosaic_apply_cfr_posterior(cfg, post)
  expect_true(all(abs(out$mu_jt - 0.08) < 1e-9))
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

test_that("config_medoid.json gets the medoid's own posterior, else the run's, else the prior", {
  cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = "MOZ")
  post_run <- .post_for(cfg, shift = 0.3)
  post_med <- .post_for(cfg, shift = -0.2)
  f <- withr::local_tempfile(fileext = ".json")

  out <- MOSAIC:::.mosaic_write_config_medoid(cfg, post_med, post_run, f)
  expect_true(file.exists(f))
  back <- MOSAIC::read_json_to_list(f)
  expect_equal(unname(as.matrix(back$mu_jt)), unname(out$mu_jt), tolerance = 1e-12)
  expect_equal(qlogis(out$mu_jt[1, ]) - qlogis(cfg$mu_jt[1, ]), rep(-0.2, ncol(cfg$mu_jt)), tolerance = 1e-6)

  out_run <- MOSAIC:::.mosaic_write_config_medoid(cfg, NULL, post_run, f)
  expect_equal(qlogis(out_run$mu_jt[1, ]) - qlogis(cfg$mu_jt[1, ]), rep(0.3, ncol(cfg$mu_jt)), tolerance = 1e-6)

  out_none <- MOSAIC:::.mosaic_write_config_medoid(cfg, NULL, NULL, f)
  expect_identical(out_none$mu_jt, cfg$mu_jt)

  bad <- post_med; bad$cfr_median <- 0.9          # infeasible: refused, prior kept
  warned <- character(0)
  out_bad <- MOSAIC:::.mosaic_write_config_medoid(cfg, bad, post_run, f,
                                                  log_warn = function(...) warned <<- c(warned, sprintf(...)))
  expect_identical(out_bad$mu_jt, cfg$mu_jt)
  expect_match(warned, "keeps the prior mu_jt")
})
