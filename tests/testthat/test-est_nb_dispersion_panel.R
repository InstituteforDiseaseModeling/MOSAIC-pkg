# =============================================================================
# test-est_nb_dispersion_panel.R
#
# v0.101.0 dispersion rules:
#   (a) k from OBSERVED weeks only -- reconstructed (who_catchup_*) and imputed
#       (fourier_*) weeks are left out of the fit (config$reported_tier);
#   (b) censored k -- a location whose fit collapses (theta run to the zero
#       boundary) or that has too few observed weeks takes the cross-country
#       panel trend, at every scale. No global floor raise, no per-country values.
# =============================================================================

.cd_rows <- function(iso, from = 46L) {
     cd <- MOSAIC::config_default
     i <- match(iso, cd$location_name)
     keep <- from:ncol(cd$reported_cases)
     list(obs = cd$reported_cases[i, keep, drop = FALSE],
          w = cd$reported_cases_weight[i, keep, drop = FALSE],
          date_start = as.Date(cd$date_start) + from - 1L, iso = iso)
}
.trend_k <- function(m) {
     tr <- MOSAIC:::.NB_DISP_PANEL_TREND
     exp(tr$intercept + tr$slope * log(m))
}

test_that("the shipped panel trend is the one config_default implies (drift guard)", {
     # A rebuild of config_default's surveillance must re-derive the constants:
     # MOSAIC:::.nb_disp_panel_trend_fit(MOSAIC::config_default) and paste the
     # intercept/slope/sigma/n into .NB_DISP_PANEL_TREND (R/est_nb_dispersion.R).
     fit <- MOSAIC:::.nb_disp_panel_trend_fit(MOSAIC::config_default, burn_in_days = 45L)
     tr <- MOSAIC:::.NB_DISP_PANEL_TREND
     expect_equal(fit$intercept, tr$intercept, tolerance = 1e-6)
     expect_equal(fit$slope, tr$slope, tolerance = 1e-6)
     expect_equal(fit$sigma, tr$sigma, tolerance = 1e-6)
     expect_identical(as.integer(fit$n), tr$n)
})

test_that("collapsed fits take the panel trend: CMR 1.41, UGA 1.00, ZAF 1.20", {
     cd <- MOSAIC::config_default
     keep <- 46:ncol(cd$reported_cases)
     tab <- MOSAIC::est_nb_dispersion(cd$reported_cases[, keep], cd$reported_cases_weight[, keep],
                                      date_start = as.Date(cd$date_start) + 45L,
                                      location_name = cd$location_name,
                                      panel_trend = MOSAIC:::.NB_DISP_PANEL_TREND)
     three <- c("CMR", "UGA", "ZAF")
     r <- tab[match(three, tab$location), ]
     expect_identical(r$status, rep("no_estimate_se_degenerate", 3))
     expect_true(all(r$panel_trend))
     expect_true(all(is.na(r$k_raw)))
     expect_equal(r$k, .trend_k(r$mean_weekly), tolerance = 1e-12)
     expect_equal(r$k, c(1.413199, 0.997813, 1.201759), tolerance = 1e-5)
     # nothing else collapses, and identified fits sit far above the rule's threshold
     expect_identical(sort(tab$location[tab$panel_trend]), three)
     ok <- is.finite(tab$k_raw) & is.finite(tab$se)
     expect_true(all(tab$se[ok] / pmax(tab$k_raw[ok], 0.1) > 0.05))
})

test_that("the panel trend gives the same k at every scale", {
     for (iso in c("CMR", "UGA", "ZAF")) {
          x <- .cd_rows(iso)
          alone <- MOSAIC::est_nb_dispersion(x$obs, x$w, date_start = x$date_start, location_name = iso,
                                             panel_trend = MOSAIC:::.NB_DISP_PANEL_TREND)
          expect_identical(alone$status, "no_estimate_se_degenerate", label = iso)
          expect_true(alone$panel_trend)
          expect_equal(alone$k, .trend_k(alone$mean_weekly), tolerance = 1e-12)
          noshrink <- MOSAIC::est_nb_dispersion(x$obs, x$w, date_start = x$date_start, shrink = FALSE,
                                                panel_trend = MOSAIC:::.NB_DISP_PANEL_TREND)
          expect_identical(noshrink$k, alone$k)
          # without a panel trend a lone collapsed location has nothing to borrow
          bare <- MOSAIC::est_nb_dispersion(x$obs, x$w, date_start = x$date_start)
          expect_false(bare$panel_trend)
          expect_true(is.infinite(bare$k))
     }
     # national, regional and full-panel resolutions agree for CMR
     ctl <- mosaic_control_defaults(); ctl$likelihood$burn_in_days <- 45L
     k_of <- function(isos) {
          cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = isos)
          sw <- MOSAIC:::.mosaic_resolve_score_window(cfg, ctl)
          r <- MOSAIC:::.mosaic_resolve_nb_dispersion(cfg, ctl, score_window = sw)
          r$cases$k[match("CMR", cfg$location_name)]
     }
     k1 <- k_of("CMR")
     expect_equal(k1, 1.413199, tolerance = 1e-5)
     expect_identical(k_of(c("CMR", "KEN", "MOZ")), k1)
     expect_identical(k_of(MOSAIC::config_default$location_name), k1)
})

# Weekly totals around a seasonal mean, spread over Monday-Sunday days.
.mk_series <- function(n_weeks = 160L, k = 0.6, seed = 3L) {
     set.seed(seed)
     mondays <- as.Date("2023-01-02") + 7L * (seq_len(n_weeks) - 1L)
     mu <- 60 * exp(1.1 * sin(2 * pi * seq_len(n_weeks) / 52)) + 4
     Y <- stats::rnbinom(n_weeks, mu = mu, size = k)
     list(mondays = mondays, Y = Y, daily = MOSAIC::downscale_weekly_values(mondays, Y)$value)
}

test_that("reconstructed and imputed weeks are left out of the dispersion fit", {
     s <- .mk_series()
     n <- length(s$daily)
     # a WHO catch-up window: weeks 60-84 replaced by their total spread evenly
     win <- 60:84
     Yr <- s$Y; Yr[win] <- round(sum(s$Y[win]) / length(win))
     daily_r <- MOSAIC::downscale_weekly_values(s$mondays, Yr)$value
     tier <- rep(1L, n); tier[rep(win, each = 7) * 7 - 6 + rep(0:6, length(win))] <- 2L
     with_all <- MOSAIC::est_nb_dispersion(matrix(daily_r, 1L), date_start = s$mondays[1],
                                           shrink = FALSE)
     with_obs <- MOSAIC::est_nb_dispersion(matrix(daily_r, 1L), date_start = s$mondays[1],
                                           shrink = FALSE, obs_tier = matrix(tier, 1L))
     # the flat window reads as low noise and inflates k when it is fitted
     expect_gt(with_all$k, with_obs$k)
     expect_identical(with_obs$n_weeks_excluded, length(win))
     expect_identical(with_obs$n_weeks, length(s$Y) - length(win))
     # identical to fitting the observed weeks alone (the excluded days set to NA)
     na_r <- daily_r; na_r[tier != 1L] <- NA
     only_obs <- MOSAIC::est_nb_dispersion(matrix(na_r, 1L), date_start = s$mondays[1], shrink = FALSE)
     expect_equal(with_obs$k_raw, only_obs$k_raw, tolerance = 1e-12)
     # ...except the level, which describes every scored week
     expect_equal(with_obs$mean_weekly, mean(Yr), tolerance = 1e-12)
     # imputed (tier 3) weeks are excluded the same way; an all-observed tier is a no-op
     tier3 <- tier; tier3[tier3 == 2L] <- 3L
     expect_equal(MOSAIC::est_nb_dispersion(matrix(daily_r, 1L), date_start = s$mondays[1], shrink = FALSE,
                                            obs_tier = matrix(tier3, 1L))$k, with_obs$k)
     expect_identical(MOSAIC::est_nb_dispersion(matrix(daily_r, 1L), date_start = s$mondays[1], shrink = FALSE,
                                                obs_tier = matrix(1L, 1L, n)),
                      with_all)
})

test_that("an outbreak known only from reconstructed weeks takes the panel trend, not Poisson", {
     # ZAF/BFA on the regenerated surveillance: enough cases overall, almost none
     # in observed weeks. The observed weeks cannot estimate k; that is not
     # evidence for the Poisson limit, which would be the tightest kernel there is.
     n_weeks <- 150L
     mondays <- as.Date("2023-01-02") + 7L * (seq_len(n_weeks) - 1L)
     Y <- rep(0, n_weeks); Y[c(30, 90)] <- 2; Y[40:68] <- 40      # outbreak in a catch-up window
     daily <- MOSAIC::downscale_weekly_values(mondays, Y)$value
     tier <- rep(1L, length(daily)); tier[(39 * 7 + 1):(68 * 7)] <- 2L
     r <- MOSAIC::est_nb_dispersion(matrix(daily, 1L), date_start = mondays[1], obs_tier = matrix(tier, 1L),
                                    panel_trend = MOSAIC:::.NB_DISP_PANEL_TREND)
     expect_identical(r$status, "no_estimate_observed_insufficient")
     expect_true(r$panel_trend)
     expect_equal(r$mean_weekly, mean(Y), tolerance = 1e-12)
     expect_equal(r$k, .trend_k(mean(Y)), tolerance = 1e-12)
     # a genuinely sparse series stays Poisson whatever its tiers
     Ys <- rep(0, n_weeks); Ys[c(10, 50)] <- 3
     ds <- MOSAIC::downscale_weekly_values(mondays, Ys)$value
     rs <- MOSAIC::est_nb_dispersion(matrix(ds, 1L), date_start = mondays[1], obs_tier = matrix(tier, 1L),
                                     panel_trend = MOSAIC:::.NB_DISP_PANEL_TREND)
     expect_identical(rs$status, "poisson_insufficient_data")
     expect_true(is.infinite(rs$k))
     expect_false(rs$panel_trend)
})

test_that("the resolver reads config$reported_tier and returns the week boundaries", {
     cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = c("KEN", "MOZ"))
     ctl <- mosaic_control_defaults()
     sw <- MOSAIC:::.mosaic_resolve_score_window(cfg, ctl)
     r0 <- MOSAIC:::.mosaic_resolve_nb_dispersion(cfg, ctl, score_window = sw)
     expect_false(r0$tier_used)
     expect_identical(r0$cases$week_offset, c(0L, 0L))
     expect_true(all(r0$table$n_weeks_excluded == 0L))
     # mark KEN's first 40 scored weeks as reconstructed
     tier <- matrix(1L, nrow(cfg$reported_cases), ncol(cfg$reported_cases))
     tier[!is.finite(cfg$reported_cases) & !is.finite(cfg$reported_deaths)] <- NA_integer_
     tier[match("KEN", cfg$location_name), 1:(30 + 7 * 41)] <- 2L
     cfg_t <- cfg; cfg_t$reported_tier <- tier
     r1 <- MOSAIC:::.mosaic_resolve_nb_dispersion(cfg_t, ctl, score_window = sw)
     expect_true(r1$tier_used)
     ken <- r1$table[r1$table$channel == "cases" & r1$table$location == "KEN", ]
     expect_gt(ken$n_weeks_excluded, 30L)
     expect_false(isTRUE(all.equal(ken$k_raw, r0$table$k_raw[r0$table$channel == "cases" &
                                                                 r0$table$location == "KEN"])))
     # get_location_config keeps the tiers row-aligned
     cfg_full <- MOSAIC::config_default
     cfg_full$reported_tier <- matrix(seq_len(length(cfg_full$reported_cases)) %% 3L + 1L,
                                      nrow(cfg_full$reported_cases))
     one <- MOSAIC::get_location_config(cfg_full, iso = "KEN")
     expect_identical(one$reported_tier,
                      cfg_full$reported_tier[match("KEN", cfg_full$location_name), , drop = FALSE])
     # a misaligned tier matrix is an error, not a silent mismatch
     bad <- cfg; bad$reported_tier <- tier[, -1]
     expect_error(MOSAIC:::.mosaic_resolve_nb_dispersion(bad, ctl, score_window = sw), "must be aligned")
     # a user override still reports the week boundaries, with a compatible table
     ctl_o <- ctl; ctl_o$likelihood$nb_k_cases <- 2
     ro <- suppressMessages(MOSAIC:::.mosaic_resolve_nb_dispersion(cfg, ctl_o, score_window = sw))
     expect_identical(ro$cases$week_offset, c(0L, 0L))
     expect_identical(names(ro$table), names(r0$table))
})

test_that("the cases scoring knob defaults to weekly and is validated", {
     expect_identical(mosaic_control_defaults()$likelihood$cases_scoring, "weekly")
     ctl <- mosaic_control_defaults(); ctl$likelihood$cases_scoring <- "hourly"
     expect_error(MOSAIC:::.mosaic_validate_and_merge_control(ctl), "cases_scoring")
     ctl$likelihood$cases_scoring <- "daily"
     expect_identical(MOSAIC:::.mosaic_validate_and_merge_control(ctl)$likelihood$cases_scoring, "daily")
})

test_that("the likelihood implementation stamp marks the weekly cases score", {
     # Resume refuses to pool shards across this stamp (test-run_MOSAIC_resume.R).
     expect_match(MOSAIC:::.mosaic_likelihood_impl_version(), "v0\\.101\\.0")
     expect_false(identical(MOSAIC:::.mosaic_likelihood_impl_version(), "R/v0.100.0+review_likelihood"))
})
