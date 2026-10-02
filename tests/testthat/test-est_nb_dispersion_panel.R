# =============================================================================
# test-est_nb_dispersion_panel.R
#
# v0.101.0 dispersion rules:
#   (a) k from OBSERVED weeks only -- reconstructed (who_catchup_*) and imputed
#       (fourier_*) weeks are left out of the fit (config$reported_tier);
#   (b) censored k -- a location whose fit collapses (theta run to the zero
#       boundary), is clamped at the lower bound, or that has too few observed
#       weeks takes the cross-country panel trend, at every scale. No global
#       floor raise, no per-country values.
# =============================================================================

# config_default rows on the scored window from day `from` (burn_in_days = 45),
# with the trust tiers when the config carries them.
.cd_rows <- function(iso, from = 46L) {
     cd <- MOSAIC::config_default
     i <- match(iso, cd$location_name)
     keep <- from:ncol(cd$reported_cases)
     list(obs = cd$reported_cases[i, keep, drop = FALSE],
          w = cd$reported_cases_weight[i, keep, drop = FALSE],
          tier = if (is.null(cd$reported_tier)) NULL else cd$reported_tier[i, keep, drop = FALSE],
          date_start = as.Date(cd$date_start) + from - 1L, iso = iso)
}
.trend_k <- function(m) {
     tr <- MOSAIC:::.NB_DISP_PANEL_TREND
     pmin(pmax(exp(tr$intercept + tr$slope * log(m)), 0.1), 1e5)
}
# The table the shipped trend is fitted from: per-location fits on the scored
# window, no panel trend, no shrinkage (observed weeks only under reported_tier).
.panel_table <- function() MOSAIC:::.nb_disp_panel_trend_fit(MOSAIC::config_default, burn_in_days = 45L)$table
# Locations that take the panel trend: no estimate of their own, or a fit
# clamped at the lower bound (censored, not a measurement).
.no_own <- function(tab) (grepl("^no_estimate", tab$status) | tab$status %in% "clamped_lower_bound") &
     is.finite(tab$mean_weekly) & tab$mean_weekly > 0

test_that("the shipped panel trend is the one config_default implies (drift guard)", {
     # A rebuild of config_default's surveillance must re-derive the constants:
     # MOSAIC:::.nb_disp_panel_trend_fit(MOSAIC::config_default, 45L) and paste the
     # intercept/slope/sigma/n into .NB_DISP_PANEL_TREND (R/est_nb_dispersion.R,
     # whose rebuild recipe names the two tests below as well). This test fails
     # by design until then.
     fit <- MOSAIC:::.nb_disp_panel_trend_fit(MOSAIC::config_default, burn_in_days = 45L)
     tr <- MOSAIC:::.NB_DISP_PANEL_TREND
     expect_equal(fit$intercept, tr$intercept, tolerance = 1e-6)
     expect_equal(fit$slope, tr$slope, tolerance = 1e-6)
     expect_equal(fit$sigma, tr$sigma, tolerance = 1e-6)
     expect_identical(as.integer(fit$n), tr$n)
})

test_that("locations without an estimate of their own take the panel trend", {
     # The locations and their k come from config_default's own panel table, so a
     # rebuild of its surveillance changes which locations, not the rule. (On
     # config_default v6.1: BFA, CIV and ZAF, too few observed weeks, CMR, fit
     # collapsed to the zero boundary, and UGA, fit clamped at the lower bound,
     # at k 0.89, 0.98, 1.10, 1.65 and 0.96; on v6.0: CMR, UGA and ZAF at 1.41,
     # 1.00 and 1.20.)
     t0 <- .panel_table()
     no_own <- .no_own(t0)
     skip_if_not(any(no_own), "every config_default location has a dispersion estimate of its own")
     x <- .cd_rows(MOSAIC::config_default$location_name)
     tab <- MOSAIC::est_nb_dispersion(x$obs, x$w, date_start = x$date_start, location_name = x$iso,
                                      obs_tier = x$tier, panel_trend = MOSAIC:::.NB_DISP_PANEL_TREND)
     # the fits are per location, so the statuses are the panel table's
     expect_identical(tab$status, t0$status)
     expect_identical(tab$panel_trend, no_own)
     # k_raw describes the fit: none without an estimate, the bound for a clamped fit
     no_est  <- no_own & grepl("^no_estimate", t0$status)
     clamped <- no_own & t0$status %in% "clamped_lower_bound"
     expect_true(all(is.na(tab$k_raw[no_est])))
     expect_true(all(tab$k_raw[clamped] == 0.1))
     expect_equal(tab$k[no_own], .trend_k(tab$mean_weekly[no_own]), tolerance = 1e-12)
     # identified fits sit far above the collapse rule's threshold
     ok <- is.finite(tab$k_raw) & is.finite(tab$se)
     expect_true(all(tab$se[ok] / pmax(tab$k_raw[ok], 0.1) > 0.05))
})

test_that("the panel trend gives the same k at every scale", {
     t0 <- .panel_table()
     no_own <- which(.no_own(t0))
     skip_if_not(length(no_own) > 0L, "every config_default location has a dispersion estimate of its own")
     for (i in no_own) {
          iso <- t0$location[i]
          x <- .cd_rows(iso)
          alone <- MOSAIC::est_nb_dispersion(x$obs, x$w, date_start = x$date_start, location_name = iso,
                                             obs_tier = x$tier, panel_trend = MOSAIC:::.NB_DISP_PANEL_TREND)
          expect_identical(alone$status, t0$status[i], label = iso)
          expect_true(alone$panel_trend)
          expect_equal(alone$k, .trend_k(t0$mean_weekly[i]), tolerance = 1e-12)
          noshrink <- MOSAIC::est_nb_dispersion(x$obs, x$w, date_start = x$date_start, shrink = FALSE,
                                                obs_tier = x$tier, panel_trend = MOSAIC:::.NB_DISP_PANEL_TREND)
          expect_identical(noshrink$k, alone$k)
          # without a panel trend a lone location with no estimate has nothing to
          # borrow, and a lone clamped fit keeps the bound
          bare <- MOSAIC::est_nb_dispersion(x$obs, x$w, date_start = x$date_start, obs_tier = x$tier)
          expect_false(bare$panel_trend)
          if (grepl("^no_estimate", t0$status[i])) expect_true(is.infinite(bare$k), label = iso)
          else expect_identical(bare$k, 0.1, label = iso)
     }
     # national, regional and full-panel resolutions agree for the first of them
     iso <- t0$location[no_own[1]]
     ctl <- mosaic_control_defaults(); ctl$likelihood$burn_in_days <- 45L
     k_of <- function(isos) {
          cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = isos)
          sw <- MOSAIC:::.mosaic_resolve_score_window(cfg, ctl)
          r <- MOSAIC:::.mosaic_resolve_nb_dispersion(cfg, ctl, score_window = sw)
          r$cases$k[match(iso, cfg$location_name)]
     }
     k1 <- k_of(iso)
     expect_equal(k1, .trend_k(t0$mean_weekly[no_own[1]]), tolerance = 1e-12)
     expect_identical(k_of(c(iso, setdiff(c("KEN", "MOZ", "ETH"), iso)[1:2])), k1)
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

test_that("a fit clamped at the lower bound takes the panel trend at every scale; an own estimate does not", {
     # v0.101.0 final review F1: a clamped fit is censored, not a measurement (on
     # UGA-like series the fit returns the bound whatever the true k), so with a
     # panel trend it takes the trend, as a location with no estimate does.
     # Location A: weekly NB with a true k of 0.05 around a flat mean, which the
     # fit reports at the 0.1 bound with an ordinary SE (not a collapse). The
     # trend is chosen so the expected k is known by hand: 0.5 * sqrt(mean weekly).
     n_weeks <- 150L
     mondays <- as.Date("2023-01-02") + 7L * (seq_len(n_weeks) - 1L)
     set.seed(1)
     YA <- stats::rnbinom(n_weeks, mu = 8, size = 0.05)
     expect_equal(sum(YA), 961)                         # fixture: mean weekly 961 / 150
     dA <- matrix(MOSAIC::downscale_weekly_values(mondays, YA)$value, 1L)
     tr <- list(intercept = log(0.5), slope = 0.5)
     k_hand <- 0.5 * sqrt(961 / 150)                    # 1.26556...
     a <- MOSAIC::est_nb_dispersion(dA, date_start = mondays[1], panel_trend = tr)
     expect_identical(a$status, "clamped_lower_bound")  # status describes the fit,
     expect_identical(a$k_raw, 0.1)
     expect_gt(a$se / a$k_raw, 0.05)                    # (an identified fit, not a collapse)
     expect_true(a$panel_trend)                         # panel_trend the k used
     expect_equal(a$k, k_hand, tolerance = 1e-12)
     expect_equal(a$k, 1.2655697004, tolerance = 1e-9)
     expect_identical(MOSAIC::est_nb_dispersion(dA, date_start = mondays[1], panel_trend = tr,
                                                shrink = FALSE)$k, a$k)
     # without a panel trend the clamped fit keeps the bound
     bare <- MOSAIC::est_nb_dispersion(dA, date_start = mondays[1])
     expect_false(bare$panel_trend)
     expect_identical(bare$k, 0.1)
     # In a panel with active shrinkage (five own estimates) A is a trend taker:
     # never shrunk toward the run's own trend, which would have moved it off the
     # bound to a blend, and the five keep exactly the k they have without it.
     five <- do.call(rbind, lapply(11:15, function(sd) .mk_series(n_weeks = n_weeks, k = 1.5, seed = sd)$daily))
     p6 <- MOSAIC::est_nb_dispersion(rbind(dA, five), date_start = mondays[1], panel_trend = tr)
     expect_gte(attr(p6, "shrinkage")$n_fit, 5L)
     expect_identical(p6$status[1], "clamped_lower_bound")
     expect_identical(p6$panel_trend, c(TRUE, rep(FALSE, 5)))
     expect_identical(p6$k[1], a$k)
     p5 <- MOSAIC::est_nb_dispersion(five, date_start = mondays[1], panel_trend = tr)
     expect_identical(p6$k[-1], p5$k)
     shrunk <- MOSAIC:::.nb_disp_shrink(p6$mean_weekly, p6$k_raw, se = p6$se, identified = p6$identified,
                                        clamped = p6$status %in% "clamped_lower_bound")$k[1]
     expect_gt(abs(log(shrunk) - log(a$k)), 0.1)
     # an own estimate is never routed: alone it keeps its fit, in the panel its shrunk fit
     expect_true(all(grepl("^ok", p6$status[-1])))
     expect_false(any(p6$panel_trend[-1]))
     b <- MOSAIC::est_nb_dispersion(five[1, , drop = FALSE], date_start = mondays[1], panel_trend = tr)
     expect_false(b$panel_trend)
     expect_identical(b$k, b$k_raw)
     expect_gt(abs(log(b$k) - log(0.5 * sqrt(b$mean_weekly))), 0.1)
})

test_that("config_default's clamped cases fits take the shipped panel trend in every scope", {
     # UGA on config_default v6.1 (clamped_lower_bound, k_raw 0.1): the trend's
     # 0.96 alone, beside data-rich locations with shrinkage active, and in the
     # full panel, where it previously resolved to 0.100, 0.105 and 0.108. The
     # locations come from the panel table, so a rebuild changes which, not the rule.
     t0 <- .panel_table()
     cl <- which(t0$status %in% "clamped_lower_bound" & is.finite(t0$mean_weekly) & t0$mean_weekly > 0)
     skip_if_not(length(cl) > 0L, "no config_default location has a clamped cases fit")
     ctl <- mosaic_control_defaults(); ctl$likelihood$burn_in_days <- 45L
     row_of <- function(isos, iso) {
          cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = isos)
          sw <- MOSAIC:::.mosaic_resolve_score_window(cfg, ctl)
          tb <- MOSAIC:::.mosaic_resolve_nb_dispersion(cfg, ctl, score_window = sw)$table
          tb[tb$channel == "cases" & tb$location == iso, ]
     }
     for (i in cl) {
          iso <- t0$location[i]
          k_exp <- .trend_k(t0$mean_weekly[i])
          with_rich <- c(iso, setdiff(c("KEN", "MOZ", "ETH", "ZMB", "NGA", "COD", "MWI"), iso)[1:6])
          # shrinkage is active among the others in that set
          x <- .cd_rows(with_rich)
          sh <- MOSAIC::est_nb_dispersion(x$obs, x$w, date_start = x$date_start, location_name = x$iso,
                                          obs_tier = x$tier, panel_trend = MOSAIC:::.NB_DISP_PANEL_TREND)
          expect_gte(attr(sh, "shrinkage")$n_fit, 5L)
          for (isos in list(iso, with_rich, MOSAIC::config_default$location_name)) {
               r <- row_of(isos, iso)
               lab <- sprintf("%s in a %d-location run", iso, length(isos))
               expect_identical(r$status, "clamped_lower_bound", label = lab)
               expect_identical(r$k_raw, 0.1, label = lab)
               expect_true(r$panel_trend, label = lab)
               expect_equal(r$k, k_exp, tolerance = 1e-12, label = lab)
          }
     }
})

test_that("the resolver reads config$reported_tier and returns the week boundaries", {
     cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = c("KEN", "MOZ"))
     ctl <- mosaic_control_defaults()
     sw <- MOSAIC:::.mosaic_resolve_score_window(cfg, ctl)
     # config_default carries reported_tier (since v6.1), so the shipped config
     # is scored on observed weeks; the baseline below is the same config without it
     expect_true(MOSAIC:::.mosaic_resolve_nb_dispersion(cfg, ctl, score_window = sw)$tier_used)
     cfg$reported_tier <- NULL
     r0 <- MOSAIC:::.mosaic_resolve_nb_dispersion(cfg, ctl, score_window = sw)
     expect_false(r0$tier_used)
     expect_false(r0$cases$tier_used); expect_false(r0$deaths$tier_used)
     expect_identical(r0$cases$week_offset, c(0L, 0L))
     expect_true(all(r0$table$n_weeks_excluded == 0L))
     # mark KEN's first 40 scored weeks as reconstructed
     tier <- matrix(1L, nrow(cfg$reported_cases), ncol(cfg$reported_cases))
     tier[!is.finite(cfg$reported_cases) & !is.finite(cfg$reported_deaths)] <- NA_integer_
     tier[match("KEN", cfg$location_name), 1:(30 + 7 * 41)] <- 2L
     cfg_t <- cfg; cfg_t$reported_tier <- tier
     r1 <- MOSAIC:::.mosaic_resolve_nb_dispersion(cfg_t, ctl, score_window = sw)
     expect_true(r1$tier_used)
     expect_true(r1$cases$tier_used); expect_true(r1$deaths$tier_used)
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
     # ... and does not claim observed weeks for the channel it replaces (CD-6):
     # tier_used is per channel, and the top-level flag is the cases channel's,
     # which the run log reports for the cases likelihood
     ro_t <- suppressMessages(MOSAIC:::.mosaic_resolve_nb_dispersion(cfg_t, ctl_o, score_window = sw))
     expect_false(ro_t$cases$tier_used)
     expect_false(ro_t$tier_used)
     expect_true(ro_t$deaths$tier_used)
})

test_that("a location sparse over every tier reports no excluded weeks (CD-6)", {
     # The Poisson limit for sparse data is not a fit, so no week was left out of one.
     n_weeks <- 30L
     mondays <- as.Date("2023-01-02") + 7L * (seq_len(n_weeks) - 1L)
     Y <- rep(0, n_weeks); Y[c(3, 17)] <- 2
     daily <- MOSAIC::downscale_weekly_values(mondays, Y)$value
     tier <- rep(1L, length(daily)); tier[1:70] <- 2L           # ten reconstructed weeks
     r <- MOSAIC::est_nb_dispersion(matrix(daily, 1L), date_start = mondays[1], obs_tier = matrix(tier, 1L))
     expect_identical(r$status, "poisson_insufficient_data")
     expect_true(is.infinite(r$k))
     expect_identical(r$n_weeks, 30L)
     expect_identical(r$n_weeks_excluded, 0L)
     # an estimated location still reports its exclusions
     s <- .mk_series()
     tier_s <- rep(1L, length(s$daily)); tier_s[1:70] <- 2L
     rs <- MOSAIC::est_nb_dispersion(matrix(s$daily, 1L), date_start = s$mondays[1],
                                     obs_tier = matrix(tier_s, 1L), shrink = FALSE)
     expect_identical(rs$n_weeks_excluded, 10L)
     expect_identical(rs$n_weeks + rs$n_weeks_excluded, length(s$Y))
})

test_that("deaths with too few observed weeks keep the every-week estimate, not Poisson (LIK-4)", {
     # Deaths have no panel trend. Under obs_tier a location whose observed weeks
     # are too few has no estimate of its own, which alone (or with fewer than
     # five estimated locations) fell to the Poisson limit -- the tightest kernel,
     # and not what the observations show. The integrated deaths likelihood keeps
     # the every-week estimate of its phi in that case; so does the deaths k.
     s <- .mk_series(n_weeks = 150L, k = 0.6, seed = 5)
     tier <- rep(2L, length(s$daily)); tier[1:70] <- 1L          # ten observed weeks only
     d0 <- s$mondays[1]
     o <- matrix(s$daily, 1L)
     with_tier <- MOSAIC::est_nb_dispersion(o, date_start = d0, obs_tier = matrix(tier, 1L))
     expect_identical(with_tier$status, "no_estimate_observed_insufficient")
     expect_true(is.infinite(with_tier$k))
     every <- MOSAIC::est_nb_dispersion(o, date_start = d0)
     expect_true(is.finite(every$k))
     dk <- MOSAIC:::.nb_disp_deaths(o, date_start = d0, obs_tier = matrix(tier, 1L))
     expect_identical(dk$k, every$k)
     expect_identical(dk$status, every$status)
     expect_identical(dk$n_weeks_excluded, 0L)
     # in a panel with active shrinkage only that location changes, to the value it
     # has alone; the others (one with a reconstructed window) keep their rows,
     # shrunk toward a trend fitted on observed-weeks estimates only
     ss <- lapply(11:15, function(sd) .mk_series(n_weeks = 150L, k = 1.5, seed = sd)$daily)
     o6 <- rbind(o, do.call(rbind, ss))
     t6 <- rbind(tier, matrix(1L, 5L, length(tier))); t6[2, 1:140] <- 2L
     plain <- MOSAIC::est_nb_dispersion(o6, date_start = d0, obs_tier = t6)
     expect_gte(attr(plain, "shrinkage")$n_fit, 5L)
     panel <- MOSAIC:::.nb_disp_deaths(o6, date_start = d0, obs_tier = t6)
     expect_identical(panel[-1, ], plain[-1, ])
     expect_identical(panel$k[1], every$k)
     expect_identical(panel$n_weeks_excluded, c(0L, 20L, 0L, 0L, 0L, 0L))
     # the run's resolver and a standalone likelihood both use it
     cfg <- list(location_name = "AAA", date_start = d0, date_stop = d0 + length(s$daily) - 1L,
                 reported_cases = o, reported_deaths = o, reported_tier = matrix(tier, 1L))
     ctl <- list(likelihood = list(nb_dispersion_shrink = TRUE))
     r <- MOSAIC:::.mosaic_resolve_nb_dispersion(cfg, ctl)
     expect_identical(r$deaths$k, every$k)
     expect_identical(r$cases$k, .trend_k(every$mean_weekly))      # cases: the panel trend
     est <- o * 1.2 + 0.1
     ll <- function(k_d) MOSAIC::calc_model_likelihood(o, est, o, est, config = cfg, nb_k_cases = 1,
                                                     nb_k_deaths = k_d, week_offset = 0L)
     expect_identical(ll(NULL), ll(every$k))
})

test_that("calc_model_likelihood() without nb_k estimates k as run_MOSAIC() resolves it (TA-03)", {
     # Observed weeks only under config$reported_tier, and the cases panel trend
     # for a location without an estimate of its own. Location A is an outbreak
     # known only from reconstructed weeks (no estimate: the panel trend); B has
     # a reconstructed window that would inflate k if it entered the fit.
     n_weeks <- 150L
     mondays <- as.Date("2023-01-02") + 7L * (seq_len(n_weeks) - 1L)
     YA <- rep(0, n_weeks); YA[c(30, 90)] <- 2; YA[40:68] <- 40
     sB <- .mk_series(n_weeks = n_weeks, k = 0.6, seed = 3)
     YB <- sB$Y; win <- 60:84; YB[win] <- round(sum(YB[win]) / length(win))
     obs <- rbind(MOSAIC::downscale_weekly_values(mondays, YA)$value,
                  MOSAIC::downscale_weekly_values(mondays, YB)$value)
     n_d <- ncol(obs)
     tier <- matrix(1L, 2L, n_d)
     tier[1, (39 * 7 + 1):(68 * 7)] <- 2L
     tier[2, rep(win, each = 7) * 7 - 6 + rep(0:6, length(win))] <- 2L
     cfg <- list(date_start = mondays[1], date_stop = mondays[1] + n_d - 1L, reported_tier = tier)
     est <- obs * 1.3 + 0.5
     nad <- matrix(NA_real_, 2L, n_d)
     ll <- function(cfg, k_c, o = obs, e = est, nd = nad)
          MOSAIC::calc_model_likelihood(o, e, nd, nd, config = cfg, nb_k_cases = k_c, nb_k_deaths = Inf,
                                        week_offset = 0L)
     k_exp <- MOSAIC::est_nb_dispersion(obs, date_start = mondays[1], obs_tier = tier,
                                        panel_trend = MOSAIC:::.NB_DISP_PANEL_TREND)
     expect_identical(k_exp$panel_trend, c(TRUE, FALSE))
     expect_equal(k_exp$k[1], .trend_k(mean(YA)), tolerance = 1e-12)
     k_all <- MOSAIC::est_nb_dispersion(obs, date_start = mondays[1], panel_trend = MOSAIC:::.NB_DISP_PANEL_TREND)$k
     k_bare <- MOSAIC::est_nb_dispersion(obs, date_start = mondays[1], obs_tier = tier)$k
     expect_gt(k_all[2], k_exp$k[2])            # the flat window reads as low noise
     expect_false(isTRUE(all.equal(k_bare[1], k_exp$k[1])))
     auto <- ll(cfg, NULL)
     expect_identical(auto, ll(cfg, k_exp$k))
     # (each wiring matters: without the tiers or without the trend the score moves)
     expect_false(isTRUE(all.equal(auto, ll(cfg, k_all))))
     expect_false(isTRUE(all.equal(auto, ll(cfg, k_bare))))
     # observations sliced without slicing the tiers: warn once and use every week
     rm(list = intersect("lik_reported_tier_dims", ls(MOSAIC:::.mosaic_once)), envir = MOSAIC:::.mosaic_once)
     keep <- 8:n_d
     cs <- cfg; cs$date_start <- mondays[1] + 7L
     expect_warning(sl <- ll(cs, NULL, obs[, keep], est[, keep], nad[, keep]), "does not match the observation")
     k_sl <- MOSAIC::est_nb_dispersion(obs[, keep], date_start = cs$date_start,
                                       panel_trend = MOSAIC:::.NB_DISP_PANEL_TREND)$k
     expect_identical(sl, ll(cs, k_sl, obs[, keep], est[, keep], nad[, keep]))
})

test_that("the cases scoring knob defaults to daily and is validated", {
     # The v0.101.0 likelihood gate blocked the weekly rule as the default (B5:
     # worse than daily on both pre-registered cases criteria).
     expect_identical(mosaic_control_defaults()$likelihood$cases_scoring, "daily")
     # calc_model_likelihood()'s own default agrees with the control default
     expect_identical(eval(formals(MOSAIC::calc_model_likelihood)$cases_scoring)[1], "daily")
     ctl <- mosaic_control_defaults(); ctl$likelihood$cases_scoring <- "hourly"
     expect_error(MOSAIC:::.mosaic_validate_and_merge_control(ctl), "cases_scoring")
     ctl$likelihood$cases_scoring <- "weekly"
     expect_identical(MOSAIC:::.mosaic_validate_and_merge_control(ctl)$likelihood$cases_scoring, "weekly")
     # an older control without the key takes the default
     ctl$likelihood$cases_scoring <- NULL
     expect_identical(MOSAIC:::.mosaic_validate_and_merge_control(ctl)$likelihood$cases_scoring, "daily")
})

test_that("the likelihood implementation stamp separates the clamped-fit routing", {
     # Resume refuses to pool shards across this stamp (test-run_MOSAIC_resume.R).
     # The resolved k is not in control.json, so only the stamp keeps shards
     # scored with a clamped fit's 0.1 (the 0.101.0 development builds) apart
     # from shards scored with the panel trend.
     v <- MOSAIC:::.mosaic_likelihood_impl_version()
     expect_match(v, "v0\\.101\\.0")
     expect_false(v %in% c("R/v0.100.0+review_likelihood", "R/v0.101.0+weekly_cases"))
})
