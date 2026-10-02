# Conditional negative-binomial dispersion estimation for surveillance counts.
#
# Replaces the former marginal method-of-moments estimator
# (.nb_size_from_obs_weighted), which computed k = m^2/(v - m) across the whole
# series. By the law of total variance, for y_t ~ NB(mu_t, k) with a time-varying
# mean,
#
#     v = m + (Var(mu) + m^2)/k + Var(mu)
#
# so that form is the special case Var(mu) = 0 -- it assumes a stationary mean.
# For an epidemic curve Var(mu) dominates, the estimate collapses, and the
# k_min floor binds essentially always (measured: 27 of 28 estimable locations
# for cases, 17 of 20 for deaths on config_default). Validated against synthetic
# data with a known k = 4, the marginal form returns 1.10 while the conditional
# estimator below returns 3.88.
#
# The approach here is the Farrington (1996) / Noufaily (2013) convention used by
# the `surveillance` package: model the mean with a trend plus seasonality, then
# take the dispersion from the residuals about that fitted mean. The dispersion
# itself is estimated by maximum likelihood (MASS::glm.nb), whose `theta` is the
# NB2 size parameter on R's dnbinom(mu=, size=) scale and therefore usable as k
# with no conversion.

# Hard numerical bounds, for NUMERICAL stability only -- deliberately not
# presented as a scientific prior. (An earlier draft justified the lower bound by
# Lloyd-Smith et al. 2005's SARS offspring-distribution k = 0.16; that is a
# dispersion of individual transmission heterogeneity and bounds nothing about
# reporting noise. Several real locations legitimately estimate below it.) The
# upper bound is hygiene: an NB is indistinguishable from a Poisson once k
# greatly exceeds max(mu). Binding at either bound is reported as a diagnostic.
.NB_DISP_K_LO <- 0.1
.NB_DISP_K_HI <- 1e5

# Minimum evidence required before a dispersion estimate is returned at all.
# MIN_TOTAL follows edgeR's filterByExpr min.total.count = 15.
.NB_DISP_MIN_WEEKS   <- 20L
.NB_DISP_MIN_TOTAL   <- 15
.NB_DISP_MIN_NONZERO <- 5L

# A fit has collapsed when its standard error is degenerate relative to the
# dispersion it would report: se / max(theta, lower bound) below this value. On
# config_default v6.0, glm.nb runs theta to the zero boundary for CMR, UGA and
# ZAF (theta 3.6e-30, 5.6e-13 and 6.3e-30, SE of the same order), reported as k
# = 0.1 with an SE of 2e-31 to 6e-14; the estimate flips between 0.1, 0.22, 3.0
# and Inf as the spline flexibility changes. The fitted mean has fallen to ~0 in
# runs of zero weeks, so the count after them is explained by unbounded
# overdispersion: the smoother failed, not the reporting process. A genuine
# estimate cannot trip the rule. The Fisher information about log k per weekly
# count is at most k^2 * trigamma(k), below 1 + pi^2 / 6 for k <= 1, so se / k
# is at least 1 / sqrt(2.64 n): 0.04 at 200 weeks, 0.006 at 10,000 (identified
# fits on config_default: 0.11 to 0.61). Below the lower bound the rule therefore
# needs theta under about 2e-4 at 185 weeks -- the zero boundary.
.NB_DISP_SE_DEGENERATE <- 1e-4

# Cross-country trend of the weekly cases dispersion on the series level,
# log k = intercept + slope * log(mean weekly cases), taken by a location whose
# own fit gives no usable estimate (see est_nb_dispersion(), argument
# panel_trend). Fitted on 2026-10-01 by .nb_disp_panel_trend_fit() on
# config_default v6.1, reported_cases on observed (tier-1) weeks of
# reported_tier, scored from day 46 (burn_in_days = 45, the v0.100.1 production
# setting): 22 locations with an estimate of their own, residual SD 1.19 on log
# k, slope 0.22 +/- 0.14 -- a flat, noisy panel, so the trend is a prior centre
# of about 1 rather than a precise prediction. On config_default v6.1 it gives
# BFA 0.89, CIV 0.98, ZAF 1.10 (too few observed weeks) and CMR 1.65 (its fit
# collapses), the four locations without an estimate of their own there, and
# UGA 0.96 (its fit is clamped at the lower bound, which is censoring rather
# than a measurement; see est_nb_dispersion()). (The v6.0 fit, on every week:
# intercept -0.195, slope 0.143, SD 1.56, n 23; CMR, UGA and ZAF at 1.41, 1.00
# and 1.20.)
#
# The constants assume burn_in_days = 45; run_MOSAIC() applies them whatever the
# run's burn-in. Refitted from day 31 (the control default burn_in_days = 30) the
# trend is intercept -0.37, slope 0.22, residual SD 1.18 on 22 locations, which
# gives BFA/CIV/CMR/ZAF/UGA 0.88/0.97/1.63/1.09/0.95: differences of at most
# 0.014 on log k.
#
# Rebuild recipe. The values depend on config_default, and three tests in
# test-est_nb_dispersion_panel.R read it:
#   1. "the shipped panel trend is the one config_default implies (drift
#      guard)" fails by design after a rebuild of the surveillance until the
#      intercept/slope/sigma/n of
#      MOSAIC:::.nb_disp_panel_trend_fit(MOSAIC::config_default, 45L) are pasted
#      here (update `source` too);
#   2. "locations without an estimate of their own take the panel trend" and
#   3. "the panel trend gives the same k at every scale" derive their expected
#      locations and k from that fit's table (observed weeks only once the
#      config carries reported_tier) and need no edit, but must pass after the
#      constants are pasted.
.NB_DISP_PANEL_TREND <- list(intercept = -0.355403044494224,
                             slope     = 0.220326123734482,
                             sigma     = 1.19345523383214,
                             n         = 22L,
                             source    = "config_default v6.1, reported_cases, observed weeks, burn_in_days 45")

# Weekly blocks must never be anchored on the first observation: whenever a
# series starts off the reporting-week boundary, anchoring there splits every
# reporting week across two blocks, inflating the apparent noise and biasing k
# downward. All 40 current MOSAIC locations report Mon-Sun, but the offset is
# DETECTED per location rather than assumed, so a differently-aligned source
# cannot be silently mis-aggregated.
.NB_DISP_ANCHOR <- as.Date("1970-01-05")   # a Monday


#' Detect whether a daily series is a downscaled weekly series
#'
#' A weekly total divided by 7 and rounded leaves every Monday-Sunday block with
#' at most two distinct values, and those two adjacent integers. All 40 MOSAIC
#' surveillance locations match this signature in 100% of complete blocks.
#'
#' @param y Numeric vector of observations on a daily grid.
#' @param dates Date vector the same length as \code{y}.
#' @return A list with \code{share} (proportion of complete weekly blocks
#'   matching the signature, \code{NA_real_} when too few blocks exist to judge)
#'   and \code{offset} (the detected block boundary, 0-6 days from Monday).
#' @keywords internal
.nb_disp_cadence <- function(y, dates) {
     ok <- is.finite(y)
     if (sum(ok) < 21L) return(list(share = NA_real_, offset = 0L))
     yy <- y[ok]; dd <- dates[ok]
     best <- list(share = NA_real_, offset = 0L)
     for (off in 0:6) {
          blk <- .nb_disp_block(dd, off)
          s <- split(yy, blk)
          s <- s[vapply(s, length, 0L) == 7L]
          if (length(s) < 3L) next
          sh <- mean(vapply(s, function(v) {
               u <- sort(unique(v))
               length(u) <= 1L || (length(u) == 2L && isTRUE(all.equal(diff(u), 1)))
          }, TRUE))
          if (is.na(best$share) || sh > best$share) best <- list(share = sh, offset = off)
     }
     best
}

#' Weekly block index
#'
#' The block formula behind every weekly aggregation in MOSAIC (see
#' \code{.mosaic_week_blocks}): week number relative to a fixed Monday,
#' shifted by \code{offset}.
#' @param dates Date vector.
#' @param offset Integer 0-6 shifting the block boundary off Monday.
#' @return Integer week index relative to the anchor epoch.
#' @keywords internal
.nb_disp_block <- function(dates, offset = 0L) {
     as.integer(floor((as.numeric(dates - .NB_DISP_ANCHOR) - offset) / 7))
}

#' Reporting weeks of a daily grid
#'
#' The one definition of the reporting weeks of a daily grid, used by the
#' weekly cases likelihood (\code{calc_model_likelihood()}) and the
#' observation-level posterior predictive (\code{calc_model_ensemble()}), on
#' the block formula \code{.nb_disp_block()} that the dispersion estimate
#' (\code{est_nb_dispersion()}) and the integrated deaths likelihood aggregate
#' with, so all of them sum the same days. MOSAIC surveillance weeks run Monday
#' to Sunday: processed weekly rows are dated by their Monday,
#' \code{downscale_weekly_values()} spreads each total over that Monday to
#' Sunday, so the daily totals of every complete week sum to the processed
#' weekly total the config was built from: exactly where that total is a whole
#' count, and within half a case where it is a fractional imputed total (79
#' weeks in config_default v6.1). Blocks are counted from a fixed Monday
#' (1970-01-05), never from the first day of the grid: config_default starts on
#' a Sunday, whose block is a one-day partial week.
#'
#' A week cut by the start or end of the grid is a partial week, and the two
#' consumers treat it differently, so each states its choice through
#' \code{partial}. The weekly cases likelihood scores weekly totals, which a
#' partial week is not, so it drops them (\code{"drop"}: their days belong to
#' no block). The observation-level predictive draws noise for every day,
#' including days that are never scored, so it keeps them (\code{"keep"}: a
#' partial week is a block of the days it has).
#'
#' @param dates Vector of consecutive daily \code{Date}s.
#' @param offset Integer 0-6, the day after Monday on which the reporting week
#'   starts: \code{est_nb_dispersion()$week_offset} (0, Monday, for every current
#'   MOSAIC location).
#' @param partial \code{"keep"} (default) or \code{"drop"}: whether a week cut
#'   by the start or end of \code{dates} is a block of the days it has, or no
#'   block at all.
#' @return A list with \code{index} (integer, the block of each day, numbered
#'   from 1 at the first block; \code{NA} for the days of a dropped partial
#'   week), and, one entry per block, \code{week_start} (\code{Date}, the first
#'   day of the block's week), \code{complete} (logical: all seven of its days
#'   lie in \code{dates}; \code{FALSE} only for a kept partial week),
#'   \code{start} and \code{end} (positions in \code{dates} of the block's first
#'   and last day).
#' @keywords internal
.mosaic_week_blocks <- function(dates, offset = 0L, partial = c("keep", "drop")) {
     partial <- match.arg(partial)
     dates <- as.Date(dates)
     if (!length(dates)) return(list(index = integer(0), week_start = as.Date(character(0)),
                                     complete = logical(0), start = integer(0), end = integer(0)))
     if (length(dates) > 1L && any(as.numeric(diff(dates)) != 1))
          stop("dates must be consecutive days.", call. = FALSE)
     if (length(offset) != 1L || !is.finite(offset) || offset < 0 || offset > 6 || offset != round(offset))
          stop("offset must be a whole number of days from 0 to 6.", call. = FALSE)
     blk <- .nb_disp_block(dates, offset)
     index <- blk - blk[1L] + 1L
     n_blk <- index[length(index)]
     n_days <- tabulate(index, nbins = n_blk)
     end <- cumsum(n_days)
     out <- list(index = index,
                 week_start = .NB_DISP_ANCHOR + as.integer(offset) + 7L * (blk[1L] + seq_len(n_blk) - 1L),
                 complete = n_days == 7L,
                 start = end - n_days + 1L,
                 end = end)
     if (partial == "drop") {
          # On consecutive days only the first and last blocks can be partial.
          kept <- which(out$complete)
          out <- list(index = match(index, kept), week_start = out$week_start[kept],
                      complete = out$complete[kept], start = out$start[kept], end = out$end[kept])
     }
     out
}

#' Aggregate a daily observation row to complete Monday-Sunday weeks
#'
#' @param y Numeric vector of daily observations (may contain \code{NA}).
#' @param dates Date vector the same length as \code{y}.
#' @param offset Integer 0-6 block-boundary offset, from \code{.nb_disp_cadence}.
#' @param w Optional per-observation confidence weights the same length as
#'   \code{y}. Verified constant within every reporting week, so the weekly
#'   weight is the within-week mean.
#' @return A data.frame with \code{week}, \code{y} (weekly total) and \code{w}
#'   (weekly weight), or \code{NULL} when no complete week exists.
#' @keywords internal
.nb_disp_weekly <- function(y, dates, w = NULL, offset = 0L) {
     ok <- is.finite(y)
     if (!is.null(w)) ok <- ok & is.finite(w)
     if (!any(ok)) return(NULL)
     y <- y[ok]; dates <- dates[ok]
     w <- if (is.null(w)) rep(1, length(y)) else w[ok]
     blk <- .nb_disp_block(dates, offset)
     n   <- as.numeric(table(blk))
     tot <- as.numeric(tapply(y, blk, sum))
     wk  <- as.numeric(tapply(w, blk, mean))
     idx <- as.integer(names(tapply(y, blk, sum)))
     keep <- n == 7L
     if (!any(keep)) return(NULL)
     data.frame(week = idx[keep], y = tot[keep], w = wk[keep])
}

#' Whether weekly totals carry enough evidence to estimate a dispersion
#'
#' @param y Numeric weekly totals.
#' @param w Optional weekly weights; weeks with a non-positive or missing weight
#'   do not count.
#' @return \code{TRUE} when the weeks meet the minimum number of weeks, total
#'   count and number of non-zero weeks.
#' @keywords internal
.nb_disp_sufficient <- function(y, w = NULL) {
     ok <- is.finite(y)
     if (!is.null(w)) ok <- ok & is.finite(w) & w > 0
     y <- y[ok]
     length(y) >= .NB_DISP_MIN_WEEKS && sum(y) >= .NB_DISP_MIN_TOTAL &&
          sum(y > 0) >= .NB_DISP_MIN_NONZERO
}

#' Fit the conditional dispersion for one location
#'
#' Descends a ladder of progressively simpler mean models. A high-degree spline
#' on a series with long runs of zeros drives fitted rates to zero and breaks the
#' IRLS ("NA/NaN/Inf in 'x'", "no valid set of coefficients"), so each rung
#' supplies Poisson coefficients as starting values -- the documented remedy --
#' and seeds \code{init.theta} from a moment estimate.
#'
#' @param week Integer week index.
#' @param y Numeric weekly totals.
#' @param wt Numeric weekly observation weights.
#' @param trend_df_per_year Spline degrees of freedom per year of data.
#' @param n_harmonics Number of seasonal harmonic pairs.
#' @return A one-row data.frame; see \code{\link{est_nb_dispersion}}.
#' @keywords internal
.nb_disp_fit_one <- function(week, y, wt, trend_df_per_year = 2, n_harmonics = 2L) {

     out <- data.frame(n_weeks = 0L, mean_weekly = NA_real_, k = NA_real_,
                       se = NA_real_, trend_df = NA_real_, rung = NA_integer_,
                       identified = NA, status = NA_character_,
                       stringsAsFactors = FALSE)

     ok <- is.finite(y) & is.finite(week) & is.finite(wt) & wt > 0
     y <- y[ok]; week <- week[ok]; wt <- wt[ok]
     n <- length(y)
     out$n_weeks <- n
     if (n) out$mean_weekly <- mean(y)

     # Insufficient evidence -> Poisson. This is the correct answer, not a
     # failure: an all-zero observed series scored against a positive prediction
     # should take the Poisson penalty. DESeq2 likewise drops all-zero units from
     # the dispersion fit while retaining them downstream.
     if (!.nb_disp_sufficient(y)) {
          out$k <- Inf
          out$status <- "poisson_insufficient_data"
          return(out)
     }

     t   <- (week - min(week)) / 52.18          # time in years
     doy <- (week %% 52.18) / 52.18             # position within the year
     X <- data.frame(y = y, t = t, .w = wt)
     for (h in seq_len(n_harmonics)) {
          X[[paste0("cos", h)]] <- cos(2 * pi * h * doy)
          X[[paste0("sin", h)]] <- sin(2 * pi * h * doy)
     }
     hterms <- paste(grep("^(cos|sin)", names(X), value = TRUE), collapse = " + ")
     df0 <- max(2L, min(round(trend_df_per_year * diff(range(t))), floor(n / 6)))

     specs <- unique(c(
          sprintf("y ~ splines::ns(t, df = %d) + %s", df0, hterms),
          sprintf("y ~ splines::ns(t, df = %d) + %s", min(df0, max(3L, ceiling(df0 / 2))), hterms),
          sprintf("y ~ splines::ns(t, df = %d) + %s", min(df0, 3L), hterms),
          paste("y ~", hterms),
          "y ~ t + cos1 + sin1",
          "y ~ t"))

     # Seed theta from a moment estimate on the same design.
     mom  <- .nb_disp_mom(X, specs[1L])
     init <- if (is.finite(mom) && mom > 0) min(max(mom, 0.05), 1e4) else 1

     wv  <- X$.w
     ctl <- stats::glm.control(maxit = 200)     # R's default 25 is too low here
     fit <- NULL; rung <- NA_integer_; used_df <- NA_real_

     for (si in seq_along(specs)) {
          f <- stats::as.formula(specs[si])
          # MASS::glm.nb re-evaluates `weights` in the FORMULA's environment, not
          # the caller's. A formula built elsewhere fails with "object of type
          # 'closure' is not subsettable" and the fit silently falls through.
          environment(f) <- environment()
          st <- tryCatch(stats::coef(suppressWarnings(stats::glm(
                    f, family = stats::poisson(), data = X, weights = wv, control = ctl))),
                    error = function(e) NULL)
          r <- tryCatch(suppressWarnings(MASS::glm.nb(
                    f, data = X, weights = wv, init.theta = init,
                    start = st, control = ctl)),
                    error = function(e) NULL)
          # The joint glm.nb IRLS can fail at a flexibility the Poisson mean fit
          # handles perfectly well. Descending the ladder in that case would
          # leave epidemic-wave variance in the residual and bias k DOWNWARD --
          # the very Var(mu) contamination this estimator exists to remove. So
          # when the Poisson mean converged, estimate theta by ML at THAT mean
          # (MASS::theta.ml) instead of giving up flexibility.
          if (is.null(r) && !is.null(st)) {
               gp <- tryCatch(suppressWarnings(stats::glm(
                         f, family = stats::poisson(), data = X,
                         weights = wv, control = ctl)), error = function(e) NULL)
               if (!is.null(gp) && isTRUE(gp$converged)) {
                    th_ml <- tryCatch(suppressWarnings(
                              MASS::theta.ml(y = X$y, mu = stats::fitted(gp),
                                             n = sum(wv), weights = wv, limit = 100)),
                              error = function(e) NULL)
                    if (!is.null(th_ml) && is.finite(th_ml) && th_ml > 0) {
                         r <- gp
                         r$theta <- as.numeric(th_ml)
                         r$SE.theta <- as.numeric(attr(th_ml, "SE"))
                         r$.theta_ml_fallback <- TRUE
                    }
               }
          }
          if (!is.null(r) && is.finite(r$theta) && r$theta > 0) {
               fit  <- r
               rung <- si
               used_df <- if (grepl("ns\\(t, df = ", specs[si], fixed = FALSE))
                    as.numeric(sub(".*df = ([0-9]+).*", "\\1", specs[si])) else 0
               break
          }
     }

     if (is.null(fit)) {
          out$status <- "no_estimate_all_rungs_failed"
          return(out)
     }

     out$rung <- rung
     out$trend_df <- used_df
     theta_ml_fb <- isTRUE(fit$.theta_ml_fallback)
     th <- as.numeric(fit$theta)
     se <- suppressWarnings(as.numeric(fit$SE.theta))

     # theta at the Poisson boundary. NB variance is mu + mu^2/theta, so once
     # theta greatly exceeds max(mu) the NB is numerically Poisson; a non-finite
     # standard error is the other signature of theta running to the boundary.
     mu_max <- suppressWarnings(max(stats::fitted(fit), na.rm = TRUE))
     if (!is.finite(th) || th >= .NB_DISP_K_HI ||
         (is.finite(mu_max) && th > 100 * mu_max)) {
          out$k <- Inf
          out$se <- NA_real_
          out$status <- "poisson_theta_at_boundary"
          return(out)
     }
     if (!is.finite(se)) {
          # A non-finite SE means the theta fit did not converge. Treat it like
          # any other non-convergence -- no estimate, inherit the trend -- NOT
          # as Poisson, which would be the tightest possible kernel.
          out$status <- "no_estimate_se_not_finite"
          return(out)
     }

     # Small-sample ML bias: theta is biased UPWARD, and the mean model consumes
     # p parameters. The (n - p)/n multiplier removes most of it (+25% -> +6% at
     # n = 140, p = 22 in simulation).
     p_mean <- length(stats::coef(fit))
     n_eff  <- length(y)
     if (is.finite(p_mean) && n_eff > p_mean + 1L) th <- th * (n_eff - p_mean) / n_eff

     # Collapsed: theta ran to the zero boundary (a bursty series or a reporting
     # dump defeated the smooth mean), so neither it nor its SE describes the
     # reporting noise. No estimate, like the non-finite SE above.
     if (se / max(th, .NB_DISP_K_LO) < .NB_DISP_SE_DEGENERATE) {
          out$se <- se
          out$status <- "no_estimate_se_degenerate"
          return(out)
     }

     out$se <- se
     out$identified <- se < th
     out$k <- min(max(th, .NB_DISP_K_LO), .NB_DISP_K_HI)
     out$status <- if (th < .NB_DISP_K_LO) "clamped_lower_bound"
                   else if (!out$identified) "ok_not_identified"
                   else if (theta_ml_fb) "ok_theta_ml_at_full_df"
                   else if (rung > 1L) "ok_reduced_mean_model"
                   else "ok"
     out
}

#' Moment estimate of NB dispersion about a fitted Poisson mean
#'
#' Used only to seed \code{init.theta}. Solves
#' \eqn{E[(y - \mu)^2 - \mu] = \mu^2 / k} by weighted pooling.
#'
#' @param X Model frame containing \code{y} and \code{.w}.
#' @param spec Character model formula.
#' @return Numeric dispersion estimate, or \code{NA_real_}.
#' @keywords internal
.nb_disp_mom <- function(X, spec) {
     f <- stats::as.formula(spec)
     environment(f) <- environment()
     wv <- X$.w
     g <- tryCatch(suppressWarnings(stats::glm(f, family = stats::poisson(), data = X,
                    weights = wv, control = stats::glm.control(maxit = 200))),
                   error = function(e) NULL)
     if (is.null(g)) return(NA_real_)
     mu <- stats::fitted(g)
     keep <- is.finite(mu) & mu > 1e-3
     if (sum(keep) < .NB_DISP_MIN_WEEKS) return(NA_real_)
     yy <- X$y[keep]; mm <- mu[keep]; ww <- wv[keep]
     den <- sum(ww * ((yy - mm)^2 - mm))
     if (!is.finite(den) || den <= 0) return(NA_real_)
     sum(ww * mm^2) / den
}

#' Shrink per-location dispersion toward a mean-dispersion trend
#'
#' Empirical-Bayes shrinkage in the style of DESeq2 (Love, Huber & Anders 2014):
#' \eqn{\log k_j \sim N(\log k_{trend}(\mu_j), \sigma_p^2)}. Each location's own
#' estimate is weighted by its PRECISION,
#' \eqn{w_j = \sigma_p^2 / (\sigma_p^2 + s_j^2)} with \eqn{s_j = se_j / k_j}
#' (delta method), and the prior variance is recovered by the DESeq2 variance
#' decomposition \eqn{\sigma_p^2 = Var(resid) - \overline{s_j^2}}. A flat blend
#' would be the posterior mean only if sampling variance equalled the prior
#' variance. Locations with no estimate inherit the trend; locations with no
#' positive mean fall back to Poisson.
#'
#' @param mean_weekly Numeric vector of per-location weekly means.
#' @param k Numeric vector of per-location dispersion estimates (may be
#'   \code{NA} or \code{Inf}).
#' @param se Numeric vector of standard errors on \code{k}, used to weight each
#'   location's own estimate by its precision.
#' @param identified Logical vector; unidentified estimates carry no usable
#'   precision and lean on the trend.
#' @param clamped Logical vector; clamped estimates are censored, so they are
#'   shrunk toward the trend but excluded from fitting it.
#' @return List with \code{k} (shrunk vector), \code{trend}, \code{sigma} and
#'   counts.
#' @keywords internal
.nb_disp_shrink <- function(mean_weekly, k, se = NULL, identified = NULL, clamped = NULL) {
     n <- length(k)
     if (is.null(identified)) identified <- rep(TRUE, n)
     identified[is.na(identified)] <- FALSE
     if (is.null(clamped)) clamped <- rep(FALSE, n)
     clamped[is.na(clamped)] <- FALSE
     if (is.null(se)) se <- rep(NA_real_, n)

     fin <- is.finite(k) & k > 0 & is.finite(mean_weekly) & mean_weekly > 0

     # A location with no estimate of its own must ALWAYS resolve to something
     # usable, independent of how many other locations there are: MOSAIC's
     # dominant usage is single-country, where no cross-location pooling is
     # possible at all. NA would otherwise reach the likelihood and make the
     # total -Inf for every simulation.
     .fill_na <- function(v) {
          miss <- !is.finite(v) & !is.infinite(v)
          if (!any(miss)) return(v)
          pool <- v[is.finite(v) & v > 0]
          v[miss] <- if (length(pool)) stats::median(pool) else Inf
          v
     }

     # Clamped values are CENSORED, not observed: including them in the trend
     # drags the low end toward the bound. Fit the trend on uncensored points
     # only, but still shrink the censored ones toward it.
     fit_ok <- fin & !clamped
     if (sum(fit_ok) < 5L) {
          # too few locations to fit a mean-dispersion trend: borrow the channel
          # median for anything missing, and leave estimated values alone.
          filled <- .fill_na(k)
          return(list(k = filled, trend = NULL, sigma = NA_real_,
                      n_fit = sum(fin), n_inherit = sum(!is.finite(k) & !is.infinite(k)),
                      n_poisson = sum(is.infinite(filled)), median_weight = 1))
     }

     lk <- log(k[fit_ok]); lm_ <- log(mean_weekly[fit_ok])
     tr <- stats::lm(lk ~ lm_)
     resid_var <- stats::var(stats::residuals(tr))

     # Sampling variance of log k_j by the delta method: Var(log k) ~ (se/k)^2.
     s2 <- rep(NA_real_, n)
     ok_se <- is.finite(se) & is.finite(k) & k > 0
     s2[ok_se] <- (se[ok_se] / k[ok_se])^2

     # DESeq2-style variance decomposition: the PRIOR variance is the residual
     # variance minus the average sampling variance. A flat 50/50 blend is the
     # posterior mean only if sampling variance equals the prior variance, which
     # it does not -- here sampling noise is a few percent of the residual, so
     # almost no shrinkage is warranted and a 50/50 blend would move
     # well-identified locations substantially.
     mean_s2 <- mean(s2[fit_ok & ok_se], na.rm = TRUE)
     if (!is.finite(mean_s2)) mean_s2 <- 0
     sigma_p2 <- max(resid_var - mean_s2, 1e-6)

     pred <- rep(NA_real_, n)
     has_mean <- is.finite(mean_weekly) & mean_weekly > 0
     if (any(has_mean))
          pred[has_mean] <- stats::predict(tr, newdata = data.frame(lm_ = log(mean_weekly[has_mean])))

     out <- k
     shr <- fin & is.finite(pred)
     # precision weight on the location's own estimate
     wj <- rep(0.5, n)
     wj[shr] <- sigma_p2 / (sigma_p2 + ifelse(is.finite(s2[shr]), s2[shr], sigma_p2))
     # an unidentified estimate carries no usable precision -> lean on the trend
     wj[shr & !identified] <- pmin(wj[shr & !identified], 0.5)
     out[shr] <- exp(wj[shr] * log(k[shr]) + (1 - wj[shr]) * pred[shr])

     inherit <- !fin & !is.infinite(k) & is.finite(pred)
     out[inherit] <- exp(pred[inherit])
     dead <- !fin & !is.infinite(k) & !is.finite(pred)
     out[dead] <- Inf

     list(k = out, trend = tr, sigma = sqrt(sigma_p2), n_fit = sum(fit_ok),
          n_inherit = sum(inherit), n_poisson = sum(dead),
          median_weight = stats::median(wj[shr], na.rm = TRUE))
}

#' Validate a panel mean-dispersion trend
#' @param trend List with numeric \code{intercept} and \code{slope}.
#' @return \code{trend}, unchanged.
#' @keywords internal
.nb_disp_check_trend <- function(trend) {
     ok <- is.list(trend) &&
          is.numeric(trend$intercept) && length(trend$intercept) == 1L && is.finite(trend$intercept) &&
          is.numeric(trend$slope) && length(trend$slope) == 1L && is.finite(trend$slope)
     if (!ok) stop("panel_trend must be NULL or a list with finite numeric `intercept` and `slope` ",
                   "(log k = intercept + slope * log(mean weekly count)).", call. = FALSE)
     trend
}

#' Dispersion predicted by a panel mean-dispersion trend
#' @param trend List with \code{intercept} and \code{slope}.
#' @param mean_weekly Numeric vector of positive weekly means.
#' @return \code{exp(intercept + slope * log(mean_weekly))}, held inside the
#'   numerical bounds.
#' @keywords internal
.nb_disp_panel_predict <- function(trend, mean_weekly) {
     k <- exp(trend$intercept + trend$slope * log(mean_weekly))
     pmin(pmax(k, .NB_DISP_K_LO), .NB_DISP_K_HI)
}

#' Fit the panel mean-dispersion trend from a configuration
#'
#' Estimates each location's cases dispersion on the scored window (from day
#' \code{burn_in_days + 1}, observed weeks only when the config carries
#' \code{reported_tier}), then regresses log k on log mean weekly cases over the
#' locations with an estimate of their own: finite, not at a bound, not
#' collapsed. Used to derive \code{.NB_DISP_PANEL_TREND}.
#'
#' @param config A config with \code{reported_cases}, \code{date_start},
#'   \code{location_name} and optionally \code{reported_cases_weight} and
#'   \code{reported_tier}.
#' @param burn_in_days Integer number of leading days left unscored.
#' @return List with \code{intercept}, \code{slope}, \code{sigma} (residual SD
#'   of log k), \code{n} and the per-location \code{table}.
#' @keywords internal
.nb_disp_panel_trend_fit <- function(config, burn_in_days = 45L) {
     as_mat <- function(x) if (is.null(x) || is.matrix(x)) x else matrix(x, nrow = 1L)
     obs <- as_mat(config$reported_cases)
     keep <- (as.integer(burn_in_days) + 1L):ncol(obs)
     sl <- function(x) { x <- as_mat(x); if (is.null(x)) NULL else x[, keep, drop = FALSE] }
     tab <- est_nb_dispersion(sl(obs), sl(config$reported_cases_weight),
                              date_start = as.Date(config$date_start) + keep[1] - 1L,
                              location_name = config$location_name, shrink = FALSE,
                              obs_tier = sl(config$reported_tier), panel_trend = NULL)
     ok <- is.finite(tab$k_raw) & !(tab$status %in% "clamped_lower_bound") &
          is.finite(tab$mean_weekly) & tab$mean_weekly > 0
     if (sum(ok) < 3L) stop("too few locations with a dispersion estimate to fit a panel trend.")
     lk <- log(tab$k_raw[ok]); lm_ <- log(tab$mean_weekly[ok])
     fit <- stats::lm(lk ~ lm_)
     list(intercept = unname(stats::coef(fit)[1]), slope = unname(stats::coef(fit)[2]),
          sigma = stats::sigma(fit), n = sum(ok), table = tab)
}


#' Estimate negative-binomial dispersion from surveillance observations
#'
#' Estimates the conditional NB dispersion \code{k} for each location, at the
#' data's native weekly reporting resolution and honouring per-observation
#' confidence weights. The mean is modelled with a spline trend plus seasonal
#' harmonics and the dispersion estimated by maximum likelihood
#' (\code{MASS::glm.nb}), following the Farrington/Noufaily convention.
#'
#' The returned \code{k} is on R's \code{dnbinom(mu=, size=)} scale and is used
#' directly by \code{\link{calc_model_likelihood}}, which scores the cases on
#' the same weekly totals. \code{k = Inf} denotes the Poisson limit and is a
#' valid, intended result.
#'
#' A location with fewer than 20 weeks, 15 cases or 5 non-zero weeks over all
#' its scored weeks takes the Poisson limit. Otherwise its own fit can still fail
#' to give an estimate (status \code{no_estimate_*}): every rung of the mean
#' model fails, the standard error of \code{theta} is not finite, or the fit
#' collapsed (\code{theta} ran to the zero boundary, so its SE is below 1e-4 of
#' the dispersion it would report; on config_default this is how bursty series
#' with reporting dumps defeat the smooth mean), or, with \code{obs_tier}, the
#' observed weeks alone fall short of the minimum. Such
#' a location takes \code{panel_trend} when it is supplied, evaluated at its
#' mean weekly count; without it, it borrows the run's own mean-dispersion trend
#' (five or more estimated locations), the median of the estimated locations,
#' or, alone, the Poisson limit.
#'
#' With \code{panel_trend}, a fit clamped at the lower bound of 0.1 (status
#' \code{clamped_lower_bound}) takes the trend too, at every scale. The clamp is
#' censoring, not a measurement. Where a series' few non-zero observed weeks are
#' mostly the edges of short outbreaks whose middle weeks are reconstructed and
#' left out of the fit (UGA on config_default v6.1: 10 non-zero of 84 observed
#' weeks), the smooth mean cannot follow the outbreaks, their variance stays in
#' the residual, and the fit returns the bound whatever the true \code{k}: on
#' synthetic series of that shape it did so for a Poisson, a \code{k = 1} and a
#' \code{k = 5} reporting process alike. At \code{k = 0.1} the cases score is
#' several times less sensitive to the level (on UGA's two observed years a
#' twofold level error costs 3.1 nats under the daily cases rule and 0.5 under
#' the weekly one, against 22 and 4.6 at the trend's 0.96), and the weekly
#' observation-level predictive puts at least half its mass on 0 for any weekly
#' mean up to 102. The row keeps
#' \code{status = "clamped_lower_bound"} and \code{k_raw} (the fit) with
#' \code{panel_trend = TRUE} (the \code{k} used). Without \code{panel_trend} a
#' clamped fit keeps the bound, shrunk toward the run's own trend when five or
#' more locations have an estimate.
#'
#' @param obs Numeric matrix of observations, \code{n_locations x n_time_steps},
#'   on a daily grid.
#' @param weights_obs Optional numeric matrix of per-observation confidence
#'   weights with the same dimensions as \code{obs}.
#' @param date_start Start date of the observation grid (\code{Date} or a string
#'   coercible to one).
#' @param location_name Optional character vector of location identifiers used to
#'   label the result.
#' @param shrink Logical; apply empirical-Bayes shrinkage toward the
#'   mean-dispersion trend across locations. Default \code{TRUE}.
#' @param trend_df_per_year Spline degrees of freedom per year for the mean
#'   model. Default \code{2}.
#' @param n_harmonics Number of seasonal harmonic pairs. Default \code{2}.
#' @param verbose Logical; report a summary of the fit. Default \code{FALSE}.
#' @param obs_tier Optional integer matrix of surveillance trust tiers with the
#'   same dimensions as \code{obs} (\code{config$reported_tier}: 1 observed, 2
#'   reconstructed, 3 imputed). When supplied, only weeks whose seven days are
#'   all tier 1 enter the fit: a reconstructed week (a WHO multi-week report
#'   spread evenly) or an imputed one (a Fourier curve through an annual total)
#'   has a synthetic shape that reads as low noise and inflates \code{k}. The
#'   Poisson rule above still uses every tier. Default \code{NULL} (all weeks).
#' @param panel_trend Optional list with numeric \code{intercept} and
#'   \code{slope}: a cross-location trend \code{log k = intercept + slope *
#'   log(mean weekly count)} taken by a location without a usable estimate of
#'   its own (no estimate, or a fit clamped at the lower bound; see Details).
#'   \code{run_MOSAIC()} supplies the cases trend fitted on config_default with
#'   \code{burn_in_days = 45} (\code{MOSAIC:::.NB_DISP_PANEL_TREND}), whatever
#'   the run's burn-in; refitted on the window of the control default (30) it
#'   barely moves (BFA, CIV, CMR, ZAF, UGA 0.88, 0.97, 1.63, 1.09, 0.95 instead
#'   of 0.89, 0.98, 1.65, 1.10, 0.96), far inside the trend's residual SD of
#'   1.19 on log k. Default \code{NULL}.
#'
#' @importFrom splines ns
#'
#' @return A data.frame with one row per location: \code{location},
#'   \code{weekly_share} and \code{week_offset} (detected reporting cadence),
#'   \code{n_weeks} (weeks in the fit; every scored week when the location takes
#'   the Poisson limit for sparse data, where no fit is attempted),
#'   \code{n_weeks_excluded} (scored weeks left out of the fit as reconstructed
#'   or imputed; 0 when no fit is attempted), \code{mean_weekly} (mean
#'   weekly count over all scored weeks), \code{k_raw}, \code{se},
#'   \code{trend_df}, \code{rung}, \code{identified}, \code{status},
#'   \code{panel_trend} (\code{TRUE} where \code{k} comes from
#'   \code{panel_trend}), and \code{k} (the value actually used).
#'   \code{run_MOSAIC()} writes this table, for both channels, to
#'   \code{2_calibration/diagnostics/nb_dispersion.csv}. Where
#'   \code{MASS::glm.nb} fails on a rung whose Poisson mean converges,
#'   \code{theta} is estimated by \code{MASS::theta.ml} at that Poisson mean.
#'   \code{status} records this fallback only as \code{ok_theta_ml_at_full_df},
#'   which ranks below \code{clamped_lower_bound} and
#'   \code{ok_not_identified}: a clamped or unidentified row does not show
#'   whether its \code{theta} came from the fallback (\code{rung} and
#'   \code{trend_df} give the mean model it was estimated at).
#'
#' @examples
#' \dontrun{
#' cfg <- MOSAIC::config_default
#' est_nb_dispersion(cfg$reported_cases, cfg$reported_cases_weight,
#'                   date_start = cfg$date_start,
#'                   location_name = cfg$location_name, verbose = TRUE)
#' }
#' @export
est_nb_dispersion <- function(obs,
                              weights_obs = NULL,
                              date_start,
                              location_name = NULL,
                              shrink = TRUE,
                              trend_df_per_year = 2,
                              n_harmonics = 2L,
                              verbose = FALSE,
                              obs_tier = NULL,
                              panel_trend = NULL) {

     if (is.null(dim(obs))) obs <- matrix(obs, nrow = 1L)
     n_loc <- nrow(obs); n_t <- ncol(obs)
     if (!is.null(weights_obs)) {
          if (is.null(dim(weights_obs))) weights_obs <- matrix(weights_obs, nrow = 1L)
          if (!all(dim(weights_obs) == c(n_loc, n_t)))
               stop("weights_obs must have the same dimensions as obs.")
     }
     if (!is.null(obs_tier)) {
          if (is.null(dim(obs_tier))) obs_tier <- matrix(obs_tier, nrow = 1L)
          if (!all(dim(obs_tier) == c(n_loc, n_t)))
               stop("obs_tier must have the same dimensions as obs.")
     }
     if (!is.null(panel_trend)) panel_trend <- .nb_disp_check_trend(panel_trend)
     if (is.null(location_name)) location_name <- as.character(seq_len(n_loc))
     if (length(location_name) != n_loc)
          stop("location_name must have one entry per row of obs.")

     dates <- seq(as.Date(date_start), by = "day", length.out = n_t)
     n_pos <- function(wk) if (is.null(wk)) 0L else sum(is.finite(wk$y) & is.finite(wk$w) & wk$w > 0)

     rows <- vector("list", n_loc)
     for (i in seq_len(n_loc)) {
          y <- as.numeric(obs[i, ])
          w <- if (is.null(weights_obs)) NULL else as.numeric(weights_obs[i, ])
          # The reporting-week boundary is a property of the reporting calendar,
          # so it is detected on every finite day whatever its tier.
          cad <- .nb_disp_cadence(y, dates)
          share <- cad$share
          wk_all <- .nb_disp_weekly(y, dates, w, offset = cad$offset)
          wk <- wk_all
          if (!is.null(obs_tier) && !is.null(wk_all)) {
               y_obs <- y
               y_obs[!(is.finite(obs_tier[i, ]) & obs_tier[i, ] == 1)] <- NA_real_
               wk <- .nb_disp_weekly(y_obs, dates, w, offset = cad$offset)
          }
          n_all <- n_pos(wk_all); n_obs <- n_pos(wk)
          n_excl <- n_all - n_obs
          r <- if (is.null(wk_all)) {
               data.frame(n_weeks = 0L, mean_weekly = NA_real_, k = Inf, se = NA_real_,
                          trend_df = NA_real_, rung = NA_integer_, identified = NA,
                          status = "poisson_no_weekly_data", stringsAsFactors = FALSE)
          } else if (!.nb_disp_sufficient(wk_all$y, wk_all$w)) {
               # Sparse over every tier: the Poisson limit (returned by the fit).
               # No fit is attempted, so no week is left out of one.
               n_excl <- 0L
               .nb_disp_fit_one(wk_all$week, wk_all$y, wk_all$w, trend_df_per_year, n_harmonics)
          } else if (is.null(wk) || !.nb_disp_sufficient(wk$y, wk$w)) {
               # Enough data, too little of it observed (e.g. an outbreak known
               # only from a spread multi-week report): no dispersion of its own,
               # which is not evidence for the Poisson limit.
               data.frame(n_weeks = n_obs, mean_weekly = NA_real_, k = NA_real_, se = NA_real_,
                          trend_df = NA_real_, rung = NA_integer_, identified = NA,
                          status = "no_estimate_observed_insufficient", stringsAsFactors = FALSE)
          } else {
               .nb_disp_fit_one(wk$week, wk$y, wk$w, trend_df_per_year, n_harmonics)
          }
          # The level of the series the likelihood scores: every scored week.
          if (n_all > 0L) {
               pos <- is.finite(wk_all$y) & is.finite(wk_all$w) & wk_all$w > 0
               r$mean_weekly <- mean(wk_all$y[pos])
          }
          rows[[i]] <- data.frame(location = location_name[i], weekly_share = share,
                                  week_offset = cad$offset, r["n_weeks"],
                                  n_weeks_excluded = as.integer(n_excl),
                                  r[setdiff(names(r), "n_weeks")], stringsAsFactors = FALSE)
     }
     res <- do.call(rbind, rows)
     names(res)[names(res) == "k"] <- "k_raw"

     # A location without a usable estimate of its own takes the panel trend at
     # its level, at every scale (a single-location run has no panel of its own
     # to borrow from): no estimate (status no_estimate_*), or a fit clamped at
     # the lower bound. The clamp is censoring, not a measurement: where the few
     # non-zero observed weeks are mostly the edges of short outbreaks whose
     # middles are reconstructed (UGA on config_default v6.1), the fit returns
     # the bound whatever the true k (Poisson, 1 or 5 on synthetic series of that
     # shape), and at the bound the cases score is several times less sensitive
     # to the level. The panel fit already leaves clamped fits out. `status`
     # keeps describing the fit and `panel_trend` the k used. The rest go through
     # shrinkage among themselves, so a location routed here is never shrunk.
     use_panel <- if (is.null(panel_trend)) rep(FALSE, n_loc) else
          (grepl("^no_estimate", res$status) | res$status %in% "clamped_lower_bound") &
          is.finite(res$mean_weekly) & res$mean_weekly > 0
     res$panel_trend <- use_panel
     res$k <- NA_real_
     if (any(use_panel))
          res$k[use_panel] <- .nb_disp_panel_predict(panel_trend, res$mean_weekly[use_panel])
     own <- !use_panel
     if (any(own)) {
          if (shrink) {
               sh <- .nb_disp_shrink(res$mean_weekly[own], res$k_raw[own], se = res$se[own],
                                     identified = res$identified[own],
                                     clamped = res$status[own] %in% "clamped_lower_bound")
               res$k[own] <- sh$k
               attr(res, "shrinkage") <- sh[c("sigma", "n_fit", "n_inherit", "n_poisson", "median_weight")]
          } else {
               # shrink = FALSE must still honour the "never NA" contract.
               k0 <- res$k_raw[own]
               miss <- !is.finite(k0) & !is.infinite(k0)
               if (any(miss)) {
                    pool <- k0[is.finite(k0) & k0 > 0]
                    k0[miss] <- if (length(pool)) stats::median(pool) else Inf
               }
               res$k[own] <- k0
          }
     }

     if (verbose) {
          fin <- is.finite(res$k)
          message(sprintf(
               "est_nb_dispersion: %d locations | %d estimated (median k = %.2f) | %d Poisson | %d at a bound | %d unidentified | %d from the panel trend",
               n_loc, sum(fin), stats::median(res$k[fin]), sum(is.infinite(res$k)),
               sum(res$status == "clamped_lower_bound", na.rm = TRUE),
               sum(res$status == "ok_not_identified", na.rm = TRUE), sum(use_panel)))
     }
     res
}


#' Resolve the per-location NB dispersion for a calibration run
#'
#' Called once by \code{\link{run_MOSAIC}} before the simulation loop. Estimates
#' the dispersion for both channels from the configured observations, or honours
#' an explicit user override.
#'
#' The override (\code{control$likelihood$nb_k_cases} /
#' \code{nb_k_deaths}) \emph{replaces} the estimate rather than bounding it, and
#' says so in the log. This is deliberate: the retired \code{nb_k_min_*} floor
#' silently overrode a data-driven estimate, which is the behaviour this design
#' removes.
#'
#' The estimate uses observed weeks only when the config carries
#' \code{reported_tier} (\code{\link{est_nb_dispersion}}, argument
#' \code{obs_tier}); without it every week enters the fit, as before that field
#' existed. A cases location whose fit gives no estimate, or is clamped at the
#' lower bound, takes the shipped panel trend (\code{.NB_DISP_PANEL_TREND},
#' fitted at \code{burn_in_days = 45}) at every scale. Deaths take no panel
#' trend (a clamped deaths fit keeps the bound): \code{run_MOSAIC()} scores deaths
#' with the reported CFR integrated out, so their NB dispersion is a diagnostic
#' (and the dispersion of a standalone deaths NB core). A deaths location whose
#' observed weeks alone are too few is estimated from every week
#' (\code{.nb_disp_deaths}), the rule the integrated deaths likelihood applies to
#' its dispersion.
#'
#' @param config A simulation config with \code{reported_cases},
#'   \code{reported_deaths} and \code{date_start}.
#' @param control A control list; \code{control$likelihood} may carry
#'   \code{nb_k_cases}, \code{nb_k_deaths} and \code{nb_dispersion_shrink}.
#' @param score_window Optional resolved scored window (\code{idx_cases},
#'   \code{idx_deaths}); the dispersion is estimated on the same window the
#'   likelihood scores.
#' @return A list with \code{cases}, \code{deaths} (each \code{k},
#'   \code{week_offset} -- the reporting-week boundary per location, which the
#'   weekly cases likelihood uses for its blocks -- a \code{summary} string and
#'   \code{tier_used}, whether \code{config$reported_tier} restricted that
#'   channel's fit to observed weeks: \code{FALSE} for a user-supplied
#'   dispersion), the combined \code{table}, and \code{tier_used}, the cases
#'   channel's (the dispersion the run log reports for the cases likelihood).
#' @keywords internal
.mosaic_resolve_nb_dispersion <- function(config, control, score_window = NULL) {

     as_mat <- function(x) if (is.null(x) || is.matrix(x)) x else matrix(x, nrow = 1L)
     n_loc <- if (is.matrix(config$reported_cases)) nrow(config$reported_cases) else 1L
     lik <- control$likelihood
     shrink <- if (is.null(lik$nb_dispersion_shrink)) TRUE else isTRUE(lik$nb_dispersion_shrink)
     tier_full <- .mosaic_config_tier(config)

     one <- function(channel) {
          obs <- as_mat(if (channel == "cases") config$reported_cases  else config$reported_deaths)
          wob <- as_mat(if (channel == "cases") config$reported_cases_weight else config$reported_deaths_weight)
          tier <- tier_full
          ovr <- if (channel == "cases") lik$nb_k_cases else lik$nb_k_deaths

          # Estimate on the SAME window the likelihood scores. The scored window
          # drops an unscored head (burn-in, deaths-era start); k estimated over
          # the full series would not correspond to the data being scored.
          d_start <- as.Date(config$date_start)
          if (!is.null(score_window) && is.matrix(obs)) {
               idx <- if (channel == "cases") score_window$idx_cases else score_window$idx_deaths
               idx <- if (is.null(idx) || !is.finite(idx)) 1L else as.integer(idx)
               if (idx > 1L && idx <= ncol(obs)) {
                    keep <- idx:ncol(obs)
                    obs <- obs[, keep, drop = FALSE]
                    if (!is.null(wob)) wob <- wob[, keep, drop = FALSE]
                    if (!is.null(tier)) tier <- tier[, keep, drop = FALSE]
                    d_start <- d_start + (idx - 1L)
               }
          }

          if (!is.null(ovr)) {
               k <- if (length(ovr) == 1L) rep(as.numeric(ovr), n_loc) else as.numeric(ovr)
               if (length(k) != n_loc)
                    stop(sprintf("control$likelihood$nb_k_%s must be length 1 or %d.", channel, n_loc))
               msg <- sprintf(
                    "NB dispersion for %s is USER-SUPPLIED and replaces the estimate entirely (k = %s).",
                    channel, if (length(unique(k)) == 1L) format(k[1]) else "per-location vector")
               if (exists("log_msg", mode = "function")) log_msg("%s", msg) else message(msg)
               # The weekly cases likelihood still needs each location's
               # reporting-week boundary, detected exactly as the estimator does.
               dates <- seq(d_start, by = "day", length.out = ncol(obs))
               offs <- vapply(seq_len(n_loc), function(i)
                    as.integer(.nb_disp_cadence(as.numeric(obs[i, ]), dates)$offset), integer(1))
               # The override table MUST carry the same columns as the estimated
               # one, or rbind() of the two channels fails when only one is
               # overridden.
               return(list(k = k, week_offset = offs, summary = "user-supplied", tier_used = FALSE,
                           table = data.frame(
                                channel = channel, location = config$location_name,
                                weekly_share = NA_real_, week_offset = offs,
                                n_weeks = NA_integer_, n_weeks_excluded = NA_integer_,
                                mean_weekly = NA_real_,
                                k_raw = k, se = NA_real_, trend_df = NA_real_,
                                rung = NA_integer_, identified = NA,
                                status = "user_override", panel_trend = FALSE, k = k,
                                stringsAsFactors = FALSE)))
          }

          tab <- if (channel == "cases") {
               est_nb_dispersion(obs, wob, date_start = d_start,
                                 location_name = config$location_name, shrink = shrink,
                                 obs_tier = tier, panel_trend = .NB_DISP_PANEL_TREND)
          } else {
               .nb_disp_deaths(obs, wob, date_start = d_start,
                               location_name = config$location_name, shrink = shrink,
                               obs_tier = tier)
          }
          fin <- is.finite(tab$k)
          list(k = tab$k, week_offset = as.integer(tab$week_offset),
               summary = if (any(fin)) sprintf("%.2f", stats::median(tab$k[fin])) else "all Poisson",
               tier_used = !is.null(tier),
               table = data.frame(channel = channel, tab, stringsAsFactors = FALSE))
     }

     cs <- one("cases"); dt <- one("deaths")
     # A non-finite, non-Inf k would make the TOTAL log-likelihood -Inf for every
     # simulation, and the run would complete "successfully" with a degenerate
     # posterior. Fail loudly instead, naming the offending locations.
     .check <- function(k, channel) {
          bad <- !( (is.finite(k) & k > 0) | is.infinite(k) )
          if (any(bad))
               stop(sprintf(
                    "est_nb_dispersion returned an unusable dispersion for %s at: %s. Every k must be finite and positive, or Inf (Poisson).",
                    channel, paste(config$location_name[bad], collapse = ", ")), call. = FALSE)
     }
     .check(cs$k, "cases"); .check(dt$k, "deaths")
     list(cases = cs, deaths = dt, table = rbind(cs$table, dt$table),
          tier_used = cs$tier_used)
}

#' Deaths dispersion, falling back to every week when observed weeks are too few
#'
#' As \code{\link{est_nb_dispersion}} (no panel trend), except that a location
#' whose observed weeks alone cannot carry an estimate (status
#' \code{no_estimate_observed_insufficient} under \code{obs_tier}) takes its
#' estimate from every scored week instead. Too few observed weeks is not
#' evidence of Poisson scatter, and deaths have no panel trend to borrow, so
#' without this a single-location run would score such a location at the
#' Poisson limit. The integrated deaths likelihood applies the same rule to its
#' dispersion (\code{.d7_setup}).
#'
#' Only those locations are re-estimated, on their own and without shrinkage,
#' so their value is the same at every scale, and the other locations' rows --
#' including their shrinkage, whose trend is fitted on observed-weeks estimates
#' only -- are exactly those of \code{est_nb_dispersion()}. A location whose
#' every-week fit gives no estimate either keeps the first table's row.
#'
#' @param obs,weights_obs,date_start,location_name,shrink,obs_tier As for
#'   \code{\link{est_nb_dispersion}}.
#' @return The \code{est_nb_dispersion()} table.
#' @keywords internal
.nb_disp_deaths <- function(obs, weights_obs = NULL, date_start, location_name = NULL,
                            shrink = TRUE, obs_tier = NULL) {
     tab <- est_nb_dispersion(obs, weights_obs, date_start = date_start,
                              location_name = location_name, shrink = shrink, obs_tier = obs_tier)
     short <- which(tab$status %in% "no_estimate_observed_insufficient")
     if (!length(short)) return(tab)
     as_mat <- function(x) if (is.null(x) || !is.null(dim(x))) x else matrix(x, nrow = 1L)
     obs <- as_mat(obs); weights_obs <- as_mat(weights_obs)
     every <- est_nb_dispersion(obs[short, , drop = FALSE],
                                if (is.null(weights_obs)) NULL else weights_obs[short, , drop = FALSE],
                                date_start = date_start, location_name = tab$location[short],
                                shrink = FALSE)
     own <- !grepl("^no_estimate", every$status)
     tab[short[own], names(every)] <- every[own, , drop = FALSE]
     tab
}

#' Surveillance trust tiers carried by a config
#'
#' \code{config$reported_tier} is an integer matrix aligned with
#' \code{reported_cases} (1 observed, 2 reconstructed, 3 imputed; \code{NA}
#' where the week has no observation), built by \code{make_config_default.R}
#' from the surveillance \code{disaggregation_method}. The confidence weights
#' cannot stand in for it: observed AI weeks and documented zeros carry
#' 0.8-0.95 while spread WHO reports carry 0.5-0.9.
#'
#' @param config A config list.
#' @return The tier matrix, or \code{NULL} when the config has none.
#' @keywords internal
.mosaic_config_tier <- function(config) {
     tier <- config$reported_tier
     if (is.null(tier)) return(NULL)
     ref <- config$reported_cases
     if (!is.matrix(ref)) ref <- matrix(ref, nrow = 1L)
     if (!is.matrix(tier)) tier <- matrix(tier, nrow = 1L)
     if (!identical(dim(tier), dim(ref)))
          stop(sprintf("config$reported_tier is %d x %d but reported_cases is %d x %d; the two must be aligned.",
                       nrow(tier), ncol(tier), nrow(ref), ncol(ref)), call. = FALSE)
     storage.mode(tier) <- "integer"
     tier
}
