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
#' @param dates Date vector.
#' @param offset Integer 0-6 shifting the block boundary off Monday.
#' @return Integer week index relative to the anchor epoch.
#' @keywords internal
.nb_disp_block <- function(dates, offset = 0L) {
     as.integer(floor((as.numeric(dates - .NB_DISP_ANCHOR) - offset) / 7))
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
     if (n < .NB_DISP_MIN_WEEKS || sum(y) < .NB_DISP_MIN_TOTAL ||
         sum(y > 0) < .NB_DISP_MIN_NONZERO) {
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


#' Estimate negative-binomial dispersion from surveillance observations
#'
#' Estimates the conditional NB dispersion \code{k} for each location, at the
#' data's native weekly reporting resolution and honouring per-observation
#' confidence weights. The mean is modelled with a spline trend plus seasonal
#' harmonics and the dispersion estimated by maximum likelihood
#' (\code{MASS::glm.nb}), following the Farrington/Noufaily convention.
#'
#' The returned \code{k} is on R's \code{dnbinom(mu=, size=)} scale and is used
#' directly by \code{\link{calc_model_likelihood}}. \code{k = Inf} denotes the
#' Poisson limit and is a valid, intended result.
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
#'
#' @importFrom splines ns
#'
#' @return A data.frame with one row per location: \code{location},
#'   \code{n_weeks}, \code{mean_weekly}, \code{weekly_share} and
#'   \code{week_offset} (detected reporting cadence),
#'   \code{k_raw}, \code{se}, \code{trend_df}, \code{rung}, \code{identified},
#'   \code{status}, and \code{k} (the shrunk value actually used).
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
                              verbose = FALSE) {

     if (is.null(dim(obs))) obs <- matrix(obs, nrow = 1L)
     n_loc <- nrow(obs); n_t <- ncol(obs)
     if (!is.null(weights_obs)) {
          if (is.null(dim(weights_obs))) weights_obs <- matrix(weights_obs, nrow = 1L)
          if (!all(dim(weights_obs) == c(n_loc, n_t)))
               stop("weights_obs must have the same dimensions as obs.")
     }
     if (is.null(location_name)) location_name <- as.character(seq_len(n_loc))
     if (length(location_name) != n_loc)
          stop("location_name must have one entry per row of obs.")

     dates <- seq(as.Date(date_start), by = "day", length.out = n_t)

     rows <- vector("list", n_loc)
     for (i in seq_len(n_loc)) {
          y <- as.numeric(obs[i, ])
          w <- if (is.null(weights_obs)) NULL else as.numeric(weights_obs[i, ])
          cad <- .nb_disp_cadence(y, dates)
          share <- cad$share
          wk <- .nb_disp_weekly(y, dates, w, offset = cad$offset)
          r <- if (is.null(wk)) {
               data.frame(n_weeks = 0L, mean_weekly = NA_real_, k = Inf, se = NA_real_,
                          trend_df = NA_real_, rung = NA_integer_, identified = NA,
                          status = "poisson_no_weekly_data", stringsAsFactors = FALSE)
          } else {
               .nb_disp_fit_one(wk$week, wk$y, wk$w, trend_df_per_year, n_harmonics)
          }
          rows[[i]] <- data.frame(location = location_name[i], weekly_share = share,
                                  week_offset = cad$offset, r, stringsAsFactors = FALSE)
     }
     res <- do.call(rbind, rows)
     names(res)[names(res) == "k"] <- "k_raw"

     if (shrink) {
          sh <- .nb_disp_shrink(res$mean_weekly, res$k_raw, se = res$se,
                                identified = res$identified,
                                clamped = res$status %in% "clamped_lower_bound")
          res$k <- sh$k
          attr(res, "shrinkage") <- sh[c("sigma", "n_fit", "n_inherit", "n_poisson", "median_weight")]
     } else {
          # shrink = FALSE must still honour the "never NA" contract.
          k0 <- res$k_raw
          miss <- !is.finite(k0) & !is.infinite(k0)
          if (any(miss)) {
               pool <- k0[is.finite(k0) & k0 > 0]
               k0[miss] <- if (length(pool)) stats::median(pool) else Inf
          }
          res$k <- k0
     }

     if (verbose) {
          fin <- is.finite(res$k)
          message(sprintf(
               "est_nb_dispersion: %d locations | %d estimated (median k = %.2f) | %d Poisson | %d at a bound | %d unidentified",
               n_loc, sum(fin), stats::median(res$k[fin]), sum(is.infinite(res$k)),
               sum(res$status == "clamped_lower_bound", na.rm = TRUE),
               sum(res$status == "ok_not_identified", na.rm = TRUE)))
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
#' @param config A simulation config with \code{reported_cases},
#'   \code{reported_deaths} and \code{date_start}.
#' @param control A control list; \code{control$likelihood} may carry
#'   \code{nb_k_cases}, \code{nb_k_deaths} and \code{nb_dispersion_shrink}.
#' @param score_window Optional resolved scored window (\code{idx_cases},
#'   \code{idx_deaths}); the dispersion is estimated on the same window the
#'   likelihood scores.
#' @return A list with \code{cases}, \code{deaths} (each \code{k} plus a
#'   \code{summary} string) and the combined \code{table}.
#' @keywords internal
.mosaic_resolve_nb_dispersion <- function(config, control, score_window = NULL) {

     n_loc <- if (is.matrix(config$reported_cases)) nrow(config$reported_cases) else 1L
     lik <- control$likelihood
     shrink <- if (is.null(lik$nb_dispersion_shrink)) TRUE else isTRUE(lik$nb_dispersion_shrink)

     one <- function(channel) {
          obs <- if (channel == "cases") config$reported_cases  else config$reported_deaths
          wob <- if (channel == "cases") config$reported_cases_weight else config$reported_deaths_weight
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
               # The override table MUST carry the same columns as the estimated
               # one, or rbind() of the two channels fails when only one is
               # overridden.
               return(list(k = k, summary = "user-supplied",
                           table = data.frame(
                                channel = channel, location = config$location_name,
                                weekly_share = NA_real_, week_offset = NA_integer_,
                                n_weeks = NA_integer_, mean_weekly = NA_real_,
                                k_raw = k, se = NA_real_, trend_df = NA_real_,
                                rung = NA_integer_, identified = NA,
                                status = "user_override", k = k,
                                stringsAsFactors = FALSE)))
          }

          tab <- est_nb_dispersion(obs, wob, date_start = d_start,
                                   location_name = config$location_name, shrink = shrink)
          fin <- is.finite(tab$k)
          list(k = tab$k,
               summary = if (any(fin)) sprintf("%.2f", stats::median(tab$k[fin])) else "all Poisson",
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
     list(cases = cs, deaths = dt, table = rbind(cs$table, dt$table))
}
