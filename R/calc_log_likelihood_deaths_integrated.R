#' Deaths log-likelihood with the reported CFR integrated out
#'
#' @description
#' Scores observed deaths against a simulated path with the reported case
#' fatality ratio (CFR) integrated out analytically. Given a path, expected
#' reported deaths on each day are the CFR times a known exposure, so the CFR's
#' level can be solved for per path rather than sampled. The CFR is modelled as
#' the time-varying prior \code{mu_jt} shifted on the logit scale by a
#' location-level offset and a smooth deviation built from one value per
#' calendar year:
#' \deqn{\mathrm{logit}\,\mu_{jt} = \mathrm{logit}\,\mu^{0}_{jt} + a_j +
#'   \sum_y B_y(t)\,\delta_{j,y},\qquad a_j \sim N(0, s_j^2),\quad
#'   \delta_{j,y} \sim N(0, \sigma_y^2),}
#' where \eqn{B_y(t)} interpolates linearly between 1 July anchors and is held
#' flat before the first and after the last -- the rule \code{make_mu_jt()} uses
#' for \eqn{\mu^0} -- so the CFR has no step at a year boundary.
#'
#' Deaths are aggregated to reporting weeks and scored with a quasi-Poisson
#' likelihood: the Poisson log-likelihood divided by a per-location dispersion
#' \eqn{\phi_j}, with a small additive background on each week's expected deaths.
#' The quasi-Poisson score for the CFR level is the Poisson score, so given the
#' path the fitted CFR reproduces the observed deaths totals (a negative binomial
#' score would weight low-count weeks far above the peak and bias the level);
#' \eqn{\phi_j} tempers the likelihood for deaths that scatter more than Poisson.
#' For each location the offsets are fitted by Newton's method and the marginal
#' likelihood is the Laplace approximation at the mode.
#'
#' @param obs_deaths Matrix \[locations x days\] (or vector, one location) of observed reported deaths.
#' @param exposure Matrix of the same shape: expected reported deaths per unit reported CFR on each day, \code{(rho / chi_epidemic) * onsets} at the onset day. Must be finite and non-negative.
#' @param base_logit Matrix of the same shape: logit of the prior \code{mu_jt} at the onset day.
#' @param dates Date vector, one per day: the reporting day (defines the weekly blocks).
#' @param sd_shift Numeric, length 1 or one per location: prior SD of the location offset \eqn{a_j} (logit scale).
#' @param sd_year Numeric scalar: prior SD of each year deviation \eqn{\delta_{j,y}} (logit scale).
#' @param onset_dates Date vector, one per day: the onset day of the deaths reported that day, which positions the year deviations (default \code{dates}).
#' @param dispersion Numeric, length 1 or one per location: the quasi-Poisson dispersion \eqn{\phi_j > 0} (1 = Poisson).
#' @param background_rel Numeric scalar >= 0: the additive background on each week's expected deaths, as a fraction of the location's mean scored weekly deaths (floored at 1e-4).
#' @param weights Optional matrix of per-day scoring weights (0 or \code{NA} = not scored); defaults to 1 wherever \code{obs_deaths} is finite. Used as given (not renormalised).
#' @param week_offset Optional integer, length 1 or one per location, 0-6 days from Monday for the reporting-week boundary; detected from \code{obs_deaths} when \code{NULL}.
#' @param years Optional integer vector of the years that carry deviations (default: every year of \code{onset_dates}); years without scored weeks keep their prior.
#'
#' @details
#' A reporting week is scored when every one of its days inside the data is
#' finite and carries positive weight, so a week cut by the start or end of the
#' data is scored on the days it has. Its weight is the mean weight of its days.
#' The background is the same relative floor the cases channel applies to a cell
#' whose prediction is zero, so a week with no onsets but observed deaths costs a
#' bounded, data-scaled amount rather than an arbitrary constant.
#'
#' @return A list with \code{ll} (marginal log-likelihood per location),
#'   \code{theta} (matrix \[locations x (1 + years)\]: the offset \code{a} and the
#'   year deviations at the mode), \code{theta_sd} (their Laplace posterior SDs),
#'   \code{vcov} (list of Laplace covariance matrices), \code{converged}
#'   (logical per location), \code{n_weeks} (scored weeks per location),
#'   \code{years}, \code{dispersion} and \code{background} (per location).
#'
#' @seealso \code{\link{calc_model_likelihood}}, \code{\link{est_CFR_hierarchical}}
#' @examples
#' dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 140)
#' set.seed(1)
#' expo <- rpois(140, 400) * 0.423 / 0.75
#' obs <- rpois(140, 0.02 * expo)
#' fit <- calc_log_likelihood_deaths_integrated(
#'      obs, expo, base_logit = rep(qlogis(0.015), 140), dates = dates,
#'      sd_shift = 0.5, sd_year = 0.7)
#' plogis(qlogis(0.015) + sum(fit$theta[1, ]))   # recovered reported CFR, ~0.02
#' @export
calc_log_likelihood_deaths_integrated <- function(obs_deaths, exposure, base_logit, dates,
                                                  sd_shift, sd_year, onset_dates = dates,
                                                  dispersion = 1, background_rel = 0.02,
                                                  weights = NULL, week_offset = NULL,
                                                  years = NULL) {
     as_mat <- function(x) if (is.matrix(x)) x else matrix(x, nrow = 1L)
     obs_deaths <- as_mat(obs_deaths); exposure <- as_mat(exposure); base_logit <- as_mat(base_logit)
     nT <- ncol(obs_deaths)
     if (!identical(dim(exposure), dim(obs_deaths)) || !identical(dim(base_logit), dim(obs_deaths)))
          stop("obs_deaths, exposure and base_logit must have the same dimensions.")
     if (length(dates) != nT || length(onset_dates) != nT)
          stop("dates and onset_dates must have one entry per day (column).")
     if (any(!is.finite(exposure) | exposure < 0)) stop("exposure must be finite and non-negative.")
     if (any(exposure > 0 & !is.finite(base_logit)))
          stop("base_logit must be finite wherever exposure is positive.")
     if (!is.null(weights)) {
          weights <- as_mat(weights)
          if (!identical(dim(weights), dim(obs_deaths))) stop("weights must match obs_deaths.")
     }
     onset_dates <- as.Date(onset_dates)
     if (is.null(years)) years <- sort(unique(as.integer(format(onset_dates, "%Y"))))
     setup <- .d7_setup(obs_deaths = obs_deaths, weights = weights, dates = as.Date(dates),
                        years = years, sd_shift = sd_shift, sd_year = sd_year,
                        phi = dispersion, background_rel = background_rel,
                        week_offset = week_offset)
     .d7_fit_all(setup, exposure = exposure, base_logit = base_logit,
                 onset_day = as.numeric(onset_dates))
}


# Precompute everything that depends only on the observations: the weekly
# blocks, weekly observed totals and weights, the per-location dispersion and
# background, the year anchors, and the priors.
#
# phi = NULL estimates each location's dispersion from the observed deaths and
# cases (.d7_dispersion); a number (or one per location) uses it as given.
# mass_weights, when supplied, rescales each location's week weights so they sum
# to what those weights alone give on the same weeks: the per-observation
# confidence weights then change which weeks count most, not the location's
# total likelihood mass -- the rule the cases channel applies
# (.weights_obs_effective in calc_model_likelihood.R).
.d7_setup <- function(obs_deaths, weights, dates, years, sd_shift, sd_year,
                      phi = NULL, background_rel = 0.02, week_offset = NULL,
                      obs_cases = NULL, mass_weights = NULL) {
     nL <- nrow(obs_deaths); nT <- ncol(obs_deaths)
     years <- as.integer(years)
     if (!length(years) || anyNA(years)) stop("years must be a non-empty integer vector.")
     rep_len_chk <- function(x, nm) {
          if (length(x) == 1L) x <- rep(x, nL)
          if (length(x) != nL) stop(sprintf("%s must have length 1 or one per location (%d).", nm, nL))
          as.numeric(x)
     }
     sd_shift <- rep_len_chk(sd_shift, "sd_shift")
     if (length(sd_year) != 1L || !is.finite(sd_year) || sd_year <= 0)
          stop("sd_year must be a single positive number.")
     if (any(!is.finite(sd_shift) | sd_shift <= 0)) stop("sd_shift must be positive.")
     if (!is.null(phi)) {
          phi <- rep_len_chk(phi, "dispersion")
          if (any(!is.finite(phi) | phi <= 0)) stop("dispersion must be positive and finite.")
     }
     if (length(background_rel) != 1L || !is.finite(background_rel) || background_rel < 0)
          stop("background_rel must be a single non-negative number.")
     if (!is.null(week_offset)) week_offset <- as.integer(rep_len_chk(week_offset, "week_offset"))
     year_rep <- as.integer(format(dates, "%Y"))

     locs <- vector("list", nL)
     for (j in seq_len(nL)) {
          y <- obs_deaths[j, ]
          w <- if (is.null(weights)) ifelse(is.finite(y), 1, 0) else weights[j, ]
          w[!is.finite(w)] <- 0
          ok <- is.finite(y) & w > 0
          off <- if (!is.null(week_offset)) week_offset[j] else {
               src <- if (!is.null(obs_cases)) obs_cases[j, ] else y
               cad <- .nb_disp_cadence(src, dates)
               if (is.na(cad$share)) 0L else as.integer(cad$offset)
          }
          blk <- .nb_disp_block(dates, off)
          # A week is scored when every one of its days inside the data is scored;
          # a week cut by the start or end of the data counts on its own days.
          n_ok <- tapply(ok, blk, sum)
          n_all <- tapply(rep(1L, nT), blk, sum)
          good <- as.integer(names(n_ok))[n_ok == n_all & n_ok > 0L]
          wk <- ifelse(ok & blk %in% good, match(blk, good), NA_integer_)
          sel <- !is.na(wk)
          D_w <- W_w <- numeric(0)
          phi_j <- if (is.null(phi)) 1 else phi[j]
          if (any(sel)) {
               nd <- tabulate(wk[sel], nbins = length(good))
               D_w <- as.numeric(rowsum(y[sel], wk[sel], reorder = TRUE))
               W_w <- as.numeric(rowsum(w[sel], wk[sel], reorder = TRUE)) / nd
               if (!is.null(mass_weights)) {
                    mw <- mass_weights[j, ]; mw[!is.finite(mw)] <- 0
                    Wt_w <- as.numeric(rowsum(mw[sel], wk[sel], reorder = TRUE)) / nd
                    if (sum(W_w) > 0) W_w <- W_w * sum(Wt_w) / sum(W_w)
               }
               if (is.null(phi) && !is.null(obs_cases)) {
                    C_w <- as.numeric(rowsum(obs_cases[j, sel], wk[sel], reorder = TRUE))
                    yr_w <- as.integer(tapply(year_rep[sel], wk[sel], function(z) z[1L]))
                    phi_j <- .d7_dispersion(D_w, C_w, yr_w)
               }
          }
          bg_j <- max(1e-4, background_rel * (if (length(D_w)) mean(D_w) else 0))
          locs[[j]] <- list(day = which(sel), week = wk[sel], D = D_w, W = W_w,
                            sd_shift = sd_shift[j], phi = phi_j, bg = bg_j)
     }
     list(locs = locs, years = years, anchors = as.numeric(as.Date(paste0(years, "-07-01"))),
          sd_year = sd_year, nL = nL, nT = nT)
}

# Quasi-Poisson dispersion of weekly observed deaths around a year-specific
# multiple of the observed cases: how much more than Poisson the deaths scatter
# once the cases -- the model's exposure -- are known. Clamped at 1 (deaths that
# track the cases more tightly than Poisson are scored as Poisson), and 1 when the
# location has too few deaths or weeks to estimate it.
.d7_dispersion <- function(D, C, yr) {
     ok <- is.finite(D) & is.finite(C) & C > 0
     if (sum(D[ok]) < 10 || sum(ok) < length(unique(yr[ok])) + 3L) return(1)
     g <- tryCatch(suppressWarnings(stats::glm(D[ok] ~ 0 + factor(yr[ok]) + offset(log(C[ok])),
                                               family = stats::quasipoisson())),
                   error = function(e) NULL)
     if (is.null(g)) return(1)
     phi <- suppressWarnings(summary(g)$dispersion)
     if (length(phi) != 1L || !is.finite(phi)) 1 else max(1, phi)
}

# Year-deviation basis: for each day (numeric date), the weight on each year's
# deviation -- linear between consecutive 1 July anchors, flat before the first
# and after the last. [n_days x n_years]; every row sums to 1.
.d7_basis <- function(day_num, anchors) {
     n <- length(day_num); Y <- length(anchors)
     B <- matrix(0, n, Y)
     if (Y == 1L) { B[, 1L] <- 1; return(B) }
     pos <- findInterval(day_num, anchors)
     i1 <- pmax(pos, 1L); i2 <- pmin(pos + 1L, Y)
     w2 <- ifelse(i1 == i2, 0, (day_num - anchors[i1]) / (anchors[i2] - anchors[i1]))
     B[cbind(seq_len(n), i1)] <- 1 - w2
     B[cbind(seq_len(n), i2)] <- B[cbind(seq_len(n), i2)] + w2
     B
}

.d7_fit_all <- function(setup, exposure, base_logit, onset_day) {
     Y <- length(setup$years)
     B_all <- .d7_basis(onset_day, setup$anchors)
     theta <- matrix(0, setup$nL, 1L + Y,
                     dimnames = list(NULL, c("a", paste0("y", setup$years))))
     theta_sd <- theta
     ll <- numeric(setup$nL); conv <- logical(setup$nL); nw <- integer(setup$nL)
     vc <- vector("list", setup$nL)
     for (j in seq_len(setup$nL)) {
          L <- setup$locs[[j]]
          f <- .d7_fit_location(D = L$D, W = L$W, week = L$week,
                                X = exposure[j, L$day], eta0 = base_logit[j, L$day],
                                B = B_all[L$day, , drop = FALSE], sd_shift = L$sd_shift,
                                sd_year = setup$sd_year, phi = L$phi, bg = L$bg)
          ll[j] <- f$ll; conv[j] <- f$converged; nw[j] <- length(L$D)
          theta[j, ] <- f$theta; theta_sd[j, ] <- sqrt(pmax(diag(f$vcov), 0)); vc[[j]] <- f$vcov
     }
     list(ll = ll, theta = theta, theta_sd = theta_sd, vcov = vc, converged = conv,
          n_weeks = nw, years = setup$years,
          dispersion = vapply(setup$locs, function(L) L$phi, numeric(1)),
          background = vapply(setup$locs, function(L) L$bg, numeric(1)))
}

# Newton / Laplace for one location. theta = (a, delta_1..delta_Y); each scored
# day's linear predictor is eta0 + a + B[day, ] %*% delta.
.d7_fit_location <- function(D, W, week, X, eta0, B, sd_shift, sd_year, phi, bg,
                             max_iter = 60L, tol = 1e-9) {
     Y <- ncol(B); d <- 1L + Y
     prior_prec <- c(1 / sd_shift^2, rep(1 / sd_year^2, Y))
     prior_ld <- sum(stats::dnorm(0, 0, c(sd_shift, rep(sd_year, Y)), log = TRUE))
     nW <- length(D)
     if (!nW) {
          # No scored weeks: the posterior is the prior and the data contribute nothing.
          return(list(ll = 0, theta = rep(0, d), vcov = diag(1 / prior_prec, d), converged = TRUE))
     }
     Bd <- cbind(1, B)                        # d eta_day / d theta
     lg_const <- -lgamma(D + 1)
     # Days arrive in time order, so each week is a contiguous run of rows and a
     # weekly sum is a difference of cumulative sums at the run ends (much
     # cheaper than rowsum()'s grouping on this hot path).
     ends <- c(which(diff(week) != 0L), length(week))
     prev <- c(0L, ends[-length(ends)])
     wsum <- function(x) { cs <- cumsum(x); cs[ends] - c(0, cs[prev[-1L]]) }
     wsum_m <- function(M) {
          for (k in seq_len(ncol(M))) M[, k] <- cumsum(M[, k])   # (apply() is slower)
          M[ends, , drop = FALSE] - rbind(0, M[prev[-1L], , drop = FALSE])
     }

     eval_at <- function(th) {
          p <- stats::plogis(eta0 + as.numeric(Bd %*% th))
          m <- wsum(p * X) + bg
          list(p = p, m = m,
               lp = sum(W * (lg_const + D * log(m) - m)) / phi - 0.5 * sum(prior_prec * th^2))
     }
     deriv_at <- function(th, ev) {
          p <- ev$p; m <- ev$m
          g <- X * p * (1 - p)                 # d m_day / d eta
          h <- g * (1 - 2 * p)                 # d2 m_day / d eta2
          u <- W * (D / m - 1) / phi           # d loglik / d m_week
          v <- -W * D / (phi * m^2)            # d2 loglik / d m_week2
          Gw <- wsum_m(Bd * g)                                # d m_week / d theta
          grad <- as.numeric(crossprod(Gw, u)) - prior_prec * th
          H <- crossprod(Gw, Gw * v) + crossprod(Bd, Bd * (h * u[week]))
          diag(H) <- diag(H) - prior_prec
          Fi <- crossprod(Gw, Gw * (W / (phi * m)))
          diag(Fi) <- diag(Fi) + prior_prec
          list(grad = grad, H = H, fisher = Fi)
     }

     # Start at the offset that matches total observed to total expected deaths.
     th <- rep(0, d)
     ev <- eval_at(th)
     tot_x <- sum(ev$m) - nW * bg; tot_d <- sum(D)
     if (tot_x > 0 && tot_d > 0) th[1] <- max(-8, min(8, log(tot_d / tot_x)))
     ev <- eval_at(th)
     converged <- FALSE
     for (it in seq_len(max_iter)) {
          dv <- deriv_at(th, ev)
          R <- tryCatch(chol(-dv$H), error = function(e) NULL)
          step <- if (!is.null(R)) backsolve(R, forwardsolve(t(R), dv$grad))
                  else solve(dv$fisher, dv$grad)
          lam <- 1; stalled <- FALSE
          repeat {
               th_new <- th + lam * step
               ev_new <- eval_at(th_new)
               if (is.finite(ev_new$lp) && ev_new$lp >= ev$lp - 1e-12) break
               lam <- lam / 2
               if (lam < 1e-8) { stalled <- TRUE; break }
          }
          if (stalled) {
               # No ascent along the Newton direction: converged only if already stationary.
               converged <- max(abs(dv$grad)) < 1e-6
               break
          }
          delta <- max(abs(th_new - th))
          th <- th_new; ev <- ev_new
          if (delta < tol) { converged <- TRUE; break }
     }
     dv <- deriv_at(th, ev)
     R <- tryCatch(chol(-dv$H), error = function(e) NULL)
     if (is.null(R)) R <- chol(dv$fisher)
     logdet <- 2 * sum(log(diag(R)))
     vcov <- chol2inv(R)
     # log p(D, theta_hat) = loglik + log prior density; Laplace marginal adds
     # (d/2) log(2 pi) - (1/2) log det(-H).
     ll_marg <- ev$lp + prior_ld + 0.5 * d * log(2 * pi) - 0.5 * logdet
     list(ll = ll_marg, theta = th, vcov = vcov, converged = converged)
}


# ---------------------------------------------------------------------------
# run_MOSAIC() adapters
# ---------------------------------------------------------------------------

# Defaults used only when a priors object predates priors_default v16.0 and so
# carries no `mu_jt` entry (the caller is warned).
.MOSAIC_MU_JT_SD_YEAR_DEFAULT    <- 0.7
.MOSAIC_MU_JT_SD_PRODUCT_DEFAULT <- 0.3
.MOSAIC_MU_JT_LOGIT_SE_DEFAULT   <- 0.3

# Reported CFR as a [nL x nT] matrix from a config, resolved exactly as the
# engine resolves it (.mosaic_mu_jt_matrix(), including the legacy-config rule).
.mosaic_config_mu_jt <- function(config, nL, nT) {
     t(.mosaic_mu_jt_matrix(config, nticks = nT, npatches = nL))
}

# Resolve, once per calibration, everything the integrated deaths likelihood
# needs that does not depend on the simulated path. Mirrors the worker's
# scoring: the same sliced window, the same per-cell weights and deaths-prefix
# zeroing, the same weights_time. The per-observation confidence weights
# (config$reported_deaths_weight) are mass-preserving, as for cases; the weekly
# background is eps_rel_cases times the mean scored weekly deaths, the cases
# channel's relative floor; the dispersion is estimated per location from the
# observed deaths and cases.
.mosaic_resolve_deaths_integration <- function(config, control, priors, score_window) {
     obs_d <- config$reported_deaths
     if (is.null(obs_d)) return(NULL)
     if (!is.matrix(obs_d)) obs_d <- matrix(obs_d, nrow = 1L)
     obs_c <- config$reported_cases
     if (!is.null(obs_c) && !is.matrix(obs_c)) obs_c <- matrix(obs_c, nrow = 1L)
     nL <- nrow(obs_d); nT <- ncol(obs_d)
     if (!is.null(obs_c) && !identical(dim(obs_c), dim(obs_d))) obs_c <- NULL
     dates_full <- as.Date(config$date_start) + seq_len(nT) - 1L
     year_full <- as.integer(format(dates_full, "%Y"))

     s0 <- if (!is.null(score_window)) min(score_window$idx_cases, score_window$idx_deaths) else 1L
     keep <- s0:nT
     w_time <- matrix(1, nL, length(keep))
     wt <- control$likelihood$.weights_time_resolved
     if (!is.null(wt)) w_time <- sweep(w_time, 2L, wt[keep], `*`)
     if (!is.null(score_window)) {
          prefix <- score_window$idx_deaths - s0
          if (prefix > 0L) w_time[, seq_len(prefix)] <- 0
     }
     w_time[!is.finite(w_time)] <- 0
     w <- w_time
     if (!is.null(config$reported_deaths_weight)) {
          wo <- config$reported_deaths_weight
          if (!is.matrix(wo)) wo <- matrix(wo, nrow = 1L)
          w <- w * wo[, keep, drop = FALSE]
     }
     w[!is.finite(w)] <- 0

     mu <- .mosaic_config_mu_jt(config, nL, nT)
     # Numerical guard only: a zero CFR has logit -Inf, and a shift of a few
     # logit units cannot lift 1e-12 to an observable level.
     base_logit_full <- stats::qlogis(pmin(pmax(mu, 1e-12), 1 - 1e-12))

     years <- sort(unique(year_full))
     # Years with observed deaths in the scored window: the location offset's width
     # averages the prior centres' SEs over these, not over forecast years.
     obs_years <- unique(year_full[keep][colSums(is.finite(obs_d[, keep, drop = FALSE])) > 0])
     pm <- priors$mu_jt
     if (is.null(pm) || is.null(pm$location)) {
          .mosaic_warn_once("mu_jt_prior_missing", paste0(
               "The priors object carries no `mu_jt` entry (priors_default v16.0 or later has one); ",
               "the integrated deaths likelihood uses default widths (sd_year ",
               .MOSAIC_MU_JT_SD_YEAR_DEFAULT, ", location sd ",
               round(sqrt(.MOSAIC_MU_JT_SD_PRODUCT_DEFAULT^2 + .MOSAIC_MU_JT_LOGIT_SE_DEFAULT^2), 3), ")."))
          sd_year <- .MOSAIC_MU_JT_SD_YEAR_DEFAULT
          sd_shift <- rep(sqrt(.MOSAIC_MU_JT_SD_PRODUCT_DEFAULT^2 + .MOSAIC_MU_JT_LOGIT_SE_DEFAULT^2), nL)
     } else {
          sd_year <- as.numeric(pm$sd_year)
          sd_prod <- as.numeric(pm$sd_product)
          sd_shift <- vapply(config$location_name, function(iso) {
               L <- pm$location[[iso]]
               se <- if (is.null(L)) NA_real_ else {
                    yy <- as.integer(unlist(L$year)); ss <- as.numeric(unlist(L$logit_se))
                    in_win <- yy %in% obs_years
                    if (any(in_win)) sqrt(mean(ss[in_win]^2)) else sqrt(mean(ss^2))
               }
               if (!is.finite(se)) se <- .MOSAIC_MU_JT_LOGIT_SE_DEFAULT
               sqrt(sd_prod^2 + se^2)
          }, numeric(1))
     }

     eps <- control$likelihood$eps_rel_cases
     if (is.null(eps) || length(eps) != 1L || !is.finite(eps) || eps < 0) eps <- 0.02
     setup <- .d7_setup(obs_deaths = obs_d[, keep, drop = FALSE], weights = w,
                        dates = dates_full[keep], years = years,
                        sd_shift = sd_shift, sd_year = sd_year, phi = NULL,
                        background_rel = eps,
                        obs_cases = if (is.null(obs_c)) NULL else obs_c[, keep, drop = FALSE],
                        mass_weights = w_time)
     list(setup = setup, base_logit_full = base_logit_full, year_full = year_full,
          dates_full = dates_full, keep = keep, n_time = nT, years = years,
          sd_shift = unname(sd_shift), sd_year = sd_year,
          dispersion = vapply(setup$locs, function(L) L$phi, numeric(1)),
          background = vapply(setup$locs, function(L) L$bg, numeric(1)))
}

# Exposure, base logit and onset day aligned to each reporting column. A
# reported death in results column c comes from onsets in column c - s, with
# s = delta_reporting_cases + 1: fatal onsets drawn at tick t are recorded one
# row later (the row of new_symptomatic), and the reporting draw reads them
# delta_reporting_cases ticks after that.
.mosaic_deaths_exposure <- function(di, onsets, params) {
     if (!is.matrix(onsets)) onsets <- matrix(onsets, nrow = 1L)
     nT <- di$n_time
     if (ncol(onsets) != nT || nrow(onsets) != nrow(di$base_logit_full))
          stop(sprintf("new_symptomatic is %d x %d; the deaths integration expects %d x %d.",
                       nrow(onsets), ncol(onsets), nrow(di$base_logit_full), nT))
     s <- as.integer(params$delta_reporting_cases) + 1L
     src <- seq_len(nT) - s
     ok <- src >= 1L
     ratio <- params$rho / params$chi_epidemic
     X <- matrix(0, nrow(onsets), nT)
     X[, ok] <- ratio * onsets[, src[ok], drop = FALSE]
     eta <- di$base_logit_full[, pmax(src, 1L), drop = FALSE]
     list(X = X, eta = eta, onset_day = as.numeric(di$dates_full[1L]) + (src - 1))
}

# Per-simulation integrated deaths log-likelihood (and the conditional posterior
# of the CFR offsets when the caller needs it).
.mosaic_deaths_ll_integrated <- function(di, results, params) {
     ex <- .mosaic_deaths_exposure(di, results$new_symptomatic, params)
     k <- di$keep
     .d7_fit_all(di$setup, exposure = ex$X[, k, drop = FALSE],
                 base_logit = ex$eta[, k, drop = FALSE], onset_day = ex$onset_day[k])
}

# Redraw a simulated path's deaths from the reported CFR's conditional posterior.
#
# Deaths do not feed back into transmission beyond removing fatal onsets from
# Isym (a fraction of a percent of symptomatic person-days), so given the path's
# onsets they can be drawn after the simulation. For each location: fit the CFR
# offsets to the observed deaths given this path (the quasi-Poisson fit, whose
# level reproduces the observed deaths totals), draw (a, delta) from the Laplace
# posterior, build the daily CFR with the same smooth year basis, and draw fatal
# onsets and reported deaths with the alignment the engine uses. Years without
# scored weeks -- including forecast years past the last observation -- carry the
# location offset a with a deviation drawn from its prior, so a forecast
# inherits the calibrated level and eases back to it over half a year.
#
# A location whose posterior-mode CFR would need a per-onset fatality
# probability >= 1 (a path that produces far too few onsets for the observed
# deaths) keeps the engine's own deaths and reports NA CFRs; the member -- its
# cases and every other location -- is kept, and `n_infeasible` counts such
# locations so the caller can report them. Dropping the member instead would
# bias the ensemble against exactly those paths.
#
# RNG: seeded locally with the engine's generator (.sim_rng_begin(), pinned to
# Mersenne-Twister whatever the caller's RNGkind) and the caller's stream
# restored, so the redraw is reproducible per (param_idx, stoch_idx) across
# sequential and parallel runs and never perturbs the caller.
.mosaic_posthoc_deaths <- function(di, results, params, seed) {
     fit <- .mosaic_deaths_ll_integrated(di, results, params)
     O <- results$new_symptomatic
     if (!is.matrix(O)) O <- matrix(O, nrow = 1L)
     nL <- nrow(O); nT <- ncol(O)
     conv <- params$rho / (params$rho_deaths * params$chi_epidemic)
     lc <- as.integer(params$delta_reporting_cases)
     # The CFR that applies to onsets in column k is the one at day k.
     Bk <- cbind(1, .d7_basis(as.numeric(di$dates_full), di$setup$anchors))
     yr_f <- factor(di$year_full, levels = di$years)

     rng_state <- .sim_rng_begin(seed)
     on.exit(.sim_rng_end(rng_state), add = TRUE)

     as_m <- function(x) if (is.null(x) || is.matrix(x)) x else matrix(x, nrow = 1L)
     eng_dd <- as_m(results$disease_deaths); eng_rd <- as_m(results$reported_deaths)
     n_infeasible <- 0L
     disease <- matrix(0L, nL, nT)
     reported <- matrix(0L, nL, nT)
     cfr_year <- matrix(NA_real_, nL, length(di$years), dimnames = list(NULL, di$years))
     for (j in seq_len(nL)) {
          R <- chol(fit$vcov[[j]])
          mu_j <- NULL
          for (attempt in 1:20) {
               th <- fit$theta[j, ] + as.numeric(crossprod(R, stats::rnorm(ncol(R))))
               mu_try <- stats::plogis(di$base_logit_full[j, ] + as.numeric(Bk %*% th))
               if (all(mu_try * conv < 1)) { mu_j <- mu_try; break }
          }
          if (is.null(mu_j)) {
               mu_mode <- stats::plogis(di$base_logit_full[j, ] + as.numeric(Bk %*% fit$theta[j, ]))
               if (all(mu_mode * conv < 1)) {
                    mu_j <- mu_mode
               } else {
                    n_infeasible <- n_infeasible + 1L
                    if (!is.null(eng_dd)) disease[j, ] <- as.integer(eng_dd[j, ])
                    if (!is.null(eng_rd)) reported[j, ] <- as.integer(eng_rd[j, ])
                    next
               }
          }
          cfr_year[j, ] <- as.numeric(tapply(mu_j, yr_f, mean))
          # Fatal onsets from column k are recorded in column k + 1 (the engine's
          # next-row write); the final column's onsets fall past the window.
          if (nT > 1L) {
               k <- seq_len(nT - 1L)
               disease[j, k + 1L] <- stats::rbinom(nT - 1L, O[j, k], mu_j[k] * conv)
          }
          c_idx <- (lc + 1L):nT
          if (length(c_idx) && lc + 1L <= nT) {
               reported[j, c_idx] <- stats::rbinom(length(c_idx), disease[j, c_idx - lc], params$rho_deaths)
          }
     }
     list(reported_deaths = reported, disease_deaths = disease, cfr_year = cfr_year,
          theta = fit$theta, n_infeasible = n_infeasible)
}

# Move a config's reported CFR to a posterior level.
#
# The CFR is integrated out, not sampled, so a config sampled after calibration
# (config_medoid.json) still carries the prior mu_jt. This shifts logit mu_jt by
# a smooth offset -- one value per calendar year, interpolated between 1 July
# anchors like the integration's year deviations -- solved so that each year's
# mean daily CFR equals that year's `cfr_median` in `cfr_posterior`
# (calc_model_ensemble()$cfr_posterior). The prior's within-year shape is kept,
# the CFR has no step at a year boundary, and a re-simulation of the returned
# config draws deaths at the posterior CFR in the engine. Years the posterior
# does not cover keep a zero offset at their anchor.
#
# The returned config is always in the v0.96.0 form. A legacy config's constant
# CFR_target is the prior it starts from, and its retired mortality fields are
# dropped so the engine reads the new mu_jt. A location whose shifted CFR would
# need a per-onset fatality probability >= 1 is refused rather than clamped.
.mosaic_apply_cfr_posterior <- function(config, cfr_posterior) {
     need <- c("location", "year", "cfr_median")
     if (!is.data.frame(cfr_posterior) || !all(need %in% names(cfr_posterior)))
          stop("cfr_posterior must be a data frame with columns ", paste(need, collapse = ", "), ".",
               call. = FALSE)
     nL <- length(config$location_name)
     dates <- seq.Date(as.Date(config$date_start), as.Date(config$date_stop), by = "day")
     nT <- length(dates)
     mu <- .mosaic_config_mu_jt(config, nL, nT)
     yr <- as.integer(format(dates, "%Y"))
     years <- sort(unique(yr))
     B <- .d7_basis(as.numeric(dates), as.numeric(as.Date(paste0(years, "-07-01"))))
     yr_f <- factor(yr, levels = years)
     out <- mu
     for (i in seq_len(nL)) {
          rows <- cfr_posterior[cfr_posterior$location == config$location_name[i] &
                                  is.finite(cfr_posterior$cfr_median) &
                                  cfr_posterior$cfr_median > 0 & cfr_posterior$cfr_median < 1, ,
                                drop = FALSE]
          if (!nrow(rows)) next
          pos <- mu[i, ] > 0
          if (!any(pos)) next
          target <- rows$cfr_median[match(years, rows$year)]
          has <- is.finite(target)
          lg <- stats::qlogis(mu[i, pos])
          s_y <- rep(0, length(years))
          # Fixed point on the yearly offsets: the basis is nearly diagonal at the
          # yearly level, so this converges in a handful of steps.
          for (it in 1:100) {
               cur <- as.numeric(tapply(stats::plogis(lg + as.numeric(B[pos, , drop = FALSE] %*% s_y)),
                                        yr_f[pos], mean))
               upd <- ifelse(has & is.finite(cur), stats::qlogis(target) - stats::qlogis(cur), 0)
               s_y <- s_y + upd
               if (max(abs(upd)) < 1e-10) break
          }
          out[i, pos] <- stats::plogis(lg + as.numeric(B[pos, , drop = FALSE] %*% s_y))
     }
     if (!is.null(config$rho_deaths) && config$rho_deaths > 0) {
          p_max <- apply(out, 1L, max) * config$rho / (config$rho_deaths * config$chi_epidemic)
          if (any(p_max >= 1))
               stop(sprintf("the posterior CFR for %s needs a per-onset fatality probability >= 1.",
                            paste(config$location_name[p_max >= 1], collapse = ", ")), call. = FALSE)
     }
     dimnames(out) <- dimnames(config$mu_jt)
     for (f in c(.MOSAIC_LEGACY_MORTALITY_FIELDS, "delta_reporting_deaths")) config[[f]] <- NULL
     config$mu_jt <- out
     config
}
