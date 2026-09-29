#' Deaths log-likelihood with the reported CFR integrated out
#'
#' @description
#' Scores observed deaths against a simulated path with the reported case
#' fatality ratio (CFR) integrated out analytically. Given a path, expected
#' reported deaths on each day are the CFR times a known exposure, so the CFR's
#' level can be solved for per path rather than sampled. The CFR is modelled as
#' the time-varying prior \code{mu_jt} shifted on the logit scale by a
#' location-level offset and a deviation per calendar year:
#' \deqn{\mathrm{logit}\,\mu_{jt} = \mathrm{logit}\,\mu^{0}_{jt} + a_j +
#'   \delta_{j,y(t)},\qquad a_j \sim N(0, s_j^2),\quad \delta_{j,y} \sim N(0, \sigma_y^2).}
#' Deaths are aggregated to complete reporting weeks and scored with a negative
#' binomial. For each location the offsets are fitted by Newton's method and the
#' marginal likelihood is the Laplace approximation at the mode.
#'
#' @param obs_deaths Matrix [locations x days] (or vector, one location) of observed reported deaths.
#' @param exposure Matrix of the same shape: expected reported deaths per unit reported CFR on each day, \code{(rho / chi_epidemic) * onsets} at the onset day.
#' @param base_logit Matrix of the same shape: logit of the prior \code{mu_jt} at the onset day.
#' @param year Integer vector, one per day: calendar year of the onset day (selects the year deviation).
#' @param dates Date vector, one per day: the reporting day (defines the weekly blocks).
#' @param sd_shift Numeric, length 1 or one per location: prior SD of the location offset \eqn{a_j} (logit scale).
#' @param sd_year Numeric scalar: prior SD of each year deviation \eqn{\delta_{j,y}} (logit scale).
#' @param k Numeric, length 1 or one per location: weekly negative-binomial dispersion (\code{Inf} for Poisson).
#' @param weights Optional matrix of per-day scoring weights (0 or \code{NA} = not scored); defaults to 1 wherever \code{obs_deaths} is finite.
#' @param week_offset Optional integer, length 1 or one per location, 0-6 days from Monday for the reporting-week boundary; detected from \code{obs_deaths} when \code{NULL}.
#' @param years Optional integer vector of the years to carry deviations for (default: every year in \code{year}); years without scored weeks keep their prior.
#'
#' @details
#' A week is scored only when all seven days are finite and carry positive
#' weight; its weight is the mean daily weight. There is no floor on the
#' expected deaths other than a numerical guard (1e-10), which binds only in a
#' week where the path has no onsets at all -- a genuine misfit.
#'
#' @return A list with \code{ll} (marginal log-likelihood per location),
#'   \code{theta} (matrix [locations x (1 + years)]: the offset \code{a} and the
#'   year deviations at the mode), \code{theta_sd} (their Laplace posterior SDs),
#'   \code{vcov} (list of Laplace covariance matrices), \code{converged}
#'   (logical per location), \code{n_weeks} (scored weeks per location) and
#'   \code{years}.
#'
#' @seealso \code{\link{calc_model_likelihood}}, \code{\link{est_CFR_hierarchical}}
#' @examples
#' dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 140)
#' set.seed(1)
#' expo <- rpois(140, 400) * 0.423 / 0.75
#' obs <- rpois(140, 0.02 * expo)
#' fit <- calc_log_likelihood_deaths_integrated(
#'      obs, expo, base_logit = rep(qlogis(0.015), 140),
#'      year = rep(2024L, 140), dates = dates, sd_shift = 0.5, sd_year = 0.7, k = Inf)
#' plogis(qlogis(0.015) + fit$theta[1, 1])   # recovered reported CFR, ~0.02
#' @export
calc_log_likelihood_deaths_integrated <- function(obs_deaths, exposure, base_logit, year, dates,
                                                  sd_shift, sd_year, k = Inf,
                                                  weights = NULL, week_offset = NULL,
                                                  years = NULL) {
     as_mat <- function(x) if (is.matrix(x)) x else matrix(x, nrow = 1L)
     obs_deaths <- as_mat(obs_deaths); exposure <- as_mat(exposure); base_logit <- as_mat(base_logit)
     nL <- nrow(obs_deaths); nT <- ncol(obs_deaths)
     if (!identical(dim(exposure), dim(obs_deaths)) || !identical(dim(base_logit), dim(obs_deaths)))
          stop("obs_deaths, exposure and base_logit must have the same dimensions.")
     if (length(year) != nT || length(dates) != nT)
          stop("year and dates must have one entry per day (column).")
     if (!is.null(weights)) {
          weights <- as_mat(weights)
          if (!identical(dim(weights), dim(obs_deaths))) stop("weights must match obs_deaths.")
     }
     setup <- .d7_setup(obs_deaths = obs_deaths, weights = weights, dates = as.Date(dates),
                        year = as.integer(year), years = years, sd_shift = sd_shift,
                        sd_year = sd_year, k = k, week_offset = week_offset)
     .d7_fit_all(setup, exposure = exposure, base_logit = base_logit, year = as.integer(year))
}


# Precompute everything that depends only on the observations: the weekly
# blocks, weekly observed totals and weights, the year labels, and the priors.
.d7_setup <- function(obs_deaths, weights, dates, year, years, sd_shift, sd_year, k,
                      week_offset = NULL, obs_cases = NULL) {
     nL <- nrow(obs_deaths); nT <- ncol(obs_deaths)
     if (is.null(years)) years <- sort(unique(year[is.finite(year)]))
     years <- as.integer(years)
     rep_len_chk <- function(x, nm) {
          if (length(x) == 1L) x <- rep(x, nL)
          if (length(x) != nL) stop(sprintf("%s must have length 1 or one per location (%d).", nm, nL))
          as.numeric(x)
     }
     sd_shift <- rep_len_chk(sd_shift, "sd_shift")
     k <- rep_len_chk(k, "k")
     if (length(sd_year) != 1L || !is.finite(sd_year) || sd_year <= 0)
          stop("sd_year must be a single positive number.")
     if (any(!is.finite(sd_shift) | sd_shift <= 0)) stop("sd_shift must be positive.")
     if (any(!(is.finite(k) | is.infinite(k)) | k <= 0)) stop("k must be positive (Inf for Poisson).")
     if (!is.null(week_offset)) week_offset <- as.integer(rep_len_chk(week_offset, "week_offset"))

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
          # A week counts only when all seven of its days are scored.
          n_ok <- tapply(ok, blk, sum)
          n_all <- tapply(rep(1L, nT), blk, sum)
          good <- as.integer(names(n_ok))[n_ok == 7L & n_all == 7L]
          wk <- ifelse(ok & blk %in% good, match(blk, good), NA_integer_)
          sel <- !is.na(wk)
          D_w <- if (any(sel)) as.numeric(rowsum(y[sel], wk[sel], reorder = TRUE)) else numeric(0)
          W_w <- if (any(sel)) as.numeric(rowsum(w[sel], wk[sel], reorder = TRUE)) / 7 else numeric(0)
          locs[[j]] <- list(day = which(sel), week = wk[sel], D = D_w, W = W_w,
                            sd_shift = sd_shift[j], k = k[j])
     }
     list(locs = locs, years = years, sd_year = sd_year, nL = nL, nT = nT)
}

.d7_fit_all <- function(setup, exposure, base_logit, year) {
     Y <- length(setup$years)
     yi <- match(year, setup$years)
     theta <- matrix(0, setup$nL, 1L + Y,
                     dimnames = list(NULL, c("a", paste0("y", setup$years))))
     theta_sd <- theta
     ll <- numeric(setup$nL); conv <- logical(setup$nL); nw <- integer(setup$nL)
     vc <- vector("list", setup$nL)
     for (j in seq_len(setup$nL)) {
          L <- setup$locs[[j]]
          f <- .d7_fit_location(D = L$D, W = L$W, week = L$week,
                                X = exposure[j, L$day], eta0 = base_logit[j, L$day],
                                yi = yi[L$day], Y = Y, sd_shift = L$sd_shift,
                                sd_year = setup$sd_year, k = L$k)
          ll[j] <- f$ll; conv[j] <- f$converged; nw[j] <- length(L$D)
          theta[j, ] <- f$theta; theta_sd[j, ] <- sqrt(pmax(diag(f$vcov), 0)); vc[[j]] <- f$vcov
     }
     list(ll = ll, theta = theta, theta_sd = theta_sd, vcov = vc, converged = conv,
          n_weeks = nw, years = setup$years)
}

# Newton / Laplace for one location. Parameters theta = (a, delta_1..delta_Y).
.d7_fit_location <- function(D, W, week, X, eta0, yi, Y, sd_shift, sd_year, k,
                             max_iter = 60L, tol = 1e-9) {
     d <- 1L + Y
     prior_prec <- c(1 / sd_shift^2, rep(1 / sd_year^2, Y))
     prior_ld <- sum(stats::dnorm(0, 0, c(sd_shift, rep(sd_year, Y)), log = TRUE))
     nW <- length(D)
     if (!nW) {
          # No scored weeks: the posterior is the prior and the data contribute nothing.
          return(list(ll = 0, theta = rep(0, d), vcov = diag(1 / prior_prec, d), converged = TRUE))
     }
     X[!is.finite(X)] <- 0
     pois <- is.infinite(k)
     lg_const <- if (pois) -lgamma(D + 1) else lgamma(D + k) - lgamma(k) - lgamma(D + 1)
     floor_m <- 1e-10

     week_ll <- function(m) {
          m <- pmax(m, floor_m)
          if (pois) lg_const + D * log(m) - m
          else lg_const + k * log(k / (k + m)) + D * log(m / (k + m))
     }
     eval_at <- function(th) {
          e <- eta0 + th[1] + th[1L + yi]
          p <- stats::plogis(e)
          m_d <- p * X
          m <- as.numeric(rowsum(m_d, week, reorder = TRUE))
          list(p = p, m = m, lp = sum(W * week_ll(m)) - 0.5 * sum(prior_prec * th^2))
     }
     deriv_at <- function(th, ev) {
          p <- ev$p; m <- pmax(ev$m, floor_m)
          g <- X * p * (1 - p)                 # dm_day / d eta
          h <- g * (1 - 2 * p)                 # d2 m_day / d eta2
          if (pois) { u <- D / m - 1; v <- -D / m^2 }
          else { u <- D / m - (D + k) / (k + m); v <- -D / m^2 + (D + k) / (k + m)^2 }
          u[ev$m <= floor_m] <- 0; v[ev$m <= floor_m] <- 0
          # d m_week / d theta: column 1 for a, columns 1 + y for the year deviations.
          Gw <- matrix(0, nW, d)
          Gw[, 1] <- as.numeric(rowsum(g, week, reorder = TRUE))
          gy <- rowsum(g, week * (Y + 1L) + yi, reorder = TRUE)
          key <- as.integer(rownames(gy))
          Gw[cbind(key %/% (Y + 1L), 1L + key %% (Y + 1L))] <- gy[, 1]
          wu <- W * u; wv <- W * v
          grad <- as.numeric(crossprod(Gw, wu)) - prior_prec * th
          H <- crossprod(Gw, Gw * wv)
          # Second-derivative term: sum over days of W*u(week) * h, placed on the
          # (a, a), (a, y), (y, y) cells of that day's year.
          hu <- h * wu[week]
          H[1, 1] <- H[1, 1] + sum(hu)
          hy <- as.numeric(tapply(hu, factor(yi, levels = seq_len(Y)), sum))
          hy[is.na(hy)] <- 0
          idx <- 1L + seq_len(Y)
          H[1, idx] <- H[1, idx] + hy
          H[idx, 1] <- H[idx, 1] + hy
          H[cbind(idx, idx)] <- H[cbind(idx, idx)] + hy
          diag(H) <- diag(H) - prior_prec
          Fi <- crossprod(Gw, Gw * (W / (m + (if (pois) 0 else m^2 / k))))
          diag(Fi) <- diag(Fi) + prior_prec
          list(grad = grad, H = H, fisher = Fi)
     }

     # Start at the offset that matches total observed to total expected deaths.
     th <- rep(0, d)
     ev <- eval_at(th)
     tot_m <- sum(ev$m); tot_d <- sum(D)
     if (tot_m > 0 && tot_d > 0) th[1] <- max(-8, min(8, log(tot_d / tot_m)))
     ev <- eval_at(th)
     converged <- FALSE
     for (it in seq_len(max_iter)) {
          dv <- deriv_at(th, ev)
          negH <- -dv$H
          R <- tryCatch(chol(negH), error = function(e) NULL)
          step <- if (!is.null(R)) backsolve(R, forwardsolve(t(R), dv$grad))
                  else solve(dv$fisher, dv$grad)
          lam <- 1
          repeat {
               th_new <- th + lam * step
               ev_new <- eval_at(th_new)
               if (is.finite(ev_new$lp) && ev_new$lp >= ev$lp - 1e-12) break
               lam <- lam / 2
               if (lam < 1e-8) { th_new <- th; ev_new <- ev; break }
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
# zeroing, the same weights_time.
.mosaic_resolve_deaths_integration <- function(config, control, priors, score_window) {
     obs_d <- config$reported_deaths
     if (is.null(obs_d)) return(NULL)
     if (!is.matrix(obs_d)) obs_d <- matrix(obs_d, nrow = 1L)
     obs_c <- config$reported_cases
     if (!is.null(obs_c) && !is.matrix(obs_c)) obs_c <- matrix(obs_c, nrow = 1L)
     nL <- nrow(obs_d); nT <- ncol(obs_d)
     dates_full <- as.Date(config$date_start) + seq_len(nT) - 1L
     year_full <- as.integer(format(dates_full, "%Y"))

     s0 <- if (!is.null(score_window)) min(score_window$idx_cases, score_window$idx_deaths) else 1L
     keep <- s0:nT
     w <- matrix(1, nL, length(keep))
     wt <- control$likelihood$.weights_time_resolved
     if (!is.null(wt)) w <- sweep(w, 2L, wt[keep], `*`)
     if (!is.null(config$reported_deaths_weight)) {
          wo <- config$reported_deaths_weight
          if (!is.matrix(wo)) wo <- matrix(wo, nrow = 1L)
          w <- w * wo[, keep, drop = FALSE]
     }
     if (!is.null(score_window)) {
          prefix <- score_window$idx_deaths - s0
          if (prefix > 0L) w[, seq_len(prefix)] <- 0
     }
     w[!is.finite(w)] <- 0

     mu <- .mosaic_config_mu_jt(config, nL, nT)
     # Numerical guard only: a zero CFR has logit -Inf, and a shift of a few
     # logit units cannot lift 1e-12 to an observable level.
     base_logit_full <- stats::qlogis(pmin(pmax(mu, 1e-12), 1 - 1e-12))

     years <- sort(unique(year_full))
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
                    in_win <- yy %in% years
                    if (any(in_win)) sqrt(mean(ss[in_win]^2)) else sqrt(mean(ss^2))
               }
               if (!is.finite(se)) se <- .MOSAIC_MU_JT_LOGIT_SE_DEFAULT
               sqrt(sd_prod^2 + se^2)
          }, numeric(1))
     }

     k <- control$likelihood$.nb_k_deaths_resolved
     if (is.null(k)) k <- rep(Inf, nL)
     setup <- .d7_setup(obs_deaths = obs_d[, keep, drop = FALSE], weights = w,
                        dates = dates_full[keep], year = year_full[keep], years = years,
                        sd_shift = sd_shift, sd_year = sd_year, k = k,
                        obs_cases = if (is.null(obs_c)) NULL else obs_c[, keep, drop = FALSE])
     list(setup = setup, base_logit_full = base_logit_full, year_full = year_full,
          dates_full = dates_full, keep = keep, n_time = nT, years = years,
          sd_shift = unname(sd_shift), sd_year = sd_year)
}

# Exposure, base logit and onset year aligned to each reporting column. A
# reported death in results column c comes from onsets in column c - s, with
# s = delta_reporting_cases + 1: fatal onsets drawn at tick t are recorded one
# row later (the row of new_symptomatic), and the reporting draw reads them
# delta_reporting_cases ticks after that.
.mosaic_deaths_exposure <- function(di, onsets, params) {
     if (!is.matrix(onsets)) onsets <- matrix(onsets, nrow = 1L)
     nT <- di$n_time
     s <- as.integer(params$delta_reporting_cases) + 1L
     src <- seq_len(nT) - s
     ok <- src >= 1L
     ratio <- params$rho / params$chi_epidemic
     X <- matrix(0, nrow(onsets), nT)
     X[, ok] <- ratio * onsets[, src[ok], drop = FALSE]
     eta <- di$base_logit_full[, pmax(src, 1L), drop = FALSE]
     yr <- di$year_full[pmax(src, 1L)]
     list(X = X, eta = eta, year = yr)
}

# Per-simulation integrated deaths log-likelihood (and the conditional posterior
# of the CFR offsets when the caller needs it).
.mosaic_deaths_ll_integrated <- function(di, results, params) {
     ex <- .mosaic_deaths_exposure(di, results$new_symptomatic, params)
     k <- di$keep
     .d7_fit_all(di$setup, exposure = ex$X[, k, drop = FALSE],
                 base_logit = ex$eta[, k, drop = FALSE], year = ex$year[k])
}

# Redraw a simulated path's deaths from the reported CFR's conditional posterior.
#
# Deaths do not feed back into transmission beyond removing fatal onsets from
# Isym (a fraction of a percent of symptomatic person-days), so given the path's
# onsets they can be drawn after the simulation. For each location: fit the CFR
# offsets to the observed deaths given this path, draw (a, delta) from the
# Laplace posterior, and draw fatal onsets and reported deaths with the same
# alignment the engine uses. Years without scored weeks -- including forecast
# years past the last observation -- carry the location offset a with a year
# deviation drawn from its prior, so a forecast inherits the calibrated level.
#
# RNG: seeded locally and the caller's stream restored, so the redraw is
# reproducible per (param_idx, stoch_idx) and never perturbs the caller.
.mosaic_posthoc_deaths <- function(di, results, params, seed) {
     fit <- .mosaic_deaths_ll_integrated(di, results, params)
     O <- results$new_symptomatic
     if (!is.matrix(O)) O <- matrix(O, nrow = 1L)
     nL <- nrow(O); nT <- ncol(O)
     conv <- params$rho / (params$rho_deaths * params$chi_epidemic)
     lc <- as.integer(params$delta_reporting_cases)
     yi_full <- match(di$year_full, di$years)

     old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
          get(".Random.seed", envir = .GlobalEnv) else NULL
     on.exit({
          if (is.null(old_seed)) {
               if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
                    rm(".Random.seed", envir = .GlobalEnv)
          } else assign(".Random.seed", old_seed, envir = .GlobalEnv)
     }, add = TRUE)
     set.seed(as.integer(seed))

     disease <- matrix(0L, nL, nT)
     reported <- matrix(0L, nL, nT)
     cfr_year <- matrix(NA_real_, nL, length(di$years), dimnames = list(NULL, di$years))
     for (j in seq_len(nL)) {
          R <- chol(fit$vcov[[j]])
          mu_j <- NULL
          for (attempt in 1:20) {
               th <- fit$theta[j, ] + as.numeric(crossprod(R, stats::rnorm(ncol(R))))
               mu_try <- stats::plogis(di$base_logit_full[j, ] + th[1] + th[1L + yi_full])
               if (all(mu_try * conv < 1)) { mu_j <- mu_try; break }
          }
          if (is.null(mu_j)) {
               .mosaic_warn_once("posthoc_cfr_draw_mode", paste0(
                    "A posterior reported-CFR draw implied a per-onset fatality probability >= 1 in ",
                    "20 attempts; that member uses the posterior mode instead."))
               th <- fit$theta[j, ]
               mu_j <- stats::plogis(di$base_logit_full[j, ] + th[1] + th[1L + yi_full])
               if (any(mu_j * conv >= 1))
                    stop("The posterior-mode reported CFR implies a per-onset fatality probability >= 1.")
          }
          cfr_year[j, ] <- as.numeric(tapply(mu_j, factor(yi_full, levels = seq_along(di$years)), mean))
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
          theta = fit$theta)
}
