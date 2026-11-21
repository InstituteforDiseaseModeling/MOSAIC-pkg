# Quasi-Poisson P-spline smoother for calc_outbreak_onset().
#
# A port of the smoother of the onset-detection handoff (18 November 2026;
# qp_core.py): cubic B-splines on equal segments, a second-order difference
# penalty, log link, a penalized Newton fit on the reported weeks only, the
# smoothing parameter chosen by GCV (with a gamma inflation of the effective
# degrees of freedom) over a fixed grid, the dispersion from the deviance, and
# the weekly growth rate r(t) = d log mu / dt with its standard error from
# phi * (B'WB + lambda S)^-1. Week t of the analysed series sits at x = t - 1,
# so r is per week.
#
# One departure: the segments have a fixed width in weeks, anchored at the
# first analysed week, where the handoff divided each series into 60 segments.
# With a fixed width the fit to weeks 1..T does not depend on how many weeks
# follow T (basis functions with no data are pinned by the penalty alone and
# extend the fit linearly at no cost), so a real-time vintage uses only the
# data known at T. A width of n / 60 reproduces the handoff's knots exactly.


.onset_fit_error <- function(message) {
     structure(class = c("onset_fit_error", "error", "condition"), list(message = message, call = NULL))
}


# B-spline basis, its first derivative, and the second-order difference
# penalty for n_total consecutive weeks, on segments of segment_weeks weeks
# from x = -0.5 (as many as cover the weeks), with `degree` extra knots at
# each end.
.onset_basis <- function(n_total, segment_weeks, degree = 3L) {
     lo <- -0.5
     n_seg <- max(1L, ceiling(n_total / segment_weeks - 1e-9))
     knots <- lo + segment_weeks * seq(-degree, n_seg + degree)
     x <- seq(0, n_total - 1)
     B <- splines::splineDesign(knots, x, ord = degree + 1L, derivs = 0L)
     dB <- splines::splineDesign(knots, x, ord = degree + 1L, derivs = 1L)
     D <- diff(diag(ncol(B)), differences = 2L)
     list(B = B, dB = dB, S = crossprod(D))
}


# Moore-Penrose inverse with numpy's default cut-off (singular values below
# 1e-15 times the largest are treated as zero).
.onset_pinv <- function(M) {
     s <- tryCatch(svd(M), error = function(e) stop(.onset_fit_error(conditionMessage(e))))
     keep <- s$d > 1e-15 * max(s$d)
     s$v[, keep, drop = FALSE] %*% (t(s$u[, keep, drop = FALSE]) / s$d[keep])
}


# Inverse of a symmetric positive definite matrix by Cholesky, falling back to
# the Moore-Penrose inverse when the factorization fails.
.onset_spd_inverse <- function(M) {
     tryCatch(chol2inv(chol(M)), error = function(e) .onset_pinv(M))
}


# One penalized Poisson fit at a fixed lambda: Newton steps with step halving
# on the penalized log-likelihood, stopping when the relative change falls
# below 1e-9 or no step halving improves it. Signals an "onset_fit_error"
# condition when the Newton system becomes non-finite or a decomposition fails.
.onset_qp_fit_one <- function(y, B, S, lambda, a0 = NULL, gamma = 1) {
     P <- lambda * S
     p <- ncol(B)
     a <- if (is.null(a0)) rep(log(max(mean(y), 1e-3)), p) else a0
     pll <- function(a) {
          e <- drop(B %*% a)
          sum(y * e - exp(e)) - 0.5 * sum(a * drop(P %*% a))
     }
     cur <- pll(a)
     ridge <- diag(p)
     for (iter in seq_len(200L)) {
          mu <- exp(drop(B %*% a))
          H <- crossprod(B, B * mu) + P
          g <- drop(crossprod(B, y - mu)) - drop(P %*% a)
          if (!all(is.finite(H)) || !all(is.finite(g))) stop(.onset_fit_error("non-finite Newton system"))
          step <- tryCatch(solve(H + 1e-8 * ridge, g),
                           error = function(e) drop(.onset_pinv(H + 1e-6 * ridge) %*% g))
          step_len <- 1
          while (step_len > 1e-8) {
               new <- pll(a + step_len * step)
               if (is.finite(new) && new >= cur - 1e-10) break
               step_len <- step_len / 2
          }
          if (step_len <= 1e-8) break
          a <- a + step_len * step
          old <- cur
          cur <- pll(a)
          if (abs(cur - old) < 1e-9 * (1 + abs(cur))) break
     }
     mu <- exp(drop(B %*% a))
     BWB <- crossprod(B, B * mu)
     Hinv <- .onset_spd_inverse(BWB + P)
     edf <- sum(Hinv * BWB)
     dev <- 2 * sum(ifelse(y > 0, y * log(y / mu), 0) - (y - mu))
     n <- length(y)
     # Past its pole (gamma x edf >= n) the GCV score falls back towards zero as
     # the fit interpolates; such a lambda is not a candidate.
     gcv <- if (n - gamma * edf > 0) n * dev / (n - gamma * edf)^2 else Inf
     list(a = a, Hinv = Hinv, edf = edf, lambda = lambda, gcv = gcv, phi = dev / max(n - edf, 1))
}


# GCV-best fit over the lambda grid, on the reported weeks `rows` of the basis.
# Each lambda starts from a0 (a cold start when that fails); lambdas whose fit
# fails or whose GCV is not finite are skipped. NULL when every lambda fails.
.onset_qp_fit <- function(y, rows, basis, lambda, gamma, a0 = NULL) {
     Bo <- basis$B[rows, , drop = FALSE]
     best <- NULL
     for (lam in lambda) {
          f <- tryCatch(.onset_qp_fit_one(y, Bo, basis$S, lam, a0, gamma), onset_fit_error = function(e) NULL)
          if (is.null(f) && !is.null(a0)) {
               f <- tryCatch(.onset_qp_fit_one(y, Bo, basis$S, lam, NULL, gamma), onset_fit_error = function(e) NULL)
          }
          if (is.null(f) || !is.finite(f$gcv)) next
          if (is.null(best) || f$gcv < best$gcv) best <- f
     }
     best
}


# Smoothed mean, growth rate and its standard error at the weeks `at` (all
# weeks by default) from a fit. A variance that rounding makes negative (an
# almost deterministic series, phi near zero) leaves the standard error NA,
# as numpy's NaN in the handoff; the rules treat such a week as not growing.
.onset_qp_summary <- function(fit, basis, at = seq_len(nrow(basis$B))) {
     V <- fit$phi * fit$Hinv
     Bs <- basis$B[at, , drop = FALSE]
     dBs <- basis$dB[at, , drop = FALSE]
     r <- drop(dBs %*% fit$a)
     v <- rowSums((dBs %*% V) * dBs)
     list(mu = exp(drop(Bs %*% fit$a)), r = r,
          se = sqrt(ifelse(v >= 0, v, NA_real_)))
}
