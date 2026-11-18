#' Detect cholera outbreak onsets in a weekly case series
#'
#' Finds every outbreak in a weekly series of suspected cases and returns its
#' onset, end and (in real time) detection week, by one of two rules: a
#' growth-rate rule on a quasi-Poisson smooth (\code{method = "growth"}) or the
#' Zheng et al. outbreak definition (\code{method = "zheng"}). Each rule runs
#' either on the full series (\code{mode = "retrospective"}) or one week at a
#' time on the data known by that week (\code{mode = "realtime"}), which records
#' the week an outbreak would have been declared.
#'
#' The series is analysed from its first to its last reported week; week
#' numbers below count from the first reported week.
#'
#' \strong{Growth rule} (the "tuned2" settings of the onset-detection handoff of
#' 18 November 2026). The series is smoothed by a quasi-Poisson P-spline: cubic
#' B-splines on segments of \code{segment_weeks} weeks from the first analysed
#' week, a second-order difference penalty, and the smoothing parameter chosen
#' by GCV (with \code{gcv_gamma}) over \code{lambda}; unreported weeks are left out
#' of the fit. This gives the smoothed mean mu and the weekly growth rate
#' r = d log(mu) / dt with its standard error. The level threshold is max(B,
#' floor), where the floor is \code{floor_per100k} cases per 100,000 per week and
#' B is the mean of mu over reported weeks outside outbreaks, iterated with the
#' outbreak set until it repeats (at most 25 rounds). An outbreak starts at a
#' reported week where mu is at or above the threshold and the next
#' \code{growth_weeks} weeks (that week included) are reported growth weeks:
#' r >= log(1 + growth_min) and r - growth_z x se > 0. A week is low when
#' unreported or when mu is below max(threshold, \code{end_frac_peak} x the
#' outbreak's running peak); \code{end_run_weeks} consecutive low weeks end the
#' outbreak at the second of them. A start within \code{merge_gap_weeks} of that
#' end continues the same outbreak, still judged against its earlier peak. An
#' outbreak whose smoothed peak stays below \code{min_size} x the threshold is
#' discarded.
#'
#' In real time the smooth is refitted at every week T from week
#' \code{realtime_start} on the reported weeks up to T (warm-started from the
#' previous week's fit). Because the segments have a fixed width, each fit uses
#' only the data known at T: the result up to T is the same whether or not the
#' series continues. Week T - 2 becomes a candidate at T (so the first decision
#' is at week \code{realtime_start} + 1) when weeks T - 2 to T are reported, its
#' smoothed level is at or above the threshold, and the growth condition holds at
#' T - 1 and T in fit T and at T - 2 and T - 1 in fit T - 1; this three-week
#' condition replaces \code{growth_weeks}. The candidate is declared at the first
#' week its newest smoothed estimate reaches \code{min_size} x the threshold (the
#' detection week), and dropped if it ends first. The real-time threshold is a
#' different statistic from the retrospective one: max(mean of the counts to
#' date outside declared outbreaks, unreported weeks counted as zero, floor). An
#' outbreak ends as in the full series, but its running peak restarts at an end,
#' so a merged continuation is judged against its own peak.
#'
#' \strong{Zheng rule} (Zheng et al. 2022, Int J Infect Dis 122:215-221; as
#' implemented in OutbreakExtractR, HopkinsIDD, main @ 4fcdc21). Eight zero weeks
#' pad the series before its first and after its last reported week, and
#' unreported weeks count as zero. The threshold is the mean weekly incidence,
#' sum(cases / population) / (weeks - 1). A week is high when its incidence is at
#' or above the threshold with cases > 0. An outbreak starts at a high week
#' followed by two strict increases in the counts (only when at least three weeks
#' are high), and ends the week after the first week that begins a run of at
#' least six low weeks; a start at or before that end belongs to the same
#' outbreak. In real time the threshold at week T uses the weeks up to T and the
#' eight leading padding weeks, week T - 2 is declared at T, and an outbreak ends
#' at the second of six consecutive weeks that are not high. The growth-rule
#' arguments do not apply.
#'
#' \strong{Caveats.}
#' \itemize{
#'   \item GCV usually selects the smallest lambda of the grid, so the knots (their
#'     width and position) set most of the smoothing, and shifting them moves growth
#'     onsets: 7.5-week segments reproduce 45 of the handoff's 96 onsets exactly and
#'     75 within two weeks.
#'   \item The dispersion estimate collapses in very sparse series (mostly zeros), which
#'     shrinks the growth standard error; there only the floor guards against small rises.
#'   \item The real-time 95% growth intervals at the newest week under-cover.
#'   \item A reporting gap of seven or more weeks always ends an outbreak.
#'   \item An onset at the first analysed week or just after an unreported week
#'     (\code{left_censored}) may have begun earlier.
#' }
#'
#' @param cases Numeric vector of weekly suspected cases, one per consecutive week;
#'   values in unreported weeks are ignored.
#' @param week_start Dates of the weeks' first days, consecutive 7 days apart.
#' @param population Population, one value or one per week (used for the per-capita
#'   floor and the Zheng incidence).
#' @param observed Logical vector, \code{TRUE} for reported weeks (default: non-missing \code{cases}).
#' @param method \code{"growth"} (default) or \code{"zheng"}.
#' @param mode \code{"retrospective"} (default, full series) or \code{"realtime"} (weekly step forward).
#' @param growth_min Minimum weekly growth of a growth week, as a proportion (0.05 = 5% a week).
#' @param growth_z Multiple of the growth standard error that r must exceed (1.28: lower 80% bound > 0); 0 drops the check.
#' @param growth_weeks Consecutive growth weeks that start an outbreak in the full series (real time uses its own three-week rule).
#' @param floor_per100k Floor on the threshold, in suspected cases per 100,000 per week.
#' @param min_size An outbreak's smoothed peak must reach this multiple of the threshold.
#' @param end_frac_peak A week below this fraction of the outbreak's running peak is low.
#' @param end_run_weeks Number of consecutive low weeks that end an outbreak (growth rule).
#' @param merge_gap_weeks A start within this many weeks of the previous end continues that outbreak.
#' @param realtime_start Week number, from the first reported week, of the first real-time estimate.
#' @param segment_weeks Width of a B-spline segment, in weeks.
#' @param gcv_gamma Inflation of the effective degrees of freedom in the GCV score.
#' @param lambda Grid of smoothing parameters searched by GCV.
#'
#' @return A data frame with one row per outbreak (zero rows when there is none):
#'   \describe{
#'     \item{outbreak_id}{1, 2, ... in onset order.}
#'     \item{onset_week}{First week of the outbreak.}
#'     \item{detection_week}{Week the outbreak was declared (real time); \code{NA} retrospectively.}
#'     \item{peak_week, peak_cases}{Week and count of the largest reported weekly count in the outbreak.}
#'     \item{peak_smoothed}{Largest smoothed mean over the outbreak's reported weeks (in real time, of the
#'       newest estimates); \code{NA} for the Zheng rule.}
#'     \item{end_week}{Last week of the outbreak; \code{NA} while it is still open at the end of the series.}
#'     \item{complete}{\code{FALSE} when the outbreak is still open at the last reported week (for the Zheng
#'       rule, also when it closes only inside the trailing padding).}
#'     \item{left_censored}{\code{TRUE} when the onset is the first analysed week or follows an unreported
#'       week, so the outbreak may have begun earlier.}
#'     \item{duration_weeks, total_cases}{Weeks and reported cases from onset to end (or to the last reported week).}
#'     \item{threshold_cases, threshold_per100k}{The level threshold at the onset week (real time: at the
#'       detection week), in cases per week and per 100,000 per week.}
#'     \item{method, mode}{As called.}
#'   }
#'   The attribute \code{"series"} is a data frame with one row per analysed week: \code{week_start},
#'   \code{cases}, \code{observed}, \code{mu} (smoothed mean; in real time the newest estimate at each
#'   week), \code{r}, \code{r_lo}, \code{r_hi} (growth rate and its 95% interval), \code{threshold}
#'   (cases per week) and \code{in_outbreak}. The smooth columns are \code{NA} for the Zheng rule.
#'
#' @seealso \code{\link{est_outbreak_onsets}} to run every location of a MOSAIC config.
#'
#' @examples
#' weeks <- seq(as.Date("2020-01-06"), by = 7, length.out = 160)
#' lambda_t <- 5 + 400 * exp(-((seq_along(weeks) - 100) / 6)^2)
#' set.seed(1)
#' x <- calc_outbreak_onset(rpois(160, lambda_t), weeks, population = 1e6)
#' x[, c("onset_week", "end_week", "peak_cases")]
#'
#' @export
calc_outbreak_onset <- function(cases, week_start, population,
                                observed = !is.na(cases),
                                method = c("growth", "zheng"),
                                mode = c("retrospective", "realtime"),
                                growth_min = 0.05, growth_z = 1.28, growth_weeks = 3L,
                                floor_per100k = 0.05, min_size = 1.5, end_frac_peak = 0.5,
                                end_run_weeks = 6L, merge_gap_weeks = 5L,
                                realtime_start = 53L, segment_weeks = 7.5, gcv_gamma = 1.4,
                                lambda = 10^seq(-2, 5, by = 0.5)) {

     method <- match.arg(method)
     mode <- match.arg(mode)
     if (is.logical(cases) && all(is.na(cases))) cases <- as.numeric(cases)
     n_in <- length(cases)
     if (!is.numeric(cases) || n_in < 1L) stop("cases must be a non-empty numeric vector.", call. = FALSE)
     week_start <- as.Date(week_start)
     if (length(week_start) != n_in || anyNA(week_start)) {
          stop("week_start must be a Date vector without NA, the same length as cases.", call. = FALSE)
     }
     if (n_in > 1L && any(as.numeric(diff(week_start)) != 7)) {
          stop("week_start must be consecutive weeks, 7 days apart.", call. = FALSE)
     }
     if (!is.numeric(population) || !length(population) %in% c(1L, n_in) ||
         any(!is.finite(population) | population <= 0)) {
          stop("population must be positive and finite, one value or one per week.", call. = FALSE)
     }
     population <- rep_len(as.numeric(population), n_in)
     if (!is.logical(observed) || length(observed) != n_in || anyNA(observed)) {
          stop("observed must be a logical vector without NA, the same length as cases.", call. = FALSE)
     }
     if (any(observed & (!is.finite(cases) | cases < 0))) {
          stop("cases must be finite and non-negative in every observed week.", call. = FALSE)
     }
     s <- list(
          growth_min = .onset_number(growth_min, "growth_min", 0),
          growth_z = .onset_number(growth_z, "growth_z", 0),
          growth_weeks = .onset_count(growth_weeks, "growth_weeks", 1L),
          floor_per100k = .onset_number(floor_per100k, "floor_per100k", 0),
          min_size = .onset_number(min_size, "min_size", 0),
          end_frac_peak = .onset_number(end_frac_peak, "end_frac_peak", 0, 1),
          end_run_weeks = .onset_count(end_run_weeks, "end_run_weeks", 2L),
          merge_gap_weeks = .onset_count(merge_gap_weeks, "merge_gap_weeks", 0L),
          realtime_start = .onset_count(realtime_start, "realtime_start", 3L),
          segment_weeks = .onset_number(segment_weeks, "segment_weeks", 0, strict = TRUE),
          gcv_gamma = .onset_number(gcv_gamma, "gcv_gamma", 0, strict = TRUE))
     if (!is.numeric(lambda) || !length(lambda) || any(!is.finite(lambda) | lambda <= 0)) {
          stop("lambda must be a non-empty vector of positive smoothing parameters.", call. = FALSE)
     }

     # Analyse from the first to the last reported week.
     idx <- if (any(observed)) min(which(observed)):max(which(observed)) else integer(0)
     n <- length(idx)
     obs <- observed[idx]
     yz <- ifelse(obs, cases[idx], 0)
     pop <- population[idx]
     weeks <- week_start[idx]
     na_n <- rep(NA_real_, n)
     series <- data.frame(week_start = weeks, cases = ifelse(obs, cases[idx], NA_real_), observed = obs,
                          mu = na_n, r = na_n, r_lo = na_n, r_hi = na_n, threshold = na_n,
                          in_outbreak = logical(n))
     floor_cases <- s$floor_per100k * pop / 1e5

     ob <- list(onset = integer(0), end = integer(0), closed = logical(0), detect = integer(0))
     thr_at <- numeric(n)
     if (n > 0L && sum(yz) > 0) {
          if (method == "growth" && mode == "retrospective") {
               basis <- .onset_basis(n, s$segment_weeks)
               fit <- .onset_qp_fit(yz[obs], which(obs), basis, lambda, s$gcv_gamma)
               if (is.null(fit)) stop("The smoother failed at every lambda.", call. = FALSE)
               sm <- .onset_qp_summary(fit, basis)
               res <- .onset_growth_retro(sm$mu, sm$se, sm$r, obs, floor_cases, s)
               ob <- .onset_unpack(res$outbreaks)
               series$mu <- sm$mu; series$r <- sm$r
               series$r_lo <- sm$r - 1.96 * sm$se; series$r_hi <- sm$r + 1.96 * sm$se
               series$threshold <- res$threshold
               series$in_outbreak <- .onset_mask(n, res$outbreaks)
               thr_at <- res$threshold
          } else if (method == "growth") {
               v <- .onset_growth_vintages(yz, obs, s, lambda)
               res <- .onset_growth_realtime(v, yz, obs, floor_cases, s, s$realtime_start)
               ob <- .onset_unpack_events(res$events)
               series$mu <- v$mu_0; series$r <- v$r_0
               series$r_lo <- v$r_0 - 1.96 * v$se_0; series$r_hi <- v$r_0 + 1.96 * v$se_0
               series$threshold <- res$threshold
               series$in_outbreak <- res$in_outbreak
               thr_at <- res$threshold
          } else if (mode == "retrospective") {
               pad <- 8L
               res <- .onset_zheng_retro(c(rep(0, pad), yz, rep(0, pad)),
                                         c(rep(pop[1L], pad), pop, rep(pop[n], pad)))
               raw <- .onset_unpack(res$outbreaks)
               ob <- list(onset = raw$onset - pad, end = raw$end - pad, detect = rep(NA_integer_, length(raw$onset)))
               ob$closed <- raw$closed & ob$end <= n
               ob$end[!ob$closed] <- NA_integer_
               thr_at <- res$threshold * pop
               series$threshold <- thr_at
               series$in_outbreak <- .onset_mask(n, Map(function(a, b) c(a, if (is.na(b)) n else b),
                                                        ob$onset, ob$end))
          } else {
               res <- .onset_zheng_realtime(yz, pop, s$realtime_start)
               ob <- .onset_unpack_events(res$events)
               series$threshold <- res$threshold
               series$in_outbreak <- res$in_outbreak
               thr_at <- res$threshold
          }
     }

     k <- length(ob$onset)
     last <- ifelse(is.na(ob$end), n, ob$end)
     at <- if (mode == "realtime") ob$detect else ob$onset
     win <- lapply(seq_len(k), function(i) ob$onset[i]:last[i])
     peak <- vapply(win, function(w) w[which.max(yz[w])], integer(1))
     peak_smoothed <- vapply(win, function(w) {
          m <- series$mu[w][obs[w]]
          if (length(m) && any(!is.na(m))) max(m, na.rm = TRUE) else NA_real_
     }, numeric(1))
     out <- data.frame(
          outbreak_id = seq_len(k),
          onset_week = weeks[ob$onset],
          detection_week = weeks[ob$detect],
          peak_week = weeks[peak],
          end_week = weeks[ob$end],
          complete = !is.na(ob$end),
          left_censored = ob$onset == 1L | !obs[pmax(ob$onset - 1L, 1L)],
          duration_weeks = as.integer(last - ob$onset + 1L),
          total_cases = vapply(win, function(w) sum(yz[w]), numeric(1)),
          peak_cases = as.numeric(yz[peak]),
          peak_smoothed = peak_smoothed,
          threshold_cases = thr_at[at],
          threshold_per100k = thr_at[at] / pop[at] * 1e5,
          method = rep(method, k),
          mode = rep(mode, k),
          stringsAsFactors = FALSE)
     attr(out, "series") <- series
     out
}


# Real-time smooth estimates: refit at every week T from realtime_start on the
# reported weeks up to T (warm-started from the previous fit), and keep mu at
# T - 2 and T and r, se at T - 1 and T. Weeks before any case have no fit; a
# week whose fit fails at every lambda is skipped with a warning.
.onset_growth_vintages <- function(yz, obs, s, lambda) {
     n <- length(yz)
     v <- data.frame(mu_m2 = rep(NA_real_, n), mu_0 = NA_real_, r_m1 = NA_real_, se_m1 = NA_real_,
                     r_0 = NA_real_, se_0 = NA_real_)
     if (n < s$realtime_start) return(v)
     basis <- .onset_basis(n, s$segment_weeks)
     cum <- cumsum(yz)
     a_prev <- NULL
     failed <- 0L
     for (Tt in s$realtime_start:n) {
          if (cum[Tt] == 0) next
          rows <- which(obs[seq_len(Tt)])
          fit <- .onset_qp_fit(yz[rows], rows, basis, lambda, s$gcv_gamma, a0 = a_prev)
          if (is.null(fit)) {
               failed <- failed + 1L
               next
          }
          a_prev <- fit$a
          sm <- .onset_qp_summary(fit, basis, at = (Tt - 2L):Tt)
          v[Tt, ] <- c(sm$mu[1L], sm$mu[3L], sm$r[2L], sm$se[2L], sm$r[3L], sm$se[3L])
     }
     if (failed) {
          warning(sprintf("calc_outbreak_onset: the smoother failed at every lambda in %d real-time week(s); those weeks were skipped.",
                          failed), call. = FALSE)
     }
     v
}


# (start, end, closed) outbreak vectors -> onset/end/closed/detect columns, the
# end of an open outbreak as NA.
.onset_unpack <- function(outbreaks) {
     onset <- vapply(outbreaks, function(o) as.integer(o[1L]), integer(1))
     end <- vapply(outbreaks, function(o) as.integer(o[2L]), integer(1))
     closed <- vapply(outbreaks, function(o) as.logical(o[3L]), logical(1))
     end[!closed] <- NA_integer_
     list(onset = onset, end = end, closed = closed, detect = rep(NA_integer_, length(onset)))
}


# Real-time events -> onset/end/closed/detect columns.
.onset_unpack_events <- function(events) {
     onset <- vapply(events, function(e) as.integer(e$onset), integer(1))
     end <- vapply(events, function(e) as.integer(e$end), integer(1))
     list(onset = onset, end = end, closed = !is.na(end),
          detect = vapply(events, function(e) as.integer(e$detect), integer(1)))
}


.onset_number <- function(x, name, min, max = Inf, strict = FALSE) {
     ok <- is.numeric(x) && length(x) == 1L && is.finite(x) && x <= max && (if (strict) x > min else x >= min)
     if (!ok) {
          stop(sprintf("%s must be a single number %s %s%s.", name, if (strict) ">" else ">=", min,
                       if (is.finite(max)) sprintf(" and <= %s", max) else ""), call. = FALSE)
     }
     as.numeric(x)
}


.onset_count <- function(x, name, min) {
     if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x != round(x) || x < min) {
          stop(sprintf("%s must be a whole number >= %d.", name, min), call. = FALSE)
     }
     as.integer(x)
}
