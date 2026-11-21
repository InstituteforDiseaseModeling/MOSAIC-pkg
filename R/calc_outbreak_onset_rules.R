# Outbreak rules for calc_outbreak_onset(): the growth-rate rule (retrospective
# and real time) and the Zheng et al. rule (retrospective and real time).
#
# Ports of the onset-detection handoff (18 November 2026):
#   growth, retrospective  tune_growth4.py::onset_mask_k, rule_rho and the
#                          threshold iteration of candidate2_eval.py ("tuned2")
#   growth, real time      rt_reml21.py::realtime() on the GCV smooth
#   Zheng, retrospective   zheng_faithful.py (OutbreakExtractR main @ 4fcdc21)
#   Zheng, real time       plot_focal_compare.py::zheng_rt
# Week indices here are 1-based from the first analysed (first reported) week;
# every outbreak is (start, end, closed) with `end` the last week of a closed
# outbreak or the last analysed week of an open one.


# Mark the weeks covered by a list of (start, end, closed) outbreaks.
.onset_mask <- function(n, outbreaks) {
     m <- logical(n)
     for (ob in outbreaks) m[ob[1L]:ob[2L]] <- TRUE
     m
}


# Weeks t where the growth condition holds at t and the next k - 1 weeks: a
# growth week is reported, r >= log(1 + growth_min) and, when growth_z > 0,
# r - growth_z * se > 0 (a week with an undefined standard error is not one).
.onset_growth_ok <- function(r, se, observed, growth_min, growth_z, growth_weeks) {
     gw <- observed & (r >= log1p(growth_min))
     if (growth_z > 0) gw <- gw & (r - growth_z * se > 0)
     gw[is.na(gw)] <- FALSE
     ok <- gw
     n <- length(gw)
     if (growth_weeks > 1L) for (j in seq_len(growth_weeks - 1L)) {
          ok <- ok & c(gw[-seq_len(min(j, n))], logical(min(j, n)))
     }
     ok
}


# Scan for outbreaks on a smoothed series (rule_rho). An outbreak starts at a
# reported week where the growth condition holds and mu >= thr. A week is low
# when unreported or mu < max(thr, end_frac_peak x the outbreak's running
# peak); end_run_weeks consecutive low weeks end it at the second of them. A
# start within merge_gap_weeks of that end continues the same outbreak (the
# running peak carries over). A finished outbreak whose peak is below
# min_size x thr at its start is discarded and the scan resumes at the next week.
.onset_growth_scan <- function(mu, observed, onset_ok, thr, end_frac_peak,
                               merge_gap_weeks, min_size, end_run_weeks) {
     n <- length(mu)
     start <- onset_ok & observed & (mu >= thr)
     out <- list()
     t <- 1L
     while (t <= n) {
          if (!start[t]) {
               t <- t + 1L
               next
          }
          s <- t; peak <- mu[t]; run <- 0L; u <- t; e <- NA_integer_
          while (u <= n) {
               if (observed[u]) peak <- max(peak, mu[u])
               low <- !observed[u] || mu[u] < max(thr[u], end_frac_peak * peak)
               run <- if (low) run + 1L else 0L
               if (run == end_run_weeks) {
                    e <- u - end_run_weeks + 2L
                    nxt <- if (e < n) which(start[(e + 1L):n]) + e else integer(0)
                    if (merge_gap_weeks > 0L && length(nxt) && nxt[1L] - e <= merge_gap_weeks) {
                         u <- nxt[1L]; run <- 0L; e <- NA_integer_
                         next
                    }
                    break
               }
               u <- u + 1L
          }
          closed <- !is.na(e)
          ee <- if (closed) e else n
          if (peak >= min_size * thr[s]) {
               out[[length(out) + 1L]] <- c(s, ee, closed)
               t <- if (closed) ee + 1L else n + 1L
          } else {
               t <- s + 1L
          }
     }
     out
}


# Retrospective growth rule: the threshold max(B, floor) with B the mean of mu
# over reported weeks outside outbreaks, iterated with the outbreak set until
# it repeats (at most 25 rounds, starting from the mean over all reported
# weeks). The threshold returned is the last B, the one that produced the set.
.onset_growth_retro <- function(mu, se, r, observed, floor_cases, s) {
     ok <- .onset_growth_ok(r, se, observed, s$growth_min, s$growth_z, s$growth_weeks)
     scan <- function(thr) .onset_growth_scan(mu, observed, ok, thr, s$end_frac_peak,
                                              s$merge_gap_weeks, s$min_size, s$end_run_weeks)
     B <- mean(mu[observed])
     obk <- scan(pmax(B, floor_cases))
     for (i in seq_len(25L)) {
          sel <- observed & !.onset_mask(length(mu), obk)
          Bn <- if (any(sel)) mean(mu[sel]) else B
          ob2 <- scan(pmax(Bn, floor_cases))
          B <- Bn
          if (identical(ob2, obk)) break
          obk <- ob2
     }
     list(outbreaks = obk, threshold = pmax(B, floor_cases))
}


# Real-time growth rule over vintages. `v` holds, per week T, the estimates of
# the fit to weeks <= T: mu at T-2 and T, r and se at T-1 and T (NA where no
# fit was made). Week T-2 becomes a candidate at T when weeks T-2..T are
# reported, mu(T-2) >= threshold, and the growth condition holds at T-1 and T
# in fit T and at T-2 and T-1 in fit T-1. A candidate is declared (detection
# week) at the first reported week whose newest estimate reaches min_size x
# threshold (never on an estimate projected over an unreported week), and
# dropped if it ends first. The threshold is max(mean of the zero-filled counts to date
# outside declared outbreaks, floor). The growth condition is always this
# three-week, two-vintage one (growth_weeks applies to the full series only).
# End as in the full series; unlike it, the running peak restarts at an end,
# so a merged continuation is not judged against the earlier peak.
.onset_growth_realtime <- function(v, yz, observed, floor_cases, s, first) {
     n <- length(yz)
     g <- log1p(s$growth_min)
     grow <- function(r, se) {
          isTRUE(r >= g) && (s$growth_z <= 0 || isTRUE(r - s$growth_z * se > 0))
     }
     in_out <- logical(n)
     thr_path <- rep(NA_real_, n)
     state <- "idle"; ev <- list(); low_run <- 0L; peak <- 0; cand <- NA_integer_
     for (Tt in seq.int(first + 1L, length.out = max(0L, n - first))) {
          base <- yz[seq_len(Tt)][!in_out[seq_len(Tt)]]
          thr <- max(if (length(base)) mean(base) else 0, floor_cases[Tt])
          thr_path[Tt] <- thr
          if (is.na(v$mu_0[Tt])) next
          if (state == "idle" && !is.na(v$mu_0[Tt - 1L])) {
               if (observed[Tt - 2L] && observed[Tt - 1L] && observed[Tt] && v$mu_m2[Tt] >= thr &&
                   grow(v$r_m1[Tt], v$se_m1[Tt]) && grow(v$r_0[Tt], v$se_0[Tt]) &&
                   grow(v$r_m1[Tt - 1L], v$se_m1[Tt - 1L]) && grow(v$r_0[Tt - 1L], v$se_0[Tt - 1L])) {
                    state <- "cand"; cand <- Tt - 2L; low_run <- 0L
                    peak <- max(v$mu_m2[Tt], v$mu_0[Tt])
               }
          }
          if (state != "idle") {
               if (observed[Tt]) peak <- max(peak, v$mu_0[Tt])
               if (state == "cand" && observed[Tt] && v$mu_0[Tt] >= s$min_size * thr) {
                    k <- length(ev)
                    if (k && !is.na(ev[[k]]$end) && cand - ev[[k]]$end <= s$merge_gap_weeks) {
                         ev[[k]]$end <- NA_integer_
                    } else {
                         ev[[k + 1L]] <- list(onset = cand, detect = Tt, end = NA_integer_)
                    }
                    in_out[ev[[length(ev)]]$onset:Tt] <- TRUE
                    state <- "in"
               }
               if (state == "in") in_out[Tt] <- TRUE
               low <- !observed[Tt] || v$mu_0[Tt] < max(thr, s$end_frac_peak * peak)
               low_run <- if (low) low_run + 1L else 0L
               if (low_run == s$end_run_weeks) {
                    if (state == "in") {
                         e <- Tt - s$end_run_weeks + 2L
                         ev[[length(ev)]]$end <- e
                         if (e < Tt) in_out[(e + 1L):Tt] <- FALSE
                    }
                    state <- "idle"; low_run <- 0L; peak <- 0
               }
          }
     }
     list(events = ev, threshold = thr_path, in_outbreak = in_out)
}


# Retrospective Zheng et al. rule on a padded series (8 phantom zero weeks
# before the first and after the last report, unreported weeks zero):
# threshold = sum(cases / pop) / (n - 1); a week is high when its rate is at
# or above it with cases > 0; a start is a high week followed by two strict
# count increases (only when at least three weeks are high); the tail weeks
# begin a run of at least six low weeks; an outbreak runs from its start to
# the week after the first tail week after it, and a start before that end
# merges into it.
.onset_zheng_retro <- function(cases, pop) {
     n <- length(cases)
     rate <- cases / pop
     thr <- sum(rate) / ((7L * n - 2L) %/% 7L)
     high <- rate >= thr & cases > 0
     d <- c(diff(cases), 0)
     start <- logical(n)
     if (sum(high) >= 3L && n >= 3L) {
          i <- seq_len(n - 2L)
          start[i] <- d[i] > 0 & d[i + 1L] > 0 & high[i]
     }
     runs <- rle(!high)
     ends <- cumsum(runs$lengths)
     begins <- ends - runs$lengths + 1L
     tail <- logical(n)
     for (k in which(runs$values & runs$lengths >= 6L)) tail[begins[k]:(ends[k] - 5L)] <- TRUE
     out <- list()
     starts <- which(start)
     k <- 1L
     while (k <= length(starts)) {
          s <- starts[k]
          tl <- if (s < n) which(tail[(s + 1L):n]) else integer(0)
          closed <- length(tl) > 0L
          e <- if (closed) s + tl[1L] + 1L else n
          out[[length(out) + 1L]] <- c(s, min(e, n), closed)
          k <- k + sum(starts[k:length(starts)] < e + 1L)
     }
     list(outbreaks = out, threshold = thr)
}


# Real-time Zheng rule: at week T the threshold is sum(rate) over weeks <= T
# divided by (T + 8 - 1), counting the 8 leading phantom weeks; week T-2 is an
# onset at T when it is high (rate >= threshold, cases > 0) and the counts
# rose at T-1 and T; an outbreak ends at the second of six consecutive weeks
# that are not high.
.onset_zheng_realtime <- function(yz, pop, first) {
     n <- length(yz)
     rate <- yz / pop
     state <- "idle"; ev <- list(); low_run <- 0L
     thr_path <- rep(NA_real_, n)
     for (Tt in seq.int(first, length.out = max(0L, n - first + 1L))) {
          thr <- sum(rate[seq_len(Tt)]) / (Tt + 8L - 1L)
          thr_path[Tt] <- thr * pop[Tt]
          hi <- function(t) rate[t] >= thr && yz[t] > 0
          if (state == "idle") {
               t <- Tt - 2L
               if (thr > 0 && hi(t) && yz[t + 1L] > yz[t] && yz[t + 2L] > yz[t + 1L]) {
                    ev[[length(ev) + 1L]] <- list(onset = t, detect = Tt, end = NA_integer_)
                    state <- "in"; low_run <- 0L
               }
          } else {
               low_run <- if (!hi(Tt)) low_run + 1L else 0L
               if (low_run == 6L) {
                    ev[[length(ev)]]$end <- Tt - 6L + 2L
                    state <- "idle"; low_run <- 0L
               }
          }
     }
     in_out <- logical(n)
     for (e in ev) in_out[e$onset:(if (is.na(e$end)) n else e$end)] <- TRUE
     list(events = ev, threshold = thr_path, in_outbreak = in_out)
}
