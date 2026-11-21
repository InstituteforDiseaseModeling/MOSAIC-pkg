# Tests for calc_outbreak_onset() and its smoother and rules.
#
# Parity with the onset-detection handoff (Python, 18 November 2026) is pinned by
# fixtures/outbreak_onset/ (see its README): weekly series of five countries, the handoff's own
# outbreak tables for them, and its smoother's fit for one country.

.wave_series <- function(n = 160L, peak_at = 100L, height = 400, width = 6, base = 5) {
     wk <- seq_len(n)
     list(week_start = seq(as.Date("2020-01-06"), by = 7, length.out = n),
          cases = round(base + height * exp(-((wk - peak_at) / width)^2)))
}

.onset_fixture <- function() {
     dir <- testthat::test_path("fixtures", "outbreak_onset")
     wk <- utils::read.csv(file.path(dir, "weekly.csv"), stringsAsFactors = FALSE)
     wk$week_start <- as.Date(wk$week_start)
     ex <- utils::read.csv(file.path(dir, "expected.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
     for (cl in c("onset_week", "detection_week", "end_week")) ex[[cl]] <- as.Date(ex[[cl]])
     list(weekly = wk, expected = ex, dir = dir)
}

# The handoff's knots: 60 segments over the analysed weeks (first to last reported).
.handoff_width <- function(cases) {
     o <- which(!is.na(cases))
     (max(o) - min(o) + 1) / 60
}

.check_parity <- function(fx, method, mode, isos = unique(fx$weekly$iso)) {
     for (iso in isos) {
          d <- fx$weekly[fx$weekly$iso == iso, ]
          got <- calc_outbreak_onset(d$cases, d$week_start, d$population, method = method, mode = mode,
                                     segment_weeks = .handoff_width(d$cases))
          ex <- fx$expected[fx$expected$iso == iso & fx$expected$method == method & fx$expected$mode == mode, ]
          info <- paste(iso, method, mode)
          expect_equal(got$onset_week, ex$onset_week, info = info)
          expect_equal(got$end_week, ex$end_week, info = info)
          expect_equal(got$detection_week, ex$detection_week, info = info)
     }
}

test_that("input checks reject malformed series and settings", {
     x <- .wave_series(20L)
     expect_error(calc_outbreak_onset(x$cases, replace(x$week_start, 1, x$week_start[1] - 1), 1e6), "7 days apart")
     expect_error(calc_outbreak_onset(x$cases, x$week_start[-1], 1e6), "same length")
     expect_error(calc_outbreak_onset(x$cases, x$week_start, 0), "population")
     expect_error(calc_outbreak_onset(x$cases, x$week_start, rep(1e6, 3)), "population")
     expect_error(calc_outbreak_onset(replace(x$cases, 3, NA), x$week_start, 1e6, observed = rep(TRUE, 20)),
                  "finite and non-negative")
     expect_error(calc_outbreak_onset(replace(x$cases, 3, -1), x$week_start, 1e6), "non-negative")
     expect_error(calc_outbreak_onset(as.character(x$cases), x$week_start, 1e6), "numeric")
     expect_error(calc_outbreak_onset(x$cases, x$week_start, 1e6, growth_weeks = 1.5), "growth_weeks")
     expect_error(calc_outbreak_onset(x$cases, x$week_start, 1e6, end_frac_peak = 2), "end_frac_peak")
     expect_error(calc_outbreak_onset(x$cases, x$week_start, 1e6, realtime_start = 2), "realtime_start")
     expect_error(calc_outbreak_onset(x$cases, x$week_start, 1e6, segment_weeks = 0), "segment_weeks")
     expect_error(calc_outbreak_onset(x$cases, x$week_start, 1e6, lambda = c(1, -1)), "lambda")
     expect_error(calc_outbreak_onset(x$cases, x$week_start, 1e6, method = "rt"), "should be one of")
})

test_that("a series with no cases or no reported week gives an empty table of the documented columns", {
     x <- .wave_series(80L)
     cols <- c("outbreak_id", "onset_week", "detection_week", "peak_week", "end_week", "complete",
               "left_censored", "duration_weeks", "total_cases", "peak_cases", "peak_smoothed",
               "threshold_cases", "threshold_per100k", "method", "mode", "source")
     for (m in c("growth", "zheng")) for (md in c("retrospective", "realtime")) for (src in c("smooth", "raw")) {
          z <- calc_outbreak_onset(rep(0, 80), x$week_start, 1e6, method = m, mode = md, source = src)
          expect_identical(names(z), cols)
          expect_equal(nrow(z), 0L)
          expect_s3_class(z$onset_week, "Date")
          expect_equal(nrow(attr(z, "series")), 80L)
          u <- calc_outbreak_onset(rep(NA, 80), x$week_start, 1e6, method = m, mode = md, source = src)  # all-NA
          expect_equal(nrow(u), 0L)
          expect_equal(nrow(attr(u, "series")), 0L)
     }
})

test_that("one clean wave gives one outbreak whose onset precedes its peak, by both rules and modes", {
     x <- .wave_series()
     g <- calc_outbreak_onset(x$cases, x$week_start, 1e6)
     expect_equal(nrow(g), 1L)
     expect_true(g$onset_week < x$week_start[100] && g$onset_week >= x$week_start[85])
     expect_equal(g$peak_week, x$week_start[100])
     expect_true(g$complete && g$end_week > x$week_start[100])
     expect_false(g$left_censored)
     expect_true(is.na(g$detection_week))
     i0 <- match(g$onset_week, x$week_start); i1 <- match(g$end_week, x$week_start)
     expect_equal(g$total_cases, sum(x$cases[i0:i1]))
     expect_equal(g$peak_cases, max(x$cases[i0:i1]))
     expect_type(g$peak_cases, "double")
     s <- attr(g, "series")
     expect_equal(g$peak_smoothed, max(s$mu[i0:i1]))
     expect_true(all(s$in_outbreak[i0:i1]))
     expect_equal(sum(s$in_outbreak), g$duration_weeks)
     expect_true(all(s$r_lo <= s$r & s$r <= s$r_hi))

     z <- calc_outbreak_onset(x$cases, x$week_start, 1e6, method = "zheng")
     expect_equal(nrow(z), 1L)
     expect_true(z$onset_week < x$week_start[100])
     expect_true(is.na(z$peak_smoothed))

     for (m in c("growth", "zheng")) {
          # a near-deterministic series: rounding can make the growth variance negative (handled, no warning)
          expect_no_warning(rt <- calc_outbreak_onset(x$cases, x$week_start, 1e6, method = m, mode = "realtime"))
          expect_equal(nrow(rt), 1L)
          expect_true(rt$detection_week >= rt$onset_week && rt$detection_week <= x$week_start[100])
     }
})

test_that("leading and trailing unreported weeks do not change the result", {
     x <- .wave_series(130L, peak_at = 90L)
     set.seed(5)
     y <- rpois(130, x$cases)
     pre <- seq(min(x$week_start) - 7 * 50, by = 7, length.out = 50)
     post <- max(x$week_start) + 7 * 1:10
     for (m in c("growth", "zheng")) for (md in c("retrospective", "realtime")) for (src in c("smooth", "raw")) {
          padded <- calc_outbreak_onset(c(rep(NA, 50), y, rep(NA, 10)), c(pre, x$week_start, post), 1e6,
                                        method = m, mode = md, source = src)
          plain <- calc_outbreak_onset(y, x$week_start, 1e6, method = m, mode = md, source = src)
          expect_equal(padded, plain, ignore_attr = TRUE, info = paste(m, md, src))
          expect_equal(nrow(attr(padded, "series")), 130L)
     }
})

test_that("the growth rule raises no outbreak in stationary noise, in either mode", {
     weeks <- seq(as.Date("2018-01-01"), by = 7, length.out = 200)
     set.seed(11)
     y <- rpois(200, 20)
     expect_equal(nrow(calc_outbreak_onset(y, weeks, 1e6)), 0L)
     expect_equal(nrow(calc_outbreak_onset(y, weeks, 1e6, mode = "realtime")), 0L)
})

test_that("real-time growth uses only the data known at each week", {
     x <- .wave_series(130L, peak_at = 75L, height = 300)
     set.seed(3)
     y <- rpois(130, x$cases + 80 * exp(-((seq_len(130) - 115) / 5)^2))
     full <- calc_outbreak_onset(y, x$week_start, 1e6, mode = "realtime")
     expect_gte(nrow(full), 1L)
     for (cut_at in c(90L, 118L)) {
          cut <- calc_outbreak_onset(y[1:cut_at], x$week_start[1:cut_at], 1e6, mode = "realtime")
          known <- full[full$detection_week <= x$week_start[cut_at], ]
          expect_equal(cut$onset_week, known$onset_week, info = cut_at)
          expect_equal(cut$detection_week, known$detection_week, info = cut_at)
          ended <- !is.na(known$end_week) & known$end_week + 7 * 4 <= x$week_start[cut_at]
          expect_equal(cut$end_week[ended], known$end_week[ended], info = cut_at)
          expect_equal(attr(cut, "series")$mu, attr(full, "series")$mu[1:cut_at], tolerance = 1e-6, info = cut_at)
     }
})

test_that("real-time Zheng uses only the data known at each week", {
     x <- .wave_series(150L, peak_at = 90L)
     set.seed(3)
     y <- rpois(150, x$cases)
     full <- calc_outbreak_onset(y, x$week_start, 1e6, method = "zheng", mode = "realtime")
     for (cut_at in c(95L, 120L)) {
          cut <- calc_outbreak_onset(y[1:cut_at], x$week_start[1:cut_at], 1e6, method = "zheng", mode = "realtime")
          known <- full[full$detection_week <= x$week_start[cut_at], ]
          expect_equal(cut$onset_week, known$onset_week)
          expect_equal(cut$detection_week, known$detection_week)
          ended <- !is.na(known$end_week) & known$end_week + 7 * 4 <= x$week_start[cut_at]
          expect_equal(cut$end_week[ended], known$end_week[ended])
     }
})

test_that("the per-capita floor suppresses a small rise in a large population", {
     x <- .wave_series(120L, peak_at = 80L, height = 20, base = 0)
     expect_equal(nrow(calc_outbreak_onset(x$cases, x$week_start, 1e9)), 0L)   # floor 500 cases/week
     small <- calc_outbreak_onset(x$cases, x$week_start, 1e5)
     expect_equal(nrow(small), 1L)
     expect_gte(small$threshold_per100k, 0.05)
})

test_that("growth_sensitive is the growth rule at floor 0.02 and growth_z 0.84, and explicit settings override it", {
     x <- .wave_series(150L, peak_at = 100L, height = 60, base = 2)
     set.seed(7)
     y <- rpois(150, x$cases)
     drop_method <- function(d) d[setdiff(names(d), "method")]
     for (md in c("retrospective", "realtime")) {
          s <- calc_outbreak_onset(y, x$week_start, 1e7, method = "growth_sensitive", mode = md)
          g <- calc_outbreak_onset(y, x$week_start, 1e7, mode = md, floor_per100k = 0.02, growth_z = 0.84)
          expect_gte(nrow(s), 1L)
          expect_equal(drop_method(s), drop_method(g), info = md)
          expect_equal(attr(s, "series"), attr(g, "series"), info = md)
          expect_true(all(s$method == "growth_sensitive"))
          o <- calc_outbreak_onset(y, x$week_start, 1e7, method = "growth_sensitive", mode = md,
                                   floor_per100k = 0.05, growth_z = 1.28)
          d <- calc_outbreak_onset(y, x$week_start, 1e7, mode = md)
          expect_equal(drop_method(o), drop_method(d), info = md)
     }
})

test_that("growth_sensitive finds a small outbreak in a large population that the growth rule misses", {
     weeks <- seq(as.Date("2020-01-06"), by = 7, length.out = 160)
     set.seed(2)
     y <- rpois(160, 3 + 30 * exp(-((seq_len(160) - 110) / 4)^2))   # peak ~33 a week; 0.05 floor = 25 a week
     for (md in c("retrospective", "realtime")) {
          expect_equal(nrow(calc_outbreak_onset(y, weeks, 5e7, mode = md)), 0L, info = md)
          s <- calc_outbreak_onset(y, weeks, 5e7, method = "growth_sensitive", mode = md)
          expect_equal(nrow(s), 1L, info = md)
          expect_true(s$onset_week >= weeks[100] && s$onset_week < weeks[110], info = md)
     }
})

test_that("the raw source's level, growth rate and standard error are those documented", {
     yz <- c(1, 2, 4, 8, 0, 16, 32)
     obs <- c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
     sg <- MOSAIC:::.onset_raw_signal(yz, obs, 2L)
     expect_equal(sg$mu, c(1, 1.5, 3, 6, 0, 16, 24))                    # mean of the reported weeks of each 2
     expect_equal(sg$r[4], log((4 + 8 + 0.5) / (1 + 2 + 0.5)) / 2)
     expect_equal(sg$se[4], sqrt(1 / 12.5 + 1 / 3.5) / 2)
     expect_true(all(is.na(sg$r[-4])))                                 # 4 reported weeks only at week 4
     s1 <- MOSAIC:::.onset_raw_signal(yz, obs, 1L)
     expect_equal(s1$mu, ifelse(obs, yz, 0))
     expect_equal(s1$r[3], log(4.5 / 2.5))
     expect_true(is.na(s1$r[5]) && is.na(s1$r[6]) && !is.na(s1$r[7]))
     # real time: the vintage table is the one raw series, lagged
     v <- MOSAIC:::.onset_raw_vintages(sg, 3L)
     expect_true(all(is.na(v[1:2, ])))
     expect_equal(v$mu_m2[7], sg$mu[5]); expect_equal(v$mu_0[7], sg$mu[7])
     expect_equal(v$r_m1[5], sg$r[4]); expect_equal(v$se_m1[5], sg$se[4])
})

test_that("the growth rules on the raw source find a clean wave in both modes and record the source", {
     x <- .wave_series()
     for (m in c("growth", "growth_sensitive")) for (md in c("retrospective", "realtime")) {
          g <- calc_outbreak_onset(x$cases, x$week_start, 1e6, method = m, mode = md, source = "raw")
          info <- paste(m, md)
          expect_equal(nrow(g), 1L, info = info)
          expect_true(g$onset_week < x$week_start[100] && g$onset_week >= x$week_start[80], info = info)
          expect_equal(g$source, "raw", info = info)
          s <- attr(g, "series")
          i0 <- match(g$onset_week, x$week_start)
          i1 <- if (is.na(g$end_week)) 160L else match(g$end_week, x$week_start)
          expect_equal(g$peak_smoothed, max(s$mu[i0:i1]), info = info)
          if (md == "realtime") expect_true(g$detection_week >= g$onset_week + 14, info = info)   # 3 growth weeks
     }
     expect_equal(calc_outbreak_onset(x$cases, x$week_start, 1e6)$source, "smooth")
     expect_equal(calc_outbreak_onset(x$cases, x$week_start, 1e6, method = "zheng", source = "smooth")$source, "raw")
     expect_error(calc_outbreak_onset(x$cases, x$week_start, 1e6, source = "counts"), "should be one of")
     expect_error(calc_outbreak_onset(x$cases, x$week_start, 1e6, source = "raw", raw_weeks = 0), "raw_weeks")
})

test_that("the raw source leaves unreported weeks empty and ends an outbreak across a long gap", {
     x <- .wave_series(150L, peak_at = 60L, height = 300)
     y <- x$cases
     y[70:80] <- NA                                                   # an 11-week gap after the peak
     for (md in c("retrospective", "realtime")) {
          g <- calc_outbreak_onset(y, x$week_start, 1e6, mode = md, source = "raw")
          s <- attr(g, "series")
          expect_true(all(is.na(s$mu[70:80])) && all(is.na(s$r[70:80])), info = md)
          expect_true(all(!is.na(s$mu[!is.na(y)])), info = md)
          expect_true(all(g$end_week <= x$week_start[72]), info = md)
     }
})

test_that("real-time growth on the raw source uses only the data known at each week", {
     x <- .wave_series(130L, peak_at = 75L, height = 300)
     set.seed(3)
     y <- rpois(130, x$cases + 80 * exp(-((seq_len(130) - 115) / 5)^2))
     full <- calc_outbreak_onset(y, x$week_start, 1e6, mode = "realtime", source = "raw")
     expect_gte(nrow(full), 1L)
     for (cut_at in c(90L, 118L)) {
          cut <- calc_outbreak_onset(y[1:cut_at], x$week_start[1:cut_at], 1e6, mode = "realtime", source = "raw")
          known <- full[full$detection_week <= x$week_start[cut_at], ]
          expect_equal(cut$onset_week, known$onset_week, info = cut_at)
          expect_equal(cut$detection_week, known$detection_week, info = cut_at)
          expect_equal(attr(cut, "series")$mu, attr(full, "series")$mu[1:cut_at], info = cut_at)
     }
})

test_that("growth_sensitive on the raw source is the raw growth rule at floor 0.02 and growth_z 0.84", {
     x <- .wave_series(150L, peak_at = 100L, height = 60, base = 2)
     set.seed(7)
     y <- rpois(150, x$cases)
     drop_method <- function(d) d[setdiff(names(d), "method")]
     for (md in c("retrospective", "realtime")) {
          s <- calc_outbreak_onset(y, x$week_start, 1e7, method = "growth_sensitive", mode = md, source = "raw")
          g <- calc_outbreak_onset(y, x$week_start, 1e7, mode = md, source = "raw", floor_per100k = 0.02,
                                   growth_z = 0.84)
          expect_equal(drop_method(s), drop_method(g), info = md)
     }
})

test_that("Zheng retrospective matches a hand-computed outbreak", {
     cases <- c(1, 4, 9, 16, 9, 4, 1, 0, 0, 0, 0, 0, 0, 0, 2)
     weeks <- seq(as.Date("2021-01-04"), by = 7, length.out = length(cases))
     z <- calc_outbreak_onset(cases, weeks, 1e5, method = "zheng")
     # padded to 31 weeks: threshold = 46 / 30 cases a week; the week with 4 starts (4 < 9 < 16);
     # the low run from week 7 to 14 begins the tail at week 7, so the outbreak ends at week 8
     expect_equal(nrow(z), 1L)
     expect_equal(z$onset_week, weeks[2])
     expect_equal(z$end_week, weeks[8])
     expect_equal(z$peak_week, weeks[4])
     expect_equal(z$total_cases, 43)
     expect_equal(z$threshold_cases, 46 / 30)
     expect_true(z$complete)
     expect_false(z$left_censored)
})

test_that("an outbreak closing only inside the Zheng padding is open", {
     cases <- c(0, 0, 3, 6, 10, 15, 22, 30)
     weeks <- seq(as.Date("2021-01-04"), by = 7, length.out = length(cases))
     z <- calc_outbreak_onset(cases, weeks, 1e5, method = "zheng")
     expect_equal(nrow(z), 1L)
     expect_equal(z$onset_week, weeks[4])
     expect_false(z$complete)
     expect_true(is.na(z$end_week))
     expect_equal(z$duration_weeks, 5L)
})

test_that("an onset at the first week or after an unreported week is left-censored", {
     weeks <- seq(as.Date("2021-01-04"), by = 7, length.out = 20)
     z <- calc_outbreak_onset(c(4, 9, 16, 20, rep(0, 16)), weeks, 1e5, method = "zheng")
     expect_true(z$left_censored[1])
     z2 <- calc_outbreak_onset(c(0, 0, NA, 4, 9, 16, 20, rep(0, 13)), weeks, 1e5, method = "zheng")
     expect_equal(z2$onset_week, weeks[4])
     expect_true(z2$left_censored)
})

test_that("the growth scan ends, merges and discards outbreaks as documented", {
     scan <- function(mu, ok, gap = 5L, size = 1.5) {
          MOSAIC:::.onset_growth_scan(mu, rep(TRUE, length(mu)), ok, rep(1, length(mu)), 0.5, gap, size, 6L)
     }
     mu <- c(rep(0.5, 4), rep(10, 6), rep(0.5, 7), rep(8, 7), rep(0.5, 16))
     ok <- seq_along(mu) %in% c(5, 18)
     # six low weeks from 11 end the first outbreak at week 12; the next start (18) is 6 weeks on
     expect_identical(scan(mu, ok), list(c(5L, 12L, 1L), c(18L, 26L, 1L)))
     # within the merge gap the second rise continues the first outbreak
     expect_identical(scan(mu, ok, gap = 6L), list(c(5L, 26L, 1L)))
     # the running peak carries over a merge: a continuation below half the first peak is low
     mu2 <- replace(mu, 18:24, 4)
     expect_identical(scan(mu2, ok, gap = 6L), list(c(5L, 19L, 1L)))
     # a rise whose peak stays below min_size x threshold is discarded
     mu3 <- replace(mu, 5:10, 1.2)
     expect_identical(scan(mu3, ok), list(c(18L, 26L, 1L)))
     # an outbreak still high at the last week stays open
     expect_identical(scan(c(rep(0.5, 4), rep(10, 6)), seq_len(10) == 5), list(c(5L, 10L, 0L)))
})

test_that("the real-time growth rule declares, ends, merges and drops candidates as documented", {
     s <- list(growth_min = 0.05, growth_z = 1.28, min_size = 1.5, end_frac_peak = 0.5, end_run_weeks = 6L,
               merge_gap_weeks = 5L)
     n <- 40L
     yz <- rep(1, n); obs <- rep(TRUE, n); fl <- rep(0, n)
     v <- data.frame(mu_m2 = rep(1, n), mu_0 = 1, r_m1 = 0, se_m1 = 0.01, r_0 = 0, se_0 = 0.01)
     v[10:11, c("r_m1", "r_0")] <- 0.2        # growth in fits 10 and 11: week 9 is a candidate at week 11
     v$mu_m2[11] <- 2
     v$mu_0[11] <- 1.2                        # below 1.5 x threshold: not yet declared
     v$mu_0[12:15] <- 3                       # declared at week 12
     v$mu_0[16:n] <- 0.5                      # six low weeks from 16 end it at 17 (declared at 21)
     rt <- function(v) MOSAIC:::.onset_growth_realtime(v, yz, obs, fl, s, 3L)
     r1 <- rt(v)
     expect_equal(r1$events, list(list(onset = 9L, detect = 12L, end = 17L)))
     expect_identical(which(r1$in_outbreak), 9:17)
     expect_equal(r1$threshold[4:n], rep(1, n - 3))
     # a candidate that ends before reaching the size check is dropped
     v_drop <- v; v_drop$mu_0[12:15] <- 1.2
     expect_length(rt(v_drop)$events, 0L)
     # a new candidate within merge_gap_weeks of the end reopens the outbreak
     v_merge <- v
     v_merge[22:23, c("r_m1", "r_0")] <- 0.2   # candidate week 21 at week 23, 4 weeks after the end
     v_merge$mu_m2[23] <- 2
     v_merge$mu_0[23:26] <- 3
     v_merge$mu_0[27:n] <- 0.5                # ends at 28 (declared at 32)
     expect_equal(rt(v_merge)$events, list(list(onset = 9L, detect = 12L, end = 28L)))
     # a week without a fit (NA) gives no decision
     v_na <- v; v_na[11, ] <- NA
     expect_length(rt(v_na)$events, 0L)
     # a candidate is declared only at a reported week: week 12 unreported, so at week 13
     obs_gap <- obs; obs_gap[12] <- FALSE
     yz_gap <- yz; yz_gap[12] <- 0
     r_gap <- MOSAIC:::.onset_growth_realtime(v, yz_gap, obs_gap, fl, s, 3L)
     expect_equal(r_gap$events, list(list(onset = 9L, detect = 13L, end = 17L)))
})

test_that("growth weeks need every one of growth_weeks consecutive weeks", {
     r <- c(0.2, 0.2, 0.2, 0.01, 0.2, 0.2, 0.2, 0.2)
     ok <- MOSAIC:::.onset_growth_ok(r, rep(0.01, 8), rep(TRUE, 8), 0.05, 1.28, 3L)
     expect_identical(ok, c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE))
     wide <- MOSAIC:::.onset_growth_ok(r, rep(0.5, 8), rep(TRUE, 8), 0.05, 1.28, 1L)
     expect_false(any(wide))                                      # lower bound below zero
     expect_identical(MOSAIC:::.onset_growth_ok(r, rep(0.5, 8), rep(TRUE, 8), 0.05, 0, 1L), r >= log1p(0.05))
     # an undefined standard error (rounding made the variance negative) is not a growth week
     na_se <- MOSAIC:::.onset_growth_ok(r, replace(rep(0.01, 8), 2, NA), rep(TRUE, 8), 0.05, 1.28, 1L)
     expect_identical(na_se, c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE))
})

test_that("the smoother reproduces the handoff's Python fit", {
     fx <- .onset_fixture()
     d <- fx$weekly[fx$weekly$iso == "BEN", ]
     py <- utils::read.csv(file.path(fx$dir, "smoother_BEN.csv"))
     n <- max(which(!is.na(d$cases)))                             # BEN reports from its first week
     y <- d$cases[seq_len(n)]
     rows <- which(!is.na(y))
     basis <- MOSAIC:::.onset_basis(n, n / 60)
     fit <- MOSAIC:::.onset_qp_fit(y[rows], rows, basis, 10^seq(-2, 5, by = 0.5), 1.4)
     s <- MOSAIC:::.onset_qp_summary(fit, basis)
     expect_equal(fit$lambda, py$lam[1])
     expect_equal(s$mu, py$mu, tolerance = 1e-7)
     expect_lt(max(abs(s$r - py$r)), 1e-7)
     expect_lt(max(abs((s$r - 1.96 * s$se) - py$lo)), 1e-6)
})

test_that("the smoother's growth rate is the derivative of its log mean and keeps the total", {
     x <- .wave_series(120L, peak_at = 70L)
     basis <- MOSAIC:::.onset_basis(120L, 7.5)
     fit <- MOSAIC:::.onset_qp_fit(x$cases, seq_len(120), basis, 10^seq(-2, 5, by = 0.5), 1.4)
     s <- MOSAIC:::.onset_qp_summary(fit, basis)
     fd <- diff(log(s$mu))
     mid <- (s$r[-1] + s$r[-120]) / 2
     expect_lt(max(abs(fd - mid)[10:110]), 0.02)
     # a Poisson fit with an unpenalised level reproduces the total
     expect_equal(sum(s$mu), sum(x$cases), tolerance = 1e-5)
})

test_that("GCV never selects a lambda past its pole (gamma x edf >= n)", {
     set.seed(7)
     y <- rnbinom(55, mu = 20, size = 4)
     basis <- MOSAIC:::.onset_basis(55L, 55 / 60)                 # 63 basis functions for 55 weeks
     fit <- MOSAIC:::.onset_qp_fit(y, seq_len(55), basis, 10^seq(-2, 5, by = 0.5), 1.4)
     expect_lt(1.4 * fit$edf, 55)
     one <- MOSAIC:::.onset_qp_fit_one(y, basis$B, basis$S, 0.01, NULL, 1.4)
     expect_gte(1.4 * one$edf, 55)
     expect_identical(one$gcv, Inf)
})

test_that("calc_outbreak_onset reproduces the handoff tables (fixture countries)", {
     fx <- .onset_fixture()
     .check_parity(fx, "growth", "retrospective")
     .check_parity(fx, "zheng", "retrospective")
     .check_parity(fx, "zheng", "realtime")
})

test_that("real-time growth reproduces the handoff table (fixture countries)", {
     skip_if_slow()                                               # ~400 refits per country, ~80 s each
     .check_parity(.onset_fixture(), "growth", "realtime", isos = c("BEN", "RWA"))
})
