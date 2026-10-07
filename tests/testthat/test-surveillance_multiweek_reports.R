# Regression tests for multi-week surveillance reports and cross-source double
# counting (v0.100.1 production-suite findings, v0.101.0 red-team fixes):
#   - process_WHO_weekly_data() spreads WHO catch-up / year-to-date reports
#     (NGA 2023 batch reports) over the weeks they cover, caps how far the
#     drop-ratio test reaches back over reported zeros, and applies the curated
#     windows of inst/extdata/surveillance_curation.csv (ZAF 2023, NAM 2025,
#     CIV 2025, NGA 2023 week 52), ZAF 2023 shaped by WHO's epidemic curve
#     (inst/extdata/surveillance_curation_shapes.csv);
#   - process_cholera_surveillance_data() keeps WHO windows whole, drops AI
#     copies of the dashboard and AI aggregates mislabelled as a week (NGA 2023
#     week 21, COG 2023 week 29), applies the curated drops and flags, and limits
#     imputed (fourier) rows to the gap left by the WHO account (ZAF 2023 ramp,
#     GHA 2024, SOM 2026, CIV 2025 residue).

# ---- WHO weekly processor --------------------------------------------------

.raw_who_rows <- function(country, year, weeks, cases, deaths = 0) {
     data.frame(country = country, year = year, week = weeks,
                cases_by_week = cases, deaths_by_week = rep_len(deaths, length(weeks)),
                stringsAsFactors = FALSE)
}

.run_who_processor <- function(raw) {
     tmp <- withr::local_tempdir(.local_envir = parent.frame())
     utils::write.csv(raw, file.path(tmp, "cholera_country_weekly.csv"), row.names = FALSE)
     PATHS <- list(DATA_SCRAPE_WHO_WEEKLY = tmp, DATA_WHO_WEEKLY = file.path(tmp, "out"))
     suppressMessages(process_WHO_weekly_data(PATHS))
     out <- utils::read.csv(file.path(PATHS$DATA_WHO_WEEKLY, "cholera_country_weekly_processed.csv"),
                            stringsAsFactors = FALSE)
     out$date_start <- as.Date(out$date_start)
     out[order(out$iso_code, out$date_start), ]
}

test_that("a year-to-date first report followed by silence is spread back to week 1", {
     # The shape of WHO's first ZAF 2023 row (week 35: 1,390 cases / 47 deaths,
     # then zeros and one sporadic case in week 49), for a country without a
     # curated window, so the rule alone applies.
     raw <- .raw_who_rows("MALAWI", 2023, 35:52,
                          c(1390, rep(0, 13), 1, 0, 0, 0), c(47, rep(0, 17)))
     out <- .run_who_processor(raw)

     win <- out[!is.na(out$catchup_start), ]
     expect_equal(nrow(win), 35L)                                   # weeks 1..35
     expect_equal(range(win$date_start), as.Date(c("2023-01-02", "2023-08-28")))
     expect_equal(nrow(out), nrow(raw) + 34L)                       # 34 unreported weeks added
     # whole counts: each week the floor or ceiling of the even share, exact total
     expect_equal(win$cases, MOSAIC:::.spread_count(1390, n = 35))
     expect_true(all(win$cases %in% c(39, 40)) && sum(win$cases) == 1390)
     expect_true(all(win$deaths %in% c(1, 2)) && sum(win$deaths) == 47)
     expect_true(all(win$disaggregation_method == "who_catchup_uniform"))
     expect_true(all(win$confidence_weight == 0.5))                 # > 26-week window
     expect_true(all(win$catchup_weeks == 35L & win$catchup_cases == 1390 & win$catchup_deaths == 47))
     expect_equal(win$cases_reported[win$week == 35], 1390)         # as published, kept
     expect_true(all(is.na(win$cases_reported[win$week < 35])))     # added weeks had no report
     expect_equal(win$week, 1:35)
     expect_true(all(win$year == 2023L))

     # Totals are conserved and rows outside the window are untouched
     expect_equal(sum(out$cases), 1391)
     expect_equal(sum(out$deaths), 47)
     rest <- out[is.na(out$catchup_start), ]
     expect_equal(rest$cases, rest$cases_reported)
     expect_equal(rest$cases[rest$week == 49], 1)
     expect_true(all(is.na(rest$disaggregation_method)))
     expect_false(any(duplicated(out[, c("iso_code", "date_start")])))
})

test_that("batch reports between silent weeks cover the silent weeks before them (NGA 2023)", {
     # Weekly reporting to week 20, an empty row in week 21, then batches every
     # few weeks with zeros between them.
     cases  <- c(seq(60, 3, length.out = 19), 0, NA, 0, 0, 0, 0, 201, 0, 0, 0, 257, 0, 0, 0, 551, 0, 0, 0, 0)
     deaths <- c(rep(1, 19), 0, NA, 0, 0, 0, 0, 3, 0, 0, 0, 2, 0, 0, 0, 27, 0, 0, 0, 0)
     out <- .run_who_processor(.raw_who_rows("NIGERIA", 2023, seq_along(cases), cases, deaths))

     wk_of <- function(start) out$week[!is.na(out$catchup_start) & out$catchup_start == as.Date(start)]
     # week 26's window stops at the unreported week 21 (the report is not
     # retrospective: a later batch follows within four weeks)
     expect_equal(wk_of("2023-05-29"), 22:26)
     expect_equal(wk_of("2023-07-03"), 27:30)
     expect_equal(wk_of("2023-07-31"), 31:34)
     expect_equal(out$cases[out$week %in% 22:26], c(40, 40, 41, 40, 40))
     expect_equal(out$deaths[out$week %in% 31:34], c(7, 7, 6, 7))
     expect_true(all(out$confidence_weight[out$week %in% 22:26] == 0.8))   # 5-13 weeks
     expect_true(all(out$confidence_weight[out$week %in% 27:34] == 0.9))   # <= 4 weeks
     # the empty week 21 and the reported zero of week 20 are left alone
     expect_true(is.na(out$cases[out$week == 21]))
     expect_equal(out$cases[out$week == 20], 0)
     expect_equal(sum(out$cases, na.rm = TRUE), sum(cases, na.rm = TRUE))
})

test_that("an outbreak onset followed by a single missed week is not spread back (TGO 2024)", {
     cases <- c(1, 0, 0, 4, rep(0, 8), 66, 0, 33, 63, 0, 0, 0, 58)
     out <- .run_who_processor(.raw_who_rows("TOGO", 2024, 30:49, cases, c(rep(0, 12), 7, 0, 2, 2, 0, 0, 0, 3)))
     expect_true(all(is.na(out$catchup_start[out$week <= 45])))
     expect_equal(out$cases[out$week == 42], 66)
     # the 58 after three silent weeks (weeks 46-48) is a catch-up of them; the
     # series ends there, so no fall can be established and it is left as reported
     expect_equal(out$cases[out$week == 49], 58)
})

test_that("the first report of a country joining during an active outbreak is taken at face value (UGA 2023)", {
     # First row week 30 (unreported weeks before), activity continues after it.
     cases <- c(43, 0, 0, 34, 6, 4, rep(0, 8))
     out <- .run_who_processor(.raw_who_rows("UGANDA", 2023, 30:43, cases, c(8, 0, 0, 1, 1, rep(0, 9))))
     expect_true(is.na(out$catchup_start[out$week == 30]))
     expect_equal(out$cases[out$week == 30], 43)
     expect_equal(min(out$week), 30L)                                # nothing added before it
     # week 33 towers over what follows (34 vs 6, 4) after two reported zeros
     expect_equal(out$week[!is.na(out$catchup_start)], 31:33)
     expect_equal(out$cases[out$week %in% 31:33], c(11, 12, 11))
})

test_that("multi-week windows never cross an epi-year boundary and need at least 20 cases", {
     # 2023 is a continuous series ending in zeros (its first row, 25 cases, is
     # followed by rising counts, so it is not a retrospective report)
     raw <- rbind(.raw_who_rows("KENYA", 2023, 40:52, c(25, 30, 45, 50, 60, rep(0, 8)), 0),
                  .raw_who_rows("KENYA", 2024, 1:12, c(0, 0, 199, 0, 0, 0, 0, 15, 0, 0, 0, 0), c(0, 0, 1, rep(0, 9))))
     out <- .run_who_processor(raw)
     win <- out[!is.na(out$catchup_start), ]
     expect_equal(win$date_start, .who_epiweek_start(2024, 1:3))     # 2023 zeros untouched
     expect_equal(win$cases, c(66, 67, 66))
     expect_equal(out$cases[out$year == 2024 & out$week == 8], 15)   # below 20 cases
     expect_equal(out$cases[out$year == 2023 & out$week %in% 40:44], c(25, 30, 45, 50, 60))
     expect_true(all(out$cases[out$year == 2023 & out$week > 44] == 0))
})

test_that("the 2x drop ratio is a boundary: 878 vs 2 x 437 spreads, 873 does not (KEN 2023 week 17)", {
     # KEN 2023 weeks 13-23: a reported zero in week 16, then 878 against a
     # largest next-four-weeks report of 437 (ratio 2.009).
     wk <- 13:23
     cases <- c(504, 382, 376, 0, 878, 310, 149, 109, 437, 239, 144)
     deaths <- c(10, 5, 5, 0, 15, 5, 2, 3, 10, 5, 0)
     out <- .run_who_processor(.raw_who_rows("KENYA", 2023, wk, cases, deaths))
     expect_equal(out$week[!is.na(out$catchup_start)], 16:17)
     expect_equal(out$cases[out$week %in% 16:17], c(439, 439))
     cases[5] <- 873                                                     # ratio 1.998
     out <- .run_who_processor(.raw_who_rows("KENYA", 2023, wk, cases, deaths))
     expect_true(all(is.na(out$catchup_start)))
     expect_equal(out$cases[out$week == 17], 873)
})

test_that("an onset after more than four reported zeros that keeps reporting is not spread", {
     # The red-team's point-source onset: 120, 50, 30, 20 after twelve reported
     # zeros halves within a week, so the drop-ratio test alone would spread it.
     cases <- c(5, rep(0, 12), 120, 50, 30, 20, 10, 5, 2)
     out <- .run_who_processor(.raw_who_rows("MALAWI", 2024, seq_along(cases), cases, 0))
     expect_true(all(is.na(out$catchup_start)))
     expect_equal(out$cases[out$week == 14], 120)
     # the cap is four reported zeros (the look-ahead): four spread, five do not
     four <- c(40, 0, 0, 0, 0, 120, 50, 30, 20, 10, 5)
     out <- .run_who_processor(.raw_who_rows("MALAWI", 2024, seq_along(four), four, 0))
     expect_equal(out$week[!is.na(out$catchup_start)], 2:6)
     five <- c(40, 0, 0, 0, 0, 0, 120, 50, 30, 20, 10)
     out <- .run_who_processor(.raw_who_rows("MALAWI", 2024, seq_along(five), five, 0))
     expect_true(all(is.na(out$catchup_start)))
})

test_that("past four reported zeros the drop-ratio test needs a series alternating with zeros (NGA 2023 week 43)", {
     # Eight reported zeros, 433, then 0, 148, 0, 0: the batch pattern of NGA 2023.
     cases <- c(40, rep(0, 8), 433, 0, 148, 0, 0, 0, 0)
     out <- .run_who_processor(.raw_who_rows("MALAWI", 2024, seq_along(cases), cases, 0))
     wk <- function(start) out$week[!is.na(out$catchup_start) & out$catchup_start == start]
     expect_equal(wk(.who_epiweek_start(2024, 2)), 2:10)
     expect_equal(sum(out$cases[out$week %in% 2:10]), 433)
     # two zeros among the next four reported weeks are needed, one is not enough
     one <- c(40, rep(0, 5), 120, 0, 50, 30, 20)
     out <- .run_who_processor(.raw_who_rows("MALAWI", 2024, seq_along(one), one, 0))
     expect_true(all(is.na(out$catchup_start)))
     two <- c(40, rep(0, 5), 120, 0, 50, 0, 30)
     out <- .run_who_processor(.raw_who_rows("MALAWI", 2024, seq_along(two), two, 0))
     expect_equal(out$week[!is.na(out$catchup_start)], 2:7)
})

test_that("the curated ZAF 2023 window follows WHO's epidemic curve, report-dated, and its own deaths curve", {
     raw <- .raw_who_rows("SOUTH AFRICA", 2023, 35:52,
                          c(1390, rep(0, 13), 1, 0, 0, 0), c(47, rep(0, 17)))
     out <- .run_who_processor(raw)
     win <- out[!is.na(out$catchup_start), ]
     expect_equal(win$week, 5:35)                                    # from the week of 1 Feb
     expect_false(any(out$year == 2023 & out$week < 5))              # weeks 1-4 not created
     # weekly cases of WHO sitrep #5 Figure 5, onset + 2 days, Monday-Sunday weeks
     # 5-28 (the imported case in week 28), none from week 29 on
     cur <- MOSAIC:::.surveillance_curation("who_window")
     sh  <- MOSAIC:::.surveillance_curation_shapes(cur)
     z   <- sh[sh$id == "ZAF-2023-AAR" & !is.na(sh$cumulative_cases), ]
     weekly <- diff(z$cumulative_cases)
     expect_length(weekly, 24L)
     expect_equal(win$cases, MOSAIC:::.spread_count(1390, c(weekly, rep(0, 7))))
     expect_true(sum(win$cases) == 1390 && sum(win$deaths) == 47)
     expect_true(all(win$cases == round(win$cases) & win$deaths == round(win$deaths)))   # whole counts
     # the Hammanskraal surge: rows of 15, 22 and 29 May, peak in 22-28 May
     expect_equal(win$cases[win$date_start %in% as.Date(c("2023-05-15", "2023-05-22", "2023-05-29"))],
                  c(220, 432, 249))
     expect_equal(win$date_start[which.max(win$cases)], as.Date("2023-05-22"))
     expect_gt(sum(win$cases[win$week %in% 18:25]), 0.95 * 1390)       # 1 May - 25 June
     expect_equal(win$cases[win$week == 28], 1)                      # the Karachi case, 14 + 2 July
     expect_true(all(win$cases[win$week %in% c(9:11, 16, 29:35)] == 0))
     # deaths follow the report-dated deaths curve, not the case curve
     expect_equal(win$deaths[win$week %in% c(8, 20:27)], c(1, 10, 14, 5, 5, 5, 3, 3, 1))
     expect_true(all(win$deaths[!win$week %in% c(8, 20:27)] == 0))
     expect_false(isTRUE(all.equal(win$deaths, MOSAIC:::.spread_count(47, c(weekly, rep(0, 7))))))
     expect_equal(win$cases_reported[win$week == 35], 1390)
     expect_true(all(win$disaggregation_method == "who_catchup_curated_shaped"))
     expect_true(all(win$catchup_curation_id == "ZAF-2023-AAR"))
     expect_true(all(win$catchup_weeks == 31L & win$confidence_weight == 0.9))
     expect_equal(sum(out$cases), 1391)
     expect_equal(sum(out$deaths), 47)
})

test_that("the curated NAM 2025 window starts at the week of the first case (Sunday 2 Mar 2025: week 9)", {
     out <- .run_who_processor(.raw_who_rows("NAMIBIA", 2025, 12:23, c(22, rep(0, 11)), 0))
     win <- out[!is.na(out$catchup_start), ]
     expect_equal(win$date_start, .who_epiweek_start(2025, 9:12))    # Mon 24 Feb - Sun 2 Mar is week 9
     expect_equal(win$cases, c(6, 5, 6, 5))                          # 22 over 4 weeks, halves rounded up
     expect_equal(min(out$date_start), .who_epiweek_start(2025, 9))
     expect_true(all(win$confidence_weight == 0.9 & win$catchup_curation_id == "NAM-2025-first-case"))
})

test_that("a WHO week runs Monday to Sunday: a date maps to the Monday on or before it", {
     d <- as.Date(c("2025-03-02", "2025-03-03", "2025-03-09", "2023-02-01", "2023-07-31", "2023-05-21"))
     expect_equal(MOSAIC:::.who_week_of_date(d),
                  as.Date(c("2025-02-24", "2025-03-03", "2025-03-03", "2023-01-30", "2023-07-31", "2023-05-15")))
     expect_true(all(as.POSIXlt(MOSAIC:::.who_week_of_date(as.Date("2024-01-01") + 0:20))$wday == 1L))
     expect_equal(MOSAIC:::.who_week_of_date(as.Date(NA)), as.Date(NA))
})

.who_rows <- function(iso, year, weeks, cases, deaths = 0) {
     ds <- .who_epiweek_start(year, weeks)
     data.frame(iso_code = iso, country = MOSAIC::convert_iso_to_country(iso), year = year, week = weeks,
                cases = cases, deaths = rep_len(deaths, length(weeks)), date_start = ds, date_stop = ds + 6L,
                month = as.integer(format(ds, "%m")), stringsAsFactors = FALSE)
}

test_that("curated windows spread an untested end-of-series report (CIV 2025 week 33) and override the zero cap (NGA 2023 week 52)", {
     cur <- MOSAIC:::.surveillance_curation("who_window")
     civ <- .who_rows("CIV", 2025, 23:33, c(45, 0, 0, 55, 9, 0, 280, 0, 0, 0, 114), c(7, 0, 0, 0, 0, 0, 12, 0, 0, 0, 1))
     rule <- MOSAIC:::.who_reallocate_catchup_reports(civ)
     expect_true(all(is.na(rule$catchup_start[rule$week >= 30])))           # the series ends at the report
     out <- MOSAIC:::.who_reallocate_catchup_reports(civ, cur)
     w <- out[out$week >= 30, ]
     expect_equal(w$cases, c(29, 28, 29, 28))                           # halves rounded up
     expect_equal(w$cases_reported, c(0, 0, 0, 114))
     expect_true(all(w$catchup_curation_id == "CIV-2025-W33" & w$confidence_weight == 0.9))
     expect_equal(out$cases[out$week %in% 28:29], c(140, 140))           # the rule's window stays
     # NGA 2023: six reported zeros then 242 and steady weekly reports -> capped by
     # the rule, restored by the curated window
     nga <- .who_rows("NGA", 2023, 44:52, c(15, 148, 0, 0, 0, 0, 0, 0, 242), c(0, 6, 0, 0, 0, 0, 0, 0, 20))
     nga <- rbind(nga, .who_rows("NGA", 2024, 1:4, c(119, 89, 85, 80), 1))
     expect_true(all(is.na(MOSAIC:::.who_reallocate_catchup_reports(nga)$catchup_start)))
     out <- MOSAIC:::.who_reallocate_catchup_reports(nga, cur)
     w <- out[out$year == 2023 & out$week >= 46, ]
     expect_equal(sum(w$cases), 242)
     expect_true(all(w$catchup_curation_id == "NGA-2023-W52" & w$catchup_weeks == 7L))
})

test_that("a curated window must cover only silent weeks of the report's epi year", {
     cur <- data.frame(id = "T1", iso_code = "MWI", action = "who_window", report_year = 2024L, report_week = 10L,
                       date_start = as.Date("2024-02-05"), date_stop = as.Date(NA), evidence = "e", reference = "r",
                       added = "2026-10-01", stringsAsFactors = FALSE)
     d <- .who_rows("MWI", 2024, 1:12, c(5, 0, 0, 0, 0, 30, 0, 0, 0, 90, 0, 0))
     expect_error(MOSAIC:::.who_reallocate_catchup_reports(d, cur), "non-silent WHO week")
     cur$date_start <- as.Date("2023-12-20")
     expect_error(MOSAIC:::.who_reallocate_catchup_reports(d, cur), "must lie in WHO epi year 2024")
     cur$date_start <- as.Date("2024-02-19"); cur$report_week <- 11L         # a zero week
     expect_warning(out <- MOSAIC:::.who_reallocate_catchup_reports(d, cur), "no positive WHO report")
     expect_true(all(is.na(out$catchup_curation_id)))
     cur$report_week <- 10L
     out <- MOSAIC:::.who_reallocate_catchup_reports(d, cur)
     expect_equal(out$week[!is.na(out$catchup_curation_id)], 8:10)
     expect_equal(out$cases[out$week %in% 8:10], c(30, 30, 30))
})

test_that("cumulative anchors give each WHO week the curve's increment, interpolating coarse anchors", {
     # WHO 2024 weeks 2-5 run Monday 8 Jan - Sunday 4 Feb; anchors at the ends of
     # weeks 1, 3 and 4: the week-2 and week-3 increments split the first segment
     a <- data.frame(date = as.Date(c("2024-01-07", "2024-01-21", "2024-01-28")),
                     cumulative_cases = c(0, 10, 40))
     wk <- .who_epiweek_start(2024, 2:5)
     expect_equal(MOSAIC:::.curated_shape_weights(a, wk, rep(TRUE, 4), "T"), c(5, 5, 30, 0))
     # a mid-week anchor is interpolated by day: 7 of the 11 days fall in week 2
     a2 <- data.frame(date = as.Date(c("2024-01-07", "2024-01-18")), cumulative_cases = c(0, 11))
     expect_equal(MOSAIC:::.curated_shape_weights(a2, wk, rep(TRUE, 4), "T"), c(7, 4, 0, 0))
     # a Sunday anchor closes its own week; the count a day later opens the next
     a3 <- data.frame(date = as.Date(c("2024-01-07", "2024-01-14", "2024-01-15")),
                      cumulative_cases = c(0, 6, 7))
     expect_equal(MOSAIC:::.curated_shape_weights(a3, wk, rep(TRUE, 4), "T"), c(6, 1, 0, 0))
     # each series uses its own rows: NA marks a row that is not an anchor of it
     # (deaths: 2 by Wed 10 Jan, then 1 more over the 18 days to 28 Jan)
     ad <- data.frame(date = as.Date(c("2024-01-07", "2024-01-10", "2024-01-21", "2024-01-28")),
                      cumulative_cases = c(0, NA, 10, 40), cumulative_deaths = c(0, 2, NA, 3))
     expect_equal(MOSAIC:::.curated_shape_weights(ad, wk, rep(TRUE, 4), "T"), c(5, 5, 30, 0))
     expect_equal(MOSAIC:::.curated_shape_weights(ad, wk, rep(TRUE, 4), "T", "cumulative_deaths"),
                  c(2 + 4 / 18, 7 / 18, 7 / 18, 0))
     # a decreasing curve handed in directly (bypassing the reader) is refused, not spread negative
     dec <- data.frame(date = as.Date(c("2024-01-07", "2024-01-14", "2024-01-21", "2024-01-28")),
                       cumulative_cases = c(0, 10, 5, 40), cumulative_deaths = c(0, 3, 2, 4))
     expect_error(MOSAIC:::.curated_shape_weights(dec, wk, rep(TRUE, 4), "T"), "case curve decreases")
     expect_error(MOSAIC:::.curated_shape_weights(dec, wk, rep(TRUE, 4), "T", "cumulative_deaths"),
                  "death curve decreases")
     # the curve must lie within the weeks that may carry cases
     expect_error(MOSAIC:::.curated_shape_weights(a, wk[1:2], c(TRUE, TRUE), "T"), "outside the window")
     expect_error(MOSAIC:::.curated_shape_weights(a, wk, c(TRUE, TRUE, FALSE, FALSE), "T"), "outside the window")
     # a shaped window with no anchors supplied is an error, not an even spread
     zaf <- .who_rows("ZAF", 2023, 35:40, c(1390, 0, 0, 0, 0, 0), c(47, 0, 0, 0, 0, 0))
     expect_error(MOSAIC:::.who_reallocate_catchup_reports(zaf, MOSAIC:::.surveillance_curation("who_window")),
                  "no anchors were supplied")
})

# A curated, shaped test window: MWI 2024 week 10 reports 90 cases (and
# report_deaths deaths) after silent weeks 6-9; the curves time them within weeks
# 6-10 (anchors on the Sundays ending weeks 5-10).
.shaped_window <- function(cases_anchor, deaths_anchor = NULL, report_deaths = 6) {
     cur <- data.frame(id = "T-SHAPE", iso_code = "MWI", action = "who_window", report_year = 2024L,
                       report_week = 10L, date_start = as.Date("2024-02-05"), date_stop = as.Date(NA),
                       shape = "cumulative", evidence = "e", reference = "r", added = "2026-10-01",
                       stringsAsFactors = FALSE)
     ends <- .who_epiweek_start(2024, 5:10) + 6L
     shapes <- data.frame(id = "T-SHAPE", date = ends, cumulative_cases = cases_anchor,
                          cumulative_deaths = if (is.null(deaths_anchor)) NA_real_ else deaths_anchor)
     d <- .who_rows("MWI", 2024, 1:12, c(5, 0, 0, 0, 0, 0, 0, 0, 0, 90, 0, 0),
                    c(0, 0, 0, 0, 0, 0, 0, 0, 0, report_deaths, 0, 0))
     out <- MOSAIC:::.who_reallocate_catchup_reports(d, cur, shapes)
     out[out$week %in% 6:10, ]
}

test_that("a curated deaths curve times the deaths; without one deaths follow the case curve", {
     cum_c <- c(0, 10, 30, 60, 80, 90)                                 # weeks 6-10: 10, 20, 30, 20, 10
     # no deaths curve: deaths follow the case curve, as before
     w0 <- .shaped_window(cum_c)
     expect_equal(w0$cases, c(10, 20, 30, 20, 10))
     expect_equal(w0$deaths, MOSAIC:::.spread_count(6, c(10, 20, 30, 20, 10)))
     # a deaths curve: the six deaths in weeks 6 and 9, totals conserved, cases unchanged
     w1 <- .shaped_window(cum_c, c(0, 3, 3, 3, 6, 6))
     expect_equal(w1$cases, w0$cases)
     expect_equal(w1$deaths, c(3, 0, 0, 3, 0))
     expect_equal(sum(w1$deaths), 6)
     expect_true(all(w1$disaggregation_method == "who_catchup_curated_shaped" & w1$confidence_weight == 0.9))
     # a deaths curve against a report without deaths is an error
     expect_error(.shaped_window(cum_c, c(0, 3, 3, 3, 6, 6), report_deaths = 0), "death curve counts 6")
})

test_that("a curve whose total is far from the report is refused (an anchor typo)", {
     # 90 reported: a case curve from 45 (half) to 91.8 (2% above) is rescaled
     expect_equal(sum(.shaped_window(c(0, 5, 15, 30, 40, 45))$cases), 90)
     expect_equal(sum(.shaped_window(c(0, 10, 30, 60, 80, 91))$cases), 90)
     expect_error(.shaped_window(c(0, 5, 15, 30, 40, 44)), "case curve counts 44 against a report of 90")
     expect_error(.shaped_window(c(0, 10, 30, 60, 80, 900)), "between 0.5 and 1.02 times the report")
     expect_error(.shaped_window(c(0, 10, 30, 60, 80, 90), c(0, 1, 2, 3, 4, 7)), "death curve counts 7")
     expect_error(.shaped_window(c(0, 10, 30, 60, 80, 90), c(0, 1, 1, 1, 2, 2)), "death curve counts 2")
})

test_that("the curation shape table is validated against the curation table", {
     cur <- MOSAIC:::.surveillance_curation("who_window")
     sh  <- MOSAIC:::.surveillance_curation_shapes(cur)
     expect_equal(unique(sh$id), "ZAF-2023-AAR")
     z  <- sh[sh$id == "ZAF-2023-AAR" & !is.na(sh$cumulative_cases), ]
     expect_equal(range(z$date), as.Date(c("2023-01-29", "2023-07-16")))  # Sundays: WHO week ends
     expect_true(all(as.POSIXlt(z$date)$wday == 0L))
     expect_equal(z$cumulative_cases[c(1, nrow(z))], c(0, 1272))         # 1,271 digitized + the Karachi case
     expect_false(is.unsorted(z$cumulative_cases))
     zd <- sh[sh$id == "ZAF-2023-AAR" & !is.na(sh$cumulative_deaths), ]
     expect_equal(zd$cumulative_deaths[c(1, nrow(zd))], c(0, 47))
     expect_equal(max(zd$date), as.Date("2023-07-04"))                   # NDoH: 47 as of 4 July
     expect_false(is.unsorted(zd$cumulative_deaths))
     tmp <- withr::local_tempfile(fileext = ".csv")
     put <- function(x) { utils::write.csv(x, tmp, row.names = FALSE, na = ""); tmp }
     raw <- utils::read.csv(system.file("extdata", "surveillance_curation_shapes.csv", package = "MOSAIC"),
                            colClasses = "character", na.strings = "")
     cc <- which(!is.na(raw$cumulative_cases))
     bad <- raw; bad$cumulative_cases[cc[3]] <- "1"
     expect_error(MOSAIC:::.surveillance_curation_shapes(cur, put(bad)), "case count must never decrease")
     bad <- raw; bad$cumulative_cases[1] <- "1"
     expect_error(MOSAIC:::.surveillance_curation_shapes(cur, put(bad)), "case count must start at 0")
     bad <- raw; bad$cumulative_cases[cc] <- "0"
     expect_error(MOSAIC:::.surveillance_curation_shapes(cur, put(bad)), "case count must end above 0")
     bad <- raw; bad$cumulative_deaths[!is.na(bad$cumulative_deaths)] <- "0"
     expect_error(MOSAIC:::.surveillance_curation_shapes(cur, put(bad)), "death count must end above 0")
     dd <- which(!is.na(raw$cumulative_deaths))
     bad <- raw; bad$cumulative_deaths[dd[1]] <- "1"
     expect_error(MOSAIC:::.surveillance_curation_shapes(cur, put(bad)), "death count must start at 0")
     bad <- raw; bad$cumulative_deaths[raw$date == "2023-05-28"] <- "12"        # below 27 May's 24
     expect_error(MOSAIC:::.surveillance_curation_shapes(cur, put(bad)), "death count must never decrease")
     bad <- raw; bad$cumulative_cases[cc[length(cc)]] <- "Inf"
     expect_error(MOSAIC:::.surveillance_curation_shapes(cur, put(bad)), "non-finite count")
     bad <- raw; bad$cumulative_cases[cc[2]] <- "12x"
     expect_error(MOSAIC:::.surveillance_curation_shapes(cur, put(bad)), "unreadable count")
     bad <- raw; bad$cumulative_cases[cc[2]] <- NA
     expect_error(MOSAIC:::.surveillance_curation_shapes(cur, put(bad)), "needs a case or death count")
     bad <- raw; bad$date[2] <- "2023-02-30"
     expect_error(MOSAIC:::.surveillance_curation_shapes(cur, put(bad)), "valid date")
     bad <- raw; bad$date[2] <- bad$date[1]
     expect_error(MOSAIC:::.surveillance_curation_shapes(cur, put(bad)), "distinct dates")
     bad <- raw; bad$id <- "NAM-2025-first-case"
     expect_error(MOSAIC:::.surveillance_curation_shapes(cur, put(bad)), "not a who_window row with shape")
     expect_error(MOSAIC:::.surveillance_curation_shapes(cur, put(raw[0, ])), "no anchors for: ZAF-2023-AAR")
     expect_error(MOSAIC:::.surveillance_curation_shapes(cur, file.path(tempdir(), "no-such-shapes.csv")),
                  "shape table not found")
     # the deaths column is optional: without it, every curve's deaths follow its cases
     nodeaths <- raw[!is.na(raw$cumulative_cases), setdiff(names(raw), "cumulative_deaths")]
     sh2 <- MOSAIC:::.surveillance_curation_shapes(cur, put(nodeaths))
     expect_true(all(is.na(sh2$cumulative_deaths)))
     tab <- utils::read.csv(system.file("extdata", "surveillance_curation.csv", package = "MOSAIC"),
                            colClasses = "character")
     tab$shape[tab$id == "AGO-2023-absence"] <- "cumulative"
     expect_error(MOSAIC:::.surveillance_curation(path = put(tab)), "who_window rows only")
     tab$shape[tab$id == "AGO-2023-absence"] <- ""
     tab$shape[tab$id == "ZAF-2023-AAR"] <- "spline"
     expect_error(MOSAIC:::.surveillance_curation(path = put(tab)), "unknown shape")
})

test_that("the surveillance curation table is complete and validated", {
     cur <- MOSAIC:::.surveillance_curation()
     expect_false(anyDuplicated(cur$id) > 0)
     expect_true(all(nzchar(cur$evidence) & nzchar(cur$reference)))
     expect_true(all(cur$action %in% c("who_window", "drop_imputed", "flag_imputed")))
     expect_true(all(c("ZAF-2023-AAR", "NAM-2025-first-case", "CIV-2025-W33", "NGA-2023-W52",
                       "SSD-2023-2024-absence", "AGO-2023-absence", "BFA-2025-unconfirmed") %in% cur$id))
     win <- cur[cur$action == "who_window", ]
     expect_true(all(!is.na(win$report_year) & !is.na(win$report_week) & !is.na(win$date_start)))
     tmp <- withr::local_tempfile(fileext = ".csv")
     bad <- utils::read.csv(system.file("extdata", "surveillance_curation.csv", package = "MOSAIC"),
                            colClasses = "character")
     bad$action[1] <- "rewrite"
     utils::write.csv(bad, tmp, row.names = FALSE)
     expect_error(MOSAIC:::.surveillance_curation(path = tmp), "unknown action")
     bad$action[1] <- "who_window"; bad$id[2] <- bad$id[1]
     utils::write.csv(bad, tmp, row.names = FALSE)
     expect_error(MOSAIC:::.surveillance_curation(path = tmp), "duplicated ids")
})

test_that(".who_epiweek_label inverts .who_epiweek_start", {
     mon <- seq(as.Date("2022-01-03"), as.Date("2027-12-27"), by = "week")
     lab <- MOSAIC:::.who_epiweek_label(mon)
     expect_equal(MOSAIC:::.who_epiweek_start(lab$year, lab$week), mon)
     expect_equal(MOSAIC:::.who_epiweek_label(as.Date(c("2025-12-29", "2026-01-05"))),
                  list(year = c(2025L, 2026L), week = c(53L, 1L)))
})

# ---- Multi-source combiner ---------------------------------------------------

# One country (ZAF unless given), weekly rows built from a list of
# list(w = week index, cases, deaths, method) per source; week 1 = 2023-01-02.
.combiner_fixture <- function(who = NULL, jhu = NULL, ai = NULL, annual = NULL, iso = "ZAF") {
     tmp <- withr::local_tempdir(.local_envir = parent.frame())
     P <- list(DATA_WHO_WEEKLY = file.path(tmp, "who"), DATA_JHU_WEEKLY = file.path(tmp, "jhu"),
               DATA_SUPP_WEEKLY = file.path(tmp, "supp"), DATA_AI_WEEKLY = file.path(tmp, "ai"),
               DATA_CHOLERA_WEEKLY = file.path(tmp, "cw"), DATA_CHOLERA_DAILY = file.path(tmp, "cd"))
     for (d in P) dir.create(d, recursive = TRUE, showWarnings = FALSE)
     base <- function(w) {
          ws <- as.Date("2023-01-02") + 7 * (w - 1)
          data.frame(iso_code = iso, country = MOSAIC::convert_iso_to_country(iso),
                     year = as.integer(format(ws, "%G")), week = as.integer(format(ws, "%V")),
                     date_start = as.character(ws), date_stop = as.character(ws + 6),
                     month = as.integer(format(ws, "%m")), stringsAsFactors = FALSE)
     }
     if (!is.null(who)) utils::write.csv(who, file.path(P$DATA_WHO_WEEKLY, "cholera_country_weekly_processed.csv"), row.names = FALSE)
     if (!is.null(jhu)) utils::write.csv(cbind(base(jhu$w), cases = jhu$cases, deaths = jhu$deaths),
                                         file.path(P$DATA_JHU_WEEKLY, "cholera_country_weekly_processed.csv"), row.names = FALSE)
     if (!is.null(ai)) utils::write.csv(cbind(base(ai$w), cases = ai$cases, deaths = ai$deaths,
                                              confidence_weight = ai$cw, disaggregation_method = ai$method),
                                        file.path(P$DATA_AI_WEEKLY, "cholera_country_weekly_processed.csv"), row.names = FALSE)
     if (!is.null(annual)) {
          P$DATA_WHO_ANNUAL <- file.path(tmp, "annual"); dir.create(P$DATA_WHO_ANNUAL)
          utils::write.csv(annual, file.path(P$DATA_WHO_ANNUAL, "who_afro_annual.csv"), row.names = FALSE)
     }
     list(P = P, base = base)
}

# WHO processed rows for a fixture: plain weeks plus at most one multi-week window
# (built with the processor's own helper, so the columns match production).
.who_processed <- function(fx, w, cases, deaths, iso = "ZAF") {
     d <- cbind(fx$base(w), cases = cases, deaths = deaths)
     d$date_start <- as.Date(d$date_start)
     d$date_stop  <- as.Date(d$date_stop)
     d$year <- MOSAIC:::.who_epiweek_label(d$date_start)$year
     d$week <- MOSAIC:::.who_epiweek_label(d$date_start)$week
     MOSAIC:::.who_reallocate_catchup_reports(d)
}

.run_combiner <- function(P) {
     suppressWarnings(suppressMessages(process_cholera_surveillance_data(P, include_ai = TRUE)))
     out <- utils::read.csv(file.path(P$DATA_CHOLERA_WEEKLY, "cholera_surveillance_weekly_combined.csv"),
                            stringsAsFactors = FALSE)
     out$date_start <- as.Date(out$date_start)
     adj <- utils::read.csv(file.path(P$DATA_CHOLERA_WEEKLY, "cholera_surveillance_weekly_adjustments.csv"),
                            stringsAsFactors = FALSE)
     list(out = out[order(out$date_start), ], adj = adj)
}

test_that("ZAF 2023: the WHO year-to-date report replaces the AI fourier ramp instead of adding to it", {
     fx <- .combiner_fixture()
     who <- .who_processed(fx, 35:52, c(1390, rep(0, 13), 1, 0, 0, 0), c(47, rep(0, 17)))
     ramp <- c(seq(70, 100, length.out = 8), seq(100, 3, length.out = 18))        # ~1,680 cases
     ai <- list(w = c(1:26, 35), cases = c(ramp, 1390), deaths = c(ramp / 30, 47),
                cw = c(rep(0.475, 26), 0.9), method = c(rep("fourier_country_k1", 26), "observed"))
     fx <- .combiner_fixture(who = who, ai = ai)
     res <- .run_combiner(fx$P)
     out <- res$out
     y23 <- out[format(out$date_start, "%Y") == "2023", ]

     expect_equal(sum(y23$cases, na.rm = TRUE), 1391)                 # not 1391 + ~1680
     expect_equal(sum(y23$deaths, na.rm = TRUE), 47)
     expect_true(all(y23$source[y23$week <= 35] == "WHO"))
     expect_true(all(y23$disaggregation_method[y23$week <= 35] == "who_catchup_uniform"))
     expect_true(max(y23$cases, na.rm = TRUE) <= 40)                   # no one-week spike
     expect_true(all(y23$confidence_weight[y23$week <= 35] == 0.5))
     # the AI copy of the week-35 report is logged as a dashboard copy
     expect_true(any(res$adj$rule == "who_copy_dropped" & res$adj$cases_before == 1390))
     expect_equal(sum(res$adj$rule == "who_catchup_uniform"), 35L)
     # the fourier rows inside the window are listed although priority would drop them too
     expect_equal(sum(res$adj$rule == "imputed_dropped_in_who_window"), 26L)
     expect_equal(sum(res$adj$cases_before[res$adj$rule == "imputed_dropped_in_who_window"]), sum(ramp))

     # the daily fit-target file carries the same totals and no daily spike
     daily <- utils::read.csv(file.path(fx$P$DATA_CHOLERA_DAILY, "cholera_surveillance_daily_combined.csv"),
                              stringsAsFactors = FALSE)
     d23 <- daily[format(as.Date(daily$date), "%Y") == "2023", ]
     expect_equal(sum(d23$cases, na.rm = TRUE), 1391)
     expect_equal(sum(d23$deaths, na.rm = TRUE), 47)
     expect_lte(max(d23$cases, na.rm = TRUE), 6)
     expect_true(all(d23$confidence_weight[as.Date(d23$date) <= as.Date("2023-09-03")] == 0.5))
})

test_that("a window observed weekly by another source takes that source's shape and keeps the WHO total (KEN 2023)", {
     fx <- .combiner_fixture(iso = "KEN")
     who <- .who_processed(fx, 1:6, c(0, 0, 880, 109, 232, 109), c(0, 0, 18, 4, 1, 0), iso = "KEN")
     jhu <- list(w = 1:6, cases = c(340, 229, 204, 170, 155, 308), deaths = NA)
     fx <- .combiner_fixture(who = who, jhu = jhu, iso = "KEN")
     res <- .run_combiner(fx$P)
     out <- res$out
     w13 <- out[out$date_start <= as.Date("2023-01-16"), ]
     expect_equal(w13$cases, c(387, 261, 232))                        # 880 in JHU's proportions
     expect_equal(w13$deaths, c(8, 5, 5))
     expect_true(all(w13$source == "WHO" & w13$disaggregation_method == "who_catchup_shaped"))
     expect_true(all(w13$confidence_weight == 0.9))
     # outside the window ordinary priority applies: WHO's own weekly counts
     expect_equal(out$cases[out$date_start == as.Date("2023-01-23")], 109)
     expect_equal(sum(res$adj$rule == "absorbed_by_who_window"), 3L)
})

test_that("a window following a curated epidemic curve keeps it when another source observes every week", {
     fx <- .combiner_fixture()
     who <- cbind(fx$base(35:52), cases = c(1390, rep(0, 17)), deaths = c(47, rep(0, 17)))
     who$date_start <- as.Date(who$date_start); who$date_stop <- as.Date(who$date_stop)
     lab <- MOSAIC:::.who_epiweek_label(who$date_start)
     who$year <- lab$year; who$week <- lab$week
     cur <- MOSAIC:::.surveillance_curation("who_window")
     who <- MOSAIC:::.who_reallocate_catchup_reports(who, cur, MOSAIC:::.surveillance_curation_shapes(cur))
     shaped <- who$cases[who$disaggregation_method %in% "who_catchup_curated_shaped"]
     expect_length(shaped, 31L)
     jhu <- list(w = 5:35, cases = rep(3, 31), deaths = 0)            # a positive count every window week
     fx <- .combiner_fixture(who = who, jhu = jhu)
     res <- .run_combiner(fx$P)
     win <- res$out[res$out$date_start >= as.Date("2023-01-30") & res$out$date_start <= as.Date("2023-08-28"), ]
     expect_equal(win$cases, shaped)                                  # not 1,390 in JHU's flat proportions
     expect_true(all(win$source == "WHO" & win$disaggregation_method == "who_catchup_curated_shaped"))
     expect_true(all(win$confidence_weight == 0.9))
     expect_equal(sum(res$adj$rule == "absorbed_by_who_window"), 31L)
     expect_false(any(res$adj$rule == "who_catchup_shaped"))
     expect_true(all(grepl("in proportion to the curated epidemic curve; curated window ZAF-2023-AAR",
                           res$adj$detail[res$adj$rule == "who_catchup_curated_shaped"], fixed = TRUE)))
})

test_that("a window only partly observed by another source keeps the even WHO spread (atomic window)", {
     fx <- .combiner_fixture(iso = "NGA")
     who <- .who_processed(fx, 1:10, c(80, 60, 0, 0, 0, 551, 0, 0, 0, 0), c(1, 1, 0, 0, 0, 27, 0, 0, 0, 0), iso = "NGA")
     ai <- list(w = c(4, 6), cases = c(62, 551), deaths = c(3, 27), cw = 0.9, method = "observed")
     fx <- .combiner_fixture(who = who, ai = ai, iso = "NGA")
     res <- .run_combiner(fx$P)
     out <- res$out
     win <- out[out$date_start >= as.Date("2023-01-16") & out$date_start <= as.Date("2023-02-06"), ]
     expect_equal(win$cases, c(138, 138, 137, 138))                    # AI's 62 does not enter
     expect_true(all(win$source == "WHO"))
     expect_true(any(res$adj$rule == "absorbed_by_who_window" & res$adj$cases_before == 62))
     expect_true(any(res$adj$rule == "who_copy_dropped" & res$adj$cases_before == 551))
     expect_equal(sum(out$cases, na.rm = TRUE), 80 + 60 + 551)
})

test_that("an AI week carrying a year-to-date total beside small WHO weeks is dropped (NGA 2023 week 21)", {
     fx <- .combiner_fixture(iso = "NGA")
     who <- .who_processed(fx, c(1:4, 6:9), c(21, 14, 2, 0, 9, 22, 35, 30), c(0, 1, 0, 0, 0, 0, 1, 2), iso = "NGA")
     ai <- list(w = c(5, 10), cases = c(1851, 40), deaths = c(52, 1), cw = 0.95, method = "observed")
     fx <- .combiner_fixture(who = who, ai = ai, iso = "NGA")
     res <- .run_combiner(fx$P)
     out <- res$out
     expect_true(is.na(out$cases[out$date_start == as.Date("2023-01-30")]))   # week 5 now unobserved
     expect_equal(out$cases[out$date_start == as.Date("2023-03-06")], 40)      # a plausible AI week stays
     expect_equal(res$adj$rule[res$adj$cases_before == 1851], "ai_aggregate_dropped")
})

test_that("imputed rows only fill the gap between the observed weeks and the WHO annual total (GHA 2024)", {
     # 2023 (weeks 1-52): fourier rows Apr-Aug beside a WHO-reported outbreak whose
     # weekly total equals the WHO annual total -> fourier emptied.
     # 2024 (weeks 53-104): observed 300, fourier 900, annual 1,000 -> the fourier
     # keeps the 700 gap.
     # 2025 (weeks 105-156): no observed week at all, fourier 800 against an annual
     # total of 500 -> the fourier keeps 500.
     fx <- .combiner_fixture(iso = "GHA")
     who <- .who_processed(fx, c(40:52, 60:62), c(10, 50, 400, 900, 700, 600, 500, 400, 300, 200, 300, 150, 108, 100, 120, 80),
                           c(rep(1, 13), 2, 2, 2), iso = "GHA")
     ai <- list(w = c(14:33, 70:79, 110:119),
                cases = c(rep(46.85, 20), rep(90, 10), rep(80, 10)),
                deaths = c(rep(0.4, 20), rep(1, 10), rep(1, 10)),
                cw = 0.475, method = "fourier_country_k1")
     annual <- data.frame(iso_code = c("GHA", "GHA", "GHA", "AFRO"), year = c(2023, 2024, 2025, 2023),
                          cases_total = c(4618, 1000, 500, 99999))
     fx <- .combiner_fixture(who = who, ai = ai, annual = annual, iso = "GHA")
     res <- .run_combiner(fx$P)
     out <- res$out
     yr <- as.integer(format(out$date_start + 3, "%Y"))
     expect_true(all(is.na(out$cases[yr == 2023 & out$date_start < as.Date("2023-10-02")])))
     expect_equal(sum(out$cases[yr == 2023], na.rm = TRUE), 4618)
     f24 <- out[yr == 2024 & out$disaggregation_method %in% "fourier_country_k1", ]
     expect_equal(sum(f24$cases), 700)
     expect_equal(sum(f24$deaths), 10 * 700 / 900)
     f25 <- out[yr == 2025 & out$disaggregation_method %in% "fourier_country_k1", ]
     expect_equal(sum(f25$cases), 500)
     expect_equal(f25$deaths, rep(500 / 800, 10))
     expect_equal(sum(res$adj$rule == "imputed_dropped_annual_accounted"), 20L)
     expect_equal(sum(res$adj$rule == "imputed_scaled_annual_residual"), 20L)
})

test_that("without DATA_WHO_ANNUAL the annual reconciliation is skipped with a message", {
     fx <- .combiner_fixture(iso = "GHA")
     who <- .who_processed(fx, 40:52, c(10, 50, 400, 900, 700, 600, 500, 400, 300, 200, 300, 150, 108), 1, iso = "GHA")
     ai <- list(w = 14:33, cases = 46.85, deaths = 0.4, cw = 0.475, method = "fourier_country_k1")
     fx <- .combiner_fixture(who = who, ai = ai, iso = "GHA")
     msgs <- testthat::capture_messages(suppressWarnings(process_cholera_surveillance_data(fx$P, include_ai = TRUE)))
     expect_true(any(grepl("not reconciled against WHO annual totals", msgs)))
     out <- utils::read.csv(file.path(fx$P$DATA_CHOLERA_WEEKLY, "cholera_surveillance_weekly_combined.csv"))
     expect_equal(sum(out$cases[out$disaggregation_method %in% "fourier_country_k1"]), 20 * 46.85)
})

test_that("the gap rule no longer keeps an imputed excess beyond the observed weeks (the SSD 2024 pattern)", {
     # Observed 300 of an annual 320; fourier 1,000 before them. Capping the
     # removal at the observed cases kept 700; the gap rule keeps the 20 missing.
     fx <- .combiner_fixture(iso = "UGA")
     who <- .who_processed(fx, 40:42, c(100, 100, 100), 1, iso = "UGA")
     ai <- list(w = 15:34, cases = 50, deaths = 0.5, cw = 0.45, method = "fourier_country_k2")
     annual <- data.frame(iso_code = "UGA", year = 2023, cases_total = 320)
     fx <- .combiner_fixture(who = who, ai = ai, annual = annual, iso = "UGA")
     out <- .run_combiner(fx$P)$out
     f <- out[out$disaggregation_method %in% "fourier_country_k2", ]
     expect_equal(nrow(f), 20L)
     expect_equal(f$cases, rep(1, 20))
     expect_equal(sum(out$cases, na.rm = TRUE), 320)
})

test_that("rescaled imputed rows under half a case are emptied, not left as weighted zero weeks (CIV 2025)", {
     # 2023: observed 503 of an annual 510; 40 fourier weeks of 12.8 keep 7 cases,
     # 0.175 a week, so every one is emptied (none would survive the daily rounding).
     # 2024: observed 500 of 526; fourier 10 x 20 + 5 x 6 + 30 x 1 keep 26 -> 2.0,
     # 0.6 (kept: at least half a case) and 0.1 (emptied).
     fx <- .combiner_fixture(iso = "CIV")
     who <- .who_processed(fx, c(30:34, 80:84), c(100, 100, 100, 100, 103, rep(100, 5)), 1, iso = "CIV")
     ai <- list(w = c(1:29, 35:45, 53:62, 63:67, 68:77, 85:104),
                cases = c(rep(12.8, 40), rep(20, 10), rep(6, 5), rep(1, 30)), deaths = 0.1,
                cw = 0.5, method = "fourier_country_k1")
     annual <- data.frame(iso_code = "CIV", year = c(2023, 2024), cases_total = c(510, 526))
     fx <- .combiner_fixture(who = who, ai = ai, annual = annual, iso = "CIV")
     res <- .run_combiner(fx$P)
     out <- res$out
     yr <- as.integer(format(out$date_start + 3, "%Y"))
     expect_false(any(out$disaggregation_method[yr == 2023] %in% "fourier_country_k1"))
     expect_equal(sum(out$cases[yr == 2023], na.rm = TRUE), 503)
     f24 <- out[yr == 2024 & out$disaggregation_method %in% "fourier_country_k1", ]
     expect_equal(f24$cases, c(rep(2, 10), rep(0.6, 5)))
     adj <- res$adj
     expect_equal(sum(adj$rule == "imputed_residue_dropped"), 70L)
     expect_equal(sum(adj$rule == "imputed_scaled_annual_residual"), 15L)
     expect_true(all(is.na(adj$cases_after[adj$rule == "imputed_residue_dropped"])))
     # no imputed day reaches the daily fit target as a zero-case day
     daily <- utils::read.csv(file.path(fx$P$DATA_CHOLERA_DAILY, "cholera_surveillance_daily_combined.csv"))
     fd <- daily[daily$disaggregation_method %in% "fourier_country_k1", ]
     expect_equal(sum(fd$cases), 25)                                     # 0.6 rounds to one case
     expect_true(all(as.Date(fd$date) >= as.Date("2024-01-01")))
})

test_that("imputed rows are reconciled by the ISO year of their week, not the calendar year of its Monday", {
     # 2024-12-30 starts ISO week 2025-W01. 2024 is fully accounted (observed =
     # annual), 2025 has a large gap: a fourier row in that week belongs to 2025.
     fx <- .combiner_fixture(iso = "MWI")
     who <- .who_processed(fx, 60:64, rep(100, 5), 1, iso = "MWI")
     ai <- list(w = c(100, 105), cases = c(30, 40), deaths = 0.2, cw = 0.5, method = "fourier_country_k1")
     annual <- data.frame(iso_code = "MWI", year = c(2024, 2025), cases_total = c(500, 1000))
     fx <- .combiner_fixture(who = who, ai = ai, annual = annual, iso = "MWI")
     out <- .run_combiner(fx$P)$out
     expect_true(is.na(out$cases[out$date_start == as.Date("2024-11-25")]))
     expect_equal(out$cases[out$date_start == as.Date("2024-12-30")], 40)
})

test_that("without an AFRO annual row the WHO weekly year-to-date total is the account (SOM 2026)", {
     # Somalia (EMRO) has no AFRO annual row. Its three WHO weekly rows of 2026
     # (82 + 78 + 73 = 233) are the epidemiological update's 2026 total, which the
     # AI spread again over January-April.
     fx <- .combiner_fixture(iso = "SOM")
     who <- .who_processed(fx, c(120:124, 157:159), c(0, 0, 0, 0, 0, 82, 78, 73), 0, iso = "SOM")
     ai <- list(w = c(125:130, 159:173), cases = c(rep(25, 6), seq(10.8, 17.1, length.out = 15)), deaths = 0,
                cw = 0.7, method = "fourier_country_k1")
     annual <- data.frame(iso_code = "GHA", year = 2026, cases_total = 10)
     fx <- .combiner_fixture(who = who, ai = ai, annual = annual, iso = "SOM")
     res <- .run_combiner(fx$P)
     out <- res$out
     yr <- as.integer(format(out$date_start + 3, "%Y"))
     expect_false(any(out$disaggregation_method[yr == 2026] %in% "fourier_country_k1"))
     expect_equal(sum(out$cases[yr == 2026], na.rm = TRUE), 233)
     expect_true(all(grepl("WHO weekly year-to-date total \\(no AFRO annual row\\) 233",
                           res$adj$detail[res$adj$rule == "imputed_dropped_annual_accounted"])))
     # a year whose WHO weekly rows are all zero is not an account (no cases or no report)
     expect_equal(sum(out$cases[yr == 2025 & out$disaggregation_method %in% "fourier_country_k1"]), 150)
})

test_that("a year-to-date account leaves alone an imputed run that starts after WHO's last report (SOM 2026, v1.0.1)", {
     # WHO stops after its three 2026 weeks (233 cases); Africa CDC then reports a
     # week of 90 and multi-week totals the AI spreads over the following weeks.
     # The year-to-date total says nothing about those weeks, so they are kept.
     fx <- .combiner_fixture(iso = "SOM")
     who <- .who_processed(fx, 157:159, c(82, 78, 73), 0, iso = "SOM")
     ai <- list(w = c(160, 161:175), cases = c(90, rep(50, 15)), deaths = 0,
                cw = c(0.85, rep(0.75, 15)), method = c("observed", rep("fourier_country_k1", 15)))
     fx <- .combiner_fixture(who = who, ai = ai, annual = data.frame(iso_code = "GHA", year = 2026, cases_total = 10), iso = "SOM")
     res <- .run_combiner(fx$P)
     yr <- as.integer(format(res$out$date_start + 3, "%Y"))
     expect_equal(sum(res$out$cases[yr == 2026], na.rm = TRUE), 233 + 90 + 15 * 50)
     expect_equal(sum(res$out$disaggregation_method %in% "fourier_country_k1"), 15L)
     expect_false(any(res$adj$rule %in% c("imputed_dropped_annual_accounted", "imputed_scaled_annual_residual")))
})

test_that("a re-spread touching the WHO weeks is reconciled while a later run is kept (v1.0.1)", {
     # Run 1 (weeks 159-165) overlaps WHO's week 159: a re-spread of the period the
     # 233 cases account for, so it is emptied. Run 2 (weeks 167-170) starts after
     # WHO's last report and is kept, as is the observed AI week between them.
     fx <- .combiner_fixture(iso = "SOM")
     who <- .who_processed(fx, 157:159, c(82, 78, 73), 0, iso = "SOM")
     ai <- list(w = c(159:165, 166, 167:170), cases = c(rep(20, 7), 30, rep(40, 4)), deaths = 0,
                cw = c(rep(0.7, 7), 0.85, rep(0.75, 4)),
                method = c(rep("fourier_country_k1", 7), "observed", rep("fourier_country_k2", 4)))
     fx <- .combiner_fixture(who = who, ai = ai, annual = data.frame(iso_code = "GHA", year = 2026, cases_total = 10), iso = "SOM")
     res <- .run_combiner(fx$P)
     yr <- as.integer(format(res$out$date_start + 3, "%Y"))
     expect_false(any(res$out$disaggregation_method %in% "fourier_country_k1"))
     expect_equal(sum(res$out$disaggregation_method %in% "fourier_country_k2"), 4L)
     expect_equal(sum(res$out$cases[yr == 2026], na.rm = TRUE), 233 + 30 + 4 * 40)
     dropped <- res$adj[res$adj$rule == "imputed_dropped_annual_accounted", ]
     expect_equal(nrow(dropped), 6L)                                        # weeks 160-165; WHO keeps 159
     expect_true(all(grepl("observed weeks 233;", dropped$detail)))
})

test_that("a current-year annual total equal to the WHO weekly sum accounts only for WHO's weeks; a completed year's does not (ZWE 2026, v1.0.1)", {
     # 2024 is complete (WHO reports into 2026): its annual 500 = the weekly sum is the
     # official count, so a later 2024 run is still reconciled away. 2026 is the
     # current year: its annual 36 is the provisional sum of WHO's three weeks, so the
     # IFRC run after WHO's last report is kept.
     fx <- .combiner_fixture(iso = "ZWE")
     who <- .who_processed(fx, c(60:64, 157:159), c(rep(100, 5), 12, 12, 12), 0, iso = "ZWE")
     ai <- list(w = c(70:75, 175:180), cases = c(rep(20, 6), rep(11, 6)), deaths = 0, cw = 0.68,
                method = "fourier_country_k3")
     annual <- data.frame(iso_code = "ZWE", year = c(2024, 2026), cases_total = c(500, 36))
     fx <- .combiner_fixture(who = who, ai = ai, annual = annual, iso = "ZWE")
     res <- .run_combiner(fx$P)
     yr <- as.integer(format(res$out$date_start + 3, "%Y"))
     expect_equal(sum(res$out$cases[yr == 2024], na.rm = TRUE), 500)
     expect_equal(sum(res$out$cases[yr == 2026], na.rm = TRUE), 36 + 6 * 11)
     expect_equal(sum(res$adj$rule == "imputed_dropped_annual_accounted"), 6L)
     expect_true(all(grepl("^WHO annual total 500", res$adj$detail[res$adj$rule == "imputed_dropped_annual_accounted"])))
})

test_that("inferred zeros are imputed: a WHO week wins, an exhausted account keeps them, the daily file carries their weight (v1.0.1)", {
     # 2023: observed 300 = the annual 300, so the fourier weeks are emptied; the
     # inferred-zero weeks add no cases and are kept at their weight. In week 42
     # WHO reports, so its row wins over the inferred zero of that week.
     fx <- .combiner_fixture(iso = "UGA")
     who <- .who_processed(fx, 40:42, c(100, 100, 100), 1, iso = "UGA")
     ai <- list(w = c(10:14, 20:24, 42), cases = c(rep(30, 5), rep(0, 5), 0), deaths = 0,
                cw = c(rep(0.45, 5), rep(0.6, 6)),
                method = c(rep("fourier_country_k2", 5), rep("inferred_zero", 6)))
     fx <- .combiner_fixture(who = who, ai = ai, annual = data.frame(iso_code = "UGA", year = 2023, cases_total = 300), iso = "UGA")
     res <- .run_combiner(fx$P)
     out <- res$out
     iz <- out[out$disaggregation_method %in% "inferred_zero", ]
     expect_equal(nrow(iz), 5L)
     expect_equal(iz$cases, rep(0, 5))
     expect_equal(iz$confidence_weight, rep(0.6, 5))
     expect_equal(out$source[out$date_start == as.Date("2023-10-16")], "WHO")
     expect_false(any(out$disaggregation_method %in% "fourier_country_k2"))
     expect_equal(sum(out$cases, na.rm = TRUE), 300)
     daily <- utils::read.csv(file.path(fx$P$DATA_CHOLERA_DAILY, "cholera_surveillance_daily_combined.csv"))
     dz <- daily[daily$disaggregation_method %in% "inferred_zero", ]
     expect_equal(nrow(dz), 35L)
     expect_true(all(dz$cases == 0 & dz$confidence_weight == 0.6))
})

test_that("an inferred zero within four weeks of another row's positive count is dropped; others are kept (v1.0.2)", {
     # WHO reports cases in weeks 40-42. The AI inferred zeros of weeks 37-46 lie
     # within 28 days of them and are dropped; those of weeks 10-14 are kept.
     fx <- .combiner_fixture(iso = "KEN")
     who <- .who_processed(fx, 40:42, c(17, 40, 67), 0, iso = "KEN")
     ai <- list(w = c(10:14, 37:39, 43:46), cases = 0, deaths = 0, cw = 0.6, method = "inferred_zero")
     fx <- .combiner_fixture(who = who, ai = ai, iso = "KEN")
     res <- .run_combiner(fx$P)
     iz <- res$out[res$out$disaggregation_method %in% "inferred_zero", ]
     expect_equal(iz$date_start, as.Date("2023-01-02") + 7 * (10:14 - 1))
     g <- res$adj[res$adj$rule == "inferred_zero_near_positive", ]
     expect_equal(nrow(g), 7L)
     expect_true(all(is.na(res$out$cases[res$out$date_start %in% (as.Date("2023-01-02") + 7 * (c(37:39, 43:46) - 1))])))
})

test_that("an AI week repeating a WHO outbreak total is dropped (COG 2023 week 29)", {
     run <- function(ai_w, ai_cases) {
          fx <- .combiner_fixture(iso = "COG")
          who <- .who_processed(fx, 30:40, c(21, 0, 0, 0, 48, rep(0, 6)), c(5, rep(0, 10)), iso = "COG")
          ai <- list(w = ai_w, cases = ai_cases, deaths = NA, cw = 0.95, method = "observed")
          fx <- .combiner_fixture(who = who, ai = ai, iso = "COG")
          .run_combiner(fx$P)
     }
     res <- run(29, 63)
     expect_false(any(res$out$date_start == as.Date("2023-07-17") & !is.na(res$out$cases)))
     expect_equal(res$adj$rule[res$adj$cases_before %in% 63], "ai_cumulative_dropped")
     expect_match(res$adj$detail[res$adj$rule == "ai_cumulative_dropped"], "WHO weekly total for the year of 69")
     expect_equal(sum(res$out$cases, na.rm = TRUE), 69)
     # not within 15% of a WHO cumulative, or far from any WHO week with cases: kept
     res <- run(c(5, 29), c(63, 40))
     expect_equal(res$out$cases[res$out$date_start %in% as.Date(c("2023-01-30", "2023-07-17"))], c(63, 40))
     expect_false(any(res$adj$rule == "ai_cumulative_dropped"))
})

test_that("curated documented absences empty imputed weeks and flagged years stay unchanged (AGO, SSD, BFA)", {
     # AGO 2023: fourier weeks dropped, the observed JHU zero kept
     fx <- .combiner_fixture(iso = "AGO")
     ai <- list(w = 1:5, cases = c(1.2, 1.1, 1.1, 1.3, 1.6), deaths = 0.02, cw = 0.45, method = "fourier_country_k2")
     fx <- .combiner_fixture(jhu = list(w = 16, cases = 0, deaths = 0), ai = ai, iso = "AGO")
     res <- .run_combiner(fx$P)
     expect_true(all(is.na(res$out$cases[res$out$date_start < as.Date("2023-02-06")])))
     expect_equal(res$out$cases[res$out$date_start == as.Date("2023-04-17")], 0)
     expect_equal(sum(res$adj$rule == "imputed_dropped_curated"), 5L)
     expect_match(res$adj$detail[res$adj$rule == "imputed_dropped_curated"][1], "^curated AGO-2023-absence")
     # SSD: only weeks wholly inside 2023-05-17..2024-09-27 are dropped
     fx <- .combiner_fixture(iso = "SSD")
     ai <- list(w = 53:91, cases = 26, deaths = 0.3, cw = 0.45, method = "fourier_country_k2")
     fx <- .combiner_fixture(ai = ai, iso = "SSD")
     res <- .run_combiner(fx$P)
     kept <- res$out[!is.na(res$out$cases), ]
     expect_equal(kept$date_start, as.Date("2024-09-23"))              # the week of the first case
     expect_equal(sum(res$adj$rule == "imputed_dropped_curated"), 38L)
     # BFA 2025: kept and listed with unchanged values
     fx <- .combiner_fixture(iso = "BFA")
     ai <- list(w = 106:130, cases = 19.3, deaths = 0.8, cw = 0.49, method = "fourier_country_k1")
     fx <- .combiner_fixture(ai = ai, iso = "BFA")
     res <- .run_combiner(fx$P)
     expect_equal(sum(res$out$cases, na.rm = TRUE), 25 * 19.3)
     fl <- res$adj[res$adj$rule == "imputed_flagged_curated", ]
     expect_equal(nrow(fl), 25L)
     expect_equal(fl$cases_after, fl$cases_before)
})

test_that("raw WHO year-to-date dump + AI ramp end to end: no spike and no double count in the daily file", {
     tmp <- withr::local_tempdir()
     raw <- .raw_who_rows("SOUTH AFRICA", 2023, 35:52, c(1390, rep(0, 13), 1, 0, 0, 0), c(47, rep(0, 17)))
     utils::write.csv(raw, file.path(tmp, "cholera_country_weekly.csv"), row.names = FALSE)
     P <- list(DATA_SCRAPE_WHO_WEEKLY = tmp, DATA_WHO_WEEKLY = file.path(tmp, "who"),
               DATA_JHU_WEEKLY = file.path(tmp, "jhu"), DATA_SUPP_WEEKLY = file.path(tmp, "supp"),
               DATA_AI_WEEKLY = file.path(tmp, "ai"), DATA_CHOLERA_WEEKLY = file.path(tmp, "cw"),
               DATA_CHOLERA_DAILY = file.path(tmp, "cd"), DATA_WHO_ANNUAL = file.path(tmp, "annual"))
     for (d in P[-1]) dir.create(d, recursive = TRUE, showWarnings = FALSE)
     utils::write.csv(data.frame(iso_code = "ZAF", year = 2023, cases_total = 1391),
                      file.path(P$DATA_WHO_ANNUAL, "who_afro_annual.csv"), row.names = FALSE)
     ws <- as.Date("2023-01-02") + 7 * (0:51)
     ramp <- c(seq(70, 100, length.out = 8), seq(100, 3, length.out = 18), rep(0, 26))
     utils::write.csv(data.frame(iso_code = "ZAF", country = "South Africa", year = 2023,
                                 week = 1:52, cases = ramp, deaths = ramp / 30,
                                 date_start = ws, date_stop = ws + 6, month = as.integer(format(ws, "%m")),
                                 confidence_weight = 0.475, disaggregation_method = "fourier_country_k1"),
                      file.path(P$DATA_AI_WEEKLY, "cholera_country_weekly_processed.csv"), row.names = FALSE)
     suppressMessages(process_WHO_weekly_data(P))
     suppressWarnings(suppressMessages(process_cholera_surveillance_data(P, include_ai = TRUE)))
     daily <- utils::read.csv(file.path(P$DATA_CHOLERA_DAILY, "cholera_surveillance_daily_combined.csv"))
     expect_equal(sum(daily$cases, na.rm = TRUE), 1391)
     expect_equal(sum(daily$deaths, na.rm = TRUE), 47)
     expect_false(any(daily$disaggregation_method %in% "fourier_country_k1" & !is.na(daily$cases)))
     # the curated window: nothing in January, all of it 30 Jan - 6 Aug, zeros after
     dd <- as.Date(daily$date)
     expect_true(all(is.na(daily$cases[dd < as.Date("2023-01-30")])))
     expect_equal(sum(daily$cases[dd >= as.Date("2023-01-30") & dd <= as.Date("2023-08-06")]), 1390)
     expect_true(all(daily$cases[dd >= as.Date("2023-07-17") & dd <= as.Date("2023-09-03")] == 0))
     expect_true(all(daily$disaggregation_method[dd >= as.Date("2023-01-30") & dd <= as.Date("2023-09-03")] == "who_catchup_curated_shaped"))
     # on WHO's report-dated curve: the busiest days fall in the week of the 432-case peak
     expect_lte(max(daily$cases, na.rm = TRUE), 62)                    # 432 / 7, whole counts
     expect_equal(sum(daily$cases[dd >= as.Date("2023-05-22") & dd <= as.Date("2023-05-28")]), 432)
     expect_equal(sum(daily$deaths[dd >= as.Date("2023-05-15") & dd <= as.Date("2023-05-21")]), 10)
     expect_true(all(dd[which(daily$cases == max(daily$cases, na.rm = TRUE))] >= as.Date("2023-05-22") &
                     dd[which(daily$cases == max(daily$cases, na.rm = TRUE))] <= as.Date("2023-05-28")))
})

test_that(".spread_count splits a whole count into whole weeks that sum exactly", {
     f <- MOSAIC:::.spread_count
     expect_equal(f(47, n = 35), diff(c(0, floor(47 * (1:35) / 35 + 0.5))))
     expect_equal(sum(f(47, n = 35)), 47)
     expect_true(all(f(47, n = 35) %in% c(1, 2)))
     expect_equal(f(0, n = 4), rep(0, 4))
     expect_equal(f(NA, n = 3), rep(NA_real_, 3))
     expect_equal(f(10, c(1, 0, 3)), c(3, 0, 7))                  # proportional, halves up, zero weight -> 0
     expect_equal(f(10.5, c(1, 1)), c(5.25, 5.25))                # non-count total spread exactly
     # halves round up, so every week gets the floor or ceiling of its exact share
     # (rounding halves to even gave 0,0,2,0 and 0,2,1: an exact share of 1 got 2)
     expect_equal(f(2, c(1, 0, 2, 1)), c(1, 0, 1, 0))
     expect_equal(f(3, c(1, 2, 3)), c(1, 1, 1))
     set.seed(11)
     for (i in seq_len(300)) {
          w <- sample(0:5, sample(2:8, 1), replace = TRUE); if (sum(w) == 0) w[1] <- 1
          tot <- sample(0:12, 1)
          sp <- f(tot, w); ex <- tot * w / sum(w)
          expect_true(sum(sp) == tot && all(sp >= floor(ex) & sp <= ceiling(ex)))
     }
})

test_that("epidemic-peak detection keeps WHO multi-week reports as observed and blanks only imputed weeks", {
     # Blanking the GHA 2024 weeks 42-45 window (a reported 1,506 cases spread over
     # four weeks) to zero carved a trough into the outbreak and lost its peak.
     m <- c(NA, "observed", "documented_zero", "who_catchup_uniform", "who_catchup_shaped",
            "fourier_country_k1", "fourier_regional_East Africa_k5", "assumed_zero")
     expect_equal(MOSAIC:::.epidemic_peaks_imputed_day(m), c(rep(FALSE, 5), rep(TRUE, 3)))
     expect_equal(MOSAIC:::.surveillance_tier(m), c(1L, 1L, 1L, 2L, 2L, 3L, 3L, 3L))
     expect_equal(MOSAIC:::.surveillance_tier(c(NA, NA)), c(1L, 1L))      # all-NA column read as logical
})
