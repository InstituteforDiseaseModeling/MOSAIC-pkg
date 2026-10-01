# Regression tests for multi-week surveillance reports and cross-source double
# counting (v0.100.1 production-suite findings):
#   - process_WHO_weekly_data() spreads WHO catch-up / year-to-date reports
#     (ZAF 2023 week 35, NGA 2023 batch reports) over the weeks they cover;
#   - process_cholera_surveillance_data() keeps WHO windows whole, drops AI
#     copies of the dashboard and AI aggregates mislabelled as a week (NGA 2023
#     week 21), and removes imputed (fourier) mass that duplicates observed weeks
#     against the WHO annual total (ZAF 2023 ramp, GHA 2024).

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

test_that("a year-to-date first report followed by silence is spread back to week 1 (ZAF 2023)", {
     # WHO's first ZAF 2023 row is week 35: 1,390 cases / 47 deaths, then zeros
     # (and one sporadic case in week 49). The report is the whole Feb-Jul outbreak.
     raw <- .raw_who_rows("SOUTH AFRICA", 2023, 35:52,
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

test_that("imputed rows lose the mass that duplicates observed weeks of the same year (GHA 2024)", {
     # 2023 (weeks 1-52): fourier rows Apr-Aug beside a WHO-reported outbreak whose
     # weekly total equals the WHO annual total -> fourier emptied.
     # 2024 (weeks 53-104): observed 300, fourier 900, annual 1,000 -> the excess
     # (200) is removed, the 700 not explained by double counting is kept.
     # 2025 (weeks 105-156): no observed week at all -> fourier untouched even
     # though it exceeds the annual total (a disagreement between annual totals).
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
     expect_equal(sum(f25$cases), 800)
     expect_equal(sum(res$adj$rule == "imputed_dropped_annual_accounted"), 20L)
     expect_equal(sum(res$adj$rule == "imputed_scaled_annual_residual"), 10L)
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
     expect_lte(max(daily$cases, na.rm = TRUE), 6)
     expect_false(any(daily$disaggregation_method %in% "fourier_country_k1" & !is.na(daily$cases)))
})

test_that(".spread_count splits a whole count into whole weeks that sum exactly", {
     f <- MOSAIC:::.spread_count
     expect_equal(f(47, n = 35), diff(c(0, round(47 * (1:35) / 35))))
     expect_equal(sum(f(47, n = 35)), 47)
     expect_true(all(f(47, n = 35) %in% c(1, 2)))
     expect_equal(f(0, n = 4), rep(0, 4))
     expect_equal(f(NA, n = 3), rep(NA_real_, 3))
     expect_equal(f(10, c(1, 0, 3)), c(2, 0, 8))                  # proportional, zero weight -> 0
     expect_equal(f(10.5, c(1, 1)), c(5.25, 5.25))                # non-count total spread exactly
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
