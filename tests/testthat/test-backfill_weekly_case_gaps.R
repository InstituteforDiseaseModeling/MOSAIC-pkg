test_that("short interior gaps are filled, long gaps and series ends are not", {
     df <- data.frame(
          iso_code = "AAA",
          date     = as.Date("2025-01-01") + 7 * (0:9),
          #             1   2   3    4    5   6   7   8    9   10
          cases    = c(10, 20, NA,  40, 100, NA, NA, NA,  80, 90),
          stringsAsFactors = FALSE)

     out <- backfill_weekly_case_gaps(df, max_interp_weeks = 2L, verbose = FALSE)

     # 1-week interior gap (between 20 and 40) -> linear midpoint 30, flagged
     expect_equal(out$cases[3], 30)
     expect_true(out$cases_interpolated[3])

     # 3-week interior gap (rows 6:8) exceeds max_interp_weeks=2 -> untouched
     expect_true(all(is.na(out$cases[6:8])))
     expect_false(any(out$cases_interpolated[6:8]))

     # reported weeks are untouched and never flagged
     expect_equal(out$cases[c(1, 2, 4, 5, 9, 10)], c(10, 20, 40, 100, 80, 90))
     expect_false(any(out$cases_interpolated[c(1, 2, 4, 5, 9, 10)]))
})

test_that("leading and trailing NA are never extrapolated", {
     df <- data.frame(
          iso_code = "AAA",
          date     = as.Date("2025-01-01") + 7 * (0:5),
          cases    = c(NA, 10, 20, NA, 40, NA),
          stringsAsFactors = FALSE)

     out <- backfill_weekly_case_gaps(df, max_interp_weeks = 2L, verbose = FALSE)

     expect_true(is.na(out$cases[1]))   # leading NA: no left anchor
     expect_true(is.na(out$cases[6]))   # trailing NA: no right anchor
     expect_equal(out$cases[4], 30)     # interior 1-week gap filled
     expect_equal(sum(out$cases_interpolated), 1L)
})

test_that("groups are handled independently and input row order is preserved", {
     # interleaved AAA/BBB rows so a per-group sort would reorder if not restored
     df <- data.frame(
          iso_code = c("AAA", "BBB", "AAA", "BBB", "AAA", "BBB"),
          date     = rep(as.Date("2025-01-01") + 7 * (0:2), each = 2),
          cases    = c(10, 5, NA, NA, 30, 25),
          stringsAsFactors = FALSE)

     out <- backfill_weekly_case_gaps(df, max_interp_weeks = 2L, verbose = FALSE)

     expect_identical(out$iso_code, df$iso_code)   # original order preserved
     expect_equal(out$cases[3], 20)                # AAA: between 10 and 30
     expect_equal(out$cases[4], 15)                # BBB: between 5 and 25
})

test_that("the holiday-during-outbreak case is repaired (regression for the MOZ artifact)", {
     # mirrors MOZ 2025-W52..2026-W02: a missing New-Year week inside a rising outbreak
     df <- data.frame(
          iso_code = "MOZ",
          date     = as.Date(c("2025-12-21", "2025-12-28", "2026-01-04", "2026-01-11")),
          cases    = c(257, 414, NA, 507),  # W51, W52, W01(missing), W02
          stringsAsFactors = FALSE)

     out <- backfill_weekly_case_gaps(df, max_interp_weeks = 2L, verbose = FALSE)

     expect_equal(out$cases[3], 460)                      # round(460.5)=460 (banker's), not 0
     expect_true(out$cases_interpolated[3])
})

test_that("duplicate / unevenly-spaced dates do not error and interpolate by date", {
     df <- data.frame(
          iso_code = "AAA",
          date     = as.Date(c("2025-01-01", "2025-01-01", "2025-01-08", "2025-01-22")),
          cases    = c(10, 10, NA, 40),
          stringsAsFactors = FALSE)
     out <- backfill_weekly_case_gaps(df, max_interp_weeks = 2L, verbose = FALSE)
     # date-weighted: 2025-01-08 is day 7 of the [day 0, day 21] span -> 10 + 30*(7/21) = 20
     expect_equal(out$cases[3], 20)
})

test_that("reported anchors (incl. fractional) are never modified or flagged (D1)", {
     df <- data.frame(
          iso_code = "AAA",
          date     = as.Date("2025-01-01") + 7 * (0:4),
          cases    = c(33.3, 44.7, NA, 60.2, 70.9),   # all anchors fractional
          stringsAsFactors = FALSE)
     out <- backfill_weekly_case_gaps(df, max_interp_weeks = 2L, verbose = FALSE)
     # anchors returned bit-identical (round_counts must NOT touch them)
     expect_equal(out$cases[c(1, 2, 4, 5)], c(33.3, 44.7, 60.2, 70.9))
     expect_false(any(out$cases_interpolated[c(1, 2, 4, 5)]))
     # only the gap is filled (rounded) + flagged
     expect_equal(out$cases[3], round((44.7 + 60.2) / 2))
     expect_true(out$cases_interpolated[3])
})

test_that("interpolation is date-weighted, not index-weighted, at uneven spacing (D2)", {
     # gap row is 7 days after the left anchor but 14 days before the right anchor
     df <- data.frame(
          iso_code = "AAA",
          date     = as.Date(c("2025-01-06", "2025-01-13", "2025-01-27")),
          cases    = c(10, NA, 40),
          stringsAsFactors = FALSE)
     out <- backfill_weekly_case_gaps(df, max_interp_weeks = 2L, verbose = FALSE)
     expect_equal(out$cases[2], 20)   # date-correct: 10 + 30*(7/21); index-based would give 25
})

test_that("gaps shouldered by a reported zero/negative are not filled (D3)", {
     # declining tail to zero, and rising from zero: min_anchor=0 leaves both NA
     z1 <- backfill_weekly_case_gaps(
          data.frame(iso_code = "A", date = as.Date("2025-01-01") + 7 * (0:2),
                     cases = c(19, NA, 0)), verbose = FALSE)
     z2 <- backfill_weekly_case_gaps(
          data.frame(iso_code = "A", date = as.Date("2025-01-01") + 7 * (0:2),
                     cases = c(0, NA, 30)), verbose = FALSE)
     expect_true(is.na(z1$cases[2]) && !z1$cases_interpolated[2])
     expect_true(is.na(z2$cases[2]) && !z2$cases_interpolated[2])
     # a negative anchor is preserved (clamp is fill-only) and its gap is not filled
     neg <- backfill_weekly_case_gaps(
          data.frame(iso_code = "A", date = as.Date("2025-01-01") + 7 * (0:2),
                     cases = c(-5, NA, 10)), verbose = FALSE)
     expect_equal(neg$cases[1], -5)
     expect_true(is.na(neg$cases[2]))
     # min_anchor is tunable: raising it leaves a low-but-positive-bounded gap unfilled
     lo <- backfill_weekly_case_gaps(
          data.frame(iso_code = "A", date = as.Date("2025-01-01") + 7 * (0:2),
                     cases = c(3, NA, 4)), min_anchor = 5, verbose = FALSE)
     expect_true(is.na(lo$cases[2]))
})

test_that("a fill that rounds to 0 is left NA and not flagged", {
     out <- backfill_weekly_case_gaps(
          data.frame(iso_code = "A", date = as.Date("2025-01-01") + 7 * (0:2),
                     cases = c(0.3, NA, 0.4)), verbose = FALSE)
     expect_true(is.na(out$cases[2]))
     expect_false(out$cases_interpolated[2])
})

test_that("phantom-duplicate (group,date) rows are not fabricated", {
     # an NA row sharing its (iso, date) with a reported row must stay NA
     df <- data.frame(
          iso_code = "MOZ",
          date     = as.Date(c("2024-01-01", "2024-01-01", "2024-01-08")),
          cases    = c(608, NA, 700),
          stringsAsFactors = FALSE)
     out <- backfill_weekly_case_gaps(df, max_interp_weeks = 2L, verbose = FALSE)
     expect_true(is.na(out$cases[2]) && !out$cases_interpolated[2])
     expect_equal(out$cases[c(1, 3)], c(608, 700))
})

test_that("non-numeric cases coerce with a warning, not silently", {
     df <- data.frame(
          iso_code = "A",
          date     = as.Date("2025-01-01") + 7 * (0:4),
          cases    = c("10", "20", "unknown", "40", "50"),
          stringsAsFactors = FALSE)
     expect_warning(backfill_weekly_case_gaps(df, max_interp_weeks = 2L, verbose = FALSE),
                    "non-numeric")
})

test_that("max_interp_weeks must be a positive integer", {
     df <- data.frame(iso_code = "AAA", date = as.Date("2025-01-01"), cases = 1)
     expect_error(backfill_weekly_case_gaps(df, max_interp_weeks = 0))
     expect_error(backfill_weekly_case_gaps(df, max_interp_weeks = NA))
})
