test_that(".drop_filled_prediction_tail drops trailing rows past each location's last genuine date", {
     drop_fn <- MOSAIC:::.drop_filled_prediction_tail

     df <- data.frame(
          iso_code = c(rep("NGA", 5), rep("CIV", 5)),
          date = as.Date(c("2026-10-27", "2026-10-28", "2026-10-29", "2026-10-30", "2026-10-31",
                           "2026-07-01", "2026-07-02", "2026-07-03", "2026-07-04", "2026-07-05")),
          psi = seq_len(10) / 10,
          stringsAsFactors = FALSE
     )
     genuine_last <- data.frame(
          iso_code = c("NGA", "CIV"),
          last_genuine_date = as.Date(c("2026-10-29", "2026-07-02")),
          stringsAsFactors = FALSE
     )

     out <- drop_fn(df, genuine_last)

     # NGA: keep through 2026-10-29 (3 of 5); CIV: keep through 2026-07-02 (2 of 5)
     expect_equal(nrow(out), 5L)
     expect_equal(max(out$date[out$iso_code == "NGA"]), as.Date("2026-10-29"))
     expect_equal(max(out$date[out$iso_code == "CIV"]), as.Date("2026-07-02"))
     # Columns preserved
     expect_named(out, c("iso_code", "date", "psi"))
     # No row on/before the cutoff is dropped
     expect_true(as.Date("2026-10-27") %in% out$date)
})

test_that(".drop_filled_prediction_tail passes through locations absent from genuine_last", {
     drop_fn <- MOSAIC:::.drop_filled_prediction_tail

     df <- data.frame(
          iso_code = c("NGA", "NGA", "ZZZ"),
          date = as.Date(c("2026-10-28", "2026-10-29", "2026-10-30")),
          psi = c(0.1, 0.2, 0.3),
          stringsAsFactors = FALSE
     )
     genuine_last <- data.frame(
          iso_code = "NGA",
          last_genuine_date = as.Date("2026-10-28"),
          stringsAsFactors = FALSE
     )

     out <- drop_fn(df, genuine_last)

     # NGA truncated to 2026-10-28; ZZZ (unknown) kept unchanged
     expect_equal(sort(as.character(out$date)), c("2026-10-28", "2026-10-30"))
     expect_true("ZZZ" %in% out$iso_code)
})

test_that(".drop_filled_prediction_tail returns input unchanged when key columns are absent", {
     drop_fn <- MOSAIC:::.drop_filled_prediction_tail

     df <- data.frame(a = 1:3, b = 4:6)
     genuine_last <- data.frame(iso_code = "NGA", last_genuine_date = as.Date("2026-01-01"))
     expect_identical(drop_fn(df, genuine_last), df)
})

# ----------------------------------------------------------------------------
# lstm_v2 regression: the daily na.locf carry-forward tail is NON-NA, so the
# drop MUST key on the explicit per-country genuine date (NOT is.na(pred)).
# This is the exact failure mode the lstm_v2 path had: .psi_weekly_to_daily_smooth
# forward-fills the daily grid out to pred_date_stop with the last genuine value,
# producing a flat constant psi tail that an NA-keyed drop would never catch.
# ----------------------------------------------------------------------------
test_that("lstm_v2 na.locf carry-forward tail (non-NA) is dropped via genuine date", {
     drop_fn <- MOSAIC:::.drop_filled_prediction_tail
     smooth_fn <- MOSAIC:::.psi_weekly_to_daily_smooth

     # Genuine weekly predictions for NGA ending ~2026-10-28, then the daily grid
     # is extended (and forward-filled) out to pred_date_stop 2027-02-04.
     wk_dates <- seq(as.Date("2026-01-07"), as.Date("2026-10-29"), by = "week")
     last_genuine <- max(wk_dates)
     wk_pred  <- 0.2 + 0.3 * sin(seq_along(wk_dates) / 5)
     pred_stop <- as.Date("2027-02-04")

     daily <- smooth_fn(weekly_dates = wk_dates, weekly_probs = wk_pred,
                        day_start = min(wk_dates), day_stop = pred_stop)
     daily$iso_code <- "NGA"

     # The carry-forward tail is NON-NA and FLAT (the bug).
     tail_idx <- daily$date > last_genuine
     expect_true(any(tail_idx))
     expect_false(anyNA(daily$pred[tail_idx]))               # filled, not NA
     expect_equal(length(unique(daily$pred[tail_idx])), 1L)  # flat constant

     # genuine_last_pred = the last GENUINE weekly date, captured pre-fill.
     genuine_last <- data.frame(iso_code = "NGA",
                                last_genuine_date = last_genuine,
                                stringsAsFactors = FALSE)

     out <- drop_fn(daily, genuine_last)

     # The flat non-NA tail is gone; the series ends at the genuine horizon.
     expect_equal(max(out$date), last_genuine)
     expect_lt(nrow(out), nrow(daily))
     # The retained head still varies (genuine signal preserved, not over-clipped).
     expect_gt(stats::sd(out$pred), 0)
})
