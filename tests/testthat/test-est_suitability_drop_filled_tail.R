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
