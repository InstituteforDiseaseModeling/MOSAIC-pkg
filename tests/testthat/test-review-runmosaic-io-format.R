# =============================================================================
# test-review-runmosaic-io-format.R
#
# io$format = "csv" was validated and documented, but nothing read it: every
# shard and combined file was parquet. It is now coerced to parquet with a
# warning, and the debug preset no longer asks for it.
# =============================================================================

test_that("io$format = 'csv' is coerced to parquet with a warning", {
  expect_warning(
    ctl <- MOSAIC:::.mosaic_validate_and_merge_control(list(io = list(format = "csv"))),
    "not supported")
  expect_identical(ctl$io$format, "parquet")
})

test_that("the debug io preset validates without a warning", {
  expect_no_warning(
    ctl <- MOSAIC:::.mosaic_validate_and_merge_control(list(io = mosaic_io_presets("debug"))))
  expect_identical(ctl$io$format, "parquet")
  expect_identical(ctl$io$compression, "none")
})

test_that("an unknown io$format is an error", {
  expect_error(MOSAIC:::.mosaic_validate_and_merge_control(list(io = list(format = "feather"))),
               "must be 'parquet'")
})
