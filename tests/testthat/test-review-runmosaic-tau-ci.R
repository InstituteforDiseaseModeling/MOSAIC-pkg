# =============================================================================
# test-review-runmosaic-tau-ci.R
#
# 1_inputs/mobility_tau_ci.csv was gated on isTRUE(control$io), which is always
# FALSE because control$io is a list, so the artifact was never written and
# departure_tau.png never had interval bars. The writer is now unconditional.
# =============================================================================

test_that("the tau CI artifact is written from the upstream fit in config order", {
  src <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(iso3 = c("AGO", "BDI", "MOZ"), mean = c(2, 1.6, 3) * 1e-5,
                              Q2.5 = c(1.9, 1.4, 2.8) * 1e-5, Q97.5 = c(2.3, 1.9, 3.2) * 1e-5),
                   src)
  dir_in <- withr::local_tempdir()
  out <- MOSAIC:::.mosaic_write_tau_ci(src, c("MOZ", "AGO", "ZZZ"), dir_in)
  expect_identical(out, file.path(dir_in, "mobility_tau_ci.csv"))
  ci <- utils::read.csv(out, stringsAsFactors = FALSE)
  expect_identical(ci$location, c("MOZ", "AGO", "ZZZ"))
  expect_equal(ci$lower, c(2.8e-5, 1.9e-5, NA))
  expect_equal(ci$upper, c(3.2e-5, 2.3e-5, NA))
  expect_false(file.exists(paste0(out, ".tmp")))
})

test_that("a missing or malformed upstream fit writes nothing", {
  dir_in <- withr::local_tempdir()
  expect_null(MOSAIC:::.mosaic_write_tau_ci(file.path(dir_in, "nope.csv"), "MOZ", dir_in))
  bad <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(iso3 = "MOZ", mean = 1), bad)
  expect_null(MOSAIC:::.mosaic_write_tau_ci(bad, "MOZ", dir_in))
  expect_false(file.exists(file.path(dir_in, "mobility_tau_ci.csv")))
})

test_that("run_MOSAIC calls the tau CI writer without an io gate", {
  body_txt <- paste(deparse(body(MOSAIC::run_MOSAIC)), collapse = "\n")
  expect_false(grepl("isTRUE(control$io)", body_txt, fixed = TRUE))
  expect_true(grepl(".mosaic_write_tau_ci(", body_txt, fixed = TRUE))
})
