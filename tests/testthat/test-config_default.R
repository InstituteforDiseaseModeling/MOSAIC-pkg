# Tests for the epidemic_peaks field shipped in config_default and the
# matching validation block in make_LASER_config(). Supports the Python
# likelihood port (laser-cholera#47) by guaranteeing the field is present
# in the shipped config so worker-side scoring has the peaks it needs.

# Minimal call shim that injects a custom epidemic_peaks into config_default
# and routes through make_LASER_config so validation runs. Strips tracking
# fields (zeta_ratio, decay_days_spread) that live on config_default but are
# not accepted by make_LASER_config's signature.
.make_minimal_then_call <- function(epidemic_peaks) {
  args <- MOSAIC::config_default
  args$metadata <- NULL
  args$zeta_ratio <- NULL
  args$decay_days_spread <- NULL
  args$output_file_path <- NULL
  args$epidemic_peaks <- epidemic_peaks
  do.call(MOSAIC::make_LASER_config, args)
}

test_that("config_default ships epidemic_peaks as a 2-col character data.frame", {
  ep <- MOSAIC::config_default$epidemic_peaks
  expect_s3_class(ep, "data.frame")
  expect_named(ep, c("iso_code", "peak_date"))
  expect_type(ep$iso_code, "character")
  expect_type(ep$peak_date, "character")
  expect_gt(nrow(ep), 0)

  # peak_date values parse as ISO yyyy-mm-dd
  parsed <- suppressWarnings(as.Date(ep$peak_date))
  expect_false(any(is.na(parsed)))
})

test_that("config_default metadata version is bumped to 3.2+", {
  v <- MOSAIC::config_default$metadata$version
  expect_true(utils::compareVersion(v, "3.2") >= 0,
              info = sprintf("got version %s", v))
})

test_that("make_LASER_config validation: malformed epidemic_peaks errors", {
  # Missing required columns
  bad <- data.frame(iso_code = "MOZ", wrong_col = "2024-01-01",
                    stringsAsFactors = FALSE)
  expect_error(
    .make_minimal_then_call(epidemic_peaks = bad),
    "epidemic_peaks is missing required column"
  )

  # Not a data.frame
  expect_error(
    .make_minimal_then_call(epidemic_peaks = list(iso_code = "MOZ",
                                                  peak_date = "2024-01-01")),
    "epidemic_peaks must be a data\\.frame"
  )

  # Unparseable date
  bad_dates <- data.frame(iso_code = "MOZ", peak_date = "not-a-date",
                          stringsAsFactors = FALSE)
  expect_error(
    .make_minimal_then_call(epidemic_peaks = bad_dates),
    "unparseable date"
  )
})

test_that("make_LASER_config validation: unknown iso_code is a hard error (v0.32.0+)", {
  unknown <- data.frame(iso_code = "ZZZ", peak_date = "2024-01-01",
                        stringsAsFactors = FALSE)
  # v0.32.0 promoted the prior warning to a hard error: laser-cholera v0.13+
  # asserts every iso_code in epidemic_peaks appears in location_name, so
  # MOSAIC fails fast at config construction instead of letting the worker
  # crash. See R/make_LASER_config.R::~918 and NEWS v0.32.0.
  expect_error(
    .make_minimal_then_call(epidemic_peaks = unknown),
    "epidemic_peaks contains iso_code\\(s\\) not in location_name: ZZZ"
  )
})

