# =============================================================================
# test-review-runmosaic-get-paths.R
#
# get_paths()' documented @return list must name every path it returns, with
# the directory the code actually uses.
# =============================================================================

test_that("every returned path is documented in ?get_paths", {
  p <- get_paths("/root")
  rd_file <- testthat::test_path("..", "..", "man", "get_paths.Rd")
  skip_if_not(file.exists(rd_file), "man/get_paths.Rd not available (installed package)")
  rd <- paste(readLines(rd_file), collapse = "\n")
  missing <- names(p)[!vapply(names(p), function(n) grepl(n, rd, fixed = TRUE), logical(1))]
  expect_identical(missing, character(0))
  expect_identical(p$DATA_RAW, file.path("/root", "MOSAIC-data/raw"))
  expect_identical(p$MODEL_INPUT, file.path("/root", "MOSAIC-pkg/model/input"))
  expect_identical(p$DATA_SUPP_WEEKLY, file.path("/root", "MOSAIC-data/processed/SUPP/daily"))
})
