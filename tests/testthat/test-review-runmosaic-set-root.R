# =============================================================================
# test-review-runmosaic-set-root.R
#
# set_root_directory() is documented to return the root; it returned the NULL
# from its final message().
# =============================================================================

test_that("set_root_directory() returns the root invisibly and sets the option", {
  old <- getOption("root_directory")
  on.exit(options(root_directory = old), add = TRUE)
  d <- withr::local_tempdir()
  expect_invisible(suppressMessages(set_root_directory(d)))
  got <- suppressMessages(set_root_directory(d))
  expect_identical(got, d)
  expect_identical(getOption("root_directory"), d)
  expect_error(suppressMessages(set_root_directory(file.path(d, "missing"))), "Cannot find root")
})
