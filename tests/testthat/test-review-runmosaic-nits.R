# =============================================================================
# test-review-runmosaic-nits.R
#
# Small run_MOSAIC() hygiene items from the deep review.
# =============================================================================

.rm_body <- function() paste(deparse(body(MOSAIC::run_MOSAIC)), collapse = "\n")

test_that("the pure-R calibration path does not run a Python environment check", {
  b <- .rm_body()
  expect_false(grepl("check_python_env(", b, fixed = TRUE))
  expect_false(grepl("PYTHONWARNINGS", b, fixed = TRUE))
})

test_that("no combine pass for the retired 'stochastic' prediction type", {
  expect_false(grepl("\"stochastic\")", .rm_body(), fixed = TRUE))
})

test_that("the tier log labels its percentage as a share of all draws", {
  b <- .rm_body()
  expect_false(grepl("of retained)", b, fixed = TRUE))
  expect_true(grepl("of all draws)", b, fixed = TRUE))
})
