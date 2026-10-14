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
  # The tier search moved to .mosaic_tier_best_subset() in 1.3.0 (shared with
  # the auto-mode stopping rule); run_MOSAIC() calls it.
  b <- paste(deparse(MOSAIC:::.mosaic_tier_best_subset), collapse = "\n")
  expect_false(grepl("of retained)", b, fixed = TRUE))
  expect_true(grepl("of all draws)", b, fixed = TRUE))
  expect_true(grepl(".mosaic_tier_best_subset(results, control, log = TRUE)",
                    gsub("\\s+", " ", .rm_body()), fixed = TRUE))
})
