# =============================================================================
# test-review-runmosaic-trajectory-map.R
#
# When the displayed ensemble is the optimized (re-sorted, subset) rebuild and
# its seeds cannot be mapped to the candidate scratch, the trajectory reduce
# used to fall back to positional candidate keys 1..n, pairing channel panels
# from the wrong members with the optimized arrays. It is now skipped.
# =============================================================================

test_that("the candidate ensemble uses positional scratch keys", {
  expect_identical(MOSAIC:::.mosaic_trajectory_member_map(FALSE, c(9, 8), c(8, 9, 7), 2L), 1:2)
})

test_that("the optimized ensemble maps members to scratch keys by seed", {
  expect_identical(MOSAIC:::.mosaic_trajectory_member_map(TRUE, c(7, 8), c(8, 9, 7), 2L),
                   c(3L, 1L))
})

test_that("an unmappable optimized ensemble yields NULL, not positional keys", {
  f <- MOSAIC:::.mosaic_trajectory_member_map
  expect_null(f(TRUE, NULL, c(8, 9, 7), 2L))
  expect_null(f(TRUE, c(7, 8), NULL, 2L))
  expect_null(f(TRUE, c(7, 8), c(8, 8, 7), 2L))     # duplicated candidate seeds
  expect_null(f(TRUE, c(7, 42), c(8, 9, 7), 2L))    # unmatched seed
  expect_null(f(TRUE, c(7, NA), c(8, 9, 7), 2L))
  expect_null(f(TRUE, c(7, 8, 9), c(8, 9, 7), 2L))  # length mismatch
})
