# =============================================================================
# test-review-runmosaic-medoid.R
#
# The medoid distance must use every location and only the scored time steps.
# It used to read location 1 only, so in a multi-location run the medoid was
# the member closest to the first-listed country.
# =============================================================================

.medoid_fixture <- function() {
  n_loc <- 3L; n_t <- 40L; n_p <- 3L; n_s <- 2L
  base <- matrix(rep(c(10, 100, 1000), n_t) * rep(seq_len(n_t), each = n_loc),
                 nrow = n_loc)
  arr <- array(NA_real_, c(n_loc, n_t, n_p, n_s))
  # Member 1 matches location 1 perfectly but is far off at locations 2 and 3.
  m1 <- base; m1[2:3, ] <- m1[2:3, ] * 20
  # Member 2 is slightly off at every location (the true medoid).
  m2 <- base * 1.1
  # Member 3 is far off everywhere.
  m3 <- base * 30
  for (s in seq_len(n_s)) {
    arr[, , 1, s] <- m1; arr[, , 2, s] <- m2; arr[, , 3, s] <- m3
  }
  list(arr = arr, central = base, n_t = n_t)
}

test_that("the medoid distance pools all locations", {
  fx <- .medoid_fixture()
  spec <- list(cases_warmup = 0L, deaths_final = FALSE)
  d <- MOSAIC:::.mosaic_medoid_distances(fx$arr, fx$central, spec)
  expect_length(d, 3L)
  expect_identical(which.min(d), 2L)
  # Location-1-only distance (the old rule) would have picked member 1.
  d_loc1 <- vapply(1:3, function(i)
    mean(abs(log(fx$arr[1, , i, 1] + 1) - log(fx$central[1, ] + 1))), numeric(1))
  expect_identical(which.min(d_loc1), 1L)
})

test_that("the medoid distance ignores the unscored head", {
  fx <- .medoid_fixture()
  arr <- fx$arr
  # Make the true medoid (member 2) wildly wrong only inside the burn-in head.
  arr[, 1:10, 2, ] <- arr[, 1:10, 2, ] * 1e4
  spec_scored <- list(cases_warmup = 0L, deaths_final = FALSE, score_idx_cases = 11L)
  d <- MOSAIC:::.mosaic_medoid_distances(arr, fx$central, spec_scored)
  expect_identical(which.min(d), 2L)
  d_all <- MOSAIC:::.mosaic_medoid_distances(arr, fx$central,
                                             list(cases_warmup = 0L, deaths_final = FALSE))
  expect_false(which.min(d_all) == 2L)
})

test_that("a single-location ensemble gives the per-location log-MAE", {
  fx <- .medoid_fixture()
  arr1 <- fx$arr[1, , , , drop = FALSE]
  cen1 <- fx$central[1, , drop = FALSE]
  spec <- list(cases_warmup = 2L, deaths_final = FALSE)
  d <- MOSAIC:::.mosaic_medoid_distances(arr1, cen1, spec)
  ref <- vapply(1:3, function(i)
    mean(abs(log(arr1[1, -(1:2), i, 1] + 1) - log(cen1[1, -(1:2)] + 1))), numeric(1))
  expect_equal(d, ref)
})
