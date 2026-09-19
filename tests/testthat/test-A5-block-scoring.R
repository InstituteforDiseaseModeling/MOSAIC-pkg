# A5: daily cells are scored as independent, but epidemic counts are strongly
# autocorrelated (tau ~ 92; VIF 2.4 -> 252 from daily to annual blocks). Pooling
# to fixed-day blocks removes most of that false precision.
withr::defer(Sys.unsetenv("INFLAB_BLOCK_DAYS"), teardown_env())

test_that("the default is inert: unset == 1 == stock behaviour", {
  set.seed(1); nt <- 28
  oc <- matrix(rpois(nt, 20), 1); ec <- matrix(rpois(nt, 22), 1)
  od <- matrix(rpois(nt, 1), 1);  ed <- matrix(rpois(nt, 2), 1)
  Sys.unsetenv("INFLAB_BLOCK_DAYS"); a <- calc_model_likelihood(oc, ec, od, ed)
  Sys.setenv(INFLAB_BLOCK_DAYS = "1"); b <- calc_model_likelihood(oc, ec, od, ed)
  expect_equal(a, b, tolerance = 0)
  Sys.unsetenv("INFLAB_BLOCK_DAYS")
})

test_that("a block straddling a zero-weight boundary is zeroed, not averaged", {
  # Without this the training likelihood would absorb held-out days.
  w <- matrix(1, 1, 28); w[1, 15:28] <- 0
  p <- MOSAIC:::.inflab_pool(w, 7L, "wmean")
  expect_equal(as.numeric(p), c(1, 1, 0, 0))

  # partial mask inside one block -> that whole block is dropped
  w2 <- matrix(1, 1, 14); w2[1, 10] <- 0
  expect_equal(as.numeric(MOSAIC:::.inflab_pool(w2, 7L, "wmean")), c(1, 0))
})

test_that("counts are summed and totals preserved", {
  x <- matrix(1:28, 1)
  p <- MOSAIC:::.inflab_pool(x, 7L, "sum")
  expect_equal(sum(p), sum(x))
  expect_length(as.numeric(p), 4L)
})

test_that("pooling propagates to the likelihood", {
  set.seed(2); nt <- 28
  oc <- matrix(rpois(nt, 20), 1); ec <- matrix(rpois(nt, 22), 1)
  od <- matrix(rpois(nt, 1), 1);  ed <- matrix(rpois(nt, 2), 1)
  Sys.unsetenv("INFLAB_BLOCK_DAYS"); d <- calc_model_likelihood(oc, ec, od, ed)
  Sys.setenv(INFLAB_BLOCK_DAYS = "7"); w <- calc_model_likelihood(oc, ec, od, ed)
  expect_false(isTRUE(all.equal(d, w)))
  Sys.unsetenv("INFLAB_BLOCK_DAYS")
})
