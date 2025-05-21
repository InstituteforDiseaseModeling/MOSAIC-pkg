
library(testthat)
library(MOSAIC)

# -------------------------------------------------------------------------
test_that("returns known values for simple synthetic data", {
     # identical prevalence  ->  correlation = 1
     I <- c(1, 2, 3, 4, 5)
     expect_equal(
          calc_spatial_correlation(I, I, N_i = 10, N_j = 10),
          1
     )

     # j is the negative of i -> correlation = -1
     expect_equal(
          calc_spatial_correlation(I, -I, N_i = 10, N_j = 10),
          -1
     )

     # hand-checked numeric example
     I_i <- c(1, 2, 3)
     I_j <- c(2, 4, 6)
     # prevalence_i = c(0.1, 0.2, 0.3)
     # prevalence_j = c(0.2, 0.4, 0.6)
     expect_equal(
          calc_spatial_correlation(I_i, I_j, N_i = 10, N_j = 10),
          cor(c(0.1, 0.2, 0.3), c(0.2, 0.4, 0.6)),
          tolerance = 1e-12
     )
})

# -------------------------------------------------------------------------
test_that("symmetry C_ij == C_ji", {
     set.seed(123)
     I_i <- rpois(20, 5)
     I_j <- rpois(20, 9)

     C_ij <- calc_spatial_correlation(I_i, I_j, N_i = 100, N_j = 100)
     C_ji <- calc_spatial_correlation(I_j, I_i, N_i = 100, N_j = 100)

     expect_equal(C_ij, C_ji)
})

# -------------------------------------------------------------------------
test_that("scalar and vector population sizes give same result", {
     I <- 1:5

     c_scalar <- calc_spatial_correlation(I, I, N_i = 10, N_j = 20)
     N_i <- rep(10, 5); N_j <- rep(20, 5)
     c_vector <- calc_spatial_correlation(I, I, N_i, N_j)

     expect_equal(c_scalar, c_vector)
})

# -------------------------------------------------------------------------
test_that("automatic NA handling and minimum paired observations", {

     I_i <- c(1, NA, 3)
     I_j <- c(1,  2, 3)

     # two complete cases remain after NA removal
     res <- calc_spatial_correlation(I_i, I_j, N_i = 10, N_j = 10)
     expect_equal(res, 1)

     # all NA in one series  ->  result should be NA
     I_bad <- c(NA, NA, NA)
     expect_true(is.na(
          calc_spatial_correlation(I_bad, I_j, N_i = 10, N_j = 10)
     ))
})

# -------------------------------------------------------------------------
test_that("returns NA when variance zero", {
     I_const <- rep(2, 4)
     expect_true(is.na(
          calc_spatial_correlation(I_const, I_const, 10, 10)
     ))
})

# -------------------------------------------------------------------------
test_that("detects prevalence > 1 and raises error", {
     I_bad <- c(10, 20)
     expect_error(
          calc_spatial_correlation(I_bad, I_bad, N_i = 5, N_j = 5),
          "Prevalence values exceed 1"
     )
})

# -------------------------------------------------------------------------
test_that("errors on mismatched vector lengths", {
     expect_error(
          calc_spatial_correlation(1:3, 1:4, 1, 1),
          "same length"
     )
     expect_error(
          calc_spatial_correlation(1:3, 1:3, N_i = 1:2, N_j = 1),
          "same length"
     )
})
