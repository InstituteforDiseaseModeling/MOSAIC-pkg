
# Generate example data from documentation
set.seed(123)
T_steps <- 10; J <- 5
beta <- matrix(runif(T_steps * J), nrow = T_steps, ncol = J)
tau <- runif(J)
pie <- matrix(runif(J^2), nrow = J, ncol = J); diag(pie) <- 0
N <- matrix(sample(200:400, T_steps * J, replace = TRUE), nrow = T_steps, ncol = J)
S <- matrix(sample(100:200, T_steps * J, replace = TRUE), nrow = T_steps, ncol = J)
V1_sus <- matrix(sample(0:50, T_steps * J, replace = TRUE), nrow = T_steps, ncol = J)
V2_sus <- matrix(sample(0:50, T_steps * J, replace = TRUE), nrow = T_steps, ncol = J)
I1 <- matrix(sample(0:10, T_steps * J, replace = TRUE), nrow = T_steps, ncol = J)
I2 <- matrix(sample(0:10, T_steps * J, replace = TRUE), nrow = T_steps, ncol = J)

# Dimension mismatch errors using example objects
testthat::test_that("errors on invalid input dimensions", {
     # S not a matrix
     expect_error(
          MOSAIC::calc_spatial_hazard(beta = as.data.frame(beta), tau, pie, N,
                              S, V1_sus, V2_sus, I1, I2)
     )
     # beta wrong dims
     expect_error(
          MOSAIC::calc_spatial_hazard(beta = matrix(1, 3, 2), tau, pie, N,
                              S, V1_sus, V2_sus, I1, I2)
     )
     # tau wrong length
     expect_error(
          MOSAIC::calc_spatial_hazard(beta, tau = c(1,1,1), pie, N,
                              S, V1_sus, V2_sus, I1, I2)
     )
     # pie wrong dims
     expect_error(
          MOSAIC::calc_spatial_hazard(beta, tau, pie = matrix(0, 3, 3), N,
                              S, V1_sus, V2_sus, I1, I2)
     )
     # N wrong dims
     expect_error(
          MOSAIC::calc_spatial_hazard(beta, tau, pie, N = matrix(1,1,1),
                              S, V1_sus, V2_sus, I1, I2)
     )
})

# Trivial 1x1 scenario: perfect reporting gives zero hazard
testthat::test_that("one-location perfect reporting gives zero hazard", {

     beta1 <- matrix(1, 1, 1)
     tau1 <- 1
     pie1 <- matrix(0, 1, 1)
     N1 <- matrix(10, 1, 1)
     S1 <- matrix(5, 1, 1)
     V1_1 <- matrix(0, 1, 1)
     V2_1 <- matrix(0, 1, 1)
     I1_1 <- matrix(1, 1, 1)
     I2_1 <- matrix(0, 1, 1)
     H <- MOSAIC::calc_spatial_hazard(beta1, tau1, pie1, N1, S1, V1_1, V2_1, I1_1, I2_1)
     expect_equal(as.numeric(H), 0)

})

# Two-location symmetric case matches manual calculation
testthat::test_that("two-location symmetric case matches manual calculation", {

     beta2 <- matrix(1, 1, 2)
     tau2 <- rep(0, 2)
     pie2 <- matrix(1/(2-1), 2, 2); diag(pie2) <- 0
     N2 <- matrix(10, 1, 2)
     S2 <- matrix(5, 1, 2)
     V1_2 <- matrix(0, 1, 2)
     V2_2 <- matrix(0, 1, 2)
     I1_2 <- matrix(1, 1, 2)
     I2_2 <- matrix(0, 1, 2)
     H <- MOSAIC::calc_spatial_hazard(beta2, tau2, pie2, N2, S2, V1_2, V2_2, I1_2, I2_2)
     expected <- (5 * (1 - exp(-(5/10) * (1/20)))) / (1 + 5)
     expect_equal(as.numeric(H[1,1]), expected)
     expect_equal(as.numeric(H[1,2]), expected)

})

# Warning when N zero produces NA hazards
testthat::test_that("warning when N has zero causing NA hazard", {

     beta1 <- matrix(1,1,1); tau1 <- 1; pie1 <- matrix(0,1,1)
     N1 <- matrix(0,1,1); S1 <- matrix(1,1,1); V1_1 <- V2_1 <- matrix(0,1,1)
     I1_1 <- matrix(1,1,1); I2_1 <- matrix(0,1,1)
     expect_warning(
          MOSAIC::calc_spatial_hazard(beta1, tau1, pie1, N1, S1, V1_1, V2_1, I1_1, I2_1)
     )

})

# Assigns row and column names correctly
testthat::test_that("assigns time and location names correctly", {

     beta_sub <- beta[1:2, 1:2]; tau_sub <- tau[1:2]; pie_sub <- pie[1:2,1:2]
     N_sub <- N[1:2,1:2]; S_sub <- S[1:2,1:2];
     V1_sub <- V1_sus[1:2,1:2]; V2_sub <- V2_sus[1:2,1:2];
     I1_sub <- I1[1:2,1:2]; I2_sub <- I2[1:2,1:2]
     times <- c("t1", "t2"); locs <- c("A", "B")
     H <- MOSAIC::calc_spatial_hazard(beta_sub, tau_sub, pie_sub,
                              N_sub, S_sub, V1_sub, V2_sub,
                              I1_sub, I2_sub,
                              time_names = times,
                              location_names = locs)
     expect_equal(rownames(H), times)
     expect_equal(colnames(H), locs)
})
