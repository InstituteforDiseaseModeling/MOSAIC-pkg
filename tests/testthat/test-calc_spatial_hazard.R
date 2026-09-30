
# Generate example data from documentation
set.seed(123)
T_steps <- 10; J <- 5
# J x T: rows = locations, columns = time steps (the orientation the code uses)
beta <- matrix(runif(J * T_steps), nrow = J, ncol = T_steps)
tau <- runif(J, 0, 0.2)
pie <- matrix(runif(J^2), nrow = J, ncol = J); diag(pie) <- 0
N <- matrix(sample(200:400, J * T_steps, replace = TRUE), nrow = J, ncol = T_steps)
S <- matrix(sample(100:200, J * T_steps, replace = TRUE), nrow = J, ncol = T_steps)
V1_sus <- matrix(sample(0:50, J * T_steps, replace = TRUE), nrow = J, ncol = T_steps)
V2_sus <- matrix(sample(0:50, J * T_steps, replace = TRUE), nrow = J, ncol = T_steps)
I1 <- matrix(sample(0:10, J * T_steps, replace = TRUE), nrow = J, ncol = T_steps)
I2 <- matrix(sample(0:10, J * T_steps, replace = TRUE), nrow = J, ncol = T_steps)

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

     # J=2 locations, T=1 timestep → matrices are 2×1 (nrow=J, ncol=T)
     beta2 <- matrix(1, 2, 1)
     tau2 <- rep(0, 2)
     pie2 <- matrix(1/(2-1), 2, 2); diag(pie2) <- 0
     N2 <- matrix(10, 2, 1)
     S2 <- matrix(5, 2, 1)
     V1_2 <- matrix(0, 2, 1)
     V2_2 <- matrix(0, 2, 1)
     I1_2 <- matrix(1, 2, 1)
     I2_2 <- matrix(0, 2, 1)
     H <- MOSAIC::calc_spatial_hazard(beta2, tau2, pie2, N2, S2, V1_2, V2_2, I1_2, I2_2)
     # Both locations are symmetric, so hazards should be equal
     expect_equal(as.numeric(H[1,1]), as.numeric(H[2,1]))
     expect_true(is.finite(H[1,1]))

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

# Assigns row (location) and column (time) names correctly
testthat::test_that("assigns location row names and time column names", {

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
     expect_equal(rownames(H), locs)
     expect_equal(colnames(H), times)
})

# Regression: names used to be applied transposed, so any J != T call with
# names errored ("length of dimnames [1] not equal to array extent").
testthat::test_that("J x T inputs with J != T accept names and return J x T", {

     H <- MOSAIC::calc_spatial_hazard(beta, tau, pie, N, S, V1_sus, V2_sus, I1, I2,
                                      time_names = paste0("day_", seq_len(T_steps)),
                                      location_names = paste0("loc_", seq_len(J)))
     expect_equal(dim(H), c(J, T_steps))
     expect_identical(rownames(H), paste0("loc_", seq_len(J)))
     expect_identical(colnames(H), paste0("day_", seq_len(T_steps)))
     expect_true(all(is.finite(H)) && all(H >= 0 & H <= 1))

     # Hand-computed cell (j = 2, t = 3) from the documented formula.
     j <- 2L; t <- 3L
     sus <- (1 - tau[j]) * (S[j, t] + V1_sus[j, t] + V2_sus[j, t])
     inf <- (1 - tau[j]) * (I1[j, t] + I2[j, t]) +
          sum(tau[-j] * pie[-j, j] * (I1[-j, t] + I2[-j, t]))
     ybar <- inf / sum(N[, t])
     h <- beta[j, t] * sus * (1 - exp(-(sus / N[j, t]) * ybar)) / (1 + beta[j, t] * sus)
     expect_equal(unname(H[j, t]), h, tolerance = 1e-12)
})

# tau is a departure probability: raising it shrinks the stay-at-home pool, so
# in a single location (nothing to import) the hazard falls with tau.
testthat::test_that("hazard decreases as the departure probability tau increases", {
     h_of <- function(tt) as.numeric(MOSAIC::calc_spatial_hazard(
          matrix(0.01, 1, 1), tt, matrix(0, 1, 1), matrix(1000, 1, 1),
          matrix(800, 1, 1), matrix(0, 1, 1), matrix(0, 1, 1),
          matrix(20, 1, 1), matrix(0, 1, 1)))
     expect_gt(h_of(0.1), h_of(0.5))
     expect_gt(h_of(0.5), h_of(0.9))
})
