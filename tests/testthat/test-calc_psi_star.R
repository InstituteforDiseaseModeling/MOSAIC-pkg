# Test file for calc_psi_star function

# 1. Input validation tests
testthat::test_that("errors on non-numeric psi", {
     expect_error(
          MOSAIC::calc_psi_star(psi = "invalid"),
          "`psi` must be numeric"
     )
})

testthat::test_that("errors on invalid parameter a", {
     expect_error(
          MOSAIC::calc_psi_star(psi = c(0.5), a = 0),
          "`a` must be a positive scalar"
     )
     expect_error(
          MOSAIC::calc_psi_star(psi = c(0.5), a = c(1, 2)),
          "`a` must be a positive scalar"
     )
     expect_error(
          MOSAIC::calc_psi_star(psi = c(0.5), a = -1),
          "`a` must be a positive scalar"
     )
})

testthat::test_that("errors on invalid parameter b", {
     expect_error(
          MOSAIC::calc_psi_star(psi = c(0.5), b = c(1, 2)),
          "`b` must be a numeric scalar"
     )
})

testthat::test_that("errors on invalid parameter z", {
     expect_error(
          MOSAIC::calc_psi_star(psi = c(0.5), z = 0),
          "`z` must be in \\(0, 1\\]"
     )
     expect_error(
          MOSAIC::calc_psi_star(psi = c(0.5), z = 1.5),
          "`z` must be in \\(0, 1\\]"
     )
     expect_error(
          MOSAIC::calc_psi_star(psi = c(0.5), z = c(0.5, 0.8)),
          "`z` must be in \\(0, 1\\]"
     )
})

testthat::test_that("errors on invalid eps", {
     expect_error(
          MOSAIC::calc_psi_star(psi = c(0.5), eps = 0),
          "`eps` must be a small positive number"
     )
     expect_error(
          MOSAIC::calc_psi_star(psi = c(0.5), eps = -1),
          "`eps` must be a small positive number"
     )
})

# 2. Warning tests for out-of-bounds values
testthat::test_that("warns when psi values are outside [0,1]", {
     expect_warning(
          MOSAIC::calc_psi_star(psi = c(-0.1, 0.5, 1.2)),
          "Some psi values are outside \\[0,1\\] and will be clipped"
     )
})

testthat::test_that("warns when NA values are present", {
     expect_warning(
          MOSAIC::calc_psi_star(psi = c(0.1, NA, 0.8)),
          "Missing values \\(NA\\) detected in psi"
     )
})

# 3. Clipping behavior tests
testthat::test_that("clips out-of-bounds values correctly", {
     psi_bad <- c(-0.5, 1.5, 0.5)
     result <- suppressWarnings(MOSAIC::calc_psi_star(psi_bad, eps = 1e-6))
     # Should be close to plogis(qlogis(eps)), plogis(qlogis(1-eps)), plogis(qlogis(0.5))
     expect_true(all(result >= 0 & result <= 1))
     expect_true(result[1] < 0.01)  # Very small due to clipping
     expect_true(result[2] > 0.99)  # Very large due to clipping
})

# 4. Mathematical correctness tests
testthat::test_that("defaults (a=1, b=0, z=1) return input unchanged", {
     psi <- c(0.1, 0.3, 0.7, 0.9)
     result <- MOSAIC::calc_psi_star(psi)
     expect_equal(result, psi, tolerance = 1e-10)
})

testthat::test_that("logit transformation works correctly", {
     psi <- c(0.1, 0.5, 0.9)
     a <- 2
     b <- -0.5
     result <- MOSAIC::calc_psi_star(psi, a = a, b = b, z = 1)  # No smoothing

     # Manual calculation
     psi_clipped <- pmin(pmax(psi, 1e-6), 1 - 1e-6)
     expected <- plogis(a * qlogis(psi_clipped) + b)

     expect_equal(result, expected, tolerance = 1e-10)
})

testthat::test_that("parameter a affects shape correctly", {
     psi <- c(0.1, 0.5, 0.9)
     result_flat <- MOSAIC::calc_psi_star(psi, a = 0.5, z = 1)   # Flatter
     result_sharp <- MOSAIC::calc_psi_star(psi, a = 2, z = 1)    # Sharper

     # With a < 1, extreme values should be less extreme
     # With a > 1, extreme values should be more extreme
     expect_true(result_flat[1] > result_sharp[1])    # Low value less low when flattened
     expect_true(result_flat[3] < result_sharp[3])    # High value less high when flattened
})

testthat::test_that("parameter b shifts baseline correctly", {
     psi <- rep(0.5, 3)
     result_up <- MOSAIC::calc_psi_star(psi, b = 1, z = 1)      # Shift up
     result_down <- MOSAIC::calc_psi_star(psi, b = -1, z = 1)   # Shift down
     result_none <- MOSAIC::calc_psi_star(psi, b = 0, z = 1)    # No shift

     expect_true(all(result_up > result_none))
     expect_true(all(result_down < result_none))
})

# 5. EWMA smoothing tests
testthat::test_that("z=1 produces no smoothing", {
     psi <- c(0.1, 0.9, 0.1, 0.9)
     result_no_smooth <- MOSAIC::calc_psi_star(psi, z = 1)
     result_transform_only <- plogis(qlogis(pmin(pmax(psi, 1e-6), 1 - 1e-6)))
     expect_equal(result_no_smooth, result_transform_only, tolerance = 1e-10)
})

testthat::test_that("EWMA smoothing reduces volatility", {
     set.seed(42)
     psi <- c(0.1, 0.9, 0.1, 0.9, 0.1, 0.9)  # Oscillating
     result_smooth <- MOSAIC::calc_psi_star(psi, z = 0.5)      # Heavy smoothing
     result_no_smooth <- MOSAIC::calc_psi_star(psi, z = 1)     # No smoothing

     # Smoothed version should have lower variance
     var_smooth <- var(result_smooth)
     var_no_smooth <- var(result_no_smooth)
     expect_true(var_smooth < var_no_smooth)
})

testthat::test_that("EWMA first value unchanged", {
     psi <- c(0.3, 0.7, 0.2)
     result <- MOSAIC::calc_psi_star(psi, a = 1.5, b = -0.2, z = 0.8)
     expected_first <- plogis(1.5 * qlogis(0.3) - 0.2)
     expect_equal(result[1], expected_first, tolerance = 1e-10)
})

testthat::test_that("EWMA produces causal smoothing", {
     psi <- c(0.1, 0.5, 0.9, 0.1)
     z <- 0.7
     result <- MOSAIC::calc_psi_star(psi, z = z, a = 1, b = 0)

     # Manual EWMA calculation
     psi_transformed <- plogis(qlogis(pmin(pmax(psi, 1e-6), 1 - 1e-6)))
     expected <- numeric(4)
     expected[1] <- psi_transformed[1]
     for (t in 2:4) {
          expected[t] <- z * psi_transformed[t] + (1 - z) * expected[t - 1]
     }

     expect_equal(result, expected, tolerance = 1e-10)
})

# 6. Edge cases
testthat::test_that("handles single element input", {
     psi <- 0.5
     result <- MOSAIC::calc_psi_star(psi, a = 2, b = -0.3, z = 0.8)
     expected <- plogis(2 * qlogis(0.5) - 0.3)
     expect_equal(result, expected, tolerance = 1e-10)
})

testthat::test_that("handles empty input", {
     psi <- numeric(0)
     result <- MOSAIC::calc_psi_star(psi)
     expect_equal(result, numeric(0))
})

testthat::test_that("handles NAs correctly in smoothing without propagation", {
     psi <- c(0.5, NA, 0.8, 0.3)
     result <- suppressWarnings(MOSAIC::calc_psi_star(psi, z = 0.8))
     expect_true(is.finite(result[1]))  # First value finite
     expect_true(is.na(result[2]))      # NA preserved at original position
     expect_true(is.finite(result[3])) # Smoothing resumes after NA
     expect_true(is.finite(result[4])) # Smoothing continues
})

# 7. Output properties tests
testthat::test_that("output is always in [0,1] range", {
     set.seed(123)
     psi <- runif(20)
     result <- MOSAIC::calc_psi_star(psi, a = runif(1, 0.1, 5), b = runif(1, -2, 2), z = runif(1, 0.1, 1))
     expect_true(all(result >= 0 & result <= 1, na.rm = TRUE))
})

testthat::test_that("output length matches input length", {
     psi <- runif(15)
     result <- MOSAIC::calc_psi_star(psi, a = 1.5, b = -0.2, z = 0.9)
     expect_equal(length(result), length(psi))
})

testthat::test_that("preserves vector names if present", {
     psi <- c(week1 = 0.1, week2 = 0.5, week3 = 0.9)
     result <- MOSAIC::calc_psi_star(psi)
     expect_equal(names(result), names(psi))
})