test_that("delta_t = 0 maps 1:1 with no NAs and correct scaling", {
     infections <- c(0, 5, 20, 60)
     sigma <- 0.25
     rho   <- 0.70
     chi_endemic  <- 0.50
     chi_epidemic <- 0.75  # unused here
     out <- calc_cases_from_infections(
          infections = infections,
          N = NULL,
          sigma = sigma,
          rho = rho,
          chi_endemic = chi_endemic,
          chi_epidemic = chi_epidemic,
          epidemic_threshold = NULL,
          delta_t = 0
     )
     expect_false(any(is.na(out$cases_true)))
     expect_false(any(is.na(out$cases_suspected)))
     expect_equal(out$cases_true,      rho * sigma * infections, tolerance = 1e-12)
     expect_equal(out$cases_suspected, (rho * sigma / chi_endemic) * infections, tolerance = 1e-12)
})

test_that("delta_t > 0 pads with NAs and shifts correctly", {
     infections <- c(0, 5, 20, 60)
     sigma <- 0.25
     rho   <- 0.70
     chi_endemic <- 0.50
     k <- 2
     out <- calc_cases_from_infections(
          infections = infections,
          N = NULL,
          sigma = sigma,
          rho = rho,
          chi_endemic = chi_endemic,
          chi_epidemic = 0.75,
          epidemic_threshold = NULL,
          delta_t = k
     )
     # First k values are NA
     expect_true(all(is.na(out$cases_true[1:k])))
     expect_true(all(is.na(out$cases_suspected[1:k])))
     # Check alignment at t = k+1..n
     expect_equal(out$cases_true[(k+1):4],      rho * sigma * infections[1:(4-k)], tolerance = 1e-12)
     expect_equal(out$cases_suspected[(k+1):4], (rho * sigma / chi_endemic) * infections[1:(4-k)], tolerance = 1e-12)
})

test_that("epidemic switching uses chi_epidemic when infections/N exceeds threshold", {
     infections <- c(10, 50, 40, 10)
     N_vec <- rep(1000, length(infections))
     thr <- 0.03  # 3% per day
     # infections/N = (0.01, 0.05, 0.04, 0.01) -> flags: FALSE, TRUE, TRUE, FALSE
     sigma <- 0.25; rho <- 0.70
     chi_endemic <- 0.50; chi_epidemic <- 0.75
     out <- calc_cases_from_infections(
          infections = infections,
          N = N_vec,
          sigma = sigma,
          rho = rho,
          chi_endemic = chi_endemic,
          chi_epidemic = chi_epidemic,
          epidemic_threshold = thr,
          delta_t = 0
     )
     # True cases unaffected by chi
     expect_equal(out$cases_true, rho * sigma * infections, tolerance = 1e-12)
     # Suspected uses chi_endemic where below threshold, chi_epidemic where above
     chi_use <- c(chi_endemic, chi_epidemic, chi_epidemic, chi_endemic)
     expected_sus <- (rho * sigma / chi_use) * infections
     expect_equal(out$cases_suspected, expected_sus, tolerance = 1e-12)
})

test_that("When epidemic_threshold = NULL, N is ignored and chi_endemic is used everywhere", {
     infections <- c(5, 10, 15)
     out <- calc_cases_from_infections(
          infections = infections,
          N = 12345,                   # should be ignored
          sigma = 0.25,
          rho = 0.70,
          chi_endemic = 0.60,
          chi_epidemic = 0.90,
          epidemic_threshold = NULL,   # bypass switching
          delta_t = 0
     )
     expect_equal(out$cases_suspected, (0.70 * 0.25 / 0.60) * infections, tolerance = 1e-12)
})

test_that("delta_t >= length(infections) returns all NAs", {
     infections <- c(1, 2, 3)
     out <- calc_cases_from_infections(
          infections = infections,
          N = NULL,
          sigma = 0.25,
          rho = 0.70,
          chi_endemic = 0.50,
          chi_epidemic = 0.75,
          epidemic_threshold = NULL,
          delta_t = length(infections)  # all outputs should be NA
     )
     expect_true(all(is.na(out$cases_true)))
     expect_true(all(is.na(out$cases_suspected)))
})

test_that("Input validation errors are thrown for bad arguments", {
     infections <- c(1, 2, 3)
     expect_error(calc_cases_from_infections(
          infections = infections,
          N = NULL,
          sigma = 1.1,
          rho = 0.7,
          chi_endemic = 0.5,
          chi_epidemic = 0.75,
          epidemic_threshold = NULL,
          delta_t = 0
     ))

     expect_error(calc_cases_from_infections(
          infections = infections,
          N = NULL,
          sigma = 0.25,
          rho = -0.1,
          chi_endemic = 0.5,
          chi_epidemic = 0.75,
          epidemic_threshold = NULL,
          delta_t = 0
     ))

     expect_error(calc_cases_from_infections(
          infections = infections,
          N = NULL,
          sigma = 0.25,
          rho = 0.7,
          chi_endemic = 0,
          chi_epidemic = 0.75,
          epidemic_threshold = NULL,
          delta_t = 0
     ))

     # Switching requested but N missing
     expect_error(calc_cases_from_infections(
          infections = infections,
          N = NULL,
          sigma = 0.25,
          rho = 0.7,
          chi_endemic = 0.5,
          chi_epidemic = 0.75,
          epidemic_threshold = 0.01,
          delta_t = 0
     ))
})