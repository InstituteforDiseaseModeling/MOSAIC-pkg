# Regression test: print.mosaic_initial_conditions_S and print.mosaic_priors
# are registered as S3 methods in the hand-maintained NAMESPACE, so print()
# dispatches to them (R >= 4.3 does not dispatch to unregistered methods found
# only on the search path).

test_that("print.mosaic_initial_conditions_S is registered and dispatched", {
     ns_file <- system.file("NAMESPACE", package = "MOSAIC")
     if (!nzchar(ns_file)) ns_file <- testthat::test_path("..", "..", "NAMESPACE")
     skip_if_not(file.exists(ns_file), "NAMESPACE not found")
     expect_true(any(grepl("^S3method\\(print, *mosaic_initial_conditions_S\\)",
                           readLines(ns_file))))
     # envir = emptyenv(): found only via the S3 registry, not the search path
     expect_false(is.null(utils::getS3method("print", "mosaic_initial_conditions_S",
                                             optional = TRUE, envir = emptyenv())))

     x <- structure(list(
          metadata = list(initial_conditions_S = list(
               method = "constrained_residual", constraint = "S = 1 - (V1+V2+E+I+R)",
               t0 = "2023-01-01", n_locations_processed = 1L, n_samples = 10L)),
          parameters_location = list(prop_S_initial = list(parameters = list(location = list(
               ETH = list(metadata = list(estimated_from_constraints = TRUE, mean = 0.8,
                                          ci_lower = 0.7, ci_upper = 0.9,
                                          constraint_violation_rate = 0))))))),
          class = c("mosaic_initial_conditions_S", "mosaic_initial_conditions", "list"))
     expect_output(print(x), "MOSAIC Initial S Conditions")
     expect_output(print(x), "ETH")
})

test_that("print.mosaic_priors is registered and dispatched", {
     ns_file <- system.file("NAMESPACE", package = "MOSAIC")
     if (!nzchar(ns_file)) ns_file <- testthat::test_path("..", "..", "NAMESPACE")
     skip_if_not(file.exists(ns_file), "NAMESPACE not found")
     expect_true(any(grepl("^S3method\\(print, *mosaic_priors\\)", readLines(ns_file))))
     expect_false(is.null(utils::getS3method("print", "mosaic_priors",
                                             optional = TRUE, envir = emptyenv())))

     x <- structure(list(
          metadata = list(version = "test", date = "2026-01-01"),
          parameters_global = list(alpha_1 = list()),
          parameters_location = list(beta_j0_tot = list(location = list(ETH = list(), KEN = list())))),
          class = c("mosaic_priors", "list"))
     expect_output(print(x), "MOSAIC Priors Object")
     expect_output(print(x), "ETH, KEN")
})
