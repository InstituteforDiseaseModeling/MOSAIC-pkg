# Regression tests (deep review, priors group) for est_initial_S().

test_that("est_initial_S runs with its default verbose = TRUE and stores metadata", {
     # Before v0.100.0 the verbose summary summed x$metadata$... over entries
     # that carried no metadata, so the default call errored after all the work.
     cfg <- list(location_name = c("ETH", "MOZ"), date_start = "2023-01-01")
     set.seed(1)
     out <- NULL
     expect_no_error(capture.output(
          out <- est_initial_S(list(), MOSAIC::priors_default, cfg, n_samples = 50)
     ))
     eth <- out$parameters_location$prop_S_initial$parameters$location$ETH
     expect_true(isTRUE(eth$metadata$estimated_from_constraints))
     expect_true(eth$metadata$mean > 0 && eth$metadata$mean < 1)
     expect_true(eth$metadata$ci_lower <= eth$metadata$mean)
     expect_true(eth$metadata$constraint_violation_rate >= 0)
     expect_s3_class(out, "mosaic_initial_conditions_S")
     # Called directly: NAMESPACE carries no S3method() registration (see review notes)
     expect_output(print.mosaic_initial_conditions_S(out), "Constrained S estimates")
})
