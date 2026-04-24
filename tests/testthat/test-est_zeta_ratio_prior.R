test_that("est_zeta_ratio_prior runs end-to-end with NULL inputs", {
     PATHS <- .mk_test_paths()
     res <- est_zeta_ratio_prior(PATHS, n_sim = 10000L, seed = 1L)
     expect_named(res, c("data", "fit", "param_df", "prediction", "diagnostics"))
     expect_named(res$diagnostics,
                  c("fit_direct", "fit_derived", "fit_combined",
                    "ratio_sample", "analytic_check", "candidate_fits"))
     expect_length(res$diagnostics$ratio_sample, 10000L)
})

test_that("est_zeta_ratio_prior is deterministic with fixed seed", {
     PATHS <- .mk_test_paths()
     r1 <- est_zeta_ratio_prior(PATHS, n_sim = 5000L, seed = 42L)
     r2 <- est_zeta_ratio_prior(PATHS, n_sim = 5000L, seed = 42L)
     expect_equal(r1$fit$meanlog, r2$fit$meanlog)
     expect_equal(r1$fit$sdlog,   r2$fit$sdlog)
})

test_that("derived channel matches the analytic independent-lognormal form", {
     PATHS <- .mk_test_paths()
     res   <- est_zeta_ratio_prior(PATHS, n_sim = 10000L, seed = 1L)
     # Derived channel uses the closed-form ratio of independent lognormals:
     #   sdlog_B = sqrt(sdlog_1^2 + sdlog_2^2)
     # analytic_check reports the same formula and should match exactly.
     expect_equal(res$diagnostics$fit_derived$sdlog,
                  res$diagnostics$analytic_check$sdlog,
                  tolerance = 1e-10)
     expect_equal(res$diagnostics$fit_derived$meanlog,
                  res$diagnostics$analytic_check$meanlog,
                  tolerance = 1e-10)
})

test_that("combined fit is between direct and derived medians", {
     PATHS <- .mk_test_paths()
     res <- est_zeta_ratio_prior(PATHS, n_sim = 10000L, seed = 1L)
     d_med <- res$diagnostics$fit_direct$median
     b_med <- res$diagnostics$fit_derived$median
     c_med <- res$diagnostics$fit_combined$median
     expect_true(c_med >= min(d_med, b_med) && c_med <= max(d_med, b_med))
})

test_that("est_zeta_ratio_prior writes all four artefacts", {
     PATHS <- .mk_test_paths()
     est_zeta_ratio_prior(PATHS, n_sim = 10000L, seed = 1L)
     expect_true(file.exists(file.path(PATHS$MODEL_INPUT, "data_zeta_ratio_prior.csv")))
     expect_true(file.exists(file.path(PATHS$MODEL_INPUT, "pred_zeta_ratio_prior.csv")))
     expect_true(file.exists(file.path(PATHS$MODEL_INPUT, "param_zeta_ratio_prior.csv")))
     expect_true(file.exists(file.path(PATHS$DOCS_FIGURES, "zeta_ratio_prior.png")))
})
