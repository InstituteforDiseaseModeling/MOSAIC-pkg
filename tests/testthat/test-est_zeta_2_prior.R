test_that("est_zeta_2_prior applies sdlog floor of 2.0", {
     skip_if_slow()
     PATHS <- .mk_test_paths()
     res <- est_zeta_2_prior(PATHS)
     expect_gte(res$fit$sdlog, 2.0)
})

test_that("zeta_2 fit is in biologically plausible range", {
     skip_if_slow()
     PATHS <- .mk_test_paths()
     res <- est_zeta_2_prior(PATHS)
     expect_true(res$fit$median > 1e3 && res$fit$median < 1e8)
})

test_that("leave-Nelson-out sensitivity is published", {
     skip_if_slow()
     PATHS <- .mk_test_paths()
     res <- est_zeta_2_prior(PATHS)
     expect_true("leave_nelson_out" %in% names(res$sensitivity))
     # leave-Nelson-out uses only Kaper review rows (n_primary_sources = 0);
     # should be flagged in the printed summary but still produce a finite fit
     expect_true(is.finite(res$sensitivity$leave_nelson_out$meanlog))
})

test_that("est_zeta_2_prior writes all four artefacts", {
     skip_if_slow()
     PATHS <- .mk_test_paths()
     est_zeta_2_prior(PATHS)
     expect_true(file.exists(file.path(PATHS$MODEL_INPUT, "data_zeta_2_prior.csv")))
     expect_true(file.exists(file.path(PATHS$MODEL_INPUT, "pred_zeta_2_prior.csv")))
     expect_true(file.exists(file.path(PATHS$MODEL_INPUT, "param_zeta_2_prior.csv")))
     expect_true(file.exists(file.path(PATHS$DOCS_FIGURES, "zeta_2_prior.png")))
})
