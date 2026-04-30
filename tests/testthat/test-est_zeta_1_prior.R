test_that("est_zeta_1_prior returns expected list structure", {
     PATHS <- .mk_test_paths()
     res <- est_zeta_1_prior(PATHS)
     expect_named(res, c("data", "fit", "param_df", "prediction", "sensitivity"))
     expect_s3_class(res$data, "data.frame")
     expect_named(res$fit, c("meanlog", "sdlog", "median", "mode", "mean",
                             "ci_lower", "ci_upper", "n_sources",
                             "mu_log10", "sd_log10"))
     expect_named(res$param_df,
                  c("variable_name", "variable_description",
                    "parameter_distribution", "i", "j", "t",
                    "parameter_name", "parameter_value"))
})

test_that("zeta_1 fit is in biologically plausible range", {
     PATHS <- .mk_test_paths()
     res <- est_zeta_1_prior(PATHS)
     expect_true(res$fit$median > 1e10 && res$fit$median < 1e13)
     expect_true(res$fit$sdlog > 0.3 && res$fit$sdlog < 3.0)
})

test_that("est_zeta_1_prior writes all four artefacts", {
     PATHS <- .mk_test_paths()
     est_zeta_1_prior(PATHS)
     expect_true(file.exists(file.path(PATHS$MODEL_INPUT, "data_zeta_1_prior.csv")))
     expect_true(file.exists(file.path(PATHS$MODEL_INPUT, "pred_zeta_1_prior.csv")))
     expect_true(file.exists(file.path(PATHS$MODEL_INPUT, "param_zeta_1_prior.csv")))
     expect_true(file.exists(file.path(PATHS$DOCS_FIGURES, "zeta_1_prior.png")))
})

test_that("severity_mix argument is honoured", {
     PATHS <- .mk_test_paths()
     res_outbreak <- est_zeta_1_prior(PATHS,
                                      severity_mix = c(severe = 0.35, moderate = 0.4, mild = 0.25))
     res_endemic  <- est_zeta_1_prior(PATHS,
                                      severity_mix = c(severe = 0.2,  moderate = 0.4, mild = 0.4))
     # severity_mix sets the Endemic severity-weighted pool row in data_df
     # (pool rows have weight 0 so they do not feed the fit — the argument is
     # honoured in the data table). Outbreak mix weights severe higher, so the
     # endemic pool zeta_1 under the outbreak mix is larger than under the
     # default endemic mix.
     pool_outbreak <- res_outbreak$data$zeta_1[
          res_outbreak$data$source == "Endemic severity-weighted pool"]
     pool_endemic  <- res_endemic$data$zeta_1[
          res_endemic$data$source  == "Endemic severity-weighted pool"]
     expect_gt(pool_outbreak, pool_endemic)
})

test_that("sensitivity fits are populated", {
     PATHS <- .mk_test_paths()
     res <- est_zeta_1_prior(PATHS)
     expect_named(res$sensitivity, c("include_calib", "volume_peak", "volume_low"))
     expect_true(all(vapply(res$sensitivity, function(x) is.finite(x$meanlog), logical(1))))
})
