# Unit tests for calc_model_posterior_distributions function
# The function now takes file paths (quantiles_file, priors_file, output_dir)
# and produces a posteriors.json file. Tests verify the file-based API.

test_that("calc_model_posterior_distributions errors on missing quantiles file", {
  expect_error(
    calc_model_posterior_distributions(
      quantiles_file = "/nonexistent/file.csv",
      priors_file = "/nonexistent/priors.json",
      output_dir = tempdir(),
      verbose = FALSE
    )
  )
})

test_that("calc_model_posterior_distributions errors on missing priors file", {
  # Create a minimal quantiles file
  tmp_quantiles <- tempfile(fileext = ".csv")
  write.csv(data.frame(parameter = "test", q50 = 0.5), tmp_quantiles, row.names = FALSE)

  expect_error(
    calc_model_posterior_distributions(
      quantiles_file = tmp_quantiles,
      priors_file = "/nonexistent/priors.json",
      output_dir = tempdir(),
      verbose = FALSE
    )
  )
  unlink(tmp_quantiles)
})

test_that("posteriors.json does not carry the prior reported-CFR block", {
  # mu_jt is integrated out, not sampled; its calibrated value is
  # cfr_posterior.csv. A verbatim copy of the prior block would read as a posterior.
  set.seed(42)
  n <- 2000
  results <- data.frame(decay_shape_1 = runif(n, 0.1, 10), is_finite = TRUE, is_retained = TRUE,
                        is_best_subset = c(rep(TRUE, 500), rep(FALSE, n - 500)),
                        weight_best = c(rep(1/500, 500), rep(0, n - 500)),
                        likelihood = rnorm(n, -100, 10))
  priors <- list(metadata = list(version = "test"),
                 parameters_global = list(
                   decay_shape_1 = list(distribution = "uniform", parameters = list(min = 0.1, max = 10))),
                 parameters_location = list(),
                 mu_jt = list(sd_year = 0.7, sd_product = 0.3,
                              location = list(AAA = list(year = 2024L, logit_mean = -3.9, logit_se = 0.2))))
  out_dir <- withr::local_tempdir()
  calc_model_posterior_quantiles(results = results, output_dir = out_dir, priors = priors, verbose = FALSE)
  priors_path <- file.path(out_dir, "priors.json")
  jsonlite::write_json(priors, priors_path, auto_unbox = TRUE, pretty = TRUE, digits = NA)
  calc_model_posterior_distributions(quantiles_file = file.path(out_dir, "posterior_quantiles.csv"),
                                     priors_file = priors_path, output_dir = out_dir, verbose = FALSE)
  post <- jsonlite::fromJSON(file.path(out_dir, "posteriors.json"), simplifyVector = FALSE)
  expect_null(post$mu_jt)
  expect_false(is.null(post$parameters_global$decay_shape_1))
})
