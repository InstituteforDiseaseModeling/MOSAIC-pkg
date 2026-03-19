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
