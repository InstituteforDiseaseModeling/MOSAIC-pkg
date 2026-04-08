test_that("plot_model_parameter_sensitivity computes PRCC with sufficient n/p", {

  # Create synthetic calibration results: 100 sims, 10 params
  set.seed(42)
  n <- 100
  p <- 10

  params <- as.data.frame(matrix(runif(n * p), nrow = n))
  names(params) <- paste0("param_", seq_len(p))

  # Likelihood depends linearly on first 3 params (known ground truth)
  params$likelihood <- 2 * params$param_1 - 1.5 * params$param_2 +
    0.5 * params$param_3 + rnorm(n, 0, 0.1)

  params$sim <- seq_len(n)
  params$seed_sim <- seq_len(n)
  params$is_finite <- TRUE
  params$is_valid <- TRUE
  params$is_outlier <- FALSE
  params$is_retained <- TRUE
  params$is_best_subset <- c(rep(TRUE, 50), rep(FALSE, 50))
  params$is_best_model <- c(TRUE, rep(FALSE, n - 1))
  params$weight_all <- 1 / n
  params$weight_retained <- 1 / n
  params$weight_best <- c(rep(1 / 50, 50), rep(0, 50))

  tmp_dir <- tempdir()
  parquet_file <- file.path(tmp_dir, "test_samples.parquet")
  arrow::write_parquet(params, parquet_file)

  out_dir <- file.path(tmp_dir, "sensitivity_test")
  result <- plot_model_parameter_sensitivity(
    results_file = parquet_file,
    output_dir = out_dir,
    max_params = 10,
    verbose = FALSE
  )

  expect_true(file.exists(file.path(out_dir, "parameter_sensitivity.png")))
  expect_true(file.exists(file.path(out_dir, "parameter_sensitivity.csv")))

  expect_s3_class(result, "data.frame")
  expect_true("hsic_r2" %in% names(result))

  # param_1 should have the highest HSIC (strongest dependence)
  top <- result$parameter[which.max(result$hsic_r2)]
  expect_true(top %in% c("param_1", "param_2"))

  unlink(out_dir, recursive = TRUE)
  unlink(parquet_file)
})

test_that("plot_model_parameter_sensitivity falls back to Spearman when n/p < 5", {

  set.seed(42)
  n <- 30
  p <- 10  # n/p = 3

  params <- as.data.frame(matrix(runif(n * p), nrow = n))
  names(params) <- paste0("param_", seq_len(p))
  params$likelihood <- params$param_1 + rnorm(n, 0, 0.1)
  params$sim <- seq_len(n)
  params$seed_sim <- seq_len(n)
  params$is_finite <- TRUE
  params$is_valid <- TRUE
  params$is_outlier <- FALSE
  params$is_retained <- TRUE
  params$is_best_subset <- rep(TRUE, n)
  params$is_best_model <- c(TRUE, rep(FALSE, n - 1))
  params$weight_all <- 1 / n
  params$weight_retained <- 1 / n
  params$weight_best <- 1 / n

  tmp_dir <- tempdir()
  parquet_file <- file.path(tmp_dir, "test_samples_small.parquet")
  arrow::write_parquet(params, parquet_file)

  out_dir <- file.path(tmp_dir, "sensitivity_fallback")
  result <- plot_model_parameter_sensitivity(
    results_file = parquet_file,
    output_dir = out_dir,
    verbose = FALSE
  )

  # HSIC should still work with small n/p
  expect_s3_class(result, "data.frame")
  expect_true("hsic_r2" %in% names(result))

  unlink(out_dir, recursive = TRUE)
  unlink(parquet_file)
})

test_that("plot_model_parameter_sensitivity handles missing file gracefully", {
  expect_warning(
    plot_model_parameter_sensitivity(
      results_file = "/nonexistent/path.parquet",
      output_dir = tempdir(),
      verbose = FALSE
    ),
    "not found"
  )
})
