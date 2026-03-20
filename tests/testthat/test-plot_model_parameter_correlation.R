test_that("plot_model_parameter_correlation generates heatmap", {

  set.seed(42)
  n <- 80
  p <- 8

  params <- as.data.frame(matrix(runif(n * p), nrow = n))
  names(params) <- paste0("param_", seq_len(p))

  # Introduce known correlations
  params$param_2 <- params$param_1 + rnorm(n, 0, 0.1)  # strong positive
  params$param_3 <- -params$param_1 + rnorm(n, 0, 0.1)  # strong negative

  params$likelihood <- rnorm(n)
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
  parquet_file <- file.path(tmp_dir, "test_cor_samples.parquet")
  arrow::write_parquet(params, parquet_file)

  out_dir <- file.path(tmp_dir, "correlation_test")
  plot_model_parameter_correlation(
    results_file = parquet_file,
    output_dir = out_dir,
    cor_threshold = 0.3,
    max_params = 8,
    verbose = FALSE
  )

  expect_true(file.exists(file.path(out_dir, "parameter_correlation.png")))

  unlink(out_dir, recursive = TRUE)
  unlink(parquet_file)
})

test_that("plot_model_parameter_correlation handles missing file gracefully", {
  expect_warning(
    plot_model_parameter_correlation(
      results_file = "/nonexistent/path.parquet",
      output_dir = tempdir(),
      verbose = FALSE
    ),
    "not found"
  )
})

test_that("plot_model_parameter_correlation skips zero-variance columns", {

  set.seed(42)
  n <- 50

  params <- data.frame(
    param_a = runif(n),
    param_b = runif(n),
    param_c = rep(0.5, n),  # zero variance — should be excluded
    param_d = runif(n),
    param_e = runif(n),
    likelihood = rnorm(n),
    sim = seq_len(n),
    seed_sim = seq_len(n),
    is_finite = TRUE,
    is_valid = TRUE,
    is_outlier = FALSE,
    is_retained = TRUE,
    is_best_subset = TRUE,
    is_best_model = c(TRUE, rep(FALSE, n - 1)),
    weight_all = 1 / n,
    weight_retained = 1 / n,
    weight_best = 1 / n
  )

  tmp_dir <- tempdir()
  parquet_file <- file.path(tmp_dir, "test_cor_zerov.parquet")
  arrow::write_parquet(params, parquet_file)

  out_dir <- file.path(tmp_dir, "correlation_zerov")
  plot_model_parameter_correlation(
    results_file = parquet_file,
    output_dir = out_dir,
    verbose = FALSE
  )

  expect_true(file.exists(file.path(out_dir, "parameter_correlation.png")))

  unlink(out_dir, recursive = TRUE)
  unlink(parquet_file)
})
