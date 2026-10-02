# Regression tests for the h1 run_MOSAIC hand-off items: the shared sampling
# defaults, the named central_method in control.json, and the run-level
# weighting / n_used columns of parameter_sensitivity.csv.

test_that("mosaic_control_defaults()$sampling is .mosaic_default_sample_args() and sample_parameters accepts every flag", {
  defaults <- MOSAIC:::.mosaic_default_sample_args()
  expect_identical(mosaic_control_defaults()$sampling, defaults)

  # A user override merges onto the shared defaults, key by key.
  ctl <- mosaic_control_defaults(sampling = list(sample_rho = FALSE))
  expect_false(ctl$sampling$sample_rho)
  expect_identical(ctl$sampling[setdiff(names(defaults), "sample_rho")],
                   defaults[setdiff(names(defaults), "sample_rho")])

  # Every default flag is one sample_parameters() recognises: none trips its
  # "Unknown sampling parameter" warning.
  cfg <- get_location_config(iso = "ETH")
  expect_no_warning(
    sample_parameters(config = cfg, PATHS = list(), seed = 1, verbose = FALSE,
                      validate = FALSE, sample_args = mosaic_control_defaults()$sampling),
    message = "Unknown sampling parameter")
})

test_that("control.json keeps the channel names of a per-channel central_method", {
  d <- withr::local_tempdir()
  inp <- file.path(d, "1_inputs"); dir.create(inp)
  ctl <- mosaic_control_defaults(predictions = list(
    central_method = c(deaths = "median", cases = "mean")))
  MOSAIC:::.mosaic_write_json(list(control = MOSAIC:::.mosaic_control_for_json(ctl)),
                              file.path(inp, "control.json"), list())
  back <- jsonlite::fromJSON(file.path(inp, "control.json"))$control$predictions$central_method
  expect_identical(back, list(cases = "mean", deaths = "median"))
  # The reader recovers it without the positional-order warning.
  expect_no_warning(cm <- MOSAIC:::.mosaic_run_central_method(inp))
  expect_identical(cm, c(cases = "mean", deaths = "median"))

  # A scalar becomes an explicit per-channel pair.
  one <- MOSAIC:::.mosaic_control_for_json(mosaic_control_defaults(predictions = list(
    central_method = "mean")))
  expect_identical(one$predictions$central_method, list(cases = "mean", deaths = "mean"))
  # The package default is itself per channel (v0.101.0) and keeps its names.
  def <- MOSAIC:::.mosaic_control_for_json(mosaic_control_defaults())
  expect_identical(def$predictions$central_method, list(cases = "median", deaths = "mean"))
  # An unresolvable value is persisted as supplied (the ensemble step reports it).
  bad <- list(predictions = list(central_method = "trimmed"))
  expect_identical(MOSAIC:::.mosaic_control_for_json(bad), bad)
})

test_that("parameter_sensitivity.csv records the weighting and n_used the plot subtitle states", {
  skip_if_not_installed("sensitivity")
  skip_if_not_installed("arrow")
  set.seed(7)
  n <- 120L
  samples <- data.frame(sim = seq_len(n), iter = 1L, seed_sim = seq_len(n), seed_iter = 1L,
                        a = stats::runif(n), b = stats::runif(n), c = stats::runif(n))
  samples$likelihood <- -50 * (samples$a - 0.5)^2 + stats::rnorm(n, sd = 0.1)
  samples$is_finite <- TRUE; samples$is_valid <- TRUE; samples$is_retained <- TRUE
  samples$weight_retained <- rep(1 / n, n)
  samples$is_best_subset <- FALSE
  d <- withr::local_tempdir()
  sp <- file.path(d, "samples.parquet"); arrow::write_parquet(samples, sp)

  res <- calc_model_parameter_sensitivity(sp, output_dir = d, n_samples = 100, verbose = FALSE)
  expect_identical(res$subset_label, "importance-weighted by weight_retained")
  csv <- utils::read.csv(file.path(d, "parameter_sensitivity.csv"), stringsAsFactors = FALSE)
  expect_true(all(csv$weighting == res$subset_label))
  expect_true(all(csv$n_used == res$n_used))

  back <- MOSAIC:::.mosaic_read_sensitivity_csv(file.path(d, "parameter_sensitivity.csv"))
  expect_identical(back$subset_label, res$subset_label)
  expect_identical(back$n_used, as.integer(res$n_used))
  expect_identical(names(back$sens_df), c("parameter", "hsic_r2", "p_value", "sig", "description"))

  skip_if_not_installed("ggplot2")
  saved <- NULL
  testthat::local_mocked_bindings(
    ggsave = function(filename, plot, ...) { saved <<- plot; invisible(filename) },
    .package = "ggplot2")
  plot_model_parameter_sensitivity(results_file = sp, output_dir = d,
                                   sensitivity = back, verbose = FALSE)
  sub <- saved$labels$subtitle
  expect_match(sub, "importance-weighted by weight_retained, n = 100", fixed = TRUE)

  # A CSV written before the columns existed still reads, with n_used = NA.
  old <- csv[, c("parameter", "hsic_r2", "p_value", "sig", "description")]
  utils::write.csv(old, file.path(d, "old.csv"), row.names = FALSE)
  back_old <- MOSAIC:::.mosaic_read_sensitivity_csv(file.path(d, "old.csv"))
  expect_identical(back_old$subset_label, "as computed at calibration")
  expect_true(is.na(back_old$n_used))
})
