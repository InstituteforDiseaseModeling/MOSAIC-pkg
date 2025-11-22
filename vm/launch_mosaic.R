# ==============================================================================
# MOSAIC ETH Production Calibration
# ==============================================================================
# VM-compatible script for production-level Ethiopia calibration
# - High ESS convergence targets (1000 per param, 99% convergence)
# - 5 iterations with adaptive batch sizing
# - NPE enabled for posterior inference
# ==============================================================================
#
# ONE-LINE SETUP AND RUN:
# curl -sSL https://raw.githubusercontent.com/InstituteforDiseaseModeling/MOSAIC-pkg/main/vm/run_mosaic_ETH.R -o ~/run_mosaic_ETH.R && r-mosaic-Rscript ~/run_mosaic_ETH.R
#
# ==============================================================================

# Set library path for VM user installation
.libPaths(c('~/R/library', .libPaths()))

# Load required packages
library(MOSAIC)

MOSAIC::attach_mosaic_env(silent = FALSE)





# Create output directory and set up logging
dir_output <- path.expand("~/MOSAIC/output/ETH_KEN")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

set_root_directory("~/MOSAIC")

cat("==============================================================================\n")
cat("MOSAIC ETH Production Calibration\n")
cat("==============================================================================\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Output directory:", dir_output, "\n")
cat("==============================================================================\n\n")

start_time <- Sys.time()

#iso_codes <- iso_codes_mosaic[iso_codes_mosaic != 'SSD']

#iso_codes <- c("MOZ", "MWI", "ZMB", "ZWE",
#               "TZA", "UGA", "KEN", "ETH")

iso_codes <- c("MOZ", "MWI", "ZMB", "ZWE")

iso_codes <- c("ETH", "KEN")

priors <- get_location_priors(iso=iso_codes)
config <- get_location_config(iso=iso_codes)

control <- mosaic_control_defaults()

control$calibration$n_simulations <- 500
control$calibration$n_iterations <- 2
control$calibration$batch_size <- 1000
control$calibration$min_batches <- 3
control$calibration$max_batches <- 10
control$calibration$max_simulations <- 1e+06

control$parallel$enable <- TRUE
control$parallel$n_cores <- parallel::detectCores()-1

control$targets$ESS_param <- 500
control$targets$ESS_param_prop <- 0.95
control$targets$ESS_best <- 100
control$targets$percentile_min <- 0.0001
control$targets$percentile_max <- 5.0
control$targets$ess_method <- 'perplexity'

control$fine_tuning$batch_sizes <- lapply(control$fine_tuning$batch_sizes, function(x) x*5)

control$sampling$sample_tau_i <- length(iso_codes) > 1
control$sampling$sample_mobility_gamma <- length(iso_codes) > 1  # Gravity model exponent
control$sampling$sample_mobility_omega <- length(iso_codes) > 1  # Mobility rate

control$likelihood$weight_cases <- 1
control$likelihood$weight_deaths <- 0.05

control$predictions$best_model_n_sims <- 100
control$predictions$ensemble_n_sims_per_param <- 10

control$npe$enable <- TRUE
control$npe$weight_strategy <- "continuous_retained"

control$paths$clean_output <- TRUE
control$io <- mosaic_io_presets("fast")

control$logging$verbose <- TRUE

result <- run_MOSAIC(
     dir_output = dir_output,
     config = config,
     priors = priors,
     control = control,
     resume = FALSE
)


# Report completion and runtime
end_time <- Sys.time()
runtime <- difftime(end_time, start_time, units = "hours")

cat("\n==============================================================================\n")
cat("MOSAIC ETH Calibration Complete\n")
cat("==============================================================================\n")
cat("End time:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Total runtime:", round(runtime, 2), "hours\n")
cat("Output directory:", dir_output, "\n")
cat("==============================================================================\n")



# Generate stochastic parameter plots for the 4_countries output
# Load parameter seeds and weights from BFRS results
sims <- arrow::read_parquet("~/MOSAIC/output/10_countries/1_bfrs/outputs/simulations.parquet")

plot_model_fit_stochastic_param(
     config = config,
     parameter_seeds = sims$seed_sim,
     parameter_weights = sims$weight_best,
     n_simulations_per_config = 5,
     PATHS = get_paths(),
     priors = priors,
     sampling_args = list(
          sample_tau_i = FALSE,
          sample_mobility_gamma = FALSE,
          sample_mobility_omega = FALSE
     ),
     output_dir = "~/MOSAIC/output/10_countries/1_bfrs/plots/predictions",
     parallel = TRUE,
     n_cores = parallel::detectCores() - 1,
     root_dir = getOption('root_directory'),
     save_predictions = TRUE,
     verbose = TRUE
)


plot_model_ppc(
predictions_dir = "~/MOSAIC/output/10_countries/1_bfrs/plots/predictions",
   output_dir = "~/MOSAIC/output/10_countries/1_bfrs/plots"
)

# ==============================================================================
# Compress output directory for transfer
# ==============================================================================

cat("\n==============================================================================\n")
cat("Compressing output directory for transfer\n")
cat("==============================================================================\n")

# Extract directory name from full path
output_parent <- path.expand("~/MOSAIC/output")
dir_name <- basename(dir_output)
tar_file <- file.path(output_parent, paste0(dir_name, ".tar.gz"))

cat("  Source directory:", dir_output, "\n")
cat("  Archive file:", tar_file, "\n")

# Check if directory exists
if (!dir.exists(dir_output)) {
  cat("  ERROR: Output directory does not exist!\n")
} else {
  # Get directory size before compression
  dir_size <- sum(file.size(list.files(dir_output, recursive = TRUE, full.names = TRUE))) / 1024^3
  cat(sprintf("  Directory size: %.2f GB\n", dir_size))

  # Compress using tar (equivalent to: tar -czf output.tar.gz directory/)
  compress_start <- Sys.time()
  result <- system2(
    command = "tar",
    args = c("-czf", tar_file, "-C", output_parent, dir_name),
    stdout = TRUE,
    stderr = TRUE
  )
  compress_time <- difftime(Sys.time(), compress_start, units = "secs")

  # Check if compression succeeded
  if (file.exists(tar_file)) {
    tar_size <- file.size(tar_file) / 1024^3
    compression_ratio <- (1 - tar_size / dir_size) * 100
    cat(sprintf("  ✓ Compression complete in %.1f seconds\n", compress_time))
    cat(sprintf("  Archive size: %.2f GB (%.1f%% compression)\n", tar_size, compression_ratio))
    cat("\n  Ready to transfer:\n")
    cat(sprintf("  scp user@server:%s .\n", tar_file))
  } else {
    cat("  ERROR: Compression failed!\n")
    cat("  Message:", paste(result, collapse = "\n"), "\n")
  }
}

cat("==============================================================================\n")

