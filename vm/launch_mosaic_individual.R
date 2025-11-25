# ==============================================================================
# MOSAIC Individual Country Calibration Pipeline
# ==============================================================================
# Production script for running MOSAIC calibration separately for each country
# in iso_codes_mosaic. Each country gets its own output directory and can be
# resumed independently.
#
# FEATURES:
# - Loops over all 40 countries in iso_codes_mosaic
# - Each country runs as separate calibration (single-location)
# - Automatic resume detection (skips completed countries)
# - Robust error handling (one failure doesn't stop others)
# - Progress tracking with detailed logging
# - Summary statistics at completion
# - Optional per-country compression
# - Master log file tracking all countries
#
# USAGE:
#   Rscript vm/launch_mosaic_individual.R
#
# CONFIGURATION:
#   Edit USER CONFIGURATION section below to customize:
#   - Which countries to run (default: all iso_codes_mosaic)
#   - Output directory base path
#   - Control settings (ESS targets, batch sizes, etc.)
#   - Compression options
#   - Parallel settings
#
# RESUME BEHAVIOR:
#   Script automatically detects completed countries by checking for:
#   - Existence of output directory
#   - Presence of convergence_results.parquet
#   - Presence of simulations.parquet
#   If all exist, country is skipped. To force re-run, delete output directory.
#
# OUTPUT STRUCTURE:
#   ~/MOSAIC/output/{ISO_CODE}/        # Per-country output
#   ~/MOSAIC/output/master_log.txt     # Master log tracking all runs
#   ~/MOSAIC/output/summary.csv        # Final summary table
#   ~/MOSAIC/output/{ISO_CODE}.tar.gz  # Optional compressed archives
#
# ==============================================================================

# ==============================================================================
# Environment Setup
# ==============================================================================

# Set library path for VM user installation
.libPaths(c('~/R/library', .libPaths()))

# Load required packages
suppressPackageStartupMessages({
  library(MOSAIC)
})

# Attach MOSAIC environment
MOSAIC::attach_mosaic_env(silent = TRUE)

# Set MOSAIC root directory
set_root_directory("~/MOSAIC")

# ==============================================================================
# USER CONFIGURATION
# ==============================================================================

# Base output directory (individual country folders created within)
OUTPUT_BASE <- path.expand("~/MOSAIC/output/individual")

# Countries to calibrate (default: all 40 countries in iso_codes_mosaic)
# To run subset, specify manually: COUNTRIES <- c("ETH", "KEN", "TZA")
data(iso_codes_mosaic)
COUNTRIES <- iso_codes_mosaic

# Resume behavior
SKIP_COMPLETED <- TRUE  # Skip countries with existing complete output
FORCE_RERUN <- FALSE    # If TRUE, ignores existing output and reruns all

# Compression options
COMPRESS_AFTER_EACH <- FALSE  # Compress immediately after each country
COMPRESS_AT_END <- TRUE       # Compress all at end
DELETE_AFTER_COMPRESS <- FALSE  # Delete uncompressed directory after compression

# Control settings
CONTROL_SETTINGS <- list(
  # Calibration
  n_simulations = 'auto',         # Per batch (use 1000 for production)
  n_iterations = 3,             # LASER iterations per simulation
  batch_size = 1000,            # Simulations per batch
  min_batches = 5,              # Minimum batches before convergence check
  max_batches = 10,             # Maximum batches
  target_r2 = 0.95,             # R? convergence threshold
  max_simulations = 1e6,        # Safety limit

  # Targets
  ESS_param = 1000,             # Target ESS per parameter
  ESS_param_prop = 1 - 1/41,        # Proportion threshold (95%)
  ESS_best = 100,               # Target for top-weighted samples
  min_best_subset = 30,         # Minimum best subset size
  max_best_subset = 1500,       # Maximum best subset size
  ess_method = 'perplexity',    # ESS calculation method

  # Fine-tuning multiplier
  fine_tuning_multiplier = 5,   # Multiply default batch sizes by this

  # Likelihood weights
  weight_cases = 1.0,           # Weight for case data
  weight_deaths = 0.05,         # Weight for death data

  # Predictions
  best_model_n_sims = 100,      # Simulations for best-fit model
  ensemble_n_sims_per_param = 10,  # Simulations per parameter set

  # NPE settings
  npe_enable = FALSE,           # Enable Neural Posterior Estimation

  # Parallel
  use_parallel = TRUE,          # Enable parallel execution
  n_cores = parallel::detectCores() - 1,  # Number of cores

  # I/O
  io_preset = "fast",           # I/O compression preset
  clean_output = TRUE,          # Clean temporary files

  # Logging
  verbose = TRUE                # Verbose output
)

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Write to both console and master log file
log_message <- function(msg, log_file) {
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
}

#' Check if country calibration is complete
is_country_complete <- function(dir_output) {
  # Check for existence of key output files
  if (!dir.exists(dir_output)) return(FALSE)

  required_files <- c(
    "convergence_results.parquet",
    "simulations.parquet"
  )

  files_exist <- sapply(required_files, function(f) {
    file.exists(file.path(dir_output, f))
  })

  return(all(files_exist))
}

#' Get directory size in GB
get_dir_size_gb <- function(dir_path) {
  if (!dir.exists(dir_path)) return(0)
  files <- list.files(dir_path, recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) return(0)
  total_size <- sum(file.size(files), na.rm = TRUE) / 1024^3
  return(total_size)
}

#' Compress output directory
compress_directory <- function(dir_output, output_base, log_file) {
  dir_name <- basename(dir_output)
  tar_file <- file.path(output_base, paste0(dir_name, ".tar.gz"))

  log_message(sprintf("  Compressing %s...\n", dir_name), log_file)

  if (!dir.exists(dir_output)) {
    log_message("  ERROR: Directory does not exist!\n", log_file)
    return(FALSE)
  }

  # Get directory size
  dir_size <- get_dir_size_gb(dir_output)
  log_message(sprintf("    Directory size: %.2f GB\n", dir_size), log_file)

  # Compress
  compress_start <- Sys.time()
  result <- system2(
    command = "tar",
    args = c("-czf", tar_file, "-C", output_base, dir_name),
    stdout = TRUE,
    stderr = TRUE
  )
  compress_time <- difftime(Sys.time(), compress_start, units = "secs")

  # Check success
  if (file.exists(tar_file)) {
    tar_size <- file.size(tar_file) / 1024^3
    compression_ratio <- (1 - tar_size / dir_size) * 100
    log_message(sprintf("     Compressed in %.1f sec: %.2f GB (%.1f%% reduction)\n",
                       compress_time, tar_size, compression_ratio), log_file)
    return(TRUE)
  } else {
    log_message("    ERROR: Compression failed!\n", log_file)
    return(FALSE)
  }
}

# ==============================================================================
# Main Pipeline
# ==============================================================================

# Create output base directory
if (!dir.exists(OUTPUT_BASE)) dir.create(OUTPUT_BASE, recursive = TRUE)

# Initialize master log file
master_log_file <- file.path(OUTPUT_BASE, "master_log.txt")
cat("", file = master_log_file)  # Clear existing log

# Print header
header <- paste0(
  "==============================================================================\n",
  "MOSAIC Individual Country Calibration Pipeline\n",
  "==============================================================================\n",
  sprintf("Start time: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  sprintf("Countries to process: %d\n", length(COUNTRIES)),
  sprintf("Output base directory: %s\n", OUTPUT_BASE),
  sprintf("Parallel execution: %s (%d cores)\n",
          ifelse(CONTROL_SETTINGS$use_parallel, "ENABLED", "DISABLED"),
          CONTROL_SETTINGS$n_cores),
  sprintf("Skip completed: %s\n", ifelse(SKIP_COMPLETED, "YES", "NO")),
  sprintf("Force rerun: %s\n", ifelse(FORCE_RERUN, "YES", "NO")),
  sprintf("NPE enabled: %s\n", ifelse(CONTROL_SETTINGS$npe_enable, "YES", "NO")),
  "==============================================================================\n\n"
)
log_message(header, master_log_file)

# Initialize tracking
pipeline_start_time <- Sys.time()
results_tracker <- data.frame(
  iso_code = character(),
  status = character(),
  runtime_hours = numeric(),
  error_message = character(),
  output_size_gb = numeric(),
  compressed = logical(),
  stringsAsFactors = FALSE
)

# ==============================================================================
# Loop Over Countries
# ==============================================================================

for (i in seq_along(COUNTRIES)) {
  iso <- COUNTRIES[i]

  # Progress header
  progress_msg <- sprintf(
    "\n==============================================================================\n",
    "Country %d/%d: %s\n",
    "==============================================================================\n",
    i, length(COUNTRIES), iso
  )
  log_message(progress_msg, master_log_file)

  # Define output directory
  dir_output <- file.path(OUTPUT_BASE, iso)

  # Check if already complete
  if (!FORCE_RERUN && SKIP_COMPLETED && is_country_complete(dir_output)) {
    skip_msg <- sprintf("   Country %s already complete (skipping)\n", iso)
    log_message(skip_msg, master_log_file)

    results_tracker <- rbind(results_tracker, data.frame(
      iso_code = iso,
      status = "SKIPPED",
      runtime_hours = 0,
      error_message = "Already complete",
      output_size_gb = get_dir_size_gb(dir_output),
      compressed = file.exists(file.path(OUTPUT_BASE, paste0(iso, ".tar.gz"))),
      stringsAsFactors = FALSE
    ))
    next
  }

  # Create output directory
  if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

  # Record start time
  country_start_time <- Sys.time()
  log_message(sprintf("  Start time: %s\n", format(country_start_time, "%H:%M:%S")), master_log_file)

  # Run calibration with error handling
  calibration_status <- tryCatch({
    # Get priors and config for single country
    log_message("  Loading configuration and priors...\n", master_log_file)
    priors <- get_location_priors(iso = iso)
    config <- get_location_config(iso = iso)

    # Build control structure
    log_message("  Building control structure...\n", master_log_file)
    control <- mosaic_control_defaults()

    # Calibration settings
    control$calibration$n_simulations <- CONTROL_SETTINGS$n_simulations
    control$calibration$n_iterations <- CONTROL_SETTINGS$n_iterations
    control$calibration$batch_size <- CONTROL_SETTINGS$batch_size
    control$calibration$min_batches <- CONTROL_SETTINGS$min_batches
    control$calibration$max_batches <- CONTROL_SETTINGS$max_batches
    control$calibration$target_r2 <- CONTROL_SETTINGS$target_r2
    control$calibration$max_simulations <- CONTROL_SETTINGS$max_simulations

    # Parallel settings
    control$parallel$enable <- CONTROL_SETTINGS$use_parallel
    control$parallel$n_cores <- CONTROL_SETTINGS$n_cores

    # Target settings
    control$targets$ESS_param <- CONTROL_SETTINGS$ESS_param
    control$targets$ESS_param_prop <- CONTROL_SETTINGS$ESS_param_prop
    control$targets$ESS_best <- CONTROL_SETTINGS$ESS_best
    control$targets$min_best_subset <- CONTROL_SETTINGS$min_best_subset
    control$targets$max_best_subset <- CONTROL_SETTINGS$max_best_subset
    control$targets$ess_method <- CONTROL_SETTINGS$ess_method

    # Fine-tuning
    control$fine_tuning$batch_sizes <- lapply(
      control$fine_tuning$batch_sizes,
      function(x) x * CONTROL_SETTINGS$fine_tuning_multiplier
    )

    # CRITICAL: Disable multi-location parameters for single-location runs
    control$sampling$sample_tau_i <- FALSE           # Travel probability
    control$sampling$sample_mobility_gamma <- FALSE  # Gravity model exponent
    control$sampling$sample_mobility_omega <- FALSE  # Mobility rate

    # Likelihood weights
    control$likelihood$weight_cases <- CONTROL_SETTINGS$weight_cases
    control$likelihood$weight_deaths <- CONTROL_SETTINGS$weight_deaths

    # Predictions
    control$predictions$best_model_n_sims <- CONTROL_SETTINGS$best_model_n_sims
    control$predictions$ensemble_n_sims_per_param <- CONTROL_SETTINGS$ensemble_n_sims_per_param

    # NPE settings
    control$npe$enable <- CONTROL_SETTINGS$npe_enable
    control$npe$architecture_tier <- CONTROL_SETTINGS$npe_architecture
    control$npe$weight_strategy <- CONTROL_SETTINGS$npe_weight_strategy
    control$npe$learning_rate <- CONTROL_SETTINGS$npe_learning_rate
    control$npe$validation_split <- CONTROL_SETTINGS$npe_validation_split
    control$npe$patience <- CONTROL_SETTINGS$npe_patience
    control$npe$n_posterior_samples <- CONTROL_SETTINGS$npe_n_posterior_samples
    control$npe$use_gpu <- CONTROL_SETTINGS$npe_use_gpu

    # I/O and paths
    control$paths$clean_output <- CONTROL_SETTINGS$clean_output
    control$io <- mosaic_io_presets(CONTROL_SETTINGS$io_preset)
    control$logging$verbose <- CONTROL_SETTINGS$verbose

    # Run MOSAIC calibration
    log_message("  Starting MOSAIC calibration...\n", master_log_file)
    result <- run_MOSAIC(
      dir_output = dir_output,
      config = config,
      priors = priors,
      control = control,
      resume = TRUE
    )

    log_message("   Calibration completed successfully\n", master_log_file)
    list(status = "SUCCESS", message = NA_character_)

  }, error = function(e) {
    error_msg <- sprintf("   ERROR: %s\n", conditionMessage(e))
    log_message(error_msg, master_log_file)
    return(list(status = "FAILED", message = conditionMessage(e)))
  })

  # Record end time and runtime
  country_end_time <- Sys.time()
  country_runtime <- difftime(country_end_time, country_start_time, units = "hours")
  log_message(sprintf("  End time: %s\n", format(country_end_time, "%H:%M:%S")), master_log_file)
  log_message(sprintf("  Runtime: %.2f hours\n", country_runtime), master_log_file)

  # Get output size
  output_size <- get_dir_size_gb(dir_output)
  log_message(sprintf("  Output size: %.2f GB\n", output_size), master_log_file)

  # Handle compression if requested
  compressed <- FALSE
  if (calibration_status$status == "SUCCESS" && COMPRESS_AFTER_EACH) {
    compressed <- compress_directory(dir_output, OUTPUT_BASE, master_log_file)

    # Delete uncompressed directory if requested
    if (compressed && DELETE_AFTER_COMPRESS) {
      log_message(sprintf("  Deleting uncompressed directory...\n"), master_log_file)
      unlink(dir_output, recursive = TRUE)
    }
  }

  # Record result
  if (calibration_status$status == "SUCCESS") {
    results_tracker <- rbind(results_tracker, data.frame(
      iso_code = iso,
      status = "SUCCESS",
      runtime_hours = as.numeric(country_runtime),
      error_message = NA_character_,
      output_size_gb = output_size,
      compressed = compressed,
      stringsAsFactors = FALSE
    ))
  } else {
    results_tracker <- rbind(results_tracker, data.frame(
      iso_code = iso,
      status = "FAILED",
      runtime_hours = as.numeric(country_runtime),
      error_message = calibration_status$message,
      output_size_gb = output_size,
      compressed = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  # Print progress summary
  n_success <- sum(results_tracker$status == "SUCCESS")
  n_failed <- sum(results_tracker$status == "FAILED")
  n_skipped <- sum(results_tracker$status == "SKIPPED")
  progress_summary <- sprintf(
    "  Progress: %d/%d complete (%d success, %d failed, %d skipped)\n\n",
    i, length(COUNTRIES), n_success, n_failed, n_skipped
  )
  log_message(progress_summary, master_log_file)
}

# ==============================================================================
# Compression at End (if requested)
# ==============================================================================

if (COMPRESS_AT_END && !COMPRESS_AFTER_EACH) {
  log_message("\n==============================================================================\n", master_log_file)
  log_message("Compressing output directories\n", master_log_file)
  log_message("==============================================================================\n", master_log_file)

  countries_to_compress <- results_tracker$iso_code[results_tracker$status == "SUCCESS"]

  for (iso in countries_to_compress) {
    dir_output <- file.path(OUTPUT_BASE, iso)
    compressed <- compress_directory(dir_output, OUTPUT_BASE, master_log_file)

    # Update tracker
    results_tracker$compressed[results_tracker$iso_code == iso] <- compressed

    # Delete if requested
    if (compressed && DELETE_AFTER_COMPRESS) {
      log_message(sprintf("  Deleting uncompressed directory %s...\n", iso), master_log_file)
      unlink(dir_output, recursive = TRUE)
    }
  }

  log_message("\n", master_log_file)
}

# ==============================================================================
# Final Summary
# ==============================================================================

pipeline_end_time <- Sys.time()
total_runtime <- difftime(pipeline_end_time, pipeline_start_time, units = "hours")

# Calculate statistics
n_total <- nrow(results_tracker)
n_success <- sum(results_tracker$status == "SUCCESS")
n_failed <- sum(results_tracker$status == "FAILED")
n_skipped <- sum(results_tracker$status == "SKIPPED")
success_rate <- if (n_total > 0) n_success / (n_total - n_skipped) * 100 else 0
avg_runtime <- if (n_success > 0) mean(results_tracker$runtime_hours[results_tracker$status == "SUCCESS"]) else 0
total_output_size <- sum(results_tracker$output_size_gb, na.rm = TRUE)

# Print summary
summary_msg <- sprintf(
  paste0(
    "\n==============================================================================\n",
    "Pipeline Complete\n",
    "==============================================================================\n",
    "End time: %s\n",
    "Total runtime: %.2f hours\n",
    "\n",
    "Results:\n",
    "  Total countries: %d\n",
    "  Successful: %d\n",
    "  Failed: %d\n",
    "  Skipped: %d\n",
    "  Success rate: %.1f%%\n",
    "\n",
    "Performance:\n",
    "  Average runtime per country: %.2f hours\n",
    "  Total output size: %.2f GB\n",
    "  Compressed archives: %d\n",
    "\n",
    "Output location: %s\n",
    "Master log: %s\n",
    "==============================================================================\n"
  ),
  format(pipeline_end_time, "%Y-%m-%d %H:%M:%S"),
  total_runtime,
  n_total,
  n_success,
  n_failed,
  n_skipped,
  success_rate,
  avg_runtime,
  total_output_size,
  sum(results_tracker$compressed),
  OUTPUT_BASE,
  master_log_file
)
log_message(summary_msg, master_log_file)

# Save summary table
summary_file <- file.path(OUTPUT_BASE, "summary.csv")
write.csv(results_tracker, summary_file, row.names = FALSE)
log_message(sprintf("\nSummary table saved to: %s\n", summary_file), master_log_file)

# Print failed countries if any
if (n_failed > 0) {
  failed_msg <- "\nFailed countries:\n"
  log_message(failed_msg, master_log_file)

  failed_countries <- results_tracker[results_tracker$status == "FAILED", ]
  for (i in 1:nrow(failed_countries)) {
    failed_line <- sprintf("  - %s: %s\n",
                          failed_countries$iso_code[i],
                          failed_countries$error_message[i])
    log_message(failed_line, master_log_file)
  }
}

# Generate transfer commands if archives exist
tar_files <- list.files(OUTPUT_BASE, pattern = "\\.tar\\.gz$", full.names = TRUE)
if (length(tar_files) > 0) {
  # Get username and IP
  username <- Sys.info()["user"]
  ip_address <- tryCatch({
    public_ip <- system2("curl", args = c("-s", "--max-time", "3", "ifconfig.me"),
                        stdout = TRUE, stderr = FALSE)
    if (length(public_ip) > 0 && nchar(trimws(public_ip)) > 0) {
      trimws(public_ip)
    } else {
      "SERVER_IP"
    }
  }, error = function(e) "SERVER_IP")

  log_message("\n==============================================================================\n", master_log_file)
  log_message("Transfer Commands\n", master_log_file)
  log_message("==============================================================================\n", master_log_file)
  log_message("To download all compressed archives:\n\n", master_log_file)

  for (tar_file in tar_files) {
    transfer_cmd <- sprintf("scp %s@%s:%s .\n", username, ip_address, tar_file)
    log_message(transfer_cmd, master_log_file)
  }

  log_message("==============================================================================\n", master_log_file)
}

cat("\n Pipeline complete!\n")
