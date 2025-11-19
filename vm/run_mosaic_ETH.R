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

# Ensure no debug mode is active (from previous sessions)
if (isdebugged(calc_convergence_diagnostics)) {
  undebug(calc_convergence_diagnostics)
}
options(error = NULL)  # Disable error recovery browser

MOSAIC::attach_mosaic_env(silent = FALSE)


# Create output directory and set up logging
dir_output <- path.expand("~/MOSAIC/output/ETH")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

set_root_directory("~/MOSAIC")

log_file <- file.path(dir_output, paste0("mosaic_ETH_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
log_con <- file(log_file, open = "wt")
sink(log_con, type = "output", split = TRUE)  # Split = TRUE shows output in console too
sink(log_con, type = "message")

cat("==============================================================================\n")
cat("MOSAIC ETH Production Calibration\n")
cat("==============================================================================\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Log file:", log_file, "\n")
cat("Output directory:", dir_output, "\n")
cat("==============================================================================\n\n")

start_time <- Sys.time()


priors_ETH <- get_location_priors(iso="ETH")
config_ETH <- get_location_config(iso="ETH")

control_ETH <- mosaic_control_defaults()

control_ETH$calibration$n_simulations <- 1000
control_ETH$calibration$n_iterations <- 1
control_ETH$calibration$batch_size <- 200
control_ETH$calibration$min_batches <- 3
control_ETH$calibration$max_batches <- 10
control_ETH$calibration$max_simulations <- 1e+06

control_ETH$parallel$enable <- TRUE
control_ETH$parallel$n_cores <- parallel::detectCores()-1

control_ETH$targets$ESS_param <- 100
control_ETH$targets$ESS_param_prop <- 0.9
control_ETH$targets$ESS_best <- 30
control_ETH$targets$ess_method <- 'perplexity'

control_ETH$fine_tuning$batch_sizes <- lapply(control_ETH$fine_tuning$batch_sizes, function(x) x*5)

control_ETH$sampling$sample_tau_i <- FALSE
control_ETH$sampling$sample_mobility_gamma <- FALSE   # Gravity model exponent
control_ETH$sampling$sample_mobility_omega <- FALSE   # Mobility rate

control_ETH$likelihood$weight_cases <- 1
control_ETH$likelihood$weight_deaths <- 0.05

control_ETH$predictions$best_model_n_sims <- 30
control_ETH$predictions$ensemble_n_sims_per_param <- 5

control_ETH$npe$enable <- TRUE
control_ETH$npe$weight_strategy <- "continuous_retained"

control_ETH$paths$clean_output <- TRUE
control_ETH$io <- mosaic_io_presets("fast")

result_ETH <- run_MOSAIC(
     dir_output = dir_output,
     config = config_ETH,
     priors = priors_ETH,
     control = control_ETH,
     resume = FALSE
)

dir_output = dir_output
config = config_ETH
priors = priors_ETH
control = control_ETH
resume = FALSE

# Report completion and runtime
end_time <- Sys.time()
runtime <- difftime(end_time, start_time, units = "hours")

cat("\n==============================================================================\n")
cat("MOSAIC ETH Calibration Complete\n")
cat("==============================================================================\n")
cat("End time:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Total runtime:", round(runtime, 2), "hours\n")
cat("Log file:", log_file, "\n")
cat("Output directory:", dir_output, "\n")
cat("==============================================================================\n")

# Close log file
sink(type = "message")
sink(type = "output")
close(log_con)

