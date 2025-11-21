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
dir_output <- path.expand("~/MOSAIC/output/10_countries")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

set_root_directory("~/MOSAIC")

cat("==============================================================================\n")
cat("MOSAIC ETH Production Calibration\n")
cat("==============================================================================\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Output directory:", dir_output, "\n")
cat("==============================================================================\n\n")

start_time <- Sys.time()

iso_codes <- c("MOZ", "MWI", "ZMB", "ZWE",
               "TZA", "BDI", "UGA", "RWA",
               "KEN", "ETH")

#iso_codes <- c("MOZ", "MWI", "ZMB", "ZWE")

priors <- get_location_priors(iso=iso_codes)
config <- get_location_config(iso=iso_codes)

control <- mosaic_control_defaults()

control$calibration$n_simulations <- 'auto'
control$calibration$n_iterations <- 3
control$calibration$batch_size <- 500
control$calibration$min_batches <- 3
control$calibration$max_batches <- 10
control$calibration$max_simulations <- 1e+06

control$parallel$enable <- TRUE
control$parallel$n_cores <- parallel::detectCores()-1

control$targets$ESS_param <- 1000
control$targets$ESS_param_prop <- 0.95
control$targets$ESS_best <- 300
control$targets$percentile_min <- 0.001
control$targets$percentile_max <- 5.0
control$targets$ess_method <- 'perplexity'

control$fine_tuning$batch_sizes <- lapply(control$fine_tuning$batch_sizes, function(x) x*5)

control$sampling$sample_tau_i <- TRUE
control$sampling$sample_mobility_gamma <- TRUE   # Gravity model exponent
control$sampling$sample_mobility_omega <- TRUE   # Mobility rate

control$likelihood$weight_cases <- 1
control$likelihood$weight_deaths <- 0.05

control$predictions$best_model_n_sims <- 100
control$predictions$ensemble_n_sims_per_param <- 10

control$npe$enable <- TRUE
control$npe$weight_strategy <- "continuous_best"

control$paths$clean_output <- TRUE
control$io <- mosaic_io_presets("fast")

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
sims <- arrow::read_parquet("~/MOSAIC/output/4_countries/1_bfrs/outputs/simulations.parquet")

plot_model_fit_stochastic_param(
     config = config,
     parameter_seeds = sims$seed_sim,
     parameter_weights = sims$weight_best,
     n_simulations_per_config = 30,
     PATHS = get_paths(),
     priors = priors,
     sampling_args = list(
          sample_tau_i = FALSE,
          sample_mobility_gamma = FALSE,
          sample_mobility_omega = FALSE
     ),
     output_dir = "~/MOSAIC/output/4_countries/1_bfrs/plots",
     parallel = TRUE,
     n_cores = parallel::detectCores() - 1,
     verbose = TRUE
)


plot_model_ppc(
predictions_dir = "~/MOSAIC/output/4_countries/1_bfrs/plots/predictions",
   output_dir = "~/MOSAIC/output/4_countries/1_bfrs/plots"
)

