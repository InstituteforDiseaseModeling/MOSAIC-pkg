# ==============================================================================
# MOSAIC ETH Production Calibration
# ==============================================================================
# VM-compatible script for production-level Ethiopia calibration
# - Works with minimal setup (VM installation)
# - High ESS convergence targets (1000 per param, 99% convergence)
# - 5 iterations with adaptive batch sizing
# - NPE enabled for posterior inference
# ==============================================================================
#
# COPY-PASTE COMMAND FOR VM (SSH):
# curl -sSL https://raw.githubusercontent.com/InstituteforDiseaseModeling/MOSAIC-pkg/main/vm/launch_mosaic_ETH.sh | bash
#
# Or manual approach:
# git clone https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg.git ~/MOSAIC/MOSAIC-pkg
# cd ~/MOSAIC/MOSAIC-pkg/vm
# chmod +x launch_mosaic_ETH.sh
# ./launch_mosaic_ETH.sh
#
# ==============================================================================

# Set library path for VM user installation
.libPaths(c('~/R/library', .libPaths()))

# Load required packages
library(MOSAIC)

MOSAIC::attach_mosaic_env(silent = FALSE)

# Set root directory (adjust path as needed)
set_root_directory("~/MOSAIC")

# Get standard MOSAIC paths
PATHS <- get_paths()

dir_output <- "~/MOSAIC/output/ETH"
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)


priors_ETH <- get_location_priors(iso="ETH")
config_ETH <- get_location_config(iso="ETH")

control_ETH <- mosaic_control_defaults()

control_ETH$calibration$n_simulations <- 'auto'
control_ETH$calibration$n_iterations <- 5
control_ETH$calibration$batch_size <- 1000
control_ETH$calibration$min_batches <- 5
control_ETH$calibration$max_batches <- 10
control_ETH$calibration$max_simulations <- 50000

control_ETH$parallel$enable <- TRUE
control_ETH$parallel$n_cores <- parallel::detectCores() - 4

control_ETH$targets$ESS_param <- 1000
control_ETH$targets$ESS_param_prop <- 0.99
control_ETH$targets$ESS_best <- 100
control_ETH$targets$ess_method <- 'perplexity'

control_ETH$fine_tuning$batch_sizes <- lapply(control_ETH$fine_tuning$batch_sizes, function(x) x*5)

control_ETH$sampling$sample_tau_i <- FALSE
control_ETH$sampling$sample_mobility_gamma <- FALSE   # Gravity model exponent
control_ETH$sampling$sample_mobility_omega <- FALSE   # Mobility rate

control_ETH$likelihood$weight_cases <- 1
control_ETH$likelihood$weight_deaths <- 0.05

control_ETH$predictions$best_model_n_sims <- 1000
control_ETH$predictions$ensemble_n_sims_per_param <- 100

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

