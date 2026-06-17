
# Load required packages
library(MOSAIC)

MOSAIC::attach_mosaic_env(silent = FALSE)


# Create output directory and set up logging
dir_output <- path.expand("~/MOSAIC/output/ETH_auto")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

set_root_directory("~/MOSAIC")


priors_ETH <- get_location_priors(iso="ETH")
config_ETH <- get_location_config(iso="ETH")

control_ETH <- mosaic_control_defaults()

control_ETH$calibration$n_simulations <- NULL   # NULL = adaptive/auto mode; integer = fixed mode
control_ETH$calibration$n_iterations <- 2
control_ETH$calibration$batch_size_adaptive <- 500
control_ETH$calibration$min_batches_adaptive <- 5
control_ETH$calibration$max_batches_adaptive <- 10
control_ETH$calibration$target_r2_adaptive <- 0.95
control_ETH$calibration$max_simulations_total <- 1e+06

control_ETH$parallel$enable <- TRUE
control_ETH$parallel$n_cores <- parallel::detectCores()-1

control_ETH$targets$ESS_param <- 100
control_ETH$targets$ESS_param_prop <- 0.9
control_ETH$targets$ESS_best <- 50
control_ETH$targets$ESS_method <- 'perplexity'

control_ETH$sampling$sample_tau_i <- FALSE
control_ETH$sampling$sample_mobility_gamma <- FALSE   # Gravity model exponent
control_ETH$sampling$sample_mobility_omega <- FALSE   # Mobility rate

control_ETH$likelihood$weight_cases <- 1
control_ETH$likelihood$weight_deaths <- 0.05

control_ETH$predictions$n_iter_best <- 30
control_ETH$predictions$n_iter_ensemble <- 5

control_ETH$paths$clean_output <- TRUE
control_ETH$io <- mosaic_io_presets("fast")

result_ETH <- run_MOSAIC(
     dir_output = dir_output,
     config = config_ETH,
     priors = priors_ETH,
     control = control_ETH
)
