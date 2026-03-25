library(MOSAIC)

# ---------------------------------------------------------------------------
# Output directory: auto-detect Docker vs local, then stamp with datetime
# ---------------------------------------------------------------------------
base_output_dir <- if (dir.exists("/workspace/output")) {
  "/workspace/output"                  # Docker container
} else {
  file.path(getwd(), "output")         # Local run (uses current directory)
}

run_stamp  <- format(Sys.time(), "%Y%m%d_%H%M%S")
dir_output <- file.path(base_output_dir, paste0("ETH_", run_stamp))
cat("Output directory:", dir_output, "\n")

# ---------------------------------------------------------------------------
# MOSAIC setup
# ---------------------------------------------------------------------------
set_root_directory("/workspace/MOSAIC")

# iso_codes <- c("ETH", "KEN")

config <- get_location_config(iso = "ETH")
priors <- get_location_priors(iso = "ETH")

# ---------------------------------------------------------------------------
# Fixed calibration control
# ---------------------------------------------------------------------------
ctrl <- mosaic_control_defaults()

# Calibration: fixed mode
ctrl$calibration$n_simulations <- 1000
ctrl$calibration$batch_size    <- 10000
ctrl$calibration$n_iterations  <- 5

# Convergence targets
ctrl$targets$ESS_param      <- 200
ctrl$targets$ESS_param_prop <- 0.9
ctrl$targets$ESS_best       <- 50
ctrl$targets$ess_method      <- "perplexity"

# Sampling flags (disable some for speed)
ctrl$sampling$sample_tau_i            <- FALSE
ctrl$sampling$sample_mobility_gamma   <- FALSE
ctrl$sampling$sample_mobility_omega   <- FALSE

# Likelihood weights
ctrl$likelihood$weight_cases  <- 1
ctrl$likelihood$weight_deaths <- 0.05

# Predictions
ctrl$predictions$best_model_n_sims        <- 30
ctrl$predictions$ensemble_n_sims_per_param <- 5

# NPE disabled
ctrl$npe$enable <- FALSE

# I/O
ctrl$paths$clean_output <- TRUE
ctrl$paths$plots        <- FALSE
ctrl$io <- mosaic_io_presets("fast")
ctrl$io$load_chunk_size <- 5000
# ctrl$io$save_simresults <- TRUE  # Enable to write per-sim parquet for validation

dask_spec <- list(
  type         = "coiled",
  n_workers    = 100,
  software     = "mosaic-acr-workers",
  scheduler_vm_types = c("Standard_D8s_v6"),
  vm_types     = c("Standard_D4s_v6"),
  timeout      = 1200,
  region       = "westus2",
  idle_timeout = "30 minutes",
  worker_options = list(nthreads = 1L)
)

result <- run_MOSAIC_dask(
  config     = config,
  priors     = priors,
  dir_output = dir_output,
  control    = ctrl,
  dask_spec  = dask_spec
)

cat("Done. Simulations:", result$summary$sims_total, "\n")
cat("Results saved to:", dir_output, "\n")
