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
ctrl$calibration$n_simulations        <- 5000
ctrl$calibration$batch_size_adaptive  <- 10000   # was batch_size (renamed v0.22.16)
ctrl$calibration$n_iterations         <- 5

# Convergence targets
ctrl$targets$ESS_param      <- 200
ctrl$targets$ESS_param_prop <- 0.9
ctrl$targets$ESS_best       <- 50
ctrl$targets$ESS_method     <- "perplexity"   # was ess_method (case-sensitive field)

# Sampling flags (disable some for speed)
ctrl$sampling$sample_tau_i            <- FALSE
ctrl$sampling$sample_mobility_gamma   <- FALSE
ctrl$sampling$sample_mobility_omega   <- FALSE

# Likelihood weights
ctrl$likelihood$weight_cases  <- 1
ctrl$likelihood$weight_deaths <- 0.05

# Predictions
ctrl$predictions$n_iter_best     <- 30
ctrl$predictions$n_iter_ensemble <- 5

# I/O
ctrl$paths$clean_output <- TRUE
ctrl$paths$plots        <- TRUE
ctrl$io <- mosaic_io_presets("fast")
ctrl$io$load_chunk_size <- 5000
# ctrl$io$save_simresults <- TRUE  # Enable to write per-sim parquet for validation

dask_spec <- list(
  type         = "coiled",
  n_workers    = 10,
  workspace    = "idm-coiled-idmad-r2",
  software     = "mosaic-acr-workers",
  scheduler_vm_types = c("Standard_D8s_v6"),
  vm_types     = c("Standard_D4s_v6"),
  timeout      = 1200,
  region       = "westus2",
  idle_timeout = "30 minutes",
  worker_options = list(nthreads = 1L)
)

result <- run_MOSAIC(
  config     = config,
  priors     = priors,
  dir_output = dir_output,
  control    = ctrl,
  dask_spec  = dask_spec
)

cat("Done. Simulations:", result$summary$sims_total, "\n")
cat("Results saved to:", dir_output, "\n")
