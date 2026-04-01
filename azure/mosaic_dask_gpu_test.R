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
dir_output <- file.path(base_output_dir, paste0("ETH_GPU_", run_stamp))
cat("Output directory:", dir_output, "\n")

# ---------------------------------------------------------------------------
# GPU availability check
# ---------------------------------------------------------------------------
cat("\n=== GPU Environment ===\n")
torch_ok <- requireNamespace("torch", quietly = TRUE)
cat("torch R package:", torch_ok, "\n")
if (torch_ok) {
  cat("torch installed:", torch::torch_is_installed(), "\n")
  cat("CUDA available:", torch::cuda_is_available(), "\n")
  if (torch::cuda_is_available()) {
    cat("CUDA device:", torch::cuda_get_device(), "\n")
  }
}
cat("\n")

# ---------------------------------------------------------------------------
# MOSAIC setup
# ---------------------------------------------------------------------------
set_root_directory("/workspace/MOSAIC")

config <- get_location_config(iso = "ETH")
priors <- get_location_priors(iso = "ETH")

# ---------------------------------------------------------------------------
# Control: GPU-enabled, small batch for quick validation
# ---------------------------------------------------------------------------
ctrl <- mosaic_control_defaults()

# Calibration: small fixed mode for GPU validation
ctrl$calibration$n_simulations <- 100000
ctrl$calibration$batch_size    <- 10000
ctrl$calibration$n_iterations  <- 5

# Enable GPU-batched likelihood
ctrl$parallel$use_gpu <- TRUE

# Convergence targets
ctrl$targets$ESS_param      <- 200
ctrl$targets$ESS_param_prop <- 0.9
ctrl$targets$ESS_best       <- 50
ctrl$targets$ess_method     <- "perplexity"

# Sampling (disable some for speed)
ctrl$sampling$sample_tau_i            <- FALSE
ctrl$sampling$sample_mobility_gamma   <- FALSE
ctrl$sampling$sample_mobility_omega   <- FALSE

# Likelihood weights
ctrl$likelihood$weight_cases  <- 1
ctrl$likelihood$weight_deaths <- 0.05

# Predictions (minimal for test)
ctrl$predictions$best_model_n_sims        <- 10
ctrl$predictions$ensemble_n_sims_per_param <- 2

# NPE disabled
ctrl$npe$enable <- FALSE

# I/O
ctrl$paths$clean_output <- TRUE
ctrl$paths$plots        <- FALSE
ctrl$io <- mosaic_io_presets("fast")
ctrl$io$load_chunk_size <- 5000

dask_spec <- list(
  type         = "coiled",
  n_workers    = 200,
  software     = "mosaic-acr-workers",
  scheduler_vm_types = c("Standard_D8s_v6"),
  vm_types     = c("Standard_D2s_v6"),
  timeout      = 600,
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

cat("\nDone. Simulations:", result$summary$sims_total, "\n")
cat("Results saved to:", dir_output, "\n")
