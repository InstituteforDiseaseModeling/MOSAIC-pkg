#!/usr/bin/env Rscript
# =============================================================================
# validate_dask_local_equivalence.R
# =============================================================================
# Level 3 end-to-end validation: run_MOSAIC() vs run_MOSAIC_dask()
#
# Runs 20 sims (sim_ids 1:20) through BOTH code paths with identical:
#   - config, priors, control (same likelihood settings)
#   - seeds (sim_id = parameter seed, seed_ij = iteration seed)
#
# Then compares per-sim likelihoods and parameter values from the output
# parquets.  If both paths produce identical LASER configs → identical
# LASER outputs → identical likelihoods → the comparison should show
# zero (or near-zero) difference.
#
# Requirements:
#   - MOSAIC package installed
#   - MOSAIC-data available at the expected root
#   - A Dask cluster (Coiled or local scheduler) for the Dask leg
#
# Usage:
#   Rscript azure/validate_dask_local_equivalence.R
#
# Or from inside a Docker container:
#   Rscript /src/MOSAIC-pkg/azure/validate_dask_local_equivalence.R
# =============================================================================

library(MOSAIC)

cat("=============================================================\n")
cat("  MOSAIC: Dask ↔ Local Equivalence Validation\n")
cat("=============================================================\n\n")

# =============================================================================
# CONFIGURATION — adjust these as needed
# =============================================================================

N_SIMS    <- 50L           # Number of simulations to compare
N_ITER    <- 1L            # Iterations per simulation
ISO       <- "ETH"         # Location (single-country for speed)

# Dask cluster spec (Coiled) — adjust to your environment
DASK_SPEC <- list(
  type                = "coiled",
  n_workers           = 5L,
  software            = "mosaic-acr-workers",
  vm_types            = c("Standard_D4s_v6"),
  scheduler_vm_types  = c("Standard_D4s_v6"),
  region              = "westus2",
  idle_timeout        = "30 minutes"
)

# Root directory auto-detection
ROOT_DIR <- if (dir.exists("/workspace/MOSAIC")) {
  "/workspace/MOSAIC"
} else {
  path.expand("~/MOSAIC")
}

# Output directory
BASE_OUTPUT <- if (dir.exists("/workspace/output")) {
  "/workspace/output"
} else {
  file.path(getwd(), "output")
}

run_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# =============================================================================
# SHARED SETUP (identical for both legs)
# =============================================================================

set_root_directory(ROOT_DIR)
PATHS <- get_paths()

cat(sprintf("Root:       %s\n", ROOT_DIR))
cat(sprintf("Location:   %s\n", ISO))
cat(sprintf("Sims:       %d  (sim_ids 1:%d)\n", N_SIMS, N_SIMS))
cat(sprintf("Iterations: %d per sim\n", N_ITER))
cat(sprintf("Output:     %s\n\n", BASE_OUTPUT))

config <- get_location_config(iso = ISO)
priors <- get_location_priors(iso = ISO)

# Build a minimal control — identical for both legs
ctrl <- mosaic_control_defaults(
  calibration = list(
    n_simulations  = N_SIMS,     # fixed mode
    n_iterations   = N_ITER,
    batch_size     = N_SIMS      # single batch
  ),
  parallel = list(
    enable  = TRUE,              # must be parallel: .mosaic_run_simulation_worker()
    n_cores = 8L,                # references `control` as free var, only available
    type    = "PSOCK",           # in workers via clusterExport (not in sequential mode)
    progress = TRUE
  ),
  paths = list(
    clean_output = TRUE,
    plots        = FALSE         # skip plots for speed
  ),
  io = c(mosaic_io_presets("fast"), save_simresults = TRUE)
)

# =============================================================================
# LEG 1: run_MOSAIC (local, sequential)
# =============================================================================

dir_local <- file.path(BASE_OUTPUT, paste0("validate_local_", run_stamp))

# Known path to simulations parquet (written before post-processing)
local_parquet <- file.path(dir_local, "2_calibration", "samples.parquet")

cat("-------------------------------------------------------------\n")
cat("LEG 1: run_MOSAIC (local, parallel)\n")
cat(sprintf("  Output → %s\n", dir_local))
cat("-------------------------------------------------------------\n")

t_local_start <- Sys.time()

# Wrap in tryCatch — post-processing may fail with small N but
# simulations.parquet is written before that stage
result_local <- tryCatch(
  run_MOSAIC(
    config     = config,
    priors     = priors,
    dir_output = dir_local,
    control    = ctrl
  ),
  error = function(e) {
    cat(sprintf("\n  NOTE: run_MOSAIC post-processing error (non-fatal):\n    %s\n", e$message))
    cat("  Simulations parquet should still be available for comparison.\n\n")
    NULL
  }
)

t_local <- as.numeric(difftime(Sys.time(), t_local_start, units = "secs"))
cat(sprintf("\nLeg 1 complete: %.1fs\n\n", t_local))

# Verify local parquet exists
if (!file.exists(local_parquet)) {
  stop("Local samples.parquet not found at: ", local_parquet,
       "\n  run_MOSAIC failed before writing simulation results.")
}

# =============================================================================
# LEG 2: run_MOSAIC_dask (Dask cluster)
# =============================================================================

dir_dask <- file.path(BASE_OUTPUT, paste0("validate_dask_", run_stamp))

# Known path to simulations parquet
dask_parquet <- file.path(dir_dask, "2_calibration", "samples.parquet")

cat("-------------------------------------------------------------\n")
cat("LEG 2: run_MOSAIC_dask (Dask cluster)\n")
cat(sprintf("  Output → %s\n", dir_dask))
cat("-------------------------------------------------------------\n")

t_dask_start <- Sys.time()

result_dask <- tryCatch(
  run_MOSAIC_dask(
    config     = config,
    priors     = priors,
    dir_output = dir_dask,
    control    = ctrl,
    dask_spec  = DASK_SPEC
  ),
  error = function(e) {
    cat(sprintf("\n  NOTE: run_MOSAIC_dask post-processing error (non-fatal):\n    %s\n", e$message))
    cat("  Simulations parquet should still be available for comparison.\n\n")
    NULL
  }
)

t_dask <- as.numeric(difftime(Sys.time(), t_dask_start, units = "secs"))
cat(sprintf("\nLeg 2 complete: %.1fs\n\n", t_dask))

# Verify dask parquet exists
if (!file.exists(dask_parquet)) {
  stop("Dask samples.parquet not found at: ", dask_parquet,
       "\n  run_MOSAIC_dask failed before writing simulation results.")
}

# =============================================================================
# COMPARISON
# =============================================================================

cat("=============================================================\n")
cat("  COMPARISON\n")
cat("=============================================================\n\n")

# Load both result parquets (use known paths, not result objects which may be NULL)
local_sims <- arrow::read_parquet(local_parquet)
dask_sims  <- arrow::read_parquet(dask_parquet)

cat(sprintf("Local sims: %d rows\n", nrow(local_sims)))
cat(sprintf("Dask  sims: %d rows\n", nrow(dask_sims)))

# Sort both by sim for consistent comparison
local_sims <- local_sims[order(local_sims$sim), ]
dask_sims  <- dask_sims[order(dask_sims$sim), ]

# Coerce sim columns to integer (Arrow may read as double vs integer depending on writer)
local_sims$sim <- as.integer(local_sims$sim)
dask_sims$sim  <- as.integer(dask_sims$sim)

# Check sim_ids match (values, not types)
if (!all(local_sims$sim == dask_sims$sim)) {
  cat("\n*** FAIL: sim_id sets differ! ***\n")
  cat("  Local sim_ids:", head(local_sims$sim, 20), "\n")
  cat("  Dask  sim_ids:", head(dask_sims$sim, 20), "\n")
  stop("Cannot compare — sim_ids don't match")
}

cat(sprintf("sim_ids match: %d simulations\n\n", nrow(local_sims)))

# --- 1. Likelihood comparison ---
cat("--- Likelihood comparison ---\n")

ll_local <- local_sims$likelihood
ll_dask  <- dask_sims$likelihood

ll_diff     <- ll_local - ll_dask
ll_abs_diff <- abs(ll_diff)
ll_rel_diff <- ifelse(
  is.finite(ll_local) & is.finite(ll_dask) & ll_local != 0,
  abs(ll_diff / ll_local),
  NA_real_
)

n_exact    <- sum(ll_diff == 0, na.rm = TRUE)
n_close    <- sum(ll_abs_diff < 1e-6, na.rm = TRUE)
n_finite   <- sum(is.finite(ll_local) & is.finite(ll_dask))
n_both_na  <- sum(is.na(ll_local) & is.na(ll_dask))
n_mismatch <- sum(is.na(ll_local) != is.na(ll_dask))

cat(sprintf("  Exact match (diff == 0):    %d / %d\n", n_exact, N_SIMS))
cat(sprintf("  Close match (|diff| < 1e-6): %d / %d\n", n_close, N_SIMS))
cat(sprintf("  Both finite:                 %d / %d\n", n_finite, N_SIMS))
cat(sprintf("  Both NA/NaN:                 %d / %d\n", n_both_na, N_SIMS))
cat(sprintf("  NA mismatch:                 %d / %d\n", n_mismatch, N_SIMS))

if (n_finite > 0) {
  cat(sprintf("\n  Abs diff — max: %.2e  mean: %.2e  median: %.2e\n",
              max(ll_abs_diff, na.rm = TRUE),
              mean(ll_abs_diff, na.rm = TRUE),
              median(ll_abs_diff, na.rm = TRUE)))
  finite_rel <- ll_rel_diff[is.finite(ll_rel_diff)]
  if (length(finite_rel) > 0) {
    cat(sprintf("  Rel diff — max: %.2e  mean: %.2e  median: %.2e\n",
                max(finite_rel), mean(finite_rel), median(finite_rel)))
  }
}

# Print per-sim detail table
cat("\n  Per-sim detail:\n")
cat(sprintf("  %7s  %15s  %15s  %12s  %12s\n",
            "sim", "ll_local", "ll_dask", "abs_diff", "rel_diff"))
cat(sprintf("  %7s  %15s  %15s  %12s  %12s\n",
            "-------", "---------------", "---------------",
            "------------", "------------"))
for (i in seq_len(nrow(local_sims))) {
  cat(sprintf("  %7d  %15.6f  %15.6f  %12.2e  %12.2e\n",
              local_sims$sim[i],
              ll_local[i], ll_dask[i],
              ll_abs_diff[i],
              ifelse(is.finite(ll_rel_diff[i]), ll_rel_diff[i], NA_real_)))
}

# --- 2. Parameter value comparison ---
cat("\n--- Parameter value comparison ---\n")

# Identify parameter columns (exclude metadata/flag columns)
meta_cols <- c("sim", "iter", "seed_sim", "seed_iter", "likelihood",
               "is_finite", "is_valid", "is_outlier", "is_retained",
               "is_best_subset", "is_best_model",
               "weight_all", "weight_retained", "weight_best")
param_cols <- setdiff(names(local_sims), meta_cols)
param_cols <- intersect(param_cols, names(dask_sims))

cat(sprintf("  Comparing %d parameter columns\n", length(param_cols)))

param_max_diffs <- numeric(length(param_cols))
names(param_max_diffs) <- param_cols

for (p in param_cols) {
  v_local <- as.numeric(local_sims[[p]])
  v_dask  <- as.numeric(dask_sims[[p]])
  param_max_diffs[p] <- max(abs(v_local - v_dask), na.rm = TRUE)
}

# Report top discrepancies
sorted_diffs <- sort(param_max_diffs, decreasing = TRUE)
cat("\n  Top 10 parameter discrepancies (max |diff| across sims):\n")
cat(sprintf("  %-30s  %12s\n", "parameter", "max_abs_diff"))
cat(sprintf("  %-30s  %12s\n", "------------------------------", "------------"))
for (i in seq_len(min(10, length(sorted_diffs)))) {
  cat(sprintf("  %-30s  %12.2e\n", names(sorted_diffs)[i], sorted_diffs[i]))
}

n_param_exact <- sum(param_max_diffs == 0)
n_param_close <- sum(param_max_diffs < 1e-10)
cat(sprintf("\n  Parameters with exact match:    %d / %d\n", n_param_exact, length(param_cols)))
cat(sprintf("  Parameters with |diff| < 1e-10: %d / %d\n", n_param_close, length(param_cols)))

# --- 3. seed_sim / seed_iter comparison ---
cat("\n--- Seed comparison ---\n")
seed_sim_match  <- identical(as.integer(local_sims$seed_sim),
                             as.integer(dask_sims$seed_sim))
seed_iter_match <- identical(as.integer(local_sims$seed_iter),
                             as.integer(dask_sims$seed_iter))
cat(sprintf("  seed_sim  match: %s\n", seed_sim_match))
cat(sprintf("  seed_iter match: %s\n", seed_iter_match))

# =============================================================================
# VERDICT
# =============================================================================

cat("\n=============================================================\n")

all_ll_match <- (n_exact == n_finite) && (n_mismatch == 0)
all_params_match <- all(param_max_diffs < 1e-10)
seeds_match <- seed_sim_match && seed_iter_match

if (all_ll_match && all_params_match && seeds_match) {
  cat("  PASS: run_MOSAIC and run_MOSAIC_dask produce identical results\n")
} else if (n_close == N_SIMS && all_params_match) {
  cat("  PASS (NEAR-EXACT): likelihoods within 1e-6, params match\n")
  cat("  (Small float diffs likely from JSON round-trip or numpy precision)\n")
} else {
  cat("  FAIL: results differ between local and Dask paths\n")
  if (!all_ll_match)      cat("    - Likelihood mismatch\n")
  if (!all_params_match)  cat("    - Parameter value mismatch\n")
  if (!seeds_match)       cat("    - Seed mismatch\n")
}

cat(sprintf("\n  Timing: local=%.1fs  dask=%.1fs\n", t_local, t_dask))
cat("=============================================================\n")

# Save comparison summary as CSV for reference
comparison_df <- data.frame(
  sim          = local_sims$sim,
  ll_local     = ll_local,
  ll_dask      = ll_dask,
  ll_abs_diff  = ll_abs_diff,
  ll_rel_diff  = ll_rel_diff,
  seed_sim_local  = local_sims$seed_sim,
  seed_sim_dask   = dask_sims$seed_sim,
  seed_iter_local = local_sims$seed_iter,
  seed_iter_dask  = dask_sims$seed_iter
)

summary_file <- file.path(BASE_OUTPUT, paste0("validation_summary_", run_stamp, ".csv"))
write.csv(comparison_df, summary_file, row.names = FALSE)
cat(sprintf("\nSummary saved: %s\n", summary_file))
