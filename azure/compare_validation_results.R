#!/usr/bin/env Rscript
# =============================================================================
# compare_validation_results.R
# =============================================================================
# Standalone comparison of existing local vs Dask simulations.parquet files.
# No cluster or MOSAIC package needed — just arrow.
#
# Usage:
#   Rscript azure/compare_validation_results.R <local_parquet> <dask_parquet>
#
# Example:
#   Rscript azure/compare_validation_results.R \
#     ~/output/validate_local_20260314_034830/1_bfrs/outputs/simulations.parquet \
#     ~/output/validate_dask_20260314_034830/1_bfrs/outputs/simulations.parquet
# =============================================================================

library(arrow)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  # Default to latest matching pair
  base <- path.expand("~/output")
  local_parquet <- file.path(base, "validate_local_20260314_034830",
                             "1_bfrs", "outputs", "simulations.parquet")
  dask_parquet  <- file.path(base, "validate_dask_20260314_034830",
                             "1_bfrs", "outputs", "simulations.parquet")
  cat("No arguments — using default paths:\n")
} else {
  local_parquet <- args[1]
  dask_parquet  <- args[2]
}

cat(sprintf("  Local: %s\n", local_parquet))
cat(sprintf("  Dask:  %s\n\n", dask_parquet))

stopifnot("Local parquet not found" = file.exists(local_parquet))
stopifnot("Dask parquet not found"  = file.exists(dask_parquet))

# =============================================================================
# LOAD AND ALIGN
# =============================================================================

local_sims <- as.data.frame(arrow::read_parquet(local_parquet))
dask_sims  <- as.data.frame(arrow::read_parquet(dask_parquet))

cat(sprintf("Local: %d rows, %d cols\n", nrow(local_sims), ncol(local_sims)))
cat(sprintf("Dask:  %d rows, %d cols\n", nrow(dask_sims), ncol(dask_sims)))

# Coerce sim to integer and sort
local_sims$sim <- as.integer(local_sims$sim)
dask_sims$sim  <- as.integer(dask_sims$sim)
local_sims <- local_sims[order(local_sims$sim), ]
dask_sims  <- dask_sims[order(dask_sims$sim), ]

N_SIMS <- nrow(local_sims)

if (!all(local_sims$sim == dask_sims$sim)) {
  cat("\n*** FAIL: sim_id sets differ! ***\n")
  cat("  Local:", head(local_sims$sim, 20), "\n")
  cat("  Dask: ", head(dask_sims$sim, 20), "\n")
  # Try intersection
  common <- intersect(local_sims$sim, dask_sims$sim)
  cat(sprintf("  Common sim_ids: %d\n", length(common)))
  if (length(common) == 0) stop("No common sim_ids to compare")
  local_sims <- local_sims[local_sims$sim %in% common, ]
  dask_sims  <- dask_sims[dask_sims$sim %in% common, ]
  N_SIMS <- nrow(local_sims)
  cat(sprintf("  Continuing with %d common sims\n", N_SIMS))
} else {
  cat(sprintf("sim_ids match: %d simulations\n", N_SIMS))
}

cat("\n=============================================================\n")
cat("  COMPARISON\n")
cat("=============================================================\n\n")

# =============================================================================
# 1. LIKELIHOOD COMPARISON
# =============================================================================
cat("--- Likelihood comparison ---\n")

ll_local <- as.numeric(local_sims$likelihood)
ll_dask  <- as.numeric(dask_sims$likelihood)

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

cat(sprintf("  Exact match (diff == 0):     %d / %d\n", n_exact, N_SIMS))
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

# Per-sim detail table
cat("\n  Per-sim detail:\n")
cat(sprintf("  %7s  %15s  %15s  %12s  %12s\n",
            "sim", "ll_local", "ll_dask", "abs_diff", "rel_diff"))
cat(sprintf("  %7s  %15s  %15s  %12s  %12s\n",
            "-------", "---------------", "---------------",
            "------------", "------------"))
for (i in seq_len(N_SIMS)) {
  cat(sprintf("  %7d  %15.6f  %15.6f  %12.2e  %12.2e\n",
              local_sims$sim[i],
              ll_local[i], ll_dask[i],
              ll_abs_diff[i],
              ifelse(is.finite(ll_rel_diff[i]), ll_rel_diff[i], NA_real_)))
}

# =============================================================================
# 2. PARAMETER VALUE COMPARISON
# =============================================================================
cat("\n--- Parameter value comparison ---\n")

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

# =============================================================================
# 3. SEED COMPARISON
# =============================================================================
cat("\n--- Seed comparison ---\n")
seed_sim_match  <- all(as.integer(local_sims$seed_sim) == as.integer(dask_sims$seed_sim))
seed_iter_match <- all(as.integer(local_sims$seed_iter) == as.integer(dask_sims$seed_iter))
cat(sprintf("  seed_sim  match: %s\n", seed_sim_match))
cat(sprintf("  seed_iter match: %s\n", seed_iter_match))

# =============================================================================
# VERDICT
# =============================================================================
cat("\n=============================================================\n")

all_ll_match     <- (n_exact == n_finite) && (n_mismatch == 0)
all_params_match <- all(param_max_diffs < 1e-10)
seeds_match      <- seed_sim_match && seed_iter_match

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
cat("=============================================================\n")

# Save comparison CSV
summary_file <- file.path(dirname(dirname(dirname(dirname(local_parquet)))),
                          "validation_comparison.csv")
comparison_df <- data.frame(
  sim          = local_sims$sim,
  ll_local     = ll_local,
  ll_dask      = ll_dask,
  ll_abs_diff  = ll_abs_diff,
  ll_rel_diff  = ll_rel_diff
)
write.csv(comparison_df, summary_file, row.names = FALSE)
cat(sprintf("\nSaved: %s\n", summary_file))
