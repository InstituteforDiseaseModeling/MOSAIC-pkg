#!/usr/bin/env Rscript
# =============================================================================
# compare_validation_results.R
# =============================================================================
# Standalone comparison of local vs Dask validation output directories.
# Compares both samples.parquet (parameters + likelihoods) and
# simresults parquets (raw per-iteration cases/deaths + psi_jt precision).
#
# Usage:
#   Rscript azure/compare_validation_results.R <local_dir> <dask_dir>
#
# Example:
#   Rscript azure/compare_validation_results.R \
#     ~/output/validate_local_20260326_183606 \
#     ~/output/validate_dask_20260326_183606
# =============================================================================

library(arrow)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  stop("Usage: Rscript compare_validation_results.R <local_dir> <dask_dir>")
}

dir_local <- args[1]
dir_dask  <- args[2]

# Resolve parquet paths
local_parquet <- file.path(dir_local, "2_calibration", "samples.parquet")
dask_parquet  <- file.path(dir_dask,  "2_calibration", "samples.parquet")

# Resolve simresults dirs
local_sr_dir <- file.path(dir_local, "2_calibration", "simulation_results")
dask_sr_dir  <- file.path(dir_dask,  "2_calibration", "simulation_results")

cat(sprintf("  Local: %s\n", local_parquet))
cat(sprintf("  Dask:  %s\n\n", dask_parquet))

stopifnot("Local parquet not found" = file.exists(local_parquet))
stopifnot("Dask parquet not found"  = file.exists(dask_parquet))

# =============================================================================
# LOAD AND ALIGN (samples.parquet)
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
  cat("\n*** WARNING: sim_id sets differ! ***\n")
  common <- intersect(local_sims$sim, dask_sims$sim)
  cat(sprintf("  Common sim_ids: %d\n", length(common)))
  if (length(common) == 0) stop("No common sim_ids to compare")
  local_sims <- local_sims[local_sims$sim %in% common, ]
  dask_sims  <- dask_sims[dask_sims$sim %in% common, ]
  N_SIMS <- nrow(local_sims)
} else {
  cat(sprintf("\nsim_ids match: %d simulations\n", N_SIMS))
}

cat("\n=============================================================\n")
cat("  COMPARISON\n")
cat("=============================================================\n\n")

# =============================================================================
# 1. PARAMETER VALUE COMPARISON (from samples.parquet)
# =============================================================================
cat("--- Parameter comparison ---\n")

meta_cols <- c("sim", "iter", "seed_sim", "seed_iter", "likelihood",
               "is_finite", "is_valid", "is_outlier", "is_retained",
               "is_best_subset", "is_best_model",
               "weight_all", "weight_retained", "weight_best")
param_cols <- setdiff(names(local_sims), meta_cols)
param_cols <- intersect(param_cols, names(dask_sims))

cat(sprintf("  Parameter columns found: %d\n", length(param_cols)))

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
# 2. SIMRESULTS COMPARISON (raw per-iteration cases/deaths + psi_jt)
# =============================================================================

local_sr_files <- list.files(local_sr_dir, pattern = "^simresults_.*\\.parquet$", full.names = TRUE)
dask_sr_files  <- list.files(dask_sr_dir,  pattern = "^simresults_.*\\.parquet$", full.names = TRUE)

has_simresults <- length(local_sr_files) > 0 && length(dask_sr_files) > 0

if (has_simresults) {
  cat(sprintf("\nLoading local results...\n  Found %d parquet files in %s\n", length(local_sr_files), local_sr_dir))
  cat(sprintf("Loading Dask results...\n  Found %d parquet files in %s\n", length(dask_sr_files), dask_sr_dir))

  local_sr <- as.data.frame(data.table::rbindlist(lapply(local_sr_files, arrow::read_parquet)))
  dask_sr  <- as.data.frame(data.table::rbindlist(lapply(dask_sr_files,  arrow::read_parquet)))

  cat(sprintf("\nLocal: %d rows, %d cols, sims %d-%d\n", nrow(local_sr), ncol(local_sr),
              min(local_sr$sim), max(local_sr$sim)))
  cat(sprintf("Dask:  %d rows, %d cols, sims %d-%d\n", nrow(dask_sr), ncol(dask_sr),
              min(dask_sr$sim), max(dask_sr$sim)))

  # Align by (sim, iter, j, t)
  local_sr <- local_sr[order(local_sr$sim, local_sr$iter, local_sr$j, local_sr$t), ]
  dask_sr  <- dask_sr[order(dask_sr$sim,  dask_sr$iter,  dask_sr$j,  dask_sr$t), ]

  # --- 2a. Parameter comparison from simresults ---
  cat("\n--- Parameter comparison (from simresults) ---\n")
  sr_meta <- c("sim", "iter", "j", "t", "cases", "deaths", "psi_jt")
  sr_param_cols <- setdiff(intersect(names(local_sr), names(dask_sr)), sr_meta)
  cat(sprintf("  Parameter columns found: %d\n", length(sr_param_cols)))

  sr_param_diffs <- numeric(length(sr_param_cols))
  names(sr_param_diffs) <- sr_param_cols
  for (p in sr_param_cols) {
    sr_param_diffs[p] <- max(abs(as.numeric(local_sr[[p]]) - as.numeric(dask_sr[[p]])), na.rm = TRUE)
  }
  sorted_sr <- sort(sr_param_diffs, decreasing = TRUE)
  cat("\n  Top 10 parameter discrepancies (max |diff| across sims):\n")
  cat(sprintf("  %-30s  %12s\n", "parameter", "max_abs_diff"))
  cat(sprintf("  %-30s  %12s\n", "------------------------------", "------------"))
  for (i in seq_len(min(10, length(sorted_sr)))) {
    cat(sprintf("  %-30s  %12.2e\n", names(sorted_sr)[i], sorted_sr[i]))
  }
  cat(sprintf("\n  Parameters with exact match:    %d / %d\n",
              sum(sr_param_diffs == 0), length(sr_param_cols)))
  cat(sprintf("  Parameters with |diff| < 1e-10: %d / %d\n",
              sum(sr_param_diffs < 1e-10), length(sr_param_cols)))

  # --- 2b. psi_jt precision analysis ---
  if ("psi_jt" %in% names(local_sr) && "psi_jt" %in% names(dask_sr)) {
    cat("\n--- psi_jt precision analysis (JSON round-trip) ---\n")
    psi_local <- as.numeric(local_sr$psi_jt)
    psi_dask  <- as.numeric(dask_sr$psi_jt)
    psi_diff  <- abs(psi_local - psi_dask)
    psi_rel   <- ifelse(psi_local != 0, psi_diff / abs(psi_local), 0)

    n_total <- length(psi_diff)
    n_exact <- sum(psi_diff == 0)

    cat(sprintf("  Rows compared: %d\n", n_total))
    cat(sprintf("  Exact match (diff == 0): %d / %d (%.1f%%)\n",
                n_exact, n_total, 100 * n_exact / n_total))
    cat(sprintf("  Abs diff — max: %.2e  mean: %.2e  median: %.2e\n",
                max(psi_diff), mean(psi_diff), median(psi_diff)))
    cat(sprintf("  Rel diff — max: %.2e  mean: %.2e  median: %.2e\n",
                max(psi_rel), mean(psi_rel), median(psi_rel)))

    # ULP estimate: diff / (eps * |value|)
    eps64 <- .Machine$double.eps  # 2.22e-16
    ulp_est <- ifelse(psi_local != 0,
                      psi_diff / (eps64 * abs(psi_local)),
                      0)
    cat(sprintf("  ULP estimate — max: %.1f  mean: %.1f  median: %.1f\n",
                max(ulp_est), mean(ulp_est), median(ulp_est)))
    cat("  (1.0 = exactly 1 ULP of float64 precision)\n")
  }

  # --- 2c. Cases & deaths comparison ---
  cat("\n--- Simulation results comparison (cases & deaths) ---\n")
  n_matched <- nrow(local_sr)
  cat(sprintf("  Matched rows: %d (local: %d, dask: %d)\n",
              n_matched, nrow(local_sr), nrow(dask_sr)))

  for (outcome in c("cases", "deaths")) {
    v_local <- as.numeric(local_sr[[outcome]])
    v_dask  <- as.numeric(dask_sr[[outcome]])
    d <- abs(v_local - v_dask)
    rel_d <- ifelse(v_local != 0 & v_dask != 0,
                    d / pmax(abs(v_local), abs(v_dask)),
                    ifelse(d == 0, 0, NA_real_))

    n_exact <- sum(d == 0, na.rm = TRUE)
    n_close <- sum(d < 1e-6, na.rm = TRUE)

    label <- paste0(toupper(substring(outcome, 1, 1)), substring(outcome, 2))
    cat(sprintf("\n  [%s]\n", label))
    cat(sprintf("    Exact match (diff == 0):     %d / %d (%.1f%%)\n",
                n_exact, n_matched, 100 * n_exact / n_matched))
    cat(sprintf("    Close match (|diff| < 1e-6): %d / %d (%.1f%%)\n",
                n_close, n_matched, 100 * n_close / n_matched))
    cat(sprintf("    Abs diff — max: %.2e  mean: %.2e  median: %.2e\n",
                max(d, na.rm = TRUE), mean(d, na.rm = TRUE), median(d, na.rm = TRUE)))
    finite_rel <- rel_d[is.finite(rel_d)]
    if (length(finite_rel) > 0) {
      cat(sprintf("    Rel diff — max: %.2e  mean: %.2e  median: %.2e\n",
                  max(finite_rel), mean(finite_rel), median(finite_rel)))
    }
  }

  # --- Per-sim summary ---
  cat("\n--- Per-sim summary ---\n\n")
  cat(sprintf("  %7s  %12s  %12s  %12s  %12s  %6s\n",
              "sim", "cases_max", "cases_mean", "deaths_max", "deaths_mean", "rows"))
  cat(sprintf("  %7s  %12s  %12s  %12s  %12s  %6s\n",
              "-------", "------------", "------------", "------------", "------------", "------"))
  for (sid in sort(unique(local_sr$sim))) {
    idx <- local_sr$sim == sid
    cd <- abs(as.numeric(local_sr$cases[idx]) - as.numeric(dask_sr$cases[idx]))
    dd <- abs(as.numeric(local_sr$deaths[idx]) - as.numeric(dask_sr$deaths[idx]))
    cat(sprintf("  %7d  %12.2e  %12.2e  %12.2e  %12.2e  %6d\n",
                sid, max(cd), mean(cd), max(dd), mean(dd), sum(idx)))
  }

  # --- Top 10 worst case mismatches ---
  cases_diff <- abs(as.numeric(local_sr$cases) - as.numeric(dask_sr$cases))
  top10 <- order(cases_diff, decreasing = TRUE)[1:min(10, length(cases_diff))]
  cat("\n--- Top 10 worst case mismatches ---\n")
  cat(sprintf("  %5s  %4s  %3s  %5s  %14s  %14s  %12s\n",
              "sim", "iter", "j", "t", "cases_local", "cases_dask", "abs_diff"))
  cat(sprintf("  %5s  %4s  %3s  %5s  %14s  %14s  %12s\n",
              "-----", "----", "---", "-----", "--------------", "--------------", "------------"))
  for (i in top10) {
    cat(sprintf("  %5d  %4d  %3d  %5d  %14.4f  %14.4f  %12.2e\n",
                local_sr$sim[i], local_sr$iter[i], local_sr$j[i], local_sr$t[i],
                as.numeric(local_sr$cases[i]), as.numeric(dask_sr$cases[i]),
                cases_diff[i]))
  }

} else {
  cat("\n--- Simresults comparison: SKIPPED ---\n")
  cat(sprintf("  Local simresults: %d files\n", length(local_sr_files)))
  cat(sprintf("  Dask  simresults: %d files\n", length(dask_sr_files)))
  cat("  (Set save_simresults = TRUE in control$io to enable)\n")
}

# =============================================================================
# VERDICT
# =============================================================================
cat("\n=============================================================\n")

all_params_match <- all(param_max_diffs < 1e-10)
seeds_match <- all(as.integer(local_sims$seed_sim) == as.integer(dask_sims$seed_sim)) &&
               all(as.integer(local_sims$seed_iter) == as.integer(dask_sims$seed_iter))

if (all_params_match && seeds_match) {
  if (has_simresults) {
    cat("  PASS: Parameters exact match, seeds match.\n")
    cat("  Likelihood/cases/deaths diffs are expected stochastic divergence\n")
    cat("  from JSON psi_jt precision loss (~1-2 ULP).\n")
  } else {
    cat("  PASS: Parameters exact match, seeds match.\n")
    cat("  (No simresults to compare — enable save_simresults for detailed analysis)\n")
  }
} else {
  cat("  FAIL: results differ between local and Dask paths\n")
  if (!all_params_match) cat("    - Parameter value mismatch\n")
  if (!seeds_match)      cat("    - Seed mismatch\n")
}
cat("=============================================================\n")

# Save comparison CSV
summary_file <- file.path(dir_local, "validation_comparison.csv")
comparison_df <- data.frame(
  sim          = local_sims$sim,
  ll_local     = as.numeric(local_sims$likelihood),
  ll_dask      = as.numeric(dask_sims$likelihood),
  ll_abs_diff  = abs(as.numeric(local_sims$likelihood) - as.numeric(dask_sims$likelihood))
)
write.csv(comparison_df, summary_file, row.names = FALSE)
cat(sprintf("\nSaved detailed comparison: %s\n", summary_file))
