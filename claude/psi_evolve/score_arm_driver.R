# =============================================================================
# score_arm_driver.R -- score one arm's psi cache against the FROZEN v3
# evaluation grid (the 9 OCV-4 production-validation cutoffs). Kept separate
# from run_arm.R so a scoring bug never costs a refit.
#
# env: PSI_ARM, PSI_MODE (selection|confirmation), PSI_CACHE (dir), PSI_TAG,
#      PSI_INTERVAL_MODE (seed|residual), PSI_COLUMN (psi|pred_smooth|pred_raw),
#      PSI_RESPONSE_VAR
# =============================================================================
suppressMessages(library(MOSAIC))
HERE  <- "/home/jgiles/psi_evolve"
ARM   <- Sys.getenv("PSI_ARM", "P000")
MODE  <- Sys.getenv("PSI_MODE", "selection")
CACHE <- Sys.getenv("PSI_CACHE", file.path(HERE, paste0("psi_cache_", ARM)))
source(file.path(HERE, "score_psi_arm.R"))

grid <- utils::read.csv(file.path(HERE, "EVAL_GRID.csv"), stringsAsFactors = FALSE)
grid <- grid[grid$grid == "prod", ]
for (k in c("cutoff", "test_start", "test_end")) grid[[k]] <- as.Date(grid[[k]])

# ---- Observed series -------------------------------------------------------
# Read from the CANONICAL PANEL, not from model/input/data_psi_suitability.csv:
# est_suitability() writes its side-effect files to fixed paths under
# PATHS$MODEL_INPUT, so N concurrent arm shards race on that file. The per-cutoff
# psi cache is safe (filenames are cutoff-keyed and disjoint).
#
# ONE FIXED SERIES FOR EVERY ARM, and this is load-bearing. WIS-skill is a ratio
# of model WIS to baseline WIS on the same observed series, so scoring two arms
# against two different series is not a comparison at all -- a country whose
# target is rescaled gets a different baseline too.
#
# MEASURED CAVEAT (2026-09-18). `target_D_rate_per_country_floored` is normalised
# per country over the panel's compile window, so the per-cutoff leak-free v7.4
# panels (compiled 2015-01-01..cutoff) do NOT carry the same target values as the
# canonical panel (2000-01-06..2027-02-04). Over the 16-country pool x 9 scored
# blocks: 1.23% of rows differ by > 0.01 and 0.70% by > 0.05, but the tail is
# heavy (TZA 1.00, SSD 0.61, COD 0.35). Since a cell's WIS is a mean over ~13
# weekly rows, one such row moves that cell.
#
# Consequence, recorded rather than hidden: an arm trained on a per-cutoff panel
# (P001, and any v7.4 arm) is scored against a series it was not trained on, and
# the direction of that bias FAVOURS the canonical-panel arms (P000 / v7.3).
# That is the conservative direction for adoption -- it makes a v7.4 win harder,
# not easier -- so it is acceptable, but a v7.4-vs-v7.3 verdict must be reported
# with `audit_target_scale.R` alongside it. See BACKLOG D7: the deeper issue is
# that the canonical target's normalisation window is itself look-ahead.
RESPONSE_VAR <- Sys.getenv("PSI_RESPONSE_VAR", "target_D_rate_per_country_floored")
panel_f <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
obs <- utils::read.csv(panel_f, stringsAsFactors = FALSE)[, c("iso_code", "date", RESPONSE_VAR)]
names(obs)[3] <- "observed"
obs$date <- as.Date(obs$date)
obs <- obs[is.finite(obs$observed), ]
obs <- obs[!duplicated(obs[, c("iso_code", "date")]), ]
cat("observed series from canonical panel column '", RESPONSE_VAR, "': ", nrow(obs),
    " rows\n", sep = "")
if (length(list.files(CACHE, pattern = "^panel_.*\\.csv$")))
     cat("NOTE: this cache carries per-cutoff panels, so its training target is NOT the\n",
         "      canonical one. Run audit_target_scale.R and report it with the score.\n", sep = "")

# ---- Load the arm's psi cache ---------------------------------------------
preds <- list(); pres <- list(); used <- integer(0); missing <- character(0)
for (i in seq_len(nrow(grid))) {
     f <- file.path(CACHE, sprintf("psi_%s.csv", format(grid$cutoff[i])))
     if (!file.exists(f)) { missing <- c(missing, format(grid$cutoff[i])); next }
     p <- utils::read.csv(f, stringsAsFactors = FALSE)
     p$date <- as.Date(p$date)
     praw <- p
     # THIS cutoff's block only: a model may only be scored on the block it was
     # fitted to forecast.
     p <- p[p$date >= grid$test_start[i] & p$date <= grid$test_end[i], ]
     if (!nrow(p)) next
     p$fold <- grid$block[i]
     keepc <- c("iso_code", "date", "fold", "psi", "q025", "q25", "q75", "q975")
     if ("pred_smooth" %in% names(p)) keepc <- c(keepc, "pred_smooth")
     if ("pred_raw" %in% names(p))    keepc <- c(keepc, "pred_raw")
     preds[[length(preds) + 1L]] <- p[, keepc]
     # D2: keep this fold's PRE-CUTOFF predictions too. Residual-mode intervals
     # are estimated from them; without this the sample is the block itself (12
     # points for block 2, zero for block 1).
     pc <- praw[praw$date <= grid$cutoff[i], , drop = FALSE]
     if (nrow(pc)) { pc$fold <- grid$block[i]
       pres[[length(pres) + 1L]] <- pc[, intersect(keepc, names(pc))] }
     used <- c(used, grid$block[i])
}
if (!length(preds)) stop("score_arm_driver: no psi cache files matched the evaluation grid in ", CACHE)
pred <- do.call(rbind, preds)
pred_pre <- if (length(pres)) do.call(rbind, pres) else NULL

# folds carries ONLY the four geometry columns; the scorer re-derives the split
# and the horizon boundaries from EVAL_GRID.csv itself and hard-errors if this
# frame disagrees with it (v3).
folds <- data.frame(fold = grid$block, train_end = grid$cutoff,
                    test_start = grid$test_start, test_end = grid$test_end)
folds <- folds[folds$fold %in% used, ]

cat("arm:", ARM, "| mode:", MODE, "| cutoffs present:", length(used), "of", nrow(grid),
    "| pred rows:", nrow(pred), "\n")
if (length(missing))
     cat("MISSING cutoffs (not scored):", paste(missing, collapse = ", "), "\n")

IM  <- Sys.getenv("PSI_INTERVAL_MODE", "seed")
PC  <- Sys.getenv("PSI_COLUMN", "psi")
res <- score_psi_arm(ARM, pred, obs, folds, mode = MODE, dir = HERE, verbose = TRUE,
                     interval_mode = IM, psi_column = PC, pred_pre = pred_pre)

cat("\n=== PER-COUNTRY WIS-SKILL vs persistence ===\n")
pi <- res$per_iso[order(-res$per_iso$w), ]
print(data.frame(iso = pi$iso_code, w = round(pi$w, 4),
                 wis_skill = round(pi$wis_skill, 4)), row.names = FALSE)

cat("\n=== BASELINE PANEL (same cells) ===\n")
print(data.frame(baseline = names(res$S_by_baseline),
                 S = round(vapply(res$S_by_baseline, function(z) z$S, numeric(1)), 4),
                 n_beat = vapply(res$S_by_baseline, function(z) z$n_beat, integer(1)),
                 n_scored = vapply(res$S_by_baseline, function(z) z$n_scored, integer(1))),
      row.names = FALSE)
cat("A6 (must beat `seasonal`):", if (isTRUE(res$beats_seasonal)) "PASS" else "FAIL", "\n")

cat("\n=== PER-HORIZON (vs persistence, same cells) ===\n")
print(data.frame(horizon = names(res$per_horizon),
                 S = round(vapply(res$per_horizon, function(z) z$S, numeric(1)), 4),
                 n_beat = vapply(res$per_horizon, function(z) z$n_beat, integer(1)),
                 cells = vapply(res$per_horizon, function(z) z$n_cells, integer(1))),
      row.names = FALSE)

saveRDS(res, file.path(HERE, sprintf("score_%s_%s_%s_%s%s.rds", ARM, MODE, IM, PC,
                                     Sys.getenv("PSI_TAG", ""))))
