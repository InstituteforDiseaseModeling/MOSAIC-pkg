# =============================================================================
# score_arm_driver.R -- score one arm's psi cache against the FROZEN evaluation
# grid. Kept separate from run_arm.R so a scoring bug never costs a refit.
#
# env: PSI_ARM, PSI_MODE (selection|confirmation), PSI_CACHE (dir), PSI_TAG
# =============================================================================
suppressMessages(library(MOSAIC))
HERE  <- "/home/jgiles/psi_evolve"
ARM   <- Sys.getenv("PSI_ARM", "A000")
MODE  <- Sys.getenv("PSI_MODE", "selection")
CACHE <- Sys.getenv("PSI_CACHE", file.path(HERE, paste0("psi_cache_", ARM)))
source(file.path(HERE, "score_psi_arm.R"))

grid <- utils::read.csv(file.path(HERE, "EVAL_GRID.csv"), stringsAsFactors = FALSE)
grid$cutoff <- as.Date(grid$cutoff)
grid$test_start <- as.Date(grid$test_start); grid$test_end <- as.Date(grid$test_end)

# Observed transmission intensity, read from the CANONICAL PANEL -- not from
# model/input/data_psi_suitability.csv.
#
# est_suitability() writes its side-effect files (data_psi_suitability.csv,
# pred_psi_suitability_day.csv, psi_suitability_config.json) to fixed paths under
# PATHS$MODEL_INPUT. When N arm shards run concurrently they all write the SAME
# paths, so that file is racing and can be read torn. The per-cutoff psi cache is
# safe (filenames are cutoff-keyed and disjoint), but the observed series is not.
#
# The target is a column of the panel and is cutoff-independent, so read it from
# there: same values, no race. RESPONSE_VAR must match the arm's spec.
RESPONSE_VAR <- Sys.getenv("PSI_RESPONSE_VAR", "target_D_rate_per_country_floored")
panel_f <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
obs <- utils::read.csv(panel_f, stringsAsFactors = FALSE)[, c("iso_code", "date", RESPONSE_VAR)]
names(obs)[3] <- "observed"
obs$date <- as.Date(obs$date)
obs <- obs[is.finite(obs$observed), ]
obs <- obs[!duplicated(obs[, c("iso_code", "date")]), ]
cat("observed series from panel column '", RESPONSE_VAR, "': ", nrow(obs), " rows\n", sep = "")

preds <- list(); used <- integer(0)
for (i in seq_len(nrow(grid))) {
     f <- file.path(CACHE, sprintf("psi_%s.csv", format(grid$cutoff[i])))
     if (!file.exists(f)) next
     p <- utils::read.csv(f, stringsAsFactors = FALSE)
     p$date <- as.Date(p$date)
     # THIS cutoff's block only: a model may only be scored on the block it was
     # fitted to forecast.
     p <- p[p$date >= grid$test_start[i] & p$date <= grid$test_end[i], ]
     if (!nrow(p)) next
     p$fold <- grid$block[i]
     keepc <- c("iso_code", "date", "fold", "psi", "q025", "q25", "q75", "q975")
     if ("pred_smooth" %in% names(p)) keepc <- c(keepc, "pred_smooth")
     if ("pred_raw" %in% names(p))    keepc <- c(keepc, "pred_raw")
     preds[[length(preds) + 1L]] <- p[, keepc]
     used <- c(used, grid$block[i])
}
if (!length(preds)) stop("score_arm_driver: no psi cache files matched the evaluation grid")
pred <- do.call(rbind, preds)
folds <- data.frame(fold = grid$block, train_end = grid$cutoff,
                    test_start = grid$test_start, test_end = grid$test_end)

cat("arm:", ARM, "| mode:", MODE, "| cutoffs present:", length(used),
    "of", nrow(grid), "| pred rows:", nrow(pred), "\n")
IM  <- Sys.getenv("PSI_INTERVAL_MODE", "seed")
PC  <- Sys.getenv("PSI_COLUMN", "psi")
res <- score_psi_arm(ARM, pred, obs, folds, mode = MODE, dir = HERE, verbose = TRUE,
                     interval_mode = IM, psi_column = PC)

cat("\n=== PER-COUNTRY WIS-SKILL vs persistence ===\n")
pi <- res$per_iso[order(-res$per_iso$w), ]
print(data.frame(iso = pi$iso_code, w = round(pi$w, 4),
                 wis_skill = round(pi$wis_skill, 4)), row.names = FALSE)
saveRDS(res, file.path(HERE, sprintf("score_%s_%s_%s_%s%s.rds", ARM, MODE, IM, PC,
                                     Sys.getenv("PSI_TAG", ""))))
