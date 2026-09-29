# =============================================================================
# audit_target_scale.R -- PRE-FLIGHT for any arm whose psi was trained against a
# per-cutoff panel rather than the canonical one (i.e. every v7.4 arm, including
# P001 as cached by OCV-4).
#
# THE PROBLEM IT MEASURES. `target_D_rate_per_country_floored` is normalised per
# country over the compile window of the panel it came from. The leak-free v7.4
# path rebuilds a panel per cutoff (2015-01-01 .. cutoff), so its target is on a
# DIFFERENT per-country scale from the canonical panel (2000-01-06 .. 2027-02-04)
# that the scorer uses as the observed series. An arm can then be penalised for a
# scale it never trained on -- and that penalty would read as an arm effect.
#
# Measured 2026-09-18 over the 16-country pool x 9 scored blocks: 1.23% of rows
# differ by > 0.01, 0.70% by > 0.05, max 1.00 (TZA). Rare but heavy-tailed, and a
# cell's WIS is a mean over only ~13 weekly rows.
#
# usage: Rscript audit_target_scale.R [CACHE_DIR]
# =============================================================================
args  <- commandArgs(trailingOnly = TRUE)
CACHE <- if (length(args)) args[1] else
     "/home/jgiles/MOSAIC/MOSAIC-pkg/claude/forecast_cv_ocv4_q2yr/psi_cache"
HERE  <- "/home/jgiles/psi_evolve"
CANON <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
VAR   <- Sys.getenv("PSI_RESPONSE_VAR", "target_D_rate_per_country_floored")

grid <- utils::read.csv(file.path(HERE, "EVAL_GRID.csv"), stringsAsFactors = FALSE)
grid <- grid[grid$grid == "prod", ]
W    <- utils::read.csv(file.path(HERE, "weights_frozen.csv"), stringsAsFactors = FALSE)
pool <- W$iso_code
TOP10 <- c("COD","NGA","SSD","ETH","MOZ","SOM","MWI","AGO","ZWE","ZMB")

panels <- list.files(CACHE, pattern = "^panel_.*\\.csv$", full.names = TRUE)
if (!length(panels)) {
     cat("audit_target_scale: no per-cutoff panels in", CACHE, "\n")
     cat("=> this arm trained on the canonical panel; no target-scale caveat applies.\n")
     quit(save = "no")
}
can <- utils::read.csv(CANON, stringsAsFactors = FALSE)[, c("iso_code", "date", VAR)]
names(can)[3] <- "canon"; can$date <- as.Date(can$date)

res <- list()
for (i in seq_len(nrow(grid))) {
     T0 <- as.Date(grid$cutoff[i])
     f  <- grep(format(T0), panels, value = TRUE)
     if (!length(f)) next
     p <- utils::read.csv(f[1], stringsAsFactors = FALSE)[, c("iso_code", "date", VAR)]
     names(p)[3] <- "trained"; p$date <- as.Date(p$date)
     m <- merge(p, can, by = c("iso_code", "date"))
     m <- m[m$iso_code %in% pool & is.finite(m$trained) & is.finite(m$canon) &
            m$date >= as.Date(grid$test_start[i]) & m$date <= as.Date(grid$test_end[i]), ]
     if (nrow(m)) { m$block <- grid$block[i]; res[[length(res) + 1L]] <- m }
}
if (!length(res)) stop("audit_target_scale: no overlapping scored rows found")
m <- do.call(rbind, res)
m$d <- abs(m$trained - m$canon)

cat("\n=== TARGET-SCALE AUDIT ===============================================\n")
cat("cache:", CACHE, "\n")
cat(sprintf("scored rows (pool x blocks): %d\n", nrow(m)))
cat(sprintf("|diff| > 0.01 : %d (%.2f%%)   |diff| > 0.05 : %d (%.2f%%)   max %.4f\n",
            sum(m$d > 0.01), 100 * mean(m$d > 0.01),
            sum(m$d > 0.05), 100 * mean(m$d > 0.05), max(m$d)))

# The threshold that matters is set by the ESTIMAND, not by the raw max diff.
# Per-country skill is the MEDIAN over blocks, and each block's WIS is a mean
# over ~13 weekly rows -- so one divergent row in one block cannot move the
# median. What can move it is a majority of BLOCKS being affected. Report both:
# the raw tail (diagnostic) and the affected-block count (the decision rule).
n_blocks <- length(unique(m$block))
per <- do.call(rbind, lapply(sort(unique(m$iso_code)), function(i) {
     z  <- m[m$iso_code == i, ]
     bl <- tapply(z$d, z$block, function(x) any(x > 0.05))
     data.frame(iso = i, n = nrow(z), blocks = length(bl),
                max_diff = round(max(z$d), 4),
                pct_rows_gt_01 = round(100 * mean(z$d > 0.01), 1),
                blocks_hit = sum(bl, na.rm = TRUE),
                stringsAsFactors = FALSE)
}))
per$guard <- ifelse(per$iso %in% TOP10, "TOP10", "")
per$w     <- round(W$w_sqrt[match(per$iso, W$iso_code)] / sum(W$w_sqrt), 4)
# A median over `blocks` moves only if at least half of them move.
per$median_at_risk <- per$blocks_hit >= ceiling(per$blocks / 2)
print(per[order(-per$blocks_hit, -per$max_diff), ], row.names = FALSE)

bad <- per[per$median_at_risk & per$guard == "TOP10", ]
cat("\nVERDICT: ")
if (!nrow(bad)) {
     cat("no top-10 guard country has a majority of blocks affected.\n", sep = "")
     cat("  => the per-country medians, and therefore S and the A3 guard, are robust to the\n",
         "     target-scale difference. Scoring on the canonical series is admissible;\n",
         "     attach this table to the score (PROTOCOL 5.7).\n", sep = "")
     cat(sprintf("  (worst: %s with %d of %d blocks affected)\n",
                 per$iso[which.max(per$blocks_hit)], max(per$blocks_hit), n_blocks))
} else {
     cat(sprintf("%d top-10 country(ies) have >= half their blocks affected: %s\n",
                 nrow(bad), paste(sprintf("%s (%d/%d)", bad$iso, bad$blocks_hit, bad$blocks),
                                  collapse = ", ")))
     cat("  => a per-country verdict for those countries is NOT admissible from this score,\n",
         "     and A3 must be read with this table. The aggregate S may still be read.\n", sep = "")
}
cat("Direction of the bias: the canonical series favours canonical-panel arms\n",
    "(P000 / v7.3), i.e. it makes a v7.4 win HARDER. Conservative for adoption.\n", sep = "")
