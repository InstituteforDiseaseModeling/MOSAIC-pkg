# =============================================================================
# promote_gate.R -- apply the PHASE 2 promotion rule, which is on TREND, not MAE.
#
# WHY THIS EXISTS. Phase 1 established that MAE rewards abandoning trend: the
# MAE-optimal blend reached 0.1511 by compressing prediction amplitude to 43% of
# observed and dropping directional accuracy to 0.342 -- BELOW CHANCE, and worse
# than the raw psi it was built from. Sorting variants by MAE sorted them by how
# far they had given up on forecasting shape. So the gate cannot be MAE.
#
# THE RULE (registered in PLAN_PHASE2.md before any phase-2 arm was run):
#   PROMOTE if   dir_acc > 0.52 in >= 4 of the 6 selection blocks
#          AND   sd_ratio in [0.7, 1.3]            (not winning by flattening,
#                                                   nor by wild over-amplitude)
#   An MAE-only gain with dir_acc <= 0.50 is NOT a promotion.
#
# Reference points measured in phase 1: every psi arm 0.45-0.50; week-of-year
# climatology 0.57; a constant predictor 0.00.
#
# usage: Rscript promote_gate.R ARM [ARM ...]
# =============================================================================
suppressMessages(library(MOSAIC))
HERE  <- "/home/jgiles/psi_evolve"
CANON <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
VAR   <- "target_D_rate_per_country_floored"
PROD  <- "/home/jgiles/MOSAIC/MOSAIC-pkg/claude/forecast_cv_ocv4_q2yr/psi_cache"
source(file.path(HERE, "shape_metrics.R"))

DIR_MIN   <- 0.52
BLOCKS_OK <- 4L
SD_LO     <- 0.7
SD_HI     <- 1.3

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("promote_gate: name at least one arm")

grid <- utils::read.csv(file.path(HERE, "EVAL_GRID.csv"), stringsAsFactors = FALSE)
grid <- grid[grid$grid == "prod" & grid$split == "selection", ]
for (k in c("cutoff", "test_start", "test_end")) grid[[k]] <- as.Date(grid[[k]])
W <- utils::read.csv(file.path(HERE, "weights_frozen.csv"), stringsAsFactors = FALSE)
W$w <- W$w_sqrt / sum(W$w_sqrt); pool <- W$iso_code
wts <- stats::setNames(W$w, W$iso_code)
obs <- utils::read.csv(CANON, stringsAsFactors = FALSE)[, c("iso_code", "date", VAR)]
names(obs)[3] <- "observed"; obs$date <- as.Date(obs$date)
obs <- obs[is.finite(obs$observed) & obs$iso_code %in% pool, ]

cache_of <- function(a) if (identical(a, "P001")) PROD else
     file.path(HERE, paste0("psi_cache_", a))

cat(sprintf("PHASE 2 PROMOTION GATE: dir_acc > %.2f in >= %d of %d blocks,",
            DIR_MIN, BLOCKS_OK, nrow(grid)), "\n")
cat(sprintf("                        AND sd_ratio in [%.1f, %.1f]\n\n", SD_LO, SD_HI))

for (a in args) {
     d <- cache_of(a)
     if (!dir.exists(d)) { cat(sprintf("%-6s -- NO CACHE at %s\n", a, d)); next }
     have <- file.exists(file.path(d, sprintf("psi_%s.csv", format(grid$cutoff))))
     if (!all(have)) {
          cat(sprintf("%-6s -- PARTIAL GRID (%d of %d); refusing to gate a subset\n",
                      a, sum(have), nrow(grid)))
          next
     }
     per <- vapply(seq_len(nrow(grid)), function(i) {
          p <- utils::read.csv(file.path(d, sprintf("psi_%s.csv", format(grid$cutoff[i]))),
                               stringsAsFactors = FALSE)
          p$date <- as.Date(p$date)
          p <- p[p$iso_code %in% pool & p$date >= grid$test_start[i] &
                 p$date <= grid$test_end[i], c("iso_code", "date", "psi")]
          m <- merge(p, obs, by = c("iso_code", "date"))
          if (!nrow(m)) return(c(dir = NA_real_, sd = NA_real_))
          m$fold <- format(grid$cutoff[i])
          sm <- .shape_metrics(m, "psi", wts, "fold")
          c(dir = sm[["dir_acc"]], sd = sm[["sd_ratio"]])
     }, numeric(2))
     dir_v <- per["dir", ]; sd_v <- per["sd", ]
     n_ok  <- sum(dir_v > DIR_MIN, na.rm = TRUE)
     sd_m  <- mean(sd_v, na.rm = TRUE)
     pass  <- n_ok >= BLOCKS_OK && is.finite(sd_m) && sd_m >= SD_LO && sd_m <= SD_HI
     cat(sprintf("%-6s dir_acc per block: %s\n", a,
                 paste(sprintf("%.2f", dir_v), collapse = " ")))
     cat(sprintf("%-6s blocks over %.2f: %d of %d | mean sd_ratio %.2f -> %s\n\n",
                 "", DIR_MIN, n_ok, nrow(grid), sd_m,
                 if (pass) "**PROMOTE**" else "no promotion"))
}
cat("Reference (phase 1): psi arms dir_acc 0.45-0.50 | climatology 0.57 | constant 0.00\n")
