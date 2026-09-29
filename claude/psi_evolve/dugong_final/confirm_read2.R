# =============================================================================
# confirm_read2.R -- THE ONE PERMITTED READ OF BLOCK 10, THE PHASE-2/3 HOLDOUT.
#
# This is the successor to confirm_read.R, which is SPENT: blocks 7/8/9 were read
# once on 2026-09-19 and are recorded in confirm_read_log.txt. They must never be
# read again. This script therefore reads ONLY `split == "confirmation2"` --
# block 10, cutoff 2026-04-15 -- and will refuse if it is ever pointed elsewhere.
#
# WHY BLOCK 10 EXISTS. Phase 2 had no sealed holdout left. The panel rebuild of
# 2026-09-21 extended the target to 2026-08-13, which made a genuinely disjoint
# block possible: block 10's scoring window opens 2026-04-30, after block 9's
# window closes 2026-04-17.
#
# POWER, STATED UP FRONT. 130 cells over 12 countries; 44 cells over 9 countries
# at weeks 9-13, against 200 in the spent 3-block holdout. It supports the
# phase-2 TREND gate (dir_acc over the full horizon, 130 cells). It CANNOT
# resolve small weeks-9-13 MAE differences. Registered before the read so a
# small number is not mistaken for a failure, nor a large one for a triumph.
#
# TARGET PROVENANCE. Block 10 is scored on the REBUILT panel (2026-09-21).
# Phase-1 arms were fitted and scored on the frozen panel
# (..._frozen_2026-09-17.csv), whose target differs materially for CMR, KEN, RWA
# and NGA because the surveillance refresh revised historical case counts. Do
# NOT tabulate a block-10 number against a phase-1 number.
#
# GUARDS: refuses without CONFIRM2_I_MEAN_IT=yes; refuses if a previous read is
# recorded; scores only confirmation2; appends an indelible record.
#
# usage: CONFIRM2_I_MEAN_IT=yes PSI_CACHE=<cache> Rscript confirm_read2.R
# =============================================================================
HERE <- Sys.getenv("PSI_HERE", "/home/jgiles/psi_evolve")
LOG  <- file.path(HERE, "confirm_read2_log.txt")

# GUARDS FIRST, before any library() or source(). The first version of this
# script sourced shape_metrics.R above the guards, so running it without the
# flag died with "cannot open the connection" instead of saying what was wrong.
# A guarded script must assert its guard before it can fail for any other reason.
if (!identical(Sys.getenv("CONFIRM2_I_MEAN_IT"), "yes"))
     stop("confirm_read2: refusing. Set CONFIRM2_I_MEAN_IT=yes -- this consumes the\n",
          "  one permitted read of BLOCK 10 (PROTOCOL 5.3).")
if (file.exists(LOG))
     stop("confirm_read2: a previous read is already recorded in ", LOG, ".\n",
          "  The holdout is write-once. Reading it again makes every confirmation\n",
          "  number in this programme uninterpretable.")

suppressMessages(library(MOSAIC))
source(file.path(HERE, "shape_metrics.R"))

CACHE <- Sys.getenv("PSI_CACHE", "")
if (!nzchar(CACHE) || !dir.exists(CACHE))
     stop("confirm_read2: set PSI_CACHE to the arm's psi cache directory.")
CANON <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
VAR   <- "target_D_rate_per_country_floored"

grid <- utils::read.csv(file.path(HERE, "EVAL_GRID.csv"), stringsAsFactors = FALSE)
grid <- grid[grid$grid == "prod" & grid$split == "confirmation2", ]   # <-- BLOCK 10 ONLY
if (nrow(grid) != 1L)
     stop("confirm_read2: expected exactly 1 confirmation2 block, found ", nrow(grid),
          ". Refusing rather than guessing which block is the holdout.")
for (k in c("cutoff", "test_start", "test_end")) grid[[k]] <- as.Date(grid[[k]])

W <- utils::read.csv(file.path(HERE, "weights_frozen.csv"), stringsAsFactors = FALSE)
W$w <- W$w_sqrt / sum(W$w_sqrt); pool <- W$iso_code
wts <- stats::setNames(W$w, W$iso_code)
obs <- utils::read.csv(CANON, stringsAsFactors = FALSE)[, c("iso_code", "date", VAR)]
names(obs)[3] <- "observed"; obs$date <- as.Date(obs$date)
obs <- obs[is.finite(obs$observed) & obs$iso_code %in% pool, ]

T0 <- grid$cutoff[1]
f  <- file.path(CACHE, sprintf("psi_%s.csv", format(T0)))
if (!file.exists(f))
     stop("confirm_read2: the arm has no psi for block 10 (", basename(f), ").\n",
          "  Fit it at cutoff ", format(T0), " before reading the holdout.")

cat("BLOCK 10 ONLY -- cutoff", format(T0), "| window", format(grid$test_start[1]),
    "..", format(grid$test_end[1]), "\n")
cat("psi cache:", CACHE, "\n")
cat("target: REBUILT panel (2026-09-21). NOT comparable to phase-1 numbers.\n\n")

p <- utils::read.csv(f, stringsAsFactors = FALSE); p$date <- as.Date(p$date)
b <- p[p$iso_code %in% pool & p$date >= grid$test_start[1] & p$date <= grid$test_end[1],
       c("iso_code", "date", "psi")]
b$wk <- as.integer(floor(as.numeric(b$date - (T0 + 14L)) / 7)) + 1L
bl_fn <- getFromNamespace(".rcv_baseline", "MOSAIC")
b$pers <- NA_real_
for (iso in unique(b$iso_code)) {
     oi <- obs[obs$iso_code == iso & obs$date <= T0, ]
     if (nrow(oi) < 8L) next
     oi <- oi[order(oi$date), ]
     j <- b$iso_code == iso
     b$pers[j] <- bl_fn(oi, b$date[j], "persistence")$point
}
m <- merge(b, obs, by = c("iso_code", "date"))
m <- m[is.finite(m$observed) & is.finite(m$psi) & m$wk >= 1 & m$wk <= 13, ]
m$fold <- "b10"

wm <- function(x, iso) { w <- wts[match(iso, names(wts))]
                         sum(w * x, na.rm = TRUE) / sum(w[is.finite(x)]) }
out <- c()
cat("=== BLOCK 10 (SEALED) -- MAE by horizon band ===\n")
for (rg in list(1:4, 5:8, 9:13)) {
     z <- m[m$wk %in% rg, ]; if (!nrow(z)) next
     a <- wm(abs(z$observed - z$psi), z$iso_code)
     pq <- if (any(is.finite(z$pers))) wm(abs(z$observed - z$pers), z$iso_code) else NA_real_
     line <- sprintf("weeks %2d-%2d : psi %.4f  persistence %.4f -> %+.1f%%  (n=%d)",
                     min(rg), max(rg), a, pq, 100 * (pq - a) / pq, nrow(z))
     cat(line, "\n"); out <- c(out, line)
}
sm <- .shape_metrics(m, "psi", wts, "fold")
line <- sprintf("\n=== TREND GATE (the pre-registered axis) ===\n  dir_acc %.3f | dcor %s | sd_ratio %.2f | bias %.2f | n=%d cells, %d isos",
                sm[["dir_acc"]],
                if (is.finite(sm[["dcor"]])) sprintf("%.3f", sm[["dcor"]]) else "undef",
                sm[["sd_ratio"]], sm[["bias"]], nrow(m), length(unique(m$iso_code)))
cat(line, "\n")
cat("  reference: phase-1 psi arms dir_acc 0.45-0.50 | climatology 0.57 | constant 0.00\n")
out <- c(out, line)

writeLines(c(sprintf("block-10 confirmation read performed %s", Sys.time()),
             sprintf("psi cache: %s", CACHE),
             sprintf("block: %s (%s .. %s)", format(T0),
                     format(grid$test_start[1]), format(grid$test_end[1])),
             "target: REBUILT panel 2026-09-21", out), LOG)
cat("\nRecorded in", LOG, "-- block 10 is now spent.\n")
