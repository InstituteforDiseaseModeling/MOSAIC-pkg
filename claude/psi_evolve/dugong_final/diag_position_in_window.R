# =============================================================================
# C6 -- POSITION-IN-WINDOW diagnostic. Class C: zero compute, existing cache.
#
# WHY. P001's per-horizon skill is NON-MONOTONE: h1mo -0.189, h2mo -0.513,
# h3mo -0.296. Skill is 1 - WIS_model/WIS_base, so a dip can come from the
# numerator (the model gets worse) or the denominator (the baseline gets
# better). Those have opposite implications: a model-side hump ~1-2 months past
# the cutoff would be a FIXABLE ARTEFACT (LOESS edge effects, the bias-correction
# affine, a carry-forward tail), while a baseline-side effect is just persistence
# being unusually good at that lead. OBJECTIVE section 3 has listed this
# diagnostic as "reported" since v1 and it has never been computed.
#
# Decomposes by weeks-past-oos0, pooled over cutoffs and the 16-country pool.
# usage: Rscript diag_position_in_window.R [CACHE_DIR]
# =============================================================================
suppressMessages(library(MOSAIC))
args  <- commandArgs(trailingOnly = TRUE)
CACHE <- if (length(args)) args[1] else
     "/home/jgiles/MOSAIC/MOSAIC-pkg/claude/forecast_cv_ocv4_q2yr/psi_cache"
HERE  <- "/home/jgiles/psi_evolve"
CANON <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
VAR   <- "target_D_rate_per_country_floored"

grid <- utils::read.csv(file.path(HERE, "EVAL_GRID.csv"), stringsAsFactors = FALSE)
grid <- grid[grid$grid == "prod" & grid$split == "selection", ]
for (k in c("cutoff","test_start","test_end")) grid[[k]] <- as.Date(grid[[k]])
W <- utils::read.csv(file.path(HERE, "weights_frozen.csv"), stringsAsFactors = FALSE)
pool <- W$iso_code
obs <- utils::read.csv(CANON, stringsAsFactors = FALSE)[, c("iso_code","date",VAR)]
names(obs)[3] <- "observed"; obs$date <- as.Date(obs$date)
obs <- obs[is.finite(obs$observed) & obs$iso_code %in% pool, ]

wis_fn <- getFromNamespace(".rcv_wis", "MOSAIC")
bl_fn  <- getFromNamespace(".rcv_baseline", "MOSAIC")

rows <- list()
for (i in seq_len(nrow(grid))) {
  f <- file.path(CACHE, sprintf("psi_%s.csv", format(grid$cutoff[i])))
  if (!file.exists(f)) next
  p <- utils::read.csv(f, stringsAsFactors = FALSE); p$date <- as.Date(p$date)
  p <- p[p$iso_code %in% pool, ]
  blk <- p[p$date >= grid$test_start[i] & p$date <= grid$test_end[i], ]
  if (!nrow(blk)) next
  for (iso in pool) {
    m <- merge(blk[blk$iso_code == iso, c("date","psi","q025","q25","q75","q975")],
               obs[obs$iso_code == iso, c("date","observed")], by = "date")
    if (nrow(m) < 4) next
    o  <- obs[obs$iso_code == iso, ]
    is_df <- o[o$date <= grid$cutoff[i], ]
    if (nrow(is_df) < 8) next
    b <- bl_fn(is_df, m$date, "persistence")
    if (!any(is.finite(b$point))) next
    wm <- wis_fn(m$observed, m$psi, m$q25, m$q75, m$q025, m$q975)
    wb <- wis_fn(m$observed, b$point, b$pi50_lo, b$pi50_hi, b$pi95_lo, b$pi95_hi)
    rows[[length(rows)+1L]] <- data.frame(
      iso_code = iso, block = grid$block[i],
      wk = as.integer(floor(as.numeric(m$date - (grid$cutoff[i] + 14L)) / 7)) + 1L,
      psi = m$psi, observed = m$observed, base = b$point,
      wis_m = wm, wis_b = wb, stringsAsFactors = FALSE)
  }
}
d <- do.call(rbind, rows)
d <- d[d$wk >= 1 & d$wk <= 13, ]
cat("cells:", nrow(d), " blocks:", length(unique(d$block)),
    " countries:", length(unique(d$iso_code)), "\n\n")

cat("=== BY WEEKS PAST THE EMBARGO (oos0 = cutoff + 14d) ===\n")
cat(sprintf("%3s %6s  %8s %8s  %8s %8s  %8s %8s %8s\n",
            "wk","n","WIS_mod","WIS_base","skill","err_mod","err_base","psi_mean","obs_mean"))
for (w in sort(unique(d$wk))) {
  z <- d[d$wk == w, ]
  wm <- mean(z$wis_m, na.rm = TRUE); wb <- mean(z$wis_b, na.rm = TRUE)
  cat(sprintf("%3d %6d  %8.4f %8.4f  %+8.3f %+8.4f %+8.4f  %8.4f %8.4f\n",
              w, nrow(z), wm, wb, 1 - wm/wb,
              mean(z$psi - z$observed, na.rm = TRUE),
              mean(z$base - z$observed, na.rm = TRUE),
              mean(z$psi, na.rm = TRUE), mean(z$observed, na.rm = TRUE)))
}
cat("\nREAD: if WIS_mod humps at wk 5-9 while WIS_base is flat, the h2mo dip is\n",
    "MODEL-side (a fixable transient). If WIS_base DIPS there instead, the dip is\n",
    "the baseline being unusually good and there is nothing to fix.\n", sep = "")
