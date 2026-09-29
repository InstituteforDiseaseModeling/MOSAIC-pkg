# Localize the horizon decay: is it the MODEL or the POST-PROCESSING?
# psi = bias-corrected(smoothed(seed-ensemble)); pred_smooth = before the
# per-country affine; pred_raw = before the LOESS smoothing. All three are in
# every cached psi file, so this costs nothing.
suppressMessages(library(MOSAIC))
CACHE <- "/home/jgiles/MOSAIC/MOSAIC-pkg/claude/forecast_cv_ocv4_q2yr/psi_cache"
HERE  <- "/home/jgiles/psi_evolve"
CANON <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
VAR   <- "target_D_rate_per_country_floored"
grid <- utils::read.csv(file.path(HERE, "EVAL_GRID.csv"), stringsAsFactors = FALSE)
grid <- grid[grid$grid == "prod" & grid$split == "selection", ]
for (k in c("cutoff","test_start","test_end")) grid[[k]] <- as.Date(grid[[k]])
pool <- utils::read.csv(file.path(HERE,"weights_frozen.csv"), stringsAsFactors=FALSE)$iso_code
obs <- utils::read.csv(CANON, stringsAsFactors = FALSE)[, c("iso_code","date",VAR)]
names(obs)[3] <- "observed"; obs$date <- as.Date(obs$date)
obs <- obs[is.finite(obs$observed) & obs$iso_code %in% pool, ]

rows <- list(); pre <- list()
for (i in seq_len(nrow(grid))) {
  f <- file.path(CACHE, sprintf("psi_%s.csv", format(grid$cutoff[i])))
  if (!file.exists(f)) next
  p <- utils::read.csv(f, stringsAsFactors = FALSE); p$date <- as.Date(p$date)
  p <- p[p$iso_code %in% pool, c("iso_code","date","psi","pred_smooth","pred_raw")]
  m <- merge(p, obs, by = c("iso_code","date"))
  # pre-cutoff (in-sample) reference level, last 26 weeks before the cutoff
  z <- m[m$date <= grid$cutoff[i] & m$date > grid$cutoff[i] - 182L, ]
  if (nrow(z)) { z$block <- grid$block[i]; pre[[length(pre)+1L]] <- z }
  b <- m[m$date >= grid$test_start[i] & m$date <= grid$test_end[i], ]
  if (!nrow(b)) next
  b$block <- grid$block[i]
  b$wk <- as.integer(floor(as.numeric(b$date - (grid$cutoff[i] + 14L)) / 7)) + 1L
  rows[[length(rows)+1L]] <- b
}
d  <- do.call(rbind, rows); d <- d[d$wk >= 1 & d$wk <= 13, ]
pp <- do.call(rbind, pre)

cat("=== IN-SAMPLE reference (last 26 wk before each cutoff) ===\n")
cat(sprintf("psi %.4f   pred_smooth %.4f   pred_raw %.4f   observed %.4f   (n=%d)\n\n",
            mean(pp$psi), mean(pp$pred_smooth), mean(pp$pred_raw), mean(pp$observed), nrow(pp)))

cat("=== OUT-OF-SAMPLE mean level by weeks past the embargo ===\n")
cat(sprintf("%3s %6s  %8s %11s %9s %9s\n","wk","n","psi","pred_smooth","pred_raw","observed"))
for (w in sort(unique(d$wk))) {
  z <- d[d$wk == w, ]
  cat(sprintf("%3d %6d  %8.4f %11.4f %9.4f %9.4f\n", w, nrow(z),
              mean(z$psi), mean(z$pred_smooth), mean(z$pred_raw), mean(z$observed)))
}
r <- range(d$wk)
f1 <- d[d$wk <= 2, ]; f2 <- d[d$wk >= 9, ]
cat(sprintf("\ndecay wk<=2 -> wk>=9:  psi %.4f -> %.4f (%.0f%%)   pred_smooth %.4f -> %.4f (%.0f%%)   pred_raw %.4f -> %.4f (%.0f%%)\n",
    mean(f1$psi), mean(f2$psi), 100*mean(f2$psi)/mean(f1$psi),
    mean(f1$pred_smooth), mean(f2$pred_smooth), 100*mean(f2$pred_smooth)/mean(f1$pred_smooth),
    mean(f1$pred_raw), mean(f2$pred_raw), 100*mean(f2$pred_raw)/mean(f1$pred_raw)))
cat(sprintf("observed over the same split: %.4f -> %.4f (%.0f%%)\n",
    mean(f1$observed), mean(f2$observed), 100*mean(f2$observed)/mean(f1$observed)))
