# Does the level correction help exactly where the TRUTH is high? If so it is a
# bet on high incidence, not a repair -- and it cannot be gated with information
# available at the cutoff.
HERE  <- "/home/jgiles/psi_evolve"
CANON <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
VAR   <- "target_D_rate_per_country_floored"
grid <- utils::read.csv(file.path(HERE,"EVAL_GRID.csv"), stringsAsFactors=FALSE)
grid <- grid[grid$grid=="prod" & grid$split=="selection", ]
for (k in c("cutoff","test_start","test_end")) grid[[k]] <- as.Date(grid[[k]])
W <- utils::read.csv(file.path(HERE,"weights_frozen.csv"), stringsAsFactors=FALSE)
obs <- utils::read.csv(CANON, stringsAsFactors=FALSE)[, c("iso_code","date",VAR)]
names(obs)[3] <- "observed"; obs$date <- as.Date(obs$date)
obs <- obs[is.finite(obs$observed) & obs$iso_code %in% W$iso_code, ]
rows <- list()
for (i in seq_len(nrow(grid))) {
  z <- obs[obs$date >= grid$test_start[i] & obs$date <= grid$test_end[i], ]
  pre <- obs[obs$date <= grid$cutoff[i] & obs$date > grid$cutoff[i]-182L, ]
  for (iso in W$iso_code) {
    a <- z$observed[z$iso_code==iso]; b <- pre$observed[pre$iso_code==iso]
    if (!length(a)) next
    rows[[length(rows)+1L]] <- data.frame(iso_code=iso, blk_obs=median(a),
      pre_obs=if (length(b)) median(b) else NA_real_, stringsAsFactors=FALSE)
  }
}
d <- do.call(rbind, rows)
agg <- stats::aggregate(cbind(blk_obs, pre_obs) ~ iso_code, d, median, na.action=na.pass)
delta <- c(COD=0.7555, NGA=0.0295, SSD=0.6241, ETH=0.4668, MOZ=-0.3485, SOM=0.5489,
           MWI=-0.1008, AGO=0.0061, ZWE=-0.3400, ZMB=-1.1242, TZA=0.2025,
           CMR=-0.1464, KEN=-0.2854, BDI=0.3505, LBR=0.4625, RWA=-0.1264)
agg$delta <- delta[agg$iso_code]
agg <- agg[order(-agg$blk_obs), ]
print(data.frame(iso=agg$iso_code, blk_observed=round(agg$blk_obs,3),
                 pre_observed=round(agg$pre_obs,3), delta=round(agg$delta,3)),
      row.names=FALSE)
ok <- is.finite(agg$blk_obs) & is.finite(agg$delta)
cat(sprintf("\ncor(BLOCK observed level, delta)      Pearson %+.3f  Spearman %+.3f\n",
  cor(agg$blk_obs[ok], agg$delta[ok]), cor(agg$blk_obs[ok], agg$delta[ok], method="spearman")))
ok2 <- is.finite(agg$pre_obs) & is.finite(agg$delta)
cat(sprintf("cor(PRE-CUTOFF observed level, delta) Pearson %+.3f  Spearman %+.3f\n",
  cor(agg$pre_obs[ok2], agg$delta[ok2]), cor(agg$pre_obs[ok2], agg$delta[ok2], method="spearman")))
cat("\nIf BLOCK correlates strongly but PRE-CUTOFF does not, the correction is a bet\n",
    "on the outcome and cannot be gated at the cutoff. If BOTH correlate, the\n",
    "pre-cutoff level is a legitimate gating variable and the fix is salvageable.\n", sep="")
