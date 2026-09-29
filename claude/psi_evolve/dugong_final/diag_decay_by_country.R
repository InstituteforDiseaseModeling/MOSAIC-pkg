# Is the level collapse UNIVERSAL or country-specific? C9c_h gained hugely in
# COD/SSD/SOM/ETH and destroyed ZMB/MOZ/ZWE. If the damaged countries are the
# ones whose psi does NOT decay, then the correction can be gated on the decay
# magnitude -- which is a property of the MODEL'S OWN OUTPUT and needs no
# observed data, so unlike B-CAL2's and C7's pre-cutoff-error selection it
# cannot fail to generalise for the usual reason.
suppressMessages(library(MOSAIC))
HERE  <- "/home/jgiles/psi_evolve"
CACHE <- "/home/jgiles/MOSAIC/MOSAIC-pkg/claude/forecast_cv_ocv4_q2yr/psi_cache"
grid <- utils::read.csv(file.path(HERE,"EVAL_GRID.csv"), stringsAsFactors=FALSE)
grid <- grid[grid$grid=="prod" & grid$split=="selection", ]
for (k in c("cutoff","test_start","test_end")) grid[[k]] <- as.Date(grid[[k]])
pool <- utils::read.csv(file.path(HERE,"weights_frozen.csv"), stringsAsFactors=FALSE)$iso_code
rows <- list()
for (i in seq_len(nrow(grid))) {
  f <- file.path(CACHE, sprintf("psi_%s.csv", format(grid$cutoff[i])))
  if (!file.exists(f)) next
  p <- utils::read.csv(f, stringsAsFactors=FALSE); p$date <- as.Date(p$date)
  p <- p[p$iso_code %in% pool, c("iso_code","date","psi")]
  T0 <- grid$cutoff[i]
  pre <- p[p$date <= T0 & p$date > T0 - 182L, ]
  e   <- p[p$date >= T0 + 15L & p$date <= T0 + 28L, ]   # wk 1-2
  l   <- p[p$date >= T0 + 71L & p$date <= T0 + 106L, ]  # wk 9-13
  for (iso in pool) {
    a <- pre$psi[pre$iso_code==iso]; b <- e$psi[e$iso_code==iso]; c_ <- l$psi[l$iso_code==iso]
    if (!length(a) || !length(b) || !length(c_)) next
    rows[[length(rows)+1L]] <- data.frame(iso_code=iso, block=grid$block[i],
      pre=median(a), early=median(b), late=median(c_),
      decay_ratio = median(c_)/max(1e-6, median(a)), stringsAsFactors=FALSE)
  }
}
d <- do.call(rbind, rows)
agg <- stats::aggregate(cbind(pre, early, late, decay_ratio) ~ iso_code, d, median)
# C9c_h per-country deltas from the gate run
delta <- c(COD=0.7555, NGA=0.0295, SSD=0.6241, ETH=0.4668, MOZ=-0.3485, SOM=0.5489,
           MWI=-0.1008, AGO=0.0061, ZWE=-0.3400, ZMB=-1.1242, TZA=0.2025,
           CMR=-0.1464, KEN=-0.2854, BDI=0.3505, LBR=0.4625, RWA=-0.1264)
agg$delta_C9c_h <- delta[agg$iso_code]
agg <- agg[order(agg$decay_ratio), ]
cat("decay_ratio = median(psi at wk9-13) / median(psi over the 26 wk before the cutoff)\n")
cat("  ~1.0 = psi holds its level (no collapse);  <<1 = psi dies off\n\n")
print(data.frame(iso=agg$iso_code, pre=round(agg$pre,3), late=round(agg$late,3),
                 decay=round(agg$decay_ratio,3), delta=round(agg$delta_C9c_h,3)),
      row.names=FALSE)
ok <- is.finite(agg$decay_ratio) & is.finite(agg$delta_C9c_h)
cat(sprintf("\nPearson  cor(decay_ratio, delta) = %+.3f   Spearman = %+.3f   (n=%d)\n",
    cor(agg$decay_ratio[ok], agg$delta_C9c_h[ok]),
    cor(agg$decay_ratio[ok], agg$delta_C9c_h[ok], method="spearman"), sum(ok)))
cat("\nA strongly NEGATIVE correlation means: the more psi collapses, the more the\n",
    "correction helps -- and gating on decay would keep the gains and drop the harm.\n", sep="")
