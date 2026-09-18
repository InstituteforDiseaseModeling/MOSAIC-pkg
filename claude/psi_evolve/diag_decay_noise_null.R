# Is the per-country DECAY-RATIO spread a real property, or fit noise?
#
# WHY. The spread 0.083 (ZWE) to 1.387 (MWI) is the headline motivation for N5:
# it is a 17x range in the one property a shared trunk forces to be common. But
# it was measured on ONE fit of P001. The wave-24 retraction showed that a
# pattern can look strong and still be indistinguishable from seed noise, so the
# same null test applies here before the number is quoted again.
#
# P000 and P000R are the SAME SPEC, differing only by seed block. If a country's
# decay ratio is a stable property, it should agree between them. If it is noise,
# it will not.
HERE <- "/home/jgiles/psi_evolve"
pool <- utils::read.csv(file.path(HERE,"weights_frozen.csv"), stringsAsFactors=FALSE)$iso_code
grid <- utils::read.csv(file.path(HERE,"EVAL_GRID.csv"), stringsAsFactors=FALSE)
grid <- grid[grid$grid=="prod" & grid$split=="selection", ]
grid$cutoff <- as.Date(grid$cutoff)

decay <- function(dir) {
  rows <- list()
  for (i in seq_len(nrow(grid))) {
    f <- file.path(dir, sprintf("psi_%s.csv", format(grid$cutoff[i])))
    if (!file.exists(f)) next
    p <- utils::read.csv(f, stringsAsFactors=FALSE); p$date <- as.Date(p$date)
    p <- p[p$iso_code %in% pool, c("iso_code","date","psi")]
    T0 <- grid$cutoff[i]
    pre <- p[p$date <= T0 & p$date > T0 - 182L, ]
    lat <- p[p$date >= T0 + 71L & p$date <= T0 + 106L, ]
    for (iso in pool) {
      a <- pre$psi[pre$iso_code==iso]; b <- lat$psi[lat$iso_code==iso]
      if (!length(a) || !length(b)) next
      rows[[length(rows)+1L]] <- data.frame(iso_code=iso,
        ratio = stats::median(b)/max(1e-6, stats::median(a)), stringsAsFactors=FALSE)
    }
  }
  if (!length(rows)) return(NULL)
  d <- do.call(rbind, rows)
  stats::aggregate(ratio ~ iso_code, d, stats::median)
}
A <- decay(file.path(HERE,"psi_cache_P000"))
B <- decay(file.path(HERE,"psi_cache_P000R"))
if (is.null(A) || is.null(B)) stop("need both P000 and P000R caches")
x <- merge(A, B, by="iso_code", suffixes=c("_P000","_P000R"))
x <- x[is.finite(x$ratio_P000) & is.finite(x$ratio_P000R), ]
print(data.frame(iso=x$iso_code, P000=round(x$ratio_P000,3), P000R=round(x$ratio_P000R,3),
                 abs_diff=round(abs(x$ratio_P000-x$ratio_P000R),3))[order(-x$ratio_P000), ],
      row.names=FALSE)
cat(sprintf("\nn = %d countries\n", nrow(x)))
cat(sprintf("range within P000 : %.3f to %.3f  (%.1fx)\n",
            min(x$ratio_P000), max(x$ratio_P000), max(x$ratio_P000)/max(1e-6,min(x$ratio_P000))))
cat(sprintf("cross-replicate cor: %+.3f Pearson / %+.3f Spearman\n",
            cor(x$ratio_P000, x$ratio_P000R), cor(x$ratio_P000, x$ratio_P000R, method="spearman")))
cat(sprintf("between-country SD %.3f   vs   within-country replicate SD %.3f   ratio %.2f\n",
            stats::sd(c(x$ratio_P000, x$ratio_P000R)),
            stats::sd(x$ratio_P000 - x$ratio_P000R)/sqrt(2),
            stats::sd(c(x$ratio_P000, x$ratio_P000R)) /
              (stats::sd(x$ratio_P000 - x$ratio_P000R)/sqrt(2))))
cat("\nREAD: a HIGH cross-replicate correlation and a between/within ratio well above 1\n",
    "mean the decay ratio is a stable country property and N5's motivation stands.\n",
    "A low correlation means it is fit noise and the 17x spread must not be quoted.\n", sep="")
