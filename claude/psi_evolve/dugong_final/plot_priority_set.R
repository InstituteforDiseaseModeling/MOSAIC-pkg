# =============================================================================
# plot_priority_set.R -- timeseries predictions of the PRIORITY SET against the
# in-sample and out-of-sample observed series.
#
#   fig7_priority_timeseries.pdf   6 countries x 3 cutoffs
#   fig8_priority_allcutoffs.pdf   4 highest-burden countries x all 6 cutoffs
#   fig9_priority_pooled.pdf       burden-weighted pooled mean by forecast week
#
# The four arms are the only ones fitted under the CORRECTED epoch code
# (.psi_epoch_from_history), so they are mutually comparable and are NOT
# comparable to the eleven earlier caches. That is the whole reason P000E
# exists, and it is why no pre-fix arm is drawn here.
#
# Base graphics only -- no new package dependency.
# usage: Rscript plot_priority_set.R
# =============================================================================
suppressMessages(library(MOSAIC))
# Shared loading + the one panel drawer. Two copies of a plotting routine is
# exactly the drift this repo keeps paying for (lesson 11).
source("/home/jgiles/psi_evolve/psi_plot_common.R")
panel <- function(iso, k, lookback = 364L)
     psi_panel(iso, k, lookback = lookback,
               title_extra = sprintf("  |  cutoff %s", format(grid$cutoff[k])))
draw_legend <- function() psi_legend(ncol = min(6, length(ARMS) + 3))

# ---- fig 7: 6 countries x 3 cutoffs ----------------------------------------
# Countries span the lambda range measured in wave 29: CMR/MOZ (psi earns real
# weight), COD/KEN (middling), MWI/RWA (lambda ~ 0).
isos <- intersect(c("MOZ","CMR","COD","KEN","MWI","RWA"), pool)
cti  <- c(2,4,6)
pdf(file.path(OUT,"fig7_priority_timeseries.pdf"), width=13.5, height=10)
par(mfrow=c(length(isos), length(cti)), mar=c(2.0,2.6,1.6,0.5), mgp=c(1.5,0.45,0),
    oma=c(4.2,2.4,3.8,0.5), cex.axis=0.7, tcl=-0.25)
for (iso in isos) for (k in cti) panel(iso, k)
mtext("Priority set: predictions against in-sample and out-of-sample observed transmission intensity",
      outer=TRUE, line=1.9, cex=1.0, font=2)
mtext("grey panel = the 13-week OOS window scored; dashed vertical = forecast origin; points = OOS observations. All four arms share the corrected-epoch code, so they are mutually comparable.",
      outer=TRUE, line=0.55, cex=0.66, col="grey30")
mtext("transmission intensity", side=2, outer=TRUE, line=0.9, cex=0.8)
draw_legend()
dev.off()

# ---- fig 8: the 4 highest-burden countries x ALL 6 cutoffs ------------------
top4 <- names(sort(wts, decreasing=TRUE))[1:4]
pdf(file.path(OUT,"fig8_priority_allcutoffs.pdf"), width=15, height=8.5)
par(mfrow=c(length(top4), nrow(grid)), mar=c(1.9,2.3,1.5,0.4), mgp=c(1.5,0.45,0),
    oma=c(4.2,2.4,3.8,0.5), cex.axis=0.62, tcl=-0.22)
for (iso in top4) for (k in seq_len(nrow(grid))) panel(iso, k, lookback=250L)
mtext("Priority set across every selection fold -- the four highest-burden countries",
      outer=TRUE, line=1.9, cex=1.0, font=2)
mtext(sprintf("rows = %s (burden weights %s); columns = the 6 selection cutoffs",
              paste(top4, collapse=", "),
              paste(sprintf("%.2f", wts[top4]), collapse=", ")),
      outer=TRUE, line=0.55, cex=0.66, col="grey30")
mtext("transmission intensity", side=2, outer=TRUE, line=0.9, cex=0.8)
draw_legend()
dev.off()

# ---- fig 9: burden-weighted pooled trajectory by forecast week --------------
# The per-country panels show shape; this shows whether an arm is right ON
# AVERAGE over the horizon we actually forecast, which is what the MAE table
# summarises into a single number.
wk_of <- function(d, T0) as.integer(floor(as.numeric(d-(T0+14L))/7))+1L
acc <- list()
for (k in seq_len(nrow(grid))) {
     T0 <- grid$cutoff[k]; ct <- format(T0)
     oo <- obs[obs$date >= grid$test_start[k] & obs$date <= grid$test_end[k], ]
     if (!nrow(oo)) next
     for (a in ARMS) {
          p <- rdpsi(a, ct); if (is.null(p)) next
          m <- merge(p[,c("iso_code","date","psi")], oo, by=c("iso_code","date"))
          if (!nrow(m)) next
          m$wk <- wk_of(m$date, T0)
          acc[[length(acc)+1L]] <- data.frame(arm=a, m[,c("iso_code","wk","psi","observed")],
                                              stringsAsFactors=FALSE)
     }
     # baselines, built per country from in-sample history
     for (bn in c("persistence","seasonal")) {
          rows <- list()
          for (iso in unique(oo$iso_code)) {
               isdf <- obs[obs$iso_code==iso & obs$date <= T0, ]
               od   <- oo[oo$iso_code==iso, ]
               if (nrow(isdf) < 8 || !nrow(od)) next
               b <- bl_fn(isdf, od$date, bn); if (is.null(b)) next
               rows[[length(rows)+1L]] <- data.frame(arm=bn, iso_code=iso,
                    wk=wk_of(od$date, T0), psi=b$point, observed=od$observed,
                    stringsAsFactors=FALSE)
          }
          if (length(rows)) acc[[length(acc)+1L]] <- do.call(rbind, rows)
     }
}
D <- do.call(rbind, acc); D <- D[D$wk >= 1 & D$wk <= 13 & is.finite(D$psi), ]
wmean <- function(x, iso) { w <- wts[iso]; sum(w*x, na.rm=TRUE)/sum(w[is.finite(x)]) }
series <- function(a, col) {
     z <- D[D$arm==a, ]
     vapply(1:13, function(w) { y <- z[z$wk==w,]
          if (!nrow(y)) NA_real_ else wmean(y[[col]], y$iso_code) }, numeric(1))
}
truth <- series("ND","observed")     # identical for every arm: same merged cells
pdf(file.path(OUT,"fig9_priority_pooled.pdf"), width=10, height=5.6)
par(mar=c(4.2,4.2,3.4,1), mgp=c(2.4,0.7,0))
allv <- c(truth, unlist(lapply(c(ARMS,"persistence","seasonal"), series, col="psi")))
plot(1:13, truth, type="n", ylim=range(0, allv[is.finite(allv)]),
     xlab="forecast week after the origin", ylab="burden-weighted transmission intensity",
     main="Priority set: pooled predicted level against the truth, by forecast week")
abline(v=c(4.5,8.5), col="grey88"); grid(nx=NA, ny=NULL, col="grey92", lty=1)
lines(1:13, series("seasonal","psi"),    col=BCOL["seas"], lwd=1.6, lty=3)
lines(1:13, series("persistence","psi"), col=BCOL["pers"], lwd=2.0, lty=2)
for (a in ARMS) lines(1:13, series(a,"psi"), col=ACOL[a], lwd=AWD[a])
lines(1:13, truth, col=BCOL["obs"], lwd=3)
points(1:13, truth, col=BCOL["obs"], pch=16, cex=0.7)
legend("topright", legend=c("OBSERVED", ARMS, "persistence", "climatology"),
       col=c(BCOL["obs"], ACOL[ARMS], BCOL["pers"], BCOL["seas"]),
       lwd=c(3, AWD[ARMS], 2.0, 1.6), lty=c(1, rep(1,length(ARMS)), 2, 3),
       bty="n", cex=0.75)
mtext("A line BELOW the observed curve is under-predicting the level; the gap at weeks 9-13 is the decay the programme has been chasing.",
      side=3, line=0.3, cex=0.68, col="grey30")
dev.off()

cat("wrote:\n")
for (f in c("fig7_priority_timeseries.pdf","fig8_priority_allcutoffs.pdf",
            "fig9_priority_pooled.pdf"))
     cat(sprintf("  %-32s %8.0f bytes\n", f, file.info(file.path(OUT,f))$size))
