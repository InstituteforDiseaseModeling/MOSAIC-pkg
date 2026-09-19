# =============================================================================
# plot_psi_evolve.R -- figures summarising the psi_evolve programme.
#
#  fig1_predictions_is_oos.pdf  observed IS + OOS with every predictor overlaid
#  fig2_trend_vs_mae.pdf        MAE against TREND skill -- the central result
#  fig3_metric_bars.pdf         per-arm bars per metric, with baseline references
#  fig4_horizon.pdf             error by forecast week: blend vs persistence vs psi
#
# Base graphics only -- no new package dependency.
# usage: Rscript plot_psi_evolve.R [ARM]        (default N8)
# =============================================================================
suppressMessages(library(MOSAIC))
HERE  <- "/home/jgiles/psi_evolve"
CANON <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
VAR   <- "target_D_rate_per_country_floored"
PROD  <- "/home/jgiles/MOSAIC/MOSAIC-pkg/claude/forecast_cv_ocv4_q2yr/psi_cache"
ARM   <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "N8"
OUT   <- file.path(HERE, "figures"); dir.create(OUT, showWarnings = FALSE)
source(file.path(HERE, "shape_metrics.R"))

grid <- utils::read.csv(file.path(HERE,"EVAL_GRID.csv"), stringsAsFactors=FALSE)
grid <- grid[grid$grid=="prod" & grid$split=="selection", ]
for (k in c("cutoff","test_start","test_end")) grid[[k]] <- as.Date(grid[[k]])
W <- utils::read.csv(file.path(HERE,"weights_frozen.csv"), stringsAsFactors=FALSE)
W$w <- W$w_sqrt/sum(W$w_sqrt); pool <- W$iso_code
wts <- stats::setNames(W$w, W$iso_code)
obs <- utils::read.csv(CANON, stringsAsFactors=FALSE)[,c("iso_code","date",VAR)]
names(obs)[3] <- "observed"; obs$date <- as.Date(obs$date)
obs <- obs[is.finite(obs$observed) & obs$iso_code %in% pool, ]
bl_fn <- getFromNamespace(".rcv_baseline","MOSAIC")

COL <- c(obs="grey25", psi="#B5123B", pers="#0167AF", seas="#F2A900", blend="#1B7837")
LAB <- c(obs="observed", psi=paste0(ARM," psi (raw)"), pers="persistence",
         seas="climatology", blend=paste0(ARM," + C12c_C11h blend"))

rdpsi <- function(cache, ct) {
     f <- file.path(cache, sprintf("psi_%s.csv", ct)); if (!file.exists(f)) return(NULL)
     p <- utils::read.csv(f, stringsAsFactors=FALSE); p$date <- as.Date(p$date); p
}
vfile <- file.path(HERE, sprintf("variants_%s.rds", ARM))
BL <- if (file.exists(vfile)) {
          v <- readRDS(vfile); if ("C12c_C11h" %in% names(v)) v[["C12c_C11h"]] else NULL
      } else NULL
if (is.null(BL)) message("NOTE: no blend dump at ", vfile, " -- fig1 omits the blend line")

# ---------- fig 1: predictions against IS and OOS observed -------------------
# Countries span the lambda range measured in wave 29: CMR/MOZ (psi earns real
# weight), COD/KEN (middling), MWI/RWA (lambda ~ 0, so the blend IS persistence).
isos <- intersect(c("MOZ","CMR","COD","KEN","MWI","RWA"), pool)
cti  <- c(2,4,6)
pdf(file.path(OUT,"fig1_predictions_is_oos.pdf"), width=13.5, height=9.5)
par(mfrow=c(length(isos), length(cti)), mar=c(2.2,2.6,1.6,0.5), mgp=c(1.5,0.45,0),
    oma=c(3.6,2.4,3.8,0.5), cex.axis=0.72, tcl=-0.25)
for (iso in isos) {
  for (k in cti) {
    T0 <- grid$cutoff[k]; ct <- format(T0)
    ts <- grid$test_start[k]; te <- grid$test_end[k]
    o  <- obs[obs$iso_code==iso & obs$date >= T0-182 & obs$date <= te, ]
    o  <- o[order(o$date), ]
    p  <- rdpsi(file.path(HERE, paste0("psi_cache_", ARM)), ct)
    pp <- if (is.null(p)) NULL else p[p$iso_code==iso & p$date >= T0-182 & p$date <= te, ]
    isdf <- obs[obs$iso_code==iso & obs$date <= T0, ]
    oo <- o[o$date >= ts, ]
    bp <- if (nrow(isdf) >= 8 && nrow(oo)) bl_fn(isdf, oo$date, "persistence") else NULL
    bs <- if (nrow(isdf) >= 8 && nrow(oo)) bl_fn(isdf, oo$date, "seasonal")    else NULL
    # BL$fold is the INTEGER block id from grid$block, NOT a date string. Matching
    # it against format(cutoff) silently returned zero rows, so the blend line
    # vanished while the legend still advertised it.
    bb <- if (!is.null(BL)) BL[BL$iso_code==iso & BL$fold==grid$block[k], ] else NULL
    if (!is.null(bb) && nrow(bb)) bb <- bb[order(bb$date), ]

    yy <- c(o$observed, if (!is.null(pp)) pp$psi, if (!is.null(bp)) bp$point,
            if (!is.null(bs)) bs$point, if (!is.null(bb)) bb$psi)
    yy <- yy[is.finite(yy)]; if (!length(yy)) { plot.new(); next }
    plot(o$date, o$observed, type="n", ylim=range(0, yy), xlab="", ylab="",
         main=sprintf("%s  |  cutoff %s", iso, ct), cex.main=0.85, font.main=1)
    # shade the OOS window
    rect(ts, par("usr")[3], te, par("usr")[4], col="grey94", border=NA)
    abline(v=T0, col="grey40", lty=2)
    box()
    if (!is.null(pp)) lines(pp$date, pp$psi, col=COL["psi"], lwd=1.5)
    if (!is.null(bs)) lines(oo$date, bs$point, col=COL["seas"], lwd=1.5, lty=3)
    if (!is.null(bp)) lines(oo$date, bp$point, col=COL["pers"], lwd=2, lty=2)
    if (!is.null(bb) && nrow(bb)) lines(bb$date, bb$psi, col=COL["blend"], lwd=2.2)
    lines(o$date, o$observed, col=COL["obs"], lwd=1.8)
    points(oo$date, oo$observed, col=COL["obs"], pch=16, cex=0.45)
  }
}
mtext("Predictions against in-sample and out-of-sample observed transmission intensity",
      outer=TRUE, line=1.9, cex=1.0, font=2)
mtext(sprintf("grey panel = the 13-week OOS window scored; dashed line = forecast origin. Rows span the lambda range: MOZ/CMR high, COD/KEN mid, MWI/RWA ~0 (blend = persistence).  arm = %s", ARM),
      outer=TRUE, line=0.55, cex=0.68, col="grey30")
mtext("transmission intensity", side=2, outer=TRUE, line=0.9, cex=0.8)
par(fig=c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=TRUE); plot.new()
legend("bottom", legend=LAB[c("obs","psi","pers","seas","blend")],
       col=COL[c("obs","psi","pers","seas","blend")],
       lwd=c(1.8,1.5,2,1.5,2.2), lty=c(1,1,2,3,1), horiz=TRUE, bty="n", cex=0.78)
dev.off()

# ---------- gather metrics for figs 2-3 --------------------------------------
known <- c("P000","P000R","P000H","N5","N6","N8","D9b","F1","F3","F4")
caches <- c(P001 = PROD)
for (a in known) { d <- file.path(HERE, paste0("psi_cache_", a)); if (dir.exists(d)) caches[a] <- d }
cts <- format(grid$cutoff)
long <- list()
for (nm in names(caches)) for (ct in cts) {
  i <- match(as.Date(ct), grid$cutoff); p <- rdpsi(caches[[nm]], ct); if (is.null(p)) next
  p <- p[p$iso_code %in% pool & p$date >= grid$test_start[i] & p$date <= grid$test_end[i], ]
  m <- merge(p[,c("iso_code","date","psi")], obs, by=c("iso_code","date")); if (!nrow(m)) next
  m$fold <- ct; long[[length(long)+1L]] <- data.frame(arm=nm, m, stringsAsFactors=FALSE)
}
L <- do.call(rbind, long)
for (bn in c("persistence","seasonal")) {
  acc <- list()
  for (ct in cts) {
    i <- match(as.Date(ct), grid$cutoff)
    z <- unique(L[L$fold==ct, c("iso_code","date","observed")])
    for (iso in unique(z$iso_code)) {
      zz <- z[z$iso_code==iso, ]; zz <- zz[order(zz$date), ]
      isdf <- obs[obs$iso_code==iso & obs$date <= grid$cutoff[i], ]
      if (nrow(isdf) < 8) next
      acc[[length(acc)+1L]] <- data.frame(arm=bn, iso_code=iso, date=zz$date,
        psi=bl_fn(isdf, zz$date, bn)$point, observed=zz$observed, fold=ct,
        stringsAsFactors=FALSE)
    }
  }
  L <- rbind(L, do.call(rbind, acc))
}
arms <- c(names(caches), "persistence", "seasonal")
M <- do.call(rbind, lapply(arms, function(nm) {
  z <- L[L$arm==nm, ]; if (!nrow(z)) return(NULL)
  per <- split(seq_len(nrow(z)), paste(z$iso_code, z$fold, sep="\r"))
  w   <- vapply(per, function(ix) wts[[z$iso_code[ix[1]]]], numeric(1))
  sm  <- .shape_metrics(z, "psi", wts, "fold")
  data.frame(arm=nm,
    MAE=stats::weighted.mean(vapply(per, function(ix) mean(abs(z$observed[ix]-z$psi[ix])), numeric(1)), w),
    R2corr=stats::weighted.mean(vapply(per, function(ix)
      suppressWarnings(stats::cor(z$observed[ix], z$psi[ix]))^2, numeric(1)), w, na.rm=TRUE),
    bias=sm[["bias"]], sd_ratio=sm[["sd_ratio"]], dcor=sm[["dcor"]],
    dir_acc=sm[["dir_acc"]], degen=sm[["degen"]], stringsAsFactors=FALSE)
}))
utils::write.csv(M, file.path(OUT,"metrics_table.csv"), row.names=FALSE)

isb <- function(a) a %in% c("persistence","seasonal")
pchv <- ifelse(isb(M$arm), 17, 19); colv <- ifelse(M$arm=="persistence", COL[["pers"]],
          ifelse(M$arm=="seasonal", COL[["seas"]], "grey30"))
colv[M$arm==ARM] <- COL[["psi"]]

# ---------- fig 2: MAE vs trend skill ----------------------------------------
pdf(file.path(OUT,"fig2_trend_vs_mae.pdf"), width=11, height=5.4)
par(mfrow=c(1,2), mar=c(4.2,4.2,3.2,1), mgp=c(2.4,0.6,0), oma=c(0,0,1.6,0))
for (v in c("dir_acc","dcor")) {
  yy <- M[[v]]; ok <- is.finite(yy) & is.finite(M$MAE)
  xr <- range(M$MAE[ok]); xr <- xr + c(-1,1)*0.06*diff(xr)
  yr2 <- range(yy[ok]); yr2 <- yr2 + c(0, 0.12*diff(yr2))
  plot(M$MAE[ok], yy[ok], pch=pchv[ok], col=colv[ok], cex=1.5, xlim=xr, ylim=yr2,
       xlab="MAE  (lower = better)",
       ylab=if (v=="dir_acc") "directional accuracy" else "corr of week-to-week changes",
       main=if (v=="dir_acc") "Getting the DIRECTION right" else "Tracking the CHANGES",
       font.main=1, cex.main=0.98)
  abline(h=if (v=="dir_acc") 0.5 else 0, col="grey55", lty=2)
  text(M$MAE[ok], yy[ok], M$arm[ok], pos=3, cex=0.62, col=colv[ok], offset=0.45)
  mtext(if (v=="dir_acc") "dashed = coin flip" else "dashed = no trend information",
        side=3, line=0.15, cex=0.66, col="grey40")
}
mtext("Better MAE does not buy trend skill: the models sit left of the baselines but flat against them",
      outer=TRUE, line=0.1, cex=0.86, font=2)
dev.off()

# ---------- fig 3: metric bars -----------------------------------------------
pdf(file.path(OUT,"fig3_metric_bars.pdf"), width=12, height=7.6)
par(mfrow=c(2,3), mar=c(6.2,4,3,0.8), mgp=c(2.4,0.6,0), oma=c(0,0,2.4,0))
spec <- list(c("MAE","MAE (lower better)",NA), c("R2corr","R2_corr (level series)",NA),
             c("bias","bias  (1 = unbiased)",1), c("sd_ratio","sd(pred)/sd(obs)  (0 = flat)",1),
             c("dcor","corr of changes  (0 = none)",0), c("dir_acc","directional accuracy",0.5))
for (s in spec) {
  v <- M[[s[1]]]; v[!is.finite(v)] <- 0
  bp <- barplot(v, names.arg=M$arm, las=2, col=colv, border=NA, cex.names=0.7,
                main=s[2], font.main=1, cex.main=0.92, ylab="")
  if (!is.na(s[3])) abline(h=as.numeric(s[3]), col="grey35", lty=2)
  if (s[1]=="dcor") text(bp[M$arm=="persistence"], 0.004, "undef", cex=0.6, srt=90, adj=0)
}
mtext("Per-arm metrics; blue = persistence, gold = climatology, red = the selected arm",
      outer=TRUE, line=0.6, cex=0.9, font=2)
dev.off()

# ---------- fig 4: error by forecast week ------------------------------------
L$wk <- NA_integer_
for (ct in cts) { i <- match(as.Date(ct), grid$cutoff); j <- L$fold==ct
  L$wk[j] <- as.integer(floor(as.numeric(L$date[j] - (grid$cutoff[i]+14L))/7)) + 1L }
byweek <- function(nm) {
  z <- L[L$arm==nm & L$wk>=1 & L$wk<=13, ]
  vapply(1:13, function(w) { zz <- z[z$wk==w, ]
    if (!nrow(zz)) NA_real_ else stats::weighted.mean(abs(zz$observed-zz$psi),
      unname(wts[match(zz$iso_code, names(wts))]), na.rm=TRUE) }, numeric(1))
}
pdf(file.path(OUT,"fig4_horizon.pdf"), width=8.2, height=5.6)
par(mar=c(4.2,4.2,3.4,1), mgp=c(2.4,0.6,0))
series <- list(persistence=byweek("persistence"), seasonal=byweek("seasonal"),
               psi=byweek(ARM), prod=byweek("P001"))
if (!is.null(BL)) {
  b <- merge(BL, obs, by=c("iso_code","date"))
  b$wk <- NA_integer_
  for (i in seq_len(nrow(grid))) { j <- b$fold == grid$block[i]
    b$wk[j] <- as.integer(floor(as.numeric(b$date[j] - (grid$cutoff[i]+14L))/7)) + 1L }
  if (!any(is.finite(b$wk)))
    stop("fig4: blend rows never matched a block -- refusing to draw a legend entry ",
         "for a line that would be absent (fold is grid$block, an integer)")
  series$blend <- vapply(1:13, function(w) { zz <- b[which(b$wk==w), ]
    if (!nrow(zz)) NA_real_ else stats::weighted.mean(abs(zz$observed-zz$psi),
      unname(wts[match(zz$iso_code, names(wts))]), na.rm=TRUE) }, numeric(1))
  if (all(!is.finite(series$blend))) series$blend <- NULL
}
yr <- range(unlist(series), na.rm=TRUE)
plot(1:13, series$persistence, type="n", ylim=yr, xlab="forecast week", ylab="MAE",
     main="Error grows with horizon -- and that is where the blend earns its gain",
     font.main=1, cex.main=0.95)
abline(v=c(4.5,8.5), col="grey88")
lines(1:13, series$prod,        col=COL[["psi"]], lwd=1.6, lty=3)
lines(1:13, series$psi,         col=COL[["psi"]], lwd=2)
lines(1:13, series$seasonal,    col=COL[["seas"]], lwd=1.8, lty=3)
lines(1:13, series$persistence, col=COL[["pers"]], lwd=2.4, lty=2)
if (!is.null(series$blend)) lines(1:13, series$blend, col=COL[["blend"]], lwd=2.6)
legend("topleft", bty="n", cex=0.76,
  legend=c("persistence","climatology","production psi", paste0(ARM," psi (raw)"),
           if (!is.null(series$blend)) paste0(ARM," + blend")),
  col=c(COL[["pers"]],COL[["seas"]],COL[["psi"]],COL[["psi"]],
        if (!is.null(series$blend)) COL[["blend"]]),
  lwd=c(2.4,1.8,1.6,2, if (!is.null(series$blend)) 2.6),
  lty=c(2,3,3,1, if (!is.null(series$blend)) 1))
dev.off()
cat("wrote 4 figures + metrics_table.csv to ", OUT, "\n", sep="")
