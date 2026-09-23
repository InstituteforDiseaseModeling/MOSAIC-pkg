# =============================================================================
# psi_plot_common.R -- shared data loading and the one prediction-panel drawer.
#
# Extracted from plot_priority_set.R when a second figure script needed the
# same panel. Two copies of a plotting routine is exactly the drift this repo
# keeps paying for (lesson 11), so there is one.
#
# Sourcing this defines: grid, W, pool, wts, obs, bl_fn, ARMS, ACOL, ALAB, AWD,
# BCOL, rdpsi(), psi_panel(), psi_legend(). It draws nothing.
# =============================================================================
HERE  <- "/home/jgiles/psi_evolve"
CANON <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
VAR   <- "target_D_rate_per_country_floored"
OUT   <- file.path(HERE, "figures"); dir.create(OUT, showWarnings = FALSE)

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

# Only corrected-epoch arms. The eleven earlier caches were fitted at
# best + patience and are NOT comparable to these -- that is why P000E exists.
# ND-wave arms are included automatically once their cache is complete, so the
# same script serves before and after that wave lands.
.ALL <- c(P000E="#6A6A6A", T2="#E08214", N9="#2166AC", ND="#B5123B",
          NDe="#D6604D", NDr="#7B3294", NDk9="#1B7837", NDi="#01665E")
.LBL <- c(P000E="P000E  LSTM baseline", T2="T2  lead = 12", N9="N9  D9b + N8",
          ND  ="ND  DLinear",           NDe="NDe  + edge padding",
          NDr ="NDr  + L2",             NDk9="NDk9  + kernel 9",
          NDi ="NDi  DLinear-I")
.complete <- function(a) {
     d <- file.path(HERE, paste0("psi_cache_", a))
     dir.exists(d) && length(list.files(d, "^psi_.*\\.csv$")) >= nrow(grid)
}
ARMS <- names(.ALL)[vapply(names(.ALL), .complete, logical(1))]
if (!length(ARMS)) stop("psi_plot_common: no arm has a complete cache")
ACOL <- .ALL[ARMS]; ALAB <- .LBL[ARMS]
AWD  <- stats::setNames(ifelse(ARMS %in% c("ND","NDe","NDr","NDk9","NDi"), 2.1, 1.3), ARMS)
BCOL <- c(obs="grey15", pers="#4393C3", seas="#F2A900")

rdpsi <- function(arm, ct) {
     f <- file.path(HERE, paste0("psi_cache_", arm), sprintf("psi_%s.csv", ct))
     if (!file.exists(f)) return(NULL)
     p <- utils::read.csv(f, stringsAsFactors=FALSE); p$date <- as.Date(p$date); p
}

#' One country-cutoff panel: observed IS + OOS with every arm overlaid.
#' @param title_col colour for the panel title (used to flag weak countries)
psi_panel <- function(iso, k, lookback = 364L, title_col = "black",
                      title_extra = "") {
     T0 <- grid$cutoff[k]; ct <- format(T0)
     ts <- grid$test_start[k]; te <- grid$test_end[k]
     o  <- obs[obs$iso_code==iso & obs$date >= T0-lookback & obs$date <= te, ]
     o  <- o[order(o$date), ]
     oo <- o[o$date >= ts, ]
     isdf <- obs[obs$iso_code==iso & obs$date <= T0, ]
     bp <- if (nrow(isdf) >= 8 && nrow(oo)) bl_fn(isdf, oo$date, "persistence") else NULL
     bs <- if (nrow(isdf) >= 8 && nrow(oo)) bl_fn(isdf, oo$date, "seasonal")    else NULL
     pl <- lapply(ARMS, function(a) {
          p <- rdpsi(a, ct); if (is.null(p)) return(NULL)
          q <- p[p$iso_code==iso & p$date >= T0-lookback & p$date <= te, ]
          if (!nrow(q)) NULL else q[order(q$date), ]
     })
     names(pl) <- ARMS
     # Axes from EVERY series, not just the observed one: a country-cutoff with
     # no observations gave an empty o$date and "need finite 'xlim' values".
     xs <- c(o$date, unlist(lapply(pl, function(q) q$date)), oo$date)
     yy <- c(o$observed, unlist(lapply(pl, function(q) q$psi)),
             if (!is.null(bp)) bp$point, if (!is.null(bs)) bs$point)
     xs <- xs[is.finite(xs)]; yy <- yy[is.finite(yy)]
     if (!length(xs) || !length(yy)) {
          plot.new(); title(main=sprintf("%s  (no data)", iso), cex.main=0.8,
                            font.main=1, col.main="grey60")
          return(invisible())
     }
     plot(range(as.Date(xs, origin="1970-01-01")), range(0, yy), type="n",
          xlab="", ylab="", xaxt="n", main="")
     title(main=sprintf("%s%s", iso, title_extra), cex.main=0.82, font.main=1,
           col.main=title_col)
     axis.Date(1, at=pretty(as.Date(xs, origin="1970-01-01"), 3), cex.axis=0.62)
     rect(ts, par("usr")[3], te, par("usr")[4], col="grey94", border=NA)
     abline(v=T0, col="grey35", lty=2)
     box()
     if (!is.null(bs)) lines(oo$date, bs$point, col=BCOL["seas"], lwd=1.3, lty=3)
     if (!is.null(bp)) lines(oo$date, bp$point, col=BCOL["pers"], lwd=1.7, lty=2)
     for (a in ARMS) if (!is.null(pl[[a]]))
          lines(pl[[a]]$date, pl[[a]]$psi, col=ACOL[a], lwd=AWD[a])
     lines(o$date, o$observed, col=BCOL["obs"], lwd=1.8)
     points(oo$date, oo$observed, col=BCOL["obs"], pch=16, cex=0.45)
     invisible()
}

psi_legend <- function(ncol = 4, cex = 0.72) {
     par(fig=c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=TRUE); plot.new()
     legend("bottom",
            legend=c("observed", unname(ALAB), "persistence", "climatology"),
            col=c(BCOL["obs"], unname(ACOL), BCOL["pers"], BCOL["seas"]),
            lwd=c(1.8, unname(AWD), 1.7, 1.3),
            lty=c(1, rep(1, length(ARMS)), 2, 3),
            ncol=ncol, bty="n", cex=cex)
}
