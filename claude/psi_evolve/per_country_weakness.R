# Where is production WEAKEST, and does ND help there?
#
# The pooled MAE table hides the thing fig8 showed: ND wins overall while being
# a near-flat line in SSD and ETH, where the LSTM visibly tracks the signal.
# This ranks every country by the CURRENT PRODUCTION ARCHITECTURE's error and
# puts ND beside it, so the ND follow-up can be aimed rather than pooled.
#
# Comparator is P000E, not P001/P000: only the corrected-epoch arms are
# comparable to ND.
# usage: Rscript per_country_weakness.R
suppressMessages(library(MOSAIC))
HERE  <- "/home/jgiles/psi_evolve"
CANON <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
VAR   <- "target_D_rate_per_country_floored"
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

# Auto-include any corrected-epoch arm with a complete cache, so the ND wave
# joins without an edit. P000E stays the comparator: it is the production
# architecture under the same epoch code as everything else here.
ARMS <- Filter(function(a) {
     d <- file.path(HERE, paste0("psi_cache_", a))
     dir.exists(d) && length(list.files(d, "^psi_.*\\.csv$")) >= nrow(grid)
}, c("P000E","T2","N9","ND","NDe","NDr","NDk9","NDi","NDeR"))
rd <- function(arm, ct) {
     f <- file.path(HERE, paste0("psi_cache_", arm), sprintf("psi_%s.csv", ct))
     if (!file.exists(f)) return(NULL)
     p <- utils::read.csv(f, stringsAsFactors=FALSE); p$date <- as.Date(p$date); p
}

acc <- list()
for (k in seq_len(nrow(grid))) {
     T0 <- grid$cutoff[k]; ct <- format(T0)
     oo <- obs[obs$date >= grid$test_start[k] & obs$date <= grid$test_end[k], ]
     if (!nrow(oo)) next
     for (a in ARMS) {
          p <- rd(a, ct); if (is.null(p)) next
          m <- merge(p[,c("iso_code","date","psi")], oo, by=c("iso_code","date"))
          if (nrow(m)) acc[[length(acc)+1L]] <-
               data.frame(arm=a, m, block=k, stringsAsFactors=FALSE)
     }
     for (iso in unique(oo$iso_code)) {          # persistence, the bar to clear
          isdf <- obs[obs$iso_code==iso & obs$date <= T0, ]
          od   <- oo[oo$iso_code==iso, ]
          if (nrow(isdf) < 8 || !nrow(od)) next
          b <- bl_fn(isdf, od$date, "persistence"); if (is.null(b)) next
          acc[[length(acc)+1L]] <- data.frame(arm="persistence", iso_code=iso,
               date=od$date, psi=b$point, observed=od$observed, block=k,
               stringsAsFactors=FALSE)
     }
}
D <- do.call(rbind, acc); D <- D[is.finite(D$psi) & is.finite(D$observed), ]

stat <- function(d) {
     if (nrow(d) < 6L) return(c(mae=NA, sdr=NA, dacc=NA))
     s <- .shape_metrics(d, "psi", rep(1, nrow(d)), "block")
     c(mae = mean(abs(d$observed - d$psi)), sdr = s[["sd_ratio"]], dacc = s[["dir_acc"]])
}
isos <- sort(unique(D$iso_code))
out <- do.call(rbind, lapply(isos, function(iso) {
     r <- lapply(c(ARMS,"persistence"), function(a) stat(D[D$arm==a & D$iso_code==iso, ]))
     names(r) <- c(ARMS,"persistence")
     data.frame(iso = iso, w = unname(wts[iso]),
                mae_P000E = r$P000E[["mae"]], mae_ND = r$ND[["mae"]],
                mae_NDe = r$NDe[["mae"]], mae_pers = r$persistence[["mae"]],
                sdr_P000E = r$P000E[["sdr"]], sdr_NDe = r$NDe[["sdr"]],
                stringsAsFactors = FALSE)
}))
# "Weakest" = the biggest BURDEN-WEIGHTED excess of the production architecture
# over persistence. Weighting matters: an unweighted ranking is dominated by
# tiny-burden countries nobody forecasts for.
out$excess     <- out$mae_P000E - out$mae_pers
out$w_excess   <- out$w * out$excess
out$ND_vs_prod  <- (out$mae_ND  - out$mae_P000E) / out$mae_P000E
out$NDe_vs_prod <- (out$mae_NDe - out$mae_P000E) / out$mae_P000E
out <- out[order(-out$w_excess), ]

cat("\nWHERE PRODUCTION IS WEAKEST  (ranked by burden-weighted excess over persistence)\n")
cat(sprintf("%-5s %6s | %8s %8s %8s %8s | %8s\n",
            "iso","weight","P000E","ND","NDe","persist","w*excess"))
cat(strrep("-", 66), "\n")
for (i in seq_len(nrow(out))) with(out[i,], cat(sprintf(
   "%-5s %6.3f | %8.4f %8.4f %8.4f %8.4f | %8.5f\n",
   iso, w, mae_P000E, mae_ND, mae_NDe, mae_pers, w_excess)))

cat("\nNDe vs the production architecture, per country (negative = NDe better):\n")
o2 <- out[order(out$NDe_vs_prod), c("iso","w","mae_P000E","mae_NDe","NDe_vs_prod","mae_pers")]
for (i in seq_len(nrow(o2))) with(o2[i,], cat(sprintf(
   "  %-5s w=%.3f  %.4f -> %.4f   %+7.1f%%   %s\n", iso, w, mae_P000E, mae_NDe,
   100*NDe_vs_prod, if (mae_NDe < mae_pers) "BEATS persistence" else "")))
for (nm in c("ND","NDe")) {
  v <- out[[paste0(nm, "_vs_prod")]]
  cat(sprintf("%s is WORSE than production in %d of %d countries (%.0f%% of burden weight); beats persistence in %d.\n",
              nm, sum(v > 0, na.rm=TRUE), nrow(out), 100*sum(out$w[v > 0], na.rm=TRUE),
              sum(out[[paste0("mae_", nm)]] < out$mae_pers, na.rm=TRUE)))
}
utils::write.csv(out, file.path(HERE, "per_country_weakness.csv"), row.names=FALSE)
cat("wrote per_country_weakness.csv\n")
