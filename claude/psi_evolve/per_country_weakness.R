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

ARMS <- c("P000E","N9","ND")
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
                mae_P000E = r$P000E[["mae"]], mae_N9 = r$N9[["mae"]],
                mae_ND = r$ND[["mae"]], mae_pers = r$persistence[["mae"]],
                sdr_P000E = r$P000E[["sdr"]], sdr_ND = r$ND[["sdr"]],
                dacc_P000E = r$P000E[["dacc"]], dacc_ND = r$ND[["dacc"]],
                stringsAsFactors = FALSE)
}))
# "Weakest" = the biggest BURDEN-WEIGHTED excess of the production architecture
# over persistence. Weighting matters: an unweighted ranking is dominated by
# tiny-burden countries nobody forecasts for.
out$excess     <- out$mae_P000E - out$mae_pers
out$w_excess   <- out$w * out$excess
out$ND_vs_prod <- (out$mae_ND - out$mae_P000E) / out$mae_P000E
out <- out[order(-out$w_excess), ]

cat("\nWHERE PRODUCTION IS WEAKEST  (ranked by burden-weighted excess over persistence)\n")
cat(sprintf("%-5s %6s | %8s %8s %8s %8s | %8s | %8s %8s | %7s %7s\n",
            "iso","weight","P000E","N9","ND","persist","w*excess","sdrP","sdrND","daccP","daccND"))
cat(strrep("-", 108), "\n")
for (i in seq_len(nrow(out))) with(out[i,], cat(sprintf(
   "%-5s %6.3f | %8.4f %8.4f %8.4f %8.4f | %8.5f | %8.2f %8.2f | %7.2f %7.2f\n",
   iso, w, mae_P000E, mae_N9, mae_ND, mae_pers, w_excess, sdr_P000E, sdr_ND,
   dacc_P000E, dacc_ND)))

cat("\nND vs the production architecture, per country (negative = ND better):\n")
o2 <- out[order(out$ND_vs_prod), c("iso","w","mae_P000E","mae_ND","ND_vs_prod")]
for (i in seq_len(nrow(o2))) with(o2[i,], cat(sprintf(
   "  %-5s w=%.3f  %.4f -> %.4f   %+6.1f%%\n", iso, w, mae_P000E, mae_ND, 100*ND_vs_prod)))
cat(sprintf("\nND is WORSE than production in %d of %d countries (%.0f%% of burden weight).\n",
            sum(out$ND_vs_prod > 0, na.rm=TRUE), nrow(out),
            100*sum(out$w[out$ND_vs_prod > 0], na.rm=TRUE)))
utils::write.csv(out, file.path(HERE, "per_country_weakness.csv"), row.names=FALSE)
cat("wrote per_country_weakness.csv\n")
