# =============================================================================
# shape_table.R -- is the prediction TRACKING THE DATA, or just flat?
#
# MAE rewards shrinking toward a slowly-moving level. This table scores every
# arm on the same cells with metrics a flat predictor CANNOT win, and includes
# PERSISTENCE and CLIMATOLOGY as rows so the flat reference is visible rather
# than assumed: persistence is constant within a block, so it scores sd_ratio 0,
# degen 1.00, dir_acc 0, and an undefined dcor. Any arm approaching those
# numbers is buying its MAE with flatness.
#
# usage: Rscript shape_table.R [ARM ...]
# =============================================================================
suppressMessages(library(MOSAIC))
HERE  <- "/home/jgiles/psi_evolve"
CANON <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
VAR   <- "target_D_rate_per_country_floored"
PROD  <- "/home/jgiles/MOSAIC/MOSAIC-pkg/claude/forecast_cv_ocv4_q2yr/psi_cache"
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
wis_fn <- getFromNamespace(".rcv_wis","MOSAIC"); bl_fn <- getFromNamespace(".rcv_baseline","MOSAIC")

args <- commandArgs(trailingOnly=TRUE)
known <- c("P000","P000R","P000H","N5","N6","N8","D9b","F1","F3","F4",
           "F5","F6","T2","N9","ND","NT","NF1","NF4",
           # P000E is the re-baseline forced by the epoch fix
           # (.psi_epoch_from_history). Rows ABOVE it were fitted at
           # best + patience; P000E, T2, N9 and ND were fitted at best. Only
           # arms on the same side of that line are directly comparable.
           "P000E")
caches <- c(P001 = PROD)
for (a in union(known, args)) {
  d <- file.path(HERE, paste0("psi_cache_", a)); if (dir.exists(d)) caches[a] <- d
}
have <- lapply(caches, function(d) format(grid$cutoff)[file.exists(
          file.path(d, sprintf("psi_%s.csv", format(grid$cutoff))))])
common <- Reduce(intersect, have)
if (length(common) < nrow(grid))
  stop("shape_table: partial grid -- ", length(common), " of ", nrow(grid))
cat(sprintf("cells: %d selection cutoffs, pool of %d countries\n\n", length(common), length(pool)))

# long frame: one row per arm x country x cutoff x date
long <- list(); wisrow <- list()
for (nm in names(caches)) {
  for (ct in common) {
    i <- match(as.Date(ct), grid$cutoff)
    p <- utils::read.csv(file.path(caches[[nm]], sprintf("psi_%s.csv", ct)), stringsAsFactors=FALSE)
    p$date <- as.Date(p$date)
    p <- p[p$iso_code %in% pool & p$date >= grid$test_start[i] & p$date <= grid$test_end[i],
           c("iso_code","date","psi","q025","q25","q75","q975")]
    m <- merge(p, obs, by=c("iso_code","date")); if (!nrow(m)) next
    m <- m[order(m$iso_code, m$date), ]
    keep <- unlist(lapply(split(seq_len(nrow(m)), m$iso_code), function(ix)
              if (length(ix) >= 4L &&
                  sum(obs$iso_code==m$iso_code[ix[1]] & obs$date<=grid$cutoff[i]) >= 8L) ix))
    m <- m[sort(keep), , drop=FALSE]; if (!nrow(m)) next
    m$fold <- ct
    long[[length(long)+1L]] <- data.frame(arm=nm, m[,c("iso_code","date","fold","observed")],
                                          pred=m$psi, stringsAsFactors=FALSE)
    wisrow[[length(wisrow)+1L]] <- data.frame(arm=nm, iso_code=m$iso_code,
      wis=wis_fn(m$observed, m$psi, m$q25, m$q75, m$q025, m$q975), stringsAsFactors=FALSE)
  }
}
L <- do.call(rbind, long)

# baselines as rows, on the SAME cells -- this is the flat reference
base_cells <- unique(L[L$arm==names(caches)[1], c("iso_code","date","fold","observed")])
for (bn in c("persistence","seasonal")) {
  acc <- list()
  for (ct in common) {
    i <- match(as.Date(ct), grid$cutoff)
    z <- base_cells[base_cells$fold==ct, ]
    for (iso in unique(z$iso_code)) {
      zz <- z[z$iso_code==iso, ]; zz <- zz[order(zz$date), ]
      is_df <- obs[obs$iso_code==iso & obs$date <= grid$cutoff[i], ]
      b <- bl_fn(is_df, zz$date, bn)
      acc[[length(acc)+1L]] <- data.frame(arm=bn, zz[,c("iso_code","date","fold","observed")],
                                          pred=b$point, stringsAsFactors=FALSE)
    }
  }
  L <- rbind(L, do.call(rbind, acc))
}

WI <- do.call(rbind, wisrow)
cat(sprintf("%-6s %7s %7s %7s %7s | %6s %8s %7s %7s %6s\n", "arm",
            "MAE","R2corr","R2sse","WIS", "bias","sd_ratio","dcor","dir_acc","degen"))
cat(strrep("-", 88), "\n")
ord <- c(names(caches), "persistence", "seasonal")
for (nm in ord) {
  z <- L[L$arm==nm, ]; if (!nrow(z)) next
  w  <- unname(wts[match(z$iso_code, names(wts))])
  key <- paste(z$iso_code, z$fold, sep="\r")
  per <- split(seq_len(nrow(z)), key)
  mae <- stats::weighted.mean(vapply(per, function(ix) mean(abs(z$observed[ix]-z$pred[ix])), numeric(1)),
          vapply(per, function(ix) wts[[z$iso_code[ix[1]]]], numeric(1)))
  r2c <- stats::weighted.mean(vapply(per, function(ix)
          suppressWarnings(stats::cor(z$observed[ix], z$pred[ix]))^2, numeric(1)),
          vapply(per, function(ix) wts[[z$iso_code[ix[1]]]], numeric(1)), na.rm=TRUE)
  # SST is computed WITHIN each country-block, matching accuracy_table.R. Using a
  # global mean instead makes every arm look far better (P001 -0.29 vs -6.39)
  # because between-country variance is then credited to the model.
  r2s <- 1 - sum(vapply(per, function(ix) sum((z$observed[ix]-z$pred[ix])^2), numeric(1))) /
             sum(vapply(per, function(ix) sum((z$observed[ix]-mean(z$observed[ix]))^2), numeric(1)))
  sm  <- .shape_metrics(z, "pred", wts, "fold")
  wv  <- if (nm %in% WI$arm) stats::weighted.mean(WI$wis[WI$arm==nm],
            unname(wts[match(WI$iso_code[WI$arm==nm], names(wts))]), na.rm=TRUE) else NA_real_
  cat(sprintf("%-6s %7.4f %7.3f %7.3f %7s | %6.2f %8.2f %7s %7.2f %6.2f\n", nm,
      mae, r2c, r2s, if (is.finite(wv)) sprintf("%.4f", wv) else "  --",
      sm[["bias"]], sm[["sd_ratio"]],
      if (is.finite(sm[["dcor"]])) sprintf("%.3f", sm[["dcor"]]) else " undef",
      sm[["dir_acc"]], sm[["degen"]]))
}
cat("\nsd_ratio = sd(pred)/sd(obs) per country-block (0 = flat, 1 = lifelike amplitude)\n")
cat("dcor     = corr of week-to-week CHANGES (the trend-tracking metric)\n")
cat("dir_acc  = share of steps with the right direction of change (0.5 = coin flip)\n")
cat("degen    = share of country-blocks where the prediction never moved\n")
