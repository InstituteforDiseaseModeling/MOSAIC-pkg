# How noisy is each metric across two fits of the SAME model? The programme has
# been scored on WIS-SKILL (a ratio to a per-country baseline). If the direct
# measures are far more stable, the "cannot resolve anything" problem is a
# metric choice, not a property of the model or the compute.
suppressMessages(library(MOSAIC))
HERE <- "/home/jgiles/psi_evolve"
src  <- file.path(HERE, "acc_cells.rds")
# recompute cells for the two replicates only
CANON <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
VAR <- "target_D_rate_per_country_floored"
grid <- utils::read.csv(file.path(HERE,"EVAL_GRID.csv"), stringsAsFactors=FALSE)
grid <- grid[grid$grid=="prod" & grid$split=="selection", ]
for (k in c("cutoff","test_start","test_end")) grid[[k]] <- as.Date(grid[[k]])
W <- utils::read.csv(file.path(HERE,"weights_frozen.csv"), stringsAsFactors=FALSE)
W$w <- W$w_sqrt/sum(W$w_sqrt); pool <- W$iso_code
obs <- utils::read.csv(CANON, stringsAsFactors=FALSE)[,c("iso_code","date",VAR)]
names(obs)[3] <- "observed"; obs$date <- as.Date(obs$date)
obs <- obs[is.finite(obs$observed) & obs$iso_code %in% pool, ]
wis_fn <- getFromNamespace(".rcv_wis","MOSAIC"); bl_fn <- getFromNamespace(".rcv_baseline","MOSAIC")
grab <- function(dir) {
  rows <- list()
  for (i in seq_len(nrow(grid))) {
    f <- file.path(dir, sprintf("psi_%s.csv", format(grid$cutoff[i]))); if (!file.exists(f)) next
    p <- utils::read.csv(f, stringsAsFactors=FALSE); p$date <- as.Date(p$date)
    p <- p[p$iso_code %in% pool & p$date>=grid$test_start[i] & p$date<=grid$test_end[i],
           c("iso_code","date","psi","q025","q25","q75","q975")]
    m <- merge(p, obs, by=c("iso_code","date")); if (!nrow(m)) next
    for (iso in unique(m$iso_code)) {
      z <- m[m$iso_code==iso,]; if (nrow(z)<4) next
      is_df <- obs[obs$iso_code==iso & obs$date<=grid$cutoff[i],]; if (nrow(is_df)<8) next
      b <- bl_fn(is_df, z$date, "persistence")
      wm <- mean(wis_fn(z$observed,z$psi,z$q25,z$q75,z$q025,z$q975),na.rm=TRUE)
      wb <- mean(wis_fn(z$observed,b$point,b$pi50_lo,b$pi50_hi,b$pi95_lo,b$pi95_hi),na.rm=TRUE)
      rows[[length(rows)+1L]] <- data.frame(iso_code=iso, fold=grid$block[i],
        mae=mean(abs(z$observed-z$psi)), rmse=sqrt(mean((z$observed-z$psi)^2)),
        wis=wm, r2=suppressWarnings(stats::cor(z$observed,z$psi))^2,
        skill=if (is.finite(wb)&&wb>0) 1-wm/wb else NA_real_, stringsAsFactors=FALSE)
    }
  }
  do.call(rbind, rows)
}
A <- grab(file.path(HERE,"psi_cache_P000")); B <- grab(file.path(HERE,"psi_cache_P000R"))
agg <- function(d, col, fn=mean) { pi <- stats::aggregate(d[[col]] ~ iso_code, d, fn)
  names(pi)[2] <- "v"; w <- W$w[match(pi$iso_code, W$iso_code)]
  sum(w/sum(w) * pi$v) }
cat(sprintf("%-10s %10s %10s %10s %10s\n","metric","P000","P000R","|diff|","% of level"))
for (m in c("mae","rmse","wis","r2")) {
  a <- agg(A,m); b <- agg(B,m)
  cat(sprintf("%-10s %10.4f %10.4f %10.4f %9.2f%%\n", toupper(m), a, b, abs(a-b),
              100*abs(a-b)/mean(abs(c(a,b)))))
}
a <- agg(A,"skill",stats::median); b <- agg(B,"skill",stats::median)
cat(sprintf("%-10s %10.4f %10.4f %10.4f %9.2f%%   <-- the metric this programme has used\n",
            "WIS-SKILL", a, b, abs(a-b), 100*abs(a-b)/mean(abs(c(a,b)))))
cat("\nREAD: if MAE/WIS/R2 differ by a fraction of a percent between two fits of the\n")
cat("same model while WIS-SKILL differs by tens of percent, the resolution problem\n")
cat("is the SKILL RATIO, not the model and not the compute.\n")
