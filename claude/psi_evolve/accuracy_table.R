# =============================================================================
# accuracy_table.R -- 12-week OOS predictive accuracy, in plain units.
#
# R2, MAE, RMSE and WIS for psi against observed transmission intensity on the
# held-out blocks, alongside the persistence and week-of-year-climatology
# baselines. Reported both country-equal and burden-weighted, because the
# weighting is this programme's objective choice, not a property of accuracy.
#
# usage: Rscript accuracy_table.R [ARM ...]      (default: all caches present)
# =============================================================================
suppressMessages(library(MOSAIC))
HERE  <- "/home/jgiles/psi_evolve"
CANON <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
VAR   <- "target_D_rate_per_country_floored"
PROD  <- "/home/jgiles/MOSAIC/MOSAIC-pkg/claude/forecast_cv_ocv4_q2yr/psi_cache"

grid <- utils::read.csv(file.path(HERE,"EVAL_GRID.csv"), stringsAsFactors=FALSE)
grid <- grid[grid$grid=="prod" & grid$split=="selection", ]
for (k in c("cutoff","test_start","test_end")) grid[[k]] <- as.Date(grid[[k]])
W <- utils::read.csv(file.path(HERE,"weights_frozen.csv"), stringsAsFactors=FALSE)
W$w <- W$w_sqrt/sum(W$w_sqrt); pool <- W$iso_code
obs <- utils::read.csv(CANON, stringsAsFactors=FALSE)[,c("iso_code","date",VAR)]
names(obs)[3] <- "observed"; obs$date <- as.Date(obs$date)
obs <- obs[is.finite(obs$observed) & obs$iso_code %in% pool, ]
wis_fn <- getFromNamespace(".rcv_wis","MOSAIC"); bl_fn <- getFromNamespace(".rcv_baseline","MOSAIC")

args <- commandArgs(trailingOnly=TRUE)
# Candidate arms = the known set UNION anything named on the command line, so a
# new ladder arm is scoreable the moment its cache exists. Previously the list
# was hardcoded and args only FILTERED it, so asking for an unknown arm (P000H)
# silently produced a table without it -- a wrong answer that looks like a right
# one. Now an unnamed-but-requested arm with no cache is an explicit error.
known <- c("P000","P000R","P000H","N5","N6","N8","D9b","F1","F2","F3","F4",
           # phase 2/3 arms -- listed so they appear in the DEFAULT table the
           # moment their cache exists. Args still ADD arms on top of this, and
           # a requested arm with no cache is an explicit error (not a silent
           # omission, which is how a wrong table once looked like a right one).
           "F5","F6","T2","N9","ND","NT","NF1","NF4")
caches <- c(P001 = PROD)
for (a in union(known, args)) {
  d <- file.path(HERE, paste0("psi_cache_", a))
  if (dir.exists(d)) caches[a] <- d
}
missing <- setdiff(args, c("P001", names(caches)))
if (length(missing))
  stop("accuracy_table: no psi cache for requested arm(s): ", paste(missing, collapse=", "))
if (length(args)) caches <- caches[names(caches) %in% c("P001", args)]

# restrict to cutoffs present in EVERY cache, so all arms are scored on identical cells
have <- lapply(caches, function(d) format(grid$cutoff)[file.exists(
          file.path(d, sprintf("psi_%s.csv", format(grid$cutoff))))])
common <- Reduce(intersect, have)
cat(sprintf("arms: %s\ncommon selection cutoffs: %d of %d  (%s)\n\n",
            paste(names(caches), collapse=", "), length(common), nrow(grid),
            paste(common, collapse=" ")))
if (!length(common)) quit(save="no")
# HARD GATE: refuse a PARTIAL selection grid. Scoring a subset of blocks is not
# merely less precise, it is not COMPARABLE to any number already on record --
# wave 29 measured per-block gains from -7.6% to +34.7% at weeks 9-13, so a
# 2-block subset can reorder the arms outright. This is the same shape as the
# PROTOCOL 5.3 breach already committed once (an accuracy table that swept the
# wrong blocks, disagreed at 0.1924 vs 0.2022, and changed which arm ranked
# best). The old behaviour printed "common selection cutoffs: 2 of 6" and
# carried on, which is easy to miss in a long table.
if (length(common) < nrow(grid)) {
     msg <- sprintf(paste0(
       "accuracy_table: PARTIAL GRID -- %d of %d selection cutoffs available.\n",
       "  missing: %s\n",
       "  Arms still running have not written every cutoff yet. Scoring now would\n",
       "  produce numbers that are NOT comparable to any figure on record.\n",
       "  Wait for the full grid, or set PSI_ALLOW_PARTIAL=1 for a deliberate,\n",
       "  clearly-labelled diagnostic (never for a REGISTRY row)."),
       length(common), nrow(grid),
       paste(setdiff(format(grid$cutoff), common), collapse=", "))
     if (!identical(Sys.getenv("PSI_ALLOW_PARTIAL"), "1")) stop(msg)
     cat("*** ", msg, "\n*** PROCEEDING UNDER PSI_ALLOW_PARTIAL=1 -- DIAGNOSTIC ONLY ***\n\n", sep="")
}

rows <- list()
for (nm in names(caches)) {
  for (ct in common) {
    i <- match(as.Date(ct), grid$cutoff)
    p <- utils::read.csv(file.path(caches[[nm]], sprintf("psi_%s.csv", ct)), stringsAsFactors=FALSE)
    p$date <- as.Date(p$date)
    p <- p[p$iso_code %in% pool & p$date >= grid$test_start[i] & p$date <= grid$test_end[i],
           c("iso_code","date","psi","q025","q25","q75","q975")]
    m <- merge(p, obs, by=c("iso_code","date"))
    if (!nrow(m)) next
    for (iso in unique(m$iso_code)) {
      z <- m[m$iso_code==iso, ]; if (nrow(z) < 4) next
      is_df <- obs[obs$iso_code==iso & obs$date <= grid$cutoff[i], ]
      if (nrow(is_df) < 8) next
      w  <- mean(wis_fn(z$observed, z$psi, z$q25, z$q75, z$q025, z$q975), na.rm=TRUE)
      bp <- bl_fn(is_df, z$date, "persistence"); bs <- bl_fn(is_df, z$date, "seasonal")
      rows[[length(rows)+1L]] <- data.frame(arm=nm, iso_code=iso, cutoff=ct,
        mae=mean(abs(z$observed - z$psi)), rmse=sqrt(mean((z$observed - z$psi)^2)),
        sse=sum((z$observed - z$psi)^2), sst=sum((z$observed - mean(z$observed))^2),
        r_corr=suppressWarnings(stats::cor(z$observed, z$psi)), wis=w,
        mae_pers=mean(abs(z$observed - bp$point)),
        wis_pers=mean(wis_fn(z$observed,bp$point,bp$pi50_lo,bp$pi50_hi,bp$pi95_lo,bp$pi95_hi),na.rm=TRUE),
        mae_seas=if (any(is.finite(bs$point))) mean(abs(z$observed - bs$point), na.rm=TRUE) else NA_real_,
        stringsAsFactors=FALSE)
    }
  }
}
d <- do.call(rbind, rows)
summ <- function(x, wt=NULL) if (is.null(wt)) mean(x, na.rm=TRUE) else
  stats::weighted.mean(x, wt, na.rm=TRUE)
cat(sprintf("%-6s %8s %8s %8s %8s %8s | %8s %8s | %8s\n","arm",
            "MAE","RMSE","R2_corr","R2_sse","WIS","MAE_pers","WIS_pers","MAE_seas"))
for (nm in names(caches)) {
  z <- d[d$arm==nm, ]; if (!nrow(z)) next
  wt <- W$w[match(z$iso_code, W$iso_code)]
  r2c <- summ(z$r_corr^2, wt); r2s <- 1 - sum(z$sse,na.rm=TRUE)/sum(z$sst,na.rm=TRUE)
  cat(sprintf("%-6s %8.4f %8.4f %8.3f %8.3f %8.4f | %8.4f %8.4f | %8.4f\n", nm,
      summ(z$mae,wt), summ(z$rmse,wt), r2c, r2s, summ(z$wis,wt),
      summ(z$mae_pers,wt), summ(z$wis_pers,wt), summ(z$mae_seas,wt)))
}
cat("\n(burden-weighted over the 16-country pool; R2_corr = mean per-cell squared\n")
cat(" correlation, R2_sse = 1 - SSE/SST pooled. Lower MAE/RMSE/WIS is better.)\n")
cat("\n-- country-equal (unweighted) --\n")
cat(sprintf("%-6s %8s %8s %8s | %8s %8s\n","arm","MAE","WIS","R2_corr","MAE_pers","MAE_seas"))
for (nm in names(caches)) {
  z <- d[d$arm==nm, ]; if (!nrow(z)) next
  cat(sprintf("%-6s %8.4f %8.4f %8.3f | %8.4f %8.4f\n", nm,
      summ(z$mae), summ(z$wis), summ(z$r_corr^2), summ(z$mae_pers), summ(z$mae_seas)))
}
