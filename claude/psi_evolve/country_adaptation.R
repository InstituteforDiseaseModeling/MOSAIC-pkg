# =============================================================================
# country_adaptation.R -- did the architecture changes actually make the GLOBAL
# model more country-specific?
#
# The N5 / N6 / N8 / D9b arms were all motivated by country variability, but
# every result so far is POOLED, which cannot tell "uniformly better" apart from
# "more differentiated". Three direct measures, all computed on the same OOS
# cells and compared against what the OBSERVED data actually does:
#
#  xcorr      mean pairwise correlation between countries' prediction series
#             within a fold. A global model with weak country adaptation moves
#             every country together on the shared seasonal signal -> xcorr near
#             1. The observed data's own xcorr is the target: ADAPTATION MEANS
#             MOVING TOWARD IT, not simply downward.
#  cv_level   between-country coefficient of variation of the per-country mean
#             prediction. Does the model give countries distinct LEVELS?
#  cv_amp     between-country CV of the per-country prediction SD. Does it give
#             them distinct AMPLITUDES?
#
# Plus per-country MAE, to see whether any arm helped the countries the
# incumbent handles worst -- the practical form of the same question.
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
pool <- W$iso_code
obs <- utils::read.csv(CANON, stringsAsFactors=FALSE)[,c("iso_code","date",VAR)]
names(obs)[3] <- "observed"; obs$date <- as.Date(obs$date)
obs <- obs[is.finite(obs$observed) & obs$iso_code %in% pool, ]

arms <- c("P000","P000H","N5","N6","N8","D9b","F3")
caches <- c(P001 = PROD)
for (a in arms) { d <- file.path(HERE, paste0("psi_cache_", a)); if (dir.exists(d)) caches[a] <- d }

# wide matrix: rows = dates in the OOS window, cols = countries
widen <- function(df, val) {
     u <- sort(unique(df$date)); iso <- sort(unique(df$iso_code))
     m <- matrix(NA_real_, length(u), length(iso), dimnames=list(as.character(u), iso))
     m[cbind(match(as.character(df$date), rownames(m)), match(df$iso_code, iso))] <- df[[val]]
     m
}
mean_xcorr <- function(m) {
     keep <- apply(m, 2, function(v) sum(is.finite(v)) >= 6L && stats::sd(v, na.rm=TRUE) > 1e-12)
     m <- m[, keep, drop=FALSE]; if (ncol(m) < 3L) return(NA_real_)
     cc <- suppressWarnings(stats::cor(m, use="pairwise.complete.obs"))
     mean(cc[upper.tri(cc)], na.rm=TRUE)
}
cvf <- function(v) { v <- v[is.finite(v)]; if (length(v) < 3L || mean(v) <= 0) NA_real_ else
                     stats::sd(v)/mean(v) }

rows <- list(); percountry <- list()
for (nm in c(names(caches), "OBSERVED")) {
  xc <- lv <- am <- numeric(0)
  for (i in seq_len(nrow(grid))) {
    ct <- format(grid$cutoff[i])
    if (nm == "OBSERVED") {
      d <- obs[obs$date >= grid$test_start[i] & obs$date <= grid$test_end[i], ]
      if (!nrow(d)) next
      m <- widen(d, "observed")
    } else {
      f <- file.path(caches[[nm]], sprintf("psi_%s.csv", ct)); if (!file.exists(f)) next
      p <- utils::read.csv(f, stringsAsFactors=FALSE); p$date <- as.Date(p$date)
      p <- p[p$iso_code %in% pool & p$date >= grid$test_start[i] & p$date <= grid$test_end[i], ]
      if (!nrow(p)) next
      m <- widen(p, "psi")
    }
    xc <- c(xc, mean_xcorr(m))
    lv <- c(lv, cvf(colMeans(m, na.rm=TRUE)))
    am <- c(am, cvf(apply(m, 2, stats::sd, na.rm=TRUE)))
    if (nm != "OBSERVED") {
      mm <- merge(if (nm=="OBSERVED") NULL else
                    data.frame(iso_code=rep(colnames(m), each=nrow(m)),
                               date=as.Date(rep(rownames(m), ncol(m))),
                               pred=as.vector(m), stringsAsFactors=FALSE),
                  obs, by=c("iso_code","date"))
      mm <- mm[is.finite(mm$pred) & is.finite(mm$observed), ]
      if (nrow(mm)) percountry[[length(percountry)+1L]] <-
        data.frame(arm=nm, iso_code=mm$iso_code, ae=abs(mm$observed-mm$pred),
                   stringsAsFactors=FALSE)
    }
  }
  rows[[length(rows)+1L]] <- data.frame(arm=nm, xcorr=mean(xc, na.rm=TRUE),
    cv_level=mean(lv, na.rm=TRUE), cv_amp=mean(am, na.rm=TRUE), stringsAsFactors=FALSE)
}
R <- do.call(rbind, rows)
obs_row <- R[R$arm=="OBSERVED", ]
cat("=== COUNTRY DIFFERENTIATION on the OOS blocks (6 selection folds) ===\n")
cat(sprintf("%-8s %9s %10s %9s\n", "arm", "xcorr", "cv_level", "cv_amp"))
for (i in seq_len(nrow(R))) cat(sprintf("%-8s %9.3f %10.3f %9.3f%s\n", R$arm[i],
   R$xcorr[i], R$cv_level[i], R$cv_amp[i], if (R$arm[i]=="OBSERVED") "   <-- TARGET" else ""))
cat("\nxcorr    = mean pairwise correlation BETWEEN countries (1.0 = every country moves together)\n")
cat("cv_level = between-country spread of mean level;  cv_amp = spread of amplitude\n")
cat("Adaptation means moving TOWARD the OBSERVED row, not merely away from the incumbent.\n")

PC <- do.call(rbind, percountry)
pc <- stats::aggregate(ae ~ arm + iso_code, PC, mean)
w <- reshape(pc, idvar="iso_code", timevar="arm", direction="wide")
names(w) <- sub("^ae\\.", "", names(w))
base <- "P001"
cmp <- setdiff(names(w), c("iso_code", base))
cat("\n=== per-country MAE, and change vs the P001 incumbent (negative = better) ===\n")
cat(sprintf("%-5s %8s", "iso", base))
for (a in cmp) cat(sprintf(" %8s", a)); cat("\n")
o <- order(-w[[base]])
for (i in o) {
  cat(sprintf("%-5s %8.4f", w$iso_code[i], w[[base]][i]))
  for (a in cmp) cat(sprintf(" %+8.1f%%", 100*(w[[a]][i]-w[[base]][i])/w[[base]][i]))
  cat("\n")
}
cat("\n(rows sorted worst-to-best under the incumbent: if an architecture change\n")
cat(" bought country ADAPTATION, the biggest gains should sit at the TOP.)\n")
