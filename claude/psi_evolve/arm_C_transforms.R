# =============================================================================
# Class-C arms against the horizon LEVEL COLLAPSE found by C6/C6b.
#
# Measured: psi is well calibrated in-sample (0.273 vs observed 0.238) and decays
# to 0.089 against observed 0.214 by week 9+ out-of-sample -- a systematic
# -0.125 bias that grows with horizon, present identically in psi, pred_smooth
# and pred_raw (so it is the MODEL, not the post-processing).
#
#   C9a  per-country logit shift so the block's median psi matches its own
#        last-26-pre-cutoff-week median.  Level only, shape untouched.
#   C9b  horizon-indexed correction from the PREVIOUS cutoff's realized error.
#   C7   convex combination with the week-of-year climatology, lambda per
#        country from pre-cutoff MAE.
#
# All three are CLASS C: identical fits, post-processing only, zero compute and
# no fit-noise floor. Every transform is applied to the point AND the four
# quantiles, so the predictive distribution is relocated as a whole -- applying
# it to the point alone would recreate the wave-7 defect where the interval is
# not centred on what is being scored.
#
# LEAKAGE: C9a uses only the model's own psi at dates <= cutoff. C7 uses observed
# data at dates <= cutoff. C9b uses the previous cutoff's psi plus observations
# up to THIS cutoff, truncated so nothing after T_i is ever read.
# =============================================================================
suppressMessages(library(MOSAIC))
HERE  <- "/home/jgiles/psi_evolve"
CACHE <- Sys.getenv("PSI_CACHE",
  "/home/jgiles/MOSAIC/MOSAIC-pkg/claude/forecast_cv_ocv4_q2yr/psi_cache")
CANON <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
VAR   <- "target_D_rate_per_country_floored"
MODE  <- Sys.getenv("PSI_MODE", "selection")
source(file.path(HERE, "score_psi_arm.R"))

EPS <- 1e-4
lg  <- function(p) log(pmin(1-EPS, pmax(EPS, p)) / (1 - pmin(1-EPS, pmax(EPS, p))))
ex  <- function(x) 1 / (1 + exp(-x))
QC  <- c("psi","q025","q25","q75","q975")

grid <- utils::read.csv(file.path(HERE,"EVAL_GRID.csv"), stringsAsFactors=FALSE)
grid <- grid[grid$grid == "prod", ]
for (k in c("cutoff","test_start","test_end")) grid[[k]] <- as.Date(grid[[k]])
pool <- utils::read.csv(file.path(HERE,"weights_frozen.csv"), stringsAsFactors=FALSE)$iso_code
obs <- utils::read.csv(CANON, stringsAsFactors=FALSE)[, c("iso_code","date",VAR)]
names(obs)[3] <- "observed"; obs$date <- as.Date(obs$date)
obs <- obs[is.finite(obs$observed), ]
obs <- obs[!duplicated(obs[, c("iso_code","date")]), ]

rd <- function(i) {
  f <- file.path(CACHE, sprintf("psi_%s.csv", format(grid$cutoff[i])))
  if (!file.exists(f)) return(NULL)
  p <- utils::read.csv(f, stringsAsFactors=FALSE); p$date <- as.Date(p$date)
  p[p$iso_code %in% pool, , drop=FALSE]
}

blocks <- list(); pres <- list()
for (i in seq_len(nrow(grid))) {
  p <- rd(i); if (is.null(p)) next
  T0  <- grid$cutoff[i]
  blk <- p[p$date >= grid$test_start[i] & p$date <= grid$test_end[i], ]
  pre <- p[p$date <= T0, ]
  if (!nrow(blk)) next
  blk$fold <- grid$block[i]; pre$fold <- grid$block[i]
  blk$wk <- as.integer(floor(as.numeric(blk$date - (T0 + 14L)) / 7)) + 1L

  # ---------- C9a: level re-anchoring (model output only) ------------------
  blk$c9a_sh <- 0
  for (iso in unique(blk$iso_code)) {
    pr <- pre[pre$iso_code == iso & pre$date > T0 - 182L, "psi"]
    bk <- blk$iso_code == iso
    if (length(pr) >= 8L && any(bk))
      blk$c9a_sh[bk] <- stats::median(lg(pr)) - stats::median(lg(blk$psi[bk]))
  }

  # ---------- C9b: horizon-indexed decay from the PREVIOUS cutoff ----------
  blk$c9b_sh <- 0
  if (i > 1L) {
    q <- rd(i - 1L)
    if (!is.null(q)) {
      Tp <- grid$cutoff[i - 1L]
      z  <- q[q$date >= Tp + 15L & q$date <= min(grid$test_end[i-1L], T0), ]
      if (nrow(z)) {
        z$wk <- as.integer(floor(as.numeric(z$date - (Tp + 14L)) / 7)) + 1L
        z <- merge(z[, c("iso_code","date","psi","wk")], obs, by=c("iso_code","date"))
        z <- z[is.finite(z$observed), ]
        if (nrow(z) > 20L) {
          # mean logit gap per horizon week, pooled across countries (one block
          # per country is too little for a per-country curve)
          cv <- stats::aggregate(list(g = lg(z$observed) - lg(z$psi)),
                                 by = list(wk = z$wk), FUN = stats::median)
          cv <- cv[order(cv$wk), ]
          blk$c9b_sh <- cv$g[match(pmin(blk$wk, max(cv$wk)), cv$wk)]
          blk$c9b_sh[!is.finite(blk$c9b_sh)] <- 0
        }
      }
    }
  }

  # ---------- C9c: HORIZON-INDEXED shift toward psi's OWN in-sample level --
  # C9a applied one flat shift per country and damaged the short horizons
  # (h1mo -0.189 -> -0.467) while helping the long ones, because at week 1-2
  # there is little decay to undo. C9b was horizon-indexed but corrected toward
  # the PREVIOUS BLOCK'S OBSERVED level, so it imported that block's realised
  # anomaly and applied it pooled across countries (dS -0.247). C9c keeps the
  # horizon indexing and corrects toward the model's OWN pre-cutoff level, per
  # country -- so the shift is ~0 at week 1 and grows exactly as the decay does,
  # and it never reads an observation.
  blk$c9c_sh <- 0
  for (iso in unique(blk$iso_code)) {
    pr <- pre[pre$iso_code == iso & pre$date > T0 - 182L, "psi"]
    bk <- which(blk$iso_code == iso)
    if (length(pr) < 8L || !length(bk)) next
    ref <- stats::median(lg(pr))
    for (w in unique(blk$wk[bk])) {
      j <- bk[blk$wk[bk] == w]
      blk$c9c_sh[j] <- ref - stats::median(lg(blk$psi[j]))
    }
  }

  # ---------- C7: combination with the week-of-year climatology ------------
  blk$c7_lam <- 1; blk$c7_clim <- NA_real_
  for (iso in unique(blk$iso_code)) {
    oi <- obs[obs$iso_code == iso & obs$date <= T0, ]
    if (nrow(oi) < 104L) next
    woy  <- as.integer(format(oi$date, "%V"))
    clim <- tapply(oi$observed, woy, mean, na.rm = TRUE)
    bk   <- blk$iso_code == iso
    cb   <- as.numeric(clim[as.character(as.integer(format(blk$date[bk], "%V")))])
    blk$c7_clim[bk] <- cb
    # lambda on a grid, minimising MAE over the last 52 PRE-cutoff weeks
    pp <- merge(pre[pre$iso_code == iso & pre$date > T0 - 364L, c("date","psi")],
                oi[, c("date","observed")], by = "date")
    if (!nrow(pp)) next
    pc <- as.numeric(clim[as.character(as.integer(format(pp$date, "%V")))])
    ok <- is.finite(pc) & is.finite(pp$psi) & is.finite(pp$observed)
    if (sum(ok) < 20L) next
    lams <- seq(0, 1, by = 0.05)
    mae  <- vapply(lams, function(L)
              mean(abs(pp$observed[ok] - (L*pp$psi[ok] + (1-L)*pc[ok]))), numeric(1))
    blk$c7_lam[bk] <- lams[which.min(mae)]
  }
  blocks[[length(blocks)+1L]] <- blk; pres[[length(pres)+1L]] <- pre
}
B <- do.call(rbind, blocks); P <- do.call(rbind, pres)
cat("blocks loaded:", length(unique(B$fold)), " rows:", nrow(B), "\n")
cat("C7 lambda distribution:\n"); print(round(tapply(B$c7_lam, B$iso_code, max), 2))

# ---- build the derived frames (transform point AND quantiles) -------------
shift_all <- function(df, sh) { for (c_ in QC) df[[c_]] <- ex(lg(df[[c_]]) + sh); df }
combo_all <- function(df, lam, clim) {
  pt <- lam * df$psi + (1 - lam) * clim
  bad <- !is.finite(pt); pt[bad] <- df$psi[bad]
  d <- lg(pt) - lg(df$psi)                       # relocate the whole distribution
  for (c_ in QC) df[[c_]] <- ex(lg(df[[c_]]) + d)
  df
}
# ---- C9d: gate the C9c half-strength correction on pre-cutoff incidence ----
# cor(pre-cutoff observed level, C9c_h delta) = +0.738, STRONGER than the
# correlation with the block outcome (+0.476), so the gate is a legitimate
# cutoff-time variable rather than a bet on what happened. Threshold = the POOL
# MEDIAN of pre-cutoff median observed for that cutoff: parameter-free, so
# nothing is tuned on the evaluation blocks.
B$c9d_sh <- 0
for (fl in unique(B$fold)) {
  T0 <- grid$cutoff[match(fl, grid$block)]
  # DEFECT FIXED 2026-09-18: `obs` spans all 40 ISOs, so a median over it was
  # dragged down by the 24 countries outside the scoring pool and the gate
  # admitted nearly everyone -- including ZMB (pre-cutoff 0.024), which is the
  # country the gate exists to exclude. The registered rule is the POOL median.
  po <- obs[obs$date <= T0 & obs$date > T0 - 182L & obs$iso_code %in% pool, ]
  lev <- tapply(po$observed, po$iso_code, stats::median)
  lev <- lev[names(lev) %in% unique(B$iso_code[B$fold == fl])]
  if (!length(lev)) next
  thr <- stats::median(lev, na.rm = TRUE)
  keep <- names(lev)[is.finite(lev) & lev > thr]
  j <- B$fold == fl & B$iso_code %in% keep
  B$c9d_sh[j] <- 0.5 * B$c9c_sh[j]
}
cat(sprintf("C9d: correction applied to %.1f%% of block rows\n",
            100 * mean(B$c9d_sh != 0)))

# ---- C7b: combination weight from OUT-OF-SAMPLE history, not pre-cutoff fit --
# C7 failed for a diagnosed reason: lambda fitted on PRE-CUTOFF error came out
# 1.00 for 14 of 16 countries, because psi is excellent in-sample (0.273 vs
# observed 0.238) and only collapses out of sample. The information a pre-cutoff
# fit cannot have is exactly the out-of-sample collapse -- but EARLIER CUTOFFS
# have already revealed it. At cutoff T_i, choose lambda per country on the
# realised out-of-sample blocks of cutoffs T_1..T_(i-1), truncated to dates <= T_i.
# Leakage-clean, and it is how an operational system would learn its own decay.
B$c7b_lam <- 1
for (fl in sort(unique(B$fold))) {
  gi <- match(fl, grid$block); T0 <- grid$cutoff[gi]
  hist <- list()
  for (k in seq_len(gi - 1L)) {
    q <- rd(k); if (is.null(q)) next
    Tk <- grid$cutoff[k]
    z  <- q[q$date >= Tk + 15L & q$date <= min(grid$test_end[k], T0), ]
    if (nrow(z)) hist[[length(hist)+1L]] <- z[, c("iso_code","date","psi")]
  }
  if (!length(hist)) next                        # first cutoff: no history, lambda = 1
  H <- do.call(rbind, hist)
  H <- merge(H, obs, by = c("iso_code","date"))
  H <- H[is.finite(H$observed), ]
  for (iso in unique(B$iso_code[B$fold == fl])) {
    h  <- H[H$iso_code == iso, ]
    oi <- obs[obs$iso_code == iso & obs$date <= T0, ]
    if (nrow(h) < 10L || nrow(oi) < 104L) next
    woy  <- as.integer(format(oi$date, "%V"))
    clim <- tapply(oi$observed, woy, mean, na.rm = TRUE)
    hc   <- as.numeric(clim[as.character(as.integer(format(h$date, "%V")))])
    ok   <- is.finite(hc)
    if (sum(ok) < 10L) next
    lams <- seq(0, 1, by = 0.05)
    mae  <- vapply(lams, function(L)
              mean(abs(h$observed[ok] - (L*h$psi[ok] + (1-L)*hc[ok]))), numeric(1))
    B$c7b_lam[B$fold == fl & B$iso_code == iso] <- lams[which.min(mae)]
  }
}
# ---- C7c: HORIZON-DEPENDENT combination weight ----------------------------
# C7b helps at long horizon (h3mo -0.243 vs P001 -0.296) and hurts badly at
# short (h1mo -0.527 vs -0.189), which is exactly what the decay implies: psi is
# still good at week 1-2 and dead by week 9+, while climatology holds the level
# throughout. So lambda should RAMP with horizon. The ramp is not chosen by
# looking at the blocks -- it is estimated per horizon-week by the same
# out-of-sample estimator as C7b, pooled across countries (one block per country
# per horizon-week is too thin for a per-country curve).
B$c7c_lam <- 1
for (fl in sort(unique(B$fold))) {
  gi <- match(fl, grid$block); T0 <- grid$cutoff[gi]
  hist <- list()
  for (k in seq_len(gi - 1L)) {
    q <- rd(k); if (is.null(q)) next
    Tk <- grid$cutoff[k]
    z  <- q[q$date >= Tk + 15L & q$date <= min(grid$test_end[k], T0), ]
    if (!nrow(z)) next
    z$wk <- as.integer(floor(as.numeric(z$date - (Tk + 14L)) / 7)) + 1L
    hist[[length(hist)+1L]] <- z[, c("iso_code","date","psi","wk")]
  }
  if (!length(hist)) next
  H <- merge(do.call(rbind, hist), obs, by = c("iso_code","date"))
  H <- H[is.finite(H$observed) & H$iso_code %in% pool, ]
  if (!nrow(H)) next
  # climatology for each history row, from that country's pre-T0 observations
  H$clim <- NA_real_
  for (iso in unique(H$iso_code)) {
    oi <- obs[obs$iso_code == iso & obs$date <= T0, ]
    if (nrow(oi) < 104L) next
    cl <- tapply(oi$observed, as.integer(format(oi$date, "%V")), mean, na.rm = TRUE)
    j  <- H$iso_code == iso
    H$clim[j] <- as.numeric(cl[as.character(as.integer(format(H$date[j], "%V")))])
  }
  H <- H[is.finite(H$clim), ]
  lams <- seq(0, 1, by = 0.05)
  for (w in sort(unique(B$wk[B$fold == fl]))) {
    h <- H[H$wk == w, ]
    if (nrow(h) < 10L) next
    mae <- vapply(lams, function(L)
             mean(abs(h$observed - (L*h$psi + (1-L)*h$clim))), numeric(1))
    B$c7c_lam[B$fold == fl & B$wk == w] <- lams[which.min(mae)]
  }
}
cat("C7c lambda by horizon week (median over blocks):\n")
print(round(tapply(B$c7c_lam, B$wk, stats::median), 2))

cat("C7b lambda (median over blocks, per country):\n")
print(round(tapply(B$c7b_lam, B$iso_code, stats::median), 2))

variants <- list(
  P001    = B,
  C9d     = shift_all(B, B$c9d_sh),
  C7b     = combo_all(B, B$c7b_lam, B$c7_clim),
  C7c     = combo_all(B, B$c7c_lam, B$c7_clim),
  C7c_C9d = combo_all(shift_all(B, B$c9d_sh), B$c7c_lam, B$c7_clim),
  C7b_C9d = combo_all(shift_all(B, B$c9d_sh), B$c7b_lam, B$c7_clim),
  C9a     = shift_all(B, B$c9a_sh),
  C9b     = shift_all(B, B$c9b_sh),
  C9c     = shift_all(B, B$c9c_sh),
  C9c_h   = shift_all(B, 0.5 * B$c9c_sh),   # half-strength: WIS punishes over-correction
  C7      = combo_all(B, B$c7_lam, B$c7_clim),
  C7_C9a  = combo_all(shift_all(B, B$c9a_sh), B$c7_lam, B$c7_clim)
)
folds <- data.frame(fold = grid$block, train_end = grid$cutoff,
                    test_start = grid$test_start, test_end = grid$test_end)
folds <- folds[folds$fold %in% unique(B$fold), ]

out <- list()
for (nm in names(variants)) {
  v  <- variants[[nm]][, c("iso_code","date","fold",QC)]
  pp <- P[, c("iso_code","date","fold",QC)]
  r  <- try(score_psi_arm(nm, v, obs, folds, mode = MODE, dir = HERE,
                          verbose = FALSE, pred_pre = pp), silent = TRUE)
  if (inherits(r, "try-error")) { cat(nm, "FAILED:", conditionMessage(attr(r,"condition")), "\n"); next }
  rr <- try(score_psi_arm(nm, v, obs, folds, mode = MODE, dir = HERE, verbose = FALSE,
                          pred_pre = pp, interval_mode = "residual"), silent = TRUE)
  out[[nm]] <- r
  saveRDS(r, file.path(HERE, sprintf("score_%s_%s_seed_psi.rds", nm, MODE)))
  if (!inherits(rr,"try-error"))
    saveRDS(rr, file.path(HERE, sprintf("score_%s_%s_residual_psi.rds", nm, MODE)))
  cat(sprintf("%-8s S %+.4f  exNGA %+.4f  n_beat %2d/16  resid %+.4f  vs_seas %+.4f  A6 %s   h1 %+.3f h2 %+.3f h3 %+.3f\n",
      nm, r$S, r$S_exNGA, r$n_beat,
      if (inherits(rr,"try-error")) NA_real_ else rr$S,
      r$S_by_baseline$seasonal$S,
      if (isTRUE(r$beats_seasonal)) "PASS" else "FAIL",
      r$per_horizon$h1mo$S, r$per_horizon$h2mo$S, r$per_horizon$h3mo$S))
}
cat("\n=== delta vs P001 ===\n")
for (nm in setdiff(names(out), "P001"))
  cat(sprintf("%-8s dS %+.4f\n", nm, out[[nm]]$S - out[["P001"]]$S))
