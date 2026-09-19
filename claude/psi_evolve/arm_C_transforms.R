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

# ---- C10/C11: use PERSISTENCE, the strongest baseline, not climatology ------
# The accuracy diagnostics say psi has weak-but-nonzero SHAPE (R2_corr 0.10-0.20)
# and catastrophically wrong LEVEL (R2_sse -1.25). Persistence is the mirror
# image: it is the last 4 observed weeks held flat, so it has the level right by
# construction and no shape at all -- and it is the best forecast on the board
# (MAE 0.1490 vs production psi 0.2229). Every combination arm so far blended
# psi with CLIMATOLOGY (MAE 0.1975), the weaker of the two baselines. Blending
# with persistence has never been tried.
#   C10  lambda*psi + (1-lambda)*persistence, lambda per country from EARLIER
#        cutoffs' realised out-of-sample MAE (leakage-clean, as in C7b)
#   C11  keep psi's SHAPE, take persistence's LEVEL: shift psi in logit space so
#        its block median matches the persistence level
#   C11h C11 at half strength
# Persistence level uses only observations at or before the cutoff.
B$pers <- NA_real_
for (fl in unique(B$fold)) {
  T0 <- grid$cutoff[match(fl, grid$block)]
  for (iso in unique(B$iso_code[B$fold == fl])) {
    oi <- obs[obs$iso_code == iso & obs$date <= T0, ]
    if (nrow(oi) < 4L) next
    oi <- oi[order(oi$date), ]
    B$pers[B$fold == fl & B$iso_code == iso] <- mean(utils::tail(oi$observed, 4), na.rm = TRUE)
  }
}
# ---- A2: a DAMPED-TREND anchor instead of flat persistence ------------------
# Persistence is a flat line at the last-4-week mean, and it carries 60-95% of
# the blend's weight -- so improving the ANCHOR raises the whole product, not
# just the psi side. A damped trend is the standard upgrade: level + a recent
# slope that decays with lead, so it extrapolates a rising or falling epidemic
# for a few weeks and then flattens. phi is chosen on EARLIER cutoffs only, from
# a fixed grid, so nothing is tuned on the evaluation blocks.
#   anchor(w) = level + slope * sum_{k=1..w} phi^k
# phi = 0 reduces exactly to flat persistence, so the arm can only help if the
# trend carries signal.
PHI_GRID <- c(0, 0.5, 0.8, 0.9)
B$trend <- 0; B$phi <- 0
for (fl in unique(B$fold)) {
  T0 <- grid$cutoff[match(fl, grid$block)]
  for (iso in unique(B$iso_code[B$fold == fl])) {
    oi <- obs[obs$iso_code == iso & obs$date <= T0, ]
    if (nrow(oi) < 8L) next
    oi <- oi[order(oi$date), ]
    tl <- utils::tail(oi$observed, 8)
    # weekly slope from an OLS fit on the last 8 observed weeks
    B$trend[B$fold == fl & B$iso_code == iso] <-
      unname(stats::coef(stats::lm(tl ~ seq_along(tl)))[2])
  }
}
# choose phi per cutoff on earlier cutoffs' realised blocks (pooled across
# countries: one scalar, and per-country would be ~1 block per cell)
for (fl in sort(unique(B$fold))) {
  gi <- match(fl, grid$block); if (gi == 1L) next
  H <- list()
  for (k in seq_len(gi - 1L)) {
    Tk <- grid$cutoff[k]
    z <- obs[obs$date >= Tk + 15L & obs$date <= min(grid$test_end[k], grid$cutoff[gi]), ]
    if (!nrow(z)) next
    z$wk <- as.integer(floor(as.numeric(z$date - (Tk + 14L)) / 7)) + 1L
    z$lev <- NA_real_; z$sl <- NA_real_
    for (iso in unique(z$iso_code)) {
      oi <- obs[obs$iso_code == iso & obs$date <= Tk, ]
      if (nrow(oi) < 8L) next
      oi <- oi[order(oi$date), ]; tl <- utils::tail(oi$observed, 8)
      z$lev[z$iso_code == iso] <- mean(utils::tail(oi$observed, 4), na.rm = TRUE)
      z$sl[z$iso_code == iso] <- unname(stats::coef(stats::lm(tl ~ seq_along(tl)))[2])
    }
    H[[length(H)+1L]] <- z[is.finite(z$lev) & is.finite(z$sl), ]
  }
  if (!length(H)) next
  H <- do.call(rbind, H)
  mae <- vapply(PHI_GRID, function(ph) {
    cum <- if (ph == 0) 0 else vapply(H$wk, function(w) sum(ph^seq_len(w)), numeric(1))
    mean(abs(H$observed - (H$lev + H$sl * cum)), na.rm = TRUE) }, numeric(1))
  B$phi[B$fold == fl] <- PHI_GRID[which.min(mae)]
}
cat("A2 damped-trend phi chosen per cutoff:\n"); print(tapply(B$phi, B$fold, max))
B$anchor <- B$pers + B$trend * ifelse(B$phi == 0, 0,
              vapply(seq_len(nrow(B)), function(i)
                sum(B$phi[i]^seq_len(max(1L, B$wk[i]))), numeric(1)))
B$anchor <- pmin(1 - 1e-4, pmax(1e-4, B$anchor))

# C10 lambda: minimise MAE of the blend on EARLIER cutoffs' realised blocks
# DEFAULT LAMBDA = 0, i.e. PURE PERSISTENCE, not pure psi.
# This initialised to 1 (pure psi), so the first cutoff -- which has no earlier
# blocks to estimate lambda from -- ran as raw psi, the weakest forecast on the
# board. Absent evidence that psi helps, the safe prior is the STRONG baseline.
# Measured cost of the wrong prior: the blend LOST to persistence at weeks 1-8
# (-5.0% and -8.7%) despite winning at 9-13.
B$c10_lam <- 0
for (fl in sort(unique(B$fold))) {
  gi <- match(fl, grid$block); T0 <- grid$cutoff[gi]
  hist <- list()
  for (k in seq_len(gi - 1L)) {
    q <- rd(k); if (is.null(q)) next
    Tk <- grid$cutoff[k]
    z <- q[q$date >= Tk + 15L & q$date <= min(grid$test_end[k], T0), c("iso_code","date","psi")]
    if (!nrow(z)) next
    z$pers <- NA_real_
    for (iso in unique(z$iso_code)) {
      oi <- obs[obs$iso_code == iso & obs$date <= Tk, ]
      if (nrow(oi) < 4L) next
      oi <- oi[order(oi$date), ]
      z$pers[z$iso_code == iso] <- mean(utils::tail(oi$observed, 4), na.rm = TRUE)
    }
    hist[[length(hist)+1L]] <- z
  }
  if (!length(hist)) next
  H <- merge(do.call(rbind, hist), obs, by = c("iso_code","date"))
  H <- H[is.finite(H$observed) & is.finite(H$pers), ]
  lams <- seq(0, 1, by = 0.05)
  for (iso in unique(B$iso_code[B$fold == fl])) {
    h <- H[H$iso_code == iso, ]; if (nrow(h) < 10L) next
    mae <- vapply(lams, function(L) mean(abs(h$observed - (L*h$psi + (1-L)*h$pers))), numeric(1))
    B$c10_lam[B$fold == fl & B$iso_code == iso] <- lams[which.min(mae)]
  }
}
cat("C10 lambda on psi (median over blocks, per country):\n")
print(round(tapply(B$c10_lam, B$iso_code, stats::median), 2))
# C11 shift: match psi's block median to the persistence level, in logit space
B$c11_sh <- 0
for (fl in unique(B$fold)) for (iso in unique(B$iso_code[B$fold == fl])) {
  j <- which(B$fold == fl & B$iso_code == iso)
  if (!length(j) || !is.finite(B$pers[j[1]])) next
  B$c11_sh[j] <- lg(B$pers[j[1]]) - stats::median(lg(B$psi[j]))
}

# ---- C12: HORIZON-DEPENDENT blend weight ------------------------------------
# C10's lambda is per country but CONSTANT across the 12-week window. It should
# not be: persistence is anchored on the last observation, so its edge is
# largest at week 1 and decays with lead, while psi's (weak) climate signal does
# not decay the same way. C7c already showed that a horizon curve POOLED across
# countries is catastrophic (-0.874) because it forces one shape onto countries
# where the other component is useless -- so the form here is MULTIPLICATIVE:
# a per-country level from C10 times a pooled horizon shape, which keeps the
# country-specific part that works and adds only one pooled degree of freedom.
#   C12b  lambda by horizon week only (pooled) -- recorded to show why it fails
#   C12c  lambda_country x horizon_shape       -- the intended form
# Both estimated on EARLIER cutoffs' realised blocks only.
B$c12b_lam <- 0; B$c12c_lam <- 0   # same prior fix
for (fl in sort(unique(B$fold))) {
  gi <- match(fl, grid$block); T0 <- grid$cutoff[gi]
  hist <- list()
  for (k in seq_len(gi - 1L)) {
    q <- rd(k); if (is.null(q)) next
    Tk <- grid$cutoff[k]
    z <- q[q$date >= Tk + 15L & q$date <= min(grid$test_end[k], T0), c("iso_code","date","psi")]
    if (!nrow(z)) next
    z$wk <- as.integer(floor(as.numeric(z$date - (Tk + 14L)) / 7)) + 1L
    z$pers <- NA_real_
    for (iso in unique(z$iso_code)) {
      oi <- obs[obs$iso_code == iso & obs$date <= Tk, ]
      if (nrow(oi) < 4L) next
      oi <- oi[order(oi$date), ]
      z$pers[z$iso_code == iso] <- mean(utils::tail(oi$observed, 4), na.rm = TRUE)
    }
    hist[[length(hist)+1L]] <- z
  }
  if (!length(hist)) next
  H <- merge(do.call(rbind, hist), obs, by = c("iso_code","date"))
  H <- H[is.finite(H$observed) & is.finite(H$pers) & H$iso_code %in% pool, ]
  if (!nrow(H)) next
  lams <- seq(0, 1, by = 0.05)
  best <- function(d) if (nrow(d) < 10L) NA_real_ else
    lams[which.min(vapply(lams, function(L)
      mean(abs(d$observed - (L*d$psi + (1-L)*d$pers))), numeric(1)))]
  # pooled horizon shape, normalised to mean 1 so it only reshapes
  hw <- vapply(sort(unique(B$wk)), function(w) best(H[H$wk == w, ]), numeric(1))
  names(hw) <- sort(unique(B$wk))
  shape <- hw / mean(hw, na.rm = TRUE); shape[!is.finite(shape)] <- 1
  j <- B$fold == fl
  B$c12b_lam[j] <- pmin(1, pmax(0, hw[as.character(B$wk[j])]))
  B$c12c_lam[j] <- pmin(1, pmax(0, B$c10_lam[j] * shape[as.character(B$wk[j])]))
  if (fl == max(unique(B$fold))) {
    cat("C12 pooled optimal lambda-on-psi by horizon week (last cutoff):\n")
    print(round(hw, 2))
  }
}
B$c12b_lam[!is.finite(B$c12b_lam)] <- 0; B$c12c_lam[!is.finite(B$c12c_lam)] <- 0

# ---- C7b: combination weight from OUT-OF-SAMPLE history, not pre-cutoff fit --
# C7 failed for a diagnosed reason: lambda fitted on PRE-CUTOFF error came out
# 1.00 for 14 of 16 countries, because psi is excellent in-sample (0.273 vs
# observed 0.238) and only collapses out of sample. The information a pre-cutoff
# fit cannot have is exactly the out-of-sample collapse -- but EARLIER CUTOFFS
# have already revealed it. At cutoff T_i, choose lambda per country on the
# realised out-of-sample blocks of cutoffs T_1..T_(i-1), truncated to dates <= T_i.
# Leakage-clean, and it is how an operational system would learn its own decay.
B$c7b_lam <- 0   # same prior fix: default to the baseline, not to psi
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
  C10     = combo_all(B, B$c10_lam, B$pers),
  C11     = shift_all(B, B$c11_sh),
  C11h    = shift_all(B, 0.5 * B$c11_sh),
  C10_C11h= combo_all(shift_all(B, 0.5 * B$c11_sh), B$c10_lam, B$pers),
  C12b    = combo_all(B, B$c12b_lam, B$pers),
  C12c    = combo_all(B, B$c12c_lam, B$pers),
  C12c_C11h = combo_all(shift_all(B, 0.5 * B$c11_sh), B$c12c_lam, B$pers),
  A2        = { z <- B; z$psi <- z$anchor; z },
  A2_C12c   = combo_all(shift_all(B, 0.5 * B$c11_sh), B$c12c_lam, B$anchor),
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

# ---- ACCURACY TABLE: MAE / RMSE / R2 / WIS, the units that matter ---------
# The skill-ratio S divides by a per-country baseline and is ~300x noisier
# across two fits of the same model than MAE is (28.41% vs 0.09%). Report the
# direct measures so a real improvement is not buried in ratio noise.
wis_fn <- getFromNamespace(".rcv_wis", "MOSAIC")
SEL_FOLDS <- grid$block[grid$split == "selection"]
acc <- function(v, nm) {
  # SELECTION BLOCKS ONLY. The first version of this table ran over all 9
  # blocks, which includes the 3 LOCKED confirmation blocks -- a PROTOCOL 5.3
  # breach (the holdout is read once, after a selection win). Caught by P001's
  # MAE disagreeing with accuracy_table.R (0.1924 vs 0.2022).
  v <- v[v$fold %in% SEL_FOLDS, , drop = FALSE]
  m <- merge(v[, c("iso_code","date",QC)], obs, by = c("iso_code","date"))
  m <- m[is.finite(m$observed) & is.finite(m$psi), ]
  if (!nrow(m)) return(NULL)
  per <- do.call(rbind, lapply(split(m, m$iso_code), function(z) {
    if (nrow(z) < 4) return(NULL)
    data.frame(iso_code = z$iso_code[1],
               mae = mean(abs(z$observed - z$psi)),
               sse = sum((z$observed - z$psi)^2),
               sst = sum((z$observed - mean(z$observed))^2),
               r2  = suppressWarnings(stats::cor(z$observed, z$psi))^2,
               wis = mean(wis_fn(z$observed, z$psi, z$q25, z$q75, z$q025, z$q975), na.rm = TRUE),
               stringsAsFactors = FALSE) }))
  wt <- W$w_sqrt[match(per$iso_code, W$iso_code)]; wt <- wt/sum(wt)
  data.frame(arm = nm, MAE = sum(wt*per$mae), R2_corr = sum(wt*per$r2, na.rm=TRUE),
             R2_sse = 1 - sum(per$sse)/sum(per$sst), WIS = sum(wt*per$wis, na.rm=TRUE),
             stringsAsFactors = FALSE)
}
W <- utils::read.csv(file.path(HERE,"weights_frozen.csv"), stringsAsFactors=FALSE)

# Fold the REFIT arms into the SAME table on the SAME cells. Previously the
# transforms table and accuracy_table.R used slightly different cell filters, so
# P001's MAE differed between them (0.2229 vs 0.2022) and the two sets of arms
# were not comparable. One table, one cell set.
for (a in c("P000","P000R","N8","N5","D9b","N6")) {
  dr <- file.path(HERE, paste0("psi_cache_", a)); if (!dir.exists(dr)) next
  rr <- list()
  for (i in seq_len(nrow(grid))) {
    f <- file.path(dr, sprintf("psi_%s.csv", format(grid$cutoff[i]))); if (!file.exists(f)) next
    q <- utils::read.csv(f, stringsAsFactors = FALSE); q$date <- as.Date(q$date)
    q <- q[q$iso_code %in% pool & q$date >= grid$test_start[i] & q$date <= grid$test_end[i], ]
    if (!nrow(q)) next
    q$fold <- grid$block[i]; rr[[length(rr)+1L]] <- q[, c("iso_code","date","fold",QC)]
  }
  if (length(rr)) variants[[a]] <- do.call(rbind, rr)
}
accs <- do.call(rbind, lapply(names(variants), function(nm) acc(variants[[nm]], nm)))
# baselines on the same cells
bl_fn <- getFromNamespace(".rcv_baseline", "MOSAIC")
bl_rows <- list()
for (bn in c("persistence","seasonal")) {
  pr <- list()
  for (fl in intersect(unique(B$fold), SEL_FOLDS)) {
    T0 <- grid$cutoff[match(fl, grid$block)]
    bk <- B[B$fold == fl, c("iso_code","date")]
    for (iso in unique(bk$iso_code)) {
      z <- bk[bk$iso_code==iso, ]
      is_df <- obs[obs$iso_code==iso & obs$date <= T0 & is.finite(obs$observed), ]
      if (nrow(is_df) < 8) next
      b <- bl_fn(is_df, z$date, bn); if (!any(is.finite(b$point))) next
      o <- merge(z, obs, by=c("iso_code","date"))
      if (nrow(o) < 4) next
      pr[[length(pr)+1L]] <- data.frame(iso_code=iso, mae=mean(abs(o$observed - b$point[seq_len(nrow(o))]), na.rm=TRUE))
    }
  }
  if (length(pr)) { q <- do.call(rbind, pr)
    per <- stats::aggregate(mae ~ iso_code, q, mean)
    wt <- W$w_sqrt[match(per$iso_code, W$iso_code)]; wt <- wt/sum(wt)
    bl_rows[[bn]] <- data.frame(arm=paste0("[",bn,"]"), MAE=sum(wt*per$mae),
                                R2_corr=NA_real_, R2_sse=NA_real_, WIS=NA_real_) }
}
accs <- rbind(accs, do.call(rbind, bl_rows))
cat("
=== ACCURACY (burden-weighted, 12-week OOS blocks) ===
")
accs$vs_P001_MAE <- round(100*(accs$MAE - accs$MAE[accs$arm=="P001"])/accs$MAE[accs$arm=="P001"], 1)
print(accs[order(accs$MAE), ], row.names = FALSE, digits = 4)
cat("(MAE lower = better; vs_P001_MAE is % change, negative = improvement)

")

# ---- PER-HORIZON MAE for the leading variant vs persistence ---------------
# The headline MAE averages weeks 1-13, and persistence is near-unbeatable at
# week 1 where the last observation IS the answer. The programme's target is the
# 12-WEEK horizon, so the number that matters is the far end. Computed from the
# SAME leakage-clean variants above (lambda estimated per cutoff from earlier
# cutoffs only) -- a hand-held version of this analysis with one hardcoded
# lambda curve applied to every block would leak the curve into the early ones.
ph <- function(v, nm) {
  m <- merge(v[v$fold %in% SEL_FOLDS, c("iso_code","date","fold","psi")],
             obs, by = c("iso_code","date"))
  m$wk <- NA_integer_
  for (fl in unique(m$fold)) {
    T0 <- grid$cutoff[match(fl, grid$block)]
    j <- m$fold == fl
    m$wk[j] <- as.integer(floor(as.numeric(m$date[j] - (T0 + 14L)) / 7)) + 1L
  }
  m <- m[is.finite(m$observed) & m$wk >= 1 & m$wk <= 13, ]
  m$w <- W$w_sqrt[match(m$iso_code, W$iso_code)]
  do.call(rbind, lapply(list(1:4, 5:8, 9:13), function(rg) {
    z <- m[m$wk %in% rg, ]
    data.frame(arm = nm, weeks = sprintf("%d-%d", min(rg), max(rg)),
               MAE = sum(z$w*abs(z$observed - z$psi))/sum(z$w), n = nrow(z),
               stringsAsFactors = FALSE) }))
}
pers_v <- B; pers_v$psi <- B$pers
lead <- if ("C12c_C11h" %in% names(variants)) "C12c_C11h" else "C10_C11h"
phz <- rbind(ph(variants[[lead]], lead), ph(pers_v, "persistence"),
             ph(variants[["P001"]], "raw_psi"))
cat("
=== MAE BY HORIZON (leakage-clean lambda, selection blocks) ===
")
w1 <- reshape(phz[, c("arm","weeks","MAE")], idvar="weeks", timevar="arm", direction="wide")
names(w1) <- sub("^MAE[.]", "", names(w1))
w1$blend_vs_pers <- sprintf("%+.1f%%", 100*(w1$persistence - w1[[lead]])/w1$persistence)
print(w1, row.names = FALSE, digits = 4)
cat("(positive blend_vs_pers = the blend BEATS persistence at that horizon)

")

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
