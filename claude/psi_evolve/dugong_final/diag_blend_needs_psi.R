# THE MISSING CONTROL: does the C12c_C11h blend need psi at all?
#
# The blend is lambda*psi + (1-lambda)*persistence with a half-strength level
# anchor. Phase 1 established that it takes its LEVEL from persistence, that
# lambda = 0 in 40% of cells (so the blend IS persistence there), and that it
# wins MAE partly by flattening (sd_ratio 1.11 -> 0.43). If psi carries real
# information, substituting a NULL for psi must degrade the blend. If it does
# not, the blend's headline (+5.7% at weeks 9-13) is a property of persistence
# and the lambda machinery, not of the suitability model.
#
# NC1/NC2/NC3 exist in the registry but were run on RAW psi, never on the BLEND.
#
# Blend reconstruction is the same code path as confirm_read.R, which was
# validated to reproduce -3.1% / -0.5% / +5.7% on the selection blocks.
suppressMessages(library(MOSAIC))
HERE  <- "/home/jgiles/psi_evolve"
CANON <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
VAR   <- "target_D_rate_per_country_floored"
# usage: Rscript diag_blend_needs_psi.R [ARM]     (or PSI_CACHE=<dir>)
#
# PROMOTED TO A GATE (2026-09-23). This started as a one-off diagnostic for N8
# and is now run on EVERY arm, because the thing it measures is the thing the
# programme kept failing to check: whether psi's TIME VARIATION contributes
# anything, as opposed to its per-country LEVEL. On N8 it did not -- replacing
# psi with its own per-country-block mean matched the real thing at weeks 9-13
# (0.1653 vs 0.1658). An arm that cannot beat its own constant has not earned a
# promotion no matter what it does to MAE.
.arg  <- commandArgs(TRUE)
ARM   <- if (length(.arg) && nzchar(.arg[1])) .arg[1] else NA_character_
CACHE <- if (!is.na(ARM)) file.path(HERE, paste0("psi_cache_", ARM)) else
              Sys.getenv("PSI_CACHE", file.path(HERE, "psi_cache_N8"))
if (is.na(ARM)) ARM <- sub("^psi_cache_", "", basename(CACHE))
if (!dir.exists(CACHE)) stop("no such cache: ", CACHE)
set.seed(11)

grid <- utils::read.csv(file.path(HERE,"EVAL_GRID.csv"), stringsAsFactors=FALSE)
gsel <- grid[grid$grid=="prod" & grid$split=="selection", ]
for (k in c("cutoff","test_start","test_end")) gsel[[k]] <- as.Date(gsel[[k]])
W <- utils::read.csv(file.path(HERE,"weights_frozen.csv"), stringsAsFactors=FALSE)
W$w <- W$w_sqrt/sum(W$w_sqrt); pool <- W$iso_code
obs <- utils::read.csv(CANON, stringsAsFactors=FALSE)[,c("iso_code","date",VAR)]
names(obs)[3] <- "observed"; obs$date <- as.Date(obs$date)
obs <- obs[is.finite(obs$observed) & obs$iso_code %in% pool, ]
EPS <- 1e-4
lg <- function(p) log(pmin(1-EPS,pmax(EPS,p))/(1-pmin(1-EPS,pmax(EPS,p))))
ex <- function(x) 1/(1+exp(-x))

rd <- function(ct) {
  f <- file.path(CACHE, sprintf("psi_%s.csv", format(ct))); if (!file.exists(f)) return(NULL)
  p <- utils::read.csv(f, stringsAsFactors=FALSE); p$date <- as.Date(p$date)
  p[p$iso_code %in% pool, c("iso_code","date","psi")]
}
# NULL substitutions, applied per country-block INSIDE the scored window so the
# lambda machinery and the anchor are untouched
subst <- function(b, mode) {
  if (mode == "real") return(b)
  for (iso in unique(b$iso_code)) {
    j <- b$iso_code == iso; v <- b$psi[j]
    b$psi[j] <- switch(mode,
      shuffle  = sample(v),                      # same values, wrong times
      constant = rep(mean(v), length(v)),        # per-country-block mean
      noise    = stats::runif(length(v)),        # U(0,1)
      flat_mid = rep(0.5, length(v)))            # a fixed constant
  }
  b
}

build <- function(mode) {
  rows <- list()
  for (i in seq_len(nrow(gsel))) {
    T0 <- gsel$cutoff[i]; p <- rd(T0); if (is.null(p)) next
    b <- p[p$date >= gsel$test_start[i] & p$date <= gsel$test_end[i], ]
    if (!nrow(b)) next
    b <- subst(b, mode)
    b$wk <- as.integer(floor(as.numeric(b$date-(T0+14L))/7))+1L
    b$pers <- NA_real_
    for (iso in unique(b$iso_code)) {
      oi <- obs[obs$iso_code==iso & obs$date<=T0, ]; if (nrow(oi)<4L) next
      oi <- oi[order(oi$date), ]
      b$pers[b$iso_code==iso] <- mean(utils::tail(oi$observed,4), na.rm=TRUE)
      pr <- p$psi[p$iso_code==iso & p$date<=T0 & p$date>T0-182L]
      j <- b$iso_code==iso
      if (length(pr)>=8 && is.finite(b$pers[which(j)[1]]))
        b$psi[j] <- ex(lg(b$psi[j]) + 0.5*(lg(b$pers[which(j)[1]]) - stats::median(lg(b$psi[j]))))
    }
    # C12c lambda from EARLIER blocks only (leakage-clean), default 0
    hist <- list()
    for (k in which(gsel$cutoff < T0)) {
      Tk <- gsel$cutoff[k]; q <- rd(Tk); if (is.null(q)) next
      z <- q[q$date >= Tk+15L & q$date <= min(gsel$test_end[k], T0), ]
      if (!nrow(z)) next
      z <- subst(z, mode)
      z$wk <- as.integer(floor(as.numeric(z$date-(Tk+14L))/7))+1L
      z$pers <- NA_real_
      for (iso in unique(z$iso_code)) {
        oi <- obs[obs$iso_code==iso & obs$date<=Tk, ]; if (nrow(oi)<4L) next
        oi <- oi[order(oi$date), ]
        z$pers[z$iso_code==iso] <- mean(utils::tail(oi$observed,4), na.rm=TRUE)
      }
      hist[[length(hist)+1L]] <- z
    }
    b$lam <- 0
    if (length(hist)) {
      H <- merge(do.call(rbind, hist), obs, by=c("iso_code","date"))
      H <- H[is.finite(H$observed) & is.finite(H$pers), ]
      lams <- seq(0,1,by=0.05)
      bestf <- function(d) if (nrow(d)<10L) NA_real_ else
        lams[which.min(vapply(lams, function(L) mean(abs(d$observed-(L*d$psi+(1-L)*d$pers))), numeric(1)))]
      per_c <- vapply(unique(b$iso_code), function(iso) {
        v <- bestf(H[H$iso_code==iso,]); if (is.finite(v)) v else 0 }, numeric(1))
      hw <- vapply(sort(unique(b$wk)), function(w) bestf(H[H$wk==w,]), numeric(1))
      shape <- hw/mean(hw, na.rm=TRUE); shape[!is.finite(shape)] <- 1
      names(shape) <- sort(unique(b$wk))
      b$lam <- pmin(1, pmax(0, per_c[b$iso_code] * shape[as.character(b$wk)]))
      b$lam[!is.finite(b$lam)] <- 0
    }
    b$blend <- b$lam*b$psi + (1-b$lam)*b$pers
    m <- merge(b, obs, by=c("iso_code","date"))
    rows[[length(rows)+1L]] <- m[is.finite(m$observed) & is.finite(m$pers), ]
  }
  d <- do.call(rbind, rows); d[d$wk>=1 & d$wk<=13, ]
}
wm <- function(x, iso) { w <- W$w[match(iso, W$iso_code)]
                         sum(w*x, na.rm=TRUE)/sum(w[is.finite(x)]) }
cat(sprintf("cache: %s\n\n", basename(CACHE)))
cat(sprintf("%-10s %9s %9s %9s %9s %8s\n","psi is","overall","wk1-4","wk5-8","wk9-13","mean lam"))
cat(strrep("-",62),"\n")
far <- c(real=NA_real_, shuffle=NA_real_, constant=NA_real_,
         noise=NA_real_, flat_mid=NA_real_)
for (mode in c("real","shuffle","constant","noise","flat_mid")) {
  d <- build(mode)
  o <- wm(abs(d$observed-d$blend), d$iso_code)
  bands <- vapply(list(1:4,5:8,9:13), function(rg) {
    z <- d[d$wk %in% rg,]; wm(abs(z$observed-z$blend), z$iso_code) }, numeric(1))
  far[mode] <- bands[3]                      # weeks 9-13, the horizon we forecast
  cat(sprintf("%-10s %9.4f %9.4f %9.4f %9.4f %8.3f\n", mode, o, bands[1], bands[2], bands[3],
              mean(d$lam, na.rm=TRUE)))
}
dp <- build("real")
cat(sprintf("\npersistence alone: overall %.4f | wk9-13 %.4f\n",
    wm(abs(dp$observed-dp$pers), dp$iso_code),
    { z <- dp[dp$wk %in% 9:13,]; wm(abs(z$observed-z$pers), z$iso_code) }))
cat("\nIf the NULL rows match 'real', the blend does not need psi and the\n")
cat("headline is a property of persistence + the lambda machinery.\n")

# ---- THE GATE ---------------------------------------------------------------
# Decisive statistic: how much the REAL psi beats its own per-country-block
# MEAN at weeks 9-13. The mean preserves psi's level and destroys its timing,
# so the difference is what the time variation is worth.
#
# THRESHOLD, MEASURED NOT ASSUMED (2026-09-23). This gate first shipped with a
# 0.5% threshold borrowed from PLAN_PHASE2's MAE rule. Running it over all ten
# existing arms showed that was far too tight: P000 scored +2.39% and P000R --
# its own REPLICATE, identical code, disjoint seed block -- scored -0.19%. The
# pair disagrees by 2.57pp and the verdict flips, so 2.57pp is the measured
# replicate floor OF THIS STATISTIC. It is ~25x the 0.1% replicate floor on
# MAE, because the gain is a small difference between two noisy quantities.
# The between-arm spread over ten arms was only 3.6pp (-1.23% to +2.39%), so
# the replicate pair alone accounts for most of the observed range: at this
# budget NO arm's timing contribution is resolvable. A threshold below the
# replicate floor manufactures PASSes, which is what the 0.5% version did for
# D9b (+0.51%), N6 (+0.71%), P000H (+0.75%) and F4 (+0.84%).
#
# To resolve a real effect, raise the budget (more seeds, or average the gain
# over several disjoint seed blocks) -- do not lower this number.
GATE_FLOOR <- 0.0257
gain <- (far[["constant"]] - far[["real"]]) / far[["constant"]]
verdict <- if (!is.finite(gain)) "INDETERMINATE" else
           if (gain >  GATE_FLOOR) "PASS" else
           if (gain < -GATE_FLOOR) "FAIL (constant is BETTER)" else
                "FAIL (within replicate noise)"
cat(sprintf("\n---- TIMING GATE ----\narm %s: real %.4f vs its own constant %.4f at wk9-13\n",
            ARM, far[["real"]], far[["constant"]]))
cat(sprintf("psi's TIME VARIATION is worth %+.2f%% (replicate floor %.2f%%)  ->  %s\n",
            100*gain, 100*GATE_FLOOR, verdict))
out <- file.path(HERE, "null_gate_results.tsv")
if (!file.exists(out))
     cat("arm\treal_wk913\tconstant_wk913\tshuffle_wk913\tnoise_wk913\tflat_mid_wk913\ttiming_gain\tverdict\ttimestamp\n",
         file = out)
cat(sprintf("%s\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%s\t%s\n", ARM,
            far[["real"]], far[["constant"]], far[["shuffle"]], far[["noise"]],
            far[["flat_mid"]], gain, verdict, format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
    file = out, append = TRUE)
cat(sprintf("appended to %s\n", out))
