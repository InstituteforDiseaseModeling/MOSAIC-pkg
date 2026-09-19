# =============================================================================
# confirm_read.R -- THE ONE PERMITTED READ OF THE LOCKED CONFIRMATION BLOCKS.
#
# PROTOCOL 5.3: the confirmation holdout is read ONCE, after a selection win,
# and the read is recorded. This script exists so that read is done deliberately
# rather than improvised -- I already breached 5.3 once by writing an accuracy
# table that swept the sealed blocks in without noticing, and the numbers were
# materially different (it changed which arm ranked best).
#
# GUARDS:
#   * refuses to run unless CONFIRM_I_MEAN_IT=yes is set
#   * refuses if a previous read is already recorded in confirm_read_log.txt
#   * scores ONLY the 3 confirmation blocks, and says so in every line it prints
#   * appends an indelible record of what was read, when, and by which arm
#
# It answers exactly one question: does the selection-set result -- the blend
# beating persistence at weeks 9-13 by 5.7% -- hold on blocks never used to
# choose anything?
#
# usage: CONFIRM_I_MEAN_IT=yes PSI_CACHE=<psi cache> Rscript confirm_read.R
# =============================================================================
suppressMessages(library(MOSAIC))
HERE <- "/home/jgiles/psi_evolve"
LOG  <- file.path(HERE, "confirm_read_log.txt")
if (!identical(Sys.getenv("CONFIRM_I_MEAN_IT"), "yes"))
     stop("confirm_read: refusing. Set CONFIRM_I_MEAN_IT=yes -- this consumes the\n",
          "  one permitted read of the LOCKED confirmation blocks (PROTOCOL 5.3).")
if (file.exists(LOG))
     stop("confirm_read: a previous read is already recorded in ", LOG, ".\n",
          "  The holdout is write-once. Reading it again is a protocol breach and\n",
          "  makes every confirmation number in this programme uninterpretable.")

CACHE <- Sys.getenv("PSI_CACHE", file.path(HERE, "psi_cache_N8"))
CANON <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
VAR <- "target_D_rate_per_country_floored"
grid <- utils::read.csv(file.path(HERE, "EVAL_GRID.csv"), stringsAsFactors = FALSE)
grid <- grid[grid$grid == "prod" & grid$split == "confirmation", ]   # <-- LOCKED ONLY
for (k in c("cutoff","test_start","test_end")) grid[[k]] <- as.Date(grid[[k]])
cat("CONFIRMATION BLOCKS ONLY:", paste(format(grid$cutoff), collapse = ", "), "\n")
cat("psi cache:", CACHE, "\n\n")

W <- utils::read.csv(file.path(HERE,"weights_frozen.csv"), stringsAsFactors=FALSE)
W$w <- W$w_sqrt/sum(W$w_sqrt); pool <- W$iso_code
obs <- utils::read.csv(CANON, stringsAsFactors=FALSE)[,c("iso_code","date",VAR)]
names(obs)[3] <- "observed"; obs$date <- as.Date(obs$date)
obs <- obs[is.finite(obs$observed) & obs$iso_code %in% pool, ]
EPS <- 1e-4
lg <- function(p) log(pmin(1-EPS,pmax(EPS,p))/(1-pmin(1-EPS,pmax(EPS,p)))); ex <- function(x) 1/(1+exp(-x))

# lambda and the level anchor are estimated from data at or before each
# confirmation cutoff -- which includes the SELECTION blocks, legitimately, since
# those are in the past relative to a confirmation origin.
rows <- list()
for (i in seq_len(nrow(grid))) {
  f <- file.path(CACHE, sprintf("psi_%s.csv", format(grid$cutoff[i])))
  if (!file.exists(f)) { cat("MISSING:", basename(f), "\n"); next }
  p <- utils::read.csv(f, stringsAsFactors=FALSE); p$date <- as.Date(p$date)
  p <- p[p$iso_code %in% pool, ]
  T0 <- grid$cutoff[i]
  b <- p[p$date >= grid$test_start[i] & p$date <= grid$test_end[i], c("iso_code","date","psi")]
  if (!nrow(b)) next
  b$wk <- as.integer(floor(as.numeric(b$date - (T0+14L))/7)) + 1L
  b$pers <- NA_real_
  for (iso in unique(b$iso_code)) {
    oi <- obs[obs$iso_code==iso & obs$date<=T0, ]; if (nrow(oi)<4L) next
    oi <- oi[order(oi$date), ]
    b$pers[b$iso_code==iso] <- mean(utils::tail(oi$observed,4), na.rm=TRUE)
    # C11h: half-strength level anchor, model output + pre-cutoff observed only
    pr <- p$psi[p$iso_code==iso & p$date<=T0 & p$date>T0-182L]
    j <- b$iso_code==iso
    if (length(pr)>=8 && is.finite(b$pers[which(j)[1]]))
      b$psi[j] <- ex(lg(b$psi[j]) + 0.5*(lg(b$pers[which(j)[1]]) - stats::median(lg(b$psi[j]))))
  }
  # C12c lambda: per-country level x pooled horizon shape, from ALL earlier blocks
  hist <- list()
  for (k in which(as.Date(utils::read.csv(file.path(HERE,"EVAL_GRID.csv"),
        stringsAsFactors=FALSE)$cutoff) < T0)) {
    g2 <- utils::read.csv(file.path(HERE,"EVAL_GRID.csv"), stringsAsFactors=FALSE)
    Tk <- as.Date(g2$cutoff[k]); fk <- file.path(CACHE, sprintf("psi_%s.csv", format(Tk)))
    if (!file.exists(fk)) next
    q <- utils::read.csv(fk, stringsAsFactors=FALSE); q$date <- as.Date(q$date)
    z <- q[q$iso_code %in% pool & q$date >= Tk+15L &
           q$date <= min(as.Date(g2$test_end[k]), T0), c("iso_code","date","psi")]
    if (!nrow(z)) next
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
  m <- m[is.finite(m$observed) & is.finite(m$pers), ]
  m$block <- format(T0)
  if (nrow(m)) rows[[length(rows)+1L]] <- m
}
d <- do.call(rbind, rows); d <- d[d$wk>=1 & d$wk<=13, ]
raw <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  f <- file.path(CACHE, sprintf("psi_%s.csv", format(grid$cutoff[i]))); if (!file.exists(f)) return(NULL)
  q <- utils::read.csv(f, stringsAsFactors=FALSE); q$date <- as.Date(q$date)
  z <- q[q$iso_code %in% pool & q$date>=grid$test_start[i] & q$date<=grid$test_end[i], c("iso_code","date","psi")]
  if (!nrow(z)) return(NULL)
  z$wk <- as.integer(floor(as.numeric(z$date-(grid$cutoff[i]+14L))/7))+1L
  z$block <- format(grid$cutoff[i]); z }))
raw <- merge(raw, obs, by=c("iso_code","date")); raw <- raw[raw$wk>=1 & raw$wk<=13, ]
wm <- function(x, iso) { w <- W$w[match(iso, W$iso_code)]; sum(w*x, na.rm=TRUE)/sum(w[is.finite(x)]) }
cat("=== CONFIRMATION (LOCKED) BLOCKS -- MAE by horizon band ===\n")
out <- c()
for (rg in list(1:4, 5:8, 9:13)) {
  z <- d[d$wk %in% rg,]; r <- raw[raw$wk %in% rg,]
  a <- wm(abs(z$observed-z$blend), z$iso_code)
  bq <- wm(abs(z$observed-z$pers), z$iso_code)
  cq <- wm(abs(r$observed-r$psi), r$iso_code)
  line <- sprintf("weeks %2d-%2d : blend %.4f  persistence %.4f  raw psi %.4f  -> blend vs pers %+.1f%%  (n=%d cells)",
                  min(rg), max(rg), a, bq, cq, 100*(bq-a)/bq, nrow(z))
  cat(line, "\n"); out <- c(out, line)
}

# --- pre-registered secondaries (PREREGISTRATION_CONFIRM.md) --------------
# Secondary 1: per-block, weeks 9-13. One block carrying the whole effect is
# a materially weaker result than three agreeing.
cat("\n=== per-block, weeks 9-13 (pre-registered secondary 1) ===\n")
z13 <- d[d$wk %in% 9:13, ]
for (blk in sort(unique(z13$block))) {
  zb <- z13[z13$block == blk, ]
  ab  <- wm(abs(zb$observed-zb$blend), zb$iso_code)
  pb  <- wm(abs(zb$observed-zb$pers),  zb$iso_code)
  line <- sprintf("  %s : blend %.4f  persistence %.4f  -> %+.1f%%  (n=%d cells, %d isos)",
                  blk, ab, pb, 100*(pb-ab)/pb, nrow(zb), length(unique(zb$iso_code)))
  cat(line, "\n"); out <- c(out, line)
}

# Secondary 2: exact paired sign test over country x block units, weeks 9-13.
# The honest unit -- cells within a country-block are strongly autocorrelated.
u <- unique(z13[, c("iso_code","block")])
dif <- vapply(seq_len(nrow(u)), function(i) {
  zz <- z13[z13$iso_code==u$iso_code[i] & z13$block==u$block[i], ]
  mean(abs(zz$observed-zz$pers)) - mean(abs(zz$observed-zz$blend))
}, numeric(1))
dif <- dif[is.finite(dif)]
nw <- sum(dif > 0); nl <- sum(dif < 0)
pv <- stats::binom.test(nw, nw+nl, 0.5)$p.value
line <- sprintf("\n=== paired sign test, weeks 9-13 (secondary 2) ===\n  blend better in %d of %d country-block units (ties %d), exact two-sided p = %.4f\n  median paired MAE gain %+.4f",
                nw, nw+nl, sum(dif==0), pv, stats::median(dif))
cat(line, "\n"); out <- c(out, line)
writeLines(c(sprintf("confirmation read performed %s", Sys.time()),
             sprintf("psi cache: %s", CACHE),
             sprintf("blocks: %s", paste(format(grid$cutoff), collapse=", ")), out), LOG)
cat("\nRecorded in", LOG, "-- the holdout is now spent.\n")
