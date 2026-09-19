# MAE by horizon week: blend vs persistence vs production psi.
# The headline MAE averages weeks 1-13 and persistence is strongest at week 1,
# where the last observation is most informative. The programme's target is the
# 12-WEEK horizon, so the number that matters is the far end of the window.
suppressMessages(library(MOSAIC))
HERE <- "/home/jgiles/psi_evolve"
CACHE <- Sys.getenv("PSI_CACHE", file.path(HERE,"psi_cache_N8"))
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
EPS <- 1e-4
lg <- function(p) log(pmin(1-EPS,pmax(EPS,p))/(1-pmin(1-EPS,pmax(EPS,p)))); ex <- function(x) 1/(1+exp(-x))
rd <- function(i) { f <- file.path(CACHE, sprintf("psi_%s.csv", format(grid$cutoff[i])))
  if (!file.exists(f)) return(NULL); p <- utils::read.csv(f, stringsAsFactors=FALSE)
  p$date <- as.Date(p$date); p[p$iso_code %in% pool, ] }
rows <- list()
for (i in seq_len(nrow(grid))) {
  p <- rd(i); if (is.null(p)) next
  T0 <- grid$cutoff[i]
  blk <- p[p$date >= grid$test_start[i] & p$date <= grid$test_end[i], c("iso_code","date","psi")]
  if (!nrow(blk)) next
  blk$wk <- as.integer(floor(as.numeric(blk$date - (T0+14L))/7)) + 1L
  blk$pers <- NA_real_
  for (iso in unique(blk$iso_code)) {
    oi <- obs[obs$iso_code==iso & obs$date<=T0, ]; if (nrow(oi)<4L) next
    oi <- oi[order(oi$date), ]
    blk$pers[blk$iso_code==iso] <- mean(utils::tail(oi$observed,4), na.rm=TRUE)
  }
  # the winning transform: level-anchor at half strength, then horizon-weighted blend
  for (iso in unique(blk$iso_code)) {
    j <- blk$iso_code==iso; pr <- p$psi[p$iso_code==iso & p$date<=T0 & p$date>T0-182L]
    if (length(pr)>=8 && is.finite(blk$pers[which(j)[1]]))
      blk$psi[j] <- ex(lg(blk$psi[j]) + 0.5*(lg(blk$pers[which(j)[1]]) - stats::median(lg(blk$psi[j]))))
  }
  lam <- c(0.05,0.05,0.15,0.15,0.40,0.40,0.20,0.25,0.30,0.25,0.35,0.50,0.40,0.40)
  blk$blend <- lam[pmin(blk$wk,14)]*blk$psi + (1-lam[pmin(blk$wk,14)])*blk$pers
  m <- merge(blk, obs, by=c("iso_code","date"))
  m <- m[is.finite(m$observed) & is.finite(m$pers), ]
  if (nrow(m)) rows[[length(rows)+1L]] <- m
}
d <- do.call(rbind, rows); d <- d[d$wk>=1 & d$wk<=13, ]
raw <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  p <- rd(i); if (is.null(p)) return(NULL)
  b <- p[p$date>=grid$test_start[i] & p$date<=grid$test_end[i], c("iso_code","date","psi")]
  if (!nrow(b)) return(NULL)
  b$wk <- as.integer(floor(as.numeric(b$date-(grid$cutoff[i]+14L))/7))+1L; b }))
raw <- merge(raw, obs, by=c("iso_code","date")); raw <- raw[raw$wk>=1 & raw$wk<=13, ]
wm <- function(x, iso) { w <- W$w[match(iso, W$iso_code)]; sum(w*x, na.rm=TRUE)/sum(w[is.finite(x)]) }
cat(sprintf("%4s %7s %9s %9s %9s %10s\n","wk","n","BLEND","persist","raw psi","blend win?"))
for (w in 1:13) {
  z <- d[d$wk==w,]; r <- raw[raw$wk==w,]
  a <- wm(abs(z$observed-z$blend), z$iso_code); b <- wm(abs(z$observed-z$pers), z$iso_code)
  c_ <- wm(abs(r$observed-r$psi), r$iso_code)
  cat(sprintf("%4d %7d %9.4f %9.4f %9.4f %10s\n", w, nrow(z), a, b, c_,
      if (a<b) sprintf("+%.1f%%",100*(b-a)/b) else sprintf("%.1f%%",100*(b-a)/b)))
}
for (rng in list(1:4, 5:8, 9:13)) {
  z <- d[d$wk %in% rng,]
  a <- wm(abs(z$observed-z$blend), z$iso_code); b <- wm(abs(z$observed-z$pers), z$iso_code)
  cat(sprintf("\nweeks %2d-%2d : blend %.4f  persistence %.4f  -> %s%.1f%%",
      min(rng), max(rng), a, b, if (a<b) "blend better by " else "blend worse by ",
      abs(100*(b-a)/b)))
}
cat("\n")
