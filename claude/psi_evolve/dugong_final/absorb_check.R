# Can calc_psi_star(a,b,z,k) absorb the ND-vs-LSTM difference?
suppressMessages(library(MOSAIC))
HERE  <- "/home/jgiles/psi_evolve"
CANON <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
VAR   <- "target_D_rate_per_country_floored"
grid <- read.csv(file.path(HERE,"EVAL_GRID.csv"), stringsAsFactors=FALSE)
grid <- grid[grid$grid=="prod" & grid$split=="selection",]
for (k in c("cutoff","test_start","test_end")) grid[[k]] <- as.Date(grid[[k]])
W <- read.csv(file.path(HERE,"weights_frozen.csv")); pool <- W$iso_code
wt <- setNames(W$w_sqrt/sum(W$w_sqrt), W$iso_code)
obs <- read.csv(CANON)[,c("iso_code","date",VAR)]; names(obs)[3]<-"y"
obs$date <- as.Date(obs$date); obs <- obs[is.finite(obs$y) & obs$iso_code %in% pool,]

get_psi <- function(arm, ct) {
  p <- read.csv(file.path(HERE, paste0("psi_cache_",arm), sprintf("psi_%s.csv", ct)))
  p$date <- as.Date(p$date); p[p$iso_code %in% pool, c("iso_code","date","psi")]
}
T_apply <- function(psi, th) calc_psi_star(psi, a=exp(th[1]), b=th[2],
                                           z=plogis(th[3]), k=th[4], warn_k_rounding=FALSE)
fit_one <- function(psid, dates_all, ytab) {
  # psid: daily psi vector aligned to dates_all; ytab: data.frame(date,y) truth subset
  idx <- match(ytab$date, dates_all)
  ok <- !is.na(idx); idx <- idx[ok]; y <- ytab$y[ok]
  obj <- function(th) { s <- T_apply(psid, th); mean(abs(s[idx]-y)) }
  best <- list(v=Inf,th=NULL)
  starts <- list(c(0,0,10,0), c(0,-1,10,0), c(0,1,10,0), c(log(2),0,10,0),
                 c(log(.5),0,10,0), c(0,0,0,0), c(0,0,10,20), c(0,0,10,-20))
  for (s0 in starts) {
    r <- try(optim(s0, obj, method="Nelder-Mead",
                   control=list(maxit=800, reltol=1e-9)), silent=TRUE)
    if (!inherits(r,"try-error") && r$value < best$v) best <- list(v=r$value, th=r$par)
  }
  list(mae_raw = obj(c(0,0,10,0)), mae_opt = best$v, th = best$th)
}

arms <- c("P000E","ND")
res <- list()
for (i in seq_len(nrow(grid))) {
  ct <- format(grid$cutoff[i])
  P <- lapply(arms, get_psi, ct=ct); names(P) <- arms
  for (iso in pool) {
    yt <- obs[obs$iso_code==iso & obs$date>=grid$test_start[i] & obs$date<=grid$test_end[i],]
    if (nrow(yt) < 6) next
    row <- list(iso=iso, block=ct, n=nrow(yt))
    for (a in arms) {
      pa <- P[[a]][P[[a]]$iso_code==iso,]; pa <- pa[order(pa$date),]
      f <- fit_one(pa$psi, pa$date, yt)
      row[[paste0("raw_",a)]] <- f$mae_raw; row[[paste0("opt_",a)]] <- f$mae_opt
      row[[paste0("th_",a)]] <- paste(round(c(exp(f$th[1]),f$th[2],plogis(f$th[3]),f$th[4]),3), collapse="/")
    }
    res[[length(res)+1]] <- as.data.frame(row, stringsAsFactors=FALSE)
  }
  cat("block", ct, "done\n")
}
R <- do.call(rbind, res)
R$w <- wt[R$iso]
write.csv(R, file.path(HERE,"absorb_result.csv"), row.names=FALSE)
wm <- function(x,w) sum(x*w)/sum(w)
cat("\n=== weighted-mean MAE over", nrow(R), "country-blocks ===\n")
cat(sprintf("RAW  : LSTM %.4f  ND %.4f   gap %.4f (%.1f%%)\n",
   wm(R$raw_P000E,R$w), wm(R$raw_ND,R$w), wm(R$raw_P000E,R$w)-wm(R$raw_ND,R$w),
   100*(1-wm(R$raw_ND,R$w)/wm(R$raw_P000E,R$w))))
cat(sprintf("OPT  : LSTM %.4f  ND %.4f   gap %.4f (%.1f%%)\n",
   wm(R$opt_P000E,R$w), wm(R$opt_ND,R$w), wm(R$opt_P000E,R$w)-wm(R$opt_ND,R$w),
   100*(1-wm(R$opt_ND,R$w)/wm(R$opt_P000E,R$w))))
cat(sprintf("\nabsorbed fraction of the gap: %.1f%%\n",
   100*(1 - (wm(R$opt_P000E,R$w)-wm(R$opt_ND,R$w))/(wm(R$raw_P000E,R$w)-wm(R$raw_ND,R$w)))))
d_raw <- R$raw_P000E-R$raw_ND; d_opt <- R$opt_P000E-R$opt_ND
cat(sprintf("\nunweighted: sign test ND better RAW %d/%d  OPT %d/%d\n",
   sum(d_raw>0), length(d_raw), sum(d_opt>0), length(d_opt)))
cat(sprintf("paired t on d_opt: mean %.5f sd %.5f  p=%.3f\n",
   mean(d_opt), sd(d_opt), t.test(d_opt)$p.value))
cat(sprintf("paired t on d_raw: mean %.5f sd %.5f  p=%.3f\n",
   mean(d_raw), sd(d_raw), t.test(d_raw)$p.value))
