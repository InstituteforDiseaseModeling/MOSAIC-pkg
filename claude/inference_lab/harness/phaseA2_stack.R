#!/usr/bin/env Rscript
# Phase A2: stack factors on top of A1b + weight_deaths = 4.
# Caches the simulated draw set so later screens cost seconds, not minutes.
PKG <- Sys.getenv("PKG"); K <- as.integer(Sys.getenv("K","500"))
NC  <- as.integer(Sys.getenv("NCORES","8")); TCUT <- as.Date(Sys.getenv("T_CUT","2025-09-01"))
CACHE <- Sys.getenv("CACHE", "/tmp/inflab_draws.rds"); OUTD <- Sys.getenv("OUTDIR",".")
suppressMessages(devtools::load_all(PKG, quiet=TRUE)); suppressMessages(library(parallel))
set_root_directory(Sys.getenv("ROOT","~/MOSAIC"))
cfg <- get_location_config(iso="ETH"); pri <- get_location_priors(iso="ETH")
dts <- seq(as.Date(cfg$date_start), by="day", length.out=ncol(cfg$reported_cases))
tr  <- dts <= TCUT; oc <- as.numeric(cfg$reported_cases); od <- as.numeric(cfg$reported_deaths)
sa  <- list(sample_tau_i=FALSE, sample_mobility_gamma=FALSE, sample_mobility_omega=FALSE, sample_kappa=FALSE)

if (file.exists(CACHE)) {
  cc <- readRDS(CACHE); draws <- cc$draws; sk <- cc$sk
  cat(sprintf("[A2] reusing cached draw set: %d draws\n", length(draws)))
} else {
  pq <- arrow::read_parquet(Sys.getenv("REFRUN"), col_select=c("sim","likelihood"))
  pq <- pq[is.finite(pq$likelihood),]
  pool <- pq$sim[pq$likelihood >= quantile(pq$likelihood, 0.80)]
  sims <- unique(round(seq(1, length(pool), length.out=K))); sims <- pool[sims]
  cat(sprintf("[A2] simulating %d draws from the top-20%% pool on %d cores\n", length(sims), NC))
  cl <- makeCluster(NC); clusterExport(cl, c("cfg","pri","sa","PKG"), envir=environment())
  invisible(clusterCall(cl, function(p){suppressMessages(devtools::load_all(p, quiet=TRUE))
                                        MOSAIC::set_root_directory(Sys.getenv("ROOT","~/MOSAIC")); TRUE}, PKG))
  draws <- parLapplyLB(cl, sims, function(sid){
    cs <- tryCatch(sample_parameters(PATHS=get_paths(), priors=pri, config=cfg, seed=sid,
                   sample_args=sa, verbose=FALSE, validate=FALSE), error=function(e) NULL)
    if (is.null(cs)) return(NULL)
    r <- tryCatch(run_simulation(config=cs, seed=sid, quiet=TRUE), error=function(e) NULL)
    if (is.null(r)) return(NULL)
    list(sim=sid, cases=as.numeric(r$results$reported_cases), deaths=as.numeric(r$results$reported_deaths))
  }); stopCluster(cl); draws <- Filter(Negate(is.null), draws)
  sk <- do.call(rbind, lapply(draws, function(d){
    n <- min(length(d$cases), length(oc))
    f <- function(i,p,o) if (length(i)<20) c(NA,NA) else
      c(as.numeric(calc_model_R2(observed=o[i], estimated=p[i], method="corr"))[1], sum(p[i])/max(sum(o[i]),1e-9))
    itr <- which(tr[1:n] & is.finite(oc[1:n])); ioo <- which(!tr[1:n] & is.finite(oc[1:n]))
    itd <- which(tr[1:n] & is.finite(od[1:n])); iod <- which(!tr[1:n] & is.finite(od[1:n]))
    a<-f(itr,d$cases,oc); b<-f(ioo,d$cases,oc); e<-f(itd,d$deaths,od); g<-f(iod,d$deaths,od)
    data.frame(sim=d$sim, r2_tr=a[1], bias_tr=a[2], r2_oo=b[1], bias_oo=b[2],
               r2_d=e[1], bias_d=e[2], r2_d_oo=g[1], bias_d_oo=g[2])}))
  saveRDS(list(draws=draws, sk=sk), CACHE); cat(sprintf("[A2] cached %d draws\n", length(draws)))
}

BASE <- list(nb_k_min_cases=3, nb_k_min_deaths=3, weight_cases=1, weight_deaths=4,
             weight_peak_timing=0, weight_peak_magnitude=0, weight_cumulative_total=0, weight_wis=0)
grid <- list(); addg <- function(nm, mods){ g<-BASE; for(k in names(mods)) g[[k]]<-mods[[k]]; grid[[nm]]<<-g }
addg("BASE_A1b_wd4", list())
addg("A1b_only_wd1", list(weight_deaths=1))
for (v in c(2,8,16)) addg(sprintf("wd_%d", v), list(weight_deaths=v))
for (v in c(10,20,50)) addg(sprintf("+nbkC_%d", v), list(nb_k_min_cases=v))
for (v in c(10,20))    addg(sprintf("+nbkD_%d", v), list(nb_k_min_deaths=v))
for (v in c(0.10,0.25,0.50)) {
  addg(sprintf("+pkTime_%.2f", v), list(weight_peak_timing=v))
  addg(sprintf("+pkMagn_%.2f", v), list(weight_peak_magnitude=v))
  addg(sprintf("+cumul_%.2f",  v), list(weight_cumulative_total=v))
  addg(sprintf("+wis_%.2f",    v), list(weight_wis=v))
}
addg("+nbkC20+wis.25",        list(nb_k_min_cases=20, weight_wis=0.25))
addg("+nbkC20+pkMagn.50",     list(nb_k_min_cases=20, weight_peak_magnitude=0.50))
addg("+pkMagn.50+wis.25",     list(weight_peak_magnitude=0.50, weight_wis=0.25))
addg("+nbkC20+pkMagn.50+wis.25", list(nb_k_min_cases=20, weight_peak_magnitude=0.50, weight_wis=0.25))
addg("+nbkC20+pkMagn.50+cum.25", list(nb_k_min_cases=20, weight_peak_magnitude=0.50, weight_cumulative_total=0.25))

ocm <- matrix(oc, nrow=1); odm <- matrix(od, nrow=1)
sp <- function(x,y){ok<-is.finite(x)&is.finite(y); if(sum(ok)<30) NA else suppressWarnings(cor(x[ok],y[ok],method="spearman"))}
res <- do.call(rbind, lapply(names(grid), function(nm){
  g <- grid[[nm]]
  ll <- vapply(draws, function(d){ n <- min(length(d$cases), ncol(ocm))
    tryCatch(do.call(calc_model_likelihood, c(list(obs_cases=ocm[,1:n,drop=FALSE],
      est_cases=matrix(d$cases[1:n],nrow=1), obs_deaths=odm[,1:n,drop=FALSE],
      est_deaths=matrix(d$deaths[1:n],nrow=1), config=cfg, verbose=FALSE), g)), error=function(e) NA_real_)
  }, numeric(1))
  okr <- is.finite(ll)&is.finite(sk$r2_oo)
  top <- if(sum(okr)>=30){q<-quantile(ll[okr],0.90); sk$r2_oo[okr][ll[okr]>=q]} else NA
  topb<- if(sum(okr)>=30){q<-quantile(ll[okr],0.90); sk$bias_oo[okr][ll[okr]>=q]} else NA
  data.frame(config=nm, sp_r2_oos=sp(ll,sk$r2_oo), sp_r2_tr=sp(ll,sk$r2_tr),
             sp_absbias_oos=sp(ll,abs(sk$bias_oo-1)), sp_absbias_tr=sp(ll,abs(sk$bias_tr-1)),
             r2oos_top10=if(all(is.na(top))) NA else median(top,na.rm=TRUE),
             bias_oos_top10=if(all(is.na(topb))) NA else median(topb,na.rm=TRUE))
}))
res <- res[order(-res$sp_r2_oos),]
cat("\n============ PHASE A2: STACKED ON A1b + weight_deaths=4 ============\n")
cat("primary = sp_r2_oos (want HIGH). r2oos_top10 = OOS R2 of the draws the LL would select.\n\n")
print(res, row.names=FALSE, digits=3)
saveRDS(res, file.path(OUTD,"phaseA2_stack.rds"))
