#!/usr/bin/env Rscript
# Phase A: discrimination screen. Simulate K draws ONCE, then score every
# likelihood configuration against those same stored predictions.
PKG <- Sys.getenv("PKG"); K <- as.integer(Sys.getenv("K","600"))
NC  <- as.integer(Sys.getenv("NCORES","8")); TCUT <- as.Date(Sys.getenv("T_CUT","2025-09-01"))
suppressMessages(devtools::load_all(PKG, quiet=TRUE)); suppressMessages(library(parallel))
set_root_directory(Sys.getenv("ROOT", "~/MOSAIC"))
cfg <- get_location_config(iso="ETH"); pri <- get_location_priors(iso="ETH")
dts <- seq(as.Date(cfg$date_start), by="day", length.out=ncol(cfg$reported_cases))
tr  <- dts <= TCUT
oc  <- as.numeric(cfg$reported_cases); od <- as.numeric(cfg$reported_deaths)
sa  <- list(sample_tau_i=FALSE, sample_mobility_gamma=FALSE, sample_mobility_omega=FALSE, sample_kappa=FALSE)

# WHERE we sample matters. Selection operates in the TOP TAIL, so global
# discrimination over the whole prior range answers the wrong question.
# RANGE="top" samples within the best TOPFRAC of draws by the A1b likelihood;
# RANGE="full" keeps the whole prior range for contrast.
RANGE   <- Sys.getenv("RANGE", "top")
TOPFRAC <- as.numeric(Sys.getenv("TOPFRAC", "0.20"))
if (identical(RANGE, "full")) {
  sims <- unique(round(seq(1, 10000, length.out = K)))
} else {
  pq <- arrow::read_parquet(Sys.getenv("REFRUN"), col_select = c("sim", "likelihood"))
  pq <- pq[is.finite(pq$likelihood), ]
  cut <- stats::quantile(pq$likelihood, 1 - TOPFRAC)
  pool <- pq$sim[pq$likelihood >= cut]
  qs   <- stats::quantile(pq$likelihood[pq$likelihood >= cut], seq(0, 1, length.out = K))
  sims <- unique(vapply(qs, function(v) pool[which.min(abs(pq$likelihood[pq$sim %in% pool] - v))], numeric(1)))
  cat(sprintf("[phaseA] top-%.0f%% pool: %d draws, LL %.0f..%.0f\n",
              100*TOPFRAC, length(pool), min(pq$likelihood[pq$sim %in% pool]), max(pq$likelihood)))
}
cat(sprintf("[phaseA] simulating %d draws on %d cores\n", length(sims), NC))
cl <- makeCluster(NC); on.exit(try(stopCluster(cl), silent=TRUE))
clusterExport(cl, c("cfg","pri","sa","PKG"), envir=environment())
invisible(clusterCall(cl, function(pkg) {
  suppressMessages(devtools::load_all(pkg, quiet = TRUE))
  MOSAIC::set_root_directory(Sys.getenv("ROOT", "~/MOSAIC"))
  TRUE
}, PKG))
draws <- parLapplyLB(cl, sims, function(sid) {
  cs <- tryCatch(sample_parameters(PATHS=get_paths(), priors=pri, config=cfg, seed=sid,
                 sample_args=sa, verbose=FALSE, validate=FALSE), error=function(e) NULL)
  if (is.null(cs)) return(NULL)
  r <- tryCatch(run_simulation(config=cs, seed=sid, quiet=TRUE), error=function(e) NULL)
  if (is.null(r)) return(NULL)
  list(sim=sid, cases=as.numeric(r$results$reported_cases),
       deaths=as.numeric(r$results$reported_deaths))
})
stopCluster(cl); draws <- Filter(Negate(is.null), draws)
cat(sprintf("[phaseA] %d draws simulated\n", length(draws)))

# predictive skill, independent of any likelihood setting
sk <- do.call(rbind, lapply(draws, function(d){
  n <- min(length(d$cases), length(oc))
  i_tr <- which(tr[1:n] & is.finite(oc[1:n])); i_oo <- which(!tr[1:n] & is.finite(oc[1:n]))
  f <- function(i, p, o) if (length(i)<20) c(NA,NA) else
       c(as.numeric(calc_model_R2(observed=o[i], estimated=p[i], method="corr"))[1],
         sum(p[i])/max(sum(o[i]),1e-9))
  a <- f(i_tr, d$cases, oc); b <- f(i_oo, d$cases, oc); dd <- f(i_tr, d$deaths, od)
  data.frame(sim=d$sim, r2_tr=a[1], bias_tr=a[2], r2_oo=b[1], bias_oo=b[2],
             r2_d=dd[1], bias_d=dd[2])
}))

# the configuration grid
base <- list(nb_k_min_cases=3, nb_k_min_deaths=3, weight_cases=1, weight_deaths=1,
             weight_peak_timing=0, weight_peak_magnitude=0,
             weight_cumulative_total=0, weight_wis=0)
grid <- list(A1b_reference = base)
add <- function(nm, k, v) { g <- base; g[[k]] <- v; grid[[nm]] <<- g }
for (v in c(10,20,50)) add(sprintf("nbk_cases_%d", v), "nb_k_min_cases", v)
for (v in c(10,20))    add(sprintf("nbk_deaths_%d", v), "nb_k_min_deaths", v)
for (v in c(0.10,0.25,0.50)) {
  add(sprintf("peak_timing_%.2f", v), "weight_peak_timing", v)
  add(sprintf("peak_magn_%.2f",  v), "weight_peak_magnitude", v)
  add(sprintf("cumulative_%.2f", v), "weight_cumulative_total", v)
  add(sprintf("wis_%.2f",        v), "weight_wis", v)
}
for (v in c(2,4)) add(sprintf("w_deaths_%d", v), "weight_deaths", v)
add("w_cases_0.5", "weight_cases", 0.5)

ocm <- matrix(oc, nrow=1); odm <- matrix(od, nrow=1)
res <- do.call(rbind, lapply(names(grid), function(nm) {
  g <- grid[[nm]]
  ll <- vapply(draws, function(d){
    n <- min(length(d$cases), ncol(ocm))
    tryCatch(do.call(calc_model_likelihood, c(list(
        obs_cases=ocm[,1:n,drop=FALSE], est_cases=matrix(d$cases[1:n],nrow=1),
        obs_deaths=odm[,1:n,drop=FALSE], est_deaths=matrix(d$deaths[1:n],nrow=1),
        config=cfg, verbose=FALSE), g)), error=function(e) NA_real_)
  }, numeric(1))
  sp <- function(x, y) {
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 30) NA else suppressWarnings(cor(x[ok], y[ok], method = "spearman"))
  }
  okr <- is.finite(ll) & is.finite(sk$r2_tr)
  top <- if (sum(okr) >= 30) { q <- quantile(ll[okr], 0.95); sk$r2_tr[okr][ll[okr] >= q] } else NA
  data.frame(config=nm, n_ok=sum(is.finite(ll)),
             sp_ll_r2      = sp(ll, sk$r2_tr),
             sp_ll_r2_oos  = sp(ll, sk$r2_oo),
             sp_ll_absbias = sp(ll, abs(sk$bias_tr-1)),
             sp_ll_absbias_d = sp(ll, abs(sk$bias_d-1)),
             r2_top5 = if (all(is.na(top))) NA else median(top, na.rm=TRUE))
}))
res <- res[order(-res$sp_ll_r2), ]
cat("\n=============== PHASE A: DISCRIMINATION SCREEN ===============\n")
cat("sp_ll_r2: want POSITIVE (higher LL -> better R2). absbias: want NEGATIVE.\n\n")
print(res, row.names=FALSE, digits=3)
saveRDS(list(res=res, sk=sk), file.path(Sys.getenv("OUTDIR","."), "phaseA_screen.rds"))
