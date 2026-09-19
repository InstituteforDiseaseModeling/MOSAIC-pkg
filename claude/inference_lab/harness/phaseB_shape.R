#!/usr/bin/env Rscript
# Phase B: which shape terms improve BIAS while RETAINING R2, in-sample AND OOS?
# Base is FIXED: A1b + B2.2, weight_cases = weight_deaths = 1,
# nb_k_min_cases = 20, nb_k_min_deaths = 3. Only the four shape terms vary.
PKG <- Sys.getenv("PKG"); NC <- as.integer(Sys.getenv("NCORES","8"))
TCUT <- as.Date(Sys.getenv("T_CUT","2025-09-01")); CACHE <- Sys.getenv("CACHE")
OUTD <- Sys.getenv("OUTDIR","."); TOPQ <- as.numeric(Sys.getenv("TOPQ","0.90"))
suppressMessages(devtools::load_all(PKG, quiet=TRUE)); suppressMessages(library(parallel)); suppressMessages(library(matrixStats))
set_root_directory(Sys.getenv("ROOT","~/MOSAIC"))
cfg <- get_location_config(iso="ETH")
oc <- as.numeric(cfg$reported_cases); od <- as.numeric(cfg$reported_deaths)
cc <- readRDS(CACHE); draws <- cc$draws; sk <- cc$sk
cat(sprintf("[B] %d cached draws\n", length(draws)))

BASE <- list(nb_k_min_cases=20, nb_k_min_deaths=3, weight_cases=1, weight_deaths=1,
             weight_peak_timing=0, weight_peak_magnitude=0,
             weight_cumulative_total=0, weight_wis=0)
grid <- list(); addg <- function(nm, mods){g<-BASE; for(k in names(mods)) g[[k]]<-mods[[k]]; grid[[nm]]<<-g}
addg("BASE", list())
TERMS <- c(pkT="weight_peak_timing", pkM="weight_peak_magnitude",
           cum="weight_cumulative_total", wis="weight_wis")
# 1) dose-response, one term at a time
for (tn in names(TERMS)) for (v in c(0.05,0.10,0.25,0.50,1.00))
  addg(sprintf("%s=%.2f", tn, v), setNames(list(v), TERMS[[tn]]))
# 2) factorial over all four at {0, 0.25, 0.50}
lv <- c(0, 0.25, 0.50)
fac <- expand.grid(pkT=lv, pkM=lv, cum=lv, wis=lv)
fac <- fac[rowSums(fac > 0) >= 2, ]        # singles already covered
for (i in seq_len(nrow(fac))) {
  r <- fac[i, ]; mods <- setNames(as.list(as.numeric(r)), unname(TERMS[names(r)]))
  nm <- paste(sprintf("%s%.2f", names(r)[r>0], as.numeric(r)[r>0]), collapse="+")
  addg(nm, mods)
}
cat(sprintf("[B] %d configurations\n", length(grid)))

ocm <- matrix(oc,nrow=1); odm <- matrix(od,nrow=1)
cl <- makeCluster(NC); clusterExport(cl, c("draws","ocm","odm","cfg","PKG"), envir=environment())
invisible(clusterCall(cl, function(p){suppressMessages(devtools::load_all(p, quiet=TRUE)); TRUE}, PKG))
llmat <- parLapplyLB(cl, grid, function(g){
  vapply(draws, function(d){ n <- min(length(d$cases), ncol(ocm))
    tryCatch(do.call(calc_model_likelihood, c(list(obs_cases=ocm[,1:n,drop=FALSE],
      est_cases=matrix(d$cases[1:n],nrow=1), obs_deaths=odm[,1:n,drop=FALSE],
      est_deaths=matrix(d$deaths[1:n],nrow=1), config=cfg, verbose=FALSE), g)),
      error=function(e) NA_real_)}, numeric(1))
}); stopCluster(cl)

# Score the ENSEMBLE of selected draws, not individual draws: production takes
# the per-timepoint median across the selected members. Individual-draw R2 is
# ~0.13 where the ensemble reaches ~0.80, so scoring draws measures the wrong
# object entirely.
dts  <- seq(as.Date(cfg$date_start), by="day", length.out=ncol(ocm))
ntk  <- min(vapply(draws, function(d) length(d$cases), numeric(1)), ncol(ocm))
Mc   <- vapply(draws, function(d) d$cases[1:ntk],  numeric(ntk))
Md   <- vapply(draws, function(d) d$deaths[1:ntk], numeric(ntk))
i_tr <- which(dts[1:ntk] <= TCUT & is.finite(oc[1:ntk]))
i_oo <- which(dts[1:ntk] >  TCUT & is.finite(oc[1:ntk]))
i_td <- which(dts[1:ntk] <= TCUT & is.finite(od[1:ntk]))
sc <- function(p, o, i) {
  if (length(i) < 20) return(c(NA_real_, NA_real_))
  c(as.numeric(calc_model_R2(observed=o[i], estimated=p[i], method="corr"))[1],
    sum(p[i]) / max(sum(o[i]), 1e-9))
}
res <- do.call(rbind, lapply(names(grid), function(nm){
  ll <- llmat[[nm]]; ok <- is.finite(ll)
  if (sum(ok) < 50) return(NULL)
  q <- quantile(ll[ok], TOPQ); sel <- which(ok & ll >= q)
  ec <- matrixStats::rowMedians(Mc[, sel, drop=FALSE])
  ed <- matrixStats::rowMedians(Md[, sel, drop=FALSE])
  a <- sc(ec, oc, i_tr); b2 <- sc(ec, oc, i_oo); dd <- sc(ed, od, i_td)
  data.frame(config=nm, n_sel=length(sel),
             r2_tr=a[1], bias_tr=a[2], r2_oos=b2[1], bias_oos=b2[2],
             r2_d=dd[1], bias_d=dd[2])
}))
b <- res[res$config=="BASE",]
res$d_r2_tr   <- res$r2_tr   - b$r2_tr
res$d_r2_oos  <- res$r2_oos  - b$r2_oos
res$d_abias_tr  <- abs(res$bias_tr-1)  - abs(b$bias_tr-1)
res$d_abias_oos <- abs(res$bias_oos-1) - abs(b$bias_oos-1)
# DOMINATING = improves (or holds) R2 on both windows AND improves bias on both
res$dominates <- res$d_r2_tr >= -0.005 & res$d_r2_oos >= -0.005 &
                 res$d_abias_tr < 0 & res$d_abias_oos < 0
cat("\n================ BASE ================\n"); print(b[,2:8], row.names=FALSE, digits=3)
cat("\n======== CONFIGS DOMINATING THE BASE (R2 held on BOTH windows, bias improved on BOTH) ========\n")
dom <- res[which(res$dominates), ]
dom <- dom[order(dom$d_abias_oos + dom$d_abias_tr), ]
if (nrow(dom)) print(head(dom[,c("config","r2_tr","bias_tr","r2_oos","bias_oos","d_r2_oos","d_abias_tr","d_abias_oos")], 15),
                     row.names=FALSE, digits=3) else cat("  NONE\n")
cat("\n======== best OOS bias regardless of dominance ========\n")
print(head(res[order(abs(res$bias_oos-1)), c("config","r2_tr","bias_tr","r2_oos","bias_oos")], 10), row.names=FALSE, digits=3)
saveRDS(res, file.path(OUTD,"phaseB_shape.rds")); cat("\nsaved\n")
