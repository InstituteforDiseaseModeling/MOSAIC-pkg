# Honest version: fit ONE (a,b,z,k) per (country,cutoff) on PRE-CUTOFF history
# (with the production MAP penalty), then evaluate on the held-out OOS window.
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
# production priors: a~truncnorm(1,1,>0); b~N(1,2.5); z~Beta(2,1); k~truncnorm(-5,20,[-90,90])
nlp <- function(a,b,z,k) 0.5*((a-1)/1)^2 + 0.5*((b-1)/2.5)^2 - log(max(z,1e-9)) + 0.5*((k+5)/20)^2
T_apply <- function(psi,a,b,z,k) calc_psi_star(psi,a=a,b=b,z=z,k=k,warn_k_rounding=FALSE)
fit_tt <- function(psid, dates_all, ytr, yte, lambda) {
  itr <- match(ytr$date, dates_all); otr <- !is.na(itr); itr<-itr[otr]; vtr<-ytr$y[otr]
  ite <- match(yte$date, dates_all); ote <- !is.na(ite); ite<-ite[ote]; vte<-yte$y[ote]
  obj <- function(th) { a<-exp(th[1]); b<-th[2]; z<-plogis(th[3]); k<-th[4]
    s <- T_apply(psid,a,b,z,k); mean(abs(s[itr]-vtr)) + lambda*nlp(a,b,z,k)/max(length(itr),1) }
  best <- list(v=Inf,th=c(0,1,10,-5))
  for (s0 in list(c(0,1,10,-5),c(0,0,10,0),c(0,-1,10,0),c(log(2),1,10,-5),
                  c(log(.5),1,10,-5),c(0,1,0,-5),c(0,1,10,20))) {
    r <- try(optim(s0,obj,method="Nelder-Mead",control=list(maxit=600,reltol=1e-8)),silent=TRUE)
    if (!inherits(r,"try-error") && r$value<best$v) best<-list(v=r$value,th=r$par)
  }
  th<-best$th; a<-exp(th[1]); b<-th[2]; z<-plogis(th[3]); k<-th[4]
  s <- T_apply(psid,a,b,z,k)
  s0 <- T_apply(psid,1,1,1,0)   # shipped config default (b=1)
  list(te_opt=mean(abs(s[ite]-vte)), te_raw=mean(abs(psid[ite]-vte)),
       te_def=mean(abs(s0[ite]-vte)), a=a,b=b,z=z,k=k)
}
arms <- c("P000E","ND"); res <- list()
for (i in seq_len(nrow(grid))) {
  ct <- format(grid$cutoff[i]); P <- lapply(arms,get_psi,ct=ct); names(P)<-arms
  for (iso in pool) {
    ytr <- obs[obs$iso_code==iso & obs$date>=grid$cutoff[i]-365*3 & obs$date<grid$cutoff[i],]
    yte <- obs[obs$iso_code==iso & obs$date>=grid$test_start[i] & obs$date<=grid$test_end[i],]
    if (nrow(ytr)<26 || nrow(yte)<6) next
    row <- list(iso=iso,block=ct,ntr=nrow(ytr),nte=nrow(yte))
    for (a in arms) {
      pa <- P[[a]][P[[a]]$iso_code==iso,]; pa<-pa[order(pa$date),]
      f <- fit_tt(pa$psi,pa$date,ytr,yte,lambda=1)
      for (nm in names(f)) row[[paste0(nm,"_",a)]] <- f[[nm]]
    }
    res[[length(res)+1]] <- as.data.frame(row,stringsAsFactors=FALSE)
  }
  cat("block",ct,"done\n")
}
R <- do.call(rbind,res); R$w <- wt[R$iso]
write.csv(R,file.path(HERE,"absorb_result2.csv"),row.names=FALSE)
wm <- function(x,w) sum(x*w,na.rm=TRUE)/sum(w[is.finite(x)])
cat("\n=== held-out OOS MAE, theta fitted on 3y pre-cutoff history (MAP), n=",nrow(R)," ===\n",sep="")
for (tag in c("te_raw","te_def","te_opt")) {
  L<-wm(R[[paste0(tag,"_P000E")]],R$w); N<-wm(R[[paste0(tag,"_ND")]],R$w)
  cat(sprintf("%-7s LSTM %.4f  ND %.4f   gap %+.4f (%+.1f%%)\n",tag,L,N,L-N,100*(1-N/L)))
}
g_raw <- wm(R$te_raw_P000E,R$w)-wm(R$te_raw_ND,R$w)
g_opt <- wm(R$te_opt_P000E,R$w)-wm(R$te_opt_ND,R$w)
g_def <- wm(R$te_def_P000E,R$w)-wm(R$te_def_ND,R$w)
cat(sprintf("\nabsorbed (vs raw psi):      %.1f%%\nabsorbed (vs shipped b=1):  %.1f%%\n",
   100*(1-g_opt/g_raw), 100*(1-g_opt/g_def)))
for (tag in c("te_raw","te_def","te_opt")) {
  d <- R[[paste0(tag,"_P000E")]]-R[[paste0(tag,"_ND")]]
  cat(sprintf("%-7s paired: ND better %d/%d  mean %+.5f  sd %.5f  t-p %.3f  wilcox-p %.3f\n",
     tag,sum(d>0),length(d),mean(d),sd(d),t.test(d)$p.value,
     suppressWarnings(wilcox.test(d)$p.value)))
}
cat("\nfitted theta (median over cells):\n")
for (a in arms) cat(sprintf("  %-6s a=%.2f b=%+.2f z=%.2f k=%+.0f\n",a,
  median(R[[paste0("a_",a)]]),median(R[[paste0("b_",a)]]),
  median(R[[paste0("z_",a)]]),median(R[[paste0("k_",a)]])))
