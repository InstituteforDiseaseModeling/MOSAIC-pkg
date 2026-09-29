# WHICH of the 4 parameters absorbs the ND-vs-LSTM gap? One-at-a-time, held-out.
suppressMessages(library(MOSAIC))
HERE<-"/home/jgiles/psi_evolve"
CANON<-"/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
VAR<-"target_D_rate_per_country_floored"
grid<-read.csv(file.path(HERE,"EVAL_GRID.csv")); grid<-grid[grid$grid=="prod"&grid$split=="selection",]
for(k in c("cutoff","test_start","test_end")) grid[[k]]<-as.Date(grid[[k]])
W<-read.csv(file.path(HERE,"weights_frozen.csv")); pool<-W$iso_code
wt<-setNames(W$w_sqrt/sum(W$w_sqrt),W$iso_code)
obs<-read.csv(CANON)[,c("iso_code","date",VAR)]; names(obs)[3]<-"y"
obs$date<-as.Date(obs$date); obs<-obs[is.finite(obs$y)&obs$iso_code%in%pool,]
gp<-function(arm,ct){p<-read.csv(file.path(HERE,paste0("psi_cache_",arm),sprintf("psi_%s.csv",ct)))
  p$date<-as.Date(p$date); p[p$iso_code%in%pool,c("iso_code","date","psi")]}
TA<-function(p,th) calc_psi_star(p,a=th[1],b=th[2],z=th[3],k=th[4],warn_k_rounding=FALSE)
# free-set variants: which params may move (others held at shipped default a=1,b=1,z=1,k=0)
VAR_SETS <- list(none=integer(0), b=2L, a=1L, k=4L, z=3L, ab=c(1L,2L), abzk=1:4)
fit<-function(psid,dates,ytr,yte,free){
  itr<-match(ytr$date,dates); o<-!is.na(itr); itr<-itr[o]; vtr<-ytr$y[o]
  ite<-match(yte$date,dates); o<-!is.na(ite); ite<-ite[o]; vte<-yte$y[o]
  base<-c(1,1,1,0)
  if(!length(free)) { s<-TA(psid,base); return(mean(abs(s[ite]-vte))) }
  lo<-c(0.02,-8,0.01,-90); hi<-c(6,8,1,90)
  obj<-function(u){th<-base; th[free]<-pmin(pmax(u,lo[free]),hi[free]); s<-TA(psid,th)
    mean(abs(s[itr]-vtr))}
  best<-list(v=Inf,u=base[free])
  st<-list(base[free]); if(1L%in%free) st<-c(st,list({x<-base[free];x[match(1L,free)]<-2;x}),
                                                list({x<-base[free];x[match(1L,free)]<-0.4;x}))
  if(4L%in%free) st<-c(st,list({x<-base[free];x[match(4L,free)]<-20;x}))
  if(2L%in%free) st<-c(st,list({x<-base[free];x[match(2L,free)]<--1;x}))
  for(u0 in st){r<-try(optim(u0,obj,method=if(length(free)==1)"Brent" else "Nelder-Mead",
      lower=if(length(free)==1)lo[free] else -Inf, upper=if(length(free)==1)hi[free] else Inf,
      control=list(maxit=400)),silent=TRUE)
    if(!inherits(r,"try-error")&&r$value<best$v) best<-list(v=r$value,u=r$par)}
  th<-base; th[free]<-pmin(pmax(best$u,lo[free]),hi[free]); s<-TA(psid,th)
  mean(abs(s[ite]-vte))
}
arms<-c("P000E","ND"); res<-list()
for(i in seq_len(nrow(grid))){ct<-format(grid$cutoff[i]);P<-lapply(arms,gp,ct=ct);names(P)<-arms
 for(iso in pool){
  ytr<-obs[obs$iso_code==iso&obs$date>=grid$cutoff[i]-365*3&obs$date<grid$cutoff[i],]
  yte<-obs[obs$iso_code==iso&obs$date>=grid$test_start[i]&obs$date<=grid$test_end[i],]
  if(nrow(ytr)<26||nrow(yte)<6) next
  row<-list(iso=iso,block=ct)
  for(a in arms){pa<-P[[a]][P[[a]]$iso_code==iso,];pa<-pa[order(pa$date),]
    for(v in names(VAR_SETS)) row[[paste0(v,"_",a)]]<-fit(pa$psi,pa$date,ytr,yte,VAR_SETS[[v]])}
  res[[length(res)+1]]<-as.data.frame(row)}
 cat("block",ct,"done\n")}
R<-do.call(rbind,res); R$w<-wt[R$iso]; write.csv(R,file.path(HERE,"absorb_result3.csv"),row.names=FALSE)
wm<-function(x,w) sum(x*w,na.rm=TRUE)/sum(w[is.finite(x)])
g0<-NA
cat("\n=== held-out OOS MAE by FREED parameter set (n=",nrow(R)," country-blocks) ===\n",sep="")
cat(sprintf("%-6s %8s %8s %8s %8s  %-7s %s\n","free","LSTM","ND","gap","gap%","absorb%","paired p"))
for(v in names(VAR_SETS)){
  L<-wm(R[[paste0(v,"_P000E")]],R$w); N<-wm(R[[paste0(v,"_ND")]],R$w); g<-L-N
  if(v=="none") g0<-g
  d<-R[[paste0(v,"_P000E")]]-R[[paste0(v,"_ND")]]
  cat(sprintf("%-6s %8.4f %8.4f %+8.4f %+7.1f%%  %6.1f%%  t=%.3f w=%.3f  ND>%d/%d\n",
    v,L,N,g,100*(1-N/L),100*(1-g/g0),t.test(d)$p.value,
    suppressWarnings(wilcox.test(d)$p.value),sum(d>0),length(d)))
}
