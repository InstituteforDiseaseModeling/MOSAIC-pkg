# Skill = 1 - WIS_m/WIS_b is bounded ABOVE by 1 (max observed 0.90) and
# UNBOUNDED BELOW (min observed -12.20). A symmetric winsor is therefore
# misspecified: the upper cap either never binds (+/-1, +/-2) or destroys real
# successes (+/-0.5 caps 18.6% of cells from above). The principled bound is
# ONE-SIDED. Measure floor and floor/|S| for one-sided floors on the same
# P000/P000R replicate pair.
HERE <- "/home/jgiles/psi_evolve"
W <- utils::read.csv(file.path(HERE,"weights_frozen.csv"), stringsAsFactors=FALSE)
W$w_sqrt <- W$w_sqrt/sum(W$w_sqrt)
A <- readRDS(file.path(HERE,"score_P000_selection_seed_psi.rds"))
B <- readRDS(file.path(HERE,"score_P000R_selection_seed_psi.rds"))
bl <- intersect(A$blocks, B$blocks)
cl <- function(X) X$cells[X$cells$horizon=="all" & X$cells$fold %in% bl &
                          is.finite(X$cells$skill_persistence), ]
ca <- cl(A); cb <- cl(B)
sk_all <- c(ca$skill_persistence, cb$skill_persistence)
agg <- function(d, fn, wmode, lo) {
  d$sk <- pmax(lo, d$skill_persistence)
  pi <- stats::aggregate(sk ~ iso_code, d, fn)
  w <- if (identical(wmode,"sqrt")) W$w_sqrt[match(pi$iso_code, W$iso_code)] else rep(1,nrow(pi))
  sum(w/sum(w) * pi$sk)
}
cat(sprintf("%-34s %8s %8s %8s %9s %7s\n","estimand","S(P000)","S(P000R)","FLOOR","floor/|S|","capped"))
best <- NULL
for (fn in list(list(n="median",f=stats::median), list(n="mean",f=mean)))
 for (wm in c("sqrt","equal"))
  for (lo in c(-Inf,-3,-2,-1)) {
    sa <- agg(ca,fn$f,wm,lo); sb <- agg(cb,fn$f,wm,lo)
    fl <- abs(sa-sb); rat <- fl/mean(abs(c(sa,sb)))
    cap <- 100*mean(sk_all < lo)
    nm <- sprintf("%s + %s%s", fn$n, wm, if (is.finite(lo)) sprintf(", floor at %g", lo) else "")
    cat(sprintf("%-34s %+8.4f %+8.4f %8.4f %9.3f %6.1f%%\n", nm, sa, sb, fl, rat, cap))
  }
cat("\nCURRENT is 'median + sqrt' (no floor). Lower floor/|S| = better resolution;\n")
cat("'capped' is the share of cells the one-sided bound touches -- all of it lower tail.\n")
