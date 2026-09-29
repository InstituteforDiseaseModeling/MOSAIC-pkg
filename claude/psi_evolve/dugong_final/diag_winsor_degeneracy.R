# Is the tiny floor under winsorising real, or is it capping most of the data?
# If a large share of cells sit at the cap, both replicates report the cap for
# them, the difference vanishes, and the "resolution gain" is just information
# being thrown away. Measure the capped fraction before recommending anything.
HERE <- "/home/jgiles/psi_evolve"
f <- file.path(HERE, c("score_P000_selection_seed_psi.rds","score_P000R_selection_seed_psi.rds",
                       "score_P001_selection_seed_psi.rds","score_C9d_selection_seed_psi.rds"))
f <- f[file.exists(f)]
all_sk <- unlist(lapply(f, function(p) {
  X <- readRDS(p); c <- X$cells
  c$skill_persistence[c$horizon == "all" & is.finite(c$skill_persistence)]
}))
cat(sprintf("pooled country-block cells across %d scored arms: n = %d\n", length(f), length(all_sk)))
cat(sprintf("skill distribution: min %.2f  q05 %.2f  q25 %.2f  median %.2f  q75 %.2f  max %.2f\n",
    min(all_sk), quantile(all_sk,.05), quantile(all_sk,.25), median(all_sk),
    quantile(all_sk,.75), max(all_sk)))
for (cap in c(2, 1, 0.5)) {
  lo <- mean(all_sk < -cap); hi <- mean(all_sk > cap)
  cat(sprintf("winsor at +/-%-4g : %5.1f%% of cells capped (%.1f%% below, %.1f%% above)\n",
      cap, 100*(lo+hi), 100*lo, 100*hi))
}
cat("\nREAD: a cap that touches only a small tail bounds outliers. A cap touching a\n")
cat("large share of cells is discarding the scale, and its low floor is degenerate.\n")
