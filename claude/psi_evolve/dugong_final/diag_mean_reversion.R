# Is the "regional" split actually MEAN REVERSION in disguise?
#
# snf_1 (AGO,MOZ,MWI,ZMB,ZWE) holds most of the countries where psi ALREADY
# beats persistence; snf_2 (COD,SSD,SOM,ETH,BDI,TZA,KEN,RWA) is where psi fails
# badly. So "every change helps snf_2 and hurts snf_1" may not be regional
# structure at all -- it may be that a country with skill -1.3 has room to gain
# and a country with skill +0.6 has room to lose. If so, region is a PROXY and
# building architecture arms on the regional story would be building on sand.
#
# Decisive test: correlate each arm's per-country delta with the INCUMBENT's
# per-country skill, then ask whether region survives conditioning on it.
HERE <- "/home/jgiles/psi_evolve"
RMAP <- system.file("extdata", "region_map_snf_k5.csv", package = "MOSAIC")
W <- utils::read.csv(file.path(HERE, "weights_frozen.csv"), stringsAsFactors = FALSE)
W$w <- W$w_sqrt / sum(W$w_sqrt)
R <- utils::read.csv(RMAP, stringsAsFactors = FALSE)
S_iso <- function(cells, blocks) {
  d <- cells[cells$horizon == "all" & cells$fold %in% blocks &
             is.finite(cells$skill_persistence), ]
  stats::aggregate(skill_persistence ~ iso_code, d, stats::median)
}
pairs <- list(
  list(nm = "C9d  vs P001 (class C, post-processing)",
       a = "score_C9d_selection_seed_psi.rds",  b = "score_P001_selection_seed_psi.rds"),
  list(nm = "P000 vs P001 (class R, data/features)",
       a = "score_P000_selection_seed_psi.rds", b = "score_P001_selection_seed_psi.rds"),
  list(nm = "P000R vs P000 (SAME SPEC -- pure fit noise)",
       a = "score_P000R_selection_seed_psi.rds", b = "score_P000_selection_seed_psi.rds")
)
for (P in pairs) {
  fa <- file.path(HERE, P$a); fb <- file.path(HERE, P$b)
  if (!file.exists(fa) || !file.exists(fb)) { cat("skip:", P$nm, "\n"); next }
  A <- readRDS(fa); B <- readRDS(fb); bl <- intersect(A$blocks, B$blocks)
  x <- merge(S_iso(A$cells, bl), S_iso(B$cells, bl), by = "iso_code",
             suffixes = c("_arm", "_inc"))
  x$delta  <- x$skill_persistence_arm - x$skill_persistence_inc
  x$inc    <- x$skill_persistence_inc
  x$region <- R$region[match(x$iso_code, R$iso_code)]
  x <- x[is.finite(x$delta) & is.finite(x$inc) & !is.na(x$region), ]
  cr <- cor(x$inc, x$delta); crs <- cor(x$inc, x$delta, method = "spearman")
  cat(sprintf("\n%s   n=%d\n", P$nm, nrow(x)))
  cat(sprintf("  cor(incumbent skill, delta) = %+.3f Pearson / %+.3f Spearman\n", cr, crs))
  s1 <- x$region == "snf_1"; s2 <- x$region == "snf_2"
  cat(sprintf("  mean incumbent skill: snf_1 %+.3f   snf_2 %+.3f\n",
              mean(x$inc[s1]), mean(x$inc[s2])))
  # does region add anything once the incumbent level is controlled for?
  if (sum(s1) >= 2 && sum(s2) >= 2) {
    m0 <- stats::lm(delta ~ inc, data = x)
    m1 <- stats::lm(delta ~ inc + I(region == "snf_1"), data = x)
    an <- stats::anova(m0, m1)
    cat(sprintf("  delta ~ inc            : R2 %.3f\n", summary(m0)$r.squared))
    cat(sprintf("  delta ~ inc + snf_1    : R2 %.3f   region adds p = %.4f\n",
                summary(m1)$r.squared, an$`Pr(>F)`[2]))
  }
}
cat("\nREAD: a strongly NEGATIVE cor means mean reversion. If region's added p is\n")
cat("non-significant once `inc` is controlled, the regional story is a PROXY for\n")
cat("'psi had room to improve there' and the architecture argument needs restating.\n")
