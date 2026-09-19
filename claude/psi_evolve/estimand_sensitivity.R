# =============================================================================
# estimand_sensitivity.R -- report an arm's delta AND the applicable floor under
# every candidate estimand, so "is this arm readable?" and "is the estimand the
# problem?" are answered by the same table.
#
# WHY. The measured floor (0.0912) is dominated by ONE country -- dropping COD
# takes it to 0.0120 -- and is a property of the AGGREGATION, not of the
# compute: floor/|S| is 0.285 under the current median-over-blocks + sqrt-burden
# and 0.114 under winsorised skill. So an arm can be unreadable purely because
# of how S is formed. Reporting the delta under one estimand alone cannot
# distinguish "no effect" from "effect below an avoidable noise level".
#
# The floors come from the P000/P000R replicate pair (same spec, disjoint seed
# block) recomputed under each estimand, so every row is self-consistent.
#
# NOTE none of these alternatives is enacted: the objective is frozen at
# median-over-blocks + sqrt-burden (PROTOCOL 5.1). This is a REPORT.
#
# usage: Rscript estimand_sensitivity.R ARM_RDS INCUMBENT_RDS
# =============================================================================
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) stop("usage: Rscript estimand_sensitivity.R ARM_RDS INCUMBENT_RDS")
HERE <- "/home/jgiles/psi_evolve"
W <- utils::read.csv(file.path(HERE, "weights_frozen.csv"), stringsAsFactors = FALSE)
W$w_sqrt <- W$w_sqrt / sum(W$w_sqrt)

cells <- function(X, bl) X$cells[X$cells$horizon == "all" & X$cells$fold %in% bl &
                                 is.finite(X$cells$skill_persistence), ]
agg <- function(cl, block_fn, wmode, wins) {
  d <- cl; d$sk <- pmax(-wins, pmin(wins, d$skill_persistence))
  pi <- stats::aggregate(sk ~ iso_code, d, block_fn)
  w <- if (identical(wmode, "sqrt")) W$w_sqrt[match(pi$iso_code, W$iso_code)] else
       rep(1, nrow(pi))
  w <- w / sum(w)
  sum(w * pi$sk)
}
VAR <- list(
  list(nm = "CURRENT median + sqrt-burden",  f = stats::median, w = "sqrt",  wins = Inf),
  list(nm = "mean-over-blocks + sqrt",       f = mean,          w = "sqrt",  wins = Inf),
  list(nm = "median + equal weights",        f = stats::median, w = "equal", wins = Inf),
  list(nm = "median + sqrt, winsor +/-1",    f = stats::median, w = "sqrt",  wins = 1),
  list(nm = "median + sqrt, winsor +/-0.5",  f = stats::median, w = "sqrt",  wins = 0.5),
  list(nm = "mean + equal, winsor +/-1",     f = mean,          w = "equal", wins = 1)
)
A <- readRDS(args[1]); B <- readRDS(args[2])
bl <- intersect(A$blocks, B$blocks)
ca <- cells(A, bl); cb <- cells(B, bl)

# replicate pair -> the floor under each estimand
fp <- file.path(HERE, c("score_P000_selection_seed_psi.rds", "score_P000R_selection_seed_psi.rds"))
haveF <- all(file.exists(fp))
if (haveF) {
  R1 <- readRDS(fp[1]); R2 <- readRDS(fp[2]); rb <- intersect(R1$blocks, R2$blocks)
  r1 <- cells(R1, rb); r2 <- cells(R2, rb)
}
cat(sprintf("\n=== ESTIMAND SENSITIVITY: %s vs %s (%d blocks) ===\n",
            A$arm_id, B$arm_id, length(bl)))
cat(sprintf("%-30s %9s %9s %9s %8s %s\n", "estimand", "S(arm)", "S(inc)", "delta", "floor", "readable?"))
for (v in VAR) {
  sa <- agg(ca, v$f, v$w, v$wins); sb <- agg(cb, v$f, v$w, v$wins)
  fl <- if (haveF) abs(agg(r1, v$f, v$w, v$wins) - agg(r2, v$f, v$w, v$wins)) else NA_real_
  rd <- if (is.na(fl)) "?" else if (abs(sa - sb) > fl) "YES" else "no"
  cat(sprintf("%-30s %+9.4f %+9.4f %+9.4f %8.4f %s\n", v$nm, sa, sb, sa - sb, fl, rd))
}
cat("\nREAD: 'readable' means |delta| exceeds the fit-noise floor THAT ESTIMAND would have.\n")
cat("A row that is readable where CURRENT is not means the arm is being hidden by the\n")
cat("aggregation, not by the model. Changing the objective is a USER decision (5.1).\n")
