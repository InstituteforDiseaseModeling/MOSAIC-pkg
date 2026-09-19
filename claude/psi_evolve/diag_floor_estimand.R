# Could a different AGGREGATION give more resolution for the same compute?
#
# The floor is 0.0912 and it is dominated by one country: COD moves 0.611
# between two fits of the same spec and carries the largest weight (0.132). If
# the estimand were less hostage to a single unstable country, the SAME arms
# would become readable. That is an objective change (PROTOCOL 5.1) and an agent
# may not enact one -- but measuring what each option would cost is free, and it
# is the decision-relevant number.
#
# Every variant is applied IDENTICALLY to the P000/P000R replicate pair, so the
# quantity reported is the floor each estimand would have.
HERE <- "/home/jgiles/psi_evolve"
A <- readRDS(file.path(HERE, "score_P000_selection_seed_psi.rds"))
B <- readRDS(file.path(HERE, "score_P000R_selection_seed_psi.rds"))
W <- utils::read.csv(file.path(HERE, "weights_frozen.csv"), stringsAsFactors = FALSE)
W$w_sqrt <- W$w_sqrt / sum(W$w_sqrt)
bl <- intersect(A$blocks, B$blocks)

cells <- function(X) X$cells[X$cells$horizon == "all" & X$cells$fold %in% bl &
                             is.finite(X$cells$skill_persistence), ]
# per (country, block) skill for each replicate
ca <- cells(A); cb <- cells(B)

agg <- function(cl, block_fn, wmode, wins = Inf, trim_iso = NULL) {
  d <- cl
  if (!is.null(trim_iso)) d <- d[d$iso_code != trim_iso, ]
  d$sk <- pmax(-wins, pmin(wins, d$skill_persistence))
  pi <- stats::aggregate(sk ~ iso_code, d, block_fn)
  w <- if (identical(wmode, "sqrt")) W$w_sqrt[match(pi$iso_code, W$iso_code)] else
       rep(1 / nrow(pi), nrow(pi))
  w <- w / sum(w)
  sum(w * pi$sk)
}
variants <- list(
  list(nm = "CURRENT: median-over-blocks, sqrt-burden",  f = stats::median, w = "sqrt", wins = Inf),
  list(nm = "mean-over-blocks, sqrt-burden",             f = mean,          w = "sqrt", wins = Inf),
  list(nm = "median, EQUAL country weights",             f = stats::median, w = "equal", wins = Inf),
  list(nm = "median, sqrt-burden, skill winsorised +/-1",f = stats::median, w = "sqrt", wins = 1),
  list(nm = "median, sqrt-burden, winsorised +/-0.5",    f = stats::median, w = "sqrt", wins = 0.5),
  list(nm = "median, equal weights, winsorised +/-1",    f = stats::median, w = "equal", wins = 1)
)
cat(sprintf("%-48s %9s %9s %8s\n", "estimand", "S(P000)", "S(P000R)", "FLOOR"))
for (v in variants) {
  sa <- agg(ca, v$f, v$w, v$wins); sb <- agg(cb, v$f, v$w, v$wins)
  cat(sprintf("%-48s %+9.4f %+9.4f %8.4f\n", v$nm, sa, sb, abs(sa - sb)))
}
cat("\n-- leave-one-country-out on the CURRENT estimand (which country carries the floor?) --\n")
base <- abs(agg(ca, stats::median, "sqrt") - agg(cb, stats::median, "sqrt"))
out <- sapply(W$iso_code, function(i)
  abs(agg(ca, stats::median, "sqrt", trim_iso = i) - agg(cb, stats::median, "sqrt", trim_iso = i)))
o <- sort(out)
cat(sprintf("full floor %.4f\n", base))
for (i in names(o)[1:5]) cat(sprintf("  drop %-4s -> floor %.4f  (%.0f%% of full)\n",
                                     i, o[[i]], 100 * o[[i]] / base))
cat("\nREAD: any variant with a materially lower floor makes the SAME arms readable at the\n")
cat("same compute. It is an objective change (5.1) and therefore a USER decision, not mine.\n")
