# =============================================================================
# region_split.R -- does an arm's benefit split along the production region map?
#
# WHY THIS IS A SCRIPT. Wave 22 found that C9d's per-country deltas separate by
# the shipped snf_k5 map (snf_1 weighted -0.259 with EVERY member non-positive,
# snf_2 +0.498, Wilcoxon p = 0.045) -- but that analysis pasted deltas out of
# the gate's printed table by hand. That is the "derived inside the wave that
# wanted it" pattern PROTOCOL section 3 criticises for the gates, and it has the
# same failure modes: a transcription slip, or a grouping quietly chosen to fit.
# One script, reading the score RDS directly, for every arm.
#
# NOTE ON INTERPRETATION: region is tested because it is the SHIPPED grouping,
# not one fitted to the deltas -- but it is still a post-hoc hypothesis, and it
# is confounded with epidemic regime (snf_2 IS the endemic/protracted-crisis
# set). Report it as suggestive, never as established.
#
# usage: Rscript region_split.R ARM_RDS INCUMBENT_RDS
# =============================================================================
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) stop("usage: Rscript region_split.R ARM_RDS INCUMBENT_RDS")
HERE <- "/home/jgiles/psi_evolve"
# Read the region map from the INSTALLED package, not from a source path: the
# compute host's MOSAIC-pkg copy is partial, and the installed extdata is the
# same file the fits actually used (.psi_resolve_region_map reads it there).
RMAP <- system.file("extdata", "region_map_snf_k5.csv", package = "MOSAIC")
if (!nzchar(RMAP) || !file.exists(RMAP))
     stop("region_split: region_map_snf_k5.csv not found in the installed MOSAIC.")
A <- readRDS(args[1]); B <- readRDS(args[2])
stopifnot(identical(A$objective_version, B$objective_version))
if (!identical(A$mode, B$mode)) stop("mode mismatch")

W <- utils::read.csv(file.path(HERE, "weights_frozen.csv"), stringsAsFactors = FALSE)
W$w <- W$w_sqrt / sum(W$w_sqrt)
R <- utils::read.csv(RMAP, stringsAsFactors = FALSE)
blocks <- intersect(A$blocks, B$blocks)

S_iso <- function(cells) {
     d <- cells[cells$horizon == "all" & cells$fold %in% blocks &
                is.finite(cells$skill_persistence), ]
     stats::aggregate(skill_persistence ~ iso_code, d, stats::median)
}
x <- merge(S_iso(A$cells), S_iso(B$cells), by = "iso_code", suffixes = c("_arm", "_inc"))
x$delta  <- x$skill_persistence_arm - x$skill_persistence_inc
x$region <- R$region[match(x$iso_code, R$iso_code)]
x$w      <- W$w[match(x$iso_code, W$iso_code)]
x <- x[is.finite(x$delta) & !is.na(x$region), ]

cat(sprintf("\n=== REGION SPLIT: %s vs %s (%d blocks, %d countries) ===\n",
            A$arm_id, B$arm_id, length(blocks), nrow(x)))
print(data.frame(iso = x$iso_code, region = x$region, w = round(x$w, 4),
                 delta = round(x$delta, 4))[order(x$region, -x$delta), ],
      row.names = FALSE)

cat("\n-- weighted mean delta by region --\n")
for (rg in sort(unique(x$region))) {
     z <- x[x$region == rg, ]
     cat(sprintf("%-6s n=%d  wsum %.3f  delta %+.4f   all non-positive: %-5s  %s\n",
                 rg, nrow(z), sum(z$w), sum(z$w * z$delta) / sum(z$w),
                 all(z$delta <= 0.01), paste(z$iso_code, collapse = ",")))
}
s1 <- x$delta[x$region == "snf_1"]; s2 <- x$delta[x$region == "snf_2"]
if (length(s1) >= 2L && length(s2) >= 2L) {
     p <- suppressWarnings(stats::wilcox.test(s1, s2)$p.value)
     cat(sprintf("\nsnf_1 (n=%d, %d positive) vs snf_2 (n=%d, %d positive):  Wilcoxon p = %.4f\n",
                 length(s1), sum(s1 > 0), length(s2), sum(s2 > 0), p))
     cat(sprintf("%s\n", if (is.finite(p) && p < 0.05)
          "=> the split REPLICATES on this arm. Suggestive, not established: post-hoc grouping, confounded with epidemic regime."
          else "=> no significant split on this arm."))
}
