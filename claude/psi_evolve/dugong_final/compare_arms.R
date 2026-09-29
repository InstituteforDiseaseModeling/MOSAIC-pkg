# =============================================================================
# compare_arms.R -- the GATE CALCULATOR. Given two scored arms, decide A1-A6
# (PROTOCOL section 3) reproducibly.
#
# WHY THIS EXISTS. Through wave 20 every gate was computed ad hoc inside the
# wave that wanted it, which is how (a) the no-regression guard shipped failing
# open in four ways, (b) a percentile bootstrap was quoted whose median sat 0.32
# from its point estimate, and (c) an exact origin-level test that disagreed with
# the bootstrap was not run until wave 20. One script, one code path, every arm.
#
# usage:
#   Rscript compare_arms.R ARM_RDS INCUMBENT_RDS            # gate an arm
#   Rscript compare_arms.R A_RDS A_REPLICATE_RDS --floor    # measure the floor
#
# --floor changes the QUESTION, not the arithmetic: the same two arms scored from
# disjoint seed blocks are the same model, so |dS| is not an effect, it is the
# resolution limit that every class-R effect must exceed (PROTOCOL 3b).
# =============================================================================
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L)
     stop("usage: Rscript compare_arms.R ARM_RDS INCUMBENT_RDS [--floor] [--nboot N]")
FLOOR <- "--floor" %in% args
NB    <- { i <- match("--nboot", args); if (!is.na(i)) as.integer(args[i + 1L]) else 4000L }
f_arm <- args[1]; f_inc <- args[2]
HERE  <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1]),
                               mustWork = FALSE))
if (!nzchar(HERE) || HERE == ".") HERE <- getwd()

A <- readRDS(f_arm); B <- readRDS(f_inc)
TOP10 <- c("COD","NGA","SSD","ETH","MOZ","SOM","MWI","AGO","ZWE","ZMB")

# ---- comparability assertions (PROTOCOL section 4, "measurement validity") ---
stopifnot(identical(A$objective_version, B$objective_version))
if (!identical(A$mode, B$mode))
     stop("mode mismatch: ", A$mode, " vs ", B$mode,
          ". A selection score and a confirmation score are not comparable.")
if (!identical(A$interval_mode, B$interval_mode))
     stop("interval_mode mismatch: ", A$interval_mode, " vs ", B$interval_mode,
          ". The interval treatment flipped two arm verdicts at v2; it must match.")
blocks <- intersect(A$blocks, B$blocks)
if (!length(blocks)) stop("no blocks in common")
if (!setequal(A$blocks, B$blocks))
     message(sprintf("NOTE: block sets differ (arm %s vs incumbent %s); restricting to the %d common.",
                     paste(A$blocks, collapse = ","), paste(B$blocks, collapse = ","),
                     length(blocks)))

W <- utils::read.csv(file.path(HERE, "weights_frozen.csv"), stringsAsFactors = FALSE)
W$w <- W$w_sqrt / sum(W$w_sqrt)

ca <- A$cells[A$cells$horizon == "all" & A$cells$fold %in% blocks, ]
cb <- B$cells[B$cells$horizon == "all" & B$cells$fold %in% blocks, ]

# The objective's estimand: median over blocks within country, then renormalised
# burden weights over the countries actually scored. Recomputed here (rather than
# taken from the score objects) so both arms are restricted to the same blocks.
S_of <- function(cells) {
     d <- cells[is.finite(cells$skill_persistence), ]
     if (!nrow(d)) return(list(S = NA_real_, per_iso = NULL))
     pi <- stats::aggregate(skill_persistence ~ iso_code, d, stats::median)
     pi <- merge(pi, W[, c("iso_code", "w")], by = "iso_code")
     pi$w <- pi$w / sum(pi$w)
     list(S = sum(pi$w * pi$skill_persistence), per_iso = pi)
}
sa <- S_of(ca); sb <- S_of(cb)
dS <- sa$S - sb$S

# ---- per-country deltas, on countries BOTH arms scored ----------------------
pj <- merge(sa$per_iso[, c("iso_code", "skill_persistence", "w")],
            sb$per_iso[, c("iso_code", "skill_persistence")],
            by = "iso_code", suffixes = c("_arm", "_inc"))
pj$delta <- pj$skill_persistence_arm - pj$skill_persistence_inc
pj$w <- pj$w / sum(pj$w)
dS_paired <- sum(pj$w * pj$delta)
dS_exNGA  <- { q <- pj[pj$iso_code != "NGA", ]; sum(q$w / sum(q$w) * q$delta) }

# ---- per-ORIGIN deltas -> the exact test ------------------------------------
# Per block: burden-weighted mean over the countries scored in that block (one
# cell per country per block, so no median is involved). This is the unit the
# exact sign test resamples, and it makes no distributional assumption -- which
# is why it is the one to believe at n = 6 selection origins.
per_origin <- do.call(rbind, lapply(blocks, function(b) {
     x <- merge(ca[ca$fold == b, c("iso_code", "skill_persistence")],
                cb[cb$fold == b, c("iso_code", "skill_persistence")],
                by = "iso_code", suffixes = c("_a", "_b"))
     if (!nrow(x)) return(NULL)
     x <- merge(x, W[, c("iso_code", "w")], by = "iso_code")
     x$w <- x$w / sum(x$w)
     data.frame(fold = b, n_iso = nrow(x),
                dS = sum(x$w * (x$skill_persistence_a - x$skill_persistence_b)))
}))
n_o   <- nrow(per_origin)
n_pos <- sum(per_origin$dS > 0)
exact_p <- if (n_o > 0L) stats::binom.test(n_pos, n_o, 0.5)$p.value else NA_real_

# ---- bootstrap over origins (the objective's estimand, resampled) -----------
set.seed(11L)
boot <- replicate(NB, {
     bb <- sample(blocks, length(blocks), replace = TRUE)
     f <- function(cells) {
          d <- do.call(rbind, lapply(seq_along(bb), function(i) {
               z <- cells[cells$fold == bb[i], ]; if (nrow(z)) { z$rep <- i; z } else NULL }))
          if (is.null(d)) return(NA_real_)
          S_of(d)$S
     }
     f(ca) - f(cb)
})
boot <- boot[is.finite(boot)]
bq <- if (length(boot) > 10L) {
     stats::quantile(boot, c(0.025, 0.5, 0.975), names = FALSE)
} else rep(NA_real_, 3)

# ---- report -----------------------------------------------------------------
cat("\n==============================================================\n")
cat(sprintf("%s:  %s  vs  %s   [obj v%d | %s | %s intervals]\n",
            if (FLOOR) "FIT-NOISE FLOOR" else "GATE", A$arm_id, B$arm_id,
            A$objective_version, A$mode, A$interval_mode))
cat("==============================================================\n")
cat(sprintf("blocks compared : %s  (n = %d)\n", paste(blocks, collapse = ","), length(blocks)))
cat(sprintf("S(%s) = %+.4f    S(%s) = %+.4f\n", A$arm_id, sa$S, B$arm_id, sb$S))
cat(sprintf("dS (all scored) = %+.4f   dS (paired countries) = %+.4f   dS exNGA = %+.4f\n",
            dS, dS_paired, dS_exNGA))

if (FLOOR) {
     cat("\n-- FLOOR (same arm, disjoint seed blocks) ---------------------\n")
     cat(sprintf("|dS|                    = %.4f   <-- every class-R delta must exceed this\n", abs(dS)))
     cat(sprintf("per-country |delta|     median %.4f   max %.4f (%s)\n",
                 stats::median(abs(pj$delta)), max(abs(pj$delta)),
                 pj$iso_code[which.max(abs(pj$delta))]))
     cat(sprintf("per-origin |dS|         median %.4f   max %.4f\n",
                 stats::median(abs(per_origin$dS)), max(abs(per_origin$dS))))
     cat("\nA per-COUNTRY claim must exceed the per-country floor, not the aggregate one:\n")
     print(data.frame(iso = pj$iso_code, abs_delta = round(abs(pj$delta), 4))[
           order(-abs(pj$delta)), ], row.names = FALSE)
     cat("\nRecord this in REGISTRY.tsv as status=NOISE-FLOOR with n_seeds, and quote it\n",
         "in every later comparison at the same seed count.\n", sep = "")
     quit(save = "no")
}

# A1: margin = max(protocol multiplicity term, the measured floor)
n_scored_arms <- tryCatch({
     r <- readLines(file.path(HERE, "REGISTRY.tsv"), warn = FALSE)
     sum(grepl("\tSCORED\t", r))
}, error = function(e) NA_integer_)
mult <- 0.01 + 0.002 * sqrt(max(1L, n_scored_arms, na.rm = TRUE))
floor_env <- suppressWarnings(as.numeric(Sys.getenv("PSI_FLOOR", "")))
margin <- if (is.finite(floor_env)) max(mult, floor_env) else mult

cat("\n-- GATES (PROTOCOL section 3) --------------------------------\n")
cat(sprintf("A1 margin       : %.4f  (multiplicity %.4f from %s scored arms%s)\n",
            margin, mult, ifelse(is.na(n_scored_arms), "?", n_scored_arms),
            if (is.finite(floor_env)) sprintf("; floor %.4f via PSI_FLOOR", floor_env)
            else "; NO FLOOR SUPPLIED -- set PSI_FLOOR"))
a1 <- is.finite(dS) && dS > margin
cat(sprintf("A1 dS > margin  : %+.4f > %.4f    %s\n", dS, margin, if (a1) "PASS" else "FAIL"))

a2b <- is.finite(bq[1]) && bq[1] > 0
cat(sprintf("A2 bootstrap    : median %+.4f  95%% CI [%+.4f, %+.4f]   %s\n",
            bq[2], bq[1], bq[3], if (a2b) "PASS" else "FAIL"))
a2e <- is.finite(exact_p) && exact_p < 0.10 && n_pos > n_o - n_pos
cat(sprintf("A2 exact test   : %d/%d origins positive, p = %.4f   %s   <-- believe this at n=%d\n",
            n_pos, n_o, exact_p, if (a2e) "PASS" else "FAIL", n_o))
a2 <- a2b && a2e
if (a2b != a2e)
     cat("   ** bootstrap and exact test DISAGREE. PROTOCOL 3 requires both; the weighted\n",
         "      statistic can be carried by a few high-weight countries while the per-origin\n",
         "      direction is random (B-CAL, wave 20: CI [+0.083,+0.275] but p = 0.40).\n", sep = "")

t10 <- pj[pj$iso_code %in% TOP10, ]
miss10 <- setdiff(TOP10, t10$iso_code)
if (length(miss10)) {
     a3 <- FALSE
     cat(sprintf("A3 guard        : CANNOT EVALUATE -- top-10 unpaired: %s   FAIL\n",
                 paste(miss10, collapse = ",")))
} else {
     worst <- min(t10$delta); wiso <- t10$iso_code[which.min(t10$delta)]
     a3 <- worst >= -0.02
     cat(sprintf("A3 guard        : worst top-10 %+.4f (%s)   %s\n", worst, wiso,
                 if (a3) "PASS" else "FAIL"))
     if (!a3 && is.finite(floor_env))
          cat(sprintf(paste0("   note: under a zero true effect P(some top-10 shows d < -0.02) ~ 0.998,",
                             " so for a class-R arm\n      read this as %s (|%.4f| vs per-arm floor %.4f).\n"),
                      if (abs(worst) > floor_env) "a REAL regression" else "WITHIN per-country noise",
                      worst, floor_env))
}

a6 <- isTRUE(A$beats_seasonal)
cat(sprintf("A6 beats seasonal: S_seasonal = %+.4f   %s\n",
            A$S_by_baseline$seasonal$S, if (a6) "PASS" else "FAIL"))
cat("A4 confirmation : not evaluated here (read ONCE, after a selection win)\n")
cat("A5 review       : human/maintainer lane\n")

cat("\n-- per-country deltas ----------------------------------------\n")
print(data.frame(iso = pj$iso_code, w = round(pj$w, 4),
                 arm = round(pj$skill_persistence_arm, 4),
                 inc = round(pj$skill_persistence_inc, 4),
                 delta = round(pj$delta, 4),
                 top10 = ifelse(pj$iso_code %in% TOP10, "*", ""))[order(-pj$w), ],
      row.names = FALSE)

cat("\n-- per-origin deltas -----------------------------------------\n")
print(per_origin, row.names = FALSE)

verdict <- if (a1 && a2 && a3 && a6) "PROCEED to A4 (confirmation)" else "REJECT"
cat(sprintf("\n==> %s\n", verdict))
if (!is.finite(floor_env))
     cat("    (no PSI_FLOOR supplied: for a class-R arm this verdict is NOT admissible --\n",
         "     PROTOCOL 3b requires the floor to be measured at this seed count first.)\n", sep = "")
