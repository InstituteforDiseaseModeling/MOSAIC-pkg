#!/usr/bin/env Rscript
# =============================================================================
# compare_wave2.R -- 72-cell (6 arms x 4 units x 3 cutoffs) comparison.
# Adds the LSTM follow-up arms to the wave-1 screen (compare.R):
#   p000  = main-production proxy (LSTM, old epoch rule)
#   p000r = p000 disjoint-seed replicate  -> F_L, the LSTM noise floor
#   prod  = LSTM + epoch fix (P000E)
#   n9    = branch defaults: D9b + N8 + epoch fix
#   nd / nd_rep = DLinear pair            -> F_D, the DLinear noise floor
# All contrasts CRN-paired per cell; negative = first-named arm better.
# =============================================================================
suppressMessages(library(MOSAIC))
root <- Sys.getenv("MOSAIC_ROOT", unset = path.expand("~/MOSAIC"))
OUT  <- file.path(root, "MOSAIC-pkg", "claude", "psi_ab", "out")
ARMS <- c("prod", "nd", "nd_rep", "p000", "p000r", "n9")
UNITS <- c("COD", "ETH", "MOZ", "NGA")
CUTOFFS <- c("2024-10-01", "2025-04-01", "2025-10-01")
cdir <- function(a, u, t) file.path(OUT, "per_arm", a, "per_unit", u, sprintf("cutoff_%s", t))

rows <- list()
for (a in ARMS) for (u in UNITS) for (t in CUTOFFS) {
     f <- file.path(cdir(a, u, t), "predictions.parquet")
     if (!file.exists(f)) next
     ev <- MOSAIC::evaluate_rolling_cv(
          predictions = as.data.frame(arrow::read_parquet(f)),
          horizons_months = c(1, 2, 3),
          baselines = c("seasonal", "persistence", "persistence_last"),
          metrics = c("cases", "deaths"),
          embargo_weeks = c(cases = 2L, deaths = 2L),
          ess_min = 50, min_cells_ci = 10L)
     d <- ev$cells; d$arm <- a; d$unit <- u; d$cutoff <- t
     rows[[length(rows) + 1L]] <- d
}
S <- do.call(rbind, rows)
arrow::write_parquet(S, file.path(OUT, "scores_cells_wave2.parquet"))

alb <- function(b) abs(log(pmax(b, 1e-3)))
wide <- function(win, met = "cases", mod = "ensemble") {
     d <- S[S$window == win & S$metric == met & S$model == mod,
            c("unit", "cutoff", "arm", "wis", "bias_ratio")]
     d$lw <- log(d$wis + 1); d$ab <- alb(d$bias_ratio)
     w <- reshape(d[, c("unit", "cutoff", "arm", "lw", "ab")],
                  idvar = c("unit", "cutoff"), timevar = "arm", direction = "wide")
     w[stats::complete.cases(w), ]
}
cr_se <- function(d, unit, cut) {
     n <- length(d); e <- d - mean(d)
     vg <- function(g) { G <- length(unique(g)); s <- tapply(e, g, sum)
                         (G / (G - 1)) * sum(s^2) / n^2 }
     viid <- sum(e^2) / n^2 * n / (n - 1)
     sqrt(max(vg(unit) + vg(cut) - viid, viid))
}
summ <- function(label, d, w) {
     se <- cr_se(d, w$unit, w$cutoff)
     cat(sprintf("  %-18s mean %+.3f (%+6.1f%%) CR-SE %.3f  better %2d/%d  [%s]\n",
                 label, mean(d), 100 * (exp(mean(d)) - 1), se, sum(d < 0), length(d),
                 paste(sprintf("%s %+.2f", names(tapply(d, w$unit, mean)),
                               tapply(d, w$unit, mean)), collapse = ", ")))
     invisible(c(mean = mean(d), se = se))
}

CONTRASTS <- list(
     "F_L p000r-p000" = c("p000r", "p000"),
     "F_D nd_rep-nd"  = c("nd_rep", "nd"),
     "epoch prod-p000" = c("prod", "p000"),
     "n9 - prod"      = c("n9", "prod"),
     "n9 - p000"      = c("n9", "p000"),
     "nd - prod"      = c("nd", "prod"),
     "nd - p000"      = c("nd", "p000"))

res <- list()
for (win in c("OOS<=3mo", "IS")) {
     w <- wide(win)
     cat(sprintf("\n=== %s  (%d paired cells, cases, ensemble) ===\n", win, nrow(w)))
     for (v in c("lw", "ab")) {
          cat(if (v == "lw") " log(WIS+1):\n" else " |log bias_ratio|:\n")
          for (nm in names(CONTRASTS)) {
               p <- CONTRASTS[[nm]]
               res[[win]][[v]][[nm]] <- summ(nm, w[[paste0(v, ".", p[1])]] - w[[paste0(v, ".", p[2])]], w)
          }
     }
}

# ---- propagation: best-subset overlap on the shared sim_id index -------------
ids <- list()
for (a in ARMS) for (u in UNITS) for (t in CUTOFFS) {
     sp <- list.files(file.path(cdir(a, u, t), "runs"), pattern = "^samples\\.parquet$",
                      recursive = TRUE, full.names = TRUE)
     if (!length(sp)) next
     s <- as.data.frame(arrow::read_parquet(sp[1], col_select = c("sim", "is_best_subset")))
     ids[[paste(a, u, t)]] <- s$sim[s$is_best_subset]
}
jac <- function(x, y) length(intersect(x, y)) / length(union(x, y))
cat("\n best-subset Jaccard (median over 12 cells):\n")
J <- sapply(CONTRASTS, function(p) median(unlist(lapply(UNITS, function(u) sapply(CUTOFFS, function(t)
     jac(ids[[paste(p[1], u, t)]], ids[[paste(p[2], u, t)]]))))))
print(round(J, 3))

saveRDS(list(S = S, res = res, J = J), file.path(OUT, "compare_wave2_results.rds"))
cat("\nwritten: scores_cells_wave2.parquet, compare_wave2_results.rds ->", OUT, "\n")
