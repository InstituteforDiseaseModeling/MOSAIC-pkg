#!/usr/bin/env Rscript
# =============================================================================
# peek.R -- INTERIM, UNBLINDED-EARLY look at prod vs nd on completed cells.
#
# THIS IS NOT THE ANALYSIS. The pre-registered design requires the nd_rep
# floor arm, which measures how much two DLinear psi fits differing only in
# seed move the downstream result. Until that exists there is no way to tell a
# real effect from a lucky psi fit -- the exact failure that has already killed
# three marginal claims in this programme (NDe's entire headline was erased by
# its own replicate NDeR).
#
# Read this as a smoke test of the pipeline and a rough magnitude, nothing more.
# =============================================================================
suppressMessages(library(MOSAIC))
root <- Sys.getenv("MOSAIC_ROOT", unset = path.expand("~/MOSAIC"))
OUT  <- file.path(root, "MOSAIC-pkg", "claude", "psi_ab", "out")

UNITS   <- c("COD", "ETH", "MOZ", "NGA")
CUTOFFS <- c("2024-10-01", "2025-04-01", "2025-10-01")

cell <- function(arm, u, t)
     file.path(OUT, "per_arm", arm, "per_unit", u, sprintf("cutoff_%s", t),
               "predictions.parquet")

score_one <- function(arm, u, t) {
     f <- cell(arm, u, t)
     if (!file.exists(f)) return(NULL)
     pu <- as.data.frame(arrow::read_parquet(f))
     ev <- MOSAIC::evaluate_rolling_cv(
          predictions = pu, horizons_months = c(1, 2, 3),
          baselines = c("seasonal", "persistence", "persistence_last"),
          metrics = c("cases", "deaths"),
          embargo_weeks = c(cases = 2L, deaths = 2L),
          ess_min = 50, min_cells_ci = 10L)
     d <- ev$cells
     d$arm <- arm; d$unit <- u; d$cutoff <- t
     d
}

rows <- list()
for (a in c("prod", "nd")) for (u in UNITS) for (t in CUTOFFS) {
     r <- score_one(a, u, t); if (!is.null(r)) rows[[length(rows) + 1L]] <- r
}
S <- do.call(rbind, rows)

# Pre-registered primary cell: model=ensemble, metric=cases, window=OOS<=3mo.
pick <- function(win, met = "cases", mod = "ensemble")
     S[S$window == win & S$metric == met & S$model == mod, ]

paired <- function(win, met = "cases") {
     d <- pick(win, met)
     w <- reshape(d[, c("unit", "cutoff", "arm", "wis", "bias_ratio", "R2_corr")],
                  idvar = c("unit", "cutoff"), timevar = "arm", direction = "wide")
     w <- w[stats::complete.cases(w[, c("wis.prod", "wis.nd")]), ]
     if (!nrow(w)) return(NULL)
     w$d_logwis  <- log(w$wis.nd + 1) - log(w$wis.prod + 1)
     w$d_absbias <- abs(log(w$bias_ratio.nd)) - abs(log(w$bias_ratio.prod))
     w
}

cat("=========================================================\n")
cat("INTERIM PEEK -- prod (LSTM) vs nd (DLinear).  NO FLOOR ARM YET.\n")
cat("=========================================================\n\n")

for (win in c("OOS<=3mo", "IS")) {
     w <- paired(win)
     if (is.null(w)) { cat(win, ": no paired cells yet\n\n"); next }
     cat("--- window:", win, "| metric: cases | model: ensemble ---\n")
     cat(sprintf("paired cells: %d  (units: %s)\n", nrow(w),
                 paste(sort(unique(w$unit)), collapse = ",")))
     print(data.frame(unit = w$unit, cutoff = w$cutoff,
                      wis_prod = round(w$wis.prod, 2), wis_nd = round(w$wis.nd, 2),
                      d_logwis = round(w$d_logwis, 4),
                      bias_prod = round(w$bias_ratio.prod, 3),
                      bias_nd = round(w$bias_ratio.nd, 3),
                      d_absbias = round(w$d_absbias, 4), row.names = NULL))
     cat(sprintf("\nmean d_logWIS  = %+.4f   (negative favours ND; %.1f%% WIS change)\n",
                 mean(w$d_logwis), 100 * (exp(mean(w$d_logwis)) - 1)))
     cat(sprintf("ND better in    %d / %d cells\n", sum(w$d_logwis < 0), nrow(w)))
     cat(sprintf("mean d|log bias| = %+.4f   (negative favours ND)\n",
                 mean(w$d_absbias)))
     cat(sprintf("ND better bias  %d / %d cells\n\n", sum(w$d_absbias < 0), nrow(w)))
}
