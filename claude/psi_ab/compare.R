#!/usr/bin/env Rscript
# =============================================================================
# compare.R -- full 36-cell comparison: treatment (prod vs nd) against the
# floor (nd vs nd_rep), IS and OOS, plus the psi_star compensation readout.
#
# Contrasts are CRN-paired cell by cell (identical sim_ids and parameter draws
# across arms; arms differ only in psi_jt).
#   T = nd - prod     the trunk effect
#   F = nd_rep - nd   same DLinear spec, different psi seed: the noise floor
# F is ANTI-CONSERVATIVE (captures DLinear fit noise only, not the LSTM's), so
# |T| inside |F| is a decisive negative; |T| outside |F| is not yet a positive.
# =============================================================================
suppressMessages(library(MOSAIC))
root <- Sys.getenv("MOSAIC_ROOT", unset = path.expand("~/MOSAIC"))
OUT  <- file.path(root, "MOSAIC-pkg", "claude", "psi_ab", "out")
ARMS <- c("prod", "nd", "nd_rep")
UNITS <- c("COD", "ETH", "MOZ", "NGA")
CUTOFFS <- c("2024-10-01", "2025-04-01", "2025-10-01")
cdir <- function(a, u, t) file.path(OUT, "per_arm", a, "per_unit", u, sprintf("cutoff_%s", t))

# ---- skill metrics -----------------------------------------------------------
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
arrow::write_parquet(S, file.path(OUT, "scores_cells.parquet"))

# |log bias| with a floor so a zero-prediction cell (bias 0) is finite rather
# than -Inf. 1e-3 = "predicted ~0.1% of observed", i.e. a dead forecast.
alb <- function(b) abs(log(pmax(b, 1e-3)))

wide <- function(win, met = "cases", mod = "ensemble") {
     d <- S[S$window == win & S$metric == met & S$model == mod,
            c("unit", "cutoff", "arm", "wis", "bias_ratio")]
     d$lw <- log(d$wis + 1); d$ab <- alb(d$bias_ratio)
     w <- reshape(d[, c("unit", "cutoff", "arm", "lw", "ab")],
                  idvar = c("unit", "cutoff"), timevar = "arm", direction = "wide")
     w[stats::complete.cases(w), ]
}

# Cluster-robust SE of a mean paired difference, two-way (unit, cutoff),
# floored at the iid variance (Cameron-Gelbach-Miller).
cr_se <- function(d, unit, cut) {
     n <- length(d); e <- d - mean(d)
     vg <- function(g) { G <- length(unique(g)); s <- tapply(e, g, sum)
                         (G / (G - 1)) * sum(s^2) / n^2 }
     viid <- sum(e^2) / n^2 * n / (n - 1)
     sqrt(max(vg(unit) + vg(cut) - viid, viid))
}

summ <- function(label, d, w) {
     se <- cr_se(d, w$unit, w$cutoff)
     cat(sprintf("  %-22s mean %+.3f  (%+5.1f%%)  CR-SE %.3f  sign %2d/%d  [unit means: %s]\n",
                 label, mean(d), 100 * (exp(mean(d)) - 1), se, sum(d < 0), length(d),
                 paste(sprintf("%s %+.2f", names(tapply(d, w$unit, mean)),
                               tapply(d, w$unit, mean)), collapse = ", ")))
     invisible(c(mean = mean(d), se = se))
}

cat("==================================================================\n")
cat("psi_ab -- 36 cells. T = nd - prod (trunk effect); F = nd_rep - nd (floor)\n")
cat("negative = nd/nd_rep better.  CRN-paired.  cases, model=ensemble\n")
cat("==================================================================\n")
res <- list()
for (win in c("OOS<=3mo", "IS")) {
     w <- wide(win)
     cat(sprintf("\n--- %s  (%d paired cells) ---\n", win, nrow(w)))
     cat(" log(WIS+1):\n")
     t_ <- summ("T  nd - prod",     w$lw.nd - w$lw.prod,     w)
     f_ <- summ("F  nd_rep - nd",   w$lw.nd_rep - w$lw.nd,   w)
     cat(sprintf("  => |T|/|F| = %.1f   (|T| %s the anti-conservative floor)\n",
                 abs(t_["mean"]) / max(abs(f_["mean"]), 1e-9),
                 if (abs(t_["mean"]) > abs(f_["mean"]) + 2 * f_["se"]) "OUTSIDE" else "inside"))
     cat(" |log bias_ratio|:\n")
     tb <- summ("T  nd - prod",     w$ab.nd - w$ab.prod,     w)
     fb <- summ("F  nd_rep - nd",   w$ab.nd_rep - w$ab.nd,   w)
     res[[win]] <- list(w = w, t = t_, f = f_, tb = tb, fb = fb)
}

# ---- psi_star compensation readout (the pre-registered primary) --------------
# Under CRN the psi_star draws are bit-identical across arms; only the weights
# differ. D_a = weighted mean |log a| over the best subset = how hard the gain
# had to work to make this psi usable.
cat("\n--- psi_star compensation (best-subset weighted posterior) ---\n")
comp <- list()
for (a in ARMS) for (u in UNITS) for (t in CUTOFFS) {
     sp <- list.files(file.path(cdir(a, u, t), "runs"), pattern = "^samples\\.parquet$",
                      recursive = TRUE, full.names = TRUE)
     if (!length(sp)) next
     s <- as.data.frame(arrow::read_parquet(sp[1]))
     col <- function(p) { k <- grep(sprintf("^psi_star_%s(_%s)?$", p, u), names(s), value = TRUE)
                          if (length(k)) s[[k[1]]] else NA_real_ }
     wcol <- intersect(c("weight_best", "weight"), names(s))[1]
     keep <- if ("is_best_subset" %in% names(s)) s$is_best_subset else rep(TRUE, nrow(s))
     wt <- s[[wcol]][keep]; wt <- wt / sum(wt)
     A <- col("a")[keep]; B <- col("b")[keep]; K <- col("k")[keep]; Z <- col("z")[keep]
     comp[[length(comp) + 1L]] <- data.frame(
          arm = a, unit = u, cutoff = t, nB = sum(keep),
          D_a = sum(wt * abs(log(A))), E_loga = sum(wt * log(A)),
          E_b = sum(wt * B), E_z = sum(wt * Z), E_k = sum(wt * K),
          ids = I(list(s$sim[keep])))
}
C <- do.call(rbind, comp)
agg <- aggregate(cbind(D_a, E_loga, E_b, E_z, E_k, nB) ~ arm, data = C, FUN = mean)
print(within(agg, { D_a <- round(D_a, 3); E_loga <- round(E_loga, 3); E_b <- round(E_b, 2)
                    E_z <- round(E_z, 3); E_k <- round(E_k, 1); nB <- round(nB) }), row.names = FALSE)

cw <- reshape(C[, c("arm", "unit", "cutoff", "D_a")], idvar = c("unit", "cutoff"),
              timevar = "arm", direction = "wide")
cat("\n D_a (lower = transform worked less hard):\n")
summ("T  nd - prod",   cw$D_a.nd - cw$D_a.prod,     cw)
summ("F  nd_rep - nd", cw$D_a.nd_rep - cw$D_a.nd,   cw)
cat("  (D_a is not log-scale; ignore the % column for this block)\n")

# ---- propagation: best-subset overlap on the shared sim_id index -------------
jac <- function(x, y) length(intersect(x, y)) / length(union(x, y))
J <- do.call(rbind, lapply(split(C, paste(C$unit, C$cutoff)), function(g) {
     get <- function(a) g$ids[[match(a, g$arm)]]
     data.frame(unit = g$unit[1], cutoff = g$cutoff[1],
                J_T = jac(get("nd"), get("prod")), J_F = jac(get("nd_rep"), get("nd")))
}))
cat(sprintf("\n best-subset Jaccard, median: T (nd vs prod) %.3f | F (nd_rep vs nd) %.3f | random ~%.3f\n",
            median(J$J_T), median(J$J_F), {b <- mean(C$nB); (b^2/5000) / (2*b - b^2/5000)}))

saveRDS(list(S = S, C = C, J = J, res = res), file.path(OUT, "compare_results.rds"))
cat("\nwritten: scores_cells.parquet, compare_results.rds ->", OUT, "\n")
