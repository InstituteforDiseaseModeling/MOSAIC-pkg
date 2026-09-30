# =============================================================================
# summarise.R -- turn a benchmark ledger into a comparison table.
#
#   Rscript inst/bench/summarise.R <ledger.csv> [baseline_arm]
#
# Comparisons are PAIRED WITHIN BLOCK. Pooling all of arm A against all of
# arm B would charge any drift between the two runs to the arm difference;
# pairing inside a block, where both arms were measured minutes apart under
# the same conditions, does not.
# =============================================================================
a <- commandArgs(TRUE)
f <- if (length(a)) a[1] else "bench_matrix.csv"
base_arm <- if (length(a) > 1) a[2] else "main"
d <- utils::read.csv(f, stringsAsFactors = FALSE)

num <- function(x) suppressWarnings(as.numeric(x))
w <- subset(d, metric == "wall_min_s"); w$v <- num(w$value)

arms <- unique(w$arm); arms <- c(base_arm, setdiff(arms, base_arm))
cat(sprintf("\nledger: %s   rows: %d   arms: %s   blocks: %s\n",
            f, nrow(d), paste(arms, collapse = ", "),
            paste(sort(unique(w$block)), collapse = ",")))
cat(sprintf("config md5: %s\n", paste(unique(d$config_md5), collapse = " / ")))

# --- machine stability -------------------------------------------------------
cal <- subset(w, workload == "machine/calibrator")
if (nrow(cal)) {
  cat(sprintf("\nmachine calibrator (version-independent): min %.4f  max %.4f  spread %.1f%%\n",
              min(cal$v), max(cal$v), 100 * (max(cal$v) / min(cal$v) - 1)))
  if (max(cal$v) / min(cal$v) - 1 > 0.10)
    cat("  WARNING: >10% drift across the session -- treat cross-arm deltas with caution\n")
}

# --- per-workload table ------------------------------------------------------
cat("\n== wall_min_s by workload x arm (paired per block) ==\n")
wls <- setdiff(unique(w$workload), "machine/calibrator")
for (wl in wls) {
  s <- subset(w, workload == wl)
  if (!nrow(s)) next
  cat(sprintf("\n%s\n", wl))
  agg <- sapply(arms, function(ar) { v <- s$v[s$arm == ar]; if (length(v)) min(v) else NA })
  for (ar in arms) {
    v <- s$v[s$arm == ar]
    if (!length(v)) { cat(sprintf("  %-7s  (absent)\n", ar)); next }
    cat(sprintf("  %-7s  min %7.4f  median %7.4f  n=%d\n", ar, min(v), stats::median(v), length(v)))
  }
  # paired ratios vs baseline, block by block
  for (ar in setdiff(arms, base_arm)) {
    bl <- intersect(s$block[s$arm == base_arm], s$block[s$arm == ar])
    if (!length(bl)) next
    r <- sapply(bl, function(b)
      min(s$v[s$arm == base_arm & s$block == b]) / min(s$v[s$arm == ar & s$block == b]))
    cat(sprintf("    %s/%s = %.2fx  [per-block %.2f-%.2f, n=%d]\n",
                base_arm, ar, mean(r), min(r), max(r), length(r)))
  }
}

# --- correctness -------------------------------------------------------------
cat("\n== correctness ==\n")
dg <- subset(d, metric == "digest")
cs <- subset(d, metric == "cases_sum"); cs$v <- num(cs$value)
for (wl in unique(dg$workload)) {
  g <- subset(dg, workload == wl); c2 <- subset(cs, workload == wl)
  per <- tapply(g$value, g$arm, function(x) length(unique(x)))
  rbits <- setdiff(unique(g$arm), "main")
  same <- if (length(rbits) == 2)
    identical(unique(g$value[g$arm == rbits[1]]), unique(g$value[g$arm == rbits[2]])) else NA
  sums <- tapply(c2$v, c2$arm, function(x) mean(x))
  sp <- if (length(sums) > 1) 100 * (max(sums) / min(sums) - 1) else NA
  cat(sprintf("  %-22s R-engine digests identical: %-5s | cases_sum spread across arms: %.2f%%\n",
              wl, as.character(same), sp))
}

# --- calibration -------------------------------------------------------------
cal2 <- subset(d, workload == "calib/fixed-small" &
                  metric %in% c("wall_min_s", "sims_per_sec", "sims_total", "sims_retained", "n_shards"))
if (nrow(cal2)) {
  cat("\n== calib/fixed-small ==\n")
  for (ar in arms) {
    s <- subset(cal2, arm == ar); if (!nrow(s)) next
    g <- function(m) { x <- s$value[s$metric == m]; if (length(x)) x[1] else "-" }
    cat(sprintf("  %-7s wall %8.2fs  sims/sec %-8s retained %-6s of %-6s shards %s\n",
                ar, num(g("wall_min_s")), g("sims_per_sec"), g("sims_retained"),
                g("sims_total"), g("n_shards")))
  }
}

# --- parallel ----------------------------------------------------------------
pp <- subset(d, workload == "parallel/throughput" & grepl("^sims_per_sec_k", metric))
if (nrow(pp)) {
  cat("\n== parallel/throughput (sims/sec through the real worker) ==\n")
  ks <- sort(unique(as.integer(sub("sims_per_sec_k", "", pp$metric))))
  cat(sprintf("  %-7s %s\n", "arm", paste(sprintf("k=%-8d", ks), collapse = "")))
  for (ar in arms) {
    s <- subset(pp, arm == ar); if (!nrow(s)) next
    v <- sapply(ks, function(k) { x <- num(s$value[s$metric == paste0("sims_per_sec_k", k)]); if (length(x)) x[1] else NA })
    cat(sprintf("  %-7s %s\n", ar, paste(sprintf("%-10.3f", v), collapse = "")))
  }
}
cat("\n")
