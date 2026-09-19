#!/usr/bin/env Rscript
# Collect finished arms, score them on the metric panel, and report PAIRED
# differences against the baseline on matching seeds (PROTOCOL.md 3.3).
# Usage: Rscript collect.R <lab_pkg_dir> <runs_root> [t_cut]
args <- commandArgs(trailingOnly = TRUE)
PKG  <- args[1]; ROOT <- args[2]
TCUT <- if (length(args) >= 3) args[3] else "2025-09-01"
suppressMessages(devtools::load_all(PKG, quiet = TRUE))
HERE <- dirname(normalizePath(sub("^--file=", "",
        grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(HERE, "holdout.R")); source(file.path(HERE, "score.R"))

cfg  <- get_location_config(iso = "ETH")
dirs <- list.dirs(ROOT, recursive = FALSE)
dirs <- dirs[grepl("_ETH_n[0-9]+_s[0-9]+$", basename(dirs))]
if (!length(dirs)) { cat("no finished runs under ", ROOT, "\n"); quit(save = "no") }

rows <- do.call(rbind, lapply(dirs, function(d) {
     b <- basename(d)
     arm  <- sub("_ETH_n[0-9]+_s[0-9]+$", "", b)
     seed <- as.integer(sub(".*_s([0-9]+)$", "\\1", b))
     s <- tryCatch(score_run(d, cfg, t_cut = TCUT), error = function(e) NULL)
     if (is.null(s)) return(NULL)
     g <- tryCatch(inference_diagnostics(d), error = function(e) NULL)
     s$arm <- arm; s$seed <- seed
     s$ess_is_all <- if (!is.null(g)) g$ess_is_all else NA_real_
     s$khat_all   <- if (!is.null(g)) g$khat_all   else NA_real_
     s$n_best     <- if (!is.null(g)) g$n_best_subset else NA_real_
     s
}))
if (is.null(rows)) { cat("no scorable runs\n"); quit(save = "no") }

cat("\n================ PER-RUN ================\n")
key <- c("arm","seed","window","r2_cases","bias_cases","r2_deaths","bias_deaths")
print(rows[rows$window %in% c("all","train","oos_h1","oos_h2","oos_h3","oos_all"), key],
      row.names = FALSE, digits = 4)

cat("\n================ ARM MEANS (across seeds) ================\n")
agg <- aggregate(cbind(r2_cases, bias_cases, r2_deaths, bias_deaths) ~ arm + window,
                 data = rows, FUN = function(x) mean(x, na.rm = TRUE))
print(agg[agg$window %in% c("all","train","oos_h1","oos_h3","oos_all"), ], row.names = FALSE, digits = 4)

cat("\n================ PAIRED vs BASELINE (same seeds) ================\n")
base <- rows[rows$arm == "baseline", ]
if (!nrow(base)) { cat("  no baseline yet\n") } else {
 for (a in setdiff(unique(rows$arm), "baseline")) {
  arm <- rows[rows$arm == a, ]
  m <- merge(arm, base, by = c("window","seed"), suffixes = c("", "_b"))
  if (!nrow(m)) next
  cat(sprintf("\n--- %s vs baseline (%d paired seed-windows) ---\n", a, length(unique(m$seed))))
  for (w in c("all","train","oos_h1","oos_h2","oos_h3","oos_all")) {
     mm <- m[m$window == w, ]; if (!nrow(mm)) next
     f <- function(x, xb) { d <- x - xb; c(mean(d, na.rm=TRUE), stats::sd(d, na.rm=TRUE)) }
     dr <- f(mm$r2_cases, mm$r2_cases_b);  db <- f(mm$bias_cases, mm$bias_cases_b)
     dr2<- f(mm$r2_deaths, mm$r2_deaths_b);db2<- f(mm$bias_deaths, mm$bias_deaths_b)
     gate <- function(d, s, better_is) {
        if (!is.finite(d) || !is.finite(s) || s == 0) return("n/a")
        ok <- if (better_is == "up") d > 2*s else d < -2*s
        sprintf("%+.4f+-%.4f %s", d, s, if (ok) "PASS(>2SD)" else "")
     }
     cat(sprintf("  %-8s r2_cases %-26s bias_cases %-26s r2_deaths %-26s bias_deaths %s\n", w,
        gate(dr[1],dr[2],"up"), gate(db[1],db[2],"down"),
        gate(dr2[1],dr2[2],"up"), gate(db2[1],db2[2],"down")))
  }
  cat(sprintf("  ess_is: %s vs %s | khat: %s vs %s\n",
     paste(signif(unique(arm$ess_is_all),4), collapse=","),
     paste(signif(unique(base$ess_is_all),4), collapse=","),
     paste(signif(unique(arm$khat_all),4), collapse=","),
     paste(signif(unique(base$khat_all),4), collapse=",")))
 }
}
saveRDS(rows, file.path(ROOT, "inflab_scores.rds"))
cat("\nsaved: ", file.path(ROOT, "inflab_scores.rds"), "\n")
