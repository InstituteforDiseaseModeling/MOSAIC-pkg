# Is dir_acc ~= 0.5 a property of the MODEL, or of the LOESS SMOOTHER?
#
# psi is LOESS-smoothed on the logit scale over the DAILY grid with span 0.025
# (inst/fixtures/B4_rolling_cv_spec.yml:94). The grid spans 2018-01-01..2027-02-04,
# ~3320 days, so the local support is ~83 days. A 13-week scored block is 91 days
# -- so psi is approximately ONE local quadratic per block and sign(diff(psi)) can
# change at most twice, while weekly surveillance flips 5-6 times. Any sufficiently
# smooth predictor is then pinned near dir_acc 0.5 REGARDLESS OF SKILL.
#
# pred_raw (un-LOESS'd cross-seed logit-median) and pred_smooth are in every psi
# CSV and were never scored. If dir_acc(pred_raw) >> dir_acc(psi), the programme's
# central finding is an artifact of post-processing, not a model failure.
suppressMessages(library(MOSAIC))
HERE  <- "/home/jgiles/psi_evolve"
CANON <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
VAR   <- "target_D_rate_per_country_floored"
PROD  <- "/home/jgiles/MOSAIC/MOSAIC-pkg/claude/forecast_cv_ocv4_q2yr/psi_cache"
source(file.path(HERE, "shape_metrics.R"))

grid <- utils::read.csv(file.path(HERE,"EVAL_GRID.csv"), stringsAsFactors=FALSE)
grid <- grid[grid$grid=="prod" & grid$split=="selection", ]
for (k in c("cutoff","test_start","test_end")) grid[[k]] <- as.Date(grid[[k]])
W <- utils::read.csv(file.path(HERE,"weights_frozen.csv"), stringsAsFactors=FALSE)
W$w <- W$w_sqrt/sum(W$w_sqrt); pool <- W$iso_code
wts <- stats::setNames(W$w, W$iso_code)
obs <- utils::read.csv(CANON, stringsAsFactors=FALSE)[, c("iso_code","date",VAR)]
names(obs)[3] <- "observed"; obs$date <- as.Date(obs$date)
obs <- obs[is.finite(obs$observed) & obs$iso_code %in% pool, ]

arms <- c(P001=PROD)
for (a in c("P000","N8","D9b","F3")) {
  d <- file.path(HERE, paste0("psi_cache_", a)); if (dir.exists(d)) arms[a] <- d
}
COLS <- c("psi","pred_smooth","pred_raw")
cat(sprintf("%-6s %-13s %8s %9s %9s %8s\n","arm","column","MAE","dir_acc","dcor","sd_ratio"))
cat(strrep("-",60),"\n")
for (a in names(arms)) {
  rows <- list()
  for (i in seq_len(nrow(grid))) {
    f <- file.path(arms[[a]], sprintf("psi_%s.csv", format(grid$cutoff[i])))
    if (!file.exists(f)) next
    p <- utils::read.csv(f, stringsAsFactors=FALSE); p$date <- as.Date(p$date)
    keep <- intersect(COLS, names(p))
    p <- p[p$iso_code %in% pool & p$date >= grid$test_start[i] & p$date <= grid$test_end[i],
           c("iso_code","date",keep)]
    m <- merge(p, obs, by=c("iso_code","date")); if (!nrow(m)) next
    m$fold <- format(grid$cutoff[i]); rows[[length(rows)+1L]] <- m
  }
  if (!length(rows)) next
  D <- do.call(rbind, rows)
  for (cc in intersect(COLS, names(D))) {
    z <- D[is.finite(D[[cc]]), ]
    sm <- .shape_metrics(z, cc, wts, "fold")
    per <- split(seq_len(nrow(z)), paste(z$iso_code, z$fold, sep="\r"))
    w <- vapply(per, function(ix) wts[[z$iso_code[ix[1]]]], numeric(1))
    mae <- stats::weighted.mean(vapply(per, function(ix)
             mean(abs(z$observed[ix]-z[[cc]][ix])), numeric(1)), w)
    cat(sprintf("%-6s %-13s %8.4f %9.3f %9s %8.2f\n", a, cc, mae,
        sm[["dir_acc"]],
        if (is.finite(sm[["dcor"]])) sprintf("%.3f", sm[["dcor"]]) else "undef",
        sm[["sd_ratio"]]))
  }
}
cat("\nIf dir_acc(pred_raw) is materially above dir_acc(psi), the coin-flip finding\n")
cat("is a SMOOTHER artifact and the fix is post-processing, not architecture.\n")
