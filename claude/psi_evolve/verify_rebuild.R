# =============================================================================
# verify_rebuild.R -- did the panel rebuild change ONLY what it should have?
#
# A rebuild can differ from the frozen panel for reasons other than the new
# surveillance data: a changed default, a refreshed covariate source, a schema
# drift. This diffs the new panel against the archived one and separates
# EXPECTED changes from UNEXPECTED ones.
#
# EXPECTED
#   * new rows at the tail (later dates with observed cases)
#   * target_D_rate_per_country_floored rescaled for RWA (+114% cp99r) and NGA
#     (+71%), and <3% elsewhere -- the period-normalisation effect
#   * covariates refreshed where their upstream file was refreshed
# UNEXPECTED (investigate before trusting the rebuild)
#   * a different column set
#   * target changes in countries whose cp99r did NOT move
#   * changes to rows BEFORE the frozen panel's last date in a country with no
#     new weeks
#
# usage: Rscript verify_rebuild.R
# =============================================================================
NEW    <- "/Users/johngiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
FROZEN <- "/Users/johngiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data_frozen_2026-09-17.csv"
TV     <- "target_D_rate_per_country_floored"
POOLF  <- "/Users/johngiles/MOSAIC/MOSAIC-pkg/claude/psi_evolve/weights_frozen.csv"

stopifnot(file.exists(NEW), file.exists(FROZEN))
a <- utils::read.csv(FROZEN, stringsAsFactors = FALSE)
b <- utils::read.csv(NEW,    stringsAsFactors = FALSE)
pool <- utils::read.csv(POOLF, stringsAsFactors = FALSE)$iso_code

cat("=== 1. SCHEMA ===\n")
cat(sprintf("frozen: %s rows x %d cols | new: %s rows x %d cols\n",
            format(nrow(a), big.mark=","), ncol(a),
            format(nrow(b), big.mark=","), ncol(b)))
only_a <- setdiff(names(a), names(b)); only_b <- setdiff(names(b), names(a))
if (!length(only_a) && !length(only_b)) cat("column sets IDENTICAL -- expected\n") else {
     cat("*** COLUMN SET CHANGED -- UNEXPECTED ***\n")
     if (length(only_a)) cat("  dropped:", paste(only_a, collapse=", "), "\n")
     if (length(only_b)) cat("  added  :", paste(only_b, collapse=", "), "\n")
}

a$date <- as.Date(a$date); b$date <- as.Date(b$date)
cat(sprintf("\nfrozen target last date: %s | new: %s\n",
            max(a$date[is.finite(a[[TV]])]), max(b$date[is.finite(b[[TV]])])))

cat("\n=== 2. TARGET CHANGE PER POOL COUNTRY (on SHARED dates only) ===\n")
cat("A country with no new weeks should show ~0 change. Anything else is a\n")
cat("covariate/default drift rather than the period-normalisation effect.\n\n")
cat(sprintf("%-5s %8s %10s %10s %9s %s\n",
            "iso","new_wks","med|rel|","max|rel|","cor","verdict"))
for (iso in pool) {
     aa <- a[a$iso_code == iso, c("date", TV)]
     bb <- b[b$iso_code == iso, c("date", TV)]
     m  <- merge(aa, bb, by = "date", suffixes = c("_old","_new"))
     m  <- m[is.finite(m[[paste0(TV,"_old")]]) & is.finite(m[[paste0(TV,"_new")]]), ]
     nw <- sum(is.finite(bb[[TV]]) & bb$date > max(aa$date[is.finite(aa[[TV]])]))
     if (!nrow(m)) { cat(sprintf("%-5s %8d %10s %10s %9s %s\n", iso, nw,"-","-","-","no shared rows")); next }
     o <- m[[paste0(TV,"_old")]]; n <- m[[paste0(TV,"_new")]]
     rel <- ifelse(o > 0, abs(n - o)/o, NA_real_)
     md <- stats::median(rel, na.rm=TRUE); mx <- max(rel, na.rm=TRUE)
     cc <- suppressWarnings(stats::cor(o, n))
     # VERDICT ON CORRELATION, not the median. The first version of this check
     # keyed on median |rel| < 0.01 and called RWA "unchanged" while its MAX
     # relative change was 30.7x -- because RWA's target sits near zero, so the
     # median cannot see a few large revisions. Correlation is the metric that
     # detects a materially different SERIES; the median only detects a shift in
     # the bulk. Report both, decide on cor.
     verdict <- if (is.finite(cc) && cc < 0.99) "*** SERIES CHANGED (cor < 0.99) ***" else
                if (is.finite(md) && md > 0.02) "rescaled (ordering kept)" else
                "unchanged"
     cat(sprintf("%-5s %8d %10.4f %10.4f %9.4f %s\n", iso, nw, md, mx, cc, verdict))
}

cat("\n=== 3. NEW ROWS GAINED PER POOL COUNTRY ===\n")
gain <- vapply(pool, function(iso) {
     aa <- a[a$iso_code == iso, ]; bb <- b[b$iso_code == iso, ]
     la <- suppressWarnings(max(aa$date[is.finite(aa[[TV]])]))
     if (!is.finite(la)) return(NA_integer_)
     sum(is.finite(bb[[TV]]) & bb$date > la)
}, integer(1))
print(sort(gain[is.finite(gain)], decreasing = TRUE))
cat(sprintf("\ntotal new pool country-weeks: %d\n", sum(gain, na.rm=TRUE)))
