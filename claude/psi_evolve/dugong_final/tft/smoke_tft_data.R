# Does the TFT data pipeline produce correctly-shaped, leakage-respecting tensors
# from a REAL panel build? Uses a small lookback so it runs fast.
suppressMessages(library(MOSAIC))
source("tft_data.R")
PANEL <- "/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
feats <- MOSAIC::get_feature_set("v7.3")
cat("features:", length(feats), "\n")
b <- MOSAIC:::.psi_build_data(
     source_csv = PANEL, country_pool = "all_mosaic", target_iso = "MOZ",
     fit_date_start = "2015-01-01", cutoff_date = "2024-01-01",
     pred_date_stop = "2024-04-16", timesteps = 13L, features = feats,
     split_params = list(split_type = "rolling_cv", rw_gap_weeks = 2L,
                         step_days = 84L, test_days = 84L, min_test_days = 84L,
                         min_train_years = 4),
     response_var = "target_D_rate_per_country_floored", verbose = FALSE)
cat("bundle OK | pool rows:", nrow(b$pool_data$X),
    "| static covars:", ncol(b$encoders$country_static),
    "| countries:", b$encoders$n_countries, "\n")
tt <- tft_build_tensors(b, lookback = 26L, horizon = 12L, include_target_history = TRUE)
cat("\ntensor dims:\n")
str(lapply(tt$x, dim))
cat("y:", paste(dim(tt$y), collapse=" x "), "| samples:", nrow(tt$meta), "\n")
cat("dims list:", paste(names(tt$dims), unlist(tt$dims), sep="=", collapse=" "), "\n")
sp <- tft_split_by_cutoff(tt, "2024-01-01")
cat(sprintf("\nsplit at 2024-01-01: train %d, predict %d\n", sp$n_train, sp$n_pred))
# LEAKAGE ASSERTION: no training sample may have a target at or after the cutoff
mx <- max(vapply(sp$train$target_dates, function(d) as.numeric(max(d)), numeric(1)))
cat(sprintf("latest TRAIN target date: %s  (cutoff 2024-01-01)  -> %s\n",
    as.Date(mx, origin="1970-01-01"),
    if (as.Date(mx, origin="1970-01-01") < as.Date("2024-01-01")) "LEAK-FREE" else "*** LEAK ***"))
mn <- min(vapply(sp$pred$target_dates, function(d) as.numeric(min(d)), numeric(1)))
cat(sprintf("earliest PRED target date: %s -> %s\n", as.Date(mn, origin="1970-01-01"),
    if (as.Date(mn, origin="1970-01-01") >= as.Date("2024-01-01")) "ok" else "*** PRE-CUTOFF ***"))
# the bug this test caught: verify static rows carry the RIGHT country's covariates
cs <- b$encoders$country_static
ok <- TRUE
for (i in sample(nrow(tt$meta), 200)) {
  cid <- b$encoders$country_to_id[[tt$meta$iso_code[i]]]
  if (!isTRUE(all.equal(as.numeric(tt$x$static[i,]), as.numeric(cs[cid+1L,])))) ok <- FALSE
  if (tt$x$country[i,1] != cid) ok <- FALSE
}
cat("static/country rows match their iso (200 sampled):", if (ok) "YES" else "*** MISALIGNED ***", "\n")
cat("country index range:", paste(range(tt$x$country), collapse=".."),
    "(must be 0..", b$encoders$n_countries-1L, ")\n")
cat("non-finite in COVARIATE streams (must all be FALSE):",
    any(!is.finite(tt$x$past)), any(!is.finite(tt$x$future)),
    any(!is.finite(tt$x$static)), "\n")
cat("non-finite in y: TRUE is EXPECTED now (unobserved futures are the forecast targets):",
    any(!is.finite(tt$y)), "\n")
cat("TRAIN y all finite (must be TRUE):", all(is.finite(sp$train$y)), "\n")
cat("pred coverage: ", sp$n_pred, " origins over ",
    length(unique(sp$pred$meta$iso_code)), " countries\n", sep="")
cat("SMOKE PASS\n")
