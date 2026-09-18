# =============================================================================
# smoke_writers.R -- exercise the psi WRITE path end to end, fast.
#
# WHY. The manifest/prediction writers are reached only by a real fit, so no unit
# test touches them. A `backend` reference that exists only on another branch
# survived a field-name test and killed 9 arm shards at the END of their first
# cutoff, discarding ~13 dugong-hours. findGlobals now catches unbound names, but
# it cannot catch a runtime failure inside the provenance block (a missing file
# for md5sum, a NULL that jsonlite rejects). This does.
#
# Deliberately tiny: 2 epochs, 1 seed, a short fit window. It is checking that
# the writers RUN and that the manifest carries its provenance -- not that the
# model is any good.
# =============================================================================
suppressMessages(library(MOSAIC))
OUT <- file.path(tempdir(), paste0("psi_smoke_", Sys.getpid()))
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
set_root_directory("/home/jgiles/MOSAIC")
PATHS <- get_paths(); PATHS$MODEL_INPUT <- OUT      # never touch production

cat("MOSAIC", as.character(packageVersion("MOSAIC")), "| MODEL_INPUT ->", OUT, "\n")
t0 <- Sys.time()
res <- est_suitability(
  PATHS,
  fit_date_start = "2020-01-01", fit_date_stop = "2022-01-01",
  feature_set    = "v7.3",
  architecture   = "lstm_v2_hierarchical_film",
  arch_control   = list(n_seeds = 1L, parallel_seeds = 1L, epochs = 2L,
                        rw_subsample = 12L),
  bias_correct   = TRUE, verbose = FALSE)
cat("fit + write completed in", round(as.numeric(difftime(Sys.time(), t0, units="secs"))), "s\n\n")

fail <- character(0)
for (f in c("pred_psi_suitability_day.csv", "pred_psi_suitability_week.csv",
            "data_psi_suitability.csv", "psi_suitability_config.json")) {
  ok <- file.exists(file.path(OUT, f))
  cat(sprintf("  %-34s %s\n", f, if (ok) "OK" else "MISSING"))
  if (!ok) fail <- c(fail, f)
}
cfg <- tryCatch(jsonlite::read_json(file.path(OUT, "psi_suitability_config.json")),
                error = function(e) NULL)
if (is.null(cfg)) { fail <- c(fail, "manifest unparseable") } else {
  pv <- cfg$provenance
  need <- c("source_csv","source_csv_md5","timesteps","lead","rw_gap_weeks",
            "n_rw_steps","smooth_span","ensemble_logit_eps","mosaic_version","written_at")
  miss <- need[!need %in% names(pv)]
  nullk <- need[vapply(need, function(k) is.null(pv[[k]]), logical(1))]
  cat("\n  provenance keys present:", length(names(pv)), "\n")
  cat("  required missing:", if (length(miss)) paste(miss, collapse=", ") else "<none>", "\n")
  cat("  required NULL   :", if (length(nullk)) paste(nullk, collapse=", ") else "<none>", "\n")
  cat("  md5 recorded    :", substr(pv$source_csv_md5 %||% "NA", 1, 12), "\n")
  cat("  mosaic_version  :", pv$mosaic_version %||% "NA", "\n")
  if (length(miss)) fail <- c(fail, paste("missing:", paste(miss, collapse=",")))
}
unlink(OUT, recursive = TRUE)
if (length(fail)) { cat("\nSMOKE FAILED:", paste(fail, collapse=" | "), "\n"); quit(status = 1) }
cat("\nSMOKE PASSED -- the psi write path and its provenance are intact.\n")
