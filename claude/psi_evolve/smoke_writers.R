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
# Deliberately tiny: 2 epochs, 1-2 seeds, a short fit window. It checks that the
# writers RUN and that the manifest carries its provenance -- not that the model
# is any good.
#
# v3 (PROTOCOL 5.6): the smoke now covers the two NEW fit-only paths as well --
# the trunk registry (`N` arms) and HA-02's epoch/ensemble decoupling. An `N` arm
# must be smoked WITH ITS TRUNK selected: a trunk that has never been built is
# not a registered arm.
#
# usage:
#   Rscript smoke_writers.R                    # all variants
#   SMOKE_VARIANTS=base,tcn Rscript smoke_writers.R
#   MOSAIC_ROOT=~/MOSAIC Rscript smoke_writers.R
# =============================================================================
suppressMessages(library(MOSAIC))
`%||%` <- function(a, b) if (is.null(a)) b else a

ROOT <- Sys.getenv("MOSAIC_ROOT", "")
if (!nzchar(ROOT)) {
     ROOT <- if (dir.exists("/home/jgiles/MOSAIC")) {
          "/home/jgiles/MOSAIC"
     } else path.expand("~/MOSAIC")
}
set_root_directory(ROOT)
cat("MOSAIC", as.character(packageVersion("MOSAIC")), "| root", ROOT, "\n")

VARIANTS <- strsplit(Sys.getenv("SMOKE_VARIANTS", "base,tcn,ha02"), ",")[[1]]

# variant -> (arch_control additions, extra provenance keys that must be non-NULL)
SPECS <- list(
  base = list(ac = list(n_seeds = 1L),
              need = character(0), note = "production trunk, one seed"),
  tcn  = list(ac = list(n_seeds = 1L, trunk = "tcn"),
              need = "trunk", note = "N-arm trunk swap (TCN)"),
  gru  = list(ac = list(n_seeds = 1L, trunk = "gru"),
              need = "trunk", note = "N-arm trunk swap (GRU)"),
  ha02 = list(ac = list(n_seeds = 2L, epoch_select_seeds = 1L),
              need = c("epoch_select_seeds", "epoch_fixed"),
              note = "HA-02 epoch/ensemble decoupling")
)

fail <- character(0)
for (v in VARIANTS) {
  if (!v %in% names(SPECS)) { fail <- c(fail, paste("unknown variant", v)); next }
  sp  <- SPECS[[v]]
  OUT <- file.path(tempdir(), paste0("psi_smoke_", v, "_", Sys.getpid()))
  dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
  PATHS <- get_paths(); PATHS$MODEL_INPUT <- OUT     # never touch production

  cat("\n---- variant:", v, "--", sp$note, "----\n")
  t0 <- Sys.time()
  ok <- tryCatch({
    est_suitability(
      PATHS,
      fit_date_start = "2020-01-01", fit_date_stop = "2022-01-01",
      feature_set    = "v7.3",
      architecture   = "lstm_v2_hierarchical_film",
      arch_control   = utils::modifyList(
                         list(parallel_seeds = 1L, epochs = 2L, rw_subsample = 12L),
                         sp$ac),
      bias_correct   = TRUE, verbose = FALSE)
    TRUE
  }, error = function(e) { cat("  FIT ERROR:", conditionMessage(e), "\n"); FALSE })
  if (!ok) { fail <- c(fail, paste0(v, ": fit errored")); unlink(OUT, recursive = TRUE); next }
  cat("  fit + write in", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s\n")

  for (f in c("pred_psi_suitability_day.csv", "pred_psi_suitability_week.csv",
              "data_psi_suitability.csv", "psi_suitability_config.json")) {
    e <- file.exists(file.path(OUT, f))
    cat(sprintf("  %-34s %s\n", f, if (e) "OK" else "MISSING"))
    if (!e) fail <- c(fail, paste0(v, ": ", f))
  }
  cfg <- tryCatch(jsonlite::read_json(file.path(OUT, "psi_suitability_config.json")),
                  error = function(e) NULL)
  if (is.null(cfg)) { fail <- c(fail, paste0(v, ": manifest unparseable")) } else {
    pv   <- cfg$provenance
    need <- c("source_csv", "source_csv_md5", "timesteps", "lead", "rw_gap_weeks",
              "n_rw_steps", "smooth_span", "ensemble_logit_eps", "mosaic_version",
              "written_at", sp$need)
    miss  <- need[!need %in% names(pv)]
    nullk <- need[vapply(need, function(k) is.null(pv[[k]]), logical(1))]
    cat("  provenance keys :", length(names(pv)),
        "| missing:", if (length(miss)) paste(miss, collapse = ",") else "<none>",
        "| NULL:", if (length(nullk)) paste(nullk, collapse = ",") else "<none>", "\n")
    cat("  trunk recorded  :", pv$trunk %||% "<none>", "\n")
    if (v == "ha02")
      cat("  epoch decoupling: selected from", pv$epoch_select_seeds %||% "?",
          "seed(s), all refit at epoch", pv$epoch_fixed %||% "?", "\n")
    if (length(miss) || length(nullk))
      fail <- c(fail, paste0(v, ": provenance ",
                             paste(unique(c(miss, nullk)), collapse = ",")))
  }
  unlink(OUT, recursive = TRUE)
}

if (length(fail)) { cat("\nSMOKE FAILED:", paste(fail, collapse = " | "), "\n"); quit(status = 1) }
cat("\nSMOKE PASSED -- psi write path, provenance, trunk registry and HA-02 all intact.\n")
