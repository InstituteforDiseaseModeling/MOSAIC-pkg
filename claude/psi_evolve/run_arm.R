# =============================================================================
# run_arm.R -- fit one arm's psi across the FROZEN evaluation grid.
#
# One psi fit per evaluation cutoff (rolling-origin), via the package's own
# prefit_rolling_cv_psi(), so each scored block is predicted by a model that saw
# no data past its cutoff. Writes a per-cutoff psi CSV cache; scoring is a
# separate step (score_psi_arm.R) so a scoring bug never costs a refit.
#
# env: PSI_ARM (id), PSI_SMOKE (1 = 2 cutoffs x 1 seed), PSI_SEEDS, PSI_CORES
# =============================================================================
suppressMessages(library(MOSAIC))
ARM    <- Sys.getenv("PSI_ARM", "A000")
SMOKE  <- Sys.getenv("PSI_SMOKE", "0") == "1"
NSEED  <- as.integer(Sys.getenv("PSI_SEEDS", "3"))
HERE   <- "/home/jgiles/psi_evolve"
dir.create(HERE, showWarnings = FALSE, recursive = TRUE)

set_root_directory("/home/jgiles/MOSAIC")
PATHS <- get_paths()

grid <- utils::read.csv(file.path(HERE, "EVAL_GRID.csv"), stringsAsFactors = FALSE)
cutoffs <- as.Date(grid$cutoff)
if (SMOKE) { cutoffs <- cutoffs[c(1, nrow(grid))]; NSEED <- 1L }

# Shard the cutoff list across processes. prefit_rolling_cv_psi() iterates its
# cutoffs serially, so concurrency has to come from running several processes
# over disjoint subsets. Each writes psi_<cutoff>.csv into the SAME cache dir,
# which is safe because the filenames are cutoff-keyed and disjoint by
# construction. Measured cost: ~11 min per (cutoff x seed).
SHARD  <- as.integer(Sys.getenv("PSI_SHARD",  "0"))   # 0-based
NSHARD <- as.integer(Sys.getenv("PSI_NSHARD", "1"))
if (NSHARD > 1L) {
     keep <- (seq_along(cutoffs) - 1L) %% NSHARD == SHARD
     cutoffs <- cutoffs[keep]
     if (!length(cutoffs)) { cat("shard", SHARD, "has no cutoffs; exiting\n"); quit(save = "no") }
}

# A000 = the INCUMBENT model: v7.3 features, concurrent target (lead 0),
# production architecture. Deliberately NOT the 12-week training geometry --
# that is what the arms vary. The evaluation grid is frozen and shared.
spec <- list(
  feature_set = "v7.3",
  arch_control = list(n_seeds = NSEED, parallel_seeds = 1L, lead = 0L)
)

cat("ARM:", ARM, "| shard", SHARD, "of", NSHARD, "| cutoffs:", length(cutoffs), "| seeds:", NSEED,
    "| MOSAIC", as.character(packageVersion("MOSAIC")), "\n")
cat("cutoffs:", paste(format(cutoffs), collapse = ", "), "\n")

t0 <- Sys.time()
res <- prefit_rolling_cv_psi(
  PATHS                = PATHS,
  cutoffs              = cutoffs,
  est_suitability_spec = spec,
  pred_date_start      = "2014-01-01",
  pred_date_stop       = "2027-02-04",
  dir_cache            = file.path(HERE, paste0("psi_cache_", ARM, if (SMOKE) "_smoke" else "")),
  verbose              = TRUE)
cat("\n===== ", ARM, " DONE in ", round(difftime(Sys.time(), t0, units = "mins"), 1),
    " min =====\n", sep = "")
str(res, max.level = 1)
