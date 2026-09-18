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

# PROTOCOL section 2 successive halving: a SCREENING arm may run on a subset of
# the evaluation grid at reduced cost, and is promoted to the full grid only if
# it looks promising. PSI_SCREEN_EVERY=k keeps every k-th cutoff (phase coverage
# is preserved because the grid's 84-day stride rotates through the year, so any
# regular subset still spans multiple seasons). Scoring is unaffected -- the
# scorer reads whichever cutoffs are present and reports the count.
SCREEN <- as.integer(Sys.getenv("PSI_SCREEN_EVERY", "1"))
if (SCREEN > 1L) {
     keep_s <- (seq_along(cutoffs) - 1L) %% SCREEN == 0L
     cutoffs <- cutoffs[keep_s]
     cat("SCREENING: every", SCREEN, "th cutoff ->", length(cutoffs), "of", nrow(grid), "\n")
}

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

# Arm definition. A000 is the INCUMBENT: v7.3 features, concurrent target,
# production CV geometry. Arms override only what they are testing, via env, so
# the registered "one change" is enforced at the launch boundary rather than by
# reading the script.
#
# PSI_GEOM=12wk applies the 12-week TRAINING geometry as a single registered
# BUNDLE: 84-day window, 2-week embargo, 2014 grid start (min_train_years=4),
# and the requested stride. These four are not independently meaningful -- an
# 84-day window is unreachable without the min_test_days change, and a day-based
# stride is meaningless without a day-based window -- so they are registered as
# one change with the components enumerated, not as four.
ac <- list(n_seeds = NSEED, parallel_seeds = 1L,
           lead = as.integer(Sys.getenv("PSI_LEAD", "0")))
# PSI_SEED_BASE shifts the whole seed block (seeds = seq(base, by=step, len=n)).
# This is what makes a REPLICATE possible: an identical arm refitted from a
# disjoint seed block measures the within-arm fit noise, which PROTOCOL section 4
# requires as the floor any delta must clear -- and which no arm in waves 0-13
# ever measured.
if (nzchar(Sys.getenv("PSI_SEED_BASE", ""))) {
  ac$seed_base <- as.integer(Sys.getenv("PSI_SEED_BASE"))
  cat("REPLICATE: seed_base =", ac$seed_base, "\n")
}
if (Sys.getenv("PSI_GEOM", "") == "12wk") {
  ac <- c(ac, list(
    test_days       = 84L,
    min_test_days   = 84L,
    rw_gap_weeks    = 2L,
    min_train_years = 4,
    step_days       = as.integer(Sys.getenv("PSI_STRIDE_DAYS", "84"))))
}
# AR-03: restrict to features OBSERVABLE at the forecast origin. At a 12-week
# origin a feature at lag L (weeks relative to the target) sits at target-L, so it
# is observable iff L >= 12; concurrent and short-lag channels must be supplied as
# projections, which past the ERA5 horizon are free-running CMIP6 with no anomaly
# skill at this lead. Measured: 20 of 38 v7.3 features are NOT observable.
#
# CONSERVATIVE BY DESIGN: this also drops ENSO4/ENSO34 short lags, which NMME
# genuinely forecasts with skill at 12 weeks. So AR-03 is a LOWER BOUND on what a
# forecast-honest feature set can do, not the best such set.
if (nzchar(Sys.getenv("PSI_EXCLUDE", ""))) {
  ac$exclude_covariates <- strsplit(Sys.getenv("PSI_EXCLUDE"), ",")[[1]]
  cat("AR-03: excluding", length(ac$exclude_covariates), "non-observable features\n")
}
spec <- list(feature_set = Sys.getenv("PSI_FEATURE_SET", "v7.3"), arch_control = ac)
cat("arch_control:\n"); utils::str(ac)

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
