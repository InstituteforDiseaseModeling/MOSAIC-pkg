# =============================================================================
# run_arm.R -- fit one arm's psi across the FROZEN v3 evaluation grid.
#
# OBJECTIVE v3: the grid IS the production validation grid (the 9 OCV-4
# quarterly cutoffs). One psi fit per cutoff (rolling-origin) via the package's
# own prefit_rolling_cv_psi(), so each scored block is predicted by a model that
# saw no data past its cutoff. Writes a per-cutoff psi CSV cache; scoring is a
# separate step (score_arm_driver.R) so a scoring bug never costs a refit.
#
# All 9 cutoffs are FITTED; only the 6 selection cutoffs are SCORED (the scorer
# enforces that). Fitting a confirmation cutoff is not a leak -- its fit is cut
# at its own cutoff -- but READING its score before a selection win is
# (PROTOCOL 5.3).
#
# env:
#   PSI_ARM              arm id (cache dir suffix)                [A000-style id]
#   PSI_GEOM             geometry preset: p000|p001|F1|F2|F3|F4    [p000]
#   PSI_FEATURE_SET      v7.3 | v7.4 (v7.4 -> leak-free panels)    [v7.3]
#   PSI_TRUNK            lstm | gru | tcn   (N arms)               [lstm]
#   PSI_SEEDS            ensemble size                             [10]
#   PSI_SEED_BASE        shifts the whole seed block (REPLICATES)  [fixture 11]
#   PSI_EPOCH_SELECT     HA-02: choose the epoch from k seeds      [0 = off]
#   PSI_LEAD             forecast lead in weeks (target arms)      [0]
#   PSI_EXCLUDE          comma-separated features to drop          [none]
#   PSI_SHARD/PSI_NSHARD shard the cutoff list across processes    [0/1]
#   PSI_SMOKE            1 = 2 cutoffs x 1 seed
# =============================================================================
suppressMessages(library(MOSAIC))
# `%||%` is base R only from 4.4.0 and MOSAIC's floor is 4.1.1; define it rather
# than discover at launch that the fold-count verification cannot run.
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
ARM    <- Sys.getenv("PSI_ARM", "P000")
SMOKE  <- Sys.getenv("PSI_SMOKE", "0") == "1"
NSEED  <- as.integer(Sys.getenv("PSI_SEEDS", "10"))
HERE   <- "/home/jgiles/psi_evolve"
dir.create(HERE, showWarnings = FALSE, recursive = TRUE)

# PROTOCOL section 2: screening at reduced origin counts is RETIRED (at 4-6
# origins the sign-flip floor is p = 0.125 and the unchanged-arm subset swing
# reaches 0.77). Fail loudly rather than silently honouring a stale launch line.
if (nzchar(Sys.getenv("PSI_SCREEN_EVERY", "")))
     stop("PSI_SCREEN_EVERY is retired at objective v3 (PROTOCOL section 2): arms run the full ",
          "9-cutoff grid or not at all. Cost control is n_seeds + HA-02, not fewer origins.")

set_root_directory("/home/jgiles/MOSAIC")
PATHS <- get_paths()

# ---- The frozen grid --------------------------------------------------------
grid <- utils::read.csv(file.path(HERE, "EVAL_GRID.csv"), stringsAsFactors = FALSE)
grid <- grid[grid$grid == "prod", ]
cutoffs <- as.Date(grid$cutoff)
if (length(cutoffs) != 9L)
     stop("EVAL_GRID.csv (grid == 'prod') must hold the 9 production cutoffs; found ",
          length(cutoffs))
if (SMOKE) { cutoffs <- cutoffs[c(1, length(cutoffs))]; NSEED <- 1L }

# Shard the cutoff list across processes. prefit_rolling_cv_psi() iterates its
# cutoffs serially, so concurrency comes from several processes over disjoint
# subsets, each writing psi_<cutoff>.csv into the SAME cache dir (filenames are
# cutoff-keyed and disjoint by construction).
SHARD  <- as.integer(Sys.getenv("PSI_SHARD",  "0"))   # 0-based
NSHARD <- as.integer(Sys.getenv("PSI_NSHARD", "1"))
if (NSHARD > 1L) {
     keep <- (seq_along(cutoffs) - 1L) %% NSHARD == SHARD
     cutoffs <- cutoffs[keep]
     if (!length(cutoffs)) { cat("shard", SHARD, "has no cutoffs; exiting\n"); quit(save = "no") }
}

# ---- Geometry presets ------------------------------------------------------
# Presets, not free-form knobs, so the registry can name a geometry and the
# one-change-per-arm rule is checkable at the launch boundary. Fold counts are
# the exact output of .psi_make_rw_cv_steps() over the 9 cutoffs (PROTOCOL 6).
GEOM <- list(
  # P000 -- est_suitability() package defaults (the shipped production model).
  # Month-based midpoint grid: 89 folds total.
  p000 = list(),
  # P001 -- the spec that the OCV-4 production validation actually ran.
  # 255 folds total. Matches psi_manifest.json in the OCV-4 psi_cache.
  p001 = list(rw_step_months = 1L, rw_test_months = 4L, rw_subsample = 2L,
              timesteps = 13L),
  # F-ladder: day-based geometry, validation window == the deployment horizon.
  # A bundle, registered with its components enumerated (PROTOCOL 2): an 84-day
  # window is unreachable without min_test_days, and a day-based stride is
  # meaningless without a day-based window.
  F4 = list(step_days = 84L, test_days = 84L, min_test_days = 84L,
            rw_gap_weeks = 2L, min_train_years = 4),      # 229 folds
  F1 = list(step_days = 28L, test_days = 84L, min_test_days = 84L,
            rw_gap_weeks = 2L, min_train_years = 4),      # 677 folds
  F2 = list(step_days = 14L, test_days = 84L, min_test_days = 84L,
            rw_gap_weeks = 2L, min_train_years = 4),      # 1351 folds
  F3 = list(step_days = 28L, test_days = 84L, min_test_days = 84L,
            rw_gap_weeks = 2L, min_train_years = 2)       # 912 folds
)
geom_name <- Sys.getenv("PSI_GEOM", "p000")
if (!geom_name %in% names(GEOM))
     stop("unknown PSI_GEOM '", geom_name, "'. Known presets: ",
          paste(names(GEOM), collapse = ", "))

ac <- c(list(n_seeds = NSEED, parallel_seeds = 1L,
             lead = as.integer(Sys.getenv("PSI_LEAD", "0"))),
        GEOM[[geom_name]])

# PSI_SEED_BASE shifts the whole seed block (seeds = seq(base, by = step, len = n)).
# This is what makes a REPLICATE possible: an identical arm refitted from a
# disjoint seed block measures the within-arm fit noise, which PROTOCOL 3b makes
# the floor any class-R delta must clear.
if (nzchar(Sys.getenv("PSI_SEED_BASE", ""))) {
  ac$seed_base <- as.integer(Sys.getenv("PSI_SEED_BASE"))
  cat("REPLICATE: seed_base =", ac$seed_base, "\n")
}

# Trunk registry (N arms). Swaps ONLY the sequence encoder; FiLM conditioning,
# head, loss and features are untouched.
TRUNK <- Sys.getenv("PSI_TRUNK", "lstm")
if (!TRUNK %in% c("lstm", "gru", "tcn"))
     stop("PSI_TRUNK must be lstm|gru|tcn; got '", TRUNK, "'")
if (TRUNK != "lstm") { ac$trunk <- TRUNK; cat("N arm: trunk =", TRUNK, "\n") }

# HA-02: run the fold loop on k seeds, refit all n_seeds at the pooled epoch.
# This is what makes the high-fold F arms affordable at the production seed
# count (F1 at 10 seeds: 53 h -> 12.4 h).
ESK <- as.integer(Sys.getenv("PSI_EPOCH_SELECT", "0"))
if (ESK > 0L) { ac$epoch_select_seeds <- ESK
  cat("HA-02: epoch selected from", ESK, "seed(s), all", NSEED, "refit at it\n") }

# AR-03-style feature restriction.
if (nzchar(Sys.getenv("PSI_EXCLUDE", ""))) {
  ac$exclude_covariates <- strsplit(Sys.getenv("PSI_EXCLUDE"), ",")[[1]]
  cat("excluding", length(ac$exclude_covariates), "feature(s)\n")
}

FEATSET <- Sys.getenv("PSI_FEATURE_SET", "v7.3")
spec <- list(feature_set = FEATSET, arch_control = ac)

# ---- Launch-time verification (wave-4 lesson) ------------------------------
# A silent 6x stride multiplication once turned a 12-fold grid into 2 and was
# caught only by comparing against an independent computation BEFORE the run.
# Do that computation here, every time, and print it.
#
# CRITICAL: resolve `ac` through .psi_load_arch_control() FIRST. The arm presets
# are sparse overrides -- `p000` is deliberately EMPTY -- and the fit resolves
# them against the B4 fixture, which pins rw_subsample = 6. Reading the raw
# preset instead computed 50 folds where the fit runs 9: the verification
# disagreed with a CORRECT fit, which is the same defect shape as CLAUDE.md
# lesson 13 (a guard keyed on raw input where the real path uses a
# defaults-merged structure), and a check that cries wolf is a check nobody
# reads. Caught at the 2026-09-18 P000 launch by this very print.
acr <- getFromNamespace(".psi_load_arch_control", "MOSAIC")(ac)
mk  <- getFromNamespace(".psi_make_rw_cv_steps", "MOSAIC")
nf <- vapply(cutoffs, function(T0) length(mk(
        fit_date_start = acr$fit_date_start %||% "2015-01-01", cutoff_date = T0,
        step_months = acr$rw_step_months %||% 1L,
        test_months = acr$rw_test_months %||% 5L,
        gap_weeks   = acr$rw_gap_weeks   %||% 4L,
        subsample   = acr$rw_subsample   %||% 1L,
        timesteps   = acr$timesteps      %||% 13L,
        min_test_days = acr$min_test_days, step_days = acr$step_days,
        test_days = acr$test_days, min_train_years = acr$min_train_years)), integer(1))

# The production psi cache is READ-ONLY (PROTOCOL 5.7): never write an arm into it.
CACHE <- file.path(HERE, paste0("psi_cache_", ARM, if (SMOKE) "_smoke" else ""))
if (grepl("forecast_cv_ocv4", CACHE, fixed = TRUE))
     stop("refusing to write into the OCV-4 production psi cache; it is read-only (PROTOCOL 5.7).")

cat("\n================ ARM ", ARM, " ================\n", sep = "")
cat("geometry     :", geom_name, "| trunk:", TRUNK, "| feature_set:", FEATSET, "\n")
cat("seeds        :", NSEED, "| epoch_select:", ESK, "| lead:", ac$lead, "\n")
cat("shard        :", SHARD, "of", NSHARD, "| MOSAIC", as.character(packageVersion("MOSAIC")), "\n")
cat("cutoffs      :", paste(format(cutoffs), collapse = ", "), "\n")
cat("inner folds  :", paste(nf, collapse = " "), " total", sum(nf), "\n")
cat("fit units    :", if (ESK > 0L) sprintf("%d fold-fits + %d refits (HA-02)",
                                            sum(nf) * ESK, length(cutoffs) * NSEED)
                      else sprintf("%d fold-fits + %d refits", sum(nf) * NSEED,
                                   length(cutoffs) * NSEED), "\n")
cat("cache        :", CACHE, "\n\n")
utils::str(ac)

# PSI_DRYRUN=1 prints the resolved plan and exits BEFORE any fitting. The
# launch-verification step (PROTOCOL 1.3) needs a way to check the geometry
# against section 6's table without burning compute; without this the only way
# to see the plan was to start the fit, and sourcing this script to inspect it
# starts 9 real fits. Learned the hard way 2026-09-18.
if (Sys.getenv("PSI_DRYRUN", "0") == "1") {
  cat("\nDRY RUN -- no fit launched. Resolved inner folds per cutoff above.\n")
  quit(save = "no", status = 0)
}

t0 <- Sys.time()
res <- prefit_rolling_cv_psi(
  PATHS                = PATHS,
  cutoffs              = cutoffs,
  est_suitability_spec = spec,
  # FIXED across every v3 arm, and deliberately equal to the OCV-4 cache's
  # window: the residual-interval estimator draws on pre-cutoff predictions
  # (D2 fix), so an arm with a different prediction span would get intervals
  # estimated from a different amount of history -- a confound with the arm.
  pred_date_start      = "2018-01-01",
  pred_date_stop       = "2027-02-04",
  dir_cache            = CACHE,
  verbose              = TRUE)
cat("\n===== ", ARM, " DONE in ", round(difftime(Sys.time(), t0, units = "mins"), 1),
    " min =====\n", sep = "")
str(res, max.level = 1)
