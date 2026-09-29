#!/usr/bin/env Rscript
# =============================================================================
# psi_ab/run.R -- 3-arm downstream A/B screen: does swapping the psi trunk
# (LSTM -> DLinear) change MOSAIC calibration IS/OOS performance?
#
# Forked from claude/forecast_cv_ocv4_q2yr_run.R. The control block is reused
# almost verbatim (it is red-teamed); the deliberate changes are listed below.
#
# ARMS (3). Frozen psi caches built by claude/psi_ab/build_caches.R:
#   prod   psi_cache_P000E  LSTM trunk                   comparator
#   nd     psi_cache_NDe    DLinear trunk                treatment
#   nd_rep psi_cache_NDeR   DLinear, seed_base=1001      NOISE FLOOR
# nd vs nd_rep is a same-spec disjoint-seed psi replicate, so it measures the
# floor the prod-vs-nd effect must clear -- at zero psi compute. This matters
# because the psi_evolve programme has already had three marginal claims killed
# by a replicate. A same-arm CALIBRATION replicate is impossible (the pipeline
# is deterministic given config/priors/control), so a psi-seed replicate is the
# only floor obtainable at all.
#
# THE FLOOR IS ANTI-CONSERVATIVE, AND THE INFERENCE MUST RESPECT THAT.
# Var(prod - nd) = var_LSTM + var_DLinear + signal, but nd-vs-nd_rep estimates
# only 2*var_DLinear. DLinear is the QUIETER method -- it never reads
# recurrent_dropout, the documented sole source of psi nondeterminism, and its
# measured replicate spread is max |dpsi| 0.18-0.60 against the LSTM's
# 0.73-0.83 -- so this floor UNDERSTATES the noise in the treatment contrast.
# The correct floor is an LSTM replicate of P000E. It does not exist on disk:
# psi_cache_P000R is MOSAIC 0.91.1 (pre-epoch-fix), so it is not a clean
# replicate of P000E (0.91.13), and refitting one on this host would build it
# under 0.90.5 -- a different package version from the arms it is the floor for.
#
# The resulting asymmetry is what makes the screen worth running as-is:
# an anti-conservative floor licenses a KILL but never a SHIP.
#   - effect INSIDE the floor  -> decisive negative; it failed to clear even an
#     understated noise band. Given the measured 69-106% psi_star absorption,
#     this is the expected outcome, and it is the cheap answer we are buying.
#   - effect OUTSIDE the floor -> NOT a ship signal. It means: buy a true LSTM
#     replicate floor at a matched package version before believing it.
#
# WHAT THIS CAN AND CANNOT CONCLUDE. All three caches were fitted at
# country_static="off", country_balance=FALSE, whereas production ships
# "auto"/TRUE. So this is a clean TRUNK experiment -- "holding everything else
# fixed, does the DLinear trunk propagate downstream?" -- and NOT a ship
# decision. A null here is still informative: the absorbing mechanism
# (psi_star re-fitting around whatever psi it is given) is downstream of and
# blind to country_static/country_balance, so absorption at these settings
# implies absorption at production settings.
#
# CHANGES vs the OCV-4 parent:
#   1. FIXED-mode calibration (n_simulations = 10000L, not adaptive). Adaptive
#      BFRS stops at a data-dependent batch, so the two arms would draw
#      different sim_id sets and Common Random Numbers would break. Fixed mode
#      uses sim_ids = seq_len(n), and parameter draws are sample_parameters(
#      seed = sim_id) with psi NOT an input to any draw -- so both arms draw
#      BIT-IDENTICAL 301-parameter vectors and run them under identical engine
#      seeds. The arms differ ONLY in psi_jt. That is exact CRN, and it is what
#      makes a 36-cell screen worth running at all.
#   2. base_config = the HOST's MOSAIC::config_default rather than the parent's
#      2018-window masked rds. NOTE the window is version-dependent: MOSAIC
#      0.90.5 (dugong) ships 2018-01-01..2027-02-04 = 3321 ticks, while 0.91.14
#      ships 2023-01-01.. = 1495 ticks. Cost scales with ticks, so the host's
#      version sets the per-cell price -- do not assume the cheaper one. The
#      script asserts >=2yr of IS history per cutoff either way, which is what
#      the `seasonal` baseline needs (R/evaluate_rolling_cv.R:338-339).
#      The parent's COVID zero-wall masking is NOT carried over; under a 2018
#      window that masking is a real data-quality fix, so if this screen is
#      ever escalated to a ship decision, revisit it.
#   3. No prefit phase. The psi caches already exist.
#
# USAGE (per-cell, env-var selected; see launch.sh)
#   PSI_AB_PHASE=calibrate PSI_AB_ARM=nd PSI_AB_UNIT=MOZ \
#   PSI_AB_CUTOFF=2025-10-01 PSI_AB_CORES=43 r-mosaic-Rscript run.R
#   PSI_AB_PHASE=score r-mosaic-Rscript run.R
# PSI_AB_DRYRUN=1 (default) validates and prints the grid with no compute.
# =============================================================================
suppressMessages(library(MOSAIC))

root  <- Sys.getenv("MOSAIC_ROOT", unset = path.expand("~/MOSAIC"))
# run_MOSAIC()/run_rolling_cv() resolve paths from the root_directory OPTION,
# not from the PATHS object alone -- without this every cell fails instantly
# with "MOSAIC root directory not set".
MOSAIC::set_root_directory(root)
PATHS <- MOSAIC::get_paths(root)

CACHE_ROOT <- path.expand(Sys.getenv("PSI_AB_CACHE_ROOT", "~/psi_ab"))
ARM_SPECS  <- readRDS(file.path(CACHE_ROOT, "arm_specs.rds"))

SPEC <- list(
     # Screen 1 (trunk): prod / nd / nd_rep.
     # Screen 2 (LSTM features, added 2026-09-28 after screen 1 found ND not
     # better): p000 = main-production proxy (no D9b/N8, old epoch rule),
     # p000r = its seed replicate (the LSTM noise floor screen 1 lacked),
     # n9 = the branch's shipped defaults (D9b + N8 + epoch fix). Screen-1
     # cells are complete and are skipped on resume.
     arms = c("prod", "nd", "nd_rep", "p000", "p000r", "n9"),

     # 4 countries. All are in the 16-country psi_evolve burden pool, all are
     # seasonal-baseline-eligible from 2025-04-01, and all four have measured
     # OCV-4 runtimes. Chosen BEFORE the ND result existed (they are the OCV-4
     # set), so the selection is outcome-blind with respect to the treatment --
     # unlike a set chosen to span ND's per-country effect, which would be
     # selection on a column the NDe/NDeR replicate already showed to be noise.
     units = list("COD", "ETH", "MOZ", "NGA"),

     # 3 cutoffs, maximally spread over the usable grid. Chosen from the five
     # that cleared an outcome-blind OOS data pre-flight (>=80 scoreable days
     # and >=50 observed cases in the OOS<=3mo window, in EVERY unit).
     # 2026-01-01 is deliberately EXCLUDED: ETH has only 39 scoreable days /
     # 35 cases there, so that cell would drop out and unbalance the panel.
     # 2024-07-01 is excluded too (sits <50% through the series; MOZ has 19
     # cases). All three below are present in all three caches.
     cutoff_dates = as.Date(c("2024-10-01", "2025-04-01", "2025-10-01")),

     horizons_months = c(1, 2, 3),
     primary_horizon = 3,

     # Cumulative-nested buckets: OOS<=1mo is a strict SUBSET of OOS<=3mo.
     # Primary is OOS<=3mo alone; 1/2mo are the decay curve, never replicates.
     embargo_weeks_cases  = 2L,
     embargo_weeks_deaths = 2L,

     metrics      = c("cases", "deaths"),
     baselines    = c("seasonal", "persistence", "persistence_last"),
     ess_min      = 50,
     min_cells_ci = 10L,      # > per-cell n here => auto CI suppressed by design;
                              # the headline is the paired per-cell delta, not an IID CI
     models         = c("ensemble", "ensemble_opt", "medoid"),
     central_method = "median",

     out_dir = Sys.getenv("PSI_AB_OUT",
                          unset = file.path(root, "MOSAIC-pkg", "claude", "psi_ab", "out")),

     control = MOSAIC::mosaic_control_defaults(
          calibration = list(
               # FIXED mode -> sim_ids 1..n -> exact CRN. n=5000 rather than
               # 10000: |B| = 1.15*ESS_best and ESS_B are closed forms in the
               # control, not functions of the data, and the 5k-vs-100k
               # posterior difference is ~92% resampling noise -- which under
               # CRN is SHARED between arms and cancels in the paired
               # difference. Halves the grid cost for no measurable loss.
               n_simulations         = 5000L,
               # Do NOT cut to 1: the per-draw stochastic replication is what
               # stabilises the likelihood, and the likelihood is the signal
               # path for the primary endpoint.
               n_iterations          = 3L),
          sampling = list(
               sample_tau_i          = FALSE,    # single-location: no diffusion-out
               sample_mobility_gamma = FALSE,
               sample_mobility_omega = FALSE,
               sample_kappa          = FALSE,
               sample_alpha_1        = FALSE),   # non-identifiable single-country
          likelihood = list(
               weight_cases   = 1.0, weight_deaths = 1.0,
               nb_k_min_cases = 10,
               nb_k_min_deaths= 3,
               burn_in_days   = 30L),
          targets = list(ESS_param = 500L, ESS_param_prop = 0.95),
          predictions = list(
               n_iter_ensemble      = 50L,
               central_method       = "median",
               optimize_objective   = "wis",
               capture_trajectories = FALSE),
          io = list(persist_ensemble_arrays = FALSE, save_simresults = FALSE))
)

PHASE   <- match.arg(Sys.getenv("PSI_AB_PHASE", "all"),
                     c("all", "grid", "calibrate", "score"))

# PHASE=grid emits the cell list, one "arm:unit:cutoff" per line, and exits.
# launch.sh consumes THIS rather than carrying its own copy of the grid --
# the first launch attempt was aborted because launch.sh had a stale hardcoded
# cutoff list that had drifted from SPEC (it still held the excluded
# 2026-01-01 and was missing 2024-10-01). One definition, one source.
if (identical(PHASE, "grid")) {
     for (arm in SPEC$arms)
          for (u in SPEC$units)
               for (.i in seq_along(SPEC$cutoff_dates))
                    cat(sprintf("%s:%s:%s\n", arm, paste(u, collapse = "+"),
                                format(as.Date(SPEC$cutoff_dates)[.i])))
     quit(save = "no", status = 0)
}
DRY_RUN <- !identical(Sys.getenv("PSI_AB_DRYRUN", unset = "1"), "0")
CORES   <- as.integer(Sys.getenv("PSI_AB_CORES", "43"))

.arm  <- Sys.getenv("PSI_AB_ARM", "")
.unit <- Sys.getenv("PSI_AB_UNIT", "")
.cut  <- Sys.getenv("PSI_AB_CUTOFF", "")
cell_arms    <- if (nzchar(.arm))  .arm                       else SPEC$arms
cell_units   <- if (nzchar(.unit)) list(.unit)                else SPEC$units
cell_cutoffs <- if (nzchar(.cut))  as.Date(.cut)              else as.Date(SPEC$cutoff_dates)

base_config <- MOSAIC::config_default
cfg_start   <- as.Date(base_config$date_start)

# ---- Pre-flight assertions (cheap, and they fail before any compute) --------
stopifnot(all(cell_arms %in% names(ARM_SPECS)))
for (arm in SPEC$arms) {
     cdir <- file.path(CACHE_ROOT, paste0("cache_", arm))
     if (!file.exists(file.path(cdir, "psi_manifest.json")))
          stop("missing psi cache for arm '", arm, "': ", cdir,
               " (run claude/psi_ab/build_caches.R on this host first)")
     man <- MOSAIC:::.rcv_psi_read_manifest(file.path(cdir, "psi_manifest.json"))
     # NB: `for (x in <Date vector>)` strips the Date class and iterates
     # numerics. Index instead, here and in every cutoff loop below.
     for (i in seq_along(SPEC$cutoff_dates))
          invisible(MOSAIC:::.rcv_psi_cache_lookup(cdir, man,
                                                   as.Date(SPEC$cutoff_dates)[i],
                                                   ARM_SPECS[[arm]]))
}
# Every cutoff must leave >= 2 years of in-sample history for the `seasonal`
# baseline, or that cutoff silently yields no skill cell.
.is_years <- as.numeric(as.Date(SPEC$cutoff_dates) - cfg_start) / 365.25
if (any(.is_years < 2))
     stop("cutoff(s) with < 2y IS history under date_start ", cfg_start, ": ",
          paste(SPEC$cutoff_dates[.is_years < 2], collapse = ", "),
          " -- the seasonal baseline would be NA.")

cat("=============================================================\n")
cat("psi_ab -- 3-arm psi trunk A/B screen\n")
cat("=============================================================\n")
cat(sprintf("MOSAIC       : %s\n", as.character(utils::packageVersion("MOSAIC"))))
cat(sprintf("arms         : %s\n", paste(SPEC$arms, collapse = ", ")))
cat(sprintf("units        : %s\n", paste(unlist(SPEC$units), collapse = ", ")))
cat(sprintf("cutoffs      : %s  (IS history %s yr)\n",
            paste(format(SPEC$cutoff_dates), collapse = ", "),
            paste(sprintf("%.2f", .is_years), collapse = "/")))
cat(sprintf("config       : %s -> %s (%d ticks, %d locations)\n",
            cfg_start, base_config$date_stop,
            as.numeric(as.Date(base_config$date_stop) - cfg_start), length(base_config$location_name)))
cat(sprintf("calibration  : FIXED n_simulations=%s x n_iterations=%s (exact CRN)\n",
            SPEC$control$calibration$n_simulations, SPEC$control$calibration$n_iterations))
cat(sprintf("grid         : %d arms x %d units x %d cutoffs = %d cells\n",
            length(SPEC$arms), length(SPEC$units), length(SPEC$cutoff_dates),
            length(SPEC$arms) * length(SPEC$units) * length(SPEC$cutoff_dates)))
cat(sprintf("this process : arm=%s unit=%s cutoff=%s cores=%d\n",
            paste(cell_arms, collapse = "+"), paste(unlist(cell_units), collapse = "+"),
            paste(format(cell_cutoffs), collapse = "+"), CORES))
cat(sprintf("out          : %s\n", SPEC$out_dir))
cat("psi caches verified: all arms resolve all cutoffs.\n")

if (DRY_RUN) { cat("\nDRY RUN -- validated, no compute. Set PSI_AB_DRYRUN=0 to execute.\n"); quit(save = "no", status = 0) }

dir.create(SPEC$out_dir, recursive = TRUE, showWarnings = FALSE)

.cell_dir <- function(arm, uid, T_k)
     file.path(SPEC$out_dir, "per_arm", arm, "per_unit", uid, sprintf("cutoff_%s", format(T_k)))

# ---- CALIBRATE --------------------------------------------------------------
if (PHASE %in% c("all", "calibrate")) {
     ctrl <- SPEC$control
     # BOTH are required: mosaic_control_defaults() ships parallel$enable=FALSE,
     # so setting n_cores alone silently runs the whole cell on ONE core.
     ctrl$parallel$enable  <- TRUE
     ctrl$parallel$n_cores <- CORES
     ctrl$paths$plots      <- FALSE
     # Thread safety for PSOCK workers (CLAUDE.md: all six, or BLAS/worker deadlock).
     Sys.setenv(OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
                NUMEXPR_NUM_THREADS = "1", TBB_NUM_THREADS = "1", NUMBA_NUM_THREADS = "1")

     for (arm in cell_arms) {
          cdir <- file.path(CACHE_ROOT, paste0("cache_", arm))
          for (u in cell_units) {
               uid <- paste(u, collapse = "+")
               for (.i in seq_along(cell_cutoffs)) {
                    T_k <- as.Date(cell_cutoffs)[.i]
                    run_dir <- .cell_dir(arm, uid, T_k)
                    if (file.exists(file.path(run_dir, "predictions.parquet"))) {
                         cat(sprintf("[skip] %s / %s @ %s already complete\n", arm, uid, format(T_k)))
                         next
                    }
                    dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
                    t0 <- Sys.time()
                    cat(sprintf("[%s] START %s / %s @ %s\n",
                                format(t0, "%F %T"), arm, uid, format(T_k)))
                    MOSAIC::run_rolling_cv(
                         PATHS                = PATHS,
                         iso                  = u,
                         n_cutoffs            = 1L,
                         latest_cutoff        = T_k,
                         step_months          = 3L,
                         horizons_months      = SPEC$horizons_months,
                         embargo_weeks        = SPEC$embargo_weeks_cases,
                         base_config          = base_config,
                         priors               = MOSAIC::priors_default,
                         control              = ctrl,
                         optimize_subset      = TRUE,
                         models               = SPEC$models,
                         central_method       = SPEC$central_method,
                         est_suitability_spec = ARM_SPECS[[arm]],
                         psi_cache            = cdir,
                         dir_output           = run_dir,
                         verbose              = TRUE)
                    cat(sprintf("[%s] DONE  %s / %s @ %s  (%.1f min)\n",
                                format(Sys.time(), "%F %T"), arm, uid, format(T_k),
                                as.numeric(difftime(Sys.time(), t0, units = "mins"))))
               }
          }
     }
}

# ---- SCORE ------------------------------------------------------------------
# IS and OOS both come from evaluate_rolling_cv(): it emits a window=="IS" row
# per cell alongside the OOS<=Nmo rows (R/evaluate_rolling_cv.R:140,151-156),
# using the same estimator, so the IS/OOS contrast is like-for-like.
if (PHASE %in% c("all", "score")) {
     .wr <- function(x, p) if (requireNamespace("arrow", quietly = TRUE))
          arrow::write_parquet(x, p) else utils::write.csv(x, sub("parquet$", "csv", p), row.names = FALSE)

     all_cells <- list(); all_preds <- list()
     for (arm in SPEC$arms) for (u in SPEC$units) {
          uid <- paste(u, collapse = "+")
          for (.i in seq_along(SPEC$cutoff_dates)) {
               T_k <- as.Date(SPEC$cutoff_dates)[.i]
               f <- file.path(.cell_dir(arm, uid, T_k), "predictions.parquet")
               if (!file.exists(f)) next
               pu <- if (requireNamespace("arrow", quietly = TRUE))
                    as.data.frame(arrow::read_parquet(f)) else utils::read.csv(f)
               ev <- MOSAIC::evaluate_rolling_cv(
                    predictions = pu, horizons_months = SPEC$horizons_months,
                    baselines = SPEC$baselines, metrics = SPEC$metrics,
                    embargo_weeks = c(cases = SPEC$embargo_weeks_cases,
                                      deaths = SPEC$embargo_weeks_deaths),
                    ess_min = SPEC$ess_min, min_cells_ci = SPEC$min_cells_ci)
               cells <- ev$cells; cells$arm <- arm
               all_cells[[length(all_cells) + 1L]] <- cells
               pu$arm <- arm; all_preds[[length(all_preds) + 1L]] <- pu
          }
     }
     if (!length(all_cells)) stop("no completed cells to score under ", SPEC$out_dir)
     .wr(do.call(rbind, all_cells), file.path(SPEC$out_dir, "scores_cells.parquet"))
     .wr(do.call(rbind, all_preds), file.path(SPEC$out_dir, "predictions_all.parquet"))
     cat(sprintf("scored %d cell-frames -> %s\n", length(all_cells), SPEC$out_dir))
}
