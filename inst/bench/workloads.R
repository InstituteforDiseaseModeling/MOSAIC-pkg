# =============================================================================
# workloads.R -- the benchmark registry.
#
# Each entry: list(id, needs, fn). `fn(ctx)` returns list(reps_s=, extra=list()).
# `needs` gates a workload on capabilities the arm may lack:
#   "root"   options(root_directory) + the ~/MOSAIC data tree (sample_parameters)
#   "rengine" the pure-R engine only (no cross-engine equivalent)
#
# Kept deliberately small. Anything that cannot clear the measured ~3-12%
# wall-clock noise floor is NOT tracked here -- a 1% row recorded at false
# precision is worse than no row.
# =============================================================================

.bench_fx <- function(ctx, name) readRDS(file.path(ctx$fixture_dir, name))

BENCH_WORKLOADS <- list(

  # --- 1. machine calibrator: version-independent normaliser -----------------
  list(id = "machine/calibrator", needs = character(0), fn = function(ctx) {
    a <- .bench_fx(ctx, "calibrator_args.rds")
    b <- a$binom; p <- a$pois; np <- ctx$calibrator_passes
    # Looped to ~30k draw calls -- the engine's own per-run draw count -- so the
    # normaliser sits at the same timescale as the thing it normalises. Done by
    # repeating a small frozen argument set rather than freezing a 30k-call one,
    # which would be an 11 MB fixture for no extra information.
    t <- .bench_time({
      for (pass in seq_len(np)) {
        for (z in b) stats::rbinom(length(z$n), z$n, z$p)
        for (l in p) stats::rpois(length(l), l)
      }
    }, reps = ctx$reps$calibrator)
    list(reps_s = t, extra = list(n_draw_calls = np * (length(b) + length(p))))
  }),

  # --- 2. engine, full config, passed as a LIST (the production shape) -------
  list(id = "engine/full", needs = character(0), fn = function(ctx) {
    cfg <- ctx$config_full
    t   <- .bench_time(.bench_run(cfg, seed = 1L), reps = ctx$reps$anchor)
    fp  <- .bench_fingerprint(.bench_run(cfg, seed = 1L))
    list(reps_s = t, extra = c(fp, list(n_locations = length(cfg$location_name),
                                        n_ticks = ctx$T_full)))
  }),

  # --- 3. engine, config passed as a PATH ------------------------------------
  # The fair R-vs-Python arm: the Python engine always reads a file, so
  # comparing it against an R run handed a pre-parsed list charges R nothing
  # for config I/O. This is also where v0.71's JSON cache shows up.
  list(id = "engine/full-path", needs = character(0), fn = function(ctx) {
    p <- ctx$config_path
    t <- .bench_time(.bench_run(p, seed = 1L), reps = ctx$reps$path)
    list(reps_s = t, extra = list(n_ticks = ctx$T_full))
  }),

  # --- 4. engine, single location -------------------------------------------
  list(id = "engine/single-loc", needs = character(0), fn = function(ctx) {
    cfg <- .bench_fx(ctx, "config_moz.rds")
    t   <- .bench_time(.bench_run(cfg, seed = 1L), reps = ctx$reps$small)
    fp  <- .bench_fingerprint(.bench_run(cfg, seed = 1L))
    list(reps_s = t, extra = c(fp, list(n_locations = 1L, n_ticks = ctx$T_full)))
  }),

  # --- 5. engine, short window ----------------------------------------------
  # The tick dimension is the expensive one -- 7x the ticks costs 12.6x the
  # time against 40x the patches costing 1.37x -- and it is super-linear, so a
  # regression in history handling shows in the T shape and nowhere else.
  list(id = "engine/short-window", needs = character(0), fn = function(ctx) {
    cfg <- .bench_fx(ctx, "config_short.rds")
    t   <- .bench_time(.bench_run(cfg, seed = 1L), reps = ctx$reps$small)
    fp  <- .bench_fingerprint(.bench_run(cfg, seed = 1L))
    list(reps_s = t, extra = c(fp, list(n_locations = 40L, n_ticks = 400L)))
  }),

  # --- 6. engine, high vaccination ------------------------------------------
  # The only workload that executes the second-dose block.
  list(id = "engine/high-vacc", needs = character(0), fn = function(ctx) {
    cfg <- .bench_fx(ctx, "config_highvacc.rds")
    t   <- .bench_time(.bench_run(cfg, seed = 1L), reps = ctx$reps$small)
    fp  <- .bench_fingerprint(.bench_run(cfg, seed = 1L))
    list(reps_s = t, extra = c(fp, list(n_locations = 40L, n_ticks = 400L)))
  }),

  # --- 7. sample_parameters(): 13% of a worker, touched by neither engine ----
  list(id = "component/sample-params", needs = "root", fn = function(ctx) {
    pr <- .bench_fx(ctx, "priors.rds"); cfg <- ctx$config_full; P <- MOSAIC::get_paths()
    i <- 0L
    t <- .bench_time({ i <<- i + 1L
      invisible(utils::capture.output(suppressMessages(
        sp <- MOSAIC::sample_parameters(PATHS = P, priors = pr, config = cfg, seed = 1000L + i))))
    }, reps = ctx$reps$component)
    list(reps_s = t, extra = list())
  }),

  # --- 8. calc_model_likelihood(): pure R, signature stable across arms ------
  list(id = "component/likelihood", needs = character(0), fn = function(ctx) {
    cfg <- ctx$config_full
    oc <- cfg$reported_cases; od <- cfg$reported_deaths
    ec <- ctx$pred$rc; ed <- ctx$pred$rd
    t <- .bench_time(
      MOSAIC::calc_model_likelihood(obs_cases = oc, est_cases = ec,
                                    obs_deaths = od, est_deaths = ed),
      reps = ctx$reps$component)
    list(reps_s = t, extra = list())
  }),

  # --- 9. one calibration-worker iteration, end to end ----------------------
  # Built by hand rather than via run_MOSAIC() so it is identical across arms
  # (main's control schema carries dask_spec; 122+ rejects it) and so the
  # per-sim cost is isolated with enough reps to resolve ~7%. This is the only
  # workload where the two removed per-simulation gc() calls appear.
  list(id = "worker/per-sim", needs = "root", fn = function(ctx) {
    pr <- .bench_fx(ctx, "priors.rds"); cfg <- ctx$config_full; P <- MOSAIC::get_paths()
    dir <- file.path(tempdir(), paste0("benchworker_", as.integer(runif(1, 1, 1e8))))
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    legacy_gc <- .bench_engine_kind() == "python" ||
                 utils::packageVersion("MOSAIC") < "0.71.0"
    i <- 0L
    t <- .bench_time({ i <<- i + 1L
      invisible(utils::capture.output(suppressMessages(
        ps <- MOSAIC::sample_parameters(PATHS = P, priors = pr, config = cfg, seed = 2000L + i))))
      res <- .bench_run(ps, seed = 2000L + i)
      ll  <- MOSAIC::calc_model_likelihood(obs_cases = cfg$reported_cases, est_cases = res$rc,
                                           obs_deaths = cfg$reported_deaths, est_deaths = res$rd)
      arrow::write_parquet(data.frame(sim = i, ll = as.numeric(ll)[1]),
                           file.path(dir, sprintf("sim_%05d.parquet", i)))
      if (legacy_gc) { gc(verbose = FALSE); gc(verbose = FALSE) }
    }, reps = ctx$reps$worker)
    list(reps_s = t, extra = list(legacy_gc = legacy_gc))
  })
)
