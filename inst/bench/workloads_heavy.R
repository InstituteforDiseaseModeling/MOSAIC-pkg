# =============================================================================
# workloads_heavy.R -- the two expensive workloads, gated behind flags.
#
# Separated because they cost minutes rather than seconds and should not run in
# every interleaved block. `--parallel` and `--calib` opt them in.
# =============================================================================

BENCH_WORKLOADS_HEAVY <- list(

  # --- 10. parallel throughput through the REAL worker path ------------------
  # An engine-only k-sweep measures a strictly weaker thing than the A-3a gate
  # asks for: it omits the shared-directory parquet write, which at high worker
  # counts is the most plausible non-scaling component. N scales with k so each
  # arm takes roughly constant wall time; k = 1 is labelled `sequential`
  # because run_MOSAIC() takes a different code path there (no cluster at all).
  list(id = "parallel/throughput", needs = c("root", "heavy"), fn = function(ctx) {
    pr <- .bench_fx(ctx, "priors.rds"); cfg <- ctx$config_full
    ks <- ctx$k_sweep
    shared <- file.path(tempdir(), paste0("benchpar_", as.integer(runif(1, 1, 1e8))))
    dir.create(shared, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(shared, recursive = TRUE), add = TRUE)
    lib <- ctx$lib; root <- getOption("root_directory")
    out <- list()
    for (k in ks) {
      n <- 8L * k
      cl <- parallel::makeCluster(k, type = "PSOCK")
      parallel::clusterExport(cl, c("lib", "root", "shared"), envir = environment())
      parallel::clusterEvalQ(cl, {
        suppressMessages(library(MOSAIC, lib.loc = lib))
        MOSAIC::set_root_directory(root)
        Sys.setenv(OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
                   OPENBLAS_NUM_THREADS = "1", ARROW_NUM_THREADS = "1")
        NULL
      })
      parallel::clusterExport(cl, c("pr", "cfg"), envir = environment())
      t0 <- proc.time()[["elapsed"]]
      # Per-simulation failures are tolerated and COUNTED, not fatal. The
      # Python engine rejects draws the R engine accepts: NumPy's Poisson
      # raises ValueError("lam value too large") above lambda ~ 9.2e18 because
      # it returns int64, while R's rpois() returns a double and samples
      # correctly. An extreme zeta_2 draw therefore kills a Python worker but
      # not an R one. Letting that abort the whole arm would mean the Python
      # baseline silently produces no row at all -- which is what happened on
      # the first run of this suite.
      res <- parallel::parLapply(cl, seq_len(n), function(i) tryCatch({
        P  <- MOSAIC::get_paths()
        invisible(utils::capture.output(suppressMessages(
          ps <- MOSAIC::sample_parameters(PATHS = P, priors = pr, config = cfg, seed = 3000L + i))))
        m  <- if (exists("run_simulation", envir = asNamespace("MOSAIC"), inherits = FALSE))
                 MOSAIC::run_simulation(config = ps, seed = 3000L + i, quiet = TRUE)
              else MOSAIC::run_LASER(config = ps, seed = 3000L + i, quiet = TRUE)
        rc <- if (inherits(m$results$reported_cases, "python.builtin.object"))
                 reticulate::py_to_r(m$results$reported_cases) else m$results$reported_cases
        arrow::write_parquet(data.frame(i = i, s = sum(as.numeric(rc))),
                             file.path(shared, sprintf("p_%05d.parquet", i)))
        TRUE
      }, error = function(e) conditionMessage(e)))
      el <- proc.time()[["elapsed"]] - t0
      parallel::stopCluster(cl)
      nfail <- sum(!vapply(res, isTRUE, logical(1)))
      out[[paste0("k", k)]] <- list(k = k, n = n, wall = el, sps = n / el, nfail = nfail)
    }
    reps <- vapply(out, function(z) z$wall, numeric(1))
    extra <- list()
    for (z in out) {
      extra[[paste0("sims_per_sec_k", z$k)]] <- round(z$sps, 4)
      extra[[paste0("wall_s_k", z$k)]]       <- round(z$wall, 4)
      extra[[paste0("n_sims_k", z$k)]]       <- z$n
      extra[[paste0("n_failed_k", z$k)]]     <- z$nfail
    }
    list(reps_s = reps, extra = extra)
  }),

  # --- 11. a real, fully pinned calibration ---------------------------------
  # FIXED mode (`calibration$n_simulations`) is the knob that actually gates
  # the loop; the adaptive knobs give a one-sided cap and never a floor.
  # Pinning the simulations alone is not enough: the best-subset grid search
  # (min..max) then drives n_subset x n_iter_ensemble + n_iter_best extra
  # engine runs -- 400-1,100 for a nominal 100 -- so those are pinned too.
  # Fresh dir_output every run: resume=FALSE with clean_output=FALSE leaves the
  # previous run's shards in place and the combine step globs the directory.
  list(id = "calib/fixed-small", needs = c("root", "calib"), fn = function(ctx) {
    pr  <- .bench_fx(ctx, "priors.rds")
    cfg <- .bench_fx(ctx, Sys.getenv("BENCH_CALIB_CFG", "config_moz.rds"))
    ctl <- list(
      calibration = list(n_simulations = ctx$calib_n, n_iterations = as.integer(Sys.getenv('BENCH_CALIB_ITER','1'))),
      targets     = list(min_best_subset = 30L, max_best_subset = 30L),
      predictions = list(n_iter_ensemble = 2L, n_iter_best = 5L),
      parallel    = list(enable = TRUE, n_cores = ctx$calib_cores),
      io          = MOSAIC::mosaic_io_presets("fast"),
      # plots default to TRUE and fire ~25 ggsaves at 300 dpi, which would
      # dominate a benchmark whose stated subject is simulation speed. Off
      # here, and noted in the README so the omission is explicit rather than
      # discovered later by someone comparing against a plotted run.
      paths       = list(plots = FALSE, clean_output = TRUE)
    )
    reps <- numeric(0); extra <- list()
    for (r in seq_len(ctx$reps$calib)) {
      d <- file.path(tempdir(), paste0("benchcal_", as.integer(runif(1, 1, 1e8))))
      t0 <- proc.time()[["elapsed"]]
      ok <- tryCatch({
        MOSAIC::run_MOSAIC(config = cfg, priors = pr, dir_output = d,
                           control = ctl, resume = FALSE)
        TRUE
      }, error = function(e) { extra$error <<- substr(conditionMessage(e), 1, 200); FALSE })
      reps <- c(reps, proc.time()[["elapsed"]] - t0)
      if (ok) {
        sh <- length(list.files(file.path(d, "2_calibration"), pattern = "\\.parquet$",
                                recursive = TRUE))
        extra$n_shards <- sh
        extra$sims_per_sec <- round(ctx$calib_n / utils::tail(reps, 1), 4)
        # Retained vs total is the draw-failure rate, and it matters: a failed
        # sample_parameters() draw returns early and costs ~20x less than a
        # completed simulation, so a priors or clamp change moves sims/sec with
        # the engine untouched. Without this column that shift is invisible.
        sj <- file.path(d, "3_results", "summary.json")
        if (file.exists(sj)) {
          js <- tryCatch(jsonlite::fromJSON(sj), error = function(e) NULL)
          extra$sims_total    <- js$n_simulations_total %||% NA
          extra$sims_retained <- js$n_simulations_retained %||% js$sims_retained %||% NA
        }
      }
      unlink(d, recursive = TRUE)
    }
    extra$n_simulations <- ctx$calib_n
    extra$n_cores <- ctx$calib_cores
    list(reps_s = reps, extra = extra)
  })
)
