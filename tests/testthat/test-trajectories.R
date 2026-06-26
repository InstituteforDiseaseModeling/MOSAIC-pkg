# Trajectory capture/reduce + plotter tests. All LASER-FREE (CI-safe per PLAN
# 14.D): capture-and-reduce is exercised via calc_model_ensemble(precomputed_
# results=) which bypasses the engine, and the plotter via a synthetic
# mosaic_trajectories fixture.

# ---- helpers ---------------------------------------------------------------

# A minimal single-location config (no LASER needed in precomputed mode).
.make_cfg <- function(n_time = 20L, n_loc = 1L) {
  mk <- function(scale) if (n_loc == 1L) as.numeric(scale * (1 + sin(seq_len(n_time)))) else
    matrix(scale * (1 + sin(seq_len(n_time * n_loc))), nrow = n_loc)
  list(
    location_name   = if (n_loc == 1L) "AAA" else paste0("L", seq_len(n_loc)),
    reported_cases  = mk(10),
    reported_deaths = mk(2),
    date_start      = "2020-01-01",
    date_stop       = as.character(as.Date("2020-01-01") + n_time - 1L)
  )
}

# Build a synthetic precomputed_results list with the trajectory channels.
.make_precomputed <- function(n_param = 3L, n_stoch = 2L, n_time = 20L,
                              n_loc = 1L, with_traj = TRUE) {
  chans <- MOSAIC:::.MOSAIC_TRAJECTORY_CHANNELS_DEFAULT
  vecmat <- function(v) if (n_loc == 1L) v else matrix(rep(v, n_loc), nrow = n_loc, byrow = TRUE)
  out <- list()
  for (s in seq_len(n_stoch)) for (p in seq_len(n_param)) {
    base <- (p + s) * (1 + cos(seq_len(n_time)))
    rec <- list(param_idx = p, stoch_idx = s, param_seed = 1000L + p,
                reported_cases  = vecmat(10 * base + p),
                reported_deaths = vecmat(2 * base + p),
                success = TRUE)
    if (with_traj) {
      traj <- list()
      for (ch in chans) traj[[ch]] <- vecmat(base + match(ch, chans))
      # make disease_deaths clearly distinct from reported_deaths scale
      traj[["disease_deaths"]] <- vecmat(5 * base + 50)
      traj[["S"]]  <- vecmat(rep(1000, n_time))
      traj[["N"]]  <- vecmat(rep(2000, n_time))
      rec$traj <- traj
      rec$traj_epi <- list(delta_reporting_cases = rep(2, n_loc),
                           epidemic_threshold    = rep(0.01, n_loc))
    }
    out[[length(out) + 1L]] <- rec
  }
  out
}

.run_ensemble <- function(n_param = 3L, n_stoch = 2L, n_time = 20L, n_loc = 1L,
                          capture = TRUE, with_traj = TRUE) {
  cfg <- .make_cfg(n_time, n_loc)
  calc_model_ensemble(
    config                   = cfg,
    parameter_seeds          = 1000L + seq_len(n_param),
    parameter_weights        = rep(1, n_param),
    n_simulations_per_config = n_stoch,
    precomputed_results      = .make_precomputed(n_param, n_stoch, n_time, n_loc, with_traj),
    capture_trajectories     = capture,
    verbose                  = FALSE
  )
}

# ---- capture + reduce ------------------------------------------------------

test_that("capture_trajectories toggle takes effect (artifact built iff TRUE)", {
  on  <- .run_ensemble(capture = TRUE)
  off <- .run_ensemble(capture = FALSE)
  expect_s3_class(on$trajectories, "mosaic_trajectories")
  expect_null(off$trajectories)
})

test_that("capture does not clobber the global RNG (RNG-purity)", {
  set.seed(99L); before <- .Random.seed
  invisible(.run_ensemble(capture = TRUE))
  expect_identical(.Random.seed, before)   # withr::with_seed restores state
})

test_that("reduction yields per-channel medians + derived series", {
  ens <- .run_ensemble()
  tr  <- ens$trajectories
  expect_s3_class(tr, "mosaic_trajectories")
  # direct channels present (reported_* sourced from arrays; rest captured)
  expect_true(all(c("reported_cases", "reported_deaths", "S", "Isym", "N") %in% tr$channels))
  # derived series present
  expect_true(all(c("I_total", "mass_balance", "CFR", "epidemic_frac") %in% tr$channels))
  # median shape [n_loc x n_time]
  expect_equal(dim(tr$summary$S$median), c(1L, 20L))
  # lines is a tidy frame with the documented columns
  expect_true(all(c("member_id", "weight", "location", "channel", "t", "value")
                  %in% names(tr$lines)))
})

test_that("field semantics: reported_deaths (observable) != disease_deaths (burden)", {
  ens <- .run_ensemble()
  tr  <- ens$trajectories
  rd  <- tr$summary$reported_deaths$median
  dd  <- tr$summary$disease_deaths$median
  expect_false(isTRUE(all.equal(as.numeric(rd), as.numeric(dd))))
  # reported_deaths median must match the weighted median of the deaths_array
  # (the reported/observable scale), NOT the captured disease_deaths channel.
  expect_equal(as.numeric(rd), as.numeric(ens$deaths_median), tolerance = 1e-8)
})

test_that("deviation-#1: reported_* trajectory median honors the supplied weights", {
  # The reducer must reduce over WHATEVER member set + weights it is handed, so
  # run_MOSAIC's optimized-subset capture yields trajectory medians equal to the
  # optimized prediction-ensemble medians. NON-UNIFORM weights: reported_cases
  # trajectory median must equal the ensemble cases_median to machine precision.
  cfg <- .make_cfg(20L, 1L)
  ens <- calc_model_ensemble(
    config                   = cfg,
    parameter_seeds          = 1000L + seq_len(3L),
    parameter_weights        = c(0.6, 0.3, 0.1),
    n_simulations_per_config = 2L,
    precomputed_results      = .make_precomputed(3L, 2L, 20L, 1L, TRUE),
    capture_trajectories     = TRUE,
    verbose                  = FALSE
  )
  rc <- ens$trajectories$summary$reported_cases$median
  expect_equal(as.numeric(rc), as.numeric(ens$cases_median), tolerance = 1e-8)
})

test_that(".rec_mat trims tick+1 flow channels instead of dropping them (DM Finding 2)", {
  # Flow/FOI channels arrive one tick too long (recorded at tick+1). They must be
  # TRIMMED to n_time and retained -- not silently dropped to an empty panel.
  for (n_loc in c(1L, 3L)) {
    n_time <- 20L; n_param <- 2L; n_stoch <- 2L
    pc <- .make_precomputed(n_param, n_stoch, n_time, n_loc, TRUE)
    for (k in seq_along(pc)) {
      pc[[k]]$traj[["incidence"]] <- if (n_loc == 1L)
        as.numeric(seq_len(n_time + 1L)) else
        matrix(as.numeric(seq_len(n_loc * (n_time + 1L))), nrow = n_loc)
    }
    ens <- calc_model_ensemble(
      config = .make_cfg(n_time, n_loc), parameter_seeds = 1000L + seq_len(n_param),
      parameter_weights = rep(1, n_param), n_simulations_per_config = n_stoch,
      precomputed_results = pc, capture_trajectories = TRUE, verbose = FALSE)
    med <- ens$trajectories$summary$incidence$median
    expect_false(is.null(med))
    expect_equal(dim(med), c(n_loc, n_time))   # trimmed to n_time, not dropped
    expect_true(any(is.finite(med)))            # populated, not all-NA
  }
})

test_that("I_total guards a missing Iasym channel (treats as 0, not NA)", {
  cfg  <- .make_cfg()
  prec <- .make_precomputed()
  for (i in seq_along(prec)) prec[[i]]$traj[["Iasym"]] <- NULL  # drop Iasym
  ens <- calc_model_ensemble(
    config = cfg, parameter_seeds = 1000L + 1:3, parameter_weights = rep(1, 3),
    n_simulations_per_config = 2L, precomputed_results = prec,
    capture_trajectories = TRUE, verbose = FALSE)
  it <- ens$trajectories$summary$I_total$median
  expect_true(!is.null(it))
  expect_true(all(is.finite(it)))                 # Iasym missing -> Isym + 0
  expect_equal(as.numeric(it), as.numeric(ens$trajectories$summary$Isym$median))
})

test_that("epidemic_frac lag direction is correct (DM F9 t-index alignment)", {
  # One member, Isym spike at t=4, delta=2 -> flag must fire at t=6 (forward lag).
  n_time <- 10L
  cfg <- .make_cfg(n_time)
  Isym <- rep(0, n_time); Isym[4] <- 100
  rec <- list(param_idx = 1L, stoch_idx = 1L, param_seed = 1L,
              reported_cases = rep(1, n_time), reported_deaths = rep(0, n_time),
              success = TRUE,
              traj = list(S = rep(1000, n_time), E = rep(0, n_time),
                          Isym = Isym, Iasym = rep(0, n_time), R = rep(0, n_time),
                          V1 = rep(0, n_time), V2 = rep(0, n_time),
                          N = rep(1000, n_time)),
              traj_epi = list(delta_reporting_cases = 2, epidemic_threshold = 0.01))
  ens <- calc_model_ensemble(
    config = cfg, parameter_seeds = 1L, parameter_weights = 1,
    n_simulations_per_config = 1L, precomputed_results = list(rec),
    capture_trajectories = TRUE, verbose = FALSE)
  ef <- ens$trajectories$summary$epidemic_frac$median
  expect_equal(ef[1, 6], 1)          # Isym[6-2]=Isym[4]=100 > 0.01*1000=10
  expect_equal(ef[1, 5], 0)          # Isym[3]=0 -> no flag
})

test_that("capability check: capture on but no channels returned -> warn + NULL", {
  expect_warning(
    ens <- .run_ensemble(with_traj = FALSE, capture = TRUE),
    "no trajectory scratch was produced")
  expect_null(ens$trajectories)
})

# ---- plotter (synthetic fixture; no LASER) ---------------------------------

.make_fixture <- function(n_loc = 1L, n_time = 30L) {
  locs <- if (n_loc == 1L) "AAA" else paste0("L", seq_len(n_loc))
  chans <- c("reported_cases", "reported_deaths", "disease_deaths", "S", "E",
             "Isym", "Iasym", "N", "I_total", "mass_balance", "CFR", "epidemic_frac")
  summ <- lapply(chans, function(ch)
    list(median = matrix(abs(rnorm(n_loc * n_time, 100, 10)), n_loc, n_time)))
  names(summ) <- chans
  t_idx <- which(seq_len(n_time) %% 7L == 1L)
  line_chans <- setdiff(chans, c("mass_balance", "CFR", "epidemic_frac"))
  parts <- list()
  for (ch in line_chans) for (i in seq_len(n_loc)) for (m in 1:5)
    parts[[length(parts) + 1L]] <- data.frame(
      member_id = m, weight = 1 / 5, location = locs[i], channel = ch,
      t = t_idx, value = abs(rnorm(length(t_idx), 100, 20)),
      stringsAsFactors = FALSE)
  structure(list(
    schema = "mosaic_trajectories", channels = chans, location_names = locs,
    n_locations = n_loc, n_time_points = n_time,
    date_start = "2021-01-01",
    date_stop = as.character(as.Date("2021-01-01") + n_time - 1L),
    n_param_sets = 5L, n_simulations_per_config = 1L, n_successful = 5L,
    summary = summ, lines = do.call(rbind, parts),
    obs_cases = matrix(abs(rnorm(n_loc * n_time, 90, 10)), n_loc, n_time),
    obs_deaths = matrix(abs(rnorm(n_loc * n_time, 9, 3)), n_loc, n_time),
    cfr_refs = data.frame(location = locs,
                          cfr_baseline = rep(0.01, n_loc),
                          cfr_epidemic = rep(0.03, n_loc),
                          stringsAsFactors = FALSE)
  ), class = "mosaic_trajectories")
}

test_that("plot_model_trajectories renders a PDF + PNG (n_loc = 1)", {
  skip_if_not_installed("ggplot2")
  fx  <- .make_fixture(n_loc = 1L)
  dir <- withr::local_tempdir()
  res <- plot_model_trajectories(fx, location = "AAA", output_dir = dir, verbose = FALSE)
  expect_true(file.exists(res$pdf))
  expect_true(file.exists(res$png))
})

test_that("plot_model_trajectories renders for multi-location (n_loc = 3)", {
  skip_if_not_installed("ggplot2")
  fx  <- .make_fixture(n_loc = 3L)
  dir <- withr::local_tempdir()
  for (loc in fx$location_names) {
    res <- plot_model_trajectories(fx, location = loc, output_dir = dir, verbose = FALSE)
    expect_true(file.exists(res$pdf))
  }
})

test_that("plot_model_trajectories errors on unknown location", {
  fx <- .make_fixture(n_loc = 1L)
  expect_error(plot_model_trajectories(fx, location = "ZZZ",
                                       output_dir = withr::local_tempdir()),
               "not found")
})

test_that(".mosaic_compute_cfr_refs returns weighted per-location regime CFRs", {
  res <- data.frame(
    is_best_subset = c(TRUE, TRUE, FALSE),
    weight_best    = c(0.5, 0.5, 0.1),
    cfr_baseline_AAA = c(0.010, 0.020, 0.99),
    cfr_epidemic_AAA = c(0.030, 0.050, 0.99))
  refs <- MOSAIC:::.mosaic_compute_cfr_refs(res, "AAA")
  expect_equal(refs$location, "AAA")
  expect_true(refs$cfr_baseline >= 0.010 && refs$cfr_baseline <= 0.020)  # subset only
  expect_true(is.finite(refs$cfr_epidemic))
  # NULL when no cfr columns present at all
  expect_null(MOSAIC:::.mosaic_compute_cfr_refs(
    data.frame(is_best_subset = TRUE, weight_best = 1), "AAA"))
})

test_that("plot_model_trajectories draws CFR regime lines when cfr_refs present", {
  skip_if_not_installed("ggplot2")
  fx  <- .make_fixture(n_loc = 1L)
  dir <- withr::local_tempdir()
  res <- plot_model_trajectories(fx, location = "AAA", output_dir = dir, verbose = FALSE)
  expect_true(file.exists(res$pdf))
  # also renders fine when cfr_refs is absent (back-compat / capture without CFR cols)
  fx$cfr_refs <- NULL
  res2 <- plot_model_trajectories(fx, location = "AAA", output_dir = dir, verbose = FALSE)
  expect_true(file.exists(res2$pdf))
})

test_that("R and Python trajectory channel lists are in lockstep (#12 guard)", {
  # A single-channel divergence between .MOSAIC_TRAJECTORY_CHANNELS_DEFAULT (R)
  # and _TRAJ_CHANNELS (Python) would silently drop that channel on the Dask
  # backend (the narrow #12 class). Parse both and assert identical (order incl).
  py <- system.file("python", "mosaic_dask_worker.py", package = "MOSAIC")
  if (!nzchar(py)) py <- testthat::test_path("..", "..", "inst", "python",
                                             "mosaic_dask_worker.py")
  skip_if_not(file.exists(py), "python worker source not found")
  src <- paste(readLines(py, warn = FALSE), collapse = "\n")
  m <- regmatches(src, regexpr("(?s)_TRAJ_CHANNELS\\s*=\\s*\\((.*?)\\)", src, perl = TRUE))
  skip_if(length(m) == 0L, "could not locate _TRAJ_CHANNELS")
  py_chans <- gsub('"', "", regmatches(m, gregexpr('"[^"]+"', m))[[1]])
  expect_identical(py_chans, MOSAIC:::.MOSAIC_TRAJECTORY_CHANNELS_DEFAULT)
})

test_that("deferred reduce returns a scratch handle; optimized-subset reduce is bit-identical", {
  # reduce_trajectories = FALSE (run_MOSAIC path): calc_model_ensemble spills to
  # scratch and returns the handle WITHOUT reducing. The caller then reduces over
  # a SUBSET (the optimized members) directly from scratch -- no re-simulation.
  sd <- file.path(tempdir(), sprintf("traj_defer_%d", as.integer(runif(1, 1, 1e7))))
  ens <- calc_model_ensemble(
    config = .make_cfg(20L, 1L), parameter_seeds = 1000L + 1:3,
    parameter_weights = c(0.6, 0.3, 0.1), n_simulations_per_config = 2L,
    precomputed_results = .make_precomputed(3L, 2L, 20L, 1L, TRUE),
    capture_trajectories = TRUE, reduce_trajectories = FALSE,
    trajectory_scratch_dir = sd, verbose = FALSE)

  expect_null(ens$trajectories)                       # NOT reduced inline
  expect_false(is.null(ens$trajectory_scratch))       # handle returned
  expect_true(dir.exists(ens$trajectory_scratch$dir))
  expect_gt(length(list.files(ens$trajectory_scratch$dir, pattern = "^sim_")), 0L)

  # Reduce over the optimized subset {param 1, 3} with weights {0.7, 0.3}.
  sub_pidx <- c(1L, 3L); sub_w <- c(0.7, 0.3)
  tr <- MOSAIC:::.mosaic_build_trajectories(
    scratch_dir = ens$trajectory_scratch$dir, subset_orig_pidx = sub_pidx,
    subset_weights = sub_w,
    cases_array  = ens$cases_array[, , sub_pidx, , drop = FALSE],
    deaths_array = ens$deaths_array[, , sub_pidx, , drop = FALSE],
    n_stoch = 2L, n_locations = 1L, n_time_points = 20L, location_names = "AAA",
    date_start = ens$date_start, date_stop = ens$date_stop, n_successful = 4L,
    obs_cases = ens$obs_cases, obs_deaths = ens$obs_deaths,
    trajectory_channels = MOSAIC:::.MOSAIC_TRAJECTORY_CHANNELS_DEFAULT,
    n_lines = 50L, verbose = FALSE)
  expect_s3_class(tr, "mosaic_trajectories")

  # reported_cases trajectory median == weighted median over the SUBSET (exact).
  sw  <- rep(sub_w, times = 2L) / 2L
  cs  <- ens$cases_array[, , sub_pidx, , drop = FALSE]
  ref <- vapply(seq_len(20L), function(t)
    MOSAIC::weighted_quantiles(as.vector(cs[1, t, , ]), sw, 0.5), numeric(1))
  expect_equal(as.numeric(tr$summary$reported_cases$median), as.numeric(ref),
               tolerance = 1e-8)
  # a compartment channel was read back from scratch (not empty).
  expect_true(any(is.finite(tr$summary$S$median)))
  unlink(ens$trajectory_scratch$dir, recursive = TRUE, force = TRUE)
})

test_that("reported_* central honors central_method = 'mean' per channel (FIX 2)", {
  # Under central_method = "mean", the prediction plots draw the weighted MEAN;
  # the trajectory reported_* central line must match (weighted mean), while a
  # channel without a "mean" request stays on the weighted median.
  sd <- file.path(tempdir(), sprintf("traj_mean_%d", as.integer(runif(1, 1, 1e7))))
  ens <- calc_model_ensemble(
    config = .make_cfg(20L, 1L), parameter_seeds = 1000L + 1:3,
    parameter_weights = c(0.6, 0.3, 0.1), n_simulations_per_config = 2L,
    precomputed_results = .make_precomputed(3L, 2L, 20L, 1L, TRUE),
    capture_trajectories = TRUE, reduce_trajectories = FALSE,
    trajectory_scratch_dir = sd, verbose = FALSE)

  sub_pidx <- 1:3; sub_w <- c(0.6, 0.3, 0.1)
  tr <- MOSAIC:::.mosaic_build_trajectories(
    scratch_dir = sd, subset_orig_pidx = sub_pidx, subset_weights = sub_w,
    cases_array = ens$cases_array, deaths_array = ens$deaths_array,
    n_stoch = 2L, n_locations = 1L, n_time_points = 20L, location_names = "AAA",
    date_start = ens$date_start, date_stop = ens$date_stop, n_successful = 6L,
    obs_cases = ens$obs_cases, obs_deaths = ens$obs_deaths,
    trajectory_channels = MOSAIC:::.MOSAIC_TRAJECTORY_CHANNELS_DEFAULT,
    central_method = c(cases = "mean", deaths = "median"),
    n_lines = 50L, verbose = FALSE)

  sw <- rep(sub_w, times = 2L) / 2L
  cmean <- vapply(seq_len(20L), function(t)
    stats::weighted.mean(as.vector(ens$cases_array[1, t, , ]), sw), numeric(1))
  expect_equal(as.numeric(tr$summary$reported_cases$median), as.numeric(cmean),
               tolerance = 1e-8)                       # cases -> weighted MEAN
  dmed <- vapply(seq_len(20L), function(t)
    MOSAIC::weighted_quantiles(as.vector(ens$deaths_array[1, t, , ]), sw, 0.5),
    numeric(1))
  expect_equal(as.numeric(tr$summary$reported_deaths$median), as.numeric(dmed),
               tolerance = 1e-8)                       # deaths -> weighted median
  unlink(sd, recursive = TRUE, force = TRUE)
})

test_that("end-to-end: optimized-subset trajectory central == ensemble_optimized (FIX 8)", {
  # Lock the contract against formula drift in EITHER path: run the real
  # optimize_ensemble_subset(), then reduce trajectories over its optimized
  # members (mapped by seed, exactly as run_MOSAIC does) and assert the reported_*
  # central equals ensemble_optimized's cases_median/deaths_median.
  sd <- file.path(tempdir(), sprintf("traj_e2e_%d", as.integer(runif(1, 1, 1e7))))
  ens <- calc_model_ensemble(
    config = .make_cfg(20L, 1L), parameter_seeds = 1000L + 1:5,
    parameter_weights = rep(0.2, 5), n_simulations_per_config = 2L,
    precomputed_results = .make_precomputed(5L, 2L, 20L, 1L, TRUE),
    capture_trajectories = TRUE, reduce_trajectories = FALSE,
    trajectory_scratch_dir = sd, verbose = FALSE)

  so <- MOSAIC::optimize_ensemble_subset(
    ensemble = ens, likelihoods = c(-10, -11, -12, -13, -14),
    seeds = ens$seeds, min_n = 2L, objective = "mae",
    central_method = "median", stride = 1L, verbose = FALSE)

  final_pidx <- match(so$optimal_seeds, ens$seeds)     # opt members -> scratch keys
  tr <- MOSAIC:::.mosaic_build_trajectories(
    scratch_dir = sd, subset_orig_pidx = final_pidx,
    subset_weights = so$optimal_weights,
    cases_array = so$ensemble_optimized$cases_array,
    deaths_array = so$ensemble_optimized$deaths_array,
    n_stoch = 2L, n_locations = 1L, n_time_points = 20L, location_names = "AAA",
    date_start = ens$date_start, date_stop = ens$date_stop, n_successful = 10L,
    obs_cases = ens$obs_cases, obs_deaths = ens$obs_deaths,
    trajectory_channels = MOSAIC:::.MOSAIC_TRAJECTORY_CHANNELS_DEFAULT,
    central_method = c(cases = "median", deaths = "median"),
    n_lines = 50L, verbose = FALSE)

  expect_equal(as.numeric(tr$summary$reported_cases$median),
               as.numeric(so$ensemble_optimized$cases_median), tolerance = 1e-8)
  expect_equal(as.numeric(tr$summary$reported_deaths$median),
               as.numeric(so$ensemble_optimized$deaths_median), tolerance = 1e-8)
  unlink(sd, recursive = TRUE, force = TRUE)
})

test_that("auto scratch dir is cleaned up after a standalone reduce", {
  before <- length(list.files(tempdir(), pattern = "^mosaic_traj_"))
  ens <- calc_model_ensemble(
    config = .make_cfg(20L, 1L), parameter_seeds = 1000L + 1:2,
    parameter_weights = c(0.5, 0.5), n_simulations_per_config = 2L,
    precomputed_results = .make_precomputed(2L, 2L, 20L, 1L, TRUE),
    capture_trajectories = TRUE, verbose = FALSE)  # reduce_trajectories defaults TRUE
  after <- length(list.files(tempdir(), pattern = "^mosaic_traj_"))
  expect_s3_class(ens$trajectories, "mosaic_trajectories")
  expect_equal(after, before)   # auto-created scratch removed on exit
})
