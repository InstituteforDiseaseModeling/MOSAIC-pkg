# =============================================================================
# Parallel figure rendering (pipeline plan item 6a).
#
# render_MOSAIC_figures() fans its three per-location figure families out across
# PSOCK workers. What has to hold:
#   1. the parallel path writes exactly the same files as the serial path;
#   2. the location set is derived in ONE place, shared by the renderer and the
#      plotting function, so the two cannot drift apart (CLAUDE.md lesson #11);
#   3. a worker closure does NOT capture render_MOSAIC_figures()'s frame --
#      parLapply serialises a closure with its environment, and that frame holds
#      the ensemble and trajectory objects, so capture would ship hundreds of MB
#      per chunk while still producing correct output. Correctness cannot catch
#      this; only an environment assertion can.
# =============================================================================

make_traj <- function(locs, Tn = 12L) {
  nL <- length(locs)
  inc <- matrix(seq_len(nL * Tn) / 10, nL, Tn)
  structure(list(
    schema         = "mosaic_trajectories",
    channels       = "incidence",
    location_names = locs,
    n_locations    = nL,
    n_time_points  = Tn,
    date_start     = "2023-01-01",
    date_stop      = as.character(as.Date("2023-01-01") + Tn - 1L),
    summary        = list(incidence = list(median = inc)),
    lines          = data.frame(member_id = integer(0), weight = numeric(0),
                                location = character(0), channel = character(0),
                                t = integer(0), value = numeric(0),
                                stringsAsFactors = FALSE)
  ), class = "mosaic_trajectories")
}

# -----------------------------------------------------------------------------
# 2. one shared derivation of the location set
# -----------------------------------------------------------------------------
test_that("location codes come from a single shared derivation", {
  methods <- list(Posterior = list(
    parameters_location = list(
      beta = list(location = list(AGO = 1, BDI = 2, ETH = 3))
    )
  ))
  expect_identical(MOSAIC:::.mosaic_location_codes_from_methods(methods),
                   c("AGO", "BDI", "ETH"))

  # no per-location block at all -> NULL, not an error
  expect_null(MOSAIC:::.mosaic_location_codes_from_methods(
    list(Posterior = list(parameters_global = list(a = 1)))))
  expect_null(MOSAIC:::.mosaic_location_codes_from_methods(list()))
})

test_that("the file-level location helper unions posteriors and quantiles", {
  d <- withr::local_tempdir()
  pj <- file.path(d, "posteriors.json")
  jsonlite::write_json(
    list(parameters_location = list(beta = list(location = list(AGO = 1, BDI = 2)))),
    pj, auto_unbox = TRUE)
  qcsv <- file.path(d, "quantiles.csv")
  utils::write.csv(data.frame(location = c("BDI", "ETH"), q = 1:2), qcsv,
                   row.names = FALSE)

  expect_setequal(MOSAIC:::.mosaic_posterior_location_codes(pj, qcsv),
                  c("AGO", "BDI", "ETH"))
  # missing files degrade to empty, never error
  expect_identical(MOSAIC:::.mosaic_posterior_location_codes(NULL, NULL),
                   character(0))
  expect_identical(
    MOSAIC:::.mosaic_posterior_location_codes(file.path(d, "nope.json"), NULL),
    character(0))
})

# -----------------------------------------------------------------------------
# 3. the worker must not drag its creator's frame along
# -----------------------------------------------------------------------------
test_that("render workers carry only their arguments, not the caller's frame", {
  w <- MOSAIC:::.mosaic_mk_render_worker(
    "plot_model_distributions",
    list(json_files = "a.json", method_names = "Prior", output_dir = "/tmp"),
    "locations")

  e <- environment(w)
  # Only the three forced arguments live here...
  expect_setequal(ls(e, all.names = TRUE), c("fn_name", "args", "arg_name"))
  # ...and the parent chain is the package namespace, NOT a render frame.
  expect_true(environmentName(parent.env(e)) %in%
                c("MOSAIC", "imports:MOSAIC", "R_GlobalEnv"))

  # The actual regression, stated as a RELATIVE property so it does not depend
  # on how large a serialised namespace reference happens to be: a worker built
  # while a 4 MB object is live in the defining frame must serialise to the same
  # size as one built in an empty frame. A closure created inside
  # render_MOSAIC_figures() would fail this, because parLapply serialises a
  # closure together with its environment -- and that frame holds the ensemble.
  mk <- function(payload) {
    force(payload)
    MOSAIC:::.mosaic_mk_render_worker("plot_model_distributions",
                                      list(f = "a.json"), "locations")
  }
  small <- length(serialize(mk(NULL), NULL))
  big   <- length(serialize(mk(runif(5e5)), NULL))   # ~4 MB live in the frame
  expect_identical(big, small)
  expect_lt(big - small, 1000L)

  expect_true(is.function(MOSAIC:::.mosaic_traj_render_worker))
})

# -----------------------------------------------------------------------------
# 1. parallel output == serial output
# -----------------------------------------------------------------------------
test_that("parallel trajectory rendering writes the same files as serial", {
  skip_on_cran()
  skip_if_not_installed("parallel")
  # Builds a PSOCK cluster, so it cannot run inside a testthat parallel worker.
  skip_if_testthat_parallel()

  locs <- c("AAA", "BBB", "CCC", "DDD")
  traj <- make_traj(locs)

  run_one <- function(n_cores) {
    root <- withr::local_tempdir()
    dirs <- MOSAIC:::.mosaic_ensure_dir_tree(root, clean_output = FALSE)
    saveRDS(MOSAIC:::.mosaic_stamp_artifact(traj),
            file.path(dirs$calibration, "trajectories_ensemble.rds"))
    suppressWarnings(render_MOSAIC_figures(root, which = "trajectories",
                                           verbose = FALSE, n_cores = n_cores))
    sort(list.files(dirs$res_fig_trajectories))
  }

  serial   <- run_one(1L)
  parallel <- run_one(2L)

  expect_gt(length(serial), 0L)
  expect_identical(parallel, serial)
})

test_that("the parallel path actually starts workers", {
  skip_on_cran()
  skip_if_testthat_parallel()

  # Output equality alone cannot detect this: a cluster that fails to start
  # falls back to lapply() and produces byte-identical figures. That is exactly
  # what happened -- make_mosaic_cluster() demanded set_root_directory(), so
  # every render silently ran serially while the tests stayed green. Assert the
  # cluster is really built, and that no root directory is needed to build it.
  withr::local_options(list(root_directory = NULL))

  root <- withr::local_tempdir()
  dirs <- MOSAIC:::.mosaic_ensure_dir_tree(root, clean_output = FALSE)
  saveRDS(MOSAIC:::.mosaic_stamp_artifact(make_traj(c("AAA", "BBB", "CCC"))),
          file.path(dirs$calibration, "trajectories_ensemble.rds"))

  warnings_seen <- character(0)
  msgs <- character(0)
  withCallingHandlers(
    render_MOSAIC_figures(root, which = "trajectories", verbose = TRUE,
                          n_cores = 2L),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_false(any(grepl("could not start a cluster", warnings_seen)))
  expect_true(any(grepl("Rendering figures on 2 workers", msgs)))
  expect_gt(length(list.files(dirs$res_fig_trajectories)), 0L)
})

test_that("make_mosaic_cluster can build without a root directory", {
  skip_on_cran()
  skip_if_testthat_parallel()

  withr::local_options(list(root_directory = NULL))
  expect_error(make_mosaic_cluster(n_cores = 2L), "Root directory not set")

  cl <- make_mosaic_cluster(n_cores = 2L, require_root = FALSE)
  on.exit(try(MOSAIC:::.mosaic_stop_cluster(cl), silent = TRUE), add = TRUE)
  expect_length(cl, 2L)
  # workers came up with MOSAIC loaded and distinct processes
  pids <- unlist(parallel::clusterEvalQ(cl, Sys.getpid()))
  expect_length(unique(pids), 2L)
  expect_true(all(unlist(parallel::clusterEvalQ(cl, "MOSAIC" %in% loadedNamespaces()))))
})

test_that("n_cores is capped and a bad value degrades to serial", {
  skip_on_cran()
  skip_if_testthat_parallel()
  root <- withr::local_tempdir()
  dirs <- MOSAIC:::.mosaic_ensure_dir_tree(root, clean_output = FALSE)
  saveRDS(MOSAIC:::.mosaic_stamp_artifact(make_traj(c("AAA", "BBB"))),
          file.path(dirs$calibration, "trajectories_ensemble.rds"))

  # NA / garbage must not propagate into makePSOCKcluster
  expect_no_error(suppressWarnings(
    render_MOSAIC_figures(root, which = "trajectories", verbose = FALSE,
                          n_cores = NA_integer_)))
  expect_gt(length(list.files(dirs$res_fig_trajectories)), 0L)

  expect_lte(MOSAIC:::.MOSAIC_DETAIL_MAX_WORKERS, 16L)
  expect_gte(MOSAIC:::.MOSAIC_DETAIL_MAX_WORKERS, 2L)
})
