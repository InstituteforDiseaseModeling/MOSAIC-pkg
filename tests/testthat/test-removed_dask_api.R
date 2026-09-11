# =============================================================================
# test-removed_dask_api.R
#
# Removed Dask/Coiled API surface must fail LOUDLY, never silently.
#
# NAMESPACE is exportPattern("^[[:alpha:]]+"), so every one of these was public
# API. They are deleted rather than deprecated -- the user base is this team and
# each had zero callers outside the package -- but deletion is only safe if the
# old call shape errors. An argument that is accepted and ignored is the exact
# failure mode of CLAUDE.md lesson #13, where a back-compat shim silently
# reverted user settings to defaults for fifteen minor versions because nothing
# validated unknown keys.
#
# These tests are the "unknown key validator" that was missing then.
# =============================================================================

test_that("removed Dask/Coiled functions error with a pointer to the replacement", {
  expect_error(check_coiled_workspace(), "removed")
  expect_error(mosaic_dask_presets(125), "removed")
  # The message must name what to do instead, not just that it is gone.
  expect_error(check_coiled_workspace(), "check_dependencies")
  expect_error(mosaic_dask_presets(125), "control\\$parallel\\$n_cores")
})

test_that("make_mosaic_cluster() is NOT removed", {
  # It builds the local PSOCK cluster the surviving backend runs on, and
  # run_MOSAIC() calls it directly. Deleting it alongside the Dask helpers --
  # which an early draft of the migration plan proposed, on the strength of its
  # Dask-era documentation -- would have broken every local run.
  expect_true(is.function(make_mosaic_cluster))
  expect_true("n_cores" %in% names(formals(make_mosaic_cluster)))
})

test_that("run_MOSAIC() rejects dask_spec instead of ignoring it", {
  expect_error(
    run_MOSAIC(config = list(location_name = "ETH"), priors = list(a = 1),
               dir_output = tempfile(), dask_spec = list(type = "coiled")),
    "dask_spec` has been removed"
  )
})

test_that("run_MOSAIC() rejects unknown arguments", {
  # Absent this check, a typo'd or stale argument vanishes into `...` and the
  # run silently proceeds on defaults.
  expect_error(
    run_MOSAIC(config = list(location_name = "ETH"), priors = list(a = 1),
               dir_output = tempfile(), n_wrokers = 4L),
    "unknown argument"
  )
})

test_that("the removed-argument registry covers every deleted argument", {
  expect_setequal(
    names(MOSAIC:::.MOSAIC_REMOVED_ARGS),
    c("dask_spec", "py_module", "visualize", "pdf", "outdir")
  )
  # Every entry must carry an actionable message, not a bare "removed".
  for (nm in names(MOSAIC:::.MOSAIC_REMOVED_ARGS)) {
    expect_gt(nchar(MOSAIC:::.MOSAIC_REMOVED_ARGS[[nm]]), 40L)
  }
})

test_that(".mosaic_reject_removed_args passes clean when nothing was supplied", {
  expect_true(MOSAIC:::.mosaic_reject_removed_args(list(), "fn"))
})

test_that(".mosaic_reject_removed_args refuses unnamed pass-through", {
  expect_error(MOSAIC:::.mosaic_reject_removed_args(list(1), "fn"), "unnamed")
})
