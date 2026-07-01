# Tests for .mosaic_ensemble_drop_arrays() -- the lightweight-persistence strip
# helper wired into run_MOSAIC()'s ensemble save sites -- and the graceful-fail
# guard in .add_reff_recompute_ci() when the persisted candidate was slimmed.

make_fake_ensemble <- function(with_arrays = TRUE) {
  n_loc <- 1L; n_time <- 6L; n_param <- 3L; n_sims <- 2L
  ca <- if (with_arrays) array(1, dim = c(n_loc, n_time, n_param, n_sims)) else NULL
  da <- if (with_arrays) array(2, dim = c(n_loc, n_time, n_param, n_sims)) else NULL
  structure(
    list(
      cases_mean               = matrix(1, n_loc, n_time),
      cases_median             = matrix(1, n_loc, n_time),
      deaths_mean              = matrix(0.1, n_loc, n_time),
      deaths_median            = matrix(0.1, n_loc, n_time),
      ci_bounds                = list(cases = list(), deaths = list()),
      obs_cases                = matrix(1, n_loc, n_time),
      obs_deaths               = matrix(0.1, n_loc, n_time),
      cases_array              = ca,
      deaths_array             = da,
      parameter_weights        = rep(1 / n_param, n_param),
      seeds                    = 1:n_param,
      n_param_sets             = n_param,
      n_simulations_per_config = n_sims,
      n_successful             = n_param,
      location_names           = "AAA",
      n_locations              = n_loc,
      n_time_points            = n_time,
      date_start               = "2020-01-01",
      date_stop                = "2020-01-06",
      envelope_quantiles       = list(),
      artifact_mask            = list(cases_warmup = 0L, deaths_final = FALSE)
    ),
    class = "mosaic_ensemble"
  )
}

test_that(".mosaic_ensemble_drop_arrays nulls both arrays and preserves class + light fields", {
  ens <- make_fake_ensemble(with_arrays = TRUE)
  light <- setdiff(names(ens), c("cases_array", "deaths_array"))
  light_before <- ens[light]

  slim <- MOSAIC:::.mosaic_ensemble_drop_arrays(ens)

  expect_null(slim$cases_array)
  expect_null(slim$deaths_array)
  expect_s3_class(slim, "mosaic_ensemble")
  # Every light field survives byte-for-byte.
  expect_identical(slim[light], light_before)
  # Original in-memory object is not mutated (copy semantics).
  expect_false(is.null(ens$cases_array))
  expect_false(is.null(ens$deaths_array))
})

test_that(".mosaic_ensemble_drop_arrays is idempotent", {
  ens <- make_fake_ensemble(with_arrays = FALSE)
  slim1 <- MOSAIC:::.mosaic_ensemble_drop_arrays(ens)
  slim2 <- MOSAIC:::.mosaic_ensemble_drop_arrays(slim1)
  expect_null(slim2$cases_array)
  expect_null(slim2$deaths_array)
  expect_identical(slim1, slim2)
})

test_that("slimmed ensemble round-trips through saveRDS well under 200 KB", {
  ens <- make_fake_ensemble(with_arrays = TRUE)
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f), add = TRUE)
  saveRDS(MOSAIC:::.mosaic_ensemble_drop_arrays(ens), f)
  back <- readRDS(f)
  expect_null(back$cases_array)
  expect_null(back$deaths_array)
  expect_true(!is.null(back$cases_median))
  expect_lt(file.info(f)$size, 200 * 1024)
})

test_that(".add_reff_recompute_ci stops informatively on an array-stripped candidate", {
  d <- tempfile("reff_slim_")
  dir.create(file.path(d, "2_calibration"), recursive = TRUE)
  dir.create(file.path(d, "1_inputs"), recursive = TRUE)

  saveRDS(make_fake_ensemble(with_arrays = FALSE),
          file.path(d, "2_calibration", "ensemble_candidate.rds"))
  # Minimal priors/control JSON so the existence checks pass and the guard is
  # the first thing to fire.
  writeLines("{}", file.path(d, "1_inputs", "priors.json"))
  writeLines("{}", file.path(d, "1_inputs", "control.json"))

  expect_error(
    MOSAIC:::.add_reff_recompute_ci(output_dir = d, base_config = list()),
    "persist_ensemble_arrays"
  )
})
