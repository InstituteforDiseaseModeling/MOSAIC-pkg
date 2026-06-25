# Round-trip tests for the "spatial" figure group of render_MOSAIC_figures().
# These assert pure read-render (P5): figures are produced from config.json + a
# packaged basemap + persisted .rds arrays only, never from a simulation.

.make_min_run_dir <- function(J = 5L, Tt = 30L, with_spatial = TRUE) {
  dir_output <- tempfile("mosaic_spatial_")
  dir.create(file.path(dir_output, "1_inputs"), recursive = TRUE)
  dir.create(file.path(dir_output, "2_calibration"), recursive = TRUE)

  cfg <- jsonlite::fromJSON(
    system.file("extdata", "config_default.json", package = "MOSAIC"),
    simplifyVector = TRUE)
  idx <- seq_len(J)
  cfg$location_name <- cfg$location_name[idx]
  cfg$longitude     <- cfg$longitude[idx]
  cfg$latitude      <- cfg$latitude[idx]
  cfg$N_j_initial   <- cfg$N_j_initial[idx]
  cfg$tau_i         <- cfg$tau_i[idx]
  jsonlite::write_json(cfg, file.path(dir_output, "1_inputs", "config.json"),
                       auto_unbox = TRUE, digits = NA)

  if (with_spatial) {
    loc <- cfg$location_name
    sv  <- MOSAIC:::.MOSAIC_ARTIFACT_SCHEMA_VERSION
    .stamp <- function(o) { attr(o, "mosaic_schema_version") <- sv; o }

    H <- matrix(runif(J * Tt, 0, 1e-4), nrow = J)
    rownames(H) <- loc
    colnames(H) <- as.character(seq(as.Date("2023-01-01"), by = "day",
                                    length.out = Tt))
    saveRDS(.stamp(list(array = H, location_name = loc,
                        estimator = "ensemble_median", kind = "spatial_hazard")),
            file.path(dir_output, "2_calibration", "spatial_hazard_ensemble.rds"))

    C <- matrix(runif(J * J, -1, 1), nrow = J); C <- (C + t(C)) / 2; diag(C) <- 1
    C[1, 2] <- NaN; C[2, 1] <- NaN  # zero-variance cell (DM#4 mask)
    dimnames(C) <- list(loc, loc)
    saveRDS(.stamp(list(array = C, location_name = loc,
                        estimator = "ensemble_median", kind = "coupling")),
            file.path(dir_output, "2_calibration", "coupling_ensemble.rds"))
  }
  dir_output
}

test_that("spatial group renders all six figures from config + artifacts (P5)", {
  skip_if_not_installed("ggplot2")
  dir_output <- .make_min_run_dir(with_spatial = TRUE)
  on.exit(unlink(dir_output, recursive = TRUE), add = TRUE)

  # Trip-wire: the renderer must NOT touch any simulation path.
  expect_no_error(
    suppressWarnings(
      render_MOSAIC_figures(dir_output, which = "spatial", verbose = FALSE))
  )

  spat <- file.path(dir_output, "3_results", "figures", "spatial")
  for (f in c("diffusion_pi.png", "departure_tau.png",
              "mobility_flux_matrix.png", "mobility_flux_network.png",
              "spatial_hazard.png", "spatial_coupling.png")) {
    expect_true(file.exists(file.path(spat, f)), info = f)
  }
})

test_that("spatial group is pure read-render: never trips a simulation path (P5)", {
  skip_if_not_installed("ggplot2")
  skip_if(packageVersion("testthat") < "3.0.0")
  dir_output <- .make_min_run_dir(with_spatial = TRUE)
  on.exit(unlink(dir_output, recursive = TRUE), add = TRUE)

  # Hard trip-wire: if the renderer reaches ANY simulation/estimation entry
  # point, these mocks abort the run and the test fails. A clean pass proves
  # the spatial group rendered entirely from config.json + persisted artifacts.
  testthat::local_mocked_bindings(
    run_LASER           = function(...) stop("P5 violated: run_LASER called"),
    calc_model_ensemble = function(...) stop("P5 violated: calc_model_ensemble called"),
    sample_parameters   = function(...) stop("P5 violated: sample_parameters called"),
    .package = "MOSAIC"
  )

  expect_no_error(
    suppressWarnings(
      render_MOSAIC_figures(dir_output, which = "spatial", verbose = FALSE))
  )
  spat <- file.path(dir_output, "3_results", "figures", "spatial")
  expect_true(file.exists(file.path(spat, "spatial_hazard.png")))
  expect_true(file.exists(file.path(spat, "spatial_coupling.png")))
})

test_that("spatial group warns-and-skips hazard/coupling when arrays absent", {
  skip_if_not_installed("ggplot2")
  dir_output <- .make_min_run_dir(with_spatial = FALSE)
  on.exit(unlink(dir_output, recursive = TRUE), add = TRUE)

  expect_warning(
    render_MOSAIC_figures(dir_output, which = "spatial", verbose = FALSE),
    "spatial hazard"
  )

  spat <- file.path(dir_output, "3_results", "figures", "spatial")
  # Mobility figs (config-only) still render; hazard/coupling are skipped.
  expect_true(file.exists(file.path(spat, "diffusion_pi.png")))
  expect_true(file.exists(file.path(spat, "mobility_flux_matrix.png")))
  expect_false(file.exists(file.path(spat, "spatial_hazard.png")))
  expect_false(file.exists(file.path(spat, "spatial_coupling.png")))
})

test_that("'spatial' is a recognized figure group", {
  d <- tempfile("mosaic_grp_"); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  # "spatial" is accepted (no Unknown-group error); an empty dir just warn-skips.
  expect_no_error(
    suppressWarnings(render_MOSAIC_figures(d, which = "spatial", verbose = FALSE)))
  expect_error(
    render_MOSAIC_figures(d, which = "not_a_group"),
    "Unknown figure group"
  )
})
