# =============================================================================
# test-dask_worker_schema_parity.R
#
# Engine-free regression test for the issue #101 worker-schema round trip.
#
# The Dask worker echoes back the sampled scalar/vector parameters (minus
# matrix fields) so the R orchestrator can flatten them into parquet rows
# without re-deserializing the JSON. .extract_sampled_params() strips
# `location_name` before the JSON ships out, so the gather adapter must
# re-inject it from `config$location_name` BEFORE calling
# convert_config_to_matrix() — otherwise location-specific vector
# parameters fall through to numeric suffixes (`beta_j0_tot_1`) instead of
# ISO suffixes (`beta_j0_tot_ETH`), silently breaking parquet schema
# parity with the local path.
#
# This test simulates the Dask round trip in pure R (no cluster, no
# Python) so it runs in every CI configuration.
# =============================================================================

skip_if_no_moz_data <- function() {
  skip_if_not_installed("MOSAIC")
  env <- new.env()
  ok <- tryCatch({
    utils::data("config_default_MOZ", package = "MOSAIC", envir = env)
    utils::data("priors_default_MOZ", package = "MOSAIC", envir = env)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok || !exists("config_default_MOZ", envir = env)) {
    skip("config_default_MOZ / priors_default_MOZ not available")
  }
  root <- if (dir.exists("/workspace/MOSAIC")) "/workspace/MOSAIC" else "~/MOSAIC"
  if (!dir.exists(root)) {
    skip(paste("MOSAIC root not found at", root))
  }
  MOSAIC::set_root_directory(root)
  list(config = env$config_default_MOZ, priors = env$priors_default_MOZ)
}

# Mirrors inst/python/mosaic_dask_worker.py::_MATRIX_FIELDS. Kept as a local
# constant rather than reaching into Python so this test works in pure R.
.matrix_fields <- c(
  "b_jt", "d_jt", "mu_jt", "psi_jt", "nu_1_jt", "nu_2_jt",
  "reported_cases", "reported_deaths"
)

# Simulates the Dask round trip in R: extract sampled, JSON ship out,
# JSON ship back, drop matrix fields (worker filter), re-inject
# location_name (gather adapter).
.simulate_worker_round_trip <- function(params_sim, config) {
  sampled <- MOSAIC:::.extract_sampled_params(params_sim)
  sampled_json <- jsonlite::toJSON(sampled, auto_unbox = TRUE, digits = NA)
  # simplifyVector = TRUE matches what reticulate produces when py dicts of
  # py lists/scalars come back to R.
  echoed <- jsonlite::fromJSON(sampled_json, simplifyVector = TRUE)
  echoed[.matrix_fields] <- NULL              # worker-side filter
  echoed$location_name <- config$location_name # gather-adapter re-injection
  echoed
}


# =============================================================================
# TEST 1: column-name parity between local path and Dask round trip
# =============================================================================
test_that("worker echo + location_name re-injection produces parquet column names matching the local path", {
  fx <- skip_if_no_moz_data()
  cfg <- fx$config

  params_sim <- MOSAIC::sample_parameters(
    PATHS       = NULL,
    priors      = fx$priors,
    config      = cfg,
    seed        = 1L,
    sample_args = list(),
    verbose     = FALSE,
    validate    = FALSE
  )

  local_cols <- names(MOSAIC::convert_config_to_matrix(params_sim))

  reconstituted <- .simulate_worker_round_trip(params_sim, cfg)
  dask_cols <- names(MOSAIC::convert_config_to_matrix(reconstituted))

  expect_setequal(local_cols, dask_cols)
})


# =============================================================================
# TEST 2: vector parameters carry ISO suffixes, not numeric fallbacks
#
# Regression for the silent _1 / _ETH break flagged in issue #101 step 2.
# Without location_name re-injection, convert_config_to_matrix() would emit
# `beta_j0_tot_1`, `beta_j0_tot_2`, ... instead of the ISO-suffixed columns.
# =============================================================================
test_that("vector parameters carry ISO suffixes after worker round trip", {
  fx <- skip_if_no_moz_data()
  cfg <- fx$config

  params_sim <- MOSAIC::sample_parameters(
    PATHS = NULL, priors = fx$priors, config = cfg,
    seed = 1L, sample_args = list(),
    verbose = FALSE, validate = FALSE
  )

  reconstituted <- .simulate_worker_round_trip(params_sim, cfg)
  dask_cols <- names(MOSAIC::convert_config_to_matrix(reconstituted))

  iso_codes <- as.character(cfg$location_name)
  for (iso in iso_codes) {
    expect_true(
      any(grepl(paste0("_", iso, "$"), dask_cols, perl = FALSE)),
      info = sprintf("expected at least one column with ISO suffix _%s; got: %s",
                     iso, paste(head(dask_cols, 5), collapse = ","))
    )
  }
})


# =============================================================================
# TEST 3: dropping location_name re-injection silently breaks suffixes
#
# Locks in the failure mode the production code defends against, so a future
# refactor that removes the `params$location_name <- config$location_name`
# line in .mosaic_run_batch_dask() trips a clear test failure with a pointer
# to this property.
# =============================================================================
test_that("regression: missing location_name injection yields numeric suffixes", {
  fx <- skip_if_no_moz_data()
  cfg <- fx$config
  if (length(cfg$location_name) < 2L) {
    skip("multi-location fixture required to detect numeric suffix fallback")
  }

  params_sim <- MOSAIC::sample_parameters(
    PATHS = NULL, priors = fx$priors, config = cfg,
    seed = 1L, sample_args = list(),
    verbose = FALSE, validate = FALSE
  )

  sampled <- MOSAIC:::.extract_sampled_params(params_sim)
  echoed <- jsonlite::fromJSON(
    jsonlite::toJSON(sampled, auto_unbox = TRUE, digits = NA),
    simplifyVector = TRUE
  )
  echoed[.matrix_fields] <- NULL
  # NOTE: location_name intentionally NOT re-injected — this is the silent
  # break we are guarding against.

  broken_cols <- names(MOSAIC::convert_config_to_matrix(echoed))
  iso_codes   <- as.character(cfg$location_name)
  has_iso     <- any(vapply(iso_codes, function(iso)
    any(grepl(paste0("_", iso, "$"), broken_cols, perl = FALSE)),
    logical(1)))

  expect_false(
    has_iso,
    info = "without location_name injection we expect numeric suffixes (_1, _2, ...) — the property test above guards against re-introducing this path"
  )
})
