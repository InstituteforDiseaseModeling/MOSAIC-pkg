# =============================================================================
# test-samples_parquet_schema.R
#
# The samples.parquet column-naming contract.
#
# Harvested from test-dask_worker_schema_parity.R when the Dask path was
# removed. That file was ~590 lines, most of it a simulated Python worker round
# trip plus tests of .mosaic_sample_and_serialize() /
# .mosaic_write_one_shard_dask() -- all of which ceased to exist with the Dask
# backend. But the property underneath was never about Dask: the parquet
# columns for per-location parameters must carry ISO suffixes
# (`beta_j0_tot_ETH`), never positional fallbacks (`beta_j0_tot_1`), because
# every downstream posterior/diagnostic join keys on the ISO form. That
# contract survives the migration and is asserted here directly against the
# path that remains.
#
# The deleted Dask assertions are not reproduced: a test that only checked
# "path A agrees with path B" is vacuous once path B is gone.
#
# Original regression context: issue #101 step 2 (silent _1 / _ETH break) and
# the per-location alpha_1 relocation in priors_default v15.16 /
# config_default v4.7 (CLAUDE.md lesson #12).
# =============================================================================

# skip_if_no_data() is centralized in helper-skips.R. It returns
# list(config = config_default, priors = priors_default) after setting the
# MOSAIC root; config_default is the global multi-location SSA config, which is
# cheap to exercise R-side (no engine sims) and gives broad ISO coverage.

# Mirrors run_MOSAIC.R's param_names_all: convert_config_to_matrix() output
# minus `seed`. Comparing against this rather than raw
# convert_config_to_matrix() output keeps the test aligned with the columns
# actually written to samples.parquet.
.parquet_columns <- function(config_like) {
  pv <- MOSAIC::convert_config_to_matrix(config_like)
  if ("seed" %in% names(pv)) pv <- pv[names(pv) != "seed"]
  names(pv)
}

.sampled_config <- function(fx, seed = 1L) {
  MOSAIC::sample_parameters(
    PATHS       = NULL,
    priors      = fx$priors,
    config      = fx$config,
    seed        = seed,
    sample_args = list(),
    verbose     = FALSE,
    validate    = FALSE
  )
}

test_that("per-location parameters get ISO-suffixed parquet columns", {
  fx <- skip_if_no_data()
  iso_codes <- as.character(fx$config$location_name)
  expect_gt(length(iso_codes), 1L)

  cols <- .parquet_columns(.sampled_config(fx))

  for (iso in iso_codes) {
    expect_true(
      any(grepl(paste0("_", iso, "$"), cols)),
      info = sprintf("no column carries ISO suffix _%s; first columns: %s",
                     iso, paste(utils::head(cols, 5), collapse = ", "))
    )
  }
})

test_that("alpha_1 expands to per-ISO columns with no scalar fallback", {
  # alpha_1 became a per-location vector in priors_default v15.16; a scalar or
  # positionally-suffixed alpha_1 column means the relocation regressed.
  fx <- skip_if_no_data()
  iso_codes <- as.character(fx$config$location_name)

  cols <- .parquet_columns(.sampled_config(fx))

  expect_true(all(paste0("alpha_1_", iso_codes) %in% cols))
  expect_false("alpha_1" %in% cols)
  expect_false("alpha_1_1" %in% cols)
})

test_that("positional suffixes appear only when location_name is absent", {
  # Pins the failure mode the ISO-suffix contract defends against:
  # convert_config_to_matrix() falls back to _1 / _2 when it cannot see
  # location_name. Any code path that drops location_name from a config before
  # serialising it silently breaks every downstream ISO join, so the fallback
  # is asserted to exist rather than left as folklore.
  fx <- skip_if_no_data()
  iso_codes <- as.character(fx$config$location_name)
  if (length(iso_codes) < 2L) {
    skip("multi-location fixture required to detect positional fallback")
  }

  stripped <- .sampled_config(fx)
  stripped$location_name <- NULL
  cols <- names(MOSAIC::convert_config_to_matrix(stripped))

  has_iso <- any(vapply(iso_codes, function(iso)
    any(grepl(paste0("_", iso, "$"), cols)), logical(1)))
  expect_false(has_iso)
  expect_true(any(grepl("_1$", cols)))
})

test_that("the column schema is stable across seeds", {
  # Column names must be a function of the config's shape, not of the drawn
  # values -- otherwise parquet shards from different sims cannot be bound.
  fx <- skip_if_no_data()
  expect_setequal(.parquet_columns(.sampled_config(fx, seed = 1L)),
                  .parquet_columns(.sampled_config(fx, seed = 99L)))
})
