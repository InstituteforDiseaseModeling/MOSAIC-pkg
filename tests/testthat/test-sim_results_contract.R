# =============================================================================
# test-sim_results_contract.R
#
# The RETURN CONTRACT of the R transmission engine: what run_simulation() promises
# its callers, independently of whether the numbers are right (that is Tier B,
# in test-sim_engine_replay.R).
#
# This matters because the contract is the migration's actual interface. The
# consumers -- calc_model_ensemble(), calc_model_likelihood(), the plot_*
# functions -- index these matrices positionally as [patch, time] and read
# per-field storage modes. Orientation and dtype are exactly the kind of thing
# that can flip in a refactor while every value stays correct, and CLAUDE.md
# lesson #12 is a case where a schema-shaped assumption drifted silently.
#
# These run free (not replayed): the contract must hold for an ordinary run,
# not only inside the test harness.
# =============================================================================

engine_run <- function() {
  fx <- readRDS(test_path("fixtures", "replay_full_pipeline.rds"))
  run_simulation(config = fx$meta$config_list, seed = 42L, quiet = TRUE)
}

test_that("the return has exactly params, results and seed", {
  out <- engine_run()
  expect_identical(names(out), c("params", "results", "seed"))
  expect_identical(out$seed, 42L)
  expect_type(out$params, "list")
  expect_type(out$results, "list")
})

test_that("all 28 channels are present, in the documented order", {
  out <- engine_run()
  # Identical, not setequal: the order is part of the contract, so that a
  # caller iterating positionally cannot be silently re-ordered underneath.
  expect_identical(names(out$results), MOSAIC:::SIM_CHANNELS)
  expect_length(out$results, 28L)
})

test_that("every channel is [npatches, time] with no dimnames", {
  out <- engine_run()
  np <- length(out$params$location_name)
  nt <- MOSAIC:::.sim_nticks(out$params)
  square <- MOSAIC:::SIM_CHANNELS_PASSTHROUGH

  for (nm in names(out$results)) {
    m <- out$results[[nm]]
    expect_true(is.matrix(m), info = nm)
    expect_identical(dim(m),
                     if (nm %in% square) c(np, np) else c(np, nt),
                     info = nm)
    # The Python return carries no dimnames and no consumer reads any. Adding
    # them looks harmless but changes identical() comparisons downstream.
    expect_null(dimnames(m), info = nm)
  }
})

test_that("storage mode is per field, and the two tables partition the channels", {
  out <- engine_run()

  # The tables are the documentation; asserting against them is what stops the
  # documentation from drifting away from the code.
  expect_setequal(c(MOSAIC:::SIM_CHANNELS_INTEGER, MOSAIC:::SIM_CHANNELS_DOUBLE),
                  MOSAIC:::SIM_CHANNELS)
  expect_length(intersect(MOSAIC:::SIM_CHANNELS_INTEGER,
                          MOSAIC:::SIM_CHANNELS_DOUBLE), 0L)

  for (nm in MOSAIC:::SIM_CHANNELS_INTEGER) {
    # Counts of people and events stay integer: they inherit the engine's
    # int32 rounding discipline, and a double here would let a fractional
    # person through unnoticed.
    expect_identical(storage.mode(out$results[[nm]]), "integer", info = nm)
  }
  for (nm in MOSAIC:::SIM_CHANNELS_DOUBLE) {
    expect_identical(storage.mode(out$results[[nm]]), "double", info = nm)
  }
})

test_that("a pipeline subset without DerivedValues returns neither diagnostic", {
  # The Python component allocates `spatial_hazard` and `coupling` in its own
  # __init__, so a subset that omits it returns neither channel. Returning the
  # zero-filled allocation would read as "no correlation anywhere" instead of
  # "not computed", which is the harder failure to notice.
  fx <- readRDS(test_path("fixtures", "replay_full_pipeline.rds"))
  out <- run_simulation(config = fx$meta$config_list, seed = 42L, quiet = TRUE,
                     components = setdiff(MOSAIC:::SIM_PIPELINE, "DerivedValues"))
  expect_false("spatial_hazard" %in% names(out$results))
  expect_false("coupling" %in% names(out$results))
  expect_length(out$results, 26L)
})

test_that("a run carries its provenance and draw-site coverage", {
  out <- engine_run()

  prov <- attr(out, "sim_provenance")
  expect_identical(prov$components, MOSAIC:::SIM_PIPELINE)
  expect_true(nzchar(prov$r_version))

  cov <- attr(out, "sim_coverage")
  expect_s3_class(cov, "data.frame")
  # A full free run exercises every draw site; a gap here means a branch that
  # no test is reaching.
  expect_setequal(cov$site, MOSAIC:::.SIM_DRAW_SITES)
  expect_true(all(cov$n_calls > 0L))
})

test_that("the end-of-run diagnostics obey their own shape rules", {
  out <- engine_run()
  np <- length(out$params$location_name)

  # coupling is a correlation matrix: symmetric, unit diagonal, in [-1, 1]
  # wherever it is defined at all.
  cpl <- out$results$coupling
  ok <- !is.nan(cpl)
  expect_equal(cpl[ok], t(cpl)[ok])
  expect_true(all(cpl[ok] >= -1 - 1e-12 & cpl[ok] <= 1 + 1e-12))
  varied <- !apply(is.nan(cpl), 1L, all)
  expect_equal(diag(cpl)[varied], rep(1, sum(varied)))

  # spatial_hazard is finite everywhere and bounded above by 1.
  #
  # It is NOT bounded below by 0, and that is the engine's behaviour rather
  # than a port artefact: the unconstrained two-harmonic seasonal envelope
  # `1 + a1 cos + b1 sin + a2 cos 2 + b2 sin 2` dips below zero for some
  # patches in the low season, so `beta_jt_human` goes negative and the hazard
  # follows it. `HumanToHuman` clamps its own rate with `pmax(..., 0)`;
  # `derivedvalues.py` has no such clamp, and the oracle produces negative
  # cells in exactly the same places (Tier B pins the values). Clamping here
  # would be a silent model change, so the sign is asserted as observed and
  # the underlying quirk is an upstream issue.
  h <- out$results$spatial_hazard
  expect_true(all(is.finite(h)))
  expect_true(all(h <= 1))
  expect_true(any(out$results$beta_jt_human < 0))
})

test_that("the engine does not disturb the caller's RNG stream", {
  # run_simulation() seeds its own stream and restores on exit. A caller that
  # loses its seed state gets an irreproducible calibration, which is the
  # worst kind of bug to find later.
  set.seed(99L)
  before <- .Random.seed
  invisible(engine_run())
  expect_identical(.Random.seed, before)
})

# -----------------------------------------------------------------------------
# Degenerate coupling cases
#
# The committed fixtures all have some patches varying and some not, so they
# never reach the two ends of .sim_coupling()'s branch. These do.
# -----------------------------------------------------------------------------

test_that("coupling is all-NaN when no patch's prevalence ever varies", {
  # Every patch flat -- a config with no seeded infection anywhere, which is a
  # legitimate run, not an error. Pearson correlation is undefined for all of
  # them, so there is nothing to fill in and no warning to emit.
  y <- matrix(0, nrow = 10L, ncol = 4L)
  expect_no_warning(cpl <- MOSAIC:::.sim_coupling(y))
  expect_identical(dim(cpl), c(4L, 4L))
  expect_true(all(is.nan(cpl)))
})

test_that("coupling isolates a single varying patch to its own diagonal cell", {
  y <- matrix(0, nrow = 10L, ncol = 3L)
  y[, 2L] <- seq_len(10L)
  cpl <- MOSAIC:::.sim_coupling(y)
  expect_equal(cpl[2L, 2L], 1)
  # A patch with no variation correlates with nothing, including itself.
  expect_true(all(is.nan(cpl[-2L, ])))
  expect_true(all(is.nan(cpl[, -2L])))
})

test_that("coupling of a single location is a 1 x 1 matrix", {
  # npatches = 1 is a real configuration (the single-location fixture) and
  # takes a different path through every reshaping step.
  expect_equal(MOSAIC:::.sim_coupling(matrix(seq_len(5L), ncol = 1L)),
               matrix(1, 1L, 1L))
  expect_true(all(is.nan(MOSAIC:::.sim_coupling(matrix(0, 5L, 1L)))))
})
