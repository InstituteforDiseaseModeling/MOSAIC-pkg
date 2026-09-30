# =============================================================================
# test-sim_engine_replay.R
#
# Tier B parity: the R transmission engine replayed against frozen recordings
# of the Python laser-cholera oracle.
#
# Each fixture holds every PRNG call the oracle made -- tick, phase, draw site,
# kind, n, probability/rate, result -- plus the result channels it produced.
# Replaying them removes the PRNG from the comparison entirely, so a failure
# localises to a tick, a phase and a draw site rather than to "the
# distributions look different".
#
# These run in pure R against committed fixtures. laser-cholera is NOT
# required and is never imported; see tests/testthat/fixtures/ORACLE.md for
# which oracle build produced them, and claude/oracle/ for the generator.
# =============================================================================

# -----------------------------------------------------------------------------
# Replay parity
# -----------------------------------------------------------------------------

test_that("replay reproduces the oracle's channels bit-for-bit", {

  skip_if_no_fixture("replay_susceptible_census")
  fx <- sim_read_fixture(fixture_path("replay_susceptible_census"))

  res <- run_simulation(
    config     = fx$meta$config_list,
    seed       = fx$meta$seed,
    components = as.character(fx$meta$components),
    rng        = "replay",
    record     = fx
  )

  expect_gt(length(fx$results), 0L)

  for (ch in names(fx$results)) {
    want <- fx$results[[ch]]
    got  <- res$results[[ch]]
    expect_false(is.null(got), info = sprintf("channel %s missing from R results", ch))
    expect_identical(dim(got), dim(want), info = sprintf("channel %s dims", ch))
    # Integer channels must be bit-identical -- no tolerance. A difference here
    # is a real arithmetic or rounding error, not float noise.
    expect_true(all(got == want),
                info = sprintf("channel %s: %d of %d elements differ",
                               ch, sum(got != want), length(want)))
  }
})

test_that("replay consumes the record exactly, in both directions", {

  skip_if_no_fixture("replay_susceptible_census")
  fx <- sim_read_fixture(fixture_path("replay_susceptible_census"))
  comps <- as.character(fx$meta$components)

  # A run that completes without error has already asserted forward
  # exhaustion (no draw past the end of the record) inside .sim_consume()
  # and backward exhaustion (no unused records) inside
  # sim_assert_replay_complete(). Truncating the record must therefore fail:
  # a port that silently skipped draws would pass a naive replay test right up
  # to the point where the offsets happened to realign.
  truncated <- fx
  keep <- seq_len(length(fx$calls$tick) - 1L)
  truncated$calls <- lapply(fx$calls, function(v) v[keep])

  expect_error(
    run_simulation(config = fx$meta$config_list, seed = fx$meta$seed,
                components = comps, rng = "replay", record = truncated),
    "Replay record exhausted"
  )

  # And a record with a spare call at the end must fail the other way.
  padded <- fx
  n <- length(fx$calls$tick)
  padded$calls <- lapply(fx$calls, function(v) c(v, v[n]))
  expect_error(
    run_simulation(config = fx$meta$config_list, seed = fx$meta$seed,
                components = comps, rng = "replay", record = padded),
    "not fully consumed"
  )
})

test_that("replay rejects a draw the oracle did not make", {

  skip_if_no_fixture("replay_susceptible_census")
  fx <- sim_read_fixture(fixture_path("replay_susceptible_census"))
  comps <- as.character(fx$meta$components)

  # Perturb one recorded probability well beyond tolerance. The engine must
  # notice that it asked for a different draw than the oracle did -- this is
  # the assertion that makes replay a proof rather than a playback.
  nudged <- fx
  nudged$values$param[1L] <- fx$values$param[1L] * 1.1 + 1e-3
  expect_error(
    run_simulation(config = fx$meta$config_list, seed = fx$meta$seed,
                components = comps, rng = "replay", record = nudged),
    "probability differs"
  )

  # Perturb a binomial trial count. `n` is compared exactly, so even 1 fails.
  nudged_n <- fx
  nudged_n$values$n[1L] <- fx$values$n[1L] + 1
  expect_error(
    run_simulation(config = fx$meta$config_list, seed = fx$meta$seed,
                components = comps, rng = "replay", record = nudged_n),
    "binomial n differs"
  )

  # Shift a recorded tick. Catches a mis-ordered phase or a loop off-by-one,
  # which is the likeliest error and the hardest to see in the output.
  nudged_tick <- fx
  nudged_tick$calls$tick[1L] <- fx$calls$tick[1L] + 5L
  expect_error(
    run_simulation(config = fx$meta$config_list, seed = fx$meta$seed,
                components = comps, rng = "replay", record = nudged_tick),
    "tick mismatch"
  )
})

# -----------------------------------------------------------------------------
# Draw-site coverage
# -----------------------------------------------------------------------------

test_that("the draw-site registry matches the oracle's site map", {
  # The oracle map enumerates the 22 active draw sites of laser-cholera v0.16.1.
  # The R registry is that set PLUS any deliberate R-only divergence, each named
  # in .SIM_RNG_ONLY_SITES and drawn only in "rng" mode (v0.89.0's stochastic
  # sigma split is the first). If the two drift apart in any OTHER way, replay
  # fails with an unmapped-site error at run time; failing here says why.
  replayable <- setdiff(.SIM_DRAW_SITES, MOSAIC:::.SIM_RNG_ONLY_SITES)
  expect_setequal(unname(.SIM_ORACLE_SITE_MAP), replayable)
  expect_length(replayable, 22L)
  expect_false(anyDuplicated(.SIM_DRAW_SITES) > 0L)
})

test_that("a run reports which draw sites it exercised", {

  skip_if_no_fixture("replay_susceptible_census")
  fx <- sim_read_fixture(fixture_path("replay_susceptible_census"))

  res <- run_simulation(config = fx$meta$config_list, seed = fx$meta$seed,
                     components = as.character(fx$meta$components),
                     rng = "replay", record = fx)

  cov <- attr(res, "sim_coverage")
  expect_s3_class(cov, "data.frame")
  # Coverage reports every registered site, including the R-only one, which is
  # structurally 0 under replay because replay uses the oracle's deterministic
  # split. That zero is the contract, not a gap -- see the assertion below.
  expect_identical(nrow(cov), length(MOSAIC:::.SIM_DRAW_SITES))
  expect_identical(
    cov$n_calls[cov$site == "infectious/sigma_split"], 0L)
  # A short run records calls, not sites: only the components in play fire.
  # The point of reporting coverage is that A-2 can gate on 22/22 rather than
  # on a green short run, which would hide untested conditional branches.
  expect_true(all(cov$n_calls[cov$site %in% c("susceptible/non_disease_deaths",
                                              "susceptible/births")] > 0L))
  expect_true(any(cov$n_calls == 0L))
})

# -----------------------------------------------------------------------------
# Engine guards
# -----------------------------------------------------------------------------

test_that("the dispatch table covers the whole pipeline", {
  # With A-3 the port is complete, so no component name can reach the
  # unported-component guard. That makes this equality the thing actually
  # worth asserting: a component added to SIM_PIPELINE without an
  # implementation would otherwise be caught only at run time, by a user.
  expect_setequal(names(MOSAIC:::.SIM_PHASE_FUNCTIONS), MOSAIC:::SIM_PIPELINE)
})

test_that("requesting an unported component errors rather than being skipped", {
  # A silently short pipeline would look green while simulating the wrong
  # model, which is the failure mode CLAUDE.md lesson #6 is about.
  #
  # This named "HumanToHuman" until A-2 ported it and "DerivedValues" until
  # A-3 did. Every component is ported now, so the guard is exercised by
  # emptying the dispatch table rather than by naming a real component --
  # deleting the test instead would retire the check that stops a
  # half-finished pipeline from passing.
  local_mocked_bindings(
    .SIM_PHASE_FUNCTIONS = list(Susceptible = sim_phase_susceptible),
    .package = "MOSAIC")
  expect_error(
    run_simulation(config = list(), components = c("Susceptible", "Census")),
    "not yet ported"
  )
})

# =============================================================================
# Tier B on the full pipeline
#
# All ten components. These are the tests that certify the port: the engine is
# driven through the oracle's recorded draw sequence and every draw argument
# and every result channel is compared.
#
# What "correct" means here, precisely:
#   * Every draw is at the expected tick, phase and site, with a bit-identical
#     integer `n`. A one-person difference in `n` decorrelates the sequence, so
#     this is compared exactly, not to a tolerance.
#   * Every INTEGER result channel is bit-identical. This is the strongest claim
#     available and it covers everything calibration consumes -- the
#     compartments, incidence, and reported cases/deaths.
#   * Float channels are compared with a scale-aware tolerance; see
#     .sim_site_tol() in R/sim_rng.R for why, and for the one draw site
#     (environmental/decay) that needs its own.
# =============================================================================

# Taken from the package rather than restated, so a component added to the
# pipeline cannot silently go untested here.
PORTED <- MOSAIC:::SIM_PIPELINE

# Scale-aware comparison, matching the replay assertion's criterion:
#   |r - o| <= rtol * (|o| + max|o|)
# The max|o| floor is what makes near-zero references (low-season Lambda,
# empty-reservoir Psi) comparable at all -- see a2_required_tolerance.R.
#
# One channel carries its own tolerance, measured rather than chosen:
#
#   spatial_hazard -- inherits `beta_jt_human`'s float32 error rather than its
#     own. The seasonal envelope `beta_j0_hum * (1 + a1 cos + b1 sin + ...)`
#     passes close to zero in the low season, where computing it in float32
#     (as the oracle does) cancels to a relative error of 7.0e-4. The hazard is
#     linear in beta there, and its own relative error at full length is
#     7.0e-4 -- the same number to within one percent, which is what identifies
#     beta as the source. `beta_jt_human` itself still passes at 1e-5 because
#     its near-zero cells are rescued by the max|o| floor; the hazard's are not,
#     because hazard magnitude tracks population and prevalence, not beta.
#
# The measured requirement over the full-length fixture is 1.5e-4; 1e-3 leaves
# the usual order of magnitude of headroom. Every other float channel, W and
# pi_ij included, clears the 1e-5 default.
CHANNEL_TOL <- c(spatial_hazard = 1e-3)

expect_channels_match <- function(got, ref, rtol = 1e-5) {
  int_ch <- MOSAIC:::SIM_CHANNELS_INTEGER
  for (nm in names(ref)) {
    expect_false(is.null(got[[nm]]), info = paste("channel missing from R:", nm))
    expect_identical(dim(got[[nm]]), dim(ref[[nm]]), info = paste("dim:", nm))
    if (nm %in% int_ch) {
      # Bit-identical, no tolerance.
      expect_identical(as.integer(got[[nm]]), as.integer(ref[[nm]]),
                       info = paste("integer channel:", nm))
    } else {
      o <- ref[[nm]]
      # `coupling` is NaN wherever a patch's prevalence never varied. The
      # positions must agree exactly -- a NaN where the oracle has a number
      # (or the reverse) is a real disagreement about whether a patch moved,
      # and comparing only the finite cells would hide it.
      expect_identical(is.nan(got[[nm]]), is.nan(o), info = paste("NaN cells:", nm))
      k <- !is.nan(o)
      o <- o[k]
      r <- got[[nm]][k]
      tol <- if (nm %in% names(CHANNEL_TOL)) CHANNEL_TOL[[nm]] else rtol
      scale <- max(abs(o))
      if (scale == 0) {
        # A channel the oracle left identically zero (e.g. an unused dose
        # schedule). The scale-aware ratio would be 0/0, so require exact zero
        # rather than skipping -- a non-zero R value here is a real bug.
        expect_equal(max(abs(r)), 0, info = paste("all-zero channel:", nm))
      } else {
        expect_lte(max(abs(r - o) / (abs(o) + scale)), tol)
      }
    }
  }
}

replay_fixture <- function(file) {
  skip_if_no_fixture(sub("\\.rds$", "", file))
  fx <- readRDS(test_path("fixtures", file))
  out <- run_simulation(config = fx$meta$config_list, seed = fx$meta$seed,
                     quiet = TRUE, components = PORTED,
                     rng = "replay", record = fx)
  list(fx = fx, out = out)
}

# -----------------------------------------------------------------------------
# Fixture inventory
# -----------------------------------------------------------------------------
# Every Tier B assertion above degrades to a skip when its fixture is absent,
# which is the right behaviour for a fresh clone but the wrong thing to leave
# silent: the port's entire parity evidence would evaporate into green skips.
# This asserts the inventory once, so a fixture that is lost, unstaged or
# renamed fails here instead of quietly reducing what Tier B covers.
test_that("every replay fixture Tier B depends on is present", {
  required <- c("replay_susceptible_census", "replay_full_pipeline",
                "replay_single_location", "replay_full_length")
  missing <- required[!file.exists(vapply(required, fixture_path, ""))]
  expect_equal(missing, character(0),
               info = paste("regenerate with claude/oracle/dump_fixture.py, or",
                            "commit them:", paste(missing, collapse = ", ")))
})

test_that("Tier B: the full ported pipeline replays the oracle draw-for-draw (40 patches)", {
  r <- replay_fixture("replay_full_pipeline.rds")
  # Reaching here means every draw matched: run_simulation asserts each one and
  # sim_assert_replay_complete() then requires the record be fully consumed,
  # so neither a skipped nor an extra draw can pass.
  expect_equal(r$fx$meta$nticks, 60L)
  expect_equal(r$fx$meta$npatches, 40L)
  expect_equal(length(r$fx$calls$site), 1315L)

  # All 22 REPLAYABLE draw sites exercised. A site with no calls is untested
  # code wearing a passing test, so this is asserted rather than assumed.
  #
  # The R-only sites are excluded by construction: under replay they are not
  # drawn at all, because drawing them would consume a variate the fixture has
  # no record of and desynchronise everything after it. They are covered
  # distributionally by test-sigma-split.R instead -- CLAUDE.md lesson #18(v)
  # says a parity harness only covers the modes it exercises, and this is the
  # mode it cannot.
  replayable <- setdiff(MOSAIC:::.SIM_DRAW_SITES, MOSAIC:::.SIM_RNG_ONLY_SITES)
  cov <- attr(r$out, "sim_coverage")
  expect_equal(sum(cov$n_calls > 0L), length(replayable))
  expect_setequal(cov$site[cov$n_calls > 0L], replayable)
})

test_that("Tier B: all 28 result channels match, integer channels bit-identically (40 patches)", {
  r <- replay_fixture("replay_full_pipeline.rds")
  expect_equal(length(r$fx$results), 28L)
  expect_setequal(names(r$fx$results), MOSAIC:::SIM_CHANNELS)
  expect_channels_match(r$out$results, r$fx$results)
})

test_that("Tier B: the end-of-run diagnostics match, including where coupling is undefined", {
  # spatial_hazard and coupling are computed once, on the final tick, from the
  # whole run -- so unlike every other channel they cannot be partially right:
  # a one-row slicing error moves every cell.
  r <- replay_fixture("replay_full_pipeline.rds")

  expect_identical(dim(r$out$results$spatial_hazard), c(40L, 60L))
  expect_identical(dim(r$out$results$coupling), c(40L, 40L))

  # Patches whose prevalence never changed have no correlation with anything;
  # the oracle leaves their rows and columns NaN. That the two engines agree on
  # WHICH patches those are is the real assertion -- it says they agree on
  # which patches the epidemic reached.
  cpl <- r$out$results$coupling
  expect_identical(is.nan(cpl), is.nan(r$fx$results$coupling))
  expect_true(any(is.nan(cpl)))
  varied <- !apply(is.nan(cpl), 1L, all)
  expect_equal(diag(cpl)[varied], rep(1, sum(varied)))
  expect_equal(cpl[varied, varied], t(cpl[varied, varied]))

  # The first column is tick 1, not the seed row: spatial_hazard's t = 0 row is
  # trimmed by the results contract, so an off-by-one here would show up as a
  # zero column the oracle does not have.
  expect_false(all(r$out$results$spatial_hazard[, 1L] == 0))
})

test_that("Tier B: the single-location degenerate case replays and matches", {
  # npatches = 1 takes different paths through the per-patch reshaping (the
  # dose-one donor matrix, the pi_ij column sums), which a 40-patch fixture
  # cannot exercise.
  r <- replay_fixture("replay_single_location.rds")
  expect_equal(r$fx$meta$npatches, 1L)
  expect_channels_match(r$out$results, r$fx$results)
})

test_that("Tier B: the full-length run matches over all 1398 ticks", {
  # The regression anchor. 60 ticks is enough to cover every draw site but not
  # to expose anything that accumulates -- the float32 reservoir drift in W
  # first breached a naive 1e-6 relative tolerance at tick 37 of this config,
  # and the Vaccinated pro-rata rounding bug that this suite caught did not
  # surface until tick 99.
  r <- replay_fixture("replay_full_length.rds")
  expect_equal(r$fx$meta$nticks, 1398L)
  expect_equal(length(r$fx$calls$site), 30751L)
  expect_channels_match(r$out$results, r$fx$results)
})

test_that("replay is strict in both directions: a truncated record errors", {
  skip_if_no_fixture("replay_full_pipeline")
  fx <- readRDS(test_path("fixtures", "replay_full_pipeline.rds"))
  short <- fx
  keep <- seq_len(length(fx$calls$site) - 5L)
  short$calls <- lapply(fx$calls, function(v) v[keep])
  expect_error(
    run_simulation(config = fx$meta$config_list, seed = fx$meta$seed, quiet = TRUE,
                components = PORTED, rng = "replay", record = short),
    "Replay record exhausted")
})

test_that("a component name that is not in the pipeline errors", {
  skip_if_no_fixture("replay_full_pipeline")
  fx <- readRDS(test_path("fixtures", "replay_full_pipeline.rds"))
  expect_error(
    run_simulation(config = fx$meta$config_list, components = c("Susceptible", "Nonsense")),
    "Unknown component")
})

test_that("phase order is the engine's, not the caller's argument order", {
  # Census before Susceptible would sum the previous tick's compartments. The
  # engine sorts the requested subset into the canonical pipeline order, so a
  # scrambled `components` still runs correctly.
  skip_if_no_fixture("replay_full_pipeline")
  fx <- readRDS(test_path("fixtures", "replay_full_pipeline.rds"))
  scrambled <- rev(PORTED)
  out <- run_simulation(config = fx$meta$config_list, seed = fx$meta$seed, quiet = TRUE,
                     components = scrambled, rng = "replay", record = fx)
  expect_channels_match(out$results, fx$results)

  expect_error(
    run_simulation(config = fx$meta$config_list, components = c("Susceptible", "Susceptible")),
    "more than once")
})
