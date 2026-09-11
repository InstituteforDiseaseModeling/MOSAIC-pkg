# =============================================================================
# test-laser_engine_replay.R
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

fixture_path <- function(name) {
  testthat::test_path("fixtures", paste0(name, ".rds"))
}

skip_if_no_fixture <- function(name) {
  if (!file.exists(fixture_path(name))) {
    testthat::skip(sprintf("replay fixture '%s' not committed", name))
  }
}

# -----------------------------------------------------------------------------
# Replay parity
# -----------------------------------------------------------------------------

test_that("replay reproduces the oracle's channels bit-for-bit", {

  skip_if_no_fixture("replay_susceptible_census")
  fx <- laser_read_fixture(fixture_path("replay_susceptible_census"))

  res <- run_LASER_R(
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
  fx <- laser_read_fixture(fixture_path("replay_susceptible_census"))
  comps <- as.character(fx$meta$components)

  # A run that completes without error has already asserted forward
  # exhaustion (no draw past the end of the record) inside .laser_consume()
  # and backward exhaustion (no unused records) inside
  # laser_assert_replay_complete(). Truncating the record must therefore fail:
  # a port that silently skipped draws would pass a naive replay test right up
  # to the point where the offsets happened to realign.
  truncated <- fx
  keep <- seq_len(length(fx$calls$tick) - 1L)
  truncated$calls <- lapply(fx$calls, function(v) v[keep])

  expect_error(
    run_LASER_R(config = fx$meta$config_list, seed = fx$meta$seed,
                components = comps, rng = "replay", record = truncated),
    "Replay record exhausted"
  )

  # And a record with a spare call at the end must fail the other way.
  padded <- fx
  n <- length(fx$calls$tick)
  padded$calls <- lapply(fx$calls, function(v) c(v, v[n]))
  expect_error(
    run_LASER_R(config = fx$meta$config_list, seed = fx$meta$seed,
                components = comps, rng = "replay", record = padded),
    "not fully consumed"
  )
})

test_that("replay rejects a draw the oracle did not make", {

  skip_if_no_fixture("replay_susceptible_census")
  fx <- laser_read_fixture(fixture_path("replay_susceptible_census"))
  comps <- as.character(fx$meta$components)

  # Perturb one recorded probability well beyond tolerance. The engine must
  # notice that it asked for a different draw than the oracle did -- this is
  # the assertion that makes replay a proof rather than a playback.
  nudged <- fx
  nudged$values$param[1L] <- fx$values$param[1L] * 1.1 + 1e-3
  expect_error(
    run_LASER_R(config = fx$meta$config_list, seed = fx$meta$seed,
                components = comps, rng = "replay", record = nudged),
    "probability differs"
  )

  # Perturb a binomial trial count. `n` is compared exactly, so even 1 fails.
  nudged_n <- fx
  nudged_n$values$n[1L] <- fx$values$n[1L] + 1
  expect_error(
    run_LASER_R(config = fx$meta$config_list, seed = fx$meta$seed,
                components = comps, rng = "replay", record = nudged_n),
    "binomial n differs"
  )

  # Shift a recorded tick. Catches a mis-ordered phase or a loop off-by-one,
  # which is the likeliest error and the hardest to see in the output.
  nudged_tick <- fx
  nudged_tick$calls$tick[1L] <- fx$calls$tick[1L] + 5L
  expect_error(
    run_LASER_R(config = fx$meta$config_list, seed = fx$meta$seed,
                components = comps, rng = "replay", record = nudged_tick),
    "tick mismatch"
  )
})

# -----------------------------------------------------------------------------
# Draw-site coverage
# -----------------------------------------------------------------------------

test_that("the draw-site registry matches the oracle's site map", {
  # Both sides enumerate the 22 active draw sites of laser-cholera v0.16.1.
  # If one gains an entry and the other does not, replay would fail with an
  # unmapped-site error at run time; failing here says why.
  expect_setequal(unname(.LASER_ORACLE_SITE_MAP), .LASER_DRAW_SITES)
  expect_length(.LASER_DRAW_SITES, 22L)
  expect_false(anyDuplicated(.LASER_DRAW_SITES) > 0L)
})

test_that("a run reports which draw sites it exercised", {

  skip_if_no_fixture("replay_susceptible_census")
  fx <- laser_read_fixture(fixture_path("replay_susceptible_census"))

  res <- run_LASER_R(config = fx$meta$config_list, seed = fx$meta$seed,
                     components = as.character(fx$meta$components),
                     rng = "replay", record = fx)

  cov <- attr(res, "laser_coverage")
  expect_s3_class(cov, "data.frame")
  expect_identical(nrow(cov), 22L)
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

test_that("requesting an unported component errors rather than being skipped", {
  # A silently short pipeline would look green while simulating the wrong
  # model, which is the failure mode CLAUDE.md lesson #6 is about.
  expect_error(
    run_LASER_R(config = list(), components = "HumanToHuman"),
    "not yet ported"
  )
})
