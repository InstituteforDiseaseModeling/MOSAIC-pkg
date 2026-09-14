# =============================================================================
# test-sim_rng_contract.R
#
# The engine's RNG contract.
#
# The Python engine owned an isolated np.random.Generator. A naive R port
# calling set.seed() would mutate the caller's global .Random.seed -- an
# observable API change -- and inside a PSOCK worker it would make results
# depend on how much randomness anything else in the worker had consumed
# first. That is a reproducibility bug that no parity test against the oracle
# would ever catch, because the oracle does not have the problem.
#
# The contract: a run's output is determined solely by seed + config, and
# calling the engine leaves the caller's stream exactly as it was.
# =============================================================================

rng_config <- function(npatches = 4L, nticks = 20L) {
  list(
    date_start = "2023-01-01",
    date_stop = as.character(as.Date("2023-01-01") + nticks - 1L),
    location_name = paste0("L", seq_len(npatches)),
    S_j_initial = rep(5000L, npatches),
    b_jt = matrix(3e-5, nrow = npatches, ncol = nticks),
    d_jt = matrix(3e-5, nrow = npatches, ncol = nticks)
  )
}

run_once <- function(seed) {
  run_simulation(rng_config(), seed = seed,
              components = c("Susceptible", "Census"), quiet = TRUE)
}

# -----------------------------------------------------------------------------
# Determinism
# -----------------------------------------------------------------------------

test_that("the same seed gives the same run, twice in one session", {
  a <- run_once(7L)
  b <- run_once(7L)
  expect_identical(a$results$S, b$results$S)
  expect_identical(a$results$births, b$results$births)
})

test_that("different seeds give different runs", {
  # Guards against the opposite failure: a seed that is accepted and ignored.
  a <- run_once(7L)
  b <- run_once(8L)
  expect_false(identical(a$results$S, b$results$S))
})

test_that("interleaved unrelated draws do not change the run", {
  # This is the test that fails if the engine inherits the caller's stream
  # instead of establishing its own.
  set.seed(1L)
  a <- run_once(7L)

  set.seed(1L)
  invisible(runif(1000L))
  b <- run_once(7L)

  expect_identical(a$results$S, b$results$S)
})

test_that("the caller's RNG stream survives a run untouched", {
  set.seed(99L)
  before <- .Random.seed
  expected_next <- { set.seed(99L); runif(3L) }

  set.seed(99L)
  invisible(run_once(7L))
  expect_identical(.Random.seed, before)
  expect_identical(runif(3L), expected_next)
})

test_that("a caller with no RNG state still has none afterwards", {
  # set.seed() has never been called in a fresh session, so .Random.seed does
  # not exist. Restoring must remove it again rather than leaving one behind.
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    rm(".Random.seed", envir = globalenv())
  }
  invisible(run_once(7L))
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("a caller's non-default RNGkind is restored, and does not change results", {
  prior <- RNGkind()
  on.exit(suppressWarnings(do.call(RNGkind, as.list(prior))), add = TRUE)

  baseline <- run_once(7L)

  suppressWarnings(RNGkind(kind = "Wichmann-Hill"))
  switched <- run_once(7L)
  expect_identical(RNGkind()[1], "Wichmann-Hill")

  # The engine sets its generator explicitly rather than inheriting it, so a
  # caller who has switched generators gets the same answer as one who has not.
  expect_identical(baseline$results$S, switched$results$S)
})

# -----------------------------------------------------------------------------
# Parallel reproducibility
# -----------------------------------------------------------------------------

test_that("PSOCK workers reproduce sequential results seed-for-seed", {
  skip_on_cran()
  skip_if_not_installed("parallel")
  # PSOCK workers are fresh R sessions: they can only reach the engine through
  # an INSTALLED package, not through devtools::load_all()'s in-memory
  # namespace. Under `devtools::test()` on a checkout with no installed MOSAIC
  # this would otherwise fail with "there is no package called 'MOSAIC'", which
  # says nothing about reproducibility. It runs under R CMD check, where the
  # package is installed to the check library.
  #
  # `skip_if_not_installed()` is the wrong detector here: under load_all the
  # namespace is registered, so requireNamespace() succeeds for a package that
  # is not on disk anywhere. Ask the library paths directly.
  skip_if(length(find.package("MOSAIC", lib.loc = .libPaths(), quiet = TRUE)) == 0L,
          "MOSAIC is not installed; PSOCK workers cannot load it")

  seeds <- 1:6
  sequential <- lapply(seeds, function(s) run_once(s)$results$S)

  cl <- parallel::makeCluster(2L, type = "PSOCK")
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, suppressMessages(library(MOSAIC)))
  parallel::clusterExport(cl, c("rng_config", "run_once"), envir = environment())
  parallel <- parallel::clusterApply(cl, seeds, function(s) run_once(s)$results$S)

  # Results must not depend on which worker picked up which seed, nor on the
  # order in which the worker processed them.
  expect_identical(parallel, sequential)
})

# -----------------------------------------------------------------------------
# Provenance
# -----------------------------------------------------------------------------

test_that("a result records what produced it", {
  res <- run_once(7L)
  prov <- attr(res, "sim_provenance")
  expect_identical(prov$r_version, R.version.string)
  expect_length(prov$rng_kind, 3L)
  expect_identical(res$seed, 7L)
})

test_that("the seed falls back to config$seed and then to 123", {
  cfg <- rng_config()
  cfg$seed <- 555L
  expect_identical(run_simulation(cfg, components = c("Susceptible", "Census"))$seed, 555L)
  # an explicit argument wins over the config
  expect_identical(run_simulation(cfg, seed = 1L,
                               components = c("Susceptible", "Census"))$seed, 1L)
  cfg$seed <- NULL
  expect_identical(run_simulation(cfg, components = c("Susceptible", "Census"))$seed, 123L)
})

# -----------------------------------------------------------------------------
# Draw-site registry consistency (v0.65.0)
#
# The first version of these tables mislabelled five infectious.py draw sites,
# omitted a real one (reported_deaths) and invented a phantom one (sigma_split,
# which is np.round(sigma * progressing), not a draw). The total still came to
# 22, so any check that counted sites rather than comparing them passed. These
# tests compare membership, which is what actually catches it.
#
# They are pure R and need no oracle. The stronger check -- re-deriving the site
# list from the laser-cholera source and diffing per site -- lives in
# claude/oracle/verify_draw_sites.py, which needs a checkout of the oracle at the
# commit pinned in fixtures/ORACLE.md.
# -----------------------------------------------------------------------------

test_that("the draw-site registry and the oracle site map describe the same sites", {
  sites <- MOSAIC:::.SIM_DRAW_SITES
  map   <- MOSAIC:::.SIM_ORACLE_SITE_MAP

  expect_setequal(sites, unname(map))
  expect_equal(length(sites), length(map))
})

test_that("every draw site is unique and well-formed", {
  sites <- MOSAIC:::.SIM_DRAW_SITES
  map   <- MOSAIC:::.SIM_ORACLE_SITE_MAP

  expect_false(anyDuplicated(sites) > 0L)
  expect_false(anyDuplicated(unname(map)) > 0L)
  expect_false(anyDuplicated(names(map)) > 0L)

  # "<phase>/<what>", and the oracle keys are "<file>.py:<line>".
  expect_true(all(grepl("^[a-z_]+/[a-z0-9_]+$", sites)))
  expect_true(all(grepl("^[a-z_]+\\.py:[0-9]+$", names(map))))

  # The phase prefix must name a component that exists in the pipeline.
  phases <- unique(sub("/.*$", "", sites))
  expect_setequal(phases, tolower(c(
    "susceptible", "exposed", "recovered", "infectious", "vaccinated",
    "humantohuman", "envtohuman", "environmental")))
})

test_that("sigma_split is not a draw site (it is np.round, not a PRNG call)", {
  # Guards the specific phantom that made the original count look right.
  expect_false("infectious/sigma_split" %in% MOSAIC:::.SIM_DRAW_SITES)
  expect_true("infectious/reported_deaths" %in% MOSAIC:::.SIM_DRAW_SITES)
})

# -----------------------------------------------------------------------------
# The fast draw path (v0.70.0)
#
# `sim_draws()` now branches on mode once and `.sim_binom`/`.sim_pois` carry a
# thin production path that skips the replay machinery. These tests pin the
# things that path could plausibly break, because none of them would show up as
# a parity failure against the replay fixtures -- those exercise the SLOW path.
# -----------------------------------------------------------------------------

test_that("the fast path is taken in rng mode and not in replay mode", {
  expect_true(MOSAIC:::sim_draws(mode = "rng", seed = 1L)$fast)
  rec <- sim_read_fixture(test_path("fixtures", "replay_single_location.rds"))
  expect_false(MOSAIC:::sim_draws(mode = "replay", record = rec)$fast)
})

test_that(".sim_pois honours `npatches` when lambda is scalar", {
  # The bug this guards: reading the draw count off `length(lambda)` instead of
  # the `npatches` argument silently returns ONE variate where the engine
  # expects npatches, which would corrupt every patch but the first.
  ctl <- MOSAIC:::sim_draws(mode = "rng", seed = 1L)
  out <- MOSAIC:::.sim_pois(ctl, "environmental/decay", 5, npatches = 7L)
  expect_length(out, 7L)
})

test_that(".sim_pois does not coerce, so a huge lambda survives", {
  # The environmental shedding rate is `zeta_1 * Isym` with zeta_1 of order
  # 1e8, so lambda reaches ~1e12 and an `as.integer()` anywhere on this path
  # returns NA. This is also the difference that made R admissible where
  # NumPy's int64 Poisson raised ValueError on 2.6% of prior draws (v0.69.0).
  ctl <- MOSAIC:::sim_draws(mode = "rng", seed = 1L)
  out <- MOSAIC:::.sim_pois(ctl, "environmental/shedding_sym", 1e12, npatches = 4L)
  expect_length(out, 4L)
  expect_true(all(is.finite(out)))
  expect_false(anyNA(out))
  expect_gt(min(out), 1e11)
})

test_that(".sim_binom recycles a scalar p exactly as a materialised one does", {
  # This identity is what makes dropping the `rep()` on the fast path
  # parity-preserving; recycling happens inside the sampler.
  n <- as.integer(rep(500L, 6L))
  a <- withr::with_seed(42L, stats::rbinom(6L, n, 0.03))
  b <- withr::with_seed(42L, stats::rbinom(6L, n, rep(0.03, 6L)))
  expect_identical(a, b)
})

test_that("an unknown draw site still errors on the fast path", {
  ctl <- MOSAIC:::sim_draws(mode = "rng", seed = 1L)
  expect_error(MOSAIC:::.sim_binom(ctl, "no/such/site", 10L, 0.1),
               "Unknown draw site")
  expect_error(MOSAIC:::.sim_pois(ctl, "no/such/site", 1, npatches = 2L),
               "Unknown draw site")
})

test_that("coverage is still counted on the fast path, and only for sites drawn", {
  ctl <- MOSAIC:::sim_draws(mode = "rng", seed = 1L)
  invisible(MOSAIC:::.sim_binom(ctl, "susceptible/births", as.integer(rep(10L, 3L)), 0.1))
  invisible(MOSAIC:::.sim_binom(ctl, "susceptible/births", as.integer(rep(10L, 3L)), 0.1))
  cov <- MOSAIC:::sim_draw_coverage(ctl)
  expect_identical(nrow(cov), length(MOSAIC:::.SIM_DRAW_SITES))
  expect_identical(cov$site, MOSAIC:::.SIM_DRAW_SITES)
  expect_identical(cov$n_calls[cov$site == "susceptible/births"], 2L)
  expect_identical(sum(cov$n_calls), 2L)
})

test_that("a full run still reports coverage over every site", {
  out <- run_once(3L)
  cov <- attr(out, "sim_coverage")
  expect_identical(cov$site, MOSAIC:::.SIM_DRAW_SITES)
  expect_type(cov$n_calls, "integer")
  expect_gt(sum(cov$n_calls), 0L)
})

test_that("tick/phase stamping is skipped in rng mode and kept in replay mode", {
  # `.sim_at()` exists only so a replay mismatch can name where it happened, so
  # it is a no-op in production. Anything instrumenting `ctl$tick` must force
  # replay mode.
  ctl_fast <- MOSAIC:::sim_draws(mode = "rng", seed = 1L)
  MOSAIC:::.sim_at(ctl_fast, 11L, "Susceptible")
  expect_true(is.na(ctl_fast$tick))
  expect_true(is.na(ctl_fast$phase))

  rec <- sim_read_fixture(test_path("fixtures", "replay_single_location.rds"))
  ctl_slow <- MOSAIC:::sim_draws(mode = "replay", record = rec)
  MOSAIC:::.sim_at(ctl_slow, 11L, "Susceptible")
  expect_identical(ctl_slow$tick, 11L)
  expect_identical(ctl_slow$phase, "Susceptible")
})
