# Invariant coverage for sample_parameters() and its initial-conditions core
# (review Batch 4 / B2-10: sample_parameters had only beta/zeta/rho sub-tests,
# no test of the IC sum-to-population invariant or seed determinism).
library(testthat)
library(MOSAIC)

# Set root directory (required for the end-to-end sample_parameters() tests,
# which read default priors/data). The .props_to_counts unit tests below need
# no data and always run.
tryCatch(set_root_directory("~/MOSAIC"), error = function(e) NULL)

props_to_counts <- getFromNamespace(".props_to_counts", "MOSAIC")
.IC <- c("S", "V1", "V2", "E", "I", "R")

# ---------------------------------------------------------------------------
# .props_to_counts — Hamilton apportionment: integer counts that sum EXACTLY
# to the population N per location, for any proportion vector (incl. drift).
# CI-safe (pure R, no data).
# ---------------------------------------------------------------------------
test_that(".props_to_counts yields non-negative integer counts summing exactly to N", {
  set.seed(1)
  for (N in c(1L, 2L, 3L, 7L, 101L, 1000003L)) {
    for (trial in seq_len(40)) {
      raw <- runif(length(.IC), 0, 1)
      sp  <- matrix(raw / sum(raw), nrow = 1, dimnames = list(NULL, .IC))  # sums to 1
      out <- props_to_counts(list(N_j_initial = N), sp, locations = "X", verbose = FALSE)
      counts <- vapply(.IC, function(c) out[[paste0(c, "_j_initial")]], numeric(1))
      expect_true(all(counts >= 0),            info = paste("N =", N))
      expect_true(all(counts == floor(counts)), info = paste("N =", N))   # whole numbers
      expect_equal(sum(counts), N)                                        # exact sum-to-N
    }
  }
  # the function returns integer-typed count vectors
  out1 <- props_to_counts(list(N_j_initial = 100L),
                          matrix(rep(1 / 6, 6), 1, dimnames = list(NULL, .IC)),
                          locations = "X", verbose = FALSE)
  expect_true(is.integer(out1$S_j_initial))
})

test_that(".props_to_counts holds across locations and tolerates proportion drift", {
  N  <- c(100L, 357L, 9L, 5L)
  sp <- rbind(
    c(0.80, 0.010, 0.005, 1e-7, 1e-7, 0.180),   # sums < 1 (deficit > 0 path)
    c(0.70, 0.020, 0.010, 0.001, 0.001, 0.270), # sums ~ 1
    c(0.90, 0.100, 0.100, 0.010, 0.010, 0.100), # sums > 1 (deficit < 0 shrink path)
    c(0.50, 0.100, 0.100, 0.050, 0.050, 0.200)
  )
  colnames(sp) <- .IC
  out    <- props_to_counts(list(N_j_initial = N), sp, locations = LETTERS[1:4], verbose = FALSE)
  counts <- sapply(.IC, function(c) out[[paste0(c, "_j_initial")]])   # [n_loc x 6]
  expect_equal(rowSums(counts), N)            # each location sums to its own N
  expect_true(all(counts >= 0))
  expect_true(all(counts == floor(counts)))
})

# ---------------------------------------------------------------------------
# sample_parameters() end-to-end (data-gated): the IC invariant must survive
# the full sampling pipeline, and a fixed seed must be deterministic.
# ---------------------------------------------------------------------------
test_that("sample_parameters: initial-condition counts sum to population per location", {
  skip_if(is.null(getOption("root_directory")), "MOSAIC root directory not set")
  # Structural invariant (holds for any seed) -> memoized fixture.
  cfg    <- .cached_sampled_config(42L)
  counts <- sapply(.IC, function(c) cfg[[paste0(c, "_j_initial")]])   # [n_loc x 6]
  expect_equal(rowSums(counts), cfg$N_j_initial)
  expect_true(all(counts >= 0))
  expect_true(all(counts == floor(counts)))
})

test_that("sample_parameters is deterministic for a fixed seed", {
  skip_if(is.null(getOption("root_directory")), "MOSAIC root directory not set")
  a <- sample_parameters(seed = 7, verbose = FALSE)
  b <- sample_parameters(seed = 7, verbose = FALSE)
  for (f in c("beta_j0_tot", "p_beta", "tau_i",
              "S_j_initial", "E_j_initial", "I_j_initial", "mu_j_baseline")) {
    expect_equal(a[[f]], b[[f]], info = f)
  }
  # A different seed should differ somewhere (sanity: sampling is actually seeded)
  d <- sample_parameters(seed = 8, verbose = FALSE)
  expect_false(isTRUE(all.equal(a$beta_j0_tot, d$beta_j0_tot)))
})
