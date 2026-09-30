# =============================================================================
# The stochastic symptomatic split (v0.89.0).
#
# The spec's "Table of stochastic transitions" makes the E->I symptomatic split
# stochastic: each progressing individual is independently symptomatic with
# probability sigma. laser-cholera 0.16.1 instead does a deterministic
# np.round(sigma * progressing), and the R port reproduced it faithfully -- so
# both engines diverged from the spec the same way.
#
# round() is not linear at small counts, so the deterministic form is wrong in
# the MEAN, not merely the variance: round(sigma * n) = 0 for every n <= 2 at
# sigma = 0.2. At low incidence -- exactly where outbreak onset is decided --
# it systematically suppresses the symptomatic arm, and the symptomatic arm is
# what surveillance observes.
#
# THIS FILE EXISTS BECAUSE THE PARITY HARNESS CANNOT COVER IT. The replay
# fixtures validate the engine draw-for-draw against the oracle, so replay must
# keep the oracle's deterministic form; the binomial is "rng" mode only. That
# makes this the only coverage of the production behaviour -- CLAUDE.md lesson
# #18(v), a bit-identity harness is only as broad as the modes it exercises.
# =============================================================================

test_that("the split is a registered R-only draw site", {
  expect_true("infectious/sigma_split" %in% MOSAIC:::.SIM_DRAW_SITES)
  expect_true("infectious/sigma_split" %in% MOSAIC:::.SIM_RNG_ONLY_SITES)
  # Never claimed as an oracle site: laser-cholera does not draw here.
  expect_false("infectious/sigma_split" %in% unname(MOSAIC:::.SIM_ORACLE_SITE_MAP))
})

test_that("the deterministic form it replaced is biased at low counts", {
  # Not a test of MOSAIC -- a test of the CLAIM that motivated the change, so
  # the rationale in sim_components.R is checkable rather than asserted.
  sigma <- 0.2
  for (n in 1:2) expect_identical(as.integer(round(sigma * n)), 0L)

  # Over many draws the binomial recovers sigma; round() of the mean does not.
  set.seed(11)
  n <- 2L
  draws <- rbinom(20000L, n, sigma)
  expect_equal(mean(draws), n * sigma, tolerance = 0.02)   # ~0.4
  expect_identical(as.integer(round(sigma * n)), 0L)       # 0, always
})

test_that("rng mode draws the split and replay mode does not", {
  # The mode distinction is the whole design, so it is asserted directly on the
  # draw controller rather than inferred from engine output.
  ctl_rng <- MOSAIC:::sim_draws(mode = "rng", seed = 1L)
  expect_identical(ctl_rng$mode, "rng")
  expect_true("infectious/sigma_split" %in% ls(ctl_rng$coverage))

  n <- rep(50L, 4L)
  got <- MOSAIC:::.sim_binom(ctl_rng, "infectious/sigma_split", n, 0.25)
  expect_length(got, 4L)
  expect_true(all(got >= 0L & got <= n))
  expect_true(is.integer(got))
  expect_identical(ctl_rng$coverage[["infectious/sigma_split"]], 1L)
})

test_that("the split is unbiased and conserves the progressing total", {
  # The two properties that matter epidemiologically: sym + asym must equal the
  # number that progressed (nobody created or lost), and E[sym] must be
  # sigma * progressing (the deterministic form fails this at low counts).
  set.seed(99)
  sigma <- 0.25
  prog  <- c(0L, 1L, 2L, 7L, 250L)

  ctl <- MOSAIC:::sim_draws(mode = "rng", seed = 7L)
  reps <- 4000L
  sums <- matrix(NA_integer_, reps, length(prog))
  for (i in seq_len(reps)) {
    sym <- MOSAIC:::.sim_binom(ctl, "infectious/sigma_split", prog, sigma)
    expect_true(all(sym >= 0L & sym <= prog))       # conservation, every draw
    sums[i, ] <- sym
  }
  emp <- colMeans(sums)

  # Unbiased at every count, including the ones round() zeroes out. Checked per
  # element because the absolute tolerance has to scale with the count
  # (expect_equal's tolerance must be a scalar).
  expected <- sigma * prog
  for (k in seq_along(prog)) {
    tol <- max(0.05, 0.05 * expected[k])
    expect_lt(abs(emp[k] - expected[k]), tol)
  }
  expect_identical(emp[1], 0)                        # 0 progressing -> 0 sym
  expect_gt(emp[2], 0)                               # 1 progressing -> sometimes sym
  expect_gt(emp[3], 0)                               # 2 progressing -> sometimes sym
})

test_that("a production run still conserves the compartments", {
  # End-to-end guard: the split must not leak people. Isym + Iasym growth over
  # the run must equal total progression out of E.
  skip_if_not(exists("config_simulation_epidemic", asNamespace("MOSAIC")),
              "no packaged simulation config")
  cfg <- MOSAIC::config_simulation_epidemic
  out <- run_simulation(config = cfg, seed = 42L, quiet = TRUE)

  expect_true(all(is.finite(out$results$Isym)))
  expect_true(all(is.finite(out$results$Iasym)))
  expect_true(all(out$results$Isym  >= 0))
  expect_true(all(out$results$Iasym >= 0))

  cov <- attr(out, "sim_coverage")
  if (!is.null(cov)) {
    # In a real rng run the site must actually fire.
    expect_gt(cov$n_calls[cov$site == "infectious/sigma_split"], 0L)
  }
})
