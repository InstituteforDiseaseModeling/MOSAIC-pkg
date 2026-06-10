# Regression tests for .mosaic_clamp_transmission_params() — the single source
# of truth for the transmission-parameter guardrail (v0.36.2 / review item R-2).
# Before v0.36.2 the clamp was duplicated inline in the calibration worker and
# the Dask serializer but MISSING from the post-cal best/medioid/ensemble
# re-samples, so a boundary-hitting winning draw could be predicted with
# different (unclamped) parameters than were scored. This helper is now applied
# at every sampling site; these tests pin its behavior so the copies can't drift.

clamp <- MOSAIC:::.mosaic_clamp_transmission_params

test_that("clamps each transmission field into its valid range", {
  out <- clamp(list(
    beta_j0_tot = c(0, -5, 2),
    beta_j0_hum = c(-1, 0, 3),
    beta_j0_env = c(-2, 1),
    p_beta      = c(-0.1, 0.5, 2),
    tau_i       = c(-0.2, 0.5, 1.5)
  ))
  expect_equal(out$beta_j0_tot, c(1e-10, 1e-10, 2))     # >= 1e-10 (GitHub #24)
  expect_equal(out$beta_j0_hum, c(0, 0, 3))             # >= 0
  expect_equal(out$beta_j0_env, c(0, 1))                # >= 0
  expect_equal(out$p_beta,      c(1e-6, 0.5, 1 - 1e-6)) # in [1e-6, 1-1e-6]
  expect_equal(out$tau_i,       c(0, 0.5, 1))           # in [0, 1]
})

test_that("is idempotent (clamping an already-clamped config is a no-op)", {
  raw   <- list(beta_j0_tot = -1, beta_j0_hum = -3, beta_j0_env = -4,
                p_beta = 5, tau_i = 9)
  once  <- clamp(raw)
  twice <- clamp(once)
  expect_identical(once, twice)
})

test_that("leaves in-range values untouched and passes NULL through", {
  in_range <- list(beta_j0_tot = 0.3, beta_j0_hum = 0.2, beta_j0_env = 0.1,
                   p_beta = 0.4, tau_i = 0.6, other_field = 42)
  expect_equal(clamp(in_range), in_range)
  expect_null(clamp(NULL))
})

test_that("tolerates configs missing some transmission fields", {
  out <- clamp(list(tau_i = 2, unrelated = "x"))
  expect_equal(out$tau_i, 1)
  expect_equal(out$unrelated, "x")
  expect_false("p_beta" %in% names(out))
})
