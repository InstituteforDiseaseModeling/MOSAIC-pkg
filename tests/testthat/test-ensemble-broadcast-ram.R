# =============================================================================
# The ensemble RAM projection must count the config broadcast.
#
# calc_model_ensemble() clusterExports the whole param_configs list to EVERY
# PSOCK worker. With local PSOCK -- the only backend left -- those workers are
# processes on this host, so every copy comes out of this machine's RAM and
# belongs in the same budget as the dense arrays.
#
# Measured at 40 locations: one sampled config is ~10 MB, so the broadcast is
# ~1.1 GB per worker at 114 parameter sets and ~9.9 GB per worker (793 GB across
# 80) at 1,000 -- minor at the subset sizes used so far, dominant at the sizes
# the optimiser is allowed to choose.
# =============================================================================

fake_configs <- function(n, mb_each = 1) {
  one <- list(payload = raw(round(mb_each * 2^20)))
  rep(list(one), n)
}

test_that("broadcast size scales with configs and workers", {
  f <- MOSAIC:::.mosaic_ensemble_broadcast_gb
  cfgs <- fake_configs(10, mb_each = 1)

  g1 <- f(cfgs, 1L)
  expect_gt(g1, 0)

  # linear in workers and in the number of configs
  expect_equal(f(cfgs, 8L), g1 * 8, tolerance = 1e-9)
  expect_equal(f(fake_configs(20, 1), 1L), g1 * 2, tolerance = 1e-6)

  # ~10 MB of payload x 10 configs x 1 worker is ~0.1 GB
  expect_gt(g1, 0.008); expect_lt(g1, 0.02)
})

test_that("degenerate inputs give zero rather than NA or an error", {
  f <- MOSAIC:::.mosaic_ensemble_broadcast_gb
  expect_identical(f(list(), 8L), 0)
  expect_identical(f(fake_configs(2), 0L), 0)
  expect_identical(f(fake_configs(2), NA_integer_), 0)
  expect_identical(f(fake_configs(2), -1L), 0)
  expect_identical(f(fake_configs(2), NULL), 0)
})

test_that("the projection adds the broadcast to the array budget", {
  p <- MOSAIC:::.mosaic_ensemble_ram_projection_gb
  base <- p(40L, 1398L, 114L, 10L)
  with_bcast <- p(40L, 1398L, 114L, 10L, broadcast_gb = 7.5)

  expect_gt(base, 0)
  expect_equal(with_bcast - base, 7.5, tolerance = 1e-9)
  # default is 0, so existing callers are unchanged
  expect_identical(p(40L, 1398L, 114L, 10L, 0L, FALSE),
                   p(40L, 1398L, 114L, 10L, 0L, FALSE, broadcast_gb = 0))
})

test_that("the warning fires on the broadcast alone and names the number", {
  # Arrays deliberately tiny; the broadcast is what crosses the threshold. This
  # is the case the guard exists for -- a subset large enough that shipping the
  # configs, not holding the results, is what exhausts the host.
  expect_warning(
    MOSAIC:::.mosaic_ensemble_check_ram(
      n_locations = 1L, n_time_points = 10L, n_param_sets = 1000L, n_stoch = 1L,
      total_ram_gb = 64, broadcast_gb = 793),
    "clusterExport of 1000 sampled configs"
  )
  expect_warning(
    MOSAIC:::.mosaic_ensemble_check_ram(
      n_locations = 1L, n_time_points = 10L, n_param_sets = 1000L, n_stoch = 1L,
      total_ram_gb = 64, broadcast_gb = 793),
    "lands on THIS host"
  )
})

test_that("no broadcast means no broadcast wording", {
  w <- tryCatch(
    MOSAIC:::.mosaic_ensemble_check_ram(
      n_locations = 40L, n_time_points = 1398L, n_param_sets = 5000L,
      n_stoch = 10L, total_ram_gb = 8, broadcast_gb = 0),
    warning = function(x) conditionMessage(x))
  expect_type(w, "character")
  expect_false(grepl("clusterExport", w))
})

test_that("an un-probed platform still short-circuits", {
  # total_ram_gb = NA must return the projection without warning, as before.
  expect_silent(
    v <- MOSAIC:::.mosaic_ensemble_check_ram(
      40L, 1398L, 1000L, 10L, total_ram_gb = NA_real_, broadcast_gb = 793))
  expect_gt(v, 793)
})
