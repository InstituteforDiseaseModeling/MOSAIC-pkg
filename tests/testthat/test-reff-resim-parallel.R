# =============================================================================
# Parallelising .mosaic_reff_resim_ci() (pipeline plan item 2).
#
# The nested for (p) for (s) re-simulation loop became a map over members. Each
# member writes only its own slot and the one running aggregate (max_abs) is
# associative, so the conversion is safe -- but three things have to hold:
#
#   1. the worker returns everything the caller needs, and captures its own
#      errors rather than killing the batch;
#   2. the worker does NOT close over the calling frame -- that frame holds
#      cases_array (hundreds of MB) and parLapply serialises a closure with its
#      environment. This is the defect that shipped in v0.80.0's renderer;
#   3. a failed member is reported, not silently dropped -- a dropped member
#      would leave NA in reff_loc and quietly bias the credible interval.
# =============================================================================

test_that("the member worker captures its own errors instead of throwing", {
  # No .rr_* globals are set, so the worker cannot find its inputs. It must
  # come back with $error rather than aborting the whole map.
  r <- MOSAIC:::.mosaic_reff_resim_member(list(p = 1L, s = 1L, saved = matrix(0, 1, 2)))
  expect_type(r, "list")
  expect_false(is.null(r$error))
  expect_identical(r$p, 1L)
  expect_identical(r$s, 1L)
})

test_that("the member worker carries no calling-frame payload", {
  # Built at file scope, so its environment is the namespace -- not a frame
  # holding cases_array. Serialised size must not depend on what is live where
  # it is referenced from.
  mk <- function(payload) {
    force(payload)
    MOSAIC:::.mosaic_reff_resim_member
  }
  small <- length(serialize(mk(NULL), NULL))
  big   <- length(serialize(mk(runif(5e5)), NULL))   # ~4 MB live in the frame
  expect_identical(big, small)

  e <- environment(MOSAIC:::.mosaic_reff_resim_member)
  expect_true(environmentName(e) %in% c("MOSAIC", "imports:MOSAIC", "R_GlobalEnv"))
})

test_that("member configs are rebuilt from seeds, not pre-built and broadcast", {
  # The serial nP-long sample_parameters() loop is gone: it was both a serial
  # cost and, once parallel, an n_param_sets x ~10 MB broadcast to every worker.
  src <- deparse(MOSAIC:::.mosaic_reff_resim_ci)
  expect_false(any(grepl("member_cfgs", src)))

  wsrc <- deparse(MOSAIC:::.mosaic_reff_resim_member)
  expect_true(any(grepl("sample_parameters", wsrc)))
  expect_true(any(grepl("\\.rr_seeds", wsrc)))
})

test_that("only the per-member slice of cases_array is put in a task", {
  # Not the whole [nL, T, nP, nS] array. At 40 locations x 1398 days x 100
  # params x 10 stochastic that array is ~450 MB; a slice is ~450 KB.
  # deparse() re-wraps lines, so match on structure rather than exact layout:
  # the task carries `saved = matrix(as.numeric(ca[...` and never the bare array.
  src <- paste(deparse(MOSAIC:::.mosaic_reff_resim_ci), collapse = " ")
  expect_true(grepl("saved = matrix(as.numeric(ca[", src, fixed = TRUE))
  expect_false(grepl("saved = ca", src, fixed = TRUE))
})

test_that("a failed member aborts with a count, rather than leaving NA", {
  src <- deparse(MOSAIC:::.mosaic_reff_resim_ci)
  expect_true(any(grepl("failed to re-simulate", src)))
  expect_true(any(grepl("\\$error", src)))
})

test_that(".mosaic_reff_resim_ci accepts a cluster", {
  expect_true("cl" %in% names(formals(MOSAIC:::.mosaic_reff_resim_ci)))
  expect_null(eval(formals(MOSAIC:::.mosaic_reff_resim_ci)$cl))
})
