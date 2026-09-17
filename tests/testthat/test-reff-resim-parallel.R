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

test_that("the member worker runs from an explicitly passed context", {
  # THIS TEST REPLACES ONE THAT CERTIFIED A BUG. v0.85.0 shipped a worker that
  # read its inputs from globalenv unconditionally, and the test written with it
  # asserted the worker returns $error when those globals are absent -- which is
  # precisely the state the SERIAL route was always in. So the serial route
  # failed on its first member with "object '.rr_base_config' not found", the
  # test passed, and a v0.84.0 red-team reviewer had to point out that the test
  # certified the defect as intended behaviour. It did.
  #
  # The replacement asserts the thing that actually matters: given a context,
  # the worker RUNS. A worker that cannot run from a passed context fails here.
  ctx <- list(base_config = NULL, priors = NULL, sampling = NULL, paths = NULL,
              seeds = 1L, max_days = 3L, floor = 1, nL = 1L, Tn = 2L)

  # sample_parameters is mocked, so this exercises the worker's own plumbing --
  # context unpacking, config rebuild from the seed, and the result shape --
  # without the engine.
  local_mocked_bindings(
    sample_parameters = function(...) list(iota = 1, gamma_1 = 1, gamma_2 = 1, sigma = 1),
    .mosaic_clamp_transmission_params = function(cfg) cfg,
    .mosaic_generation_time_pmf = function(...) c(0.5, 0.5),
    run_simulation = function(...) list(results = list(
      incidence = matrix(c(1, 2), 1, 2), reported_cases = matrix(c(1, 2), 1, 2))),
    .mosaic_reff_to_mat = function(x, nL, Tn) matrix(as.numeric(x), nL, Tn),
    .cori_reff = function(inc, g, infectiousness_floor = 1) rep(1.0, length(inc)),
    .package = "MOSAIC"
  )

  r <- MOSAIC:::.mosaic_reff_resim_member(
    list(p = 1L, s = 1L, saved = matrix(c(1, 2), 1, 2)), ctx = ctx)

  expect_null(r$error)          # the point: it RAN
  expect_identical(r$p, 1L)
  expect_identical(r$s, 1L)
  expect_length(r$reff, 1L)     # one per location
  expect_length(r$reff[[1L]], 2L)
})

test_that("a genuinely broken member is reported, not thrown", {
  # Error capture still matters -- one bad member must not kill the batch --
  # but it is asserted on a REAL failure, not on the absence of setup.
  ctx <- list(base_config = NULL, priors = NULL, sampling = NULL, paths = NULL,
              seeds = 1L, max_days = 3L, floor = 1, nL = 1L, Tn = 2L)
  local_mocked_bindings(
    sample_parameters = function(...) stop("engine exploded"),
    .package = "MOSAIC"
  )
  r <- MOSAIC:::.mosaic_reff_resim_member(
    list(p = 2L, s = 3L, saved = matrix(0, 1, 2)), ctx = ctx)
  expect_false(is.null(r$error))
  expect_match(r$error, "engine exploded")
  expect_identical(r$p, 2L); expect_identical(r$s, 3L)
})

test_that("the serial route supplies a context rather than relying on globals", {
  # The structural guard for the v0.85.0 bug: the serial branch must PASS ctx.
  src <- paste(deparse(MOSAIC:::.mosaic_reff_resim_ci), collapse = " ")
  # deparse() re-wraps long lines, so match the call's two halves rather than a
  # single literal.
  expect_true(grepl("lapply(tasks, function(tk)", src, fixed = TRUE))
  expect_true(grepl(".mosaic_reff_resim_member(tk,", src, fixed = TRUE))
  expect_true(grepl("ctx)", src, fixed = TRUE))
  # and the parallel branch must NOT pass it per task (that would ship it n times)
  expect_true(grepl("parLapplyLB(cl, tasks, .w)", src, fixed = TRUE))
})

test_that("add_reproductive_numbers threads n_cores to the resim helper", {
  # v0.85.0 added n_cores to the exported function but used it inside a nested
  # helper that has no such formal, so recompute_ci = TRUE died with
  # "object 'n_cores' not found" before any dispatch -- on BOTH routes.
  expect_true("n_cores" %in% names(formals(add_reproductive_numbers)))
  expect_true("n_cores" %in% names(formals(MOSAIC:::.add_reff_recompute_ci)))
  src <- paste(deparse(add_reproductive_numbers), collapse = " ")
  expect_true(grepl("n_cores = n_cores", src, fixed = TRUE))
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

  wsrc <- paste(deparse(MOSAIC:::.mosaic_reff_resim_member), collapse = " ")
  expect_true(grepl("sample_parameters", wsrc, fixed = TRUE))
  # The seed comes from the shared context, not a pre-built config list.
  expect_true(grepl("ctx$seeds", wsrc, fixed = TRUE))
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
