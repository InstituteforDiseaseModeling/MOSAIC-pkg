# =============================================================================
# Guard test for inst/bench/.
#
# This exists because `R CMD check` parses ONLY NAMESPACE and R/. It runs no
# static analysis over inst/ at all -- no non-ASCII check, no `:::` NOTE, no
# undefined-global detection. Verified: inst/examples/simulate_outbreak_settings.R
# carries non-ASCII characters and the baseline WARNING does not list it.
#
# So without this test the benchmark suite rots silently. The concrete trigger
# is already on the roadmap: `micro/draw-wrapper` benchmarks MOSAIC:::.sim_binom
# and needs a label from .SIM_DRAW_SITES, both of which the planned draw-wrapper
# optimisation (pipeline-performance-plan.md) removes. The day that lands, the
# suite breaks and nothing else would say so.
#
# Deliberately cheap: it sources the registry and exercises the two workloads
# that need neither a root_directory nor a cluster, at one rep.
# =============================================================================

bench_dir <- function() {
  p <- system.file("bench", package = "MOSAIC")
  if (nzchar(p) && dir.exists(p)) return(p)
  p <- file.path(testthat::test_path("..", ".."), "inst", "bench")   # load_all
  if (dir.exists(p)) return(normalizePath(p)) else ""
}

test_that("inst/bench sources cleanly and its registry is well-formed", {
  d <- bench_dir()
  skip_if(!nzchar(d), "inst/bench not found")

  env <- new.env(parent = globalenv())
  for (f in c("compat.R", "harness.R", "workloads.R", "workloads_heavy.R")) {
    expect_no_error(sys.source(file.path(d, f), envir = env))
  }

  wl <- c(get("BENCH_WORKLOADS", env), get("BENCH_WORKLOADS_HEAVY", env))
  expect_gt(length(wl), 5L)
  for (w in wl) {
    expect_true(is.character(w$id) && nzchar(w$id))
    expect_true(is.function(w$fn))
    expect_true(is.character(w$needs))
  }
  expect_equal(anyDuplicated(vapply(wl, `[[`, "", "id")), 0L)
})

test_that("the internals inst/bench reaches into still exist", {
  # Each of these is called by a workload. If an optimisation removes one, this
  # fails here rather than the next time someone runs the suite.
  expect_true(is.function(MOSAIC:::.sim_binom))
  expect_true(is.function(MOSAIC:::sim_draws))
  expect_true(is.character(MOSAIC:::.SIM_DRAW_SITES))
  expect_true(is.function(MOSAIC:::.mosaic_set_all_thread_env))
  expect_true(is.function(MOSAIC::run_simulation))
})

test_that("a no-dependency workload runs end to end", {
  skip_on_cran()
  d <- bench_dir(); skip_if(!nzchar(d), "inst/bench not found")
  env <- new.env(parent = globalenv())
  for (f in c("compat.R", "harness.R", "workloads.R")) sys.source(file.path(d, f), envir = env)

  ctx <- list(fixture_dir = file.path(d, "fixtures"),
              reps = list(calibrator = 1L, small = 1L),
              calibrator_passes = 1L)
  cal <- Filter(function(w) identical(w$id, "machine/calibrator"), get("BENCH_WORKLOADS", env))[[1]]
  r <- cal$fn(ctx)
  expect_true(is.numeric(r$reps_s) && length(r$reps_s) == 1L && r$reps_s >= 0)
})
