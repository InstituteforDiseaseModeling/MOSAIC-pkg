# =============================================================================
# DerivedValues must not copy state$rows per row it writes (deep review,
# engine-05; CLAUDE.md lesson #18).
#
# `state$rows[[r]]$spatial_hazard <- v` is a nested subassignment into
# state$rows, which duplicated the whole nticks + 1 pointer list once per tick
# on the final tick: O(nticks^2) garbage per run. The fix binds each row
# environment and writes through the binding.
# =============================================================================

test_that("sim_phase_derived_values writes spatial_hazard without copying state$rows", {
  skip_if_not(capabilities("profmem"), "R built without memory profiling (tracemem)")
  cfg <- MOSAIC::config_simulation_epidemic
  comps <- setdiff(MOSAIC:::SIM_PIPELINE, "DerivedValues")
  par <- MOSAIC:::sim_params(cfg, components = MOSAIC:::SIM_PIPELINE)
  ctl <- MOSAIC:::sim_draws(mode = "rng", seed = 1L)
  rng <- MOSAIC:::.sim_rng_begin(1L)
  on.exit(MOSAIC:::.sim_rng_end(rng), add = TRUE)
  state <- MOSAIC:::sim_alloc_state(par$nticks, par$npatches)
  state <- MOSAIC:::sim_seed_state(state, par, ctl)
  state <- MOSAIC:::.sim_seed_census(state, par, ctl)
  phases <- MOSAIC:::.SIM_PHASE_FUNCTIONS[comps]
  for (tick in seq.int(0L, par$nticks - 1L)) for (ph in phases) state <- ph(state, par, ctl, tick)

  run_derived <- function(state) {
    tracemem(state$rows)
    on.exit(untracemem(state$rows))
    utils::capture.output(
      MOSAIC:::sim_phase_derived_values(state, par, ctl, par$nticks - 1L))
  }
  trace <- run_derived(state)
  expect_identical(sum(grepl("tracemem", trace)), 0L)
  H <- vapply(state$rows[seq.int(2L, par$nticks + 1L)],
              function(e) e$spatial_hazard, numeric(par$npatches))
  expect_true(all(is.finite(H)))
})
