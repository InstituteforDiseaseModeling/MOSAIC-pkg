# =============================================================================
# test-sim_alloc_state.R
#
# v0.70.0 changed the engine's state representation: each channel used to be a
# list of `nticks + 1` per-tick vectors held on the state environment, and is
# now a per-tick environment holding every channel (`state$rows[[row]]$S`).
# The change was made because `state$S[[i]] <- v` is a subassignment into an
# environment-held list, so `[[<-` duplicated the whole pointer vector on every
# write -- about 35 percent of a production run (see sim_alloc_state's docs).
#
# These tests pin the properties the rest of the engine relies on and that a
# 100-cell bit-identity check would NOT catch on its own:
#
#   - the shapes, the storage modes, and the nticks-vs-nticks+1 dose contract;
#   - `.sim_gather()` at npatches == 1, where the tempting `vapply()` form would
#     silently transpose a single-patch series (and, as eight Tier B replay
#     tests found, would also reject the doubles a replayed integer channel
#     legitimately holds);
#   - that the zero prototypes shared across rows are never mutated in place.
# =============================================================================

test_that("state$rows is one environment per tick, plus the seed row", {
  st <- MOSAIC:::sim_alloc_state(nticks = 5L, npatches = 3L)
  expect_length(st$rows, 6L)
  expect_true(all(vapply(st$rows, is.environment, logical(1))))
  expect_identical(st$.nticks, 5L)
  expect_identical(st$.npatches, 3L)
})

test_that("storage mode is per channel and matches the results contract", {
  st <- MOSAIC:::sim_alloc_state(nticks = 4L, npatches = 2L)
  r <- st$rows[[2L]]
  for (nm in c("S", "E", "Isym", "Iasym", "R", "V1", "V2", "N", "births",
               "non_disease_deaths", "disease_deaths", "new_symptomatic",
               "incidence", "incidence_env", "incidence_human",
               "reported_cases", "reported_deaths",
               "dose_one_doses", "dose_two_doses")) {
    expect_identical(r[[nm]], integer(2L), info = nm)
  }
  for (nm in c("Lambda", "Psi", "W", "spatial_hazard")) {
    expect_identical(r[[nm]], numeric(2L), info = nm)
  }
})

test_that("the prototype registry agrees with what was actually allocated", {
  # `sim_results()` and `.sim_gather()` both read `$.proto` / `$.channels`
  # instead of carrying their own storage-mode table, so a drift between the
  # registry and the allocation would produce wrong-typed results silently
  # (lesson 15: assert set membership against the source, not a count).
  st <- MOSAIC:::sim_alloc_state(nticks = 3L, npatches = 2L)
  expect_identical(st$.channels, names(st$.proto))
  r <- st$rows[[1L]]
  expect_setequal(st$.channels, ls(r, all.names = TRUE))
  for (nm in st$.channels) {
    expect_identical(typeof(r[[nm]]), typeof(st$.proto[[nm]]), info = nm)
  }
})

test_that("doses are nticks-shaped: the final row does not carry them", {
  # `dose_one_doses` / `dose_two_doses` are nticks-shaped in the Python engine,
  # and the phases only ever write them at `here` (1..nticks). Omitting them
  # from the last row keeps a stray read returning NULL rather than a
  # plausible-looking zero.
  st <- MOSAIC:::sim_alloc_state(nticks = 4L, npatches = 2L)
  expect_identical(st$rows[[4L]]$dose_one_doses, integer(2L))
  expect_null(st$rows[[5L]]$dose_one_doses)
  expect_null(st$rows[[5L]]$dose_two_doses)
  # Everything else does exist on the final row.
  expect_identical(st$rows[[5L]]$S, integer(2L))
})

test_that("writing one row leaves the other rows at zero", {
  # Every row is seeded from the same shared zero vector, which is only safe
  # because no channel is ever mutated in place -- each write rebinds the name
  # to a freshly computed vector. If some future edit mutated a channel in
  # place, every tick in the run would change at once.
  st <- MOSAIC:::sim_alloc_state(nticks = 5L, npatches = 3L)
  st$rows[[3L]]$S <- c(1L, 2L, 3L)
  expect_identical(st$rows[[3L]]$S, c(1L, 2L, 3L))
  expect_identical(st$rows[[2L]]$S, integer(3L))
  expect_identical(st$rows[[4L]]$S, integer(3L))
})

test_that(".sim_gather returns [rows, npatches] and preserves storage mode", {
  st <- MOSAIC:::sim_alloc_state(nticks = 4L, npatches = 3L)
  for (i in 1:5) st$rows[[i]]$S <- rep(i * 10L, 3L)
  m <- MOSAIC:::.sim_gather(st, "S", 2:5)
  expect_identical(dim(m), c(4L, 3L))
  expect_identical(typeof(m), "integer")
  expect_identical(m[, 1L], c(20L, 30L, 40L, 50L))

  for (i in 1:5) st$rows[[i]]$W <- rep(i / 2, 3L)
  w <- MOSAIC:::.sim_gather(st, "W", 1:5)
  expect_identical(dim(w), c(5L, 3L))
  expect_identical(typeof(w), "double")
  expect_identical(w[, 2L], c(0.5, 1, 1.5, 2, 2.5))
})

test_that(".sim_gather keeps time in the rows when npatches == 1", {
  # The trap: a `vapply()` over the row environments returns a bare length-nrows
  # vector when npatches == 1, and `t()` on that gives a 1 x nrows matrix --
  # the wrong way round. `rbind` is what keeps time in the rows here.
  st <- MOSAIC:::sim_alloc_state(nticks = 4L, npatches = 1L)
  for (i in 1:5) st$rows[[i]]$S <- i * 10L
  m <- MOSAIC:::.sim_gather(st, "S", 2:5)
  expect_identical(dim(m), c(4L, 1L))
  expect_identical(m[, 1L], c(20L, 30L, 40L, 50L))
})

test_that("a single-patch run returns [1, nticks] results, not a transpose", {
  # The end-to-end consequence of the test above, through sim_results().
  cfg <- MOSAIC::config_default
  n_loc <- length(cfg$location_name)
  idx <- 1L
  for (nm in names(cfg)) {
    v <- cfg[[nm]]
    if (is.matrix(v)) {
      if (nrow(v) == n_loc && ncol(v) == n_loc) cfg[[nm]] <- v[idx, idx, drop = FALSE]
      else if (nrow(v) == n_loc)                cfg[[nm]] <- v[idx, , drop = FALSE]
    } else if (is.atomic(v) && !is.null(v) && length(v) == n_loc) cfg[[nm]] <- v[idx]
  }
  cfg$location_name <- MOSAIC::config_default$location_name[idx]
  # Deliberately NOT shortening date_stop: the per-tick `*_jt` matrices are
  # sized from the full window, so trimming the window without also trimming
  # every one of them fails validation in both arms and tests nothing.

  res <- MOSAIC::run_simulation(config = cfg, seed = 1L, quiet = TRUE)
  nt <- ncol(res$results$reported_cases)
  expect_identical(nrow(res$results$reported_cases), 1L)
  expect_identical(dim(res$results$S), c(1L, nt))
  expect_identical(dim(res$results$dose_one_doses), c(1L, nt))
})
