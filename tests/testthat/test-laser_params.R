# =============================================================================
# test-laser_params.R
#
# Validation and normalisation of engine inputs.
#
# These are driven from the validation rules, NOT from the Python oracle. Tier
# B replay parity proves the engine agrees with the oracle on VALID inputs and
# says nothing whatever about invalid or borderline ones -- and the coercion /
# broadcasting / bounds-checking that laser-cholera's params.py performed had
# to land somewhere when the engine moved to R. This is that coverage.
# =============================================================================

mini_config <- function(npatches = 3L, nticks = 5L, ...) {
  cfg <- list(
    seed = 42L,
    date_start = "2023-01-01",
    date_stop = as.character(as.Date("2023-01-01") + nticks - 1L),
    location_name = paste0("L", seq_len(npatches)),
    S_j_initial = rep(1000L, npatches),
    b_jt = matrix(1e-4, nrow = npatches, ncol = nticks),
    d_jt = matrix(2e-5, nrow = npatches, ncol = nticks)
  )
  utils::modifyList(cfg, list(...))
}

# -----------------------------------------------------------------------------
# nticks and dates
# -----------------------------------------------------------------------------

test_that("nticks is inclusive of date_stop", {
  par <- laser_params(mini_config(nticks = 5L), components = c("Susceptible", "Census"))
  expect_identical(par$nticks, 5L)
  expect_identical(par$npatches, 3L)
})

test_that("a single-day window is one tick, not zero", {
  cfg <- mini_config(nticks = 1L)
  par <- laser_params(cfg, components = c("Susceptible", "Census"))
  expect_identical(par$nticks, 1L)
})

test_that("missing or inverted dates error clearly", {
  expect_error(laser_params(mini_config(date_start = NULL)), "date_start")
  cfg <- mini_config()
  cfg$date_stop <- "2022-01-01"
  expect_error(laser_params(cfg), "precedes")
  cfg$date_stop <- "not-a-date"
  expect_error(suppressWarnings(laser_params(cfg)), "Unparseable|precedes")
})

test_that("a missing location_name errors rather than guessing npatches", {
  expect_error(laser_params(mini_config(location_name = NULL)), "location_name")
})

# -----------------------------------------------------------------------------
# Orientation -- the highest-risk check in the whole port
# -----------------------------------------------------------------------------

test_that("time matrices are accepted in either orientation and stored time-major", {
  # On-disk configs are [npatches, nticks]; engine state is [nticks, npatches].
  # Both inputs must yield the same time-major result.
  cfg_pn <- mini_config(npatches = 3L, nticks = 5L)
  par_pn <- laser_params(cfg_pn, components = c("Susceptible", "Census"))
  expect_identical(dim(par_pn$b_jt), c(5L, 3L))

  cfg_np <- cfg_pn
  cfg_np$b_jt <- t(cfg_pn$b_jt)
  cfg_np$d_jt <- t(cfg_pn$d_jt)
  par_np <- laser_params(cfg_np, components = c("Susceptible", "Census"))
  expect_equal(par_np$b_jt, par_pn$b_jt)
})

test_that("an unusable matrix shape errors and names both shapes", {
  cfg <- mini_config(npatches = 3L, nticks = 5L)
  cfg$b_jt <- matrix(1e-4, nrow = 4L, ncol = 7L)
  expect_error(laser_params(cfg), "4 x 7")
  expect_error(laser_params(cfg), "b_jt")
})

test_that("a square time matrix is unambiguous only because both readings agree", {
  # npatches == nticks makes [npatches, nticks] and [nticks, npatches]
  # indistinguishable. The engine takes the on-disk reading first; this test
  # pins that choice so a future refactor cannot silently flip it.
  cfg <- mini_config(npatches = 4L, nticks = 4L)
  cfg$b_jt <- matrix(seq_len(16), nrow = 4L, ncol = 4L) / 1e5
  par <- laser_params(cfg, components = c("Susceptible", "Census"))
  expect_equal(par$b_jt, t(cfg$b_jt))
})

# -----------------------------------------------------------------------------
# Broadcasting
# -----------------------------------------------------------------------------

test_that("scalars and per-patch vectors broadcast to full time matrices", {
  cfg <- mini_config(npatches = 3L, nticks = 5L, b_jt = 1e-4)
  par <- laser_params(cfg, components = c("Susceptible", "Census"))
  expect_identical(dim(par$b_jt), c(5L, 3L))
  expect_true(all(par$b_jt == 1e-4))

  cfg2 <- mini_config(npatches = 3L, nticks = 5L, b_jt = c(1e-4, 2e-4, 3e-4))
  par2 <- laser_params(cfg2, components = c("Susceptible", "Census"))
  expect_identical(dim(par2$b_jt), c(5L, 3L))
  # a per-patch constant must vary across columns (patches), not rows (time)
  expect_equal(par2$b_jt[1, ], c(1e-4, 2e-4, 3e-4))
  expect_equal(par2$b_jt[1, ], par2$b_jt[5, ])
})

test_that("a vector of the wrong length errors rather than recycling", {
  # R's silent recycling is exactly what must not happen here.
  cfg <- mini_config(npatches = 3L, nticks = 5L, S_j_initial = c(1L, 2L))
  expect_error(laser_params(cfg, components = c("Susceptible", "Census")),
               "length 2")
})

# -----------------------------------------------------------------------------
# Non-finite and bounds
# -----------------------------------------------------------------------------

test_that("non-finite inputs are rejected at the boundary", {
  # rbinom(n, size, prob) returns NA for a non-finite prob WITHOUT warning, so
  # a single bad config value would otherwise propagate through a whole run and
  # surface only as an NA likelihood.
  for (bad in list(NA_real_, NaN, Inf, -Inf)) {
    cfg <- mini_config()
    cfg$d_jt[1, 1] <- bad
    expect_error(laser_params(cfg, components = c("Susceptible", "Census")),
                 "non-finite", info = format(bad))
  }
})

test_that("compartment counts must be non-negative whole numbers", {
  cfg <- mini_config(S_j_initial = c(1000L, -5L, 10L))
  expect_error(laser_params(cfg, components = c("Susceptible", "Census")),
               "negative")

  cfg2 <- mini_config(S_j_initial = c(1000, 10.5, 10))
  expect_error(laser_params(cfg2, components = c("Susceptible", "Census")),
               "non-integral")
})

# -----------------------------------------------------------------------------
# Config sources
# -----------------------------------------------------------------------------

test_that("a JSON path is accepted and an unsupported format is refused", {
  cfg <- mini_config()
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(cfg, tmp, auto_unbox = TRUE, matrix = "rowmajor")
  par <- laser_params(tmp, components = c("Susceptible", "Census"))
  expect_identical(par$nticks, 5L)

  yml <- withr::local_tempfile(fileext = ".yaml")
  file.create(yml)
  # YAML was advertised by the old Python bridge's docstring but never actually
  # supported by get_parameters(); the R engine refuses it explicitly instead
  # of failing obscurely later.
  expect_error(laser_params(yml), "\\.json")

  expect_error(laser_params("does-not-exist.json"), "not found")
  expect_error(laser_params(42), "must be a list or a path")
})

# -----------------------------------------------------------------------------
# Compartment resolution and seeding
# -----------------------------------------------------------------------------

test_that("compartments in play follow the pipeline subset", {
  expect_identical(laser_params(mini_config(),
                                components = c("Susceptible", "Census"))$compartments,
                   "S")
  expect_identical(.laser_compartments(LASER_PIPELINE),
                   c("S", "E", "Isym", "Iasym", "R", "V1", "V2"))
  # Census alone brings no compartments into existence.
  expect_length(.laser_compartments("Census"), 0L)
})

test_that("I_j_initial splits by sigma and the two halves sum back exactly", {
  # Isym takes round(sigma * I) and Iasym the remainder, so the split is
  # lossless by construction (infectious.py:83-84). `as.integer(round(x))`,
  # never as.integer(x), because as.integer() truncates.
  cfg <- mini_config(I_j_initial = c(101L, 7L, 0L), sigma = 0.5,
                     E_j_initial = rep(0L, 3), R_j_initial = rep(0L, 3))
  comps <- c("Susceptible", "Exposed", "Infectious", "Recovered", "Census")
  par <- laser_params(cfg, components = comps)
  state <- laser_seed_state(laser_alloc_state(par$nticks, par$npatches), par)

  expect_identical(state$Isym[1, ] + state$Iasym[1, ], c(101L, 7L, 0L))
  # round-half-to-even: 0.5 * 101 = 50.5 -> 50, not 51
  expect_identical(state$Isym[1, ], c(50L, 4L, 0L))
})

test_that("sigma outside [0, 1] is rejected", {
  cfg <- mini_config(I_j_initial = rep(10L, 3), sigma = 1.5)
  expect_error(laser_params(cfg, components = c("Infectious", "Census")),
               "exceeds")
})
