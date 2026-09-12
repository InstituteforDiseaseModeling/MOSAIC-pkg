# =============================================================================
# test-sim_params.R
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
    d_jt = matrix(2e-5, nrow = npatches, ncol = nticks),

    # Dynamics parameters, so any component subset can be requested. Each
    # component validates its own, so a config missing these can only run
    # Susceptible + Census. Values are plausible but arbitrary: these tests are
    # about normalisation and validation, not about dynamics -- parity with the
    # engine is test-sim_engine_replay.R's job.
    epsilon = 1 / 365,                                  # Recovered
    iota = 0.5, gamma_1 = 0.2, gamma_2 = 0.25,          # Infectious
    sigma = 0.25, rho = 0.1, rho_deaths = 0.4,
    chi_endemic = 1, chi_epidemic = 1,
    delta_reporting_cases = 0L, delta_reporting_deaths = 0L,
    mu_j_baseline = rep(1e-3, npatches),
    mu_j_slope = rep(0, npatches),
    mu_j_epidemic_factor = rep(0, npatches),
    epidemic_threshold = rep(0.01, npatches),
    omega_1 = 1 / 730, omega_2 = 1 / 1095,              # Vaccinated
    phi_1 = 0.6, phi_2 = 0.8,
    nu_1_jt = matrix(0, nrow = npatches, ncol = nticks),
    nu_2_jt = matrix(0, nrow = npatches, ncol = nticks),
    tau_i = rep(0.05, npatches),                        # HumanToHuman
    alpha_1 = rep(0.95, npatches), alpha_2 = 1,
    beta_j0_hum = rep(0.3, npatches),
    latitude = seq(-10, 10, length.out = npatches),
    longitude = seq(20, 40, length.out = npatches),
    a_1_j = rep(0.1, npatches), b_1_j = rep(0.1, npatches),
    a_2_j = rep(0.05, npatches), b_2_j = rep(0.05, npatches),
    p = 1L, mobility_omega = 1, mobility_gamma = 1,
    theta_j = rep(0.2, npatches),                       # EnvToHuman
    kappa = 1e5, beta_j0_env = rep(0.2, npatches),
    zeta_1 = 1e6, zeta_2 = 1e5,                         # Environmental
    psi_jt = matrix(0.5, nrow = npatches, ncol = nticks),
    decay_days_short = 3, decay_days_long = 90,
    decay_shape_1 = 1, decay_shape_2 = 1
  )
  utils::modifyList(cfg, list(...))
}

# -----------------------------------------------------------------------------
# nticks and dates
# -----------------------------------------------------------------------------

test_that("nticks is inclusive of date_stop", {
  par <- sim_params(mini_config(nticks = 5L), components = c("Susceptible", "Census"))
  expect_identical(par$nticks, 5L)
  expect_identical(par$npatches, 3L)
})

test_that("a single-day window is one tick, not zero", {
  cfg <- mini_config(nticks = 1L)
  par <- sim_params(cfg, components = c("Susceptible", "Census"))
  expect_identical(par$nticks, 1L)
})

test_that("missing or inverted dates error clearly", {
  expect_error(sim_params(mini_config(date_start = NULL)), "date_start")
  cfg <- mini_config()
  cfg$date_stop <- "2022-01-01"
  expect_error(sim_params(cfg), "precedes")
  cfg$date_stop <- "not-a-date"
  expect_error(suppressWarnings(sim_params(cfg)), "Unparseable|precedes")
})

test_that("a missing location_name errors rather than guessing npatches", {
  expect_error(sim_params(mini_config(location_name = NULL)), "location_name")
})

# -----------------------------------------------------------------------------
# Orientation -- the highest-risk check in the whole port
# -----------------------------------------------------------------------------

test_that("time matrices are accepted in either orientation and stored time-major", {
  # On-disk configs are [npatches, nticks]; engine state is [nticks, npatches].
  # Both inputs must yield the same time-major result.
  cfg_pn <- mini_config(npatches = 3L, nticks = 5L)
  par_pn <- sim_params(cfg_pn, components = c("Susceptible", "Census"))
  expect_identical(dim(par_pn$b_jt), c(5L, 3L))

  cfg_np <- cfg_pn
  cfg_np$b_jt <- t(cfg_pn$b_jt)
  cfg_np$d_jt <- t(cfg_pn$d_jt)
  par_np <- sim_params(cfg_np, components = c("Susceptible", "Census"))
  expect_equal(par_np$b_jt, par_pn$b_jt)
})

test_that("an unusable matrix shape errors and names both shapes", {
  cfg <- mini_config(npatches = 3L, nticks = 5L)
  cfg$b_jt <- matrix(1e-4, nrow = 4L, ncol = 7L)
  expect_error(sim_params(cfg), "4 x 7")
  expect_error(sim_params(cfg), "b_jt")
})

test_that("a square time matrix is unambiguous only because both readings agree", {
  # npatches == nticks makes [npatches, nticks] and [nticks, npatches]
  # indistinguishable. The engine takes the on-disk reading first; this test
  # pins that choice so a future refactor cannot silently flip it.
  cfg <- mini_config(npatches = 4L, nticks = 4L)
  cfg$b_jt <- matrix(seq_len(16), nrow = 4L, ncol = 4L) / 1e5
  par <- sim_params(cfg, components = c("Susceptible", "Census"))
  expect_equal(par$b_jt, t(cfg$b_jt))
})

# -----------------------------------------------------------------------------
# Broadcasting
# -----------------------------------------------------------------------------

test_that("scalars and per-patch vectors broadcast to full time matrices", {
  cfg <- mini_config(npatches = 3L, nticks = 5L, b_jt = 1e-4)
  par <- sim_params(cfg, components = c("Susceptible", "Census"))
  expect_identical(dim(par$b_jt), c(5L, 3L))
  expect_true(all(par$b_jt == 1e-4))

  cfg2 <- mini_config(npatches = 3L, nticks = 5L, b_jt = c(1e-4, 2e-4, 3e-4))
  par2 <- sim_params(cfg2, components = c("Susceptible", "Census"))
  expect_identical(dim(par2$b_jt), c(5L, 3L))
  # a per-patch constant must vary across columns (patches), not rows (time)
  expect_equal(par2$b_jt[1, ], c(1e-4, 2e-4, 3e-4))
  expect_equal(par2$b_jt[1, ], par2$b_jt[5, ])
})

test_that("a vector of the wrong length errors rather than recycling", {
  # R's silent recycling is exactly what must not happen here.
  cfg <- mini_config(npatches = 3L, nticks = 5L, S_j_initial = c(1L, 2L))
  expect_error(sim_params(cfg, components = c("Susceptible", "Census")),
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
    expect_error(sim_params(cfg, components = c("Susceptible", "Census")),
                 "non-finite", info = format(bad))
  }
})

test_that("compartment counts must be non-negative whole numbers", {
  cfg <- mini_config(S_j_initial = c(1000L, -5L, 10L))
  expect_error(sim_params(cfg, components = c("Susceptible", "Census")),
               "negative")

  cfg2 <- mini_config(S_j_initial = c(1000, 10.5, 10))
  expect_error(sim_params(cfg2, components = c("Susceptible", "Census")),
               "non-integral")
})

# -----------------------------------------------------------------------------
# Config sources
# -----------------------------------------------------------------------------

test_that("a JSON path is accepted and an unsupported format is refused", {
  cfg <- mini_config()
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(cfg, tmp, auto_unbox = TRUE, matrix = "rowmajor")
  par <- sim_params(tmp, components = c("Susceptible", "Census"))
  expect_identical(par$nticks, 5L)

  yml <- withr::local_tempfile(fileext = ".yaml")
  file.create(yml)
  # YAML was advertised by the old Python bridge's docstring but never actually
  # supported by get_parameters(); the R engine refuses it explicitly instead
  # of failing obscurely later.
  expect_error(sim_params(yml), "\\.json")

  expect_error(sim_params("does-not-exist.json"), "not found")
  expect_error(sim_params(42), "must be a list or a path")
})

# -----------------------------------------------------------------------------
# Compartment resolution and seeding
# -----------------------------------------------------------------------------

test_that("compartments in play follow the pipeline subset", {
  expect_identical(sim_params(mini_config(),
                                components = c("Susceptible", "Census"))$compartments,
                   "S")
  expect_identical(.sim_compartments(SIM_PIPELINE),
                   c("S", "E", "Isym", "Iasym", "R", "V1", "V2"))
  # Census alone brings no compartments into existence.
  expect_length(.sim_compartments("Census"), 0L)
})

test_that("I_j_initial splits by sigma and the two halves sum back exactly", {
  # Isym takes round(sigma * I) and Iasym the remainder, so the split is
  # lossless by construction (infectious.py:83-84). `as.integer(round(x))`,
  # never as.integer(x), because as.integer() truncates.
  cfg <- mini_config(I_j_initial = c(101L, 7L, 0L), sigma = 0.5,
                     E_j_initial = rep(0L, 3), R_j_initial = rep(0L, 3))
  comps <- c("Susceptible", "Exposed", "Infectious", "Recovered", "Census")
  par <- sim_params(cfg, components = comps)
  state <- sim_seed_state(sim_alloc_state(par$nticks, par$npatches), par)

  expect_identical(state$Isym[[1]] + state$Iasym[[1]], c(101L, 7L, 0L))
  # round-half-to-even: 0.5 * 101 = 50.5 -> 50, not 51
  expect_identical(state$Isym[[1]], c(50L, 4L, 0L))
})

test_that("sigma outside [0, 1] is rejected, and a per-patch sigma is too", {
  cfg <- mini_config(I_j_initial = rep(10L, 3), sigma = 1.5)
  expect_error(sim_params(cfg, components = c("Infectious", "Census")),
               "must be <= 1")

  # sigma is a SCALAR in the engine (`params.py` scalars table). Accepting a
  # per-patch vector here would take a config the Python engine rejects
  # outright, so the length check has to fire.
  cfg_vec <- mini_config(I_j_initial = rep(10L, 3), sigma = c(0.2, 0.3, 0.4))
  expect_error(sim_params(cfg_vec, components = c("Infectious", "Census")),
               "must be a single value")
})

# -----------------------------------------------------------------------------
# epidemic_peaks: an oracle crash the R engine must not inherit
#
# `params.py:584` does `params.epidemic_peaks.iso_code` after building a
# DataFrame, which raises AttributeError when the peak list is empty -- so a
# config truncated to a window or a location with no recorded peak cannot be run
# by the Python engine at all (claude/oracle/truncate_config.py deletes the key
# to work around it; see fixtures/ORACLE.md).
#
# The R engine does not consume epidemic_peaks: peak-based scoring lives in
# calc_model_likelihood(), not in the transmission model. These tests pin that,
# so the oracle's bug is not reproduced along with its behaviour.
# -----------------------------------------------------------------------------

test_that("the engine accepts a config with empty, zero-row or absent epidemic_peaks", {
  skip_if_no_fixture("replay_single_location")
  fx <- readRDS(test_path("fixtures", "replay_single_location.rds"))
  base <- fx$meta$config_list
  ported <- c("Susceptible", "Exposed", "Recovered", "Infectious", "Vaccinated",
              "Census", "HumanToHuman", "EnvToHuman", "Environmental")

  variants <- list(
    absent      = { c <- base; c$epidemic_peaks <- NULL; c },
    empty_list  = { c <- base; c$epidemic_peaks <- list(); c },
    zero_row_df = { c <- base; c$epidemic_peaks <- data.frame(); c },
    populated   = { c <- base
                    c$epidemic_peaks <- data.frame(iso_code = "MOZ", year = 2023L,
                                                   week = 5L, stringsAsFactors = FALSE)
                    c }
  )

  for (nm in names(variants)) {
    expect_no_error(
      run_simulation(config = variants[[nm]], seed = 1L, quiet = TRUE,
                  components = ported),
      message = paste("epidemic_peaks variant:", nm)
    )
  }
})

test_that("epidemic_peaks does not reach the normalised parameters at all", {
  skip_if_no_fixture("replay_single_location")
  fx <- readRDS(test_path("fixtures", "replay_single_location.rds"))
  cfg <- fx$meta$config_list
  cfg$epidemic_peaks <- data.frame(iso_code = "MOZ", year = 2023L, week = 5L)
  par <- sim_params(cfg, components = c("Susceptible", "Census"))

  # It is carried on `par$config` (the return contract exposes the config
  # verbatim) but is not promoted to a normalised parameter, so no component can
  # branch on it.
  expect_null(par$epidemic_peaks)
  expect_false(is.null(par$config$epidemic_peaks))
})
