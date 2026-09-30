# =============================================================================
# Boundary validation in sim_params() (deep review, engine-02 / engine-03).
#
# engine-02: a missing or misspelled compartment initial field used to be read
# as a zero compartment, so a raw config without S_j_initial ran a complete,
# silent, all-zero epidemic. The gravity populations had the same `%||% 0`.
# engine-03: nu_jt_sources accepted V1/V2, which the Vaccinated phase reads as
# zero donors, so vaccination silently delivered no doses.
# =============================================================================

.epi_cfg <- function() MOSAIC::config_simulation_epidemic

test_that("a missing compartment initial field is refused, not read as zero", {
  for (f in c("S_j_initial", "E_j_initial", "R_j_initial", "V1_j_initial", "V2_j_initial")) {
    cfg <- .epi_cfg()
    cfg[[f]] <- NULL
    expect_error(MOSAIC::run_simulation(cfg, seed = 1L, quiet = TRUE),
                 sprintf("missing `%s`", f), info = f)
  }
  cfg <- .epi_cfg()
  cfg$I_j_initial <- NULL
  expect_error(MOSAIC:::sim_params(cfg), "missing `I_j_initial`")
})

test_that("a field outside the pipeline's compartments is not demanded", {
  # Susceptible + Census brings only S into existence.
  cfg <- .epi_cfg()
  cfg$E_j_initial <- NULL
  cfg$V1_j_initial <- NULL
  par <- MOSAIC:::sim_params(cfg, components = c("Susceptible", "Census"))
  expect_identical(par$compartments, "S")
})

test_that("the gravity populations require every initial field, whatever the subset", {
  # Without Vaccinated in the pipeline V2 is not a compartment, so the only thing
  # that reads V2_j_initial is the gravity model's N_j -- which used to take 0.
  cfg <- .epi_cfg()
  cfg$V2_j_initial <- NULL
  expect_error(MOSAIC:::sim_params(cfg, components = c("Susceptible", "Census", "HumanToHuman")),
               "missing `V2_j_initial`")
  par <- MOSAIC:::sim_params(.epi_cfg(), components = c("Susceptible", "Census", "HumanToHuman"))
  cfg <- .epi_cfg()
  expect_equal(par$N_j_gravity,
               as.integer(cfg$S_j_initial + cfg$E_j_initial + cfg$I_j_initial +
                            cfg$R_j_initial + cfg$V1_j_initial + cfg$V2_j_initial))
})

test_that("nu_jt_sources rejects V1 and V2, which the engine cannot draw donors from", {
  cfg <- .epi_cfg()
  cfg$nu_jt_sources <- "V1"
  expect_error(MOSAIC:::sim_params(cfg), "cannot donate.*V1")
  cfg$nu_jt_sources <- c("S", "V2")
  expect_error(MOSAIC:::sim_params(cfg), "cannot donate.*V2")
  # The five valid donors, in any order, are still accepted.
  cfg$nu_jt_sources <- c("R", "S", "E", "Iasym", "Isym")
  expect_identical(MOSAIC:::sim_params(cfg)$nu_jt_sources, c("R", "S", "E", "Iasym", "Isym"))
})
