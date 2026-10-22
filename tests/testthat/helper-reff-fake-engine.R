# A deterministic two-location engine for the R_eff capture and ensemble-object
# tests (test-postcal-speed-v150.R). Route incidences, decay rates and stocks
# depend on the seed and on beta_j0_tot, so members differ. Also sourced by
# claude/v150/fixture/make_ens_v142.R, which built fixtures/ensemble_v142_mock.rds
# with MOSAIC 1.4.2.
.reff_fake_engine <- function(config, seed = NULL, quiet = TRUE, ...) {
  nL <- 2L; Tn <- 60L
  s <- as.numeric(seed); b <- as.numeric(config$beta_j0_tot)[1]
  tt <- matrix(rep(seq_len(Tn), each = nL), nL)
  inc_h <- round(20 * (1 + sin(tt / 9 + s / 7 + b)) + 5)
  inc_e <- round(10 * (1 + cos(tt / 11 + s / 5)) + 3)
  rc <- round(0.3 * (inc_h + inc_e))
  list(results = list(
    incidence_human = inc_h, incidence_env = inc_e, incidence = inc_h + inc_e,
    delta_jt = matrix(0.05 + 0.01 * (tt %% 7), nL), E = 2 * (inc_h + inc_e),
    Isym = inc_h + 1, Iasym = inc_e + 1, reported_cases = rc,
    reported_deaths = round(0.02 * rc), new_symptomatic = rc))
}
.reff_cfg <- function(seed) {
  list(location_name = c("AAA", "BBB"), date_start = "2020-01-01", date_stop = "2020-02-29",
       reported_cases = matrix(1, 2, 60), reported_deaths = matrix(0, 2, 60),
       beta_j0_tot = (seed %% 13) / 10, iota = 1.4, gamma_1 = 0.2, gamma_2 = 0.5,
       sigma = 0.25, zeta_1 = 1e6, zeta_2 = 1e5, seed = seed)
}
