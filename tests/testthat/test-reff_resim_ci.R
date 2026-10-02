# Tests for the re-simulated posterior R_eff credible interval machinery:
#   - .mosaic_reff_to_mat()          (orientation-robust channel coercion)
#   - per-member weighted-quantile reduction (known weights -> known quantiles)
#   - burn-in exclusion (.add_reff_mask_burn_in, and the inline mask in
#     .add_reff_recompute_ci with the re-simulation mocked)
#   - .mosaic_build_trajectories() grid: stride = 1 -> full daily-consecutive set
#
# The full re-simulation (.mosaic_reff_resim_ci / add_reproductive_numbers
# recompute_ci) drives the simulation engine and is exercised by the smoke
# test, not the unit suite.

# -----------------------------------------------------------------------------
# .mosaic_reff_to_mat: orientation-robust coercion to nL x Tn
# -----------------------------------------------------------------------------
test_that(".mosaic_reff_to_mat coerces a single-location vector to 1 x Tn", {
  v <- as.numeric(1:10)
  m <- MOSAIC:::.mosaic_reff_to_mat(v, nL = 1L, Tn = 10L)
  expect_equal(dim(m), c(1L, 10L))
  expect_equal(as.numeric(m), v)
})

test_that(".mosaic_reff_to_mat coerces an nL x Tn matrix preserving values", {
  nL <- 3L; Tn <- 7L
  M  <- matrix(seq_len(nL * Tn), nrow = nL, ncol = Tn)
  out <- MOSAIC:::.mosaic_reff_to_mat(M, nL = nL, Tn = Tn)
  expect_equal(dim(out), c(nL, Tn))
  expect_equal(out, M)
})

test_that(".mosaic_reff_to_mat trims a trailing tick+1 column", {
  nL <- 2L; Tn <- 5L
  M  <- matrix(seq_len(nL * (Tn + 1L)), nrow = nL, ncol = Tn + 1L)
  out <- MOSAIC:::.mosaic_reff_to_mat(M, nL = nL, Tn = Tn)
  expect_equal(dim(out), c(nL, Tn))
  expect_equal(out, M[, seq_len(Tn), drop = FALSE])
})

# -----------------------------------------------------------------------------
# Weighted-quantile reduction over members: known weights -> known quantiles
# -----------------------------------------------------------------------------
test_that("per-member weighted quantiles recover the weighted median + 95% bounds", {
  # Four members with values 1,2,3,4 and equal weights -> the weighted-quantile
  # reduction must match weighted_quantiles() exactly (the function the resim
  # path calls per (location, t) cell).
  vals <- c(1, 2, 3, 4)
  w    <- rep(0.25, 4)
  probs <- c(0.025, 0.5, 0.975)
  qs <- weighted_quantiles(vals, w, probs)
  expect_length(qs, 3L)
  expect_true(all(diff(qs) >= 0))           # monotone
  expect_equal(qs[2L], weighted_quantiles(vals, w, 0.5))  # median consistency

  # Skewed weights pull the median toward the heavily-weighted value.
  w2 <- c(0.85, 0.05, 0.05, 0.05)
  med2 <- weighted_quantiles(vals, w2, 0.5)
  expect_lt(med2, weighted_quantiles(vals, rep(0.25, 4), 0.5))
})

# -----------------------------------------------------------------------------
# Burn-in exclusion: .add_reff_mask_burn_in() on a small R_eff table
# -----------------------------------------------------------------------------
test_that(".add_reff_mask_burn_in sets the leading days to NA in every estimate", {
  Tn <- 20L; bid <- 5L
  central <- 1 + 0.1 * seq_len(Tn)
  reff <- data.frame(location = "AAA", t = seq_len(Tn), central = central,
                     q0.025 = central - 0.2, q0.975 = central + 0.2)
  attr(reff, "central_matrix") <- matrix(central, nrow = 1L)
  attr(reff, "route_central")  <- list(human = matrix(central / 2, nrow = 1L))

  out <- MOSAIC:::.add_reff_mask_burn_in(reff, bid)

  early <- out$t <= bid
  for (col in c("central", "q0.025", "q0.975")) {
    expect_true(all(is.na(out[[col]][early])), info = col)
    expect_true(all(is.finite(out[[col]][!early])), info = col)
  }
  expect_true(all(is.na(attr(out, "central_matrix")[1, seq_len(bid)])))
  expect_true(all(is.finite(attr(out, "central_matrix")[1, (bid + 1L):Tn])))
  expect_true(all(is.na(attr(out, "route_central")$human[1, seq_len(bid)])))
  expect_identical(attr(out, "burn_in_days"), bid)
  # bid = 0 is a no-op apart from recording the attribute.
  expect_equal(unname(MOSAIC:::.add_reff_mask_burn_in(reff, 0L)$central), central)
})

# The recompute_ci path masks burn-in inline rather than through the helper
# above, so it needs its own check. The re-simulation is mocked; the masking,
# assembly and attributes are the real code.
test_that(".add_reff_recompute_ci NAs the burn-in days of every estimand", {
  nL <- 2L; Tn <- 12L; bid <- 4L
  probs <- c(0.025, 0.5, 0.975)
  out_dir <- withr::local_tempdir()
  dir.create(file.path(out_dir, "2_calibration"))
  dir.create(file.path(out_dir, "1_inputs"))
  saveRDS(list(cases_array = array(1, dim = c(nL, Tn, 1L, 1L)),
               location_names = c("AAA", "BBB"), date_start = "2024-01-01"),
          file.path(out_dir, "2_calibration", "ensemble_candidate.rds"))
  jsonlite::write_json(list(), file.path(out_dir, "1_inputs", "priors.json"))
  jsonlite::write_json(list(control = list(sampling = list())),
                       file.path(out_dir, "1_inputs", "control.json"))

  fake_resim <- function(...) {
    mk <- function(v) matrix(v, nL, Tn)
    central <- list(R_eff = mk(1.5), R_hum = mk(1), R_env = mk(0.5))
    qmats <- lapply(central, function(m) array(rep(m, 3L), dim = c(nL, Tn, 3L)))
    list(central = central, qmats = qmats, probs = probs, kernel_params = list(),
         central_definition = "test", peak_Rt = NULL, peak_window = 7L,
         medoid_member = 1L, gate_frac = 0.9, gate_rel_err_pct = 0,
         gate_rel_err_max = 0, gate_agg_rel_err = 0, gate_cor_median = 1,
         gate_cor_min = 1, gate_max_abs_diff = 0, gate_n_outliers = 0L,
         n_members = 1L)
  }
  testthat::local_mocked_bindings(.mosaic_reff_resim_ci = fake_resim,
                                  get_paths = function(...) list(),
                                  .package = "MOSAIC")

  out <- MOSAIC:::.add_reff_recompute_ci(out_dir, base_config = list(),
                                         burn_in_days = bid, verbose = FALSE)
  val_cols <- c("central", "q2.5", "q50", "q97.5")
  expect_true(all(val_cols %in% names(out)))
  early <- out$t <= bid
  expect_setequal(unique(out$estimand), c("R_eff", "R_hum", "R_env"))
  for (col in val_cols) {
    expect_true(all(is.na(out[[col]][early])), info = col)
    expect_true(all(is.finite(out[[col]][!early])), info = col)
  }
  expect_true(all(is.na(attr(out, "central_matrix")[, seq_len(bid)])))
  expect_true(all(is.finite(attr(out, "central_matrix")[, (bid + 1L):Tn])))
  for (r in attr(out, "route_central"))
    expect_true(all(is.na(r[, seq_len(bid)])))
  expect_identical(attr(out, "burn_in_days"), bid)
})

# -----------------------------------------------------------------------------
# Medoid member selection (run_MOSAIC criterion) -- hand-verifiable fixture
# -----------------------------------------------------------------------------
test_that(".mosaic_reff_select_medoid_member picks the param set + stoch rerun nearest the central", {
  # nL = 1, Tn = 4, nP = 3 param sets, nS = 3 stochastic reruns.
  # Central cases = c(10, 20, 30, 40). Param set 2 sits on it; sets 1/3 are scaled
  # far off, so the param-set medoid must be p = 2. Within p = 2 the three reruns
  # are (0.7x, 1.0x, 1.3x) of central: the within-set stochastic MEDIAN is the
  # middle rerun (= central exactly), so rerun s = 2 has distance 0 and is the
  # unambiguous within-set medoid -> member id m = (2 - 1) * 3 + 2 = 5.
  Tn <- 4L; nP <- 3L; nS <- 3L
  ca <- array(NA_real_, dim = c(1L, Tn, nP, nS))
  central <- c(10, 20, 30, 40)
  ca[1, , 1, 1] <- central * 5;   ca[1, , 1, 2] <- central * 6;  ca[1, , 1, 3] <- central * 7
  ca[1, , 2, 1] <- central * 0.7; ca[1, , 2, 2] <- central;      ca[1, , 2, 3] <- central * 1.3
  ca[1, , 3, 1] <- central / 5;   ca[1, , 3, 2] <- central / 6;  ca[1, , 3, 3] <- central / 7
  cen_mat <- matrix(central, nrow = 1L)

  sel <- MOSAIC:::.mosaic_reff_select_medoid_member(ca, cen_mat, nP, nS)
  expect_equal(sel$param_idx, 2L)
  expect_equal(sel$stoch_idx, 2L)
  expect_equal(sel$member_id, (2L - 1L) * nP + 2L)   # = 5
})

test_that(".mosaic_reff_select_medoid_member agrees with run_MOSAIC's all-location medoid", {
  # nL = 2: location 1 favours param set 1, the larger location 2 favours set 3.
  # A location-1-only selector picks p = 1; run_MOSAIC's pooled, masked
  # .mosaic_medoid_distances() picks p = 3, and the R_eff selector must follow it.
  Tn <- 12L; nP <- 3L; nS <- 2L
  c1 <- rep(10, Tn); c2 <- rep(1000, Tn)
  ca <- array(NA_real_, dim = c(2L, Tn, nP, nS))
  for (s in 1:2) {
    ca[1, , 1, s] <- c1;       ca[2, , 1, s] <- c2 * 8
    ca[1, , 2, s] <- c1 * 2;   ca[2, , 2, s] <- c2 * 3
    ca[1, , 3, s] <- c1 * 1.6; ca[2, , 3, s] <- c2
  }
  cen <- rbind(c1, c2)
  mask <- list(cases_warmup = 2L, deaths_final = TRUE, score_idx_cases = 3L)
  d_run <- MOSAIC:::.mosaic_medoid_distances(ca, cen, mask)
  sel <- MOSAIC:::.mosaic_reff_select_medoid_member(ca, cen, nP, nS, mask_spec = mask)
  expect_identical(which.min(d_run), 3L)
  expect_identical(sel$param_idx, which.min(d_run))
})

test_that(".mosaic_reff_select_medoid_member ignores the unscored head (artifact mask)", {
  # Set 1 matches the central everywhere except a huge burn-in head; set 2 is
  # close on the head and off afterwards. Scored from step 6, set 1 must win.
  Tn <- 10L; nP <- 2L; nS <- 1L
  cen <- matrix(rep(50, Tn), nrow = 1L)
  ca <- array(NA_real_, dim = c(1L, Tn, nP, nS))
  ca[1, , 1, 1] <- c(rep(5000, 5), rep(50, 5))
  ca[1, , 2, 1] <- c(rep(50, 5), rep(80, 5))
  mask <- list(cases_warmup = 0L, deaths_final = TRUE, score_idx_cases = 6L)
  expect_identical(MOSAIC:::.mosaic_reff_select_medoid_member(ca, cen, nP, nS,
                                                              mask_spec = mask)$param_idx, 1L)
})

test_that(".mosaic_reff_select_medoid_member returns NA when central is absent/mismatched", {
  ca <- array(1, dim = c(1L, 4L, 2L, 2L))
  expect_true(is.na(MOSAIC:::.mosaic_reff_select_medoid_member(ca, NULL, 2L, 2L)$member_id))
  # Length-mismatched central -> NA (defensive).
  expect_true(is.na(MOSAIC:::.mosaic_reff_select_medoid_member(
    ca, matrix(1, 1, 3), 2L, 2L)$member_id))
})

# -----------------------------------------------------------------------------
# Per-member peak R_t (explosivity statistic): time-max + cross-member quantiles
# -----------------------------------------------------------------------------
test_that("per-member peak R_t = post-burn-in time-max reduced by weighted_quantiles", {
  # Three members; per-member R_t series with a leading burn-in spike that must
  # be EXCLUDED from the time-max. Members peak (post-burn-in) at 2.0, 3.0, 4.0.
  bid <- 2L
  M <- rbind(
    c(9.9, 9.9, 1.0, 1.5, 2.0, 1.2),   # member 1: burn-in 9.9 excluded; peak 2.0
    c(9.9, 9.9, 1.5, 3.0, 2.5, 1.0),   # member 2: peak 3.0
    c(9.9, 9.9, 2.0, 2.0, 4.0, 3.0))   # member 3: peak 4.0
  w <- c(0.1, 0.2, 0.7)         # >50% mass on member 3 (peak 4.0)
  probs <- c(0.025, 0.5, 0.975)

  burn_idx <- seq_len(bid)
  Mm <- M; Mm[, burn_idx] <- NA_real_
  member_peaks <- apply(Mm, 1L, function(r) { r <- r[is.finite(r)]; max(r) })
  expect_equal(member_peaks, c(2.0, 3.0, 4.0))   # burn-in spike not the max

  ref <- weighted_quantiles(member_peaks, w, probs)
  expect_true(all(diff(ref) >= 0))
  expect_equal(ref[2L], weighted_quantiles(c(2, 3, 4), w, 0.5))
  # Member 3 carries 70% of the mass, so the weighted median is pulled ABOVE the
  # unweighted median 3.0 toward its peak 4.0: the explosivity stat is posterior-
  # weighted. Hand value from the midpoint plotting positions
  # (cumsum(w) - w/2)/sum(w) = (0.05, 0.20, 0.65): p = 0.5 lands between x = 3
  # and x = 4, giving 3 + (0.5 - 0.20)/(0.65 - 0.20) = 11/3.
  #
  # This was 23/7 = 3.2857 before v0.71.1, when weighted_quantiles() interpolated
  # against the upper weight-block edge and so under-credited the dominant
  # member. The corrected value sits closer to 4.0, which is what this test's own
  # comment asks for.
  expect_gt(ref[2L], 3.0)
  expect_equal(ref[2L], 11 / 3, tolerance = 1e-6)   # = 3.666666...
  expect_equal(ref[3L], weighted_quantiles(c(2, 3, 4), w, 0.975))
})

# -----------------------------------------------------------------------------
# .mosaic_build_trajectories grid (Phase-2 fix): stride = 1 -> full daily grid
# -----------------------------------------------------------------------------
test_that(".mosaic_build_trajectories emits a full daily grid at stride 1 and day-1-anchored strides", {
  # The old grid `which(seq_len(n) %% stride == 1L)` was EMPTY at stride 1.
  n_t <- 30L
  scratch <- withr::local_tempdir()
  saveRDS(list(traj = list(S = matrix(as.numeric(seq_len(n_t)), nrow = 1L))),
          file.path(scratch, "sim_1_1.rds"))
  arr <- array(as.numeric(seq_len(n_t)), dim = c(1L, n_t, 1L, 1L))
  build <- function(stride) MOSAIC:::.mosaic_build_trajectories(
    scratch_dir = scratch, subset_orig_pidx = 1L, subset_weights = 1,
    cases_array = arr, deaths_array = arr, n_stoch = 1L, n_locations = 1L,
    n_time_points = n_t, location_names = "AAA", date_start = "2024-01-01",
    date_stop = "2024-01-30", n_successful = 1L, obs_cases = NULL,
    obs_deaths = NULL, trajectory_channels = "S", line_stride = stride,
    verbose = FALSE)

  t1 <- sort(unique(build(1L)$lines$t[build(1L)$lines$channel == "S"]))
  expect_equal(t1, seq_len(n_t))
  t7 <- sort(unique(build(7L)$lines$t))
  expect_equal(t7, c(1L, 8L, 15L, 22L, 29L))
})

# -----------------------------------------------------------------------------
# End to end through the REAL engine: .mosaic_reff_resim_ci on a tiny ensemble
# -----------------------------------------------------------------------------
test_that(".mosaic_reff_resim_ci reduces real engine members into three estimands", {
  skip_if_not(exists("config_simulation_epidemic", asNamespace("MOSAIC")))
  base <- MOSAIC::config_simulation_epidemic
  base$zeta_1 <- 1e6; base$zeta_2 <- 2e5
  nP <- 2L; nS <- 2L
  # Member configs differ by seed; sample_parameters is replaced by a
  # deterministic perturbation so the test needs no priors, but the engine,
  # channel extraction, kernels and reduction are all real.
  member_cfg <- function(seed) {
    cfg <- base; cfg$gamma_1 <- base$gamma_1 * (1 + 0.1 * (seed %% 3)); cfg
  }
  seeds <- c(11L, 12L)
  Tn <- 366L; nL <- length(base$location_name)
  ca <- array(NA_real_, dim = c(nL, Tn, nP, nS))
  # Each member's 7-day-window peak R_eff at location 1, computed here from the
  # member's own run, for the peak_Rt check below. Member id m = (s-1)*nP + p.
  pk1 <- w_m <- numeric(nP * nS)
  for (p in seq_len(nP)) for (s in seq_len(nS)) {
    cfg <- member_cfg(seeds[p]); cfg$seed <- p * 1000L + s
    rs <- run_simulation(config = cfg, seed = cfg$seed, quiet = TRUE)$results
    ca[, , p, s] <- rs$reported_cases
    rw <- MOSAIC:::.mosaic_reff_routes(
      rs$incidence_human[1, ], rs$incidence_env[1, ], rs$delta_jt[1, ],
      MOSAIC:::.mosaic_reff_config_kernel(cfg), 1,
      init = MOSAIC:::.mosaic_reff_init(rs$E[1, 1], rs$Isym[1, 1], rs$Iasym[1, 1],
                                        rs$incidence[1, 1]),
      window = 7L)$R_eff
    rw[1:10] <- NA_real_
    m <- (s - 1L) * nP + p
    pk1[m] <- max(rw[is.finite(rw)]); w_m[m] <- c(0.6, 0.4)[p] / nS
  }
  ens <- structure(list(seeds = seeds, parameter_weights = c(0.6, 0.4),
                        cases_array = ca, n_param_sets = nP,
                        n_simulations_per_config = nS,
                        location_names = base$location_name,
                        date_start = base$date_start,
                        cases_mean = apply(ca, c(1, 2), mean),
                        cases_median = apply(ca, c(1, 2), stats::median)),
                   class = "mosaic_ensemble")
  local_mocked_bindings(
    sample_parameters = function(PATHS, priors, config, seed, ...) member_cfg(seed),
    .mosaic_clamp_transmission_params = function(cfg) cfg,
    .package = "MOSAIC")

  # Default cases_central_method is "median" (v0.101.0) and the ensemble carries
  # cases_median (as calc_model_ensemble() output does), so the medoid target
  # needs no fallback.
  expect_no_warning(
    res <- MOSAIC:::.mosaic_reff_resim_ci(ens, base_config = base, priors = NULL,
                                          sampling_args = NULL, PATHS = NULL,
                                          burn_in_days = 10L, verbose = FALSE))
  expect_equal(res$gate_rel_err_pct, 0)                 # bitwise-reproducible engine
  expect_named(res$central, c("R_eff", "R_hum", "R_env"))
  expect_equal(dim(res$qmats$R_eff), c(nL, Tn, 3L))
  both <- is.finite(res$central$R_hum) & is.finite(res$central$R_env)
  expect_true(any(both))
  expect_equal(res$central$R_eff[both], res$central$R_hum[both] + res$central$R_env[both])
  expect_setequal(unique(res$peak_Rt$estimand), c("R_eff", "R_hum", "R_env"))
  expect_equal(nrow(res$peak_Rt), 3L * nL)
  expect_equal(res$peak_window, 7L)
  row1 <- res$peak_Rt[res$peak_Rt$estimand == "R_eff" &
                        res$peak_Rt$location == base$location_name[1], ]
  expect_equal(unname(unlist(row1[, c("q2.5", "q50", "q97.5")])),
               weighted_quantiles(pk1, w_m, c(0.025, 0.5, 0.975)))
  expect_equal(unname(res$kernel_params[["gamma_1"]]),
               member_cfg(seeds[res$medoid_member$param_idx])$gamma_1)
})
