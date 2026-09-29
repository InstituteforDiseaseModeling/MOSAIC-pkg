# Tests for the route-decomposed Cori R_eff (R_eff = R_hum + R_env):
#   - .mosaic_reff_route_kernel()     engine-derived cohort state tables
#   - .mosaic_reff_infectiousness()   Lambda_hum / Lambda_env (exact filter)
#   - .mosaic_reff_kernel_pmf()       constant-delta kernels
#   - .cori_reff()                    route ratio + floor
#   - .mosaic_reff_init()             initial infectious stocks
#   - calc_Reff()                     direct path, schema, CI plumbing

kern_default <- function(zeta_1 = 1, zeta_2 = 0.5)
  MOSAIC:::.mosaic_reff_route_kernel(iota = 1 / 1.4, gamma_1 = 0.1,
                                     gamma_2 = 0.5, sigma = 0.25,
                                     zeta_1 = zeta_1, zeta_2 = zeta_2)

# -----------------------------------------------------------------------------
# Kernel tables and constant-delta kernels
# -----------------------------------------------------------------------------
test_that("route kernel state tables are probabilities and D_h is their sum", {
  k <- kern_default()
  expect_true(all(k$Ps >= 0) && all(k$Pa >= 0))
  expect_true(all(k$Ps + k$Pa <= 1 + 1e-12))
  expect_equal(k$p_i, -expm1(-1 / 1.4))
  expect_equal(sum(k$Ps + k$Pa), k$D_h, tolerance = 1e-8)
  expect_equal(k$w1 + k$w2, 1)
  expect_equal(k$w1, 1 / 1.5)
})

test_that("human kernel mean equals the discrete transmission-weighted mean", {
  k  <- kern_default()
  kp <- MOSAIC:::.mosaic_reff_kernel_pmf(k, delta = 0.05)
  expect_equal(sum(kp$hum), 1, tolerance = 1e-8)
  # Lag = 1 (infection -> next-day incidence) + arrival day in I (geometric,
  # mean 1/p_i) + age in I at transmission, transmission-weighted across the
  # two classes: sum sigma_k (1-p_k)/p_k^2 / sum sigma_k/p_k.
  num <- 0.25 * (1 - k$p1) / k$p1^2 + 0.75 * (1 - k$p2) / k$p2^2
  expected <- 1 + 1 / k$p_i + num / k$D_h
  expect_equal(kp$mean_hum, expected, tolerance = 1e-6)
  # The old kernel used the duration-weighted mean; the transmission-weighted
  # one is materially longer for these parameters.
  expect_gt(kp$mean_hum, 1 / (1 / 1.4) + (0.25 / 0.1 + 0.75 / 0.5))
})

test_that("environmental kernel sums to 1 and adds the reservoir residence", {
  k <- kern_default()
  for (d in c(1 / 16, 1 / 60, 1 / 200)) {
    kp <- MOSAIC:::.mosaic_reff_kernel_pmf(k, delta = d)
    expect_equal(sum(kp$env), 1, tolerance = 1e-5)
    s_num <- k$w1 * 0.25 * (1 - k$p1) / k$p1^2 + k$w2 * 0.75 * (1 - k$p2) / k$p2^2
    s_den <- k$w1 * 0.25 / k$p1 + k$w2 * 0.75 / k$p2
    # Shedding day (arrival + age at shedding) -> enters W next day -> residence
    # (geometric, mean (1-d)/d) -> drives incidence the day after.
    expected <- 1 / k$p_i + s_num / s_den + 1 + (1 - d) / d + 1
    expect_equal(kp$mean_env, expected, tolerance = 1e-3 * expected)
  }
  # The environmental interval is far longer than the human one.
  expect_gt(MOSAIC:::.mosaic_reff_kernel_pmf(k, 1 / 16)$mean_env,
            2 * MOSAIC:::.mosaic_reff_kernel_pmf(k, 1 / 16)$mean_hum)
})

test_that("route kernel validates inputs", {
  expect_error(MOSAIC:::.mosaic_reff_route_kernel(-1, 0.1, 0.5, 0.25, 1, 1), "iota")
  expect_error(MOSAIC:::.mosaic_reff_route_kernel(0.7, 0.1, 0.5, 1.5, 1, 1), "sigma")
  expect_error(MOSAIC:::.mosaic_reff_route_kernel(0.7, 0.1, 0.5, 0.25, 0, 0), "shedding")
})

# -----------------------------------------------------------------------------
# Lambda_env is instantaneous (frozen at t) under time-varying delta
# -----------------------------------------------------------------------------
test_that("time-varying Lambda_env matches the frozen-at-t definition by brute force", {
  set.seed(11)
  Tn <- 80L
  k <- kern_default(zeta_1 = 2, zeta_2 = 1)
  inc <- rpois(Tn, 50)
  delta <- 1 / (16 + 180 * (0.5 + 0.5 * sin(seq_len(Tn) / 9)))
  lam <- MOSAIC:::.mosaic_reff_infectiousness(inc, delta, k)

  # Brute force, written independently of the filter: each past cohort's cells
  # present at t - 1 under the ACTUAL past decay path, divided by one
  # infection's lifetime shedding valued at today's decay rate.
  cells_at <- function(u, n) {       # cells of a unit cohort infected at u, at n
    if (n <= u) return(0)
    tot <- 0
    for (m in u:(n - 1L)) {          # shed from I at m enters W at m + 1
      k_age <- m - u
      if (k_age < 1L || k_age > length(k$Ps)) next
      shed <- k$w1 * k$Ps[k_age] + k$w2 * k$Pa[k_age]
      surv <- if (n > m + 1L) prod(1 - delta[(m + 2L):n]) else 1
      tot <- tot + shed * surv
    }
    tot
  }
  S_w <- sum(k$w1 * k$Ps + k$w2 * k$Pa)
  brute <- vapply(seq_len(Tn), function(t) {
    if (t < 2L) return(0)
    sum(vapply(seq_len(t - 1L), function(u) inc[u] * cells_at(u, t - 1L), 0)) *
      delta[t] / S_w
  }, numeric(1))
  expect_equal(lam$Lambda_env, brute, tolerance = 1e-8)
})

test_that("R at t does not depend on anything after t (truncation invariance)", {
  set.seed(12)
  Tn <- 400L; k <- kern_default()
  inc_h <- rpois(Tn, 20); inc_e <- rpois(Tn, 80)
  delta <- 1 / (16 + 180 * (0.5 + 0.5 * sin(seq_len(Tn) / 25)))
  full <- MOSAIC:::.mosaic_reff_routes(inc_h, inc_e, delta, k)
  for (T0 in c(150L, 260L)) {
    cut <- MOSAIC:::.mosaic_reff_routes(inc_h[1:T0], inc_e[1:T0], delta[1:T0], k)
    for (e in c("R_eff", "R_hum", "R_env"))
      expect_equal(cut[[e]], full[[e]][1:T0])
  }
})

test_that("decay rates above 1 are capped at 1, as the engine clamps decay to W", {
  Tn <- 60L; k <- kern_default()
  inc <- rep(30, Tn)
  d_hi <- rep(c(0.05, 2), length.out = Tn)
  d_1  <- pmin(d_hi, 1)
  expect_equal(MOSAIC:::.mosaic_reff_infectiousness(inc, d_hi, k)$Lambda_env,
               MOSAIC:::.mosaic_reff_infectiousness(inc, d_1, k)$Lambda_env)
  expect_error(MOSAIC:::.mosaic_reff_infectiousness(inc, rep(0, Tn), k), "delta")
  expect_true(is.finite(MOSAIC:::.mosaic_reff_kernel_pmf(k, 2)$mean_env))
})

test_that("constant-delta Lambda_env equals convolution with the env kernel", {
  set.seed(3)
  Tn <- 150L; d <- 1 / 30
  k <- kern_default()
  inc <- rpois(Tn, 20)
  lam <- MOSAIC:::.mosaic_reff_infectiousness(inc, rep(d, Tn), k)
  kp  <- MOSAIC:::.mosaic_reff_kernel_pmf(k, d, tail = 1e-12)
  conv <- vapply(seq_len(Tn), function(t) {
    L <- seq_len(t - 1L)
    if (!length(L)) return(0)
    sum(kp$env[L] * inc[t - L])
  }, numeric(1))
  expect_equal(lam$Lambda_env, conv, tolerance = 1e-8)
  conv_h <- vapply(seq_len(Tn), function(t) {
    L <- seq_len(min(t - 1L, length(kp$hum)))
    if (!length(L)) return(0)
    sum(kp$hum[L] * inc[t - L])
  }, numeric(1))
  expect_equal(lam$Lambda_hum, conv_h, tolerance = 1e-8)
})

# -----------------------------------------------------------------------------
# The reconstruction matches the ENGINE (the non-tautological check)
# -----------------------------------------------------------------------------
test_that("I and W rebuilt from incidence alone track the simulated stocks", {
  skip_if_not(exists("config_simulation_epidemic", asNamespace("MOSAIC")))
  cfg <- MOSAIC::config_simulation_epidemic
  cfg$zeta_1 <- 1e6; cfg$zeta_2 <- 2e5          # large shedding -> smooth W
  r <- run_simulation(config = cfg, seed = 7L, quiet = TRUE)$results
  kern <- MOSAIC:::.mosaic_reff_config_kernel(cfg)
  i <- which.max(rowSums(r$incidence))
  init <- MOSAIC:::.mosaic_reff_init(r$E[i, 1], r$Isym[i, 1], r$Iasym[i, 1],
                                     r$incidence[i, 1])
  lam <- MOSAIC:::.mosaic_reff_infectiousness(
    r$incidence[i, ], r$delta_jt[i, ], kern, init = init,
    shed_abs = (1 - cfg$theta_j[i]) * c(cfg$zeta_1, cfg$zeta_2))
  I_obs <- r$Isym[i, ] + r$Iasym[i, ]
  W_obs <- r$W[i, ]
  win <- which(I_obs > 100)
  expect_gt(length(win), 30L)
  expect_gt(stats::cor(lam$I_hat[win], I_obs[win]), 0.99)
  expect_lt(abs(stats::median(lam$I_hat[win] / I_obs[win]) - 1), 0.05)
  expect_gt(stats::cor(lam$W_hat[win], W_obs[win]), 0.99)
  expect_lt(abs(stats::median(lam$W_hat[win] / W_obs[win]) - 1), 0.05)
  # A one-day misalignment would still correlate > 0.99, so also require that
  # the reconstruction lines up best at zero shift.
  shift_err <- function(hat, obs, s) {
    w <- win[win + s >= 1 & win + s <= length(obs)]
    sqrt(mean((hat[w] - obs[w + s])^2)) / mean(obs[w])
  }
  for (pair in list(list(lam$I_hat, I_obs), list(lam$W_hat, W_obs))) {
    errs <- vapply(-2:2, function(s) shift_err(pair[[1]], pair[[2]], s), 0)
    expect_equal(which.min(errs), 3L)
  }
})

test_that("human R recovers the engine's true instantaneous R (human-only run)", {
  skip_if_not(exists("config_simulation_epidemic", asNamespace("MOSAIC")))
  cfg <- MOSAIC::config_simulation_epidemic
  cfg$tau_i <- rep(0, length(cfg$tau_i))
  kern <- MOSAIC:::.mosaic_reff_config_kernel(cfg)
  # One realization's median ratio scatters by about +/-5% (seed to seed it runs
  # 0.91-1.08 here), so the check pools 8 seeds. The kernel ignores the fatal
  # onsets that never enter Isym (p_fatal 3.3% at this config), which moves the
  # ratio by about -0.5%.
  one_seed <- function(seed) {
    r <- run_simulation(config = cfg, seed = seed, quiet = TRUE)$results
    i <- which.max(rowSums(r$incidence_human)); t <- 2:ncol(r$incidence)
    rr <- MOSAIC:::.mosaic_reff_routes(
      r$incidence_human[i, ], r$incidence_env[i, ], r$delta_jt[i, ], kern,
      init = MOSAIC:::.mosaic_reff_init(r$E[i, 1], r$Isym[i, 1], r$Iasym[i, 1],
                                        r$incidence[i, 1]))
    # Engine FOI per susceptible: beta_jt_human * I^alpha_1 / N^alpha_2, so one
    # infectious person causes beta * S * I^(alpha_1 - 1) / N^alpha_2 infections a
    # day for D_h days.
    I <- (r$Isym + r$Iasym)[i, t - 1L]
    truth <- r$beta_jt_human[i, t] * r$S[i, t] * I^(cfg$alpha_1 - 1) /
      r$N[i, t - 1L]^cfg$alpha_2 * kern$D_h
    ok <- is.finite(rr$R_hum[t]) & I > 100
    c(n = sum(ok), ratio = stats::median(rr$R_hum[t][ok] / truth[ok]))
  }
  res <- vapply(1:8, one_seed, numeric(2))
  expect_true(all(res["n", ] > 40))
  expect_lt(abs(stats::median(res["ratio", ]) - 1), 0.05)
})

test_that("environmental R recovers the engine's true instantaneous R (linear dose)", {
  skip_if_not(exists("config_simulation_epidemic", asNamespace("MOSAIC")))
  cfg <- MOSAIC::config_simulation_epidemic
  cfg$tau_i <- rep(0, length(cfg$tau_i)); cfg$beta_j0_hum <- rep(0, length(cfg$tau_i))
  cfg$kappa <- 1e12; cfg$zeta_1 <- 1e8; cfg$zeta_2 <- 1e7
  cfg$beta_j0_env <- rep(1e3, length(cfg$tau_i))
  r <- run_simulation(config = cfg, seed = 3L, quiet = TRUE)$results
  kern <- MOSAIC:::.mosaic_reff_config_kernel(cfg)
  i <- which.max(rowSums(r$incidence_env)); t <- 2:ncol(r$incidence)
  rr <- MOSAIC:::.mosaic_reff_routes(
    r$incidence_human[i, ], r$incidence_env[i, ], r$delta_jt[i, ], kern,
    init = MOSAIC:::.mosaic_reff_init(r$E[i, 1], r$Isym[i, 1], r$Iasym[i, 1],
                                      r$incidence[i, 1]))
  # Linear dose (W/N << kappa): per-susceptible hazard beta_env (1-theta) W /
  # (N kappa); one infection's lifetime reservoir at today's decay is
  # (1-theta) (zeta_1 sigma/p1 + zeta_2 (1-sigma)/p2) / delta_t cells.
  th <- cfg$theta_j[i]; d <- pmin(r$delta_jt[i, t], 1)
  life <- (1 - th) * (cfg$zeta_1 * cfg$sigma / kern$p1 +
                        cfg$zeta_2 * (1 - cfg$sigma) / kern$p2) / d
  truth <- r$beta_jt_env[i, t] * (1 - th) * r$S[i, t] /
    (r$N[i, t - 1L] * cfg$kappa) * life
  I <- (r$Isym + r$Iasym)[i, t - 1L]
  ok <- is.finite(rr$R_env[t]) & I > 20 & t > 60
  expect_gt(sum(ok), 40L)
  expect_lt(abs(stats::median(rr$R_env[t][ok] / truth[ok]) - 1), 0.1)
})

# -----------------------------------------------------------------------------
# Route ratio, additivity, Euler-Lotka
# -----------------------------------------------------------------------------
test_that(".cori_reff divides where the denominator clears the floor", {
  r <- MOSAIC:::.cori_reff(c(10, 5, NA, 8), c(5, 0.5, 4, 0))
  expect_equal(r, c(2, NA, NA, NA))
  expect_equal(MOSAIC:::.cori_reff(c(1, 1), c(0.5, 2), infectiousness_floor = 0),
               c(2, 0.5))
  expect_error(MOSAIC:::.cori_reff(1:3, 1:2), "same length")
  expect_error(MOSAIC:::.cori_reff(1, 1, infectiousness_floor = -1), "floor")
})

test_that("R_eff is R_hum + R_env; a gated route with no infections counts as 0", {
  set.seed(5)
  Tn <- 200L; k <- kern_default()
  inc_h <- rpois(Tn, 30); inc_e <- rpois(Tn, 70)
  rr <- MOSAIC:::.mosaic_reff_routes(inc_h, inc_e, rep(1 / 40, Tn), k)
  ok <- is.finite(rr$R_hum) & is.finite(rr$R_env)
  expect_true(any(ok))
  expect_equal(rr$R_eff[ok], rr$R_hum[ok] + rr$R_env[ok])

  rr0 <- MOSAIC:::.mosaic_reff_routes(inc_h + inc_e, numeric(Tn), rep(1 / 40, Tn), k)
  f <- is.finite(rr0$R_eff)
  expect_true(all(is.na(rr0$R_env[f]) | rr0$R_env[f] == 0))
  expect_equal(rr0$R_eff[f], rr0$R_hum[f])

  # Route below the floor: with 0 own infections it contributes 0; with
  # infections it leaves the total undefined. Total >= each defined route.
  Lh <- c(5, 5, 5); Le <- c(0.2, 0.2, 0.2)
  r2 <- local({
    testthat::local_mocked_bindings(
      .mosaic_reff_infectiousness = function(...) list(Lambda_hum = Lh, Lambda_env = Le),
      .package = "MOSAIC")
    MOSAIC:::.mosaic_reff_routes(c(10, 10, 10), c(0, 3, 0), rep(0.1, 3), k)
  })
  expect_equal(r2$R_hum, c(2, 2, 2))
  expect_true(all(is.na(r2$R_env)))
  expect_equal(r2$R_eff, c(2, NA, 2))
})

test_that("route plateaus match the discrete Euler-Lotka values under growth", {
  Tn <- 1500L; r <- 0.01; f <- 0.3; d <- 1 / 50
  k <- kern_default()
  inc <- exp(r * seq_len(Tn))
  rr <- MOSAIC:::.mosaic_reff_routes(f * inc, (1 - f) * inc, rep(d, Tn), k,
                                     infectiousness_floor = 0)
  kp <- MOSAIC:::.mosaic_reff_kernel_pmf(k, d, tail = 1e-12)
  el <- function(g) 1 / sum(g * exp(-r * seq_along(g)))
  late <- (Tn - 50L):Tn
  expect_equal(mean(rr$R_hum[late]), f * el(kp$hum), tolerance = 1e-6)
  expect_equal(mean(rr$R_env[late]), (1 - f) * el(kp$env), tolerance = 1e-4)
  # The long environmental interval makes the same growth rate imply a much
  # larger environmental R than a human-timed kernel would.
  expect_gt(el(kp$env), 1.5 * el(kp$hum))
})

test_that("initial infectious stocks enter Lambda, so R is defined from day 2", {
  Tn <- 40L; k <- kern_default()
  inc <- rep(0, Tn); inc[10:Tn] <- 5
  no_init <- MOSAIC:::.mosaic_reff_infectiousness(inc, rep(0.05, Tn), k)
  with_init <- MOSAIC:::.mosaic_reff_infectiousness(inc, rep(0.05, Tn), k,
                                                    init = c(10, 50, 100))
  expect_true(all(no_init$Lambda_hum[2:9] == 0))
  expect_true(all(with_init$Lambda_hum[2:9] > 1))
  expect_true(all(with_init$Lambda_env[3:9] > 0))
  # The initial people are propagated exactly like an incidence cohort: 50
  # symptomatic and 100 asymptomatic infectious on day 1 give Lambda_hum[2] =
  # 150 / D_h.
  expect_equal(MOSAIC:::.mosaic_reff_infectiousness(
    numeric(Tn), rep(0.05, Tn), k, init = c(0, 50, 100))$Lambda_hum[2], 150 / k$D_h)
  # Result index 1 already holds one day of shedding from the initial people.
  k2 <- kern_default(zeta_1 = 3, zeta_2 = 1)
  lam0 <- MOSAIC:::.mosaic_reff_infectiousness(numeric(5), rep(0.05, 5), k2,
                                              init = c(0, 8, 4),
                                              shed_abs = c(3, 1))
  expect_equal(lam0$W_hat[1], 3 * 8 + 1 * 4)
  expect_equal(lam0$Lambda_env[2],
               (k2$w1 * 8 + k2$w2 * 4) * 0.05 /
                 (k2$w1 * k2$sigma / k2$p1 + k2$w2 * (1 - k2$sigma) / k2$p2))
  expect_equal(MOSAIC:::.mosaic_reff_init(20, 3, 4, 5), c(15, 3, 4))
  expect_equal(MOSAIC:::.mosaic_reff_init(NULL, NULL, 4, 5), c(0, 0, 4))
})

test_that("cell quantiles need half the member weight defined", {
  M <- rbind(c(1, NA, 3), c(2, NA, NA), c(3, 5, NA))
  w <- c(0.2, 0.3, 0.5)
  q <- MOSAIC:::.mosaic_reff_cell_quantiles(M, w, 0.5)
  expect_equal(q[1, 1], weighted_quantiles(c(1, 2, 3), w, 0.5))
  expect_equal(q[2, 1], 5)                 # member 3 alone holds 0.5 of the weight
  expect_true(is.na(q[3, 1]))              # member 1 alone holds 0.2
})

# -----------------------------------------------------------------------------
# calc_Reff() on a real engine trajectory
# -----------------------------------------------------------------------------
sim_traj_fixture <- function(lines = NULL) {
  cfg <- MOSAIC::config_simulation_epidemic
  cfg$zeta_1 <- 1e6; cfg$zeta_2 <- 2e5
  r <- run_simulation(config = cfg, seed = 7L, quiet = TRUE)$results
  nL <- nrow(r$incidence); Tn <- ncol(r$incidence)
  ch <- c("incidence", "incidence_human", "incidence_env", "E", "Isym", "Iasym")
  if (is.null(lines))
    lines <- data.frame(member_id = integer(0), weight = numeric(0),
                        location = character(0), channel = character(0),
                        t = integer(0), value = numeric(0),
                        stringsAsFactors = FALSE)
  traj <- structure(list(
    schema = "mosaic_trajectories", channels = ch,
    location_names = cfg$location_name, n_locations = nL, n_time_points = Tn,
    date_start = cfg$date_start, date_stop = cfg$date_stop,
    summary = stats::setNames(lapply(ch, function(x)
      list(median = matrix(as.numeric(r[[x]]), nL, Tn))), ch),
    lines = lines), class = "mosaic_trajectories")
  list(traj = traj, cfg = cfg, r = r)
}

test_that("calc_Reff returns the three estimands, matching the route core", {
  skip_if_not(exists("config_simulation_epidemic", asNamespace("MOSAIC")))
  fx <- sim_traj_fixture()
  res <- calc_Reff(fx$traj, fx$cfg, verbose = FALSE)
  expect_s3_class(res, "reproductive_numbers")
  expect_setequal(unique(res$estimand), c("R_eff", "R_hum", "R_env"))
  nL <- fx$traj$n_locations; Tn <- fx$traj$n_time_points
  expect_equal(nrow(res), 3L * nL * Tn)
  expect_equal(attr(res, "kernel"), "route_instantaneous")

  kern <- MOSAIC:::.mosaic_reff_config_kernel(fx$cfg)
  i <- 3L
  ref <- MOSAIC:::.mosaic_reff_routes(
    fx$r$incidence_human[i, ], fx$r$incidence_env[i, ], fx$r$delta_jt[i, ], kern,
    init = MOSAIC:::.mosaic_reff_init(fx$r$E[i, 1], fx$r$Isym[i, 1],
                                      fx$r$Iasym[i, 1], fx$r$incidence[i, 1]))
  loc <- fx$cfg$location_name[i]
  for (e in c("R_eff", "R_hum", "R_env"))
    expect_equal(res$central[res$estimand == e & res$location == loc], ref[[e]])
  expect_equal(attr(res, "central_matrix")[i, ], ref$R_eff)
  kp <- attr(res, "kernel_params")
  expect_true(all(c("mean_hum", "mean_env_min", "mean_env_max") %in% names(kp)))
  expect_gt(kp[["mean_env_max"]], kp[["mean_hum"]])
})

test_that("calc_Reff errors without the route channels or kernel params", {
  skip_if_not(exists("config_simulation_epidemic", asNamespace("MOSAIC")))
  fx <- sim_traj_fixture()
  tr <- fx$traj; tr$summary$incidence_env <- NULL
  expect_error(calc_Reff(tr, fx$cfg, verbose = FALSE), "incidence_env")
  cfg <- fx$cfg; cfg$zeta_2 <- NULL
  expect_error(calc_Reff(fx$traj, cfg, verbose = FALSE), "zeta_2")
  expect_error(calc_Reff(list(a = 1), fx$cfg, verbose = FALSE), "mosaic_trajectories")
})

test_that("calc_Reff refuses a CI from lines that do not start on day 1", {
  skip_if_not(exists("config_simulation_epidemic", asNamespace("MOSAIC")))
  lines <- do.call(rbind, lapply(c("incidence_human", "incidence_env"), function(ch)
    data.frame(member_id = 1L, weight = 1, location = "FOO", channel = ch,
               t = 21:300, value = 5, stringsAsFactors = FALSE)))
  fx <- sim_traj_fixture(lines)
  expect_warning(res <- calc_Reff(fx$traj, fx$cfg, verbose = FALSE), "day 21")
  expect_equal(attr(res, "ci_source"), "unavailable_lines_not_from_start")
  expect_true(all(is.na(res$q50)))
})

test_that("calc_Reff rejects a config whose locations or start date differ", {
  skip_if_not(exists("config_simulation_epidemic", asNamespace("MOSAIC")))
  fx <- sim_traj_fixture()
  cfg <- fx$cfg; cfg$location_name <- rev(cfg$location_name)
  expect_error(calc_Reff(fx$traj, cfg, verbose = FALSE), "location_name")
  tr <- fx$traj; tr$date_start <- "2019-12-01"
  expect_error(calc_Reff(tr, fx$cfg, verbose = FALSE), "date_start")
})

test_that("calc_Reff warn-skips the CI on strided lines and NA-fills with none", {
  skip_if_not(exists("config_simulation_epidemic", asNamespace("MOSAIC")))
  fx0 <- sim_traj_fixture()
  res0 <- calc_Reff(fx0$traj, fx0$cfg, verbose = FALSE)
  expect_equal(attr(res0, "ci_source"), "unavailable_no_incidence_lines")
  expect_true(all(is.na(res0$q2.5)))

  ts <- seq(1L, 300L, by = 7L)
  lines <- do.call(rbind, lapply(c("incidence_human", "incidence_env"), function(ch)
    data.frame(member_id = 1L, weight = 1, location = "FOO", channel = ch,
               t = ts, value = 5, stringsAsFactors = FALSE)))
  fx <- sim_traj_fixture(lines)
  expect_warning(res <- calc_Reff(fx$traj, fx$cfg, verbose = FALSE), "strided")
  expect_equal(attr(res, "ci_source"), "unavailable_strided_lines")
  expect_true(any(is.finite(res$central)))
})

test_that("calc_Reff posterior quantiles align member weights by id", {
  skip_if_not(exists("config_simulation_epidemic", asNamespace("MOSAIC")))
  fx0 <- sim_traj_fixture()
  Tn <- fx0$traj$n_time_points
  # Three members at LOC FOO, member 2 absent at BAR: positions shift there.
  rates <- c(`1` = 0.004, `2` = 0.008, `3` = 0.02)
  mk <- function(loc, ids) do.call(rbind, lapply(ids, function(id) {
    v <- 20 * exp(rates[[as.character(id)]] * seq_len(Tn))
    rbind(data.frame(member_id = id, weight = 1 / length(ids), location = loc,
                     channel = "incidence_human", t = seq_len(Tn), value = 0.3 * v,
                     stringsAsFactors = FALSE),
          data.frame(member_id = id, weight = 1 / length(ids), location = loc,
                     channel = "incidence_env", t = seq_len(Tn), value = 0.7 * v,
                     stringsAsFactors = FALSE))
  }))
  lines <- rbind(mk("FOO", c(1, 2, 3)), mk("BAR", c(1, 3)))
  fx <- sim_traj_fixture(lines)
  w <- c(`1` = 1e-6, `2` = 1e-6, `3` = 1)
  probs <- c(0.025, 0.5, 0.975)
  res <- calc_Reff(fx$traj, fx$cfg, probs = probs, weights = w, verbose = FALSE)
  expect_equal(attr(res, "ci_source"), "weighted_quantiles_per_member")

  kern <- MOSAIC:::.mosaic_reff_config_kernel(fx$cfg)
  delta <- fx$r$delta_jt
  tstar <- 300L
  member_R <- function(id, i) {
    v <- 20 * exp(rates[[as.character(id)]] * seq_len(Tn))
    init <- MOSAIC:::.mosaic_reff_init(fx$r$E[i, 1], fx$r$Isym[i, 1], fx$r$Iasym[i, 1],
                                       fx$r$incidence[i, 1])
    MOSAIC:::.mosaic_reff_routes(0.3 * v, 0.7 * v, delta[i, ], kern,
                                 init = init)$R_eff[tstar]
  }
  i_bar <- match("BAR", fx$cfg$location_name)
  got <- as.numeric(res[res$location == "BAR" & res$estimand == "R_eff" &
                          res$t == tstar, c("q2.5", "q50", "q97.5")])
  ref <- weighted_quantiles(c(member_R(1, i_bar), member_R(3, i_bar)),
                            w[c("1", "3")], probs)
  expect_equal(got, ref, tolerance = 1e-8)
  bug <- weighted_quantiles(c(member_R(1, i_bar), member_R(3, i_bar)),
                            unname(w)[c(1, 2)], probs)
  expect_false(isTRUE(all.equal(got, bug)))
})
