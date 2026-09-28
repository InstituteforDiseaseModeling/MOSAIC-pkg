# Tests for the route-decomposed Cori R_eff (R_eff = R_hum + R_env):
#   - .mosaic_reff_route_kernel()     engine-derived cohort state tables
#   - .mosaic_reff_infectiousness()   Lambda_hum / Lambda_env (exact filter)
#   - .mosaic_reff_kernel_pmf()       constant-delta kernels
#   - .cori_reff()                    route ratio + floor
#   - .mosaic_reff_ic_start()         initial-condition mask
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
# Lambda_env is exact under time-varying delta
# -----------------------------------------------------------------------------
test_that("time-varying Lambda_env equals a brute-force per-cohort sum", {
  set.seed(11)
  Tn <- 80L
  k <- kern_default(zeta_1 = 2, zeta_2 = 1)
  inc <- rpois(Tn, 50)
  delta <- 1 / (16 + 180 * (0.5 + 0.5 * sin(seq_len(Tn) / 9)))
  lam <- MOSAIC:::.mosaic_reff_infectiousness(inc, delta, k)

  # Brute force: propagate each cohort alone along the SAME delta path (held at
  # delta[T] beyond the series), normalize by its own lifetime total.
  H <- 6000L
  d_ext <- c(delta, rep(delta[Tn], H))
  cohort_W <- function(u) {
    n <- Tn + H; E <- Is <- Ia <- W <- numeric(n); E[u] <- 1
    for (t in seq_len(n - 1L)) {
      fl <- k$p_i * E[t]
      E[t + 1L]  <- E[t] - fl + (t + 1L == u)
      Is[t + 1L] <- Is[t] * (1 - k$p1) + k$sigma * fl
      Ia[t + 1L] <- Ia[t] * (1 - k$p2) + (1 - k$sigma) * fl
      W[t + 1L]  <- W[t] * (1 - d_ext[t + 1L]) + k$w1 * Is[t] + k$w2 * Ia[t]
    }
    W
  }
  brute <- numeric(Tn)
  for (u in seq_len(Tn)) {
    W <- cohort_W(u)
    prof <- W / sum(W)
    brute <- brute + inc[u] * c(0, prof[seq_len(Tn - 1L)])
  }
  expect_equal(lam$Lambda_env, brute, tolerance = 1e-6)
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
  out <- run_simulation(config = cfg, seed = 7L, quiet = TRUE)
  r <- out$results
  kern <- MOSAIC:::.mosaic_reff_config_kernel(cfg)
  i <- which.max(rowSums(r$incidence))
  lam <- MOSAIC:::.mosaic_reff_infectiousness(
    r$incidence[i, ], r$delta_jt[i, ], kern,
    shed_abs = (1 - cfg$theta_j[i]) * c(cfg$zeta_1, cfg$zeta_2))
  I_obs <- r$Isym[i, ] + r$Iasym[i, ]
  W_obs <- r$W[i, ]
  win <- which(I_obs > 100)
  win <- win[win > 30]
  expect_gt(length(win), 30L)
  expect_gt(stats::cor(lam$I_hat[win], I_obs[win]), 0.99)
  expect_lt(abs(stats::median(lam$I_hat[win] / I_obs[win]) - 1), 0.05)
  expect_gt(stats::cor(lam$W_hat[win], W_obs[win]), 0.99)
  expect_lt(abs(stats::median(lam$W_hat[win] / W_obs[win]) - 1), 0.05)
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

test_that("R_eff is exactly R_hum + R_env; a silent route contributes 0", {
  set.seed(5)
  Tn <- 200L; k <- kern_default()
  inc_h <- rpois(Tn, 30); inc_e <- rpois(Tn, 70)
  rr <- MOSAIC:::.mosaic_reff_routes(inc_h, inc_e, rep(1 / 40, Tn), k)
  ok <- is.finite(rr$R_hum) & is.finite(rr$R_env)
  expect_true(any(ok))
  expect_equal(rr$R_eff[ok], rr$R_hum[ok] + rr$R_env[ok])
  expect_true(all(is.na(rr$R_eff[!ok])))

  rr0 <- MOSAIC:::.mosaic_reff_routes(inc_h + inc_e, numeric(Tn), rep(1 / 40, Tn), k)
  f <- is.finite(rr0$R_eff)
  expect_true(all(rr0$R_env[f] == 0))
  expect_equal(rr0$R_eff[f], rr0$R_hum[f])
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

test_that(".mosaic_reff_ic_start finds the first explained day", {
  obs <- c(100, 60, 40, 30, 30, 30)
  hat <- c(0, 10, 30, 29, 31, 30)
  expect_equal(MOSAIC:::.mosaic_reff_ic_start(hat, obs, tol = 0.95), 4L)
  expect_equal(MOSAIC:::.mosaic_reff_ic_start(hat, NULL), 0L)
  expect_equal(MOSAIC:::.mosaic_reff_ic_start(c(0, 0), c(5, 5)), 2L)
  # rescale: a constant scale mismatch is not a transient.
  obs2 <- c(1000, 300, 200, 200, 200, 200, 200, 200)
  hat2 <- c(0, 40, 38, 40, 40, 40, 40, 40)
  expect_equal(MOSAIC:::.mosaic_reff_ic_start(hat2, obs2), 8L)
  expect_equal(MOSAIC:::.mosaic_reff_ic_start(hat2, obs2, rescale = TRUE), 3L)
})

test_that("the IC mask blanks the leading days of each route", {
  Tn <- 100L; k <- kern_default()
  inc <- rep(10, Tn)
  stocks <- list(I = c(rep(1e4, 20), rep(40, Tn - 20)),
                 W = c(rep(1e9, 50), rep(1, Tn - 50)))
  rr <- MOSAIC:::.mosaic_reff_routes(inc / 2, inc / 2, rep(0.05, Tn), k,
                                     stocks = stocks, shed_abs = c(1, 0.5))
  expect_true(rr$ic_start[["hum"]] >= 20L)
  expect_true(rr$ic_start[["env"]] >= 50L)
  expect_true(all(is.na(rr$R_env[seq_len(rr$ic_start[["env"]])])))
  expect_true(all(is.na(rr$R_eff[seq_len(rr$ic_start[["env"]])])))
})

# -----------------------------------------------------------------------------
# calc_Reff() on a real engine trajectory
# -----------------------------------------------------------------------------
sim_traj_fixture <- function(lines = NULL) {
  cfg <- MOSAIC::config_simulation_epidemic
  cfg$zeta_1 <- 1e6; cfg$zeta_2 <- 2e5
  r <- run_simulation(config = cfg, seed = 7L, quiet = TRUE)$results
  nL <- nrow(r$incidence); Tn <- ncol(r$incidence)
  ch <- c("incidence", "incidence_human", "incidence_env", "W", "Isym", "Iasym")
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
  expect_equal(attr(res, "kernel"), "route_exact")

  kern <- MOSAIC:::.mosaic_reff_config_kernel(fx$cfg)
  i <- 3L
  ref <- MOSAIC:::.mosaic_reff_routes(
    fx$r$incidence_human[i, ], fx$r$incidence_env[i, ], fx$r$delta_jt[i, ], kern,
    stocks = list(I = fx$r$Isym[i, ] + fx$r$Iasym[i, ], W = fx$r$W[i, ]),
    shed_abs = (1 - fx$cfg$theta_j[i]) * c(fx$cfg$zeta_1, fx$cfg$zeta_2),
    ic_rescale = TRUE)
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
  ic <- attr(res, "ic_start")
  tstar <- 300L
  member_R <- function(id, i) {
    v <- 20 * exp(rates[[as.character(id)]] * seq_len(Tn))
    MOSAIC:::.mosaic_reff_routes(0.3 * v, 0.7 * v, delta[i, ], kern,
                                 ic_start = c(hum = ic$hum[i], env = ic$env[i])
                                 )$R_eff[tstar]
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
