# Tests for the Cori effective reproductive number (R_eff) Phase 1:
#   - .mosaic_generation_time_pmf()  (two-clock moment-matched kernel)
#   - .cori_reff()                    (pure renewal core)
#   - calc_Reff()                     (ensemble-aware wrapper)
#
# Canonical theory: MOSAIC-docs/04-model-description.Rmd eq:R / eq:I-star and
# the generation-interval moment eqs.

# -----------------------------------------------------------------------------
# Test 1: generation-time kernel -- normalization + moment match
# -----------------------------------------------------------------------------
test_that(".mosaic_generation_time_pmf is a normalized pmf summing to 1", {
  g <- MOSAIC:::.mosaic_generation_time_pmf(
    iota = 1 / 1.4, gamma_1 = 0.1, gamma_2 = 0.5, sigma = 0.25, max_days = 56L)
  expect_length(g, 56L)
  expect_equal(sum(g), 1, tolerance = 1e-12)
  expect_true(all(g >= 0))
})

test_that(".mosaic_generation_time_pmf moments match E[G] and V[G]", {
  iota <- 1 / 1.4; gamma_1 <- 0.1; gamma_2 <- 0.5; sigma <- 0.25
  # Long horizon so daily-binning + truncation error is negligible.
  g <- MOSAIC:::.mosaic_generation_time_pmf(iota, gamma_1, gamma_2, sigma,
                                            max_days = 400L)

  gamma_eff <- 1 / (sigma / gamma_1 + (1 - sigma) / gamma_2)
  E_target  <- 1 / iota + 1 / gamma_eff
  V_inf     <- sigma / gamma_1^2 + (1 - sigma) / gamma_2^2 +
    sigma * (1 - sigma) * (1 / gamma_1 - 1 / gamma_2)^2
  V_target  <- 1 / iota^2 + V_inf

  # Stored provenance attributes equal the analytic moments exactly.
  expect_equal(attr(g, "mean"), E_target, tolerance = 1e-10)
  expect_equal(attr(g, "var"),  V_target, tolerance = 1e-10)
  expect_equal(attr(g, "shape"), E_target^2 / V_target, tolerance = 1e-10)
  expect_equal(attr(g, "rate"),  E_target / V_target,   tolerance = 1e-10)

  # The mean-preserving (EpiEstim discr_si / Cori 2013) discretization places
  # mass so the discretized mean equals E[G] EXACTLY at integer lags d = 1, 2,...
  # (NOT the d - 0.5 bin-midpoint of a naive CDF difference, which would land at
  # E[G] + ~0.5). With max_days = 400 the truncation error is negligible.
  d  <- seq_along(g)
  m1 <- sum(d * g)
  m2 <- sum((d - m1)^2 * g)
  expect_equal(m1, E_target, tolerance = 0.005 * E_target)
  expect_equal(m2, V_target, tolerance = 0.05 * V_target)
})

test_that(".mosaic_generation_time_pmf is mean-preserving (no +0.5 CDF-diff bias)", {
  # Regression for the de-biasing fix: a naive Gamma-CDF difference F(d)-F(d-1)
  # over [d-1,d] yields a discretized mean of ~E[G] + 0.5 (here a ~10% inflation
  # at this low-shape, right-skewed kernel). The mean-preserving discretization
  # must recover E[G] to a TIGHT tolerance, and put zero mass at lag 0.
  iota <- 1 / 1.4; gamma_1 <- 0.1; gamma_2 <- 0.5; sigma <- 0.25
  gamma_eff <- 1 / (sigma / gamma_1 + (1 - sigma) / gamma_2)
  E_target  <- 1 / iota + 1 / gamma_eff       # 5.4 days

  # Long horizon so truncation does not bias the discretized mean.
  g <- MOSAIC:::.mosaic_generation_time_pmf(iota, gamma_1, gamma_2, sigma,
                                            max_days = 600L)
  d <- seq_along(g)
  realized_mean <- sum(d * g)

  # Mean-preserving: within 0.1% of target (the analytic discr_si recovers it
  # exactly; only floating-point + far-tail truncation remain).
  expect_equal(realized_mean, E_target, tolerance = 1e-3 * E_target)

  # A naive CDF-difference kernel over the SAME target Gamma would sit ~0.5 high;
  # assert our kernel is demonstrably below that biased value (catches a
  # regression back to F(d)-F(d-1)).
  s <- E_target^2 / (1 / iota^2 +
        sigma / gamma_1^2 + (1 - sigma) / gamma_2^2 +
        sigma * (1 - sigma) * (1 / gamma_1 - 1 / gamma_2)^2)
  r <- s / E_target
  d600 <- seq_len(600L)
  g_cdfdiff <- stats::pgamma(d600, s, rate = r) -
    stats::pgamma(d600 - 1, s, rate = r)
  g_cdfdiff <- g_cdfdiff / sum(g_cdfdiff)
  mean_cdfdiff <- sum(d600 * g_cdfdiff)
  expect_gt(mean_cdfdiff - realized_mean, 0.3)   # de-biased by ~0.5 day

  # Lag 1 carries the smallest lag's mass; g must be a finite, normalized pmf.
  expect_equal(sum(g), 1, tolerance = 1e-12)
  expect_true(all(g >= 0))
  expect_true(is.finite(g[1]))
})

test_that("V_inf uses the over-dispersed sigma-mixture form (differs from naive)", {
  # For sigma in (0,1) with gamma_1 != gamma_2 the between-class term is > 0,
  # so V_inf strictly exceeds the naive 1/gamma_eff^2.
  gamma_1 <- 0.1; gamma_2 <- 0.5; sigma <- 0.25
  gamma_eff <- 1 / (sigma / gamma_1 + (1 - sigma) / gamma_2)
  V_inf     <- sigma / gamma_1^2 + (1 - sigma) / gamma_2^2 +
    sigma * (1 - sigma) * (1 / gamma_1 - 1 / gamma_2)^2
  between   <- sigma * (1 - sigma) * (1 / gamma_1 - 1 / gamma_2)^2
  expect_gt(between, 0)
  expect_gt(V_inf, 1 / gamma_eff^2)

  # The pmf var attribute reflects this (latent variance is identical either way).
  g <- MOSAIC:::.mosaic_generation_time_pmf(1 / 1.4, gamma_1, gamma_2, sigma,
                                            max_days = 400L)
  V_naive_total <- 1 / (1 / 1.4)^2 + 1 / gamma_eff^2
  expect_gt(attr(g, "var"), V_naive_total)
})

test_that(".mosaic_generation_time_pmf validates its inputs", {
  expect_error(MOSAIC:::.mosaic_generation_time_pmf(-1, 0.1, 0.5, 0.25))
  expect_error(MOSAIC:::.mosaic_generation_time_pmf(0.7, 0.1, 0.5, 1.5))
  expect_error(MOSAIC:::.mosaic_generation_time_pmf(0.7, 0.1, 0.5, 0.25, max_days = 0L))
})

# -----------------------------------------------------------------------------
# Test 2: Cori estimator on a hand-verifiable synthetic series + known kernel
# -----------------------------------------------------------------------------
test_that(".cori_reff matches a hand calculation on a tiny series + kernel", {
  # Kernel over 2 lags: g(1) = 0.6, g(2) = 0.4 (already normalized).
  g   <- c(0.6, 0.4)
  inc <- c(10, 20, 30, 40)
  r   <- MOSAIC:::.cori_reff(inc, g)

  # t = 1: empty window  -> NA
  # t = 2: denom = g(1)*inc(1) = 0.6*10 = 6;        R = 20 / 6
  # t = 3: denom = g(1)*inc(2) + g(2)*inc(1)
  #              = 0.6*20 + 0.4*10 = 16;             R = 30 / 16
  # t = 4: denom = 0.6*30 + 0.4*20 = 26;            R = 40 / 26
  expect_true(is.na(r[1]))
  expect_equal(r[2], 20 / 6,  tolerance = 1e-10)
  expect_equal(r[3], 30 / 16, tolerance = 1e-10)
  expect_equal(r[4], 40 / 26, tolerance = 1e-10)
})

test_that(".cori_reff renormalizes a non-normalized kernel before use", {
  g_raw  <- c(3, 2)            # sums to 5 -> normalized to c(0.6, 0.4)
  g_norm <- c(0.6, 0.4)
  inc    <- c(10, 20, 30, 40)
  expect_equal(MOSAIC:::.cori_reff(inc, g_raw),
               MOSAIC:::.cori_reff(inc, g_norm), tolerance = 1e-12)
})

test_that(".cori_reff returns ~1 on a flat incidence series", {
  g   <- MOSAIC:::.mosaic_generation_time_pmf(1 / 1.4, 0.1, 0.5, 0.25, max_days = 30L)
  inc <- rep(50, 200)
  r   <- MOSAIC:::.cori_reff(inc, g)
  # Once the denominator window is fully populated, flat incidence -> R = 1
  # (the kernel sums to 1, so denom = mean level = numerator).
  tail_vals <- r[150:200]
  expect_equal(tail_vals, rep(1, length(tail_vals)), tolerance = 1e-6)
})

test_that(".cori_reff returns NA for the warm-up steps with an empty window", {
  g   <- c(0.5, 0.3, 0.2)
  inc <- c(5, 5, 5, 5, 5)
  r   <- MOSAIC:::.cori_reff(inc, g, infectiousness_floor = 0)
  expect_true(is.na(r[1]))           # no history at all
  expect_false(is.na(r[2]))          # one lag available
})

# -----------------------------------------------------------------------------
# Test 2c: infectiousness-floor gate -- IC seed spike + deep-trough guards
# -----------------------------------------------------------------------------
test_that(".cori_reff floors the IC-seed spike to NA (not R ~ thousands)", {
  # Real-series repro: t = 1 is an initial-condition seed (a single large
  # injection), so the t = 2 denominator = g(1) * 1 is a tiny fraction of an
  # effective infection and the raw ratio explodes (~3882 in the field report).
  g   <- MOSAIC:::.mosaic_generation_time_pmf(1 / 1.4, 0.1, 0.5, 0.25, max_days = 30L)
  inc <- c(1, 977, 990, 1005, 1020, 1011, 998)

  # Without a floor the t = 2 step is a meaningless spike.
  r_nofloor <- MOSAIC:::.cori_reff(inc, g, infectiousness_floor = 0)
  expect_gt(r_nofloor[2], 100)                       # the pathological spike

  # With the default floor (~1 effective past infection) the IC-seeded step is NA
  # because its generation-weighted denominator g(1)*1 << 1.
  r <- MOSAIC:::.cori_reff(inc, g)                   # default floor = 1
  expect_true(is.na(r[2]))
  # g(1) for this kernel is < 1, so denom at t = 2 is below the floor.
  expect_lt(g[1], 1)
})

test_that(".cori_reff floor leaves normal-denominator steps unaffected", {
  # A well-populated series (large counts, full window) has a denominator far
  # above the floor, so the gate must NOT change those values.
  g   <- MOSAIC:::.mosaic_generation_time_pmf(1 / 1.4, 0.1, 0.5, 0.25, max_days = 30L)
  inc <- rep(500, 60)
  r_floor   <- MOSAIC:::.cori_reff(inc, g)                    # default floor = 1
  r_nofloor <- MOSAIC:::.cori_reff(inc, g, infectiousness_floor = 0)
  expect_equal(r_floor[40:60], r_nofloor[40:60], tolerance = 1e-12)
  expect_true(all(is.finite(r_floor[40:60])))
})

test_that(".cori_reff floor blanks deep inter-epidemic troughs", {
  # A deep trough (denominator ~0) otherwise yields meaningless R ~ 0.01.
  g   <- MOSAIC:::.mosaic_generation_time_pmf(1 / 1.4, 0.1, 0.5, 0.25, max_days = 30L)
  # Big epidemic, then a long near-zero trough, then a tiny blip.
  inc <- c(rep(2000, 20), rep(1e-3, 40), 5)
  r   <- MOSAIC:::.cori_reff(inc, g)                  # default floor = 1
  # The blip at the end sits on a near-zero past window -> gated to NA.
  expect_true(is.na(r[length(inc)]))
})

# -----------------------------------------------------------------------------
# Test 2b: Euler-Lotka regression on exponential-growth incidence
# -----------------------------------------------------------------------------
test_that(".cori_reff plateau equals the discrete Euler-Lotka value (self-consistency)", {
  # I_t = exp(r t); for ANY fixed normalized kernel g the renewal estimator's
  # post-warm-up plateau equals R = 1 / sum_tau exp(-r*tau) g(tau) BY ALGEBRA.
  # This checks the estimator's internal self-consistency only -- it is
  # TAUTOLOGICAL in g (any normalized kernel passes, even a reversed one), so it
  # canNOT detect a biased discretization. The continuous-truth test below is the
  # one that catches kernel-mean bias.
  rate <- 0.05
  g    <- MOSAIC:::.mosaic_generation_time_pmf(1 / 1.4, 0.1, 0.5, 0.25, max_days = 40L)
  t    <- seq_len(300)
  inc  <- exp(rate * t)

  reff <- MOSAIC:::.cori_reff(inc, g, infectiousness_floor = 0)

  tau   <- seq_along(g)
  R_EL  <- 1 / sum(exp(-rate * tau) * g)

  plateau <- reff[250:300]
  expect_equal(plateau, rep(R_EL, length(plateau)), tolerance = 1e-6)
})

test_that(".cori_reff plateau matches the CONTINUOUS Euler-Lotka value (catches kernel bias)", {
  # NON-tautological regression: compare the renewal plateau to the value implied
  # by the underlying CONTINUOUS Gamma generation interval, NOT by the discretized
  # kernel. For exponential growth I_t = e^{rt}, the continuous Euler-Lotka
  # identity is  R = 1 / E[e^{-r X}],  X ~ Gamma(shape s, rate rate_g). The Gamma
  # MGF gives E[e^{-r X}] = (rate_g/(rate_g + r))^s, so
  #   R_continuous = (1 + r/rate_g)^s.
  # A mean-preserving discretization reproduces this within a fraction of a
  # percent (residual = far-tail truncation); a naive CDF-difference kernel
  # (mean shifted +~0.5 day) misses it by ~2-3% at this growth rate.
  iota <- 1 / 1.4; gamma_1 <- 0.1; gamma_2 <- 0.5; sigma <- 0.25
  gamma_eff <- 1 / (sigma / gamma_1 + (1 - sigma) / gamma_2)
  E_G <- 1 / iota + 1 / gamma_eff
  V_inf <- sigma / gamma_1^2 + (1 - sigma) / gamma_2^2 +
    sigma * (1 - sigma) * (1 / gamma_1 - 1 / gamma_2)^2
  V_G <- 1 / iota^2 + V_inf
  s      <- E_G^2 / V_G
  rate_g <- E_G / V_G

  rate <- 0.05
  R_continuous <- (1 + rate / rate_g)^s

  # Long horizon so far-tail truncation is negligible at this skew.
  g   <- MOSAIC:::.mosaic_generation_time_pmf(iota, gamma_1, gamma_2, sigma,
                                              max_days = 200L)
  inc <- exp(rate * seq_len(600))
  plateau <- MOSAIC:::.cori_reff(inc, g, infectiousness_floor = 0)[550:600]

  # With the de-biased kernel this is well within 0.5%; a CDF-difference kernel
  # would be off by ~2.5% and fail.
  expect_equal(mean(plateau), R_continuous, tolerance = 0.005 * R_continuous)
  expect_lt(abs(mean(plateau) - R_continuous) / R_continuous, 0.005)
})

# -----------------------------------------------------------------------------
# Helper: build a minimal mosaic_trajectories fixture
# -----------------------------------------------------------------------------
make_traj_fixture <- function(inc_med, lines = NULL, date_start = "2023-01-01") {
  nL <- nrow(inc_med); Tn <- ncol(inc_med)
  loc_names <- paste0("LOC", seq_len(nL))
  if (is.null(lines))
    lines <- data.frame(member_id = integer(0), weight = numeric(0),
                        location = character(0), channel = character(0),
                        t = integer(0), value = numeric(0),
                        stringsAsFactors = FALSE)
  structure(list(
    schema         = "mosaic_trajectories",
    channels       = "incidence",
    location_names = loc_names,
    n_locations    = nL,
    n_time_points  = Tn,
    date_start     = date_start,
    date_stop      = as.character(as.Date(date_start) + Tn - 1L),
    summary        = list(incidence = list(median = inc_med)),
    lines          = lines
  ), class = "mosaic_trajectories")
}

test_config <- function() list(iota = 1 / 1.4, gamma_1 = 0.1,
                               gamma_2 = 0.5, sigma = 0.25)

# -----------------------------------------------------------------------------
# Test 3: infection-incidence numerator wiring + per-location vectorization
# -----------------------------------------------------------------------------
test_that("calc_Reff central matches the renewal on the median incidence channel", {
  rate <- 0.04
  Tn   <- 120L
  inc1 <- exp(rate * seq_len(Tn))
  inc2 <- exp(0.02 * seq_len(Tn))    # a different patch -> different R_eff
  inc_med <- rbind(inc1, inc2)
  traj <- make_traj_fixture(inc_med)
  cfg  <- test_config()

  res <- calc_Reff(traj, cfg, max_days = 40L, verbose = FALSE)

  expect_s3_class(res, "reproductive_numbers")
  expect_true(all(c("location", "date", "t", "estimand", "central") %in% names(res)))
  expect_setequal(unique(res$estimand), "R_eff")
  expect_equal(attr(res, "series"), "infection_incidence")
  expect_equal(attr(res, "kernel"), "moment_matched")

  g <- MOSAIC:::.mosaic_generation_time_pmf(cfg$iota, cfg$gamma_1, cfg$gamma_2,
                                            cfg$sigma, max_days = 40L)
  exp1 <- MOSAIC:::.cori_reff(inc1, g)
  exp2 <- MOSAIC:::.cori_reff(inc2, g)

  c1 <- res$central[res$location == "LOC1"]
  c2 <- res$central[res$location == "LOC2"]
  expect_equal(c1, exp1, tolerance = 1e-10)
  expect_equal(c2, exp2, tolerance = 1e-10)
  # Distinct per-patch incidence -> distinct R_eff (vectorization is real).
  expect_false(isTRUE(all.equal(c1, c2)))
})

test_that("calc_Reff errors when the incidence channel is absent", {
  traj <- make_traj_fixture(matrix(1, 1, 10))
  traj$summary$incidence <- NULL
  expect_error(calc_Reff(traj, test_config(), verbose = FALSE),
               "incidence")
})

test_that("calc_Reff errors on a non-trajectory ensemble and missing kernel params", {
  expect_error(calc_Reff(list(foo = 1), test_config(), verbose = FALSE),
               "mosaic_trajectories")
  traj <- make_traj_fixture(matrix(1, 1, 10))
  bad  <- test_config(); bad$sigma <- NULL
  expect_error(calc_Reff(traj, bad, verbose = FALSE), "sigma")
})

test_that("calc_Reff accepts a mosaic_ensemble carrying $trajectories", {
  traj <- make_traj_fixture(matrix(exp(0.03 * seq_len(60)), nrow = 1))
  ens  <- list(trajectories = traj)
  res  <- calc_Reff(ens, test_config(), max_days = 30L, verbose = FALSE)
  expect_s3_class(res, "reproductive_numbers")
  expect_equal(nrow(res), 60L)
})

# -----------------------------------------------------------------------------
# Test 4: posterior reduction matches a brute-force weighted_quantiles reference
# -----------------------------------------------------------------------------
test_that("calc_Reff posterior quantiles match a brute-force reference (daily lines)", {
  set.seed(101)
  Tn <- 40L
  nL <- 1L
  inc_med <- matrix(exp(0.03 * seq_len(Tn)), nrow = nL)

  # Three weighted members, DAILY-consecutive lines (stride 1) over t = 1..Tn.
  members <- 1:3
  mw      <- c(0.2, 0.5, 0.3)
  member_inc <- list(
    exp(0.025 * seq_len(Tn)),
    exp(0.035 * seq_len(Tn)),
    exp(0.045 * seq_len(Tn)))
  line_parts <- lapply(seq_along(members), function(mi) {
    data.frame(member_id = members[mi], weight = mw[mi], location = "LOC1",
               channel = "incidence", t = seq_len(Tn), value = member_inc[[mi]],
               stringsAsFactors = FALSE)
  })
  lines <- do.call(rbind, line_parts)
  traj  <- make_traj_fixture(inc_med, lines = lines)
  cfg   <- test_config()
  probs <- c(0.025, 0.5, 0.975)

  res <- calc_Reff(traj, cfg, max_days = 30L, probs = probs, verbose = FALSE)
  expect_equal(attr(res, "ci_source"), "weighted_quantiles_per_member")

  # Brute-force reference at a chosen late time t* (window fully populated).
  g <- MOSAIC:::.mosaic_generation_time_pmf(cfg$iota, cfg$gamma_1, cfg$gamma_2,
                                            cfg$sigma, max_days = 30L)
  reff_m <- vapply(member_inc, function(v) MOSAIC:::.cori_reff(v, g)[35L],
                   numeric(1))
  ref_q  <- weighted_quantiles(reff_m, mw, probs)

  row <- res[res$location == "LOC1" & res$t == 35L, , drop = FALSE]
  got_q <- c(row[["q2.5"]], row[["q50"]], row[["q97.5"]])
  expect_equal(got_q, ref_q, tolerance = 1e-10)

  # Median quantile column exists and the central (point) column is finite there.
  expect_true(is.finite(row$central))
})

# -----------------------------------------------------------------------------
# Test 6: strided / absent lines -> point estimate with NA CI (no fabrication)
# -----------------------------------------------------------------------------
test_that("calc_Reff warn-skips the CI when lines are time-strided", {
  Tn <- 60L
  inc_med <- matrix(exp(0.03 * seq_len(Tn)), nrow = 1)
  # Strided lines: t = 1, 8, 15, ... (stride 7).
  t_strided <- seq(1L, Tn, by = 7L)
  lines <- data.frame(member_id = 1L, weight = 1, location = "LOC1",
                      channel = "incidence", t = t_strided,
                      value = exp(0.03 * t_strided), stringsAsFactors = FALSE)
  traj <- make_traj_fixture(inc_med, lines = lines)

  expect_warning(
    res <- calc_Reff(traj, test_config(), max_days = 30L, verbose = FALSE),
    "strided")
  expect_equal(attr(res, "ci_source"), "unavailable_strided_lines")
  expect_true(all(is.na(res[["q2.5"]])))
  # Point estimate still present.
  expect_true(any(is.finite(res$central)))
})

test_that("calc_Reff returns NA CI when no incidence lines are present", {
  inc_med <- matrix(exp(0.03 * seq_len(50)), nrow = 1)
  traj <- make_traj_fixture(inc_med)   # empty lines
  res  <- calc_Reff(traj, test_config(), max_days = 30L, verbose = FALSE)
  expect_equal(attr(res, "ci_source"), "unavailable_no_incidence_lines")
  expect_true(all(is.na(res[["q2.5"]])))
})

# -----------------------------------------------------------------------------
# Test 7: weights indexed by MEMBER ID, not per-location position
# -----------------------------------------------------------------------------
test_that("calc_Reff `weights` align by member id when a member is dropped in one location", {
  # Two locations, three members (ids 1,2,3). LOC1 carries all three; LOC2 is
  # MISSING member 2 entirely (only ids 1,3 present), so the surviving members'
  # per-location POSITIONS differ: LOC1 (1,2,3) at positions (1,2,3) but LOC2
  # (1,3) at positions (1,2). A position-indexed weight lookup (`weights[mi]`)
  # would map LOC2 position 2 to weights[2] -- member 2's tiny weight -- and put
  # the mass on the WRONG member. An id-indexed lookup must put the big weight on
  # the SAME member id (3) in both locations.
  Tn <- 40L
  inc_med <- rbind(exp(0.03 * seq_len(Tn)), exp(0.02 * seq_len(Tn)))

  member_inc <- list(
    `1` = exp(0.020 * seq_len(Tn)),
    `2` = exp(0.030 * seq_len(Tn)),
    `3` = exp(0.045 * seq_len(Tn)))   # member 3: by far the highest growth -> highest R

  mk_lines <- function(loc, ids) {
    do.call(rbind, lapply(ids, function(id) {
      v <- member_inc[[as.character(id)]]
      data.frame(member_id = id, weight = 1 / length(ids), location = loc,
                 channel = "incidence", t = seq_len(Tn), value = v,
                 stringsAsFactors = FALSE)
    }))
  }
  # LOC1 has all 3 members; LOC2 is missing member 2 entirely (positions shift).
  l1 <- mk_lines("LOC1", c(1, 2, 3))
  l2 <- mk_lines("LOC2", c(1, 3))
  lines <- rbind(l1, l2)
  traj  <- make_traj_fixture(inc_med, lines = lines)

  # Global weights keyed by member id: put essentially ALL mass on member id 3.
  w <- c(`1` = 1e-6, `2` = 1e-6, `3` = 1)
  probs <- c(0.025, 0.5, 0.975)
  res <- calc_Reff(traj, test_config(), max_days = 30L, probs = probs,
                   weights = w, verbose = FALSE)
  expect_equal(attr(res, "ci_source"), "weighted_quantiles_per_member")

  # Build the id-aligned (correct) and position-aligned (buggy) references at a
  # late time. The function must match the id-aligned reference in BOTH
  # locations, and must DIFFER from the position-aligned reference in LOC2 (where
  # the missing member 2 shifts member 3 from position 3 to position 2).
  g <- MOSAIC:::.mosaic_generation_time_pmf(test_config()$iota, test_config()$gamma_1,
                                            test_config()$gamma_2, test_config()$sigma,
                                            max_days = 30L)
  tstar <- 28L
  probs <- c(0.025, 0.5, 0.975)
  r_by_id <- c(`1` = MOSAIC:::.cori_reff(member_inc[["1"]], g)[tstar],
               `2` = MOSAIC:::.cori_reff(member_inc[["2"]], g)[tstar],
               `3` = MOSAIC:::.cori_reff(member_inc[["3"]], g)[tstar])

  qcols <- c("q2.5", "q50", "q97.5")
  got1 <- as.numeric(res[res$location == "LOC1" & res$t == tstar, qcols])
  got2 <- as.numeric(res[res$location == "LOC2" & res$t == tstar, qcols])

  # LOC1 present ids = 1,2,3; LOC2 present ids = 1,3. Correct (id-keyed) weights.
  ref1_id <- weighted_quantiles(r_by_id[c("1", "2", "3")], w[c("1", "2", "3")], probs)
  ref2_id <- weighted_quantiles(r_by_id[c("1", "3")],      w[c("1", "3")],      probs)
  expect_equal(got1, ref1_id, tolerance = 1e-8)
  expect_equal(got2, ref2_id, tolerance = 1e-8)

  # The position-BUG answer for LOC2 would pair the present members (ids 1,3)
  # with weights[c(1,2)] = (member1, member2) weights -- putting the big weight
  # on the WRONG (low-R) member. Assert the function did NOT do that.
  ref2_pos <- weighted_quantiles(r_by_id[c("1", "3")], unname(w)[c(1, 2)], probs)
  expect_false(isTRUE(all.equal(got2, ref2_pos)))
  # And the upper tail is anchored to member 3 (the heavy-weight high-R member).
  expect_gt(got2[3], r_by_id["1"])
})

test_that("calc_Reff `weights = NULL` keeps each member's carried weight (member-aligned)", {
  Tn <- 40L
  inc_med <- matrix(exp(0.03 * seq_len(Tn)), nrow = 1)
  members <- 1:3
  mw      <- c(0.2, 0.5, 0.3)
  member_inc <- list(exp(0.025 * seq_len(Tn)), exp(0.035 * seq_len(Tn)),
                     exp(0.045 * seq_len(Tn)))
  lines <- do.call(rbind, lapply(seq_along(members), function(mi)
    data.frame(member_id = members[mi], weight = mw[mi], location = "LOC1",
               channel = "incidence", t = seq_len(Tn), value = member_inc[[mi]],
               stringsAsFactors = FALSE)))
  traj <- make_traj_fixture(inc_med, lines = lines)
  probs <- c(0.025, 0.5, 0.975)
  res <- calc_Reff(traj, test_config(), max_days = 30L, probs = probs,
                   verbose = FALSE)

  g <- MOSAIC:::.mosaic_generation_time_pmf(test_config()$iota, test_config()$gamma_1,
                                            test_config()$gamma_2, test_config()$sigma,
                                            max_days = 30L)
  reff_m <- vapply(member_inc, function(v) MOSAIC:::.cori_reff(v, g)[35L], numeric(1))
  ref_q  <- weighted_quantiles(reff_m, mw, probs)
  row <- res[res$location == "LOC1" & res$t == 35L, , drop = FALSE]
  expect_equal(c(row[["q2.5"]], row[["q50"]], row[["q97.5"]]), ref_q, tolerance = 1e-10)
})

# -----------------------------------------------------------------------------
# Test 5b: internal (mid-series) non-finite incidence
# -----------------------------------------------------------------------------
test_that(".cori_reff treats a mid-series non-finite value as 0 in the denom, NA in the numerator", {
  # Documented behavior: a non-finite I_t (i) yields NA at its own step (NA
  # numerator) and (ii) contributes 0 to later denominators (treated as absent
  # history), so downstream steps still compute from the finite history.
  g   <- c(0.6, 0.4)
  inc <- c(10, 20, NA, 40, 50)
  r   <- MOSAIC:::.cori_reff(inc, g, infectiousness_floor = 0)

  # t = 3 has NA incidence -> NA at that step.
  expect_true(is.na(r[3]))
  # t = 4 denom = g(1)*inc(3 as 0) + g(2)*inc(2) = 0.6*0 + 0.4*20 = 8; R = 40/8.
  expect_equal(r[4], 40 / 8, tolerance = 1e-10)
  # t = 5 denom = g(1)*inc(4) + g(2)*inc(3 as 0) = 0.6*40 + 0.4*0 = 24; R = 50/24.
  expect_equal(r[5], 50 / 24, tolerance = 1e-10)
})

# -----------------------------------------------------------------------------
# Test 5c: CI window whose daily-consecutive lines start at t_min > 1
# -----------------------------------------------------------------------------
test_that("calc_Reff handles a daily-consecutive CI window that starts at t_min > 1", {
  # The lines are daily-consecutive but only cover t = 21..60 (a forecast-window
  # capture). History before t_min is unavailable to the member reconstruction,
  # so: (i) cells before t_min have NA CI, (ii) the per-member renewal restarts
  # its own warm-up at t_min (the first few in-window steps are NA), and (iii)
  # later in-window cells get a finite CI.
  Tn <- 60L
  t_min <- 21L
  inc_med <- matrix(exp(0.03 * seq_len(Tn)), nrow = 1)
  members <- 1:3
  mw      <- c(0.3, 0.4, 0.3)
  t_win   <- seq(t_min, Tn, by = 1L)
  rates   <- c(0.025, 0.035, 0.045)
  lines <- do.call(rbind, lapply(seq_along(members), function(mi)
    data.frame(member_id = members[mi], weight = mw[mi], location = "LOC1",
               channel = "incidence", t = t_win,
               value = exp(rates[mi] * t_win), stringsAsFactors = FALSE)))
  traj <- make_traj_fixture(inc_med, lines = lines)
  res  <- calc_Reff(traj, test_config(), max_days = 30L,
                    probs = c(0.025, 0.5, 0.975), verbose = FALSE)
  expect_equal(attr(res, "ci_source"), "weighted_quantiles_per_member")

  # Cells before the window start carry no CI.
  before <- res[res$location == "LOC1" & res$t < t_min, , drop = FALSE]
  expect_true(all(is.na(before[["q50"]])))

  # A late in-window cell (well past the window-local warm-up) has a finite CI
  # matching a brute-force reconstruction on the window-local incidence.
  g <- MOSAIC:::.mosaic_generation_time_pmf(test_config()$iota, test_config()$gamma_1,
                                            test_config()$gamma_2, test_config()$sigma,
                                            max_days = 30L)
  tstar <- 55L
  local_idx <- tstar - t_min + 1L
  reff_m <- vapply(seq_along(members), function(mi)
    MOSAIC:::.cori_reff(exp(rates[mi] * t_win), g)[local_idx], numeric(1))
  ref_q <- weighted_quantiles(reff_m, mw, c(0.025, 0.5, 0.975))
  row <- res[res$location == "LOC1" & res$t == tstar, , drop = FALSE]
  expect_equal(c(row[["q2.5"]], row[["q50"]], row[["q97.5"]]), ref_q, tolerance = 1e-8)
  expect_true(is.finite(row[["q50"]]))
})

# -----------------------------------------------------------------------------
# Test 8 pointer: the converted TODO stubs from test-lasik_calculations.R now
# live in this file (kernel + estimator tests above).
# -----------------------------------------------------------------------------
test_that("reproductive-number tests have a home (converted lasik TODO stubs)", {
  expect_true(exists("calc_Reff"))
  expect_true(is.function(MOSAIC:::.cori_reff))
  expect_true(is.function(MOSAIC:::.mosaic_generation_time_pmf))
})
