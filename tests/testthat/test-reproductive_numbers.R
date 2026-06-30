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

  # Mean/variance of the realized binned pmf reproduce the targets within the
  # daily-binning tolerance (bin midpoints d - 0.5).
  d  <- seq_along(g)
  mx <- (d - 0.5)
  m1 <- sum(mx * g)
  m2 <- sum((mx - m1)^2 * g)
  expect_equal(m1, E_target, tolerance = 0.05 * E_target)
  expect_equal(m2, V_target, tolerance = 0.05 * V_target)
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
  r   <- MOSAIC:::.cori_reff(inc, g)
  expect_true(is.na(r[1]))           # no history at all
  expect_false(is.na(r[2]))          # one lag available
})

# -----------------------------------------------------------------------------
# Test 2b: Euler-Lotka regression on exponential-growth incidence
# -----------------------------------------------------------------------------
test_that(".cori_reff recovers the Euler-Lotka R on exponential growth", {
  # I_t = exp(r t); with a fixed normalized kernel g, the renewal estimator's
  # post-warm-up plateau equals R = 1 / sum_tau exp(-r*tau) g(tau).
  rate <- 0.05
  g    <- MOSAIC:::.mosaic_generation_time_pmf(1 / 1.4, 0.1, 0.5, 0.25, max_days = 40L)
  t    <- seq_len(300)
  inc  <- exp(rate * t)

  reff <- MOSAIC:::.cori_reff(inc, g)

  tau   <- seq_along(g)
  R_EL  <- 1 / sum(exp(-rate * tau) * g)

  # Evaluate on a late plateau (window fully populated, far past warm-up).
  plateau <- reff[250:300]
  expect_equal(plateau, rep(R_EL, length(plateau)), tolerance = 1e-6)
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
# Test 8 pointer: the converted TODO stubs from test-lasik_calculations.R now
# live in this file (kernel + estimator tests above).
# -----------------------------------------------------------------------------
test_that("reproductive-number tests have a home (converted lasik TODO stubs)", {
  expect_true(exists("calc_Reff"))
  expect_true(is.function(MOSAIC:::.cori_reff))
  expect_true(is.function(MOSAIC:::.mosaic_generation_time_pmf))
})
