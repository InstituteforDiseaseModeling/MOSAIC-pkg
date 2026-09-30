# Regression tests for the production-readiness review, group "likelihood".
# Each block names the finding it pins; expected values are hand-computed.

cum_nb <- MOSAIC:::.ll_cumulative_progressive_nb

# ---- likelihood-02: k = Inf is the Poisson limit in the cumulative term ------

test_that("cumulative term scores k = Inf as Poisson, finite k as size k * n (likelihood-02)", {
  obs <- rep(10, 4); est <- rep(10, 4)
  expect_equal(cum_nb(obs, est, c(0.5, 1), k_data = Inf),
               mean(c(dpois(20, 20, log = TRUE) / 2, dpois(40, 40, log = TRUE) / 4)),
               tolerance = 1e-12)
  expect_equal(cum_nb(obs, est, c(0.5, 1), k_data = 2),
               mean(c(dnbinom(20, mu = 20, size = 4, log = TRUE) / 2,
                      dnbinom(40, mu = 40, size = 8, log = TRUE) / 4)),
               tolerance = 1e-12)
  # NULL / NA k fall back to the (unscaled) option value.
  expect_equal(cum_nb(obs, est, 1, k_data = NA_real_),
               dnbinom(40, mu = 40, size = 10, log = TRUE) / 4, tolerance = 1e-12)
  expect_equal(cum_nb(obs, est, 1, k_data = NULL),
               dnbinom(40, mu = 40, size = 10, log = TRUE) / 4, tolerance = 1e-12)
})

test_that("a Poisson location is scored at least as tightly as a large-k location (likelihood-02)", {
  obs <- rep(100, 20); est <- rep(130, 20)
  ll_pois <- cum_nb(obs, est, c(0.5, 1), k_data = Inf)
  ll_big  <- cum_nb(obs, est, c(0.5, 1), k_data = 1e4)
  # Pre-fix, k = Inf fell to NB(size = 10) and scored the mismatch far more
  # leniently than k = 1e4 (inverted at the Poisson boundary).
  expect_lt(ll_pois, ll_big)
  expect_equal(ll_pois, ll_big, tolerance = 0.05)
})

# ---- likelihood-03: joint masking; eps floor instead of log(1e6) penalty -----

test_that("cumulative term ignores predictions in observation gaps (likelihood-03)", {
  obs <- c(rep(10, 30), rep(NA, 30))
  ll_perfect <- cum_nb(obs, rep(10, 60), k_data = Inf)
  ll_gapzero <- cum_nb(obs, c(rep(10, 30), rep(0, 30)), k_data = Inf)
  # Pre-fix: -0.347 vs -0.143 -- the gap-zero trajectory scored better.
  expect_equal(ll_perfect, ll_gapzero, tolerance = 1e-12)
  # Hand value: tp 0.25 -> 15 cells, 0.5 -> 30, 0.75 and 1 -> the same 30 cells.
  expect_equal(ll_perfect,
               mean(c(dpois(150, 150, log = TRUE) / 15, rep(dpois(300, 300, log = TRUE) / 30, 3))),
               tolerance = 1e-12)
})

test_that("cumulative term masks cells that weights_time zeroes (likelihood-03)", {
  obs <- rep(10, 6); est <- c(10, 10, 10, 100, 100, 100)
  expect_equal(cum_nb(obs, est, 1, k_data = Inf, weights_time = c(1, 1, 1, 0, 0, 0)),
               dpois(30, 30, log = TRUE) / 3, tolerance = 1e-12)
})

test_that("a zero prediction is scored at the core's eps floor, not a count-linear penalty (likelihood-03)", {
  obs <- rep(5, 4); est <- rep(0, 4)
  # eps_j = max(1e-4, 0.02 * 5) = 0.1 per cell -> predicted sum 0.4.
  expect_equal(cum_nb(obs, est, 1, k_data = Inf, eps_rel = 0.02),
               dpois(20, 0.4, log = TRUE) / 4, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(cum_nb(obs, est, 1, k_data = Inf), -20 * log(1e6) / 4)))
})

# ---- likelihood-04: documented N_obs / N_eval scaling of WIS and cumulative --

test_that("WIS enters at N_obs / length(wis_quantiles) times the per-cell WIS (likelihood-04)", {
  oc <- matrix(c(10, 10, 10, 12), 1); ec <- matrix(10, 1, 4)
  od <- matrix(NA_real_, 1, 4);       ed <- matrix(0, 1, 4)
  ll <- function(w, q) calc_model_likelihood(oc, ec, od, ed, nb_k_cases = Inf, nb_k_deaths = Inf,
                                             weight_wis = w, wis_quantiles = q)
  # Median only: WIS = (0.5 * mean|y - 10|) / 0.5 = 0.5; N_obs = 4, 1 quantile.
  expect_equal(ll(1, 0.5) - ll(0, 0.5), -4 * 0.5, tolerance = 1e-10)
  q5 <- c(0.025, 0.25, 0.5, 0.75, 0.975)
  wis5 <- MOSAIC:::.compute_wis_parametric_row(oc[1, ], ec[1, ], rep(1, 4), q5, k_use = Inf)
  expect_equal(ll(0.3, q5) - ll(0, q5), 0.3 * (4 / 5) * -wis5, tolerance = 1e-10)
})

test_that("the cumulative term enters at N_obs / length(cumulative_timepoints) (likelihood-04)", {
  oc <- matrix(c(10, 12, 8, 11, 9, 10, 13, 7), 1); ec <- matrix(10, 1, 8)
  od <- matrix(NA_real_, 1, 8);                     ed <- matrix(0, 1, 8)
  tp <- c(0.5, 1)
  ll <- function(w) calc_model_likelihood(oc, ec, od, ed, nb_k_cases = Inf, nb_k_deaths = Inf,
                                          weight_cumulative_total = w, cumulative_timepoints = tp)
  expect_equal(ll(0.5) - ll(0),
               0.5 * (8 / 2) * mean(c(dpois(41, 40, log = TRUE) / 4, dpois(80, 80, log = TRUE) / 8)),
               tolerance = 1e-10)
})

# ---- likelihood-05: weights_time zero on all of a location's data -----------

test_that("a location whose data all fall where weights_time = 0 is skipped, not an error (likelihood-05)", {
  n <- 60
  oc <- rbind(c(rep(10, 30), rep(NA, 30)), rep(10, n))
  ec <- matrix(11, 2, n)
  od <- rbind(c(rep(1, 30), rep(NA, 30)), rep(1, n))
  ed <- matrix(1, 2, n)
  wt <- c(rep(0, 30), rep(1, 30))
  # Pre-fix: stop("All weights are zero, cannot compute likelihood.")
  ll_both <- calc_model_likelihood(oc, ec, od, ed, weights_time = wt,
                                   nb_k_cases = Inf, nb_k_deaths = Inf)
  ll_loc2 <- calc_model_likelihood(oc[2, , drop = FALSE], ec[2, , drop = FALSE],
                                   od[2, , drop = FALSE], ed[2, , drop = FALSE],
                                   weights_time = wt, nb_k_cases = Inf, nb_k_deaths = Inf)
  expect_true(is.finite(ll_both))
  expect_equal(ll_both, ll_loc2)
  expect_equal(ll_loc2, 30 * (dpois(10, 11, log = TRUE) + dpois(1, 1, log = TRUE)), tolerance = 1e-10)
})

# ---- likelihood-06: the documented NA return is reachable -------------------

test_that("all-missing observations return NA rather than 0 (likelihood-06)", {
  na <- matrix(NA_real_, 2, 10); est <- matrix(5, 2, 10)
  expect_identical(calc_model_likelihood(na, est, na, est, nb_k_cases = Inf, nb_k_deaths = Inf),
                   NA_real_)
  # An integrated deaths score with no scored weeks (exactly 0) adds nothing ...
  expect_identical(calc_model_likelihood(na, est, na, est, nb_k_cases = Inf, nb_k_deaths = Inf,
                                         ll_deaths_core = c(0, 0)), NA_real_)
  # ... but a real integrated deaths score is kept.
  expect_equal(calc_model_likelihood(na, est, na, est, nb_k_cases = Inf, nb_k_deaths = Inf,
                                     ll_deaths_core = c(-5, 0), weights_location = c(2, 1)),
               -10)
})

# ---- likelihood-09: per-location years for the offset width ------------------

test_that("the location offset width averages SEs over the years that location observes (likelihood-09)", {
  cfg <- MOSAIC::config_simulation_endemic
  d <- as.Date(cfg$date_start) + seq_len(ncol(cfg$reported_deaths)) - 1L
  yr <- as.integer(format(d, "%Y"))
  obs <- matrix(1, length(cfg$location_name), length(d))
  obs[1, yr < 2024] <- NA                 # location 1 observes 2024 only
  cfg$reported_deaths <- obs
  cfg$reported_cases <- obs * 20
  se <- c(0.1, 0.2, 0.3, 0.4, 0.5)        # 2020..2024
  pri <- list(mu_jt = list(sd_year = 0.7, sd_product = 0.3,
                           location = setNames(lapply(cfg$location_name, function(i)
                             list(year = 2020:2024, logit_mean = rep(qlogis(0.02), 5),
                                  logit_se = se)), cfg$location_name)))
  di <- MOSAIC:::.mosaic_resolve_deaths_integration(cfg, list(likelihood = list()), pri, NULL)
  expect_equal(di$sd_shift[1], sqrt(0.3^2 + 0.5^2), tolerance = 1e-12)
  expect_equal(di$sd_shift[2], sqrt(0.3^2 + mean(se^2)), tolerance = 1e-12)
})
