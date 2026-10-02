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

# ---- likelihood-07 through the worker: the n_iterations collapse --------------

test_that("the worker's n_iterations collapse keeps a -Inf replicate (likelihood-07)", {
  cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = "MOZ")
  for (f in c("reported_cases_weight", "reported_deaths_weight")) cfg[[f]] <- NULL
  oc <- cfg$reported_cases; od <- cfg$reported_deaths
  oc[!is.finite(oc)] <- 0; od[!is.finite(od)] <- 0
  stub <- function(config, seed = NULL, quiet = TRUE, ...) {
    list(results = list(reported_cases = round(oc * 1.2), reported_deaths = round(od * 0.4)))
  }
  sw <- list(idx_cases = 31L, idx_deaths = 31L, n_time = ncol(cfg$reported_deaths))
  ls <- list(weight_cases = 1, weight_deaths = 1, eps_rel_cases = 0.02, eps_rel_deaths = 0.25,
             .nb_k_cases_resolved = 0.5, .nb_k_deaths_resolved = 1, .score_window_resolved = sw,
             weights_location = 1, weight_peak_timing = 0, weight_peak_magnitude = 0,
             weight_cumulative_total = 0, weight_wis = 0, sigma_peak_time = 1, sigma_peak_log = 0.5,
             .deaths_integration = list(setup = list(nL = 1L)))
  core_seq <- c(-100, -Inf); n_call <- 0L
  local_mocked_bindings(
    sample_parameters = function(...) cfg,
    run_simulation = stub,
    .mosaic_deaths_ll_integrated = function(di, results, params) {
      n_call <<- n_call + 1L
      list(ll = core_seq[n_call])
    },
    .package = "MOSAIC")
  run_worker <- function(n_it) {
    n_call <<- 0L
    MOSAIC:::.mosaic_run_simulation_worker(
      sim_id = 1L, n_iterations = n_it, priors = NULL, config = cfg, PATHS = NULL,
      dir_cal_samples = tempdir(), param_names_all = "gamma_1",
      param_lookup = MOSAIC:::.mosaic_build_param_lookup("gamma_1", cfg$location_name),
      sampling_args = list(), io = NULL, likelihood_settings = ls, write_shard = FALSE)
  }
  ll_one <- unname(run_worker(1L)[1, "likelihood"])   # replicate 1 alone (core = -100)
  m2 <- run_worker(2L)
  expect_true(is.finite(ll_one))
  expect_equal(nrow(m2), 1L)
  # Replicate 2 scores -Inf (a zero likelihood): the collapse averages likelihoods,
  # so the result is ll_one + log(1/2), not ll_one (which dropping it would give).
  expect_equal(unname(m2[1, "likelihood"]), ll_one - log(2), tolerance = 1e-10)
  expect_equal(unname(m2[1, "likelihood"]), calc_log_mean_exp(c(ll_one, -Inf)), tolerance = 1e-10)
})

# ---- likelihood-03: cumulative sums honour zero confidence weights ------------

test_that("the cumulative sums exclude cells with zero confidence weight (likelihood-03)", {
  obs <- c(10, 20, 30, 40); est <- c(12, 1000, 30, 40)
  ll <- MOSAIC:::.ll_cumulative_progressive_nb(obs, est, timepoints = 1, k_data = Inf,
                                               weights_obs = c(1, 0, 1, 1))
  expect_equal(ll, dpois(80, 82, log = TRUE) / 3, tolerance = 1e-12)
  # Without the confidence row, cell 2's 1000 enters the predicted sum.
  ll_all <- MOSAIC:::.ll_cumulative_progressive_nb(obs, est, timepoints = 1, k_data = Inf)
  expect_equal(ll_all, dpois(100, 1082, log = TRUE) / 4, tolerance = 1e-12)
})

# ---- likelihood-06 in run_MOSAIC: fail fast when nothing is scorable ----------

test_that(".mosaic_unscorable_locations flags locations with no scored-window observation", {
  oc <- rbind(c(NA, 5, NA, NA), c(NA, NA, NA, NA), c(NA, NA, NA, NA))
  od <- rbind(c(NA, NA, NA, NA), c(NA, NA, NA, 1), c(NA, 2, NA, NA))
  # The per-day rule (weekly_cases = FALSE): any finite observation counts.
  u <- function(...) MOSAIC:::.mosaic_unscorable_locations(oc, od, ..., weekly_cases = FALSE)
  expect_identical(u(), c(FALSE, FALSE, FALSE))
  # From step 3 on, location 1's case and location 3's death are both unscored.
  expect_identical(u(3L, 3L), c(TRUE, FALSE, TRUE))
  # Cases start at min(idx_cases, idx_deaths), the worker's shared slice start.
  expect_identical(u(3L, 2L), c(FALSE, FALSE, FALSE))
  # The weekly default: one observed day of cases is not three complete weeks.
  expect_identical(MOSAIC:::.mosaic_unscorable_locations(oc, od), c(TRUE, FALSE, FALSE))
})

test_that(".mosaic_unscorable_locations applies the weekly core's complete-week rule (LIK-2)", {
  # A cases-only location short of three complete weeks scores NA in
  # calc_model_likelihood() under the weekly core, so the pre-flight check must
  # see it as unscorable too, or a run of such locations would start on a flat
  # likelihood. The dates are not known there: the bound takes the best of the
  # seven week alignments, so it never flags a location the likelihood scores.
  d0 <- as.Date("2024-01-01")                     # a Monday
  na28 <- rep(NA_real_, 28)
  two   <- c(rep(5, 14), rep(NA, 14))            # two complete Monday-Sunday weeks
  three <- c(rep(5, 21), rep(NA, 7))
  f <- function(x, d = na28, ...) MOSAIC:::.mosaic_unscorable_locations(matrix(x, 1L), matrix(d, 1L), ...)
  lik <- function(x) MOSAIC::calc_model_likelihood(matrix(x, 1L), matrix(6, 1L, 28L), matrix(na28, 1L),
                                                   matrix(na28, 1L), config = list(date_start = d0,
                                                   date_stop = d0 + 27L), nb_k_cases = 2, nb_k_deaths = Inf)
  expect_true(f(two));    expect_identical(lik(two), NA_real_)
  expect_false(f(three)); expect_true(is.finite(lik(three)))
  expect_false(f(two, d = c(rep(NA, 27), 1)))      # a death keeps it scorable
  expect_false(f(two, weekly_cases = FALSE))        # the per-day rule
  # cases before idx_cases are masked by the worker: 20 days left
  expect_true(f(c(rep(NA, 7), rep(5, 21)), idx_cases = 9L))
  # a run cut by the window start: 21 finite days hold three 7-day blocks at
  # one alignment, so the conservative bound passes them
  expect_false(f(c(rep(NA, 2), rep(5, 21), rep(NA, 5))))
  expect_identical(MOSAIC:::.max_complete_weeks(rbind(rep(TRUE, 6), rep(TRUE, 6))), c(0L, 0L))
  # runs of 14 and 13 days: at most 2 + 1 blocks, reached at one alignment
  expect_identical(MOSAIC:::.max_complete_weeks(rbind(c(rep(TRUE, 14), FALSE, rep(TRUE, 13)),
                                                      rep(TRUE, 28), c(rep(TRUE, 13), FALSE, rep(TRUE, 14)))),
                   c(3L, 4L, 2L + 1L))
})

test_that("run_MOSAIC stops up front when no location has a scorable observation", {
  cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = "ERI")
  skip_if(any(is.finite(cfg$reported_cases)) || any(is.finite(cfg$reported_deaths)),
          "ERI has observations in this config_default")
  pri <- MOSAIC::get_location_priors("ERI", MOSAIC::priors_default)
  withr::local_options(root_directory = withr::local_tempdir())
  expect_error(
    suppressWarnings(suppressMessages(run_MOSAIC(
      cfg, pri, file.path(withr::local_tempdir(), "out"),
      control = mosaic_control_defaults(calibration = list(n_simulations = 10L))))),
    "every likelihood would be NA")
})
