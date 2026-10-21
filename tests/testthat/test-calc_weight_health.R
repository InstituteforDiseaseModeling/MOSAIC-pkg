# calc_weight_health(): the 1.4.0 quantities behind the convergence status
# (ESS_all, best-fit selection, member MCE, tempered k-hat, eta*, flat) and the
# status from calc_convergence_status(). Hand-computed fixtures; nothing here
# may change a posterior weight.

test_that("coarsened ESS and the best-fit selection match a hand-computed fixture", {
  # eta = alpha / n_obs = 5 / 10 = 0.5, so w ∝ exp(0.5 * (0, -1, -2, -3)).
  # ESS_all = (1 + e^-.5 + e^-1 + e^-1.5)^2 / (1 + e^-1 + e^-2 + e^-3) = 3.1095799253566.
  # ratio 0.5 -> target 1.5548: 1 draw ESS 1, 2 draws (1 + e^-.5)^2 / (1 + e^-1) = 1.9242 -> 2 members.
  ll <- c(0, -1, -2, -3)
  wh <- calc_weight_health(ll, params = data.frame(x = c(1, 2, 3, 4)), n_obs = 10,
                           coarsening_alpha = 5, best_subset_ratio = 0.5, ess_best_target = 2,
                           min_members = 1, max_members = 4, n_batches = 2L)
  expect_null(wh$coarsened$eta_ref)                                # retired with WS2
  expect_equal(wh$coarsened$ess_kish, 3.1095799253566, tolerance = 1e-12)
  expect_equal(wh$coarsened$ess_prop, 3.1095799253566 / 4, tolerance = 1e-12)
  expect_identical(wh$coarsened$n_obs, 10)
  expect_identical(wh$bestfit$n_members, 2L)
  expect_identical(wh$bestfit$idx, 1:2)
  expect_equal(wh$bestfit$ess_best, (1 + exp(-0.5))^2 / (1 + exp(-1)), tolerance = 1e-14)
  expect_equal(wh$bestfit$weights, c(1, exp(-0.5)) / (1 + exp(-0.5)), tolerance = 1e-14)
  expect_equal(wh$bestfit$mass, (1 + exp(-0.5)) / sum(exp(-0.5 * 0:3)), tolerance = 1e-14)
  expect_identical(wh$targets$ess_all_target, 4)
  expect_identical(wh$coarsened$eta, 0.5)
  expect_null(wh$likelihood_concentration)
  # the transitional members block of WS1 is gone (run_MOSAIC reads bestfit)
  expect_null(wh[["weights_used", exact = TRUE]])
})

test_that("the member MCE is the seed-batch fixture, and the status reads it", {
  # Uniform likelihood, x = 1..4, two batches {1, 3}, {2, 4}: batch means 2 and 3,
  # posterior SD sqrt(1.25), ratio sqrt(0.4), MCE sqrt(0.4) / sqrt(2) = sqrt(0.2).
  wh <- calc_weight_health(rep(0, 4), params = data.frame(x = 1:4), n_obs = 10,
                           best_subset_ratio = 1, ess_best_target = 4, min_members = 1,
                           max_members = 4, n_batches = 2L)
  expect_identical(wh$bestfit$n_members, 4L)
  expect_equal(wh$mce$p90, sqrt(0.2), tolerance = 1e-14)
  expect_equal(wh$convergence$checks$value[wh$convergence$checks$check == "mce"], sqrt(0.2),
               tolerance = 1e-14)
  expect_identical(wh$convergence$checks$colour[2], "red")
})

test_that("deprecated weights / ess_target / batch_max / ess_method warn and change nothing", {
  set.seed(2)
  n <- 500
  ll <- rnorm(n)
  wu <- numeric(n); wu[37] <- 1
  P <- data.frame(a = runif(n), b = rnorm(n))
  expect_warning(wh <- calc_weight_health(ll, weights = wu, params = P, n_obs = 100, ess_target = 1000,
                                          batch_max = 0.2, ess_method = "perplexity"),
                 "weights, ess_target, batch_max, ess_method ignored since MOSAIC 1.4.0")
  wh0 <- calc_weight_health(ll, params = P, n_obs = 100)
  expect_identical(wh, wh0)
})

test_that("the status is calc_convergence_status() on the computed quantities", {
  set.seed(4)
  n <- 20000
  th <- runif(n)
  ll <- -4e5 * (th - 0.5)^2           # untempered: sd 0.0011, ESS share ~0.003 (not flat)
  P <- data.frame(theta = th, phi = rnorm(n))
  # broad posterior (alpha 4 over 4e5 cells): ESS_all large -> green
  ok <- calc_weight_health(ll, params = P, n_obs = 4e5, coarsening_alpha = 4)
  expect_identical(ok$status, "green")
  expect_identical(ok$reasons, "")
  expect_gte(ok$bestfit$ess_best, 200)
  expect_lt(ok$informativeness$ess_untempered_frac, 0.01)
  again <- calc_convergence_status(
    ess_best = ok$bestfit$ess_best, ess_best_target = 200, mce_p90 = ok$mce$p90,
    khat = ok$coarsened$khat, ess_all = ok$coarsened$ess_kish,
    ess_all_target = 1000, n_draws = n, size_bound = ok$bestfit$binding, eta_star = ok$eta_star$eta,
    ess_untempered_frac = ok$informativeness$ess_untempered_frac,
    n_tied_max = ok$informativeness$n_tied_max, khat_cause = ok$coarsened$khat_status,
    khat_trend = ok$coarsened$khat_trend, khat_trend_slope = ok$coarsened$khat_trend_slope,
    n_attempted = n)
  expect_identical(ok$convergence[names(again)], again)
  expect_identical(MOSAIC:::.mosaic_health_status(ok), ok$convergence)
  # sharp posterior (eta = 10): too few effective draws -> red, with a projection
  bad <- calc_weight_health(ll, params = P, n_obs = 4e5, coarsening_alpha = 4e6)
  expect_identical(bad$status, "red")
  expect_match(bad$reasons, "ensemble ESS [0-9]+ of 200: about [0-9.,]+[kM]? more simulations needed")
  expect_equal(bad$convergence$n_draws_projected, n * 1000 / bad$coarsened$ess_kish)
  # simulations attempted convert the projection: half the draws failed -> twice the simulations
  half <- calc_weight_health(c(ll, rep(NA, n)), params = rbind(P, P), n_obs = 4e5, coarsening_alpha = 4e6,
                             sim = seq_len(2 * n))
  expect_identical(half$context$n_attempted, 2 * n)
  expect_equal(half$convergence$success_rate, 0.5)
  conf <- function(t) min(max(0.25 * t, 500), 1e4)
  tot_bad <- max(bad$convergence$n_draws_projected, n * (bad$mce$p90 / 0.1)^2)
  expect_equal(bad$convergence$sims_projected_for_green, tot_bad + conf(tot_bad), tolerance = 1e-10)
  expect_equal(half$convergence$sims_projected_for_green, 2 * tot_bad + conf(2 * tot_bad), tolerance = 1e-10)
  # context flags pass through
  fx <- calc_weight_health(ll, params = P, n_obs = 4e5, coarsening_alpha = 4, fixed_mode = TRUE,
                           slope = 0.5, budget_exhausted = TRUE)
  expect_identical(ok$bestfit$binding, "max_members")          # broad posterior: 0.2 x ESS_all > 1000 members
  expect_identical(fx$convergence$flags,
                   c("budget_cap_reached", "ess_growth_stalled", "ensemble_size_bound", "fixed_budget"))
  expect_identical(fx$slope, 0.5)
  expect_true(is.na(ok$slope))                         # single snapshot
})

test_that("the eta = 1 concentration never enters the status", {
  set.seed(5)
  n <- 20000
  th <- runif(n)
  ll <- -1e9 * (th - 0.5)^2           # eta = 1 collapses onto one draw
  wh <- calc_weight_health(ll, params = data.frame(theta = th), n_obs = 1e9,
                           coarsening_alpha = 2)
  expect_lt(calc_is_diagnostics(ll)$ess_is, 2)
  expect_identical(wh$status, "green")
})

test_that("k-hat is on the tempered weights, not the eta = 1 ratios", {
  set.seed(8)
  n <- 3000
  ll <- -abs(rnorm(n, sd = 50))
  wh <- calc_weight_health(ll, params = data.frame(a = runif(n)), n_obs = 1000)
  expect_identical(wh$coarsened$khat, calc_is_diagnostics((10 / 1000) * ll)$khat)
  expect_false(identical(wh$coarsened$khat, calc_is_diagnostics(ll)$khat))
  expect_identical(wh$convergence$checks$value[3], wh$coarsened$khat)
})

test_that("eta* is the largest eta meeting the ESS_all target, and n_eff = n_obs * eta*", {
  set.seed(6)
  n <- 20000
  th <- runif(n)
  ll <- -2e4 * (th - 0.3)^2
  wh <- calc_weight_health(ll, params = data.frame(theta = th), n_obs = 2e4,
                           ess_best_target = 100, best_subset_ratio = 0.2)   # target 500
  es <- wh$eta_star
  expect_identical(es$target, 500)
  expect_true(is.finite(es$eta) && es$eta < 1)
  expect_gte(es$ess_kish, 500)
  w_up <- MOSAIC:::.mosaic_tempered_weights(ll, es$eta * 1.01)
  expect_lt(calc_model_ess(w_up, method = "kish"), 500)
  expect_identical(es$binding, "ess")
  expect_equal(es$n_eff, 2e4 * es$eta)
  # eta* is a diagnostic: flatness is the untempered-ESS share, not eta*
  expect_identical(wh$flat, wh$informativeness$ess_untempered_frac >= 0.01)
  expect_match(es$note, "No Monte Carlo-noise ceiling")
})

test_that("a flat likelihood is yellow; too few draws are not assessed (never a false green)", {
  set.seed(7)
  n <- 5000
  flat <- calc_weight_health(rnorm(n, sd = 0.01), params = data.frame(a = runif(n)),
                             n_obs = 100, ess_best_target = 100)
  expect_identical(flat$eta_star$eta, 1)
  expect_gt(flat$informativeness$ess_untempered_frac, 0.99)
  expect_true(flat$flat)
  expect_identical(flat$status, "yellow")
  expect_match(flat$reasons, "the data carry little information")
  # a stub-sized run of an informative likelihood: untempered ESS ~1 of 50 -> not flat (PN6)
  few <- calc_weight_health(-1e4 * abs(rnorm(50)), params = data.frame(a = runif(50)), n_obs = 100)
  expect_true(is.na(few$eta_star$eta))
  expect_true(is.na(few$eta_star$n_eff))
  expect_lt(few$informativeness$ess_untempered, 20)
  expect_false(few$flat)
  expect_identical(few$convergence$checks$colour[4], "green")
  expect_identical(few$bestfit$binding, "min_members")
  # a flat likelihood on the same 50 draws is still flat
  expect_true(calc_weight_health(rnorm(50, sd = 0.01), params = data.frame(a = runif(50)), n_obs = 100)$flat)
})

test_that("an undefined eta* no longer gives a false 'data inform' green (R1/C3)", {
  # 300 draws, fewer than the ESS_all target 1,000: eta* is undefined either way
  set.seed(71)
  n <- 300
  P <- data.frame(a = runif(n))
  sharp <- calc_weight_health(-1e4 * abs(rnorm(n)), params = P, n_obs = 1000)
  expect_true(is.na(sharp$eta_star$eta))
  expect_lt(sharp$informativeness$ess_untempered_frac, 0.01)     # ESS about 1 of 300
  expect_false(sharp$flat)
  expect_identical(sharp$convergence$checks$colour[4], "green")
  expect_match(sharp$convergence$checks$reason[4], "the untempered likelihood ESS is 0.33% of the draws")
  flat <- calc_weight_health(rep(-5, n) + rnorm(n, sd = 1e-3), params = P, n_obs = 1000)
  expect_true(is.na(flat$eta_star$eta))
  expect_true(flat$flat)                                           # detected at 300 draws too
  expect_identical(flat$convergence$checks$colour[4], "yellow")
  expect_true(MOSAIC:::.mosaic_weight_health_fields(flat)$flat_likelihood)
})

test_that("a BFA-like plateau is flat at 7k and at 50k draws, though eta* flips (D1, T1)", {
  # 5% of the draws tie at the best log-likelihood; the rest are sharply worse (as BFA:
  # share 0.054 at 7k, 0.053 at 50k; 808 ties at 50k). Old rule: eta* < 1 at 7k (green),
  # eta* = 1 at 50k (yellow). New rule: flat at both.
  plateau <- function(N, seed) {
    set.seed(seed)
    ll <- -1e4 - 1e5 * abs(rnorm(N))
    ll[sample(N, 0.05 * N)] <- 0
    calc_weight_health(ll, params = data.frame(a = runif(N), b = rnorm(N)), n_obs = 3133)
  }
  small <- plateau(7000, 1)
  big <- plateau(50000, 2)
  expect_lt(small$eta_star$eta, 1)
  expect_identical(big$eta_star$eta, 1)
  for (w in list(small, big)) {
    expect_equal(w$informativeness$ess_untempered_frac, 0.05, tolerance = 1e-9)
    expect_true(w$flat)
    expect_identical(w$convergence$checks$colour[4], "yellow")
    expect_match(w$convergence$checks$reason[4], "draws tie at the best log-likelihood (a plateau", fixed = TRUE)
  }
  expect_identical(small$informativeness$n_tied_max, 350L)
  expect_identical(big$informativeness$n_tied_max, 2500L)
})

test_that("undefined k-hat: green for tied tails, yellow 'not assessed' otherwise (T11)", {
  tied <- calc_weight_health(rep(-1, 3000), params = data.frame(a = runif(3000)), n_obs = 1000)
  expect_match(tied$coarsened$khat_status, "tied")
  expect_identical(tied$convergence$checks$colour[3], "green")
  expect_identical(MOSAIC:::.mosaic_weight_health_fields(tied)$khat_status, "undefined (weights nearly flat)")
  short <- calc_weight_health(-(1:15), params = data.frame(a = runif(15)), n_obs = 1000, min_members = 1)
  expect_identical(short$coarsened$khat_status, "insufficient draws for a tail fit")
  expect_identical(short$convergence$checks$colour[3], "yellow")
  expect_match(short$convergence$checks$reason[3], "not assessed (insufficient draws for a tail fit)", fixed = TRUE)
  expect_identical(MOSAIC:::.mosaic_weight_health_fields(short)$khat_status,
                   "undefined (not assessed: insufficient draws for a tail fit)")
})

test_that("the k-hat trend is read from sim-order prefixes N/4..N", {
  set.seed(31)
  N <- 8000
  ll <- -abs(rnorm(N, sd = 300))
  tr <- MOSAIC:::.mosaic_khat_trend(ll, seq_len(N), eta = 0.01)
  expect_identical(tr$n, c(2000, 2378, 2828, 3364, 4000, 4757, 5657, 6727, 8000)[c(1, 3, 5, 7, 9)])
  kh <- vapply(tr$n, function(n) MOSAIC:::.mosaic_khat_tempered(ll[seq_len(n)], 0.01), numeric(1))
  expect_equal(tr$khat, kh)
  x <- log2(tr$n)
  expect_equal(tr$slope, unname(stats::coef(stats::lm(kh ~ x))[2]), tolerance = 1e-10)
  fit <- summary(stats::lm(kh ~ x))$coefficients
  expect_equal(tr$se, fit[2, 2], tolerance = 1e-10)
  expect_identical(tr$trend, if (abs(tr$slope) < 2 * tr$se) "uncertain"
                             else if (tr$slope <= -0.02) "falling" else "not falling")
  # order follows sim, not position
  o <- sample(N)
  expect_equal(MOSAIC:::.mosaic_khat_trend(ll[o], o, 0.01)$khat, tr$khat)
  expect_true(is.na(MOSAIC:::.mosaic_khat_trend(ll[1:50], NULL, 0.01)$trend))
  expect_true(is.na(MOSAIC:::.mosaic_khat_trend(ll, NULL, NA)$trend))
})

test_that("summary fields are exactly the schema 2 names, and the dropped 1.3.0 fields are absent", {
  set.seed(8)
  n <- 3000
  wh <- calc_weight_health(-abs(rnorm(n, sd = 50)), params = data.frame(a = runif(n)),
                           n_obs = 1000, fixed_mode = TRUE)
  f <- MOSAIC:::.mosaic_weight_health_fields(wh)
  schema2 <- c("convergence_status", "convergence_label", "convergence_reasons",
               "convergence_reasons_text", "convergence_flags", "convergence_flags_text",
               "check_ess_status", "check_mce_status", "check_khat_status",
               "check_informativeness_status", "posterior_weighting", "coarsening_alpha", "eta",
               "n_obs_scored", "ess_all", "ess_all_target", "ess_best", "ess_best_target",
               "best_subset_ratio", "best_subset_mass", "ensemble_members", "ensemble_members_unique",
               "ensemble_binding", "mce_median", "mce_p90", "mce_max", "khat_tempered", "khat_status",
               "eta_star", "eta_star_n_eff", "flat_likelihood", "ess_growth_slope", "sims_used",
               "sims_projected_for_target", "budget_exhausted", "sims_projected_for_green",
               "projection_reliable", "ess_untempered_frac", "n_locations", "alpha_per_location",
               "ess_coarsened", "weight_health_status")
  expect_identical(names(f), schema2)
  for (gone in c("eta_ref", "ess_coarsened_prop", "weight_health_reasons", "likelihood_concentration_note",
                 "convergence_flag_messages", "ensemble_size_binding"))
    expect_false(gone %in% names(f), info = gone)
  expect_false(any(grepl("^weights_used|^batch_ratio", names(f))))
  expect_identical(f$convergence_status, wh$status)
  expect_identical(f$weight_health_status, if (wh$status == "green") "ok" else "warn")
  expect_identical(f$ess_coarsened, f$ess_all)
  expect_identical(as.character(f$convergence_flags), "fixed_budget")
  expect_identical(f$convergence_flags_text, "fixed-budget run: the status is assessed, not used to stop")
  expect_identical(f$check_khat_status, wh$convergence$checks$colour[3])
  expect_s3_class(f$convergence_reasons, "AsIs")
  expect_identical(f$sims_used, wh$n_valid)
  expect_false(f$budget_exhausted)
  expect_false(f$projection_reliable)                      # no slope supplied
  expect_identical(f$ess_untempered_frac, wh$informativeness$ess_untempered_frac)
  expect_true(is.na(f$n_locations) && is.na(f$alpha_per_location))
  f3 <- MOSAIC:::.mosaic_weight_health_fields(
    calc_weight_health(-abs(rnorm(n, sd = 50)), params = data.frame(a = runif(n)), n_obs = 1000,
                       n_locations = 4L, slope = 0.95))
  expect_identical(f3$n_locations, 4L)
  expect_equal(f3$alpha_per_location, 2.5)
  expect_true(f3$projection_reliable)
  if (identical(f3$convergence_status, "green")) expect_true(is.na(f3$sims_projected_for_green))
  expect_true(f$convergence_label %in% c("Converged: enough simulations", "Usable with caution",
                                         "Not converged: more simulations needed"))
  expect_match(f$khat_status, "^-?[0-9.]+ \\((good|marginal|unreliable)\\)$")
  # target met -> no projection; target missed -> N x target / ESS_all
  if (f$ess_all >= f$ess_all_target) expect_true(is.na(f$sims_projected_for_target))
  low <- calc_weight_health(-abs(rnorm(n, sd = 5000)), params = data.frame(a = runif(n)), n_obs = 100)
  fl <- MOSAIC:::.mosaic_weight_health_fields(low)
  expect_lt(fl$ess_all, fl$ess_all_target)
  expect_equal(fl$sims_projected_for_target, n * 1000 / fl$ess_all)
  # flat weights: k-hat undefined
  fz <- MOSAIC:::.mosaic_weight_health_fields(
    calc_weight_health(rep(-1, n), params = data.frame(a = runif(n)), n_obs = 1000))
  expect_identical(fz$khat_status, "undefined (weights nearly flat)")
  expect_true(fz$flat_likelihood)
  expect_equal(fz$ess_untempered_frac, 1)
  # vector fields stay JSON arrays; named per-parameter values survive the JSON shape
  js <- jsonlite::fromJSON(jsonlite::toJSON(f, auto_unbox = TRUE, na = "null"), simplifyVector = FALSE)
  expect_true(is.list(js$convergence_flags))
  wj <- MOSAIC:::.mosaic_weight_health_for_json(wh)
  expect_identical(names(wj$mce$per_parameter), "a")
  # a block read back from JSON (NA as "NA", vectors as lists) gives the same fields
  tf <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(list(wh = wh), tf, auto_unbox = TRUE, digits = NA)
  fj <- expect_silent(MOSAIC:::.mosaic_weight_health_fields(jsonlite::read_json(tf)$wh))
  expect_identical(fj$convergence_flags, f$convergence_flags)
  expect_identical(fj$convergence_status, f$convergence_status)
  expect_identical(fj$check_mce_status, f$check_mce_status)
  expect_equal(fj$ess_best, f$ess_best, tolerance = 1e-12)
  expect_true(is.na(fj$ess_growth_slope))
  # a missing weight-health block still yields every field, as NA
  f0 <- MOSAIC:::.mosaic_weight_health_fields(NULL)
  expect_identical(names(f0), names(f))
  expect_true(is.na(f0$ess_all) && is.na(f0$convergence_status) && is.na(f0$weight_health_status))
  expect_length(f0$convergence_reasons, 0L)
})

test_that("calc_weight_health validates its inputs", {
  expect_error(calc_weight_health(1:3, params = data.frame(a = 1:3), n_obs = 10, sim = 1:2),
               "one entry per draw")
  expect_error(calc_weight_health(1:3, params = data.frame(a = 1:3), n_obs = 10,
                                  coarsening_alpha = 0), "coarsening_alpha")
  expect_error(calc_weight_health(1:3, params = data.frame(a = 1:2), n_obs = 10), "one row per draw")
  expect_error(calc_weight_health(1:3, params = data.frame(a = 1:3), n_obs = 10,
                                  best_subset_ratio = 1.5), "ratio must be a single number")
  expect_error(calc_weight_health(1:3, params = data.frame(a = 1:3), n_obs = 10,
                                  thresholds = list(bogus = 1)), "unknown convergence threshold")
  expect_warning(calc_weight_health(c(0, -1, -2), params = data.frame(a = 1:3), n_obs = 10,
                                    min_members = 1, ess_target = 100, batch_max = 0.2),
                 "ess_target, batch_max ignored since MOSAIC 1.4.0")
})

test_that("n_obs unknown is an error: the learning rate is undefined", {
  expect_error(calc_weight_health(c(0, -1, -2, -3), params = data.frame(a = 1:4), n_obs = NA),
               "n_obs must be a single positive number")
})

test_that(".mosaic_n_scored_obs counts finite observations inside the scored window", {
  cfg <- list(reported_cases  = matrix(c(1, NA, 3, 4, 5, NA), nrow = 1),
              reported_deaths = matrix(c(NA, NA, NA, NA, NA, 1), nrow = 1))
  expect_identical(MOSAIC:::.mosaic_n_scored_obs(cfg), 5)
  # cases from step 3, deaths from step 3: cases 3,4,5 + deaths at 6
  expect_identical(MOSAIC:::.mosaic_n_scored_obs(cfg, list(idx_cases = 3L, idx_deaths = 3L)), 4)
})

# A pre-1.4.0 control.json: the current defaults without the 1.4.0 keys.
.legacy_control <- function(control, weighting) {
  control$targets[c("posterior_weighting", "best_subset_ratio", "min_ensemble_members",
                    "max_ensemble_members", "status_thresholds")] <- NULL
  control$targets$best_subset_weighting <- weighting
  control
}

test_that("calc_run_weight_health backfills a legacy (saturated) run without touching its outputs", {
  skip_if_not_installed("arrow")
  d <- withr::local_tempdir()
  dir.create(file.path(d, "1_inputs")); dir.create(file.path(d, "2_calibration"))
  set.seed(9)
  n_t <- 400
  config <- list(location_name = "MOZ", date_start = "2020-01-01",
                 date_stop = as.character(as.Date("2020-01-01") + n_t - 1),
                 reported_cases = rpois(n_t, 20), reported_deaths = rpois(n_t, 1))
  jsonlite::write_json(config, file.path(d, "1_inputs", "config.json"), auto_unbox = TRUE, digits = NA)
  control <- mosaic_control_defaults(likelihood = list(burn_in_days = 10L),
                                     calibration = list(n_simulations = 3000L),
                                     targets = list(ESS_param = 200, ESS_best = 100))
  control <- .legacy_control(control, "saturated")
  jsonlite::write_json(list(control = MOSAIC:::.mosaic_control_for_json(control)),
                       file.path(d, "1_inputs", "control.json"),
                       auto_unbox = TRUE, digits = NA, null = "null", na = "null")
  n <- 3000
  s <- data.frame(sim = seq_len(n), beta_j0_tot_MOZ = exp(rnorm(n)), gamma_1 = runif(n))
  s$likelihood <- -200 * log(s$beta_j0_tot_MOZ)^2
  top <- order(-s$likelihood)[1:100]
  s$weight_best <- 0; s$weight_best[top] <- 1 / 100
  s$weight_best_opt <- 0; s$weight_best_opt[top[1:40]] <- 1 / 40
  arrow::write_parquet(s, file.path(d, "2_calibration", "samples.parquet"))
  before <- tools::md5sum(file.path(d, "2_calibration", "samples.parquet"))

  r <- calc_run_weight_health(d, write = TRUE)
  expect_true(file.exists(file.path(d, "3_results", "weight_health.json")))
  expect_identical(unname(tools::md5sum(file.path(d, "2_calibration", "samples.parquet"))), unname(before))
  expect_identical(r$fields$n_obs_scored, as.integer(n_t - 10))             # burn-in removed
  expect_equal(r$fields$coarsening_alpha, 10)                     # control default
  expect_equal(r$fields$eta, 10 / (n_t - 10))
  expect_identical(r$fields$posterior_weighting, "saturated")
  js <- jsonlite::read_json(file.path(d, "3_results", "weight_health.json"))
  expect_equal(js$fields$ess_coarsened, r$fields$ess_coarsened, tolerance = 1e-12)
  r2 <- calc_run_weight_health(d, write = FALSE, coarsening_alpha = 6)
  expect_warning(calc_run_weight_health(d, write = FALSE, weight_col = "weight_best"), "weight_col is ignored")
  expect_gt(r2$fields$ess_coarsened, r$fields$ess_coarsened)    # smaller alpha, flatter
  # a pre-1.4.0 control is assessed on the 1.4.0 defaults: its ESS_best (100) is the
  # legacy tier target, not the ensemble target; the run is fixed-budget
  expect_identical(r$fields$ess_best_target, 200)
  expect_identical(r$fields$best_subset_ratio, 0.2)
  expect_identical(r$fields$ess_all_target, 1000)
  expect_identical(as.character(r$fields$convergence_flags), "fixed_budget")
  expect_true(r$fields$convergence_status %in% c("green", "yellow", "red"))
  expect_identical(js$fields$convergence_status, r$fields$convergence_status)
  expect_true(is.list(js$fields$convergence_reasons))
  expect_true(is.list(js$weight_health$mce$per_parameter))       # named, a JSON object
  rp <- MOSAIC:::.mosaic_ess_replay(s$likelihood, s$sim, 10 / (n_t - 10))
  ref <- calc_weight_health(s$likelihood, params = s[, c("beta_j0_tot_MOZ", "gamma_1")],
                            n_obs = n_t - 10, sim = s$sim, fixed_mode = TRUE,
                            slope = MOSAIC:::.mosaic_gate_slope(rp$n, rp$ess_all),
                            n_attempted = n, n_locations = 1L)
  expect_true(is.finite(r$fields$ess_growth_slope))               # replayed (C20)
  expect_identical(r$fields$n_locations, 1L)
  expect_identical(r$weight_health$bestfit$idx, ref$bestfit$idx)
  expect_identical(r$weight_health$convergence, ref$convergence)
  # explicit overrides win
  r3 <- calc_run_weight_health(d, write = FALSE, best_subset_ratio = 0.5, ess_best_target = 50,
                               min_members = 10, max_members = 2000)
  expect_identical(r3$fields$ess_all_target, 100)
  expect_identical(r3$weight_health$targets$min_members, 10L)
})

test_that("calc_run_weight_health on a 1.3.0 coarsened run assesses the 1.4.0 status on the same posterior", {
  skip_if_not_installed("arrow")
  d <- withr::local_tempdir()
  dir.create(file.path(d, "1_inputs")); dir.create(file.path(d, "2_calibration"))
  set.seed(19)
  n_t <- 300
  config <- list(location_name = "MOZ", date_start = "2020-01-01",
                 date_stop = as.character(as.Date("2020-01-01") + n_t - 1),
                 reported_cases = rpois(n_t, 20), reported_deaths = rpois(n_t, 1))
  jsonlite::write_json(config, file.path(d, "1_inputs", "config.json"), auto_unbox = TRUE, digits = NA)
  control <- mosaic_control_defaults(likelihood = list(burn_in_days = 0L),
                                     targets = list(ESS_param = 100, min_best_subset = 30,
                                                    max_best_subset = 500))
  control <- .legacy_control(control, "coarsened")
  jsonlite::write_json(list(control = MOSAIC:::.mosaic_control_for_json(control)),
                       file.path(d, "1_inputs", "control.json"),
                       auto_unbox = TRUE, digits = NA, null = "null", na = "null")
  n <- 2000
  s <- data.frame(sim = seq_len(n), beta_j0_tot_MOZ = exp(rnorm(n)), gamma_1 = runif(n))
  s$likelihood <- -3000 * log(s$beta_j0_tot_MOZ)^2
  cp <- MOSAIC:::.mosaic_coarsened_posterior(s$likelihood, n_t, 10, 30, 500, 7L)
  s$weight_all <- cp$weights
  s$weight_best <- cp$member_weights
  s$is_best_subset <- cp$counts > 0
  arrow::write_parquet(s, file.path(d, "2_calibration", "samples.parquet"))
  r <- calc_run_weight_health(d, write = FALSE)
  expect_identical(r$fields$posterior_weighting, "coarsened")
  expect_equal(r$fields$ess_all, cp$ess_kish, tolerance = 1e-10)          # same alpha: same posterior
  expect_identical(r$fields$ess_best_target, 200)                        # 1.4.0 defaults
  expect_identical(r$fields$convergence_flags, I(character(0)))           # adaptive, under its cap
})


test_that("the k-hat trend label follows the 2-SE rule (TN2)", {
  set.seed(3)
  for (i in 1:15) {
    ll <- -abs(stats::rt(6000, df = 3)) * stats::runif(1, 5, 200)
    tr <- MOSAIC:::.mosaic_khat_trend(ll, NULL, eta = 0.02)
    if (is.na(tr$trend)) next
    expect_identical(tr$trend, if (abs(tr$slope) < 2 * tr$se) "uncertain"
                               else if (tr$slope <= -0.02) "falling" else "not falling")
  }
  # tied log-likelihoods: k-hat undefined at every size, so no trend
  expect_identical(MOSAIC:::.mosaic_khat_trend(rep(-1, 3000), NULL, eta = 0.02)$trend, NA_character_)
})

test_that("a per-scheme ESS_all target drives the reasons and projections (TN3)", {
  set.seed(41)
  n <- 20000
  th <- runif(n)
  ll <- -4e5 * (th - 0.5)^2
  P <- data.frame(theta = th)
  base <- calc_weight_health(ll, params = P, n_obs = 4e5, coarsening_alpha = 4e6)
  coarse <- calc_weight_health(ll, params = P, n_obs = 4e5, coarsening_alpha = 4e6, ess_all_target = 200)
  expect_identical(base$targets$ess_all_target, 1000)
  expect_identical(coarse$targets$ess_all_target, 200)
  expect_equal(coarse$convergence$n_draws_projected, n * 200 / coarse$coarsened$ess_kish)
  expect_identical(coarse$eta_star$target, 200)
  expect_error(calc_weight_health(ll, params = P, n_obs = 4e5, ess_all_target = -1), "ess_all_target")
  # the budget and the confirming-batch bounds are carried for re-derivation
  b <- calc_weight_health(ll, params = P, n_obs = 4e5, coarsening_alpha = 4e6, max_simulations = 150000,
                          batch_size_min = 1000, batch_size_max = 2000)
  expect_identical(b$context$max_simulations, 150000)
  expect_identical(MOSAIC:::.mosaic_health_status(b), b$convergence)
  tot <- max(b$convergence$n_draws_projected, n * (b$mce$p90 / 0.1)^2)
  expect_equal(b$convergence$sims_projected_for_green, tot + 2000, tolerance = 1e-10)
})

test_that("a degenerate weight-health block never reads 'not assessed: not assessed' (CN4)", {
  conv <- calc_convergence_status(ess_best = NA, mce_p90 = NA, khat = NA, n_draws = 1, n_attempted = 40)
  wh <- list(n_draws = 40, n_valid = 1, convergence = conv,
             coarsened = list(khat = NA_real_, khat_status = "not assessed"),
             targets = list(), bestfit = list(), mce = list())
  f <- MOSAIC:::.mosaic_weight_health_fields(wh)
  expect_identical(f$khat_status, "undefined (not assessed: fewer than two valid draws)")
  expect_match(f$convergence_reasons[1], "^the run failed")
  expect_true(is.na(f$sims_projected_for_green))
  expect_true(is.na(f$flat_likelihood))
  wh$n_valid <- 3000
  expect_identical(MOSAIC:::.mosaic_weight_health_fields(wh)$khat_status,
                   "undefined (not assessed: the tail could not be fitted)")
})
