# calc_convergence_status(): the single green / yellow / red rule (MOSAIC 1.4.0,
# SPEC_CONVERGENCE.md section 5). Every band boundary of every check, the
# worst-of rule, the reasons and the flags.

st <- function(...) {
  args <- utils::modifyList(list(ess_best = 300, ess_best_target = 200, mce_p90 = 0.05,
                                 khat = 0.2, flat = FALSE), list(...))
  do.call(calc_convergence_status, args)
}
colour_of <- function(s, check) s$checks$colour[s$checks$check == check]
flag_txt <- function(s, id) s$flag_text[s$flags == id]

test_that("all four checks green gives green with no reasons and no flags", {
  s <- st()
  expect_identical(s$status, "green")
  expect_identical(s$label, "Converged: enough simulations")
  expect_identical(s$checks$check, c("ess", "mce", "khat", "informativeness"))
  expect_identical(s$checks$colour, rep("green", 4))
  expect_identical(s$reasons, character(0))
  expect_identical(s$flags, character(0))
  expect_identical(s$flag_text, character(0))
  expect_identical(names(s$checks), c("check", "label", "value", "threshold_green", "threshold_yellow",
                                      "colour", "reason"))
  expect_equal(s$checks$threshold_green, c(200, 0.10, 0.5, 0.01))
  expect_equal(s$checks$threshold_yellow, c(100, 0.15, 0.7, NA))
})

test_that("ensemble ESS bands: >= target green, >= 0.5 x target yellow, below red", {
  expect_identical(colour_of(st(ess_best = 200), "ess"), "green")
  expect_identical(colour_of(st(ess_best = 199.999), "ess"), "yellow")
  expect_identical(colour_of(st(ess_best = 100), "ess"), "yellow")
  expect_identical(colour_of(st(ess_best = 99.999), "ess"), "red")
  expect_identical(colour_of(st(ess_best = 0), "ess"), "red")
  expect_identical(colour_of(st(ess_best = NA), "ess"), "red")
  # the bands follow the target
  expect_identical(colour_of(st(ess_best = 399, ess_best_target = 400), "ess"), "yellow")
  expect_identical(colour_of(st(ess_best = 199, ess_best_target = 400), "ess"), "red")
  expect_identical(colour_of(st(ess_best = 140, thresholds = list(ess_yellow_frac = 0.8)),
                             "ess"), "red")
})

test_that("Monte Carlo error bands: <= 0.10 green, <= 0.15 yellow, above red", {
  expect_identical(colour_of(st(mce_p90 = 0), "mce"), "green")
  expect_identical(colour_of(st(mce_p90 = 0.10), "mce"), "green")
  expect_identical(colour_of(st(mce_p90 = 0.1000001), "mce"), "yellow")
  expect_identical(colour_of(st(mce_p90 = 0.15), "mce"), "yellow")
  expect_identical(colour_of(st(mce_p90 = 0.1500001), "mce"), "red")
  expect_identical(colour_of(st(mce_p90 = Inf), "mce"), "red")
  expect_identical(colour_of(st(mce_p90 = NA), "mce"), "yellow")
  expect_identical(colour_of(st(mce_p90 = 0.12, thresholds = list(mce_green = 0.125)), "mce"), "green")
})

test_that("k-hat bands: < 0.5 green, < 0.7 yellow, >= 0.7 red", {
  expect_identical(colour_of(st(khat = -0.3), "khat"), "green")
  expect_identical(colour_of(st(khat = 0.4999), "khat"), "green")
  expect_identical(colour_of(st(khat = 0.5), "khat"), "yellow")
  expect_identical(colour_of(st(khat = 0.6999), "khat"), "yellow")
  expect_identical(colour_of(st(khat = 0.7), "khat"), "red")
  expect_identical(colour_of(st(khat = 3), "khat"), "red")
})

test_that("an undefined k-hat is green only for tied tail ratios; any other cause is not assessed (T11)", {
  s <- st(khat = NA, khat_cause = "tail ratios tied: k-hat undefined")
  expect_identical(colour_of(s, "khat"), "green")
  expect_identical(s$checks$reason[s$checks$check == "khat"], "undefined (weights nearly flat)")
  expect_identical(s$status, "green")
  for (cause in c("insufficient draws for a tail fit", "degenerate: importance ratios underflow to zero",
                  "degenerate: fewer than 5 distinct tail ratios", "eta undefined")) {
    u <- st(khat = NA, khat_cause = cause)
    expect_identical(colour_of(u, "khat"), "yellow")
    expect_identical(u$reasons, paste0("Pareto k-hat of the tempered weights not assessed (", cause, ")"))
  }
  # no cause given: not assessed, never green
  n <- st(khat = NA)
  expect_identical(colour_of(n, "khat"), "yellow")
  expect_match(n$reasons, "not assessed (the tail could not be fitted)", fixed = TRUE)
})

test_that("the k-hat reason follows its trend and no longer claims the PSIS band meaning (D3)", {
  y <- st(khat = 0.62)$reasons
  expect_match(y, "Pareto k-hat of the tempered weights is 0.62 (green < 0.5, red >= 0.7)", fixed = TRUE)
  expect_match(y, "may be optimistic; run more simulations and check that k-hat falls", fixed = TRUE)
  expect_false(grepl("usable|converges slowly|falls slowly", y))
  # falling: the draws at which the trend reaches 0.5, and whether that is within the budget (TN2/RN4)
  # CIV-like: 0.62 at 50,000 valid draws falling 0.03 per doubling -> 50,000 x 2^(0.12 / 0.03) = 800,000
  yf <- st(khat = 0.62, khat_trend = "falling", khat_trend_slope = -0.03, n_draws = 50000,
           max_simulations = 150000, slope = 1)$reasons
  expect_match(yf, paste0("k-hat is falling as draws accumulate (by about 0.03 per doubling of the draws): ",
                          "at that rate it would reach 0.5 at about 800k simulations, beyond the ",
                          "max_simulations_total budget of 150,000"), fixed = TRUE)
  expect_false(grepl("expected", yf))                              # never a promise
  # ZAF-like: 0.575 falling 0.42 per doubling -> 50,000 x 2^(0.075 / 0.42) = 56,600: within the budget
  zf <- st(khat = 0.575, khat_trend = "falling", khat_trend_slope = -0.42, n_draws = 50000,
           max_simulations = 150000, n_attempted = 1e5, slope = 1)$reasons
  expect_match(zf, sprintf("at about %s simulations, within the max_simulations_total budget",
                           MOSAIC:::.mosaic_fmt_draws(50000 * 2^(0.075 / 0.42) / 0.5)), fixed = TRUE)
  # no budget known (fixed mode): the projection without a budget clause
  expect_match(st(khat = 0.62, khat_trend = "falling", khat_trend_slope = -0.03, n_draws = 50000)$reasons,
               "simulations$")
  rf <- st(khat = 0.93, khat_trend = "falling", khat_trend_slope = -0.1, n_draws = 50000)$reasons
  expect_match(rf, "0.93 (>= 0.7): a few draws dominate the weights", fixed = TRUE)
  expect_match(rf, "falling as draws accumulate (by about 0.10 per doubling", fixed = TRUE)
  expect_false(grepl("sharper sampler", rf))
  # stalled ESS growth: the k-hat reason repeats, never contradicts, the inefficiency remedy
  sf <- st(khat = 0.62, khat_trend = "falling", khat_trend_slope = -0.03, n_draws = 50000, slope = 0.3)$reasons
  expect_match(sf, "ESS growth has stalled, so more simulations alone remain inefficient", fixed = TRUE)
  # uncertain trend
  un <- st(khat = 0.62, khat_trend = "uncertain")$reasons
  expect_match(un, "the k-hat trend over the latter part of the run is uncertain", fixed = TRUE)
  rn <- st(khat = 0.93, khat_trend = "not falling")$reasons
  expect_match(rn, "k-hat is not falling as draws accumulate: more simulations alone are inefficient; use a sharper sampler or review the priors",
               fixed = TRUE)
  ru <- st(khat = 0.93)$reasons
  expect_match(ru, "k-hat usually falls as draws accumulate: run more simulations and check that it falls", fixed = TRUE)
  # the trend never changes the colour
  expect_identical(colour_of(st(khat = 0.93, khat_trend = "falling"), "khat"), "red")
})

test_that("a flat likelihood is yellow, never red; unknown informativeness is yellow", {
  s <- st(flat = TRUE)
  expect_identical(colour_of(s, "informativeness"), "yellow")
  expect_identical(s$status, "yellow")
  expect_identical(s$label, "Usable with caution")
  expect_identical(s$reasons,
                   "the data carry little information: the posterior is close to the prior (flat likelihood)")
  expect_identical(colour_of(st(flat = NA), "informativeness"), "yellow")
  expect_match(st(flat = NA)$reasons, "data informativeness not assessed")
  # eta* is a diagnostic only: it is neither the value nor the verdict
  e <- st(flat = FALSE, eta_star = 1, ess_untempered_frac = 3e-5, n_draws = 50000)
  expect_identical(colour_of(e, "informativeness"), "green")
  expect_equal(e$checks$value[4], 3e-5)
})

test_that("informativeness is the N-invariant untempered-ESS share (D1): BFA-like flat at every N", {
  # BFA on the production pool: share 0.0537 at 7k draws, 0.0525 at 50k (808 tied at the max)
  for (N in c(7000, 15000, 30000, 50000)) {
    b <- st(ess_untempered_frac = 0.053, n_draws = N, n_tied_max = 0.016 * N)
    expect_identical(colour_of(b, "informativeness"), "yellow")
    expect_true(b$flat)
    expect_identical(b$status, "yellow")
    expect_match(b$reasons, "spreads its weight over 5.3% of the draws, >= 1%", fixed = TRUE)
    expect_match(b$reasons, sprintf("%s draws tie at the best log-likelihood (a plateau",
                                    MOSAIC:::.mosaic_fmt_count(0.016 * N)), fixed = TRUE)
    # an informative model: ESS(eta = 1) about 1, so a share of about 1/N
    g <- st(ess_untempered_frac = 1 / N, n_draws = N, n_tied_max = 1)
    expect_identical(colour_of(g, "informativeness"), "green")
    expect_false(g$flat)
    expect_false(grepl("plateau", g$checks$reason[4]))
  }
  expect_identical(colour_of(st(ess_untempered_frac = 0.01, n_draws = 5000), "informativeness"), "yellow")
  expect_identical(colour_of(st(ess_untempered_frac = 0.0099, n_draws = 5000), "informativeness"), "green")
  # the share decides even when a legacy flat verdict is passed
  expect_identical(colour_of(st(flat = TRUE, ess_untempered_frac = 2e-5, n_draws = 50000), "informativeness"),
                   "green")
  # the threshold follows the control
  expect_identical(colour_of(st(ess_untempered_frac = 0.03, n_draws = 5000,
                                thresholds = list(flat_ess_frac = 0.05)), "informativeness"), "green")
})

test_that("a flat verdict needs an untempered ESS of at least 20 (PN6); < 2 valid draws is a failed run", {
  # a collapsed (informative) likelihood on a stub-sized run: share 1/60 >= 1%, but ESS 1 -> not flat
  few <- st(ess_untempered_frac = 1 / 60, n_draws = 60)
  expect_identical(colour_of(few, "informativeness"), "green")
  expect_false(few$flat)
  expect_match(few$checks$reason[4], "the untempered likelihood ESS is 1, below the 20 needed to call it flat",
               fixed = TRUE)
  # boundary: ESS exactly 20 (share 0.2 of 100) is flat, 19 is not
  expect_true(st(ess_untempered_frac = 0.2, n_draws = 100)$flat)
  expect_false(st(ess_untempered_frac = 0.19, n_draws = 100)$flat)
  # a genuinely flat likelihood on a small run is still flat (ESS 30 of 60)
  expect_true(st(ess_untempered_frac = 0.5, n_draws = 60)$flat)
  # BFA at 1,000 draws: share 0.054, ESS 54 -> flat
  expect_true(st(ess_untempered_frac = 0.054, n_draws = 1000)$flat)
  # fewer than two valid draws: the run failed; it leads the reasons, every check reads not assessed
  one <- st(ess_best = NA, mce_p90 = NA, khat = NA, n_draws = 1, n_attempted = 40)
  expect_true(is.na(one$flat))
  expect_identical(one$status, "red")
  expect_identical(one$reasons[1], paste0("the run failed: only 1 of 40 simulations has a finite likelihood, so no ",
                                          "posterior or ensemble can be formed; more simulations of the same setup ",
                                          "will not help: check the scored observations, the priors and the ",
                                          "simulation logs"))
  expect_identical(one$reasons[-1], c(
    "ensemble ESS not computed: fewer than two draws have a finite log-likelihood",
    "Monte Carlo error not assessed: fewer than two draws have a finite log-likelihood",
    "Pareto k-hat of the tempered weights not assessed: fewer than two draws have a finite log-likelihood",
    "data informativeness not assessed: fewer than two draws have a finite log-likelihood"))
  expect_false(any(grepl("run more simulations|more simulations needed", one$reasons)))
  expect_true(is.na(one$sims_projected_for_green))
  expect_match(st(n_draws = 0)$reasons[1], "only 0 of the simulations have a finite likelihood", fixed = TRUE)
})

test_that("a plateau makes 'fewer than 5 distinct tail ratios' count as tied (TN1)", {
  cause <- "degenerate: fewer than 5 distinct tail ratios"
  p <- st(khat = NA, khat_cause = cause, n_tied_max = 450, ess_untempered_frac = 0.05, n_draws = 30000)
  expect_identical(colour_of(p, "khat"), "green")
  expect_identical(p$checks$reason[3], "undefined (weights nearly flat)")
  expect_true(p$sampling_green)
  # without a plateau it stays not assessed
  q <- st(khat = NA, khat_cause = cause, n_tied_max = 9)
  expect_identical(colour_of(q, "khat"), "yellow")
  expect_match(q$reasons, "not assessed (degenerate: fewer than 5 distinct tail ratios)", fixed = TRUE)
  # other causes are not rescued by a plateau
  expect_identical(colour_of(st(khat = NA, khat_cause = "insufficient draws for a tail fit", n_tied_max = 500),
                             "khat"), "yellow")
  # no doubled "not assessed"
  expect_identical(MOSAIC:::.mosaic_khat_cause_text("not assessed"), "the tail could not be fitted")
  expect_identical(MOSAIC:::.mosaic_khat_cause_text("not assessed: tail shorter than 5 draws"), "tail shorter than 5 draws")
  expect_identical(MOSAIC:::.mosaic_khat_cause_text(NA), "the tail could not be fitted")
  expect_match(st(khat = NA, khat_cause = "not assessed")$reasons, "not assessed (the tail could not be fitted)",
               fixed = TRUE)
})

test_that("the overall status is the worst row", {
  expect_identical(st(ess_best = 150)$status, "yellow")
  expect_identical(st(mce_p90 = 0.12)$status, "yellow")
  expect_identical(st(khat = 0.6)$status, "yellow")
  expect_identical(st(ess_best = 150, khat = 0.9)$status, "red")
  expect_identical(st(mce_p90 = 0.2, flat = TRUE)$status, "red")
  expect_identical(st(ess_best = 50)$label, "Not converged: more simulations needed")
  # every combination of colours: status = max rank
  grid <- expand.grid(e = c(300, 150, 50), m = c(0.05, 0.12, 0.2), k = c(0.2, 0.6, 0.8),
                      f = c(FALSE, TRUE))
  for (i in seq_len(nrow(grid))) {
    s <- st(ess_best = grid$e[i], mce_p90 = grid$m[i], khat = grid$k[i], flat = grid$f[i])
    worst <- c("green", "yellow", "red")[max(match(s$checks$colour, c("green", "yellow", "red")))]
    expect_identical(s$status, worst)
    expect_length(s$reasons, sum(s$checks$colour != "green"))
    expect_identical(s$sampling_green, all(s$checks$colour[1:3] == "green"))
  }
})

test_that("ESS reasons quote the shortfall and the projected draws in the SPEC wording", {
  # ESS_all 700 of 1000 at 50k draws: 50k * 1000 / 700 = 71,429 -> 21,429 more -> "21k"
  y <- st(ess_best = 140, ess_all = 700, ess_all_target = 1000, n_draws = 50000, slope = 0.98)
  expect_identical(y$reasons, "ensemble ESS 140 of 200: about 21k more simulations would reach the target")
  expect_equal(y$n_draws_projected, 50000 * 1000 / 700)
  expect_equal(y$n_draws_more, 50000 * 1000 / 700 - 50000)
  expect_true(y$projection_reliable)
  # red: "needed"
  r <- st(ess_best = 60, ess_all = 300, ess_all_target = 1000, n_draws = 50000, slope = 1)
  expect_identical(r$reasons[1], "ensemble ESS 60 of 200: about 117k more simulations needed")
  # a supplied projection is used as is
  p <- st(ess_best = 60, n_draws = 50000, n_draws_projected = 140000, slope = 1)
  expect_match(p$reasons[1], "about 90k more simulations needed", fixed = TRUE)
  # no projection available: the generic remedy
  expect_identical(st(ess_best = 60)$reasons[1], "ensemble ESS 60 of 200: run more simulations")
  # stalled growth: the inefficiency remedy is added and flagged
  s <- st(ess_best = 60, ess_all = 300, ess_all_target = 1000, n_draws = 50000, slope = 0.5)
  expect_match(s$reasons[1], "ESS growth has stalled, so more simulations alone are inefficient; use a sharper sampler or review the priors",
               fixed = TRUE)
  expect_match(flag_txt(s, "ess_growth_stalled"), "slope 0.50 < 0.8")
  # k-hat no longer qualifies the projection (D3): a heavy tail with a measured slope is reliable
  k <- st(ess_best = 140, khat = 0.6, ess_all = 700, ess_all_target = 1000, n_draws = 50000, slope = 0.96)
  expect_false(grepl("unreliable", k$reasons[1]))
  expect_true(k$projection_reliable)
  expect_match(st(ess_best = NA)$reasons[1], "could not be computed")
})

test_that("every reason that quotes a projection carries the reliability qualifier (R5, D7)", {
  base <- list(ess_best = 140, ess_all = 700, ess_all_target = 1000, n_draws = 50000, mce_p90 = 0.12)
  # slope unknown: indicative only, on the ESS and the MCE reasons
  u <- do.call(st, base)
  expect_false(u$projection_reliable)
  expect_length(grep("(indicative only: the ESS growth slope is not yet known)", u$reasons, fixed = TRUE), 2L)
  # stalled (UGA: slope 0.29): unreliable, on both
  g <- do.call(st, c(base, slope = 0.29))
  expect_false(g$projection_reliable)
  expect_length(grep("(unreliable: ESS growth slope 0.29 < 0.8)", g$reasons, fixed = TRUE), 2L)
  # a measured, unstalled slope: no qualifier anywhere
  ok <- do.call(st, c(base, slope = 0.8))
  expect_true(ok$projection_reliable)
  expect_false(any(grepl("unreliable|indicative", ok$reasons)))
  # the helper is the same rule
  expect_false(MOSAIC:::.mosaic_projection_reliable(NA))
  expect_false(MOSAIC:::.mosaic_projection_reliable(0.79))
  expect_true(MOSAIC:::.mosaic_projection_reliable(0.8))
  expect_true(MOSAIC:::.mosaic_projection_reliable(0.9, slope_stall = 0.9))
})

test_that("simulations projected for green: the larger projection, at the run's success rate (D7)", {
  # 50,000 valid of 100,000 attempted (rate 0.5). ESS: 50,000 x 1000 / 700 = 71,428.6 valid draws;
  # MCE: 50,000 x (0.12 / 0.10)^2 = 72,000 valid draws. Green needs max = 72,000 valid
  # = 144,000 simulations. ESS shortfall 21,428.6 / 0.5 = 42,857 simulations -> "43k";
  # MCE shortfall 22,000 / 0.5 = 44,000 -> "44k".
  # Plus one confirming batch: a quarter of 144,000 = 36,000, capped at batch_size_max 10,000.
  s <- st(ess_best = 140, ess_all = 700, ess_all_target = 1000, n_draws = 50000, n_attempted = 1e5,
          mce_p90 = 0.12, slope = 1)
  expect_equal(s$sims_projected_for_green, 144000 + 10000, tolerance = 1e-12)
  # the confirming batch follows the batch bounds: uncapped (a quarter), and the floor
  expect_equal(st(ess_best = 140, ess_all = 700, ess_all_target = 1000, n_draws = 50000, n_attempted = 1e5,
                  mce_p90 = 0.12, batch_size_max = 1e6)$sims_projected_for_green, 144000 * 1.25, tolerance = 1e-12)
  expect_equal(st(ess_best = 140, ess_all = 70, ess_all_target = 100, n_draws = 1000,
                  mce_p90 = 0.05)$sims_projected_for_green, 1000 / 0.7 + 500, tolerance = 1e-12)
  expect_equal(s$success_rate, 0.5)
  expect_match(s$reasons[1], "about 43k more simulations would reach the target", fixed = TRUE)
  expect_match(s$reasons[2], "about 44k more simulations would bring it to the target", fixed = TRUE)
  # n_draws_projected stays in valid draws (sims_projected_for_target)
  expect_equal(s$n_draws_projected, 50000 * 1000 / 700)
  # ESS projection larger: 50,000 x 1000 / 300 = 166,667 valid draws (MCE 72,000)
  e <- st(ess_best = 60, ess_all = 300, ess_all_target = 1000, n_draws = 50000, mce_p90 = 0.12)
  expect_equal(e$sims_projected_for_green, 50000 * 1000 / 300 + 10000, tolerance = 1e-12)
  # green: NA
  expect_true(is.na(st(n_draws = 50000, ess_all = 2000, ess_all_target = 1000)$sims_projected_for_green))
  # not green, but no check that more draws fix: NA (flat only; k-hat only; ESS met but MCE green)
  fl <- st(ess_untempered_frac = 0.05, n_draws = 50000, ess_all = 900, ess_all_target = 1000)
  expect_identical(fl$status, "yellow")
  expect_true(fl$sampling_green)
  expect_true(is.na(fl$sims_projected_for_green))      # sampling green: null even with ESS_all below its target
  expect_true(is.na(st(khat = 0.9, n_draws = 50000)$sims_projected_for_green))
  # an infinite MCE gives no projection
  expect_true(is.na(st(mce_p90 = Inf, n_draws = 50000)$sims_projected_for_green))
  # attempted unknown or inconsistent: rate 1
  expect_identical(st(n_draws = 100, n_attempted = 50)$success_rate, 1)
})

test_that("Monte Carlo error reasons name the value as a standard error, the target and the remedy", {
  m <- st(mce_p90 = 0.2, n_draws = 50000, slope = 1)  # 50k * ((0.2 / 0.1)^2 - 1) = 150k more
  expect_identical(m$reasons, paste0("Monte Carlo standard error of the posterior means is 20.0% of the posterior SD ",
                                     "(90th percentile over parameters; target 10.0%): about 150k more ",
                                     "simulations would bring it to the target"))
  expect_match(st(mce_p90 = 0.12)$reasons, "12.0% of the posterior SD.*run more simulations$")
  expect_match(st(mce_p90 = Inf)$reasons, "fewer than two seed batches hold ensemble members")
  expect_match(st(mce_p90 = NA)$reasons, "not assessed")
  # a near-degenerate value cannot print an absurd percentage (T5)
  big <- st(mce_p90 = 7.2e27)$reasons
  expect_match(big, "is more than 1,000% of the posterior SD", fixed = TRUE)
  expect_false(grepl("e\\+", big))
})

test_that("flags never change the colour", {
  s <- st(budget_exhausted = TRUE, n_draws = 150000, slope = 0.3, size_bound = "max_members",
          fixed_mode = TRUE)
  expect_identical(s$status, "green")
  expect_identical(s$flags, c("budget_cap_reached", "ess_growth_stalled", "ensemble_size_bound",
                              "fixed_budget"))
  expect_length(s$flag_text, 4L)
  expect_identical(flag_txt(s, "budget_cap_reached"),
                   "budget cap reached: 150,000 simulations (max_simulations_total)")
  expect_match(flag_txt(s, "ensemble_size_bound"), "max_members bound")
  expect_identical(flag_txt(s, "fixed_budget"), "fixed-budget run: the status is assessed, not used to stop")
  expect_match(flag_txt(st(size_bound = "min_members"), "ensemble_size_bound"), "min_members bound")
  expect_length(st(size_bound = "ratio")$flags, 0L)
  expect_length(st(slope = 0.8)$flags, 0L)                      # 0.8 is not stalled
  expect_length(st(slope = NA, budget_exhausted = NA, fixed_mode = NA)$flags, 0L)
  expect_identical(flag_txt(st(budget_exhausted = TRUE), "budget_cap_reached"),
                   "budget cap reached (max_simulations_total)")
})

test_that("thresholds are validated and unknown names rejected", {
  expect_error(st(thresholds = list(mce_gren = 0.1)), "unknown convergence threshold")
  expect_error(st(thresholds = list(mce_green = 0.2)), "mce_green <= mce_yellow")
  expect_error(st(thresholds = list(khat_green = 0.8)), "khat_green <= khat_yellow")
  expect_error(st(thresholds = list(ess_yellow_frac = 1.5)), "ess_yellow_frac")
  expect_error(st(thresholds = list(flat_ess_frac = 2)), "flat_ess_frac must be in")
  expect_error(st(thresholds = list(mce_green = -1)), "positive")
  expect_error(st(thresholds = list(0.1)), "named list")
  expect_identical(st(thresholds = list(slope_stall = 0.9))$thresholds$slope_stall, 0.9)
})

test_that("inputs must be scalars", {
  expect_error(st(ess_best = c(100, 200)), "ess_best must be a single number")
  expect_error(st(mce_p90 = "0.1"), "mce_p90 must be a single number")
  expect_error(st(flat = c(TRUE, FALSE)), "flat must be a single logical")
  expect_error(st(ess_best_target = 0), "ess_best_target must be positive")
  expect_error(st(ess_best_target = NA), "ess_best_target must be a positive finite number")
  expect_error(st(size_bound = c("a", "b")), "size_bound")
})

test_that("calc_weight_health() formals carry the single-source defaults", {
  d <- MOSAIC:::.mosaic_convergence_defaults()
  f <- formals(calc_weight_health)
  expect_identical(eval(f$best_subset_ratio), d$best_subset_ratio)
  expect_identical(eval(f$ess_best_target), d$ESS_best)
  expect_identical(eval(f$min_members), d$min_ensemble_members)
  expect_identical(eval(f$max_members), d$max_ensemble_members)
  expect_identical(eval(formals(calc_convergence_status)$ess_best_target), d$ESS_best)
  expect_identical(d$thresholds, list(ess_yellow_frac = 0.5, mce_green = 0.10, mce_yellow = 0.15,
                                      khat_green = 0.5, khat_yellow = 0.7, slope_stall = 0.8,
                                      flat_ess_frac = 0.01))
  # the control defaults carry the same bands
  expect_identical(as.list(mosaic_control_defaults()$targets$status_thresholds), d$thresholds)
})

test_that("the draw formatter rounds to two significant digits and never up to the next thousand (C7, R6)", {
  f <- MOSAIC:::.mosaic_fmt_draws
  expect_identical(f(1022), "1,000")                 # was "2k"
  expect_identical(f(1450.2), "1,500")
  expect_identical(f(797), "800")
  expect_identical(f(850.2), "850")
  expect_identical(f(42.4), "42")
  expect_identical(f(20001), "20k")
  expect_identical(f(21429), "21k")
  expect_identical(f(116667), "117k")
  expect_identical(f(456693), "457k")
  expect_identical(f(1234567), "1.2M")
  expect_identical(f(2e6), "2.0M")
  expect_identical(f(9999), "10k")                   # rounds up into the next unit
  expect_identical(f(999999), "1.0M")                # was "1000k"
  expect_identical(f(NA), "NA")
  for (x in c(1022, 1999, 9999, 10001, 99999, 999999)) {
    shown <- as.numeric(gsub("[,kM]", "", f(x))) * c(1, 1e3, 1e6)[1 + grepl("k", f(x)) + 2 * grepl("M", f(x))]
    expect_lt(abs(shown / x - 1), 0.05)
  }
})

test_that("the stopping rule needs sampling GREEN at two consecutive checks and the minimum run length (D5)", {
  h <- function(...) lapply(list(...), function(x) list(n = x[[1]], sampling_green = x[[2]]))
  rule <- MOSAIC:::.mosaic_stop_rule
  expect_false(rule(h(list(5000, TRUE)), TRUE))                        # one check only
  expect_false(rule(h(list(5000, FALSE), list(6000, TRUE)), TRUE))     # first green
  expect_true(rule(h(list(5000, TRUE), list(6000, TRUE)), TRUE))
  expect_false(rule(h(list(5000, TRUE), list(6000, TRUE)), FALSE))     # minimum run length not met
  expect_false(rule(h(list(5000, TRUE), list(6000, FALSE), list(7000, TRUE)), TRUE))
  expect_true(rule(h(list(4000, FALSE), list(5000, TRUE), list(6000, FALSE),
                     list(7000, TRUE), list(8750, TRUE)), TRUE))
  # a resume re-deriving the same draw count counts once (the latest entry)
  expect_false(rule(h(list(5000, FALSE), list(6000, TRUE), list(6000, TRUE)), TRUE))
  expect_true(rule(h(list(5000, TRUE), list(6000, FALSE), list(6000, TRUE), list(7000, TRUE)), TRUE))
  expect_false(rule(list(), TRUE))
  # entries read back from run_state.json work
  js <- jsonlite::fromJSON(jsonlite::toJSON(h(list(5000, TRUE), list(6000, TRUE)), auto_unbox = TRUE),
                           simplifyVector = FALSE)
  expect_true(rule(js, TRUE))
  # an older entry without sampling_green falls back to its status
  old <- list(list(n = 5000, status = "green"), list(n = 6000, status = "green"))
  expect_true(rule(old, TRUE))
  expect_false(rule(list(list(n = 5000, status = "yellow"), list(n = 6000, status = "green")), TRUE))
})

test_that("a flat likelihood does not block the stop but keeps the status yellow (BFA)", {
  flat <- st(ess_untempered_frac = 0.053, n_draws = 7000, n_tied_max = 113)
  expect_identical(flat$status, "yellow")
  expect_true(flat$sampling_green)
  wh <- list(convergence = flat)
  expect_true(MOSAIC:::.mosaic_sampling_green(wh))
  e <- function(n) list(n = n, status = flat$status, sampling_green = MOSAIC:::.mosaic_sampling_green(wh))
  expect_true(MOSAIC:::.mosaic_stop_rule(list(e(6500), e(7000)), TRUE))
  # any sampling check behind blocks it
  for (s2 in list(st(ess_best = 150), st(mce_p90 = 0.12), st(khat = 0.6), st(khat = NA))) {
    expect_false(s2$sampling_green)
    expect_false(MOSAIC:::.mosaic_sampling_green(list(convergence = s2)))
  }
  expect_true(st()$sampling_green)
  # derived from the checks when the field is absent (a block read back from an older JSON)
  ck <- jsonlite::fromJSON(jsonlite::toJSON(list(checks = flat$checks)), simplifyVector = FALSE)
  expect_true(MOSAIC:::.mosaic_sampling_green(list(convergence = ck)))
  expect_false(MOSAIC:::.mosaic_sampling_green(list()))
})

# ---- glossary ------------------------------------------------------------------

test_that("the metric glossary parses and every entry has the required fields", {
  g <- MOSAIC:::.mosaic_glossary()
  expect_s3_class(g, "data.frame")
  expect_identical(names(g), MOSAIC:::.mosaic_glossary_fields())
  expect_false(anyDuplicated(g$key) > 0)
  expect_true(all(vapply(g, function(x) all(nzchar(trimws(x))), logical(1))))
  expect_identical(attr(g, "version"), "1.4.0")
  # every SPEC section 2/5 metric and every 1.4.0 summary field shown is defined
  need <- c("draws", "coarsening_alpha", "ess_all", "best_fit_ensemble", "ess_best",
            "best_subset_mass", "mce", "khat_tempered", "eta_star", "flat_likelihood",
            "ess_untempered_frac", "ess_growth_slope", "convergence_status", "convergence_flags",
            "ensemble_members", "sims_projected_for_target", "sims_projected_for_green",
            "projection_reliable", "alpha_per_location", "posterior_members", "kl_divergence",
            "r2", "bias", "coverage", "wis")
  expect_true(all(need %in% g$key), info = paste(setdiff(need, g$key), collapse = ", "))
  # every threshold is either "none" or marked as a convention / literature band
  expect_true(all(grepl("none|convention|Vehtari", g$threshold)))
  fl <- g$formula[g$key == "convergence_flags"]
  for (id in c("budget_cap_reached", "ess_growth_stalled", "ensemble_size_bound", "fixed_budget"))
    expect_match(fl, id, fixed = TRUE)
  expect_true("ensemble_binding" %in% g$key)
  bad <- withr::local_tempfile(fileext = ".yml")
  writeLines(c("version: '1'", "metrics:", "  - key: x", "    label: X"), bad)
  expect_error(MOSAIC:::.mosaic_glossary(bad), "lacks: definition")
})
