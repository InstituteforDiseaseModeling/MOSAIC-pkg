# MOSAIC 1.4.0 statistical core: best-fit ensemble selection, member Monte
# Carlo error, tempered k-hat, ESS growth slope and the draw projection.
# Hand-computed fixtures (closed forms in the comments).

sel <- function(...) MOSAIC:::.mosaic_bestfit_select(...)

# ---- selection: hand-computed fixture ------------------------------------------

test_that("best-fit selection matches a hand-computed fixture", {
  # ll = 0, -1, ..., -4 at eta = 1: w = e^0 .. e^-4.
  # ESS_all = (sum w)^2 / sum w^2 = 2.13498737852815. ratio 0.5 -> target 1.0675;
  # subset ESS after 1 draw = 1, after 2 = (1 + e^-1)^2 / (1 + e^-2) = 1.64805 -> 2 members.
  ll <- c(-2, 0, -4, -1, -3)                 # shuffled: positions 2, 4 are the best two
  s <- sel(ll, eta = 1, ratio = 0.5, min_members = 1, max_members = 5)
  w <- exp(-(0:4))
  expect_identical(s$idx, c(2L, 4L))
  expect_equal(s$ess_all, sum(w)^2 / sum(w^2), tolerance = 1e-14)
  expect_equal(s$ess_all, 2.13498737852815, tolerance = 1e-13)
  expect_equal(s$ess_best, (1 + exp(-1))^2 / (1 + exp(-2)), tolerance = 1e-14)
  expect_equal(s$ess_target, 0.5 * s$ess_all, tolerance = 1e-14)
  expect_equal(s$weights, c(1, exp(-1)) / (1 + exp(-1)), tolerance = 1e-14)
  expect_equal(s$weights[1], stats::plogis(1), tolerance = 1e-14)
  expect_equal(s$mass, (1 + exp(-1)) / sum(w), tolerance = 1e-14)   # 0.870530303811567
  expect_identical(s$n_members, 2L)
  expect_identical(s$binding, "ratio")
  expect_false(s$capped)
  expect_true(is.na(s$capped_reason))
  expect_identical(s$ll_cut, -1)
  # ratio 0.9 -> target 1.9215: 2 draws give 1.648, 3 give 1.9587 -> 3 members
  expect_identical(sel(ll, 1, 0.9, 1, 5)$n_members, 3L)
  # ratio 1 needs every draw (the full set reaches ESS_all exactly)
  s1 <- sel(ll, 1, 1, 1, 5)
  expect_identical(s1$n_members, 5L)
  expect_equal(s1$mass, 1)
  expect_equal(s1$ess_best, s1$ess_all, tolerance = 1e-14)
})

test_that("the subset ESS reaches the ratio target and the selection is the first crossing", {
  set.seed(11)
  ll <- -abs(rnorm(5000, sd = 300))
  s <- sel(ll, eta = 0.01, ratio = 0.2, min_members = 1, max_members = 5000)
  w <- exp(0.01 * (sort(ll, decreasing = TRUE) - max(ll)))
  ess_sub <- cumsum(w)^2 / cumsum(w^2)
  expect_gte(s$ess_best, s$ess_target)
  expect_lt(ess_sub[s$n_members - 1L], s$ess_target)
  expect_true(all(diff(ess_sub) >= -1e-9))           # adding a smaller weight never lowers the ESS
  expect_identical(s$idx, order(-ll)[seq_len(s$n_members)])
})

test_that("ties are broken by ascending sim id, and an all-equal likelihood takes the lowest sims", {
  ll <- c(-1, 0, 0, -1, 0)
  s <- sel(ll, 1, 1, 1, 5, sim = c(50, 40, 30, 20, 10))
  expect_identical(s$idx, c(5L, 3L, 2L, 4L, 1L))      # 0-ties by sim 10, 30, 40; -1-ties by 20, 50
  # all log-likelihoods equal: weights equal, subset ESS = n, ceiling(ratio * n) members
  eq <- sel(rep(-7, 200), eta = 0.3, ratio = 0.2, min_members = 1, max_members = 200, sim = 200:1)
  expect_equal(eq$ess_all, 200)
  expect_identical(eq$n_members, 40L)
  expect_equal(eq$ess_best, 40)
  expect_equal(eq$weights, rep(1 / 40, 40))
  expect_equal(eq$mass, 0.2)
  expect_identical(sort(eq$idx), 161:200)             # sims 1..40
})

# ---- selection: edge cases -----------------------------------------------------

test_that("the min_members bound raises a small selection and is reported", {
  ll <- c(0, -50, -60, -70, -80, -90)                 # one dominant draw: ratio met at n = 1
  s <- sel(ll, 1, 0.5, min_members = 3, max_members = 6)
  expect_identical(s$n_members, 3L)
  expect_identical(s$binding, "min_members")
  expect_true(s$capped)
  expect_match(s$capped_reason, "raised to min_members = 3")
  expect_gte(s$ess_best, s$ess_target)
})

test_that("fewer valid draws than min_members makes every valid draw a member", {
  ll <- c(0, -1, NA, -Inf, -2, NaN, Inf)
  s <- sel(ll, 1, 0.2, min_members = 30, max_members = 1000)
  expect_identical(s$n_valid, 3L)
  expect_identical(s$n_members, 3L)
  expect_identical(sort(s$idx), c(1L, 2L, 5L))
  expect_identical(s$binding, "min_members")
  expect_true(s$capped)
  expect_match(s$capped_reason, "only 3 valid draws, fewer than min_members = 30")
  expect_equal(sum(s$weights), 1)
})

test_that("a target unreachable within max_members stops at max_members with ess_best < target", {
  ll <- rep(0, 1000)                                  # ESS_all 1000, ratio 0.5 -> 500 members needed
  s <- sel(ll, 1, 0.5, min_members = 30, max_members = 100)
  expect_identical(s$n_members, 100L)
  expect_identical(s$binding, "max_members")
  expect_true(s$capped)
  expect_equal(s$ess_best, 100)
  expect_lt(s$ess_best, s$ess_target)
  expect_match(s$capped_reason, "not reached within max_members = 100")
  expect_match(s$capped_reason, "ensemble ESS 100.0")
})

test_that("NA, NaN and infinite log-likelihoods are never members", {
  ll <- c(NA, -1, Inf, 0, -Inf, NaN, -2)
  s <- sel(ll, 1, 1, 1, 10)
  expect_identical(s$idx, c(4L, 2L, 7L))
  expect_identical(s$n_valid, 3L)
})

test_that("invalid ratio, eta, bounds and inputs are errors", {
  ll <- c(0, -1, -2)
  for (r in list(0, -0.1, 1.0001, NA_real_, Inf, c(0.1, 0.2), "0.2"))
    expect_error(sel(ll, 1, r, 1, 3), "ratio must be a single number in \\(0, 1\\]")
  for (e in list(0, -1, NA_real_, Inf))
    expect_error(sel(ll, e, 0.2, 1, 3), "eta must be")
  expect_error(sel(ll, 1, 0.2, 5, 3), "exceeds max_members")
  expect_error(sel(ll, 1, 0.2, 0, 3), "positive whole numbers")
  expect_error(sel(ll, 1, 0.2, 1.5, 3), "positive whole numbers")
  expect_error(sel(c(NA, -Inf), 1, 0.2, 1, 3), "no draw has a finite log-likelihood")
  expect_error(sel(ll, 1, 0.2, 1, 3, sim = 1:2), "one entry per draw")
  expect_identical(sel(ll, 1, 1, 1, 3)$n_members, 3L)          # ratio = 1 is allowed
})

# ---- Monte Carlo error of the members' posterior means --------------------------

test_that("member MCE matches a hand-computed fixture", {
  # Members sims 1..4 (equal weights), x = 0, 1, 2, 3; 2 batches by (sim - 1) mod 2:
  # {1, 3} -> mean 1, {2, 4} -> mean 2. SD of batch means sqrt(0.5); members'
  # posterior SD sqrt(1.25); ratio sqrt(0.4); MCE = sqrt(0.4) / sqrt(2) = sqrt(0.2).
  P <- matrix(c(0, 1, 2, 3, 99), ncol = 1, dimnames = list(NULL, "x"))   # row 5 is not a member
  m <- MOSAIC:::.mosaic_member_mce(P, member_idx = 1:4, member_w = rep(1, 4), sim = 1:5, n_batches = 2)
  expect_equal(unname(m$per_parameter["x"]), sqrt(0.2), tolerance = 1e-14)
  expect_equal(m$median, sqrt(0.2), tolerance = 1e-14)
  expect_equal(m$p90, sqrt(0.2), tolerance = 1e-14)
  expect_equal(m$max, sqrt(0.2), tolerance = 1e-14)
  expect_identical(m$n_batches_used, 2L)
  # Weighted: w = (3, 1, 1, 1). Batch 1 {x0 w3, x2 w1} mean 0.5; batch 2 {x1, x3} mean 2.
  # Posterior mean 1, var (3*1 + 0 + 1 + 4) / 6 = 4/3. Ratio sd(c(.5, 2)) / sqrt(4/3).
  mw <- MOSAIC:::.mosaic_member_mce(P, 1:4, c(3, 1, 1, 1), 1:5, n_batches = 2)
  expect_equal(unname(mw$per_parameter), stats::sd(c(0.5, 2)) / sqrt(4 / 3) / sqrt(2), tolerance = 1e-14)
})

test_that("member MCE summarises over parameters and skips constant ones", {
  set.seed(5)
  n <- 4000
  P <- cbind(a = rnorm(n), b = runif(n), k = 1, c = rexp(n))
  m <- MOSAIC:::.mosaic_member_mce(P, 1:n, rep(1, n), sim = seq_len(n), n_batches = 10)
  expect_true(is.na(m$per_parameter[["k"]]))
  expect_identical(m$n_parameters, 3L)
  v <- m$per_parameter[c("a", "b", "c")]
  expect_equal(m$median, stats::median(v))
  expect_equal(m$p90, unname(stats::quantile(v, 0.9)))
  expect_equal(m$max, max(v))
  # IID equal weights: MCE ~ 1/sqrt(n) = 0.0158; the ratio is noisy, so a loose bound
  expect_lt(m$max, 0.04)
  # data frame input keeps numeric columns
  m2 <- MOSAIC:::.mosaic_member_mce(as.data.frame(P), 1:n, rep(1, n), seq_len(n), 10)
  expect_equal(m2$per_parameter, m$per_parameter)
})

test_that("batches with no members are left out; fewer than two batches is Inf", {
  P <- matrix(c(0, 1, 2, 3, 4, 5), ncol = 1, dimnames = list(NULL, "x"))
  # 3 batches, members sims 1, 2, 4, 5 -> batches 1, 2, 1, 2 (batch 3 empty):
  # batch means (0 + 3)/2 = 1.5 and (1 + 4)/2 = 2.5; divided by sqrt(2), not sqrt(3)
  m <- MOSAIC:::.mosaic_member_mce(P, c(1, 2, 4, 5), rep(1, 4), sim = 1:6, n_batches = 3)
  expect_identical(m$n_batches_used, 2L)
  ps <- sqrt(mean((c(0, 1, 3, 4) - 2)^2))
  expect_equal(unname(m$per_parameter), stats::sd(c(1.5, 2.5)) / ps / sqrt(2), tolerance = 1e-14)
  # all members in one batch -> Inf, and the summaries are Inf (not NaN)
  one <- MOSAIC:::.mosaic_member_mce(P, c(1, 4), c(1, 1), sim = 1:6, n_batches = 3)
  expect_identical(unname(one$per_parameter), Inf)
  expect_identical(one$p90, Inf)
  expect_identical(one$max, Inf)
  expect_identical(one$n_batches_used, 1L)
  expect_error(MOSAIC:::.mosaic_member_mce(P, 7, 1, 1:6), "index rows")
  expect_error(MOSAIC:::.mosaic_member_mce(P, 1:2, c(1, -1), 1:6), "non-negative")
  expect_error(MOSAIC:::.mosaic_member_mce(P, 1:2, c(1, 1), 1:6, n_batches = 1), ">= 2")
})

test_that("a batch holding under 1% of the members' weight is left out of B' and the SD (T5, C13)", {
  # 3 batches; members sims 1, 2, 4, 5 (weight 1) and sim 3 at weight 1e-9 (alone in batch 3).
  # Batch 1 {x 1, 3} mean 2, batch 2 {x 2, 4} mean 3; batch 3 (x = 10) holds 2.5e-10 of the
  # weight and is dropped. Posterior SD sqrt(1.25) (the negligible member moves it < 1e-7),
  # ratio sqrt(0.5) / sqrt(1.25), MCE sqrt(0.4) / sqrt(2) = sqrt(0.2). Without the floor the
  # batch means (2, 3, 10) give 4.36 / 1.118 / sqrt(3) = 2.25.
  P <- matrix(c(1, 2, 10, 3, 4, 0), ncol = 1, dimnames = list(NULL, "x"))
  m <- MOSAIC:::.mosaic_member_mce(P, c(1, 2, 3, 4, 5), c(1, 1, 1e-9, 1, 1), sim = 1:6, n_batches = 3)
  expect_identical(m$n_batches_used, 2L)
  expect_rel_equal(unname(m$per_parameter), sqrt(0.2), rel = 1e-6)
  # the floor is a share of the members' weight: 1% counts, just below does not
  w_at <- function(share) { r <- share / (1 - share); c(1, 1, 4 * r, 1, 1) }
  expect_identical(MOSAIC:::.mosaic_member_mce(P, 1:5, w_at(0.01), 1:6, 3)$n_batches_used, 3L)
  expect_identical(MOSAIC:::.mosaic_member_mce(P, 1:5, w_at(0.0099), 1:6, 3)$n_batches_used, 2L)
  # only one batch above the floor: Inf
  one <- MOSAIC:::.mosaic_member_mce(P, c(1, 2, 4), c(1, 1e-9, 1), sim = 1:6, n_batches = 3)
  expect_identical(one$p90, Inf)
  expect_identical(one$n_batches_used, 1L)
})

test_that("the Inf-safe quantile agrees with type 7 on finite values", {
  set.seed(1)
  x <- rnorm(37)
  for (p in c(0, 0.1, 0.5, 0.9, 1))
    expect_equal(MOSAIC:::.mosaic_quantile_inf(x, p), unname(stats::quantile(x, p)))
  expect_identical(MOSAIC:::.mosaic_quantile_inf(c(1, Inf, Inf), 0.9), Inf)
  expect_identical(MOSAIC:::.mosaic_quantile_inf(c(0.1, 0.2, Inf), 0.5), 0.2)
  expect_true(is.na(MOSAIC:::.mosaic_quantile_inf(numeric(0), 0.9)))
})

# ---- tempered k-hat --------------------------------------------------------------

test_that("the tempered k-hat is calc_is_diagnostics() on eta * log-likelihood", {
  set.seed(8)
  ll <- -abs(rnorm(5000, sd = 400))
  for (eta in c(0.002, 0.01, 0.05)) {
    k <- MOSAIC:::.mosaic_khat_tempered(ll, eta)
    expect_identical(as.numeric(k), calc_is_diagnostics(eta * ll)$khat)
  }
  # non-finite draws are dropped; flat weights give NA with a status
  expect_identical(as.numeric(MOSAIC:::.mosaic_khat_tempered(c(ll, -Inf, NA), 0.01)),
                   as.numeric(MOSAIC:::.mosaic_khat_tempered(ll, 0.01)))
  flat <- MOSAIC:::.mosaic_khat_tempered(rep(-3, 1000), 0.01)
  expect_true(is.na(flat))
  expect_match(attr(flat, "status"), "tied")
  expect_true(is.na(MOSAIC:::.mosaic_khat_tempered(ll, NA_real_)))
  # sharper tempering, heavier tail
  expect_gt(MOSAIC:::.mosaic_khat_tempered(ll, 0.05), MOSAIC:::.mosaic_khat_tempered(ll, 0.002))
})

# ---- ESS growth slope and projection ---------------------------------------------

test_that("the ESS growth slope is the log-log OLS slope of the last points", {
  n <- c(5000, 10000, 20000, 40000)
  expect_equal(MOSAIC:::.mosaic_ess_growth_slope(n, 0.02 * n), 1, tolerance = 1e-12)
  expect_equal(MOSAIC:::.mosaic_ess_growth_slope(n, 3 * sqrt(n)), 0.5, tolerance = 1e-12)
  # only the last 3 points count by default: an early kink is ignored
  ess <- c(500, 200, 400, 800)
  expect_equal(MOSAIC:::.mosaic_ess_growth_slope(n, ess), 1, tolerance = 1e-12)
  x <- log(n); y <- log(ess)
  expect_equal(MOSAIC:::.mosaic_ess_growth_slope(n, ess, n_last = 4),
               unname(stats::coef(stats::lm(y ~ x))[2]), tolerance = 1e-12)
  # unordered input is ordered by n
  expect_equal(MOSAIC:::.mosaic_ess_growth_slope(rev(n), rev(0.02 * n)), 1, tolerance = 1e-12)
  # fewer than 3 usable points -> NA
  expect_true(is.na(MOSAIC:::.mosaic_ess_growth_slope(c(1000, 2000), c(10, 20))))
  expect_true(is.na(MOSAIC:::.mosaic_ess_growth_slope(c(1000, 2000, 3000), c(10, NA, 30))))
  expect_true(is.na(MOSAIC:::.mosaic_ess_growth_slope(c(1000, 1000, 1000), c(10, 11, 12))))
  expect_true(is.na(MOSAIC:::.mosaic_ess_growth_slope(numeric(0), numeric(0))))
  expect_error(MOSAIC:::.mosaic_ess_growth_slope(1:3, 1:2), "same length")
})

test_that("the draw projection is N x target / ESS_all", {
  expect_equal(MOSAIC:::.mosaic_projected_draws(50000, 109, 1000), 50000 * 1000 / 109)
  expect_equal(MOSAIC:::.mosaic_projected_draws(50000, 2500, 1000), 20000)
  expect_true(is.na(MOSAIC:::.mosaic_projected_draws(NA, 109, 1000)))
  expect_true(is.na(MOSAIC:::.mosaic_projected_draws(50000, 0, 1000)))
  expect_true(is.na(MOSAIC:::.mosaic_projected_draws(50000, 100, NA)))
})
