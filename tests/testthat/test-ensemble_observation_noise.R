# =============================================================================
# test-ensemble_observation_noise.R
#
# The observation-level posterior predictive of calc_model_ensemble() (v0.101.0):
#   * cases: weekly totals ~ NB(member weekly total, k), apportioned to days by
#     systematic sampling; deaths: weekly variance phi * E around the expected
#     reported deaths (coupled to the engine deaths);
#   * the weekly blocks are the surveillance's ISO Monday-Sunday weeks
#     (.nb_disp_block() with the location's reporting-week offset);
#   * central lines stay engine-level, intervals are observation-level;
#   * every consumer of member trajectories reads the engine arrays.
# =============================================================================

# A one-location config on a daily grid.
.obs_cfg <- function(n_time, date_start = "2023-01-02") {
  list(location_name = "AAA",
       reported_cases = rep(10, n_time), reported_deaths = rep(1, n_time),
       date_start = date_start,
       date_stop = as.character(as.Date(date_start) + n_time - 1L))
}

# Canned engine records for n_param x n_stoch members.
.obs_records <- function(n_param, n_stoch, cases_fun, deaths_fun = NULL, expected = NULL) {
  recs <- vector("list", n_param * n_stoch); i <- 0L
  for (p in seq_len(n_param)) for (s in seq_len(n_stoch)) {
    cs <- cases_fun(p, s)
    i <- i + 1L
    recs[[i]] <- list(param_idx = p, stoch_idx = s, success = TRUE,
                      reported_cases  = matrix(cs, nrow = 1L),
                      reported_deaths = matrix(if (is.null(deaths_fun)) rep(0, length(cs))
                                               else deaths_fun(p, s), nrow = 1L),
                      expected_deaths = if (is.null(expected)) NULL else matrix(expected, nrow = 1L))
  }
  recs
}

# Monday of each date's ISO week (locale-free).
.iso_monday <- function(d) d - ((as.POSIXlt(d)$wday + 6L) %% 7L)

# Minimal run-level deaths integration for mocked members (no CFR draws).
.obs_di <- function(n_time, phi, start = "2023-01-02") {
  list(setup = list(nL = 1L), n_time = n_time, dispersion = phi, years = 2023L,
       base_logit_full = matrix(stats::qlogis(0.01), 1L, n_time),
       year_full = as.integer(format(as.Date(start) + seq_len(n_time) - 1L, "%Y")))
}

# ---- blocks: ISO weeks, never weeks counted from date_start ------------------

test_that("weekly blocks are ISO Monday-Sunday weeks, not weeks counted from date_start", {
  # 2023-01-01 is a Sunday: it closes the ISO week of 2022-12-26, so it is a
  # block of its own and the next block starts on Monday 2023-01-02.
  d0 <- as.Date("2023-01-01"); n <- 30L
  dates <- d0 + seq_len(n) - 1L
  blk <- MOSAIC:::.mosaic_observation_blocks(n, d0, 0L)[[1]]
  mon <- .iso_monday(dates)
  expect_identical(blk$block, as.integer(factor(as.character(mon), levels = unique(as.character(mon)))))
  expect_identical(blk$start[1:3], c(1L, 2L, 9L))
  expect_identical(blk$end[1:2],   c(1L, 8L))
  expect_true(all(as.POSIXlt(dates[blk$start[-1]])$wday == 1L))      # Mondays
  # The same blocks est_nb_dispersion() estimates k on.
  raw <- MOSAIC:::.nb_disp_block(dates, 0L)
  expect_identical(blk$block, as.integer(factor(raw, levels = unique(raw))))
  # A reporting-week offset moves the boundary (offset 2 = weeks start Wednesday).
  blk2 <- MOSAIC:::.mosaic_observation_blocks(n, d0, 2L)[[1]]
  expect_true(all(as.POSIXlt(dates[blk2$start[-1]])$wday == 3L))
  # One element per location, sharing a block set per distinct offset.
  bl <- MOSAIC:::.mosaic_observation_blocks(n, d0, c(0L, 2L, 0L))
  expect_length(bl, 3L)
  expect_identical(bl[[1]], bl[[3]]); expect_identical(bl[[2]], blk2)
})

test_that("the predictive's blocks are the likelihood's reporting weeks, edge weeks kept", {
  # Single source: .mosaic_week_blocks(). The likelihood drops a week cut by
  # the window edge (partial = "drop"); the predictive keeps it, because every
  # day -- the never-scored burn-in included -- gets an observation-level draw.
  d0 <- as.Date("2023-01-01"); n <- 45L
  dates <- d0 + seq_len(n) - 1L
  for (off in 0:6) {
    wb  <- MOSAIC:::.mosaic_week_blocks(dates, off, partial = "keep")
    blk <- MOSAIC:::.mosaic_observation_blocks(n, d0, off)[[1]]
    expect_identical(blk, list(block = wb$index, start = wb$start, end = wb$end))
    expect_identical(sum(!is.na(MOSAIC:::.mosaic_week_blocks(dates, off, partial = "drop")$index)),
                     7L * sum(wb$complete))
  }
  # Undated: weeks counted from column 1 (as if it were a Monday), shifted by
  # the offset -- the rule the predictive used before it shared the helper.
  for (off in c(0L, 3L)) {
    raw <- floor((seq_len(n) - 1L - off) / 7)
    blk <- MOSAIC:::.mosaic_observation_blocks(n, NULL, off)[[1]]
    expect_identical(blk$block, cumsum(c(TRUE, diff(raw) != 0)))
    expect_identical(MOSAIC:::.mosaic_observation_blocks(n, "not a date", off)[[1]], blk)
  }
})

# ---- cases: analytic NB moments; weekly coherence -----------------------------

test_that("cases: weekly totals reproduce the analytic NB mean and variance on ISO weeks", {
  # Window starts on Sunday 2023-01-01 (config_default v6.x's start): day 1 is its own
  # block, days 2-29 are four ISO weeks with engine total C = 110 each.
  pat   <- c(5, 10, 20, 40, 20, 10, 5)
  daily <- c(7, rep(pat, 4))
  n_stoch <- 4000L
  cfg <- .obs_cfg(29L, "2023-01-01")
  local_mocked_ensemble_sims(.obs_records(1L, n_stoch, function(p, s) daily))
  for (k in c(2, 0.5, Inf)) {
    ens <- calc_model_ensemble(config = cfg, configs = list(cfg),
                               n_simulations_per_config = n_stoch,
                               observation_model = list(k_cases = k), verbose = FALSE)
    expect_true(ens$observation_model$cases)
    expect_identical(ens$observation_model$k_cases, k)
    eng <- ens$cases_engine_array[1, , 1, ]
    expect_true(all(eng == daily))                                    # engine untouched
    obs <- ens$cases_array[1, , 1, ]
    expect_true(all(obs == round(obs)) && all(obs >= 0))              # integer counts
    wk  <- rep(1:4, each = 7L)
    W   <- apply(obs[-1, ], 2, function(x) tapply(x, wk, sum))        # [4, n_stoch] ISO-week totals
    C <- 110
    v <- C + if (is.finite(k)) C^2 / k else 0
    expect_lt(abs(mean(W) - C), 4 * sqrt(v / length(W)))
    expect_equal(stats::var(as.vector(W)), v, tolerance = 0.12)
    # weeks are independent draws (blocks align with the ISO weeks summed here)
    expect_lt(abs(stats::cor(W[1, ], W[2, ])), 0.06)
  }
})

test_that("each member's daily draws sum exactly to its one weekly draw, apportioned by its own shape", {
  x <- c(0, 3, 7, 0, 12, 5, 1,  4, 4, 4, 4, 4, 4, 4,  0, 0, 0, 0, 0, 0, 0,  9, 1, 0, 0, 2, 30, 8)
  blk <- MOSAIC:::.mosaic_observation_blocks(28L, "2023-01-02", 0L)[[1]]
  k <- 0.7
  C <- as.numeric(tapply(x, blk$block, sum))
  pos <- C > 0
  for (seed in 1:25) {
    set.seed(seed); y <- MOSAIC:::.mosaic_obs_cases_row(x, blk, k)
    # Replay the generator: one NB draw per block with a positive total comes first.
    set.seed(seed); Y <- numeric(4L); Y[pos] <- stats::rnbinom(sum(pos), size = k, mu = C[pos])
    expect_identical(as.numeric(tapply(y, blk$block, sum)), Y)
    share <- ifelse(C[blk$block] > 0, Y[blk$block] * x / C[blk$block], 0)
    expect_true(all(abs(y - share) < 1))                              # floor or ceiling of its share
    expect_true(all(y[x == 0] == 0))                                  # no cases on a zero-engine day
    expect_true(all(y == round(y)))
  }
  # The apportionment is unbiased: E[y_t] = x_t (mean-preserving observation noise).
  ys <- vapply(1:20000, function(sd) { set.seed(sd); MOSAIC:::.mosaic_obs_cases_row(x, blk, 3) },
               numeric(28L))
  se <- sqrt(apply(ys, 1, stats::var) / ncol(ys))
  expect_true(all(abs(rowMeans(ys) - x) <= 4 * se + 1e-12))
})

# ---- deaths: quasi-Poisson variance around the expected deaths ----------------

test_that("deaths: weekly totals carry the quasi-Poisson variance phi * E; phi = 1 keeps the engine deaths", {
  E_daily <- rep(c(1, 2, 4, 8, 4, 2, 1), 4)                           # E_w = 22
  n_stoch <- 4000L
  set.seed(11)
  r_draws <- lapply(seq_len(n_stoch), function(s) stats::rpois(28L, E_daily))
  local_mocked_ensemble_sims(.obs_records(1L, n_stoch, function(p, s) rep(50, 28),
                                          function(p, s) r_draws[[s]], expected = E_daily))
  cfg <- .obs_cfg(28L)
  ens <- calc_model_ensemble(config = cfg, configs = list(cfg), n_simulations_per_config = n_stoch,
                             deaths_integration = .obs_di(28L, phi = 3),
                             observation_model = list(k_cases = 5), verbose = FALSE)
  expect_true(ens$observation_model$deaths)
  expect_identical(ens$observation_model$phi_deaths, 3)
  wk <- rep(1:4, each = 7L)
  Wd <- apply(ens$deaths_array[1, , 1, ], 2, function(x) tapply(x, wk, sum))
  We <- apply(ens$deaths_engine_array[1, , 1, ], 2, function(x) tapply(x, wk, sum))
  expect_lt(abs(mean(Wd) - 22), 4 * sqrt(3 * 22 / length(Wd)))
  expect_equal(stats::var(as.vector(Wd)), 3 * 22, tolerance = 0.12)
  expect_equal(stats::var(as.vector(We)), 22, tolerance = 0.12)       # engine: Poisson part only
  expect_true(all(ens$deaths_array == round(ens$deaths_array)))

  ens1 <- calc_model_ensemble(config = cfg, configs = list(cfg), n_simulations_per_config = n_stoch,
                              deaths_integration = .obs_di(28L, phi = 1),
                              observation_model = list(k_cases = 5), verbose = FALSE)
  expect_false(ens1$observation_model$deaths)
  expect_identical(ens1$deaths_array, ens1$deaths_engine_array)
  # Without a deaths integration there is no deaths dispersion: deaths stay engine-level.
  ens2 <- calc_model_ensemble(config = cfg, configs = list(cfg), n_simulations_per_config = 10L,
                              observation_model = list(k_cases = 5), verbose = FALSE)
  expect_false(ens2$observation_model$deaths)
  expect_null(ens2$observation_model$phi_deaths)
})

test_that("the deaths noise draws with the deaths likelihood's phi, from observed weeks under reported_tier", {
  # 52 Monday-Sunday weeks across two years; deaths scatter about 4x Poisson
  # around 6% of the cases, except a reconstructed window (weeks 10-21) spread
  # flat, where the deaths track the cases exactly. Weekly totals are multiples
  # of 7, so the daily values are whole.
  set.seed(8)
  n_wk <- 52L; n <- 7L * n_wk
  C_w <- 7 * round((200 + 150 * sin(seq_len(n_wk) / 4)^2) / 7)
  D_w <- 7 * stats::rnbinom(n_wk, mu = 0.06 * C_w / 7, size = 3)
  rec <- 10:21
  C_w[rec] <- 7 * round(mean(C_w[rec]) / 7); D_w[rec] <- 7 * round(0.06 * C_w[rec] / 7)
  cfg <- .obs_cfg(n, date_start = "2023-07-03")
  cfg$reported_cases  <- matrix(rep(C_w / 7, each = 7), 1L)
  cfg$reported_deaths <- matrix(rep(D_w / 7, each = 7), 1L)
  cfg$mu_jt <- 0.02
  cfg$reported_tier <- matrix(ifelse(rep(seq_len(n_wk), each = 7) %in% rec, 2L, 1L), 1L)
  pri <- list(mu_jt = list(sd_year = 0.7, sd_product = 0.3,
                           location = list(AAA = list(year = 2023:2024, logit_mean = rep(qlogis(0.02), 2),
                                                      logit_se = rep(0.2, 2)))))
  di <- MOSAIC:::.mosaic_resolve_deaths_integration(cfg, list(likelihood = list()), pri, NULL)
  yr <- as.integer(format(as.Date("2023-07-03") + 7L * (seq_len(n_wk) - 1L), "%Y"))
  expect_true(di$tier_used)
  expect_identical(di$dispersion, MOSAIC:::.d7_dispersion(D_w[-rec], C_w[-rec], yr[-rec]))
  expect_gt(di$dispersion, MOSAIC:::.d7_dispersion(D_w, C_w, yr))
  expect_identical(di$dispersion, vapply(di$setup$locs, function(L) L$phi, numeric(1)))

  E_daily <- rep(0.02 * C_w / 7, each = 7)
  local_mocked_ensemble_sims(.obs_records(1L, 4L, function(p, s) rep(C_w / 7, each = 7),
                                          function(p, s) round(E_daily), expected = E_daily))
  ens <- calc_model_ensemble(config = cfg, configs = list(cfg), n_simulations_per_config = 4L,
                             deaths_integration = di,
                             observation_model = list(k_cases = 5), verbose = FALSE)
  expect_true(ens$observation_model$deaths)
  expect_identical(ens$observation_model$phi_deaths, di$dispersion)
})

test_that("the deaths coupling thins or tops up the engine deaths within one weekly factor", {
  blk <- MOSAIC:::.mosaic_observation_blocks(14L, "2023-01-02", 0L)[[1]]
  e <- rep(c(0, 1, 2, 3, 2, 1, 0), 2); r <- c(0, 1, 3, 2, 2, 0, 0, 0, 2, 1, 5, 1, 1, 0)
  for (seed in 1:200) {
    set.seed(seed); d <- MOSAIC:::.mosaic_obs_deaths_row(r, e, blk, phi = 4)
    set.seed(seed); G <- stats::rgamma(2L, shape = 9 / 3, rate = 9 / 3)
    for (b in 1:2) {
      i <- blk$block == b
      if (G[b] < 1) expect_true(all(d[i] <= r[i])) else expect_true(all(d[i] >= r[i]))
    }
    expect_true(all(d[e == 0 & r == 0] == 0))
  }
  expect_identical(MOSAIC:::.mosaic_obs_deaths_row(r, e, blk, phi = 1), r)
})

# ---- central lines engine-level; intervals observation-level ------------------

.het_records <- function(n_param, n_stoch, n_time, seed = 3) {
  set.seed(seed)
  base <- 40 * exp(sin(seq_len(n_time) / 4))
  lev  <- exp(stats::rnorm(n_param, 0, 0.4))
  draws <- lapply(seq_len(n_param * n_stoch), function(i)
    stats::rpois(n_time, base * lev[(i - 1L) %% n_param + 1L]))
  .obs_records(n_param, n_stoch, function(p, s) draws[[(s - 1L) * n_param + p]],
               function(p, s) stats::rbinom(n_time, draws[[(s - 1L) * n_param + p]], 0.02))
}

test_that("central lines are engine-level summaries; intervals and predictive median are observation-level", {
  P <- 12L; S <- 5L; Tn <- 35L
  cfg <- .obs_cfg(Tn)
  w <- rev(seq_len(P)); w <- w / sum(w)
  local_mocked_ensemble_sims(.het_records(P, S, Tn))
  run <- function(om) calc_model_ensemble(config = cfg, configs = rep(list(cfg), P),
                                          parameter_weights = w, n_simulations_per_config = S,
                                          observation_model = om, verbose = FALSE)
  ens0 <- run(NULL)
  ens  <- run(list(k_cases = 0.13))
  # No observation model: the arrays are the engine draws, nothing else changes.
  expect_identical(ens0$cases_array, ens0$cases_engine_array)
  expect_false(ens0$observation_model$cases)
  expect_identical(ens0$predictive_median$cases, ens0$cases_median)
  # With one: identical engine draws and central lines ...
  expect_identical(ens$cases_engine_array, ens0$cases_engine_array)
  for (f in c("cases_mean", "cases_median", "deaths_mean", "deaths_median"))
    expect_identical(ens[[f]], ens0[[f]], info = f)
  sw <- rep(ens$parameter_weights, times = S) / S
  med_eng <- apply(ens$cases_engine_array[1, , , ], 1, function(v) weighted_quantiles(as.vector(v), sw, 0.5))
  expect_equal(as.numeric(ens$cases_median), med_eng, tolerance = 0)
  # ... but the envelope and the predictive median are quantiles of the observation draws.
  q_obs <- apply(ens$cases_array[1, , , ], 1, function(v)
    weighted_quantiles(as.vector(v), sw, c(0.5, 0.025, 0.975, 0.25, 0.75)))
  expect_equal(as.numeric(ens$predictive_median$cases), q_obs[1, ], tolerance = 0)
  expect_equal(as.numeric(ens$ci_bounds$cases[[1]]$lower), q_obs[2, ], tolerance = 0)
  expect_equal(as.numeric(ens$ci_bounds$cases[[1]]$upper), q_obs[3, ], tolerance = 0)
  expect_equal(as.numeric(ens$ci_bounds$cases[[2]]$lower), q_obs[4, ], tolerance = 0)
  expect_equal(as.numeric(ens$ci_bounds$cases[[2]]$upper), q_obs[5, ], tolerance = 0)
  expect_gt(mean(ens$ci_bounds$cases[[1]]$upper - ens$ci_bounds$cases[[1]]$lower),
            mean(ens0$ci_bounds$cases[[1]]$upper - ens0$ci_bounds$cases[[1]]$lower))
  # At k = 0.13 a median of the observation draws collapses: the reason the
  # central line is the engine median, not the predictive one.
  expect_lt(sum(ens$predictive_median$cases), 0.6 * sum(ens$cases_median))
})

test_that("the prediction table's quantiles nest at small k: predicted_median is the predictive median", {
  # Release red team OBS-1: predicted_median was the engine median while the ci_*
  # columns were observation-level, so at small k the published median lay above
  # its own 50% band, failing the frozen v1.0 evaluator's M-OUTPUT check
  # (l95 <= l50 <= median <= u50 <= u95 on >= 99% of days).
  P <- 12L; S <- 5L; Tn <- 35L
  cfg <- .obs_cfg(Tn)
  w <- rev(seq_len(P)); w <- w / sum(w)
  recs <- .het_records(P, S, Tn, seed = 13)
  E <- rep(c(0.5, 1, 2, 3, 2, 1, 0.5), 5)
  for (i in seq_along(recs)) recs[[i]]$expected_deaths <- matrix(E, 1L)
  local_mocked_ensemble_sims(recs)
  ens <- calc_model_ensemble(config = cfg, configs = rep(list(cfg), P), parameter_weights = w,
                             n_simulations_per_config = S,
                             deaths_integration = .obs_di(Tn, phi = 3),
                             observation_model = list(k_cases = 0.14), verbose = FALSE)
  expect_true(ens$observation_model$cases)
  expect_true(ens$observation_model$deaths)
  nested <- function(t) t$ci_1_lower <= t$ci_2_lower & t$ci_2_lower <= t$predicted_median &
    t$predicted_median <= t$ci_2_upper & t$ci_2_upper <= t$ci_1_upper

  for (cm in list(c(cases = "median", deaths = "mean"), "median")) {
    tbl <- MOSAIC:::.mosaic_assemble_prediction_table(ens, central_method = cm,
                                                      n_cases_warmup_mask = 0L)
    cas <- tbl$metric == "Suspected Cases"
    expect_true(all(is.finite(as.matrix(tbl[, c("predicted_median", "ci_1_lower", "ci_1_upper",
                                                 "ci_2_lower", "ci_2_upper")]))))
    expect_true(all(nested(tbl)))
    expect_identical(tbl$predicted_median[cas],  as.numeric(ens$predictive_median$cases[1, ]))
    expect_identical(tbl$predicted_median[!cas], as.numeric(ens$predictive_median$deaths[1, ]))
    # The central line and the mean stay engine-level.
    expect_identical(tbl$predicted_central[cas], as.numeric(ens$cases_median[1, ]))
    expect_identical(tbl$predicted_mean[cas],    as.numeric(ens$cases_mean[1, ]))
    expect_identical(tbl$predicted_central[!cas],
                     as.numeric(if (identical(cm, "median")) ens$deaths_median[1, ] else ens$deaths_mean[1, ]))
  }
  # The fixture is in the regime that broke: the engine median sits above the
  # observation-level 50% band on most days, so the old column fails nesting.
  old <- tbl; old$predicted_median[cas] <- as.numeric(ens$cases_median[1, ])
  expect_gt(mean(!nested(old)[cas]), 0.5)

  # Without observation noise predicted_median is the engine median, which then
  # shares its draws with the engine-level intervals.
  eng <- ens; eng$observation_model$cases <- FALSE; eng$observation_model$deaths <- FALSE
  te <- MOSAIC:::.mosaic_assemble_prediction_table(eng, central_method = "mean",
                                                   n_cases_warmup_mask = 0L)
  expect_identical(te$predicted_median[cas],  as.numeric(ens$cases_median[1, ]))
  expect_identical(te$predicted_median[!cas], as.numeric(ens$deaths_median[1, ]))

  # The plotted burn-in head of a supplied (masked) median table is filled with
  # the engine median, not with the predictive median.
  masked <- MOSAIC:::.mosaic_assemble_prediction_table(ens, central_method = "median",
                                                       score_idx_cases = 8L, score_idx_deaths = 8L)
  shown <- MOSAIC:::.mosaic_display_prediction_table(
    ens, MOSAIC:::.mosaic_resolve_central_method("median"), mask_final_deaths_step = FALSE,
    head_cases = 7L, head_deaths = 7L, prediction_table = masked)
  sc <- shown$metric == "Suspected Cases"
  expect_identical(shown$predicted_central[sc][1:7], as.numeric(ens$cases_median[1, 1:7]))
  expect_identical(shown$predicted_central[!sc][1:7], as.numeric(ens$deaths_median[1, 1:7]))
  expect_identical(shown$predicted_median[sc][1:7], as.numeric(ens$predictive_median$cases[1, 1:7]))
  expect_false(identical(shown$predicted_central[sc][1:7], shown$predicted_median[sc][1:7]))
})

test_that("the medoid, R_eff medoid and optimizer selection read the engine arrays (unchanged by the noise)", {
  P <- 12L; S <- 5L; Tn <- 35L
  cfg <- .obs_cfg(Tn)
  w <- rev(seq_len(P)); w <- w / sum(w)
  local_mocked_ensemble_sims(.het_records(P, S, Tn, seed = 9))
  run <- function(om) calc_model_ensemble(config = cfg, configs = rep(list(cfg), P),
                                          parameter_weights = w, n_simulations_per_config = S,
                                          observation_model = om, verbose = FALSE)
  ens0 <- run(NULL); ens <- run(list(k_cases = 0.5))

  d0 <- MOSAIC:::.mosaic_medoid_distances(ens0$cases_array, ens0$cases_median, ens0$artifact_mask)
  d1 <- MOSAIC:::.mosaic_medoid_distances(MOSAIC:::.mosaic_engine_array(ens, "cases"),
                                          ens$cases_median, ens$artifact_mask)
  expect_identical(d1, d0)
  expect_false(isTRUE(all.equal(
    MOSAIC:::.mosaic_medoid_distances(ens$cases_array, ens$cases_median, ens$artifact_mask), d0)))
  m0 <- MOSAIC:::.mosaic_reff_select_medoid_member(ens0$cases_array, ens0$cases_median, P, S,
                                                   mask_spec = ens0$artifact_mask)
  m1 <- MOSAIC:::.mosaic_reff_select_medoid_member(MOSAIC:::.mosaic_engine_array(ens, "cases"),
                                                   ens$cases_median, P, S,
                                                   mask_spec = ens$artifact_mask)
  expect_identical(m1, m0)
  # run_MOSAIC()'s v0.101.0 wiring (release red team TA-02): the medoid distance,
  # the trajectory reduction and the implied CFR read the engine arrays; the
  # candidate and medoid ensembles both get the run's observation model; the
  # weekly offsets are resolved once with k.
  src <- gsub("[[:space:]]", "", paste(deparse(MOSAIC::run_MOSAIC), collapse = ""))
  pin <- function(pattern, n = 1L)
    expect_identical(lengths(regmatches(src, gregexpr(pattern, src, fixed = TRUE))), n, info = pattern)
  pin(".mosaic_medoid_distances(.mosaic_engine_array(ensemble,\"cases\")")
  expect_match(src, paste0(".mosaic_build_trajectories\\([^)]*cases_array=\\.mosaic_engine_array\\(ensemble,",
                           "\"cases\"\\),deaths_array=\\.mosaic_engine_array\\(ensemble,\"deaths\"\\)"))
  pin(paste0(".mosaic_calc_cfr_period_implied(cases_array=.mosaic_engine_array(ensemble,\"cases\"),",
             "deaths_array=.mosaic_engine_array(ensemble,\"deaths\")"))
  pin("observation_model=obs_model_run", 2L)
  pin("reduce_trajectories=FALSE,deaths_integration=control$likelihood$.deaths_integration,observation_model=obs_model_run")
  pin("capture_trajectories=FALSE,deaths_integration=control$likelihood$.deaths_integration,observation_model=obs_model_run")
  pin("obs_model_run<-.mosaic_resolve_observation_model(config,control)")
  pin("control$likelihood$.cases_week_offset_resolved<-.nb_disp$cases$week_offset")

  # Optimizer: identical selection; the rebuilt envelope comes from the selected
  # members' observation draws (through the likelihood sort permutation).
  ll <- -100 - c(3, 0.5, 7, 1, 2, 9, 0, 4, 6, 5, 8, 1.5)
  o0 <- optimize_ensemble_subset(ens0, ll, min_n = 4L, verbose = FALSE)
  o1 <- optimize_ensemble_subset(ens,  ll, min_n = 4L, verbose = FALSE)
  expect_identical(o1$optimal_n, o0$optimal_n)
  expect_equal(o1$evaluation_table, o0$evaluation_table, tolerance = 0)
  eo <- o1$ensemble_optimized
  for (f in c("cases_mean", "cases_median", "deaths_mean", "deaths_median"))
    expect_identical(eo[[f]], o0$ensemble_optimized[[f]], info = f)
  idx <- order(ll, decreasing = TRUE)[seq_len(o1$optimal_n)]
  expect_identical(eo$cases_array, ens$cases_array[, , idx, , drop = FALSE])
  expect_identical(eo$cases_engine_array, ens$cases_engine_array[, , idx, , drop = FALSE])
  ref <- MOSAIC:::.mosaic_ensemble_summaries(eo$cases_engine_array, o1$optimal_weights,
                                             eo$envelope_quantiles, obs_array = eo$cases_array)
  expect_identical(eo$ci_bounds$cases, ref$ci_bounds)
  expect_identical(eo$predictive_median$cases, ref$predictive_median)
  expect_identical(eo$observation_model, ens$observation_model)
})

test_that("trajectories reduce the engine trajectories, matching the engine central line", {
  P <- 4L; S <- 3L; Tn <- 21L
  cfg <- .obs_cfg(Tn)
  recs <- .het_records(P, S, Tn, seed = 21)
  for (i in seq_along(recs)) recs[[i]]$traj <- list(S = matrix(1000, 1L, Tn), Isym = matrix(5, 1L, Tn))
  local_mocked_ensemble_sims(recs)
  ens <- calc_model_ensemble(config = cfg, configs = rep(list(cfg), P), n_simulations_per_config = S,
                             capture_trajectories = TRUE, trajectory_channels = c("S", "Isym"),
                             observation_model = list(k_cases = 0.3), verbose = FALSE)
  tr <- ens$trajectories
  expect_equal(as.numeric(tr$summary$reported_cases$median), as.numeric(ens$cases_median), tolerance = 1e-10)
  expect_equal(as.numeric(tr$summary$reported_deaths$median), as.numeric(ens$deaths_mean), tolerance = 1e-10)
  # The thinned display lines are the engine member values at their own (member,
  # day) positions -- not observation draws. member_id = (s - 1) * P + j.
  ln <- tr$lines[tr$lines$channel == "reported_cases", ]
  expect_gt(nrow(ln), 0L)
  j <- ((ln$member_id - 1L) %% P) + 1L; s <- ((ln$member_id - 1L) %/% P) + 1L
  expect_identical(as.numeric(ln$value),
                   as.numeric(ens$cases_engine_array[cbind(1L, ln$t, j, s)]))
  expect_false(identical(as.numeric(ln$value), as.numeric(ens$cases_array[cbind(1L, ln$t, j, s)])))
})

# ---- reproducibility, RNG hygiene, slimming, memory ---------------------------

test_that("observation draws are reproducible per member and leave the caller's RNG alone", {
  P <- 3L; S <- 4L; Tn <- 21L
  cfg <- .obs_cfg(Tn)
  local_mocked_ensemble_sims(.het_records(P, S, Tn, seed = 5))
  run <- function(n_param) calc_model_ensemble(
    config = cfg, configs = rep(list(cfg), n_param), n_simulations_per_config = S,
    observation_model = list(k_cases = 1), verbose = FALSE)
  set.seed(77); before <- .Random.seed
  a <- run(P)
  expect_identical(.Random.seed, before)
  b <- run(P)
  expect_identical(a$cases_array, b$cases_array)
  # A member's draw depends only on its own (param_idx, stoch_idx) seed.
  one <- run(1L)
  expect_identical(one$cases_array[, , 1L, , drop = FALSE], a$cases_array[, , 1L, , drop = FALSE])
})

test_that("slimming drops all four arrays; the engine accessor falls back for pre-v0.101.0 ensembles", {
  ens <- structure(list(cases_array = array(1, c(1, 2, 1, 1)), deaths_array = array(2, c(1, 2, 1, 1)),
                        cases_engine_array = array(3, c(1, 2, 1, 1)),
                        deaths_engine_array = array(4, c(1, 2, 1, 1)),
                        cases_mean = matrix(1, 1, 2)), class = "mosaic_ensemble")
  slim <- MOSAIC:::.mosaic_ensemble_drop_arrays(ens)
  for (f in c("cases_array", "deaths_array", "cases_engine_array", "deaths_engine_array"))
    expect_null(slim[[f]], info = f)
  expect_s3_class(slim, "mosaic_ensemble")
  expect_identical(slim$cases_mean, ens$cases_mean)
  expect_null(MOSAIC:::.mosaic_engine_array(slim, "cases"))
  expect_identical(MOSAIC:::.mosaic_engine_array(ens, "deaths"), ens$deaths_engine_array)
  legacy <- ens; legacy$cases_engine_array <- NULL
  expect_identical(MOSAIC:::.mosaic_engine_array(legacy, "cases"), legacy$cases_array)
  # An observation-model ensemble that lost only its engine pair has no engine
  # array for a channel that was drawn with noise: NULL, so the callers' "no
  # arrays" errors fire, never its observation draws. A channel drawn without
  # noise still falls back, its array being the engine draws.
  part <- ens; part$cases_engine_array <- NULL; part$deaths_engine_array <- NULL
  part$observation_model <- list(cases = TRUE, deaths = FALSE)
  expect_null(MOSAIC:::.mosaic_engine_array(part, "cases"))
  expect_identical(MOSAIC:::.mosaic_engine_array(part, "deaths"), part$deaths_array)
  expect_error(optimize_ensemble_subset(part, -1, verbose = FALSE), "no prediction arrays")
})

test_that("the RAM projection counts the observation arrays and the expected-deaths payload", {
  one <- (40 * 1398 * 1000 * 10 * 8) / 2^30
  p <- MOSAIC:::.mosaic_ensemble_ram_projection_gb
  expect_equal(p(40, 1398, 1000, 10), 4 * one)
  expect_equal(p(40, 1398, 1000, 10, n_obs_arrays = 2L, n_record_extra = 1L), 7 * one)
})

# ---- observation-model specification -----------------------------------------

test_that("the observation model accepts the list form and the nb_dispersion.csv table", {
  locs <- c("AAA", "BBB")
  expect_null(MOSAIC:::.mosaic_normalize_observation_model(NULL, locs))
  om <- MOSAIC:::.mosaic_normalize_observation_model(list(k_cases = 2), locs)
  expect_identical(om, list(k_cases = c(2, 2), week_offset = c(0L, 0L)))
  tab <- data.frame(channel = c("deaths", "cases", "cases", "deaths"),
                    location = c("AAA", "BBB", "AAA", "BBB"),
                    k = c(9, Inf, 0.4, 9), week_offset = c(0L, 3L, 1L, 0L))
  om2 <- MOSAIC:::.mosaic_normalize_observation_model(tab, locs)
  expect_identical(om2, list(k_cases = c(0.4, Inf), week_offset = c(1L, 3L)))
  expect_error(MOSAIC:::.mosaic_normalize_observation_model(list(k_cases = c(1, 2, 3)), locs), "length 1 or 2")
  expect_error(MOSAIC:::.mosaic_normalize_observation_model(list(k_cases = -1), locs), "positive")
  expect_error(MOSAIC:::.mosaic_normalize_observation_model(list(k_cases = NA_real_), locs), "positive")
  expect_error(MOSAIC:::.mosaic_normalize_observation_model(list(k_cases = 1, week_offset = 9L), locs), "0-6")
  expect_error(MOSAIC:::.mosaic_normalize_observation_model(tab[tab$location == "AAA", ], locs), "BBB")
})

test_that("run_MOSAIC's observation model is the dispersion its likelihood scored with", {
  cfg <- list(location_name = c("AAA", "BBB"), date_start = "2023-01-01",
              reported_cases = rbind(rep(c(rep(5, 7), rep(9, 7)), 6)[1:80],
                                     rep(c(rep(5, 7), rep(9, 7)), 6)[1:80]))
  tab <- data.frame(channel = c("cases", "cases", "deaths", "deaths"),
                    location = c("AAA", "BBB", "AAA", "BBB"), week_offset = c(0L, 0L, 0L, 0L),
                    k = c(0.7, 3, 1, 1))
  ctl <- list(likelihood = list(.nb_k_cases_resolved = c(0.7, 3), .nb_dispersion_table = tab,
                                .score_window_resolved = list(idx_cases = 1L)))
  om <- MOSAIC:::.mosaic_resolve_observation_model(cfg, ctl)
  expect_identical(om, list(k_cases = c(0.7, 3), week_offset = c(0L, 0L)))
  # A table without the offsets (run_MOSAIC's own tables always carry them) has
  # them detected from the observed cases, as est_nb_dispersion() detects them.
  # This series' constant weekly blocks start on the window's first day, Sunday
  # 2023-01-01, i.e. 6 days after Monday.
  tab$week_offset <- NA_integer_
  om2 <- MOSAIC:::.mosaic_resolve_observation_model(cfg, list(likelihood = list(
    .nb_k_cases_resolved = c(0.7, 3), .nb_dispersion_table = tab)))
  expect_identical(om2$week_offset, c(6L, 6L))
  # Shifted one day later the blocks start on Monday: offset 0.
  cfg2 <- cfg; cfg2$reported_cases <- cbind(1, cfg$reported_cases[, -80])
  om3 <- MOSAIC:::.mosaic_resolve_observation_model(cfg2, list(likelihood = list(
    .nb_k_cases_resolved = c(0.7, 3), .nb_dispersion_table = tab)))
  expect_identical(om3$week_offset, c(0L, 0L))
  expect_null(MOSAIC:::.mosaic_resolve_observation_model(cfg, list(likelihood = list())))
})

# ---- real engine: the post-hoc deaths' expectation ----------------------------

test_that("the post-hoc redraw's expected_deaths is the mean of its reported deaths, day by day", {
  skip_if_not(exists("config_simulation_epidemic", asNamespace("MOSAIC")))
  cfg <- MOSAIC::config_simulation_epidemic
  cfg$mu_jt[] <- 0.15; cfg$delta_reporting_cases <- 2L
  cfg$I_j_initial[1] <- 10L
  r <- MOSAIC::run_simulation(cfg, seed = 4L, quiet = TRUE)$results
  cfg$reported_deaths <- r$reported_deaths
  pri <- list(mu_jt = list(sd_year = 0.7, sd_product = 0.3,
                           location = setNames(lapply(cfg$location_name, function(i)
                             list(year = 2020L, logit_mean = qlogis(0.15), logit_se = 0.2)), cfg$location_name)))
  di <- MOSAIC:::.mosaic_resolve_deaths_integration(cfg, list(likelihood = list()), pri, NULL)
  draws <- lapply(1:300, function(s) MOSAIC:::.mosaic_posthoc_deaths(di, r, cfg, seed = s))
  E   <- Reduce(`+`, lapply(draws, `[[`, "expected_deaths")) / length(draws)
  D   <- Reduce(`+`, lapply(draws, `[[`, "reported_deaths")) / length(draws)
  expect_identical(dim(E), dim(r$reported_deaths))
  # No expected death before the reporting lag + onset row.
  expect_true(all(E[, seq_len(cfg$delta_reporting_cases + 1L)] == 0))
  # Day by day the redraw's reported deaths average to its expectation (an
  # alignment off by one day would fail on the epidemic's rising/falling flanks).
  big <- E > 2
  expect_gt(sum(big), 20)
  z <- (D[big] - E[big]) / sqrt(E[big] / length(draws))
  expect_lt(max(abs(z)), 5)
  expect_lt(abs(mean(z)), 0.5)
})

test_that("an end-to-end ensemble on the real engine carries expected deaths and draws both channels", {
  skip_if_not(exists("config_simulation_epidemic", asNamespace("MOSAIC")))
  cfg <- MOSAIC::config_simulation_epidemic
  cfg$mu_jt[] <- 0.05; cfg$I_j_initial[1] <- 10L
  r <- MOSAIC::run_simulation(cfg, seed = 4L, quiet = TRUE)$results
  cfg$reported_cases <- r$reported_cases; cfg$reported_deaths <- r$reported_deaths
  pri <- list(mu_jt = list(sd_year = 0.7, sd_product = 0.3,
                           location = setNames(lapply(cfg$location_name, function(i)
                             list(year = 2020L, logit_mean = qlogis(0.05), logit_se = 0.2)), cfg$location_name)))
  di <- MOSAIC:::.mosaic_resolve_deaths_integration(cfg, list(likelihood = list()), pri, NULL)
  di$dispersion[] <- 2.5
  nL <- length(cfg$location_name)
  ens <- calc_model_ensemble(config = cfg, configs = list(cfg, cfg), n_simulations_per_config = 2L,
                             deaths_integration = di,
                             observation_model = list(k_cases = rep(1.5, nL)), verbose = FALSE)
  expect_true(ens$observation_model$cases)
  expect_true(ens$observation_model$deaths)
  expect_identical(dim(ens$cases_array), dim(ens$cases_engine_array))
  expect_false(identical(ens$cases_array, ens$cases_engine_array))
  expect_false(identical(ens$deaths_array, ens$deaths_engine_array))
  # Weekly (block) totals of every member are integers; days on which a member
  # has no engine cases get no observed cases.
  expect_true(all(ens$cases_array == round(ens$cases_array)))
  expect_true(all(ens$cases_array[ens$cases_engine_array == 0] == 0))
})

# ---- the v0.100.1 KEN rehearsal (local data only) -----------------------------

test_that("KEN rehearsal: observation-level arrays restore weekly interval coverage", {
  dir <- path.expand("~/MOSAIC/output/production/v2026-10.01/national/KEN")
  skip_if_not(dir.exists(dir), "the v0.100.1 KEN rehearsal run is not on this machine")
  ens <- readRDS(file.path(dir, "2_calibration", "ensemble_candidate.rds"))
  skip_if(is.null(ens$cases_array), "rehearsal ensemble saved without arrays")
  nb  <- utils::read.csv(file.path(dir, "2_calibration", "diagnostics", "nb_dispersion.csv"))
  om  <- MOSAIC:::.mosaic_normalize_observation_model(nb, ens$location_names)
  blocks <- MOSAIC:::.mosaic_observation_blocks(ens$n_time_points, ens$date_start, om$week_offset)
  eng <- MOSAIC:::.mosaic_engine_array(ens, "cases")           # pre-v0.101.0: engine-level
  d <- dim(eng); obs <- eng
  for (p in seq_len(d[3])) for (s in seq_len(d[4])) {
    m <- matrix(eng[, , p, s], d[1])
    if (!all(is.finite(m))) next
    obs[, , p, s] <- MOSAIC:::.mosaic_observation_draw(
      m, matrix(0, d[1], d[2]), NULL, blocks, om$k_cases, NULL,
      seed = MOSAIC:::.mosaic_derive_seed(p * 1000L + s, "observation"))$cases
  }
  # Complete ISO weeks inside the scored window with finite observations (the
  # acceptance evaluator's weeks for the cases channel).
  dates <- as.Date(ens$date_start) + seq_len(d[2]) - 1L
  y <- as.numeric(if (is.matrix(ens$obs_cases)) ens$obs_cases[1, ] else ens$obs_cases)
  ok <- seq_len(d[2]) >= ens$artifact_mask$score_idx_cases & is.finite(y)
  wk <- as.character(.iso_monday(dates))
  full <- names(which(tapply(ok, wk, sum) == 7L))
  sel <- ok & wk %in% full
  wm <- rep(ens$parameter_weights / sum(ens$parameter_weights), times = d[4]) / d[4]
  weekly <- function(A) rowsum(matrix(A[1, sel, , ], sum(sel)), wk[sel])
  yw <- as.numeric(rowsum(y[sel], wk[sel]))
  cov <- function(W) {
    q <- t(apply(W, 1, weighted_quantiles, w = wm, probs = c(0.025, 0.975, 0.25, 0.75)))
    c(cov95 = mean(yw >= q[, 1] & yw <= q[, 2]), cov50 = mean(yw >= q[, 3] & yw <= q[, 4]))
  }
  ce <- cov(weekly(eng)); co <- cov(weekly(obs))
  expect_gt(length(yw), 150)
  # Statistician's task3 (v0.100.1): engine cov95 0.30 -> 1.00, cov50 0.13 -> 0.74.
  expect_lt(ce[["cov95"]], 0.5)
  expect_gte(co[["cov95"]], 0.95)
  expect_gt(co[["cov50"]], 0.5); expect_lt(co[["cov50"]], 0.9)
  # Mean-preserving: the members' weekly predictive mean is unchanged.
  expect_equal(sum(weekly(obs) %*% wm) / sum(weekly(eng) %*% wm), 1, tolerance = 0.1)
})
