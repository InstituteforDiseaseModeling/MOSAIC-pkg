# fit.weekly of calibration_diagnostics.json (MOSAIC 1.5.1): every complete
# week, from weekly-summed member trajectories (.mosaic_calibration_weekly()).

# One location, 17 days from Monday 2024-01-01: two complete weeks and a
# partial third. Two members (weights 0.75 / 0.25) x two stochastic runs, so the
# trajectory weights are 0.375, 0.125, 0.375, 0.125 (param-fastest).
weekly_fixture <- function() {
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 17)
  rate <- c(1, 2, 3, 4)                         # trajectory r = (p, s): (1,1) (2,1) (1,2) (2,2)
  mk <- function(scale_w1, scale_w2) {
    a <- array(NA_real_, c(1, 17, 2, 2))
    for (r in 1:4) {
      p <- (r - 1) %% 2 + 1; s <- (r - 1) %/% 2 + 1
      a[1, , p, s] <- rate[r] * ifelse(seq_along(dates) <= 7, scale_w1, scale_w2)
    }
    a
  }
  eng_c <- mk(1, 2)
  eng_c[1, 10, 2, 2] <- NA                      # trajectory (2,2) misses a day of week 2
  ens <- list(cases_engine_array = eng_c, cases_array = mk(10, 10),
              deaths_engine_array = mk(0.1, 0.1), deaths_array = mk(1, 1),
              parameter_weights = c(0.75, 0.25), location_names = "AAA",
              date_start = "2024-01-01", date_stop = "2024-01-17", n_time_points = 17L,
              observation_model = list(cases = TRUE, deaths = TRUE))
  pt <- rbind(
    data.frame(location = "AAA", date = dates, metric = "Suspected Cases",
               observed = c(rep(1, 7), 1, 1, NA, 1, 1, 1, 1, 1, 1, 1),
               predicted_central = 1, central_method = "median",
               ci_1_lower = 0, ci_1_upper = 4, ci_2_lower = 0.5, ci_2_upper = 2),
    data.frame(location = "AAA", date = dates, metric = "Deaths",
               observed = 0, predicted_central = c(NA, rep(0.5, 16)), central_method = "mean",
               ci_1_lower = 0, ci_1_upper = 2, ci_2_lower = 0, ci_2_upper = 1))
  list(pt = pt, ens = ens)
}

test_that("weekly central line and intervals come from weekly-summed trajectories (hand-computed)", {
  fx <- weekly_fixture()
  w <- MOSAIC:::.mosaic_calibration_weekly(fx$pt, fx$ens)
  expect_identical(w$meta$source, "trajectories")
  expect_identical(w$meta$central_method, list(cases = "median", deaths = "mean"))
  expect_identical(w$meta$intervals, list(cases = "observation", deaths = "observation"))
  wk <- w$weekly
  # complete weeks only: the partial week from 2024-01-15 is left out
  expect_identical(wk$week[wk$metric == "cases"], c("2024-01-01", "2024-01-08"))
  c1 <- wk[wk$metric == "cases" & wk$week == "2024-01-01", ]
  # engine weekly totals 7, 14, 21, 28 at weights .375, .125, .375, .125: midpoint
  # positions .1875, .4375, .6875, .9375, so the median is 14 + 7 x (.5 - .4375) / .25
  expect_equal(c1$predicted, 15.75)
  # observation-level totals 70, 140, 210, 280: 2.5% and 97.5% clamp to the ends,
  # 25% = 70 + 70 x .0625 / .25, 75% = 210 + 70 x .0625 / .25
  expect_equal(c(c1$lo95, c1$lo50, c1$hi50, c1$hi95), c(70, 87.5, 227.5, 280))
  expect_true(c1$scored)
  expect_equal(c1$observed, 7)
  c2 <- wk[wk$metric == "cases" & wk$week == "2024-01-08", ]
  # trajectory (2,2) has a missing day, so it is dropped and the rest renormalised:
  # totals 14, 28, 42 at .375, .125, .375 -> positions .214, .5, .786 -> median 28
  expect_equal(c2$predicted, 28)
  # a missing observation: not scored, no observed total over a short week
  expect_false(c2$scored)
  expect_true(is.na(c2$observed))
  # deaths: central method "mean": .375 x .7 + .125 x 1.4 + .375 x 2.1 + .125 x 2.8
  d2 <- wk[wk$metric == "deaths" & wk$week == "2024-01-08", ]
  expect_equal(d2$predicted, 1.575)
  expect_true(d2$scored)
  expect_equal(d2$observed, 0)
  # the burn-in day (central line NA on 2024-01-01) leaves week 1 without model values
  d1 <- wk[wk$metric == "deaths" & wk$week == "2024-01-01", ]
  expect_true(all(is.na(unlist(d1[c("predicted", "lo50", "hi50", "lo95", "hi95")]))))
  expect_false(d1$scored)
})

test_that("a sparse series: the weekly median of trajectories is not the sum of daily medians", {
  # Three equally weighted trajectories, each with all seven cases on a
  # different day: every weekly total is 7, while every daily median is 0.
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 7)
  a <- array(0, c(1, 7, 3, 1))
  for (p in 1:3) a[1, p, p, 1] <- 7
  ens <- list(cases_engine_array = a, cases_array = a, deaths_engine_array = a, deaths_array = a,
              parameter_weights = rep(1 / 3, 3), location_names = "AAA", date_start = "2024-01-01",
              date_stop = "2024-01-07", n_time_points = 7L, observation_model = list(cases = TRUE, deaths = TRUE))
  daily_med <- vapply(1:7, function(t) MOSAIC::weighted_quantiles(a[1, t, , 1], rep(1 / 3, 3), 0.5), numeric(1))
  expect_identical(sum(daily_med), 0)
  pt <- data.frame(location = "AAA", date = dates, metric = "Suspected Cases", observed = 1,
                   predicted_central = daily_med)
  w <- MOSAIC:::.mosaic_calibration_weekly(pt, ens, central_method = c(cases = "median", deaths = "mean"))
  expect_equal(w$weekly$predicted, 7)
  expect_equal(c(w$weekly$lo95, w$weekly$hi95), c(7, 7))
})

test_that("the central method can be passed, and without arrays the daily table is summed", {
  fx <- weekly_fixture()
  w <- MOSAIC:::.mosaic_calibration_weekly(fx$pt, fx$ens, central_method = c(cases = "mean", deaths = "mean"))
  c1 <- w$weekly[w$weekly$metric == "cases" & w$weekly$week == "2024-01-01", ]
  expect_equal(c1$predicted, .375 * 7 + .125 * 14 + .375 * 21 + .125 * 28)
  ens <- fx$ens; ens$cases_array <- NULL
  for (e in list(NULL, ens)) {
    s <- MOSAIC:::.mosaic_calibration_weekly(fx$pt, e)
    expect_identical(s$meta$source, "daily_sums")
    x <- s$weekly[s$weekly$metric == "cases" & s$weekly$week == "2024-01-01", ]
    expect_equal(c(x$predicted, x$lo95, x$hi95, x$lo50, x$hi50), c(7, 0, 28, 3.5, 14))
  }
  e <- MOSAIC:::.mosaic_calibration_weekly(NULL)
  expect_identical(nrow(e$weekly), 0L)
  expect_identical(e$meta$source, "none")
  expect_identical(names(e$weekly), c("location", "metric", "week", "scored", "observed", "predicted",
                                      "lo50", "hi50", "lo95", "hi95"))
})

test_that("the weekly mean of trajectories equals the sum of the daily ensemble means", {
  skip_on_cran()
  set.seed(3)
  n_t <- 28; P <- 5; S <- 3
  eng <- array(stats::rgamma(n_t * P * S, 2, 0.1), c(1, n_t, P, S))
  w <- stats::runif(P); w <- w / sum(w)
  ens <- list(cases_engine_array = eng, cases_array = eng, deaths_engine_array = eng, deaths_array = eng,
              parameter_weights = w, location_names = "AAA", date_start = "2024-01-01",
              date_stop = "2024-01-28", n_time_points = n_t, observation_model = list(cases = FALSE, deaths = FALSE))
  sw <- rep(w, times = S) / S
  daily_mean <- vapply(seq_len(n_t), function(t) sum(as.vector(eng[1, t, , ]) * sw), numeric(1))
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = n_t)
  pt <- data.frame(location = "AAA", date = dates, metric = "Deaths", observed = 1, predicted_central = daily_mean)
  out <- MOSAIC:::.mosaic_calibration_weekly(pt, ens, central_method = c(cases = "median", deaths = "mean"))
  expect_equal(out$weekly$predicted, as.vector(tapply(daily_mean, rep(1:4, each = 7), sum)), tolerance = 1e-12)
  expect_identical(out$meta$intervals, list(cases = "engine", deaths = "engine"))
})
