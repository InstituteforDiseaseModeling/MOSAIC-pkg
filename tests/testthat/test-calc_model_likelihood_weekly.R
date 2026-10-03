# =============================================================================
# test-calc_model_likelihood_weekly.R
#
# v0.101.0: cases_scoring = "weekly" scores the cases NB core on reporting-week
# totals of the observed and simulated daily cases, on the weeks
# est_nb_dispersion() estimates k from. The default stays the per-day cells
# ("daily"): the v0.101.0 likelihood gate blocked the weekly rule as the default.
# These tests therefore request the weekly rule explicitly. Hand-computed
# fixtures pin the value; the rest pin the block rule (Monday to Sunday, partial
# weeks dropped), the weights, the scored window, the daily default and,
# qualitatively, the re-selection evidence that motivated the weekly rule.
# =============================================================================

# One location on a daily grid that starts on a WEDNESDAY: a 5-day partial week
# (3 cases a day, prediction 100 a day -- both must be ignored), then five
# complete Monday-Sunday weeks whose observed totals are spread over their days
# and whose predictions are uneven within the week.
.wk_fixture <- function() {
     d0 <- as.Date("2024-01-03")
     Y <- c(70, 35, 14, 21, 0)
     obs <- c(rep(3, 5), rep(Y / 7, each = 7))
     est <- c(rep(100, 5), c(0, 0, 30, 30, 0, 0, 0), rep(7, 7), c(0, 0, 0, 7, 0, 0, 0),
              rep(4, 7), rep(1, 7))
     list(d0 = d0, Y = Y, M = c(60, 49, 7, 28, 7), obs = obs, est = est,
          cfg = list(date_start = d0, date_stop = d0 + length(obs) - 1L))
}
.one <- function(x) matrix(x, nrow = 1L)
.ll1 <- function(obs, est, cfg, k = 2, cases_scoring = "weekly", ...) {
     MOSAIC::calc_model_likelihood(.one(obs), .one(est), .one(obs * 0), .one(est * 0),
                                   config = cfg, nb_k_cases = k, nb_k_deaths = Inf,
                                   ll_deaths_core = 0, cases_scoring = cases_scoring, ...)
}

test_that("the weekly cases core equals the hand-computed NB over complete weeks", {
     fx <- .wk_fixture()
     ll <- .ll1(fx$obs, fx$est, fx$cfg)
     eps <- max(1e-4, 0.02 * mean(fx$Y))          # floor relative to the mean WEEKLY count
     hand <- sum(stats::dnbinom(fx$Y, mu = pmax(fx$M, eps), size = 2, log = TRUE))
     expect_equal(ll, hand, tolerance = 1e-12)
     expect_equal(ll, -19.7951764389, tolerance = 1e-9)
     # Poisson limit: the same weekly cells
     llp <- .ll1(fx$obs, fx$est, fx$cfg, k = Inf)
     expect_equal(llp, sum(stats::dpois(fx$Y, pmax(fx$M, eps), log = TRUE)), tolerance = 1e-12)
     expect_equal(llp, -24.1097126608, tolerance = 1e-9)
})

test_that("the default cases_scoring = 'daily' is the per-day score", {
     fx <- .wk_fixture()
     lld <- .ll1(fx$obs, fx$est, fx$cfg, cases_scoring = "daily")
     eps_d <- max(1e-4, 0.02 * mean(fx$obs))
     expect_equal(lld, sum(stats::dnbinom(fx$obs, mu = pmax(fx$est, eps_d), size = 2, log = TRUE)),
                  tolerance = 1e-12)
     expect_equal(lld, -266.0109806011, tolerance = 1e-9)
     # it is the default: no argument, or NULL, scores per day
     dflt <- MOSAIC::calc_model_likelihood(.one(fx$obs), .one(fx$est), .one(fx$obs * 0), .one(fx$est * 0),
                                           config = fx$cfg, nb_k_cases = 2, nb_k_deaths = Inf,
                                           ll_deaths_core = 0)
     expect_identical(dflt, lld)
     expect_identical(.ll1(fx$obs, fx$est, fx$cfg, cases_scoring = NULL), lld)
     expect_false(isTRUE(all.equal(lld, .ll1(fx$obs, fx$est, fx$cfg))))   # the weekly rule differs
     # and equals the undated call, which has no reporting weeks to form
     expect_identical(lld, .ll1(fx$obs, fx$est, NULL))
     expect_error(.ll1(fx$obs, fx$est, fx$cfg, cases_scoring = "monthly"), "should be one of")
})

test_that("confidence weights: the week takes its days' mean weight, mass-preserving", {
     fx <- .wk_fixture()
     wob <- c(rep(0.9, 5), rep(c(1, 0.5, 0.5, 1, 1), each = 7))
     ll <- .ll1(fx$obs, fx$est, fx$cfg, weights_obs_cases = .one(wob))
     w <- c(1, 0.5, 0.5, 1, 1); w <- w / sum(w) * 5       # rescaled to the 5 scored weeks
     eps <- max(1e-4, 0.02 * mean(fx$Y))
     hand <- sum(w * stats::dnbinom(fx$Y, mu = pmax(fx$M, eps), size = 2, log = TRUE))
     expect_equal(ll, hand, tolerance = 1e-12)
     expect_equal(ll, -19.6736385075, tolerance = 1e-9)
     # an all-ones weight matrix is the unweighted score
     expect_identical(.ll1(fx$obs, fx$est, fx$cfg, weights_obs_cases = .one(rep(1, 40))),
                      .ll1(fx$obs, fx$est, fx$cfg))
     # weights uneven within a week enter as their mean
     g <- MOSAIC:::.mosaic_week_blocks(fx$d0 + 0:39)$index
     wv <- rep(1, 40); wv[6:12] <- c(1, 1, 1, 1, 0, 0, 0)
     cells <- MOSAIC:::.cases_weekly_cells(fx$obs, fx$est, g, rep(1, 40), wv)
     raw <- c(4 / 7, 1, 1, 1, 1)
     expect_equal(cells$w, raw / sum(raw) * 5, tolerance = 1e-12)
     expect_equal(cells$gate, sum(raw), tolerance = 1e-12)
})

test_that("weekly totals are exact sums of the days and partial weeks are dropped", {
     fx <- .wk_fixture()
     g <- MOSAIC:::.mosaic_week_blocks(fx$d0 + 0:39)$index
     cells <- MOSAIC:::.cases_weekly_cells(fx$obs, fx$est, g, rep(1, 40))
     expect_identical(cells$y, fx$Y)
     expect_identical(cells$mu, fx$M)
     expect_identical(cells$n_weeks, 5L)
     # the leading partial week (Wed-Sun) does not count, whatever it holds
     base <- .ll1(fx$obs, fx$est, fx$cfg)
     o2 <- fx$obs; o2[1:5] <- 500; e2 <- fx$est; e2[1:5] <- 0
     expect_identical(.ll1(o2, e2, fx$cfg), base)
     # nor does a trailing partial week (Mon-Wed)
     cfg3 <- fx$cfg; cfg3$date_stop <- fx$cfg$date_stop + 3L
     expect_identical(.ll1(c(fx$obs, 9, 9, 9), c(fx$est, 0, 0, 0), cfg3), base)
     # but a change inside a complete week does
     e4 <- fx$est; e4[20] <- e4[20] + 50
     expect_false(isTRUE(all.equal(.ll1(fx$obs, e4, fx$cfg), base)))
     # moving prediction mass between days of one week changes nothing
     e5 <- fx$est; e5[13:19] <- c(49, 0, 0, 0, 0, 0, 0)
     expect_identical(.ll1(fx$obs, e5, fx$cfg), base)
})

test_that("a week with a missing day is not scored, and a non-finite prediction is counted", {
     fx <- .wk_fixture()
     eps <- max(1e-4, 0.02 * mean(fx$Y[-2]))
     o2 <- fx$obs; o2[15] <- NA                  # inside week 2
     expect_equal(.ll1(o2, fx$est, fx$cfg),
                  sum(stats::dnbinom(fx$Y[-2], mu = pmax(fx$M[-2], eps), size = 2, log = TRUE)),
                  tolerance = 1e-12)
     e2 <- fx$est; e2[15] <- NaN
     g <- MOSAIC:::.mosaic_week_blocks(fx$d0 + 0:39)$index
     cells <- MOSAIC:::.cases_weekly_cells(fx$obs, e2, g, rep(1, 40))
     expect_identical(cells$y, fx$Y[-2])
     expect_identical(cells$gate, 5L)            # the gate reads observations only
     expect_identical(cells$n_nonfinite, 1L)
     expect_identical(MOSAIC:::.cases_weekly_cells(fx$obs, fx$est, g, rep(1, 40))$n_nonfinite, 0L)
})

test_that("a non-finite simulated count on a scored week fails the path", {
     # A failed engine path (reported_cases NA at an N = 0 patch, say) must get no
     # weight. Dropping its weeks instead left the cases core at 0, the best score
     # there is, and a +Inf day removed that week's penalty, so the failed path
     # outranked valid ones.
     fx <- .wk_fixture()
     base <- .ll1(fx$obs, fx$est, fx$cfg)
     expect_identical(.ll1(fx$obs, rep(NA_real_, 40), fx$cfg), -Inf)
     # the engine channel is integer, so a failed path is NA_integer_; production
     # passes an integrated deaths score, which must not rescue it
     expect_identical(MOSAIC::calc_model_likelihood(.one(fx$obs), matrix(NA_integer_, 1L, 40L),
                                                    .one(fx$obs * 0), .one(fx$est * 0), config = fx$cfg,
                                                    nb_k_cases = 2, nb_k_deaths = Inf, ll_deaths_core = -5,
                                                    cases_scoring = "weekly"),
                      -Inf)
     for (bad in c(NA, NaN, Inf)) {
          e <- fx$est; e[15] <- bad                # a day of the second complete week
          expect_identical(.ll1(fx$obs, e, fx$cfg), -Inf, label = paste("est", bad))
     }
     # a non-finite value on a day that no scored week covers changes nothing
     e <- fx$est; e[1:5] <- c(NA, Inf, NaN, NA, NA)           # the dropped leading partial week
     expect_identical(.ll1(fx$obs, e, fx$cfg), base)
     o <- fx$obs; o[15] <- NA; e <- fx$est; e[16:17] <- NA     # week 2 has a missing observation
     expect_identical(.ll1(o, e, fx$cfg), .ll1(o, fx$est, fx$cfg))
     # ... or on a week whose confidence weight is 0, which adds nothing to the score
     wob <- c(rep(1, 5), rep(c(1, 0, 1, 1, 1), each = 7))
     e <- fx$est; e[14] <- NA
     expect_true(is.finite(.ll1(fx$obs, e, fx$cfg, weights_obs_cases = .one(wob))))
     wob[13:19] <- 0.5
     expect_identical(.ll1(fx$obs, e, fx$cfg, weights_obs_cases = .one(wob)), -Inf)
     # the default per-day rule keeps its earlier treatment: a missing day weighs
     # nothing, and only a path with no usable day is -Inf
     e <- fx$est; e[15] <- NA
     expect_true(is.finite(.ll1(fx$obs, e, fx$cfg, cases_scoring = "daily")))
     expect_identical(.ll1(fx$obs, rep(NA_real_, 40), fx$cfg, cases_scoring = "daily"), -Inf)
})

test_that("fewer than three scored weeks leave the cases core out", {
     fx <- .wk_fixture()
     keep <- 1:19                                 # partial week + 2 complete weeks
     cfg <- list(date_start = fx$d0, date_stop = fx$d0 + 18L)
     # deaths observed (zeros) with an integrated deaths score: the location
     # counts, with its cases core left out
     expect_identical(.ll1(fx$obs[keep], fx$est[keep], cfg), 0)
     # the daily rule would score them
     expect_true(.ll1(fx$obs[keep], fx$est[keep], cfg, cases_scoring = "daily") < 0)
})

test_that("a location short of three scored weeks and without deaths is NA, not a constant", {
     # The NA rule used the per-day gate (three observed days), so this location
     # scored 0 for every draw: in a single-location run, a flat likelihood and
     # uniform weights instead of the 'every likelihood is NA' stop.
     fx <- .wk_fixture()
     keep <- 1:19                                 # partial week + 2 complete weeks
     cfg <- list(date_start = fx$d0, date_stop = fx$d0 + 18L)
     na <- rep(NA_real_, 19)
     ll <- function(est, cases_scoring = "weekly", ...)
          MOSAIC::calc_model_likelihood(.one(fx$obs[keep]), .one(est), .one(na), .one(na), config = cfg,
                                        nb_k_cases = 2, nb_k_deaths = Inf, cases_scoring = cases_scoring, ...)
     expect_identical(ll(fx$est[keep]), NA_real_)
     expect_identical(ll(fx$est[keep] * 50), NA_real_)
     expect_identical(ll(fx$est[keep], ll_deaths_core = 0), NA_real_)   # run_MOSAIC with no deaths
     # the per-day rule scores the 19 days, and a cases shape term keeps the per-day gate
     expect_true(is.finite(ll(fx$est[keep], cases_scoring = "daily")))
     wis <- ll(fx$est[keep], weight_wis = 0.1)
     expect_true(is.finite(wis) && wis < 0)
     # beside a scored location it contributes nothing, as before
     two <- MOSAIC::calc_model_likelihood(rbind(c(fx$obs[keep], rep(NA, 21)), fx$obs),
                                          rbind(fx$est, fx$est), matrix(NA_real_, 2, 40),
                                          matrix(NA_real_, 2, 40), config = fx$cfg,
                                          nb_k_cases = 2, nb_k_deaths = Inf, cases_scoring = "weekly")
     expect_equal(two, -19.7951764389, tolerance = 1e-9)
     # the weighted gate: five complete weeks at confidence weight 0.5 make 2.5, at 0.7 make 3.5
     na40 <- rep(NA_real_, 40)
     llw <- function(w) MOSAIC::calc_model_likelihood(.one(fx$obs), .one(fx$est), .one(na40), .one(na40),
                                                    config = fx$cfg, nb_k_cases = 2, nb_k_deaths = Inf,
                                                    weights_obs_cases = .one(rep(w, 40)),
                                                    cases_scoring = "weekly")
     expect_identical(llw(0.5), NA_real_)
     expect_equal(llw(0.7), -19.7951764389, tolerance = 1e-9)
     # the NB deaths core (no ll_deaths_core) follows the same weekly gate
     lld <- function(mode) MOSAIC::calc_model_likelihood(.one(na), .one(na), .one(fx$obs[keep]),
                                                       .one(fx$est[keep]), config = cfg, nb_k_cases = Inf,
                                                       nb_k_deaths = 2, cases_scoring = mode)
     expect_identical(lld("weekly"), NA_real_)
     expect_true(is.finite(lld("daily")))
})

test_that("without ll_deaths_core the NB deaths core scores reporting-week totals", {
     # Lesson 7: the weekly rule applies to both channels. Deaths were scored as one
     # NB cell per day at the weekly k, the over-count the weekly cases core removes.
     fx <- .wk_fixture()
     na <- rep(NA_real_, 40)
     lld <- function(est_d, cases_scoring = "weekly", ...)
          MOSAIC::calc_model_likelihood(.one(na), .one(na), .one(fx$obs), .one(est_d), config = fx$cfg,
                                        nb_k_cases = Inf, nb_k_deaths = 2, cases_scoring = cases_scoring, ...)
     eps <- max(1e-4, 0.25 * mean(fx$Y))         # eps_rel_deaths, relative to the mean WEEKLY count
     expect_equal(lld(fx$est), sum(stats::dnbinom(fx$Y, mu = pmax(fx$M, eps), size = 2, log = TRUE)),
                  tolerance = 1e-12)
     expect_equal(lld(fx$est), -19.7951764389, tolerance = 1e-9)
     # moving predicted deaths between the days of a week changes nothing; per day it did
     e5 <- fx$est; e5[13:19] <- c(49, 0, 0, 0, 0, 0, 0)
     expect_identical(lld(e5), lld(fx$est))
     expect_false(isTRUE(all.equal(lld(e5, cases_scoring = "daily"), lld(fx$est, cases_scoring = "daily"))))
     # "daily" is the legacy per-day score
     eps_d <- max(1e-4, 0.25 * mean(fx$obs))
     expect_equal(lld(fx$est, cases_scoring = "daily"),
                  sum(stats::dnbinom(fx$obs, mu = pmax(fx$est, eps_d), size = 2, log = TRUE)), tolerance = 1e-12)
     # the deaths blocks are the reporting weeks detected on the cases: a series
     # reported Thursday to Wednesday
     d0 <- as.Date("2024-01-04"); Y <- c(14, 28, 7, 21)
     o <- .one(rep(Y / 7, each = 7)); e <- .one(rep(c(10, 30, 5, 25), each = 7) / 7)
     thu <- MOSAIC::calc_model_likelihood(o, e, o, e, config = list(date_start = d0, date_stop = d0 + 27L),
                                          nb_k_cases = 2, nb_k_deaths = 2, weight_cases = 0,
                                          cases_scoring = "weekly")
     expect_equal(thu, sum(stats::dnbinom(Y, mu = c(10, 30, 5, 25), size = 2, log = TRUE)), tolerance = 1e-12)
     # and a failed path fails here too
     e2 <- fx$est; e2[20] <- NA
     expect_identical(lld(e2), -Inf)
})

test_that("the cumulative term scores weekly-cell sums at size k * n / 7", {
     # n scored days of a weekly series are n / 7 weekly NB(k) totals, so their sum
     # has size k * n / 7, not the k * n of n independent NB(k) cells.
     fx <- .wk_fixture()
     na <- rep(NA_real_, 40)
     ll <- function(w, cases_scoring = "weekly", ...)
          MOSAIC::calc_model_likelihood(.one(fx$obs), .one(fx$est), .one(na), .one(na), config = fx$cfg,
                                        nb_k_cases = 2, nb_k_deaths = Inf, weight_cumulative_total = w,
                                        cases_scoring = cases_scoring, ...)
     eps <- max(1e-4, 0.02 * mean(fx$obs))
     cum <- function(div) mean(vapply(c(0.25, 0.5, 0.75, 1), function(p) {
          m <- round(40 * p)
          stats::dnbinom(sum(fx$obs[1:m]), mu = sum(pmax(fx$est[1:m], eps)), size = 2 * m / div,
                         log = TRUE) / m
     }, numeric(1)))
     # N_obs = 40 daily steps over 4 timepoints
     expect_equal(ll(0.5) - ll(0), 0.5 * (40 / 4) * cum(7), tolerance = 1e-10)
     # each time step is a cell under the per-day rule
     expect_equal(ll(0.5, cases_scoring = "daily") - ll(0, cases_scoring = "daily"),
                  0.5 * (40 / 4) * cum(1), tolerance = 1e-10)
     expect_gt(abs(cum(7) - cum(1)), 1e-3)
     expect_equal(MOSAIC:::.ll_cumulative_progressive_nb(fx$obs, fx$est, k_data = 2, cells_per_k = 7), cum(7),
                  tolerance = 1e-12)
})

test_that("weekly time steps and inconsistent dates are recognised", {
     fx <- .wk_fixture()
     # time steps that are weeks: one cell per step, i.e. the per-step score
     wcfg <- list(date_start = as.Date("2024-01-08"), date_stop = as.Date("2024-01-08") + 7L * 4L)
     expect_identical(.ll1(fx$Y, fx$M, wcfg), .ll1(fx$Y, fx$M, NULL))
     # dates that describe neither grid stop rather than misplace the weeks
     bad <- fx$cfg; bad$date_stop <- bad$date_stop + 10L
     expect_error(.ll1(fx$obs, fx$est, bad), "cannot be placed")
})

test_that("week_offset selects the reporting-week boundary and is validated", {
     fx <- .wk_fixture()
     base <- .ll1(fx$obs, fx$est, fx$cfg)
     expect_identical(.ll1(fx$obs, fx$est, fx$cfg, week_offset = 0L), base)
     expect_identical(.ll1(fx$obs, fx$est, fx$cfg, week_offset = NA), base)   # NA = detect
     expect_false(isTRUE(all.equal(.ll1(fx$obs, fx$est, fx$cfg, week_offset = 3L), base)))
     expect_error(.ll1(fx$obs, fx$est, fx$cfg, week_offset = 7L), "0 to 6")
     expect_error(.ll1(fx$obs, fx$est, fx$cfg, week_offset = c(0L, 1L)), "length 1 or n_locations")
     # a series reported Thursday-Wednesday is detected and aggregated on its own weeks
     d0 <- as.Date("2024-01-04")                 # a Thursday
     Y <- c(14, 28, 7, 21)
     cfg <- list(date_start = d0, date_stop = d0 + 27L)
     obs <- rep(Y / 7, each = 7); est <- rep(c(10, 30, 5, 25), each = 7) / 7
     off <- MOSAIC::est_nb_dispersion(.one(obs), date_start = d0, shrink = FALSE)$week_offset
     expect_identical(off, 3L)
     expect_equal(.ll1(obs, est, cfg),
                  sum(stats::dnbinom(Y, mu = c(10, 30, 5, 25), size = 2, log = TRUE)), tolerance = 1e-12)
})

test_that("the shared block helper gives Monday-Sunday weeks from any start day", {
     b <- MOSAIC:::.mosaic_week_blocks(as.Date("2023-01-01") + 0:15)   # Sunday start
     expect_identical(b$index, c(1L, rep(2L, 7), rep(3L, 7), 4L))
     expect_identical(b$week_start, as.Date(c("2022-12-26", "2023-01-02", "2023-01-09", "2023-01-16")))
     expect_true(all(format(b$week_start, "%u") == "1"))
     expect_identical(b$complete, c(FALSE, TRUE, TRUE, FALSE))
     b3 <- MOSAIC:::.mosaic_week_blocks(as.Date("2024-01-04") + 0:6, offset = 3L)
     expect_identical(b3$index, rep(1L, 7))
     expect_identical(b3$complete, TRUE)
     expect_identical(format(b3$week_start, "%a"), "Thu")
     expect_error(MOSAIC:::.mosaic_week_blocks(as.Date("2024-01-01") + c(0, 2)), "consecutive")
     expect_error(MOSAIC:::.mosaic_week_blocks(as.Date("2024-01-01") + 0:6, offset = 9), "0 to 6")
     expect_error(MOSAIC:::.mosaic_week_blocks(as.Date("2024-01-01") + 0:6, partial = "trim"),
                  "should be one of")
})

test_that("partial = 'drop' removes the edge weeks that partial = 'keep' makes blocks of", {
     dates <- as.Date("2023-01-01") + 0:15                            # Sunday to Monday
     k <- MOSAIC:::.mosaic_week_blocks(dates, partial = "keep")
     expect_identical(k$start, c(1L, 2L, 9L, 16L))
     expect_identical(k$end,   c(1L, 8L, 15L, 16L))
     d <- MOSAIC:::.mosaic_week_blocks(dates, partial = "drop")
     expect_identical(d$index, c(NA, rep(1L, 7), rep(2L, 7), NA))
     expect_identical(d$week_start, as.Date(c("2023-01-02", "2023-01-09")))
     expect_identical(d$complete, c(TRUE, TRUE))
     expect_identical(d$start, c(2L, 9L))
     expect_identical(d$end,   c(8L, 15L))
     # A grid of whole weeks has nothing to drop.
     w <- as.Date("2023-01-02") + 0:13
     expect_identical(MOSAIC:::.mosaic_week_blocks(w, partial = "drop"),
                      MOSAIC:::.mosaic_week_blocks(w, partial = "keep"))
     # A grid inside one week has no complete block.
     n <- MOSAIC:::.mosaic_week_blocks(as.Date("2023-01-03") + 0:3, partial = "drop")
     expect_identical(n$index, rep(NA_integer_, 4))
     expect_length(n$start, 0L)
})

test_that("the weekly cells are the same from a dropped-edge and a kept-edge block index", {
     # The likelihood drops the edge weeks explicitly (partial = "drop"); its
     # seven-usable-days rule drops them from a kept-edge index too, so the two
     # give the same cells -- and a grid with no complete week gives none.
     set.seed(11)
     for (rep in 1:40) {
          d0 <- as.Date("2023-01-01") + sample(0:6, 1)
          n <- sample(c(3L, 20L, 61L), 1)
          dates <- d0 + seq_len(n) - 1L
          obs <- stats::rpois(n, 6); est <- stats::rgamma(n, 2, 0.3)
          obs[sample(n, max(1L, n %/% 10))] <- NA
          est[sample(n, 1)] <- NA
          wt <- stats::runif(n, 0, 1); wt[sample(n, 1)] <- 0
          wob <- rep(stats::runif(ceiling(n / 7) + 1, 0.5, 1), each = 7)[seq_len(n)]
          off <- sample(0:6, 1)
          gk <- MOSAIC:::.mosaic_week_blocks(dates, off, partial = "keep")$index
          gd <- MOSAIC:::.mosaic_week_blocks(dates, off, partial = "drop")$index
          for (wo in list(NULL, wob))
               expect_identical(MOSAIC:::.cases_weekly_cells(obs, est, gd, wt, wo),
                                MOSAIC:::.cases_weekly_cells(obs, est, gk, wt, wo))
     }
})

test_that("downscaled weekly totals are recovered exactly on a Sunday-start grid", {
     set.seed(5)
     mondays <- as.Date("2023-01-02") + 7L * (0:29)
     Y <- stats::rnbinom(30, mu = 40, size = 0.8)
     dl <- MOSAIC::downscale_weekly_values(mondays, Y)
     # a Sunday start (config_default v6.x began on Sunday 2023-01-01): prepend that day
     dates <- c(as.Date("2023-01-01"), dl$date)
     obs <- c(5, dl$value)
     b <- MOSAIC:::.mosaic_week_blocks(dates)
     cells <- MOSAIC:::.cases_weekly_cells(obs, obs, b$index, rep(1, length(obs)))
     expect_identical(cells$y, as.numeric(Y))
     expect_identical(b$week_start[b$complete], mondays)
})

test_that("the scored window and burn-in are respected through the worker", {
     cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = "MOZ")
     for (f in c("reported_cases_weight", "reported_deaths_weight", "reported_tier")) cfg[[f]] <- NULL
     oc <- cfg$reported_cases; od <- cfg$reported_deaths
     oc[!is.finite(oc)] <- 0; od[!is.finite(od)] <- 0
     cfg$reported_cases <- oc; cfg$reported_deaths <- od
     nT <- ncol(oc)
     # The scored window starts mid-week on day s0, so the Monday-Sunday block holding
     # it straddles the burn-in boundary and is partial, and the first scored week
     # starts on the next Monday (day wk1). The geometry comes from the config's own
     # dates: from Sunday 2023-01-01, day 31 is a Tuesday, the straddling block is days
     # 30-36 and wk1 = 37; from Monday 2018-01-01, day 31 is a Wednesday, days 29-35
     # and wk1 = 36.
     dates <- as.Date(cfg$date_start) + seq_len(nT) - 1L
     s0 <- if (format(dates[31L], "%u") == "1") 32L else 31L   # a Monday leaves no straddle
     wk1 <- s0 + (8L - as.integer(format(dates[s0], "%u"))) %% 7L
     expect_identical(format(dates[c(wk1 - 7L, wk1)], "%u"), c("1", "1"))
     expect_true(wk1 - 7L < s0 && s0 < wk1)
     sw <- list(idx_cases = s0, idx_deaths = s0, n_time = nT)
     ls <- list(weight_cases = 1, weight_deaths = 0, eps_rel_cases = 0.02, eps_rel_deaths = 0.25,
                cases_scoring = "weekly",
                .nb_k_cases_resolved = 0.5, .nb_k_deaths_resolved = 1, .score_window_resolved = sw,
                .cases_week_offset_resolved = 0L, weights_location = 1, weight_peak_timing = 0,
                weight_peak_magnitude = 0, weight_cumulative_total = 0, weight_wis = 0,
                sigma_peak_time = 1, sigma_peak_log = 0.5)
     worker_ll <- function(est, ls) {
          stub <- function(config, seed = NULL, quiet = TRUE, ...)
               list(results = list(reported_cases = est, reported_deaths = od))
          testthat::local_mocked_bindings(sample_parameters = function(...) cfg,
                                          run_simulation = stub, .package = "MOSAIC")
          m <- MOSAIC:::.mosaic_run_simulation_worker(
               sim_id = 1L, n_iterations = 1L, priors = NULL, config = cfg, PATHS = NULL,
               dir_cal_samples = tempdir(), param_names_all = "gamma_1",
               param_lookup = MOSAIC:::.mosaic_build_param_lookup("gamma_1", cfg$location_name),
               sampling_args = list(), io = NULL, likelihood_settings = ls, write_shard = FALSE)
          unname(m[1, "likelihood"])
     }
     base <- round(oc * 1.1) + 1
     ll0 <- worker_ll(base, ls)
     expect_true(is.finite(ll0))
     pre <- seq_len(wk1 - 1L)                                  # burn-in + the straddling week
     head <- base; head[, pre] <- head[, pre] * 40 + 300
     expect_identical(worker_ll(head, ls), ll0)
     first <- wk1 + 0:6
     inside <- base; inside[, first] <- inside[, first] * 40 + 300
     expect_false(isTRUE(all.equal(worker_ll(inside, ls), ll0)))
     # the worker's weekly score is calc_model_likelihood() on the sliced window
     keep <- s0:nT
     cl <- cfg; cl$date_start <- as.Date(cfg$date_start) + (s0 - 1L)
     direct <- MOSAIC::calc_model_likelihood(oc[, keep, drop = FALSE], base[, keep, drop = FALSE],
                                             od[, keep, drop = FALSE], od[, keep, drop = FALSE],
                                             config = cl, nb_k_cases = 0.5, nb_k_deaths = 1,
                                             weight_deaths = 0, week_offset = 0L, cases_scoring = "weekly")
     expect_equal(ll0, direct, tolerance = 1e-10)
     # offsets left to detection give the same score; the daily rule does not
     ls_det <- ls; ls_det$.cases_week_offset_resolved <- NULL
     expect_identical(worker_ll(base, ls_det), ll0)
     ls_d <- ls; ls_d$cases_scoring <- "daily"
     ll_d <- worker_ll(base, ls_d)
     expect_false(isTRUE(all.equal(ll_d, ll0)))
     # settings without the key (an older control) score with the daily default
     ls_n <- ls; ls_n$cases_scoring <- NULL
     expect_identical(worker_ll(base, ls_n), ll_d)
})

test_that("weekly scoring selects the correctly levelled draws that daily scoring misses", {
     # Qualitative reproduction of the v0.100.1 re-selection evidence: a pool of
     # draws that differ in LEVEL (multiplier m) and each carry one stochastic daily
     # realisation, scored against a downscaled weekly series at the weekly k.
     # Daily cells rank the draws mostly by within-week noise against the flat
     # spread and favour over-predicting draws, which are less often 0 on a day;
     # weekly totals rank them by level.
     #
     # One pool is one draw of a noisy experiment: over seeds 1-70 a single pool
     # missed one of these criteria at the former single-pool thresholds in 16
     # seeds. The criteria are therefore medians over five independent pools,
     # with thresholds that a single pool misses in at most 3 of those 70 seeds,
     # so a re-rolled median (three of five pools would have to miss) fails with
     # probability of order 1e-3 or less.
     crit <- function(seed) {
          set.seed(seed)
          n_w <- 60L
          d0 <- as.Date("2023-01-02")
          mu_w <- 40 * exp(1.3 * sin(2 * pi * seq_len(n_w) / 26)) + 3
          Yw <- stats::rnbinom(n_w, mu = mu_w, size = 1)
          obs <- .one(MOSAIC::downscale_weekly_values(d0 + 7L * (seq_len(n_w) - 1L), Yw)$value)
          cfg <- list(date_start = d0, date_stop = d0 + ncol(obs) - 1L)
          m <- exp(stats::runif(300, log(0.5), log(2)))
          # (week_offset supplied, as run_MOSAIC() supplies it: detecting it on
          # every call costs more than the scoring)
          sc <- t(vapply(m, function(mi) {
               est <- .one(stats::rnbinom(ncol(obs), mu = rep(mi * mu_w / 7, each = 7), size = 2))
               c(daily  = .ll1(obs, est, cfg, k = 1, cases_scoring = "daily"),
                 weekly = .ll1(obs, est, cfg, k = 1, week_offset = 0L))
          }, numeric(2)))
          top <- function(s) order(-s)[1:30]
          lvl_d <- exp(mean(log(m[top(sc[, "daily"])])))
          lvl_w <- exp(mean(log(m[top(sc[, "weekly"])])))
          rho_d <- stats::cor(sc[, "daily"], -abs(log(m)), method = "spearman")
          rho_w <- stats::cor(sc[, "weekly"], -abs(log(m)), method = "spearman")
          c(lvl_d = lvl_d, lvl_ratio = abs(log(lvl_w)) / abs(log(lvl_d)),
            err_ratio = mean(abs(log(m[top(sc[, "weekly"])]))) / mean(abs(log(m[top(sc[, "daily"])]))),
            rho_w = rho_w, rho_diff = rho_w - rho_d,
            sd_ratio = stats::sd(sc[, "daily"]) / stats::sd(sc[, "weekly"]))
     }
     med <- apply(vapply(1:5, crit, numeric(6)), 1, stats::median)
     expect_gt(med[["lvl_d"]], 1.3)              # daily selects over-prediction
     expect_lt(med[["lvl_ratio"]], 1)            # weekly selects a level nearer 1
     expect_lt(med[["err_ratio"]], 0.8)
     expect_gt(med[["rho_w"]], 0.25)             # weekly ranks the draws by level
     expect_gt(med[["rho_diff"]], 0.2)
     # and the daily score carries far more spread from realisation noise
     expect_gt(med[["sd_ratio"]], 10)
})

test_that("config_default weights are constant within each reporting week", {
     # Justifies the weekly weight = mean of the days' weights: the confidence weight
     # is a property of the reporting week, replicated over its days.
     cfg <- MOSAIC::config_default
     b <- MOSAIC:::.mosaic_week_blocks(as.Date(cfg$date_start) + seq_len(ncol(cfg$reported_cases)) - 1L)
     for (W in list(cfg$reported_cases_weight, cfg$reported_deaths_weight)) {
          spread <- apply(W, 1, function(w) {
               r <- tapply(w, b$index, function(x) if (all(is.finite(x))) diff(range(x)) else 0)
               max(r[b$complete])
          })
          expect_true(all(spread == 0))
     }
})

test_that("config_default's daily cases are reporting weeks spread over Monday-Sunday blocks", {
     # A weekly total spread by downscale_weekly_values() leaves each of its days
     # at one of two adjacent integers. Every complete block of the shared helper
     # passes for every location, so the blocks are the reporting weeks; a block
     # one day off would straddle two weeks and fail wherever consecutive weeks
     # differ by more than one case a day.
     cfg <- MOSAIC::config_default
     b <- MOSAIC:::.mosaic_week_blocks(as.Date(cfg$date_start) + seq_len(ncol(cfg$reported_cases)) - 1L)
     # ISO weeks, not weeks counted from date_start: the first block is complete
     # exactly when the window starts on a Monday (2018-01-01 does; Sunday
     # 2023-01-01 left a one-day first block)
     expect_identical(b$complete[1], format(as.Date(cfg$date_start), "%u") == "1")
     spread_ok <- function(v) {
          u <- sort(unique(v)); length(u) <= 1L || (length(u) == 2L && diff(u) == 1)
     }
     n_checked <- 0L
     for (i in seq_along(cfg$location_name)) {
          y <- as.numeric(cfg$reported_cases[i, ])
          full <- which(b$complete & tapply(is.finite(y), b$index, all))
          ok <- vapply(full, function(k) spread_ok(y[b$index == k]), logical(1))
          expect_true(all(ok), label = paste(cfg$location_name[i], "blocks are reporting weeks"))
          n_checked <- n_checked + length(full)
          # and one day later the blocks straddle weeks wherever the level moves
          if (i == match("MOZ", cfg$location_name)) {
               b1 <- MOSAIC:::.mosaic_week_blocks(as.Date(cfg$date_start) + seq_along(y) - 1L, offset = 1L)
               f1 <- which(b1$complete & tapply(is.finite(y), b1$index, all))
               expect_lt(mean(vapply(f1, function(k) spread_ok(y[b1$index == k]), logical(1))), 0.5)
          }
     }
     expect_gt(n_checked, 5000L)
})

test_that("weekly sums of the daily surveillance reproduce the processed weekly totals", {
     # Every complete Monday-Sunday week of the downscaled daily series sums
     # exactly to the processed weekly row dated by that Monday (fractional
     # imputed totals are rounded by the downscale), checked on the processed
     # files, which process_cholera_surveillance_data() writes together, and on
     # config_default for the countries whose daily cases match the current
     # processed daily file (a config built from older surveillance differs where
     # the surveillance was revised since; that is staleness, not alignment).
     local_test_root()
     p <- MOSAIC::get_paths()
     f_w <- file.path(p$DATA_CHOLERA_WEEKLY, "cholera_surveillance_weekly_combined.csv")
     f_d <- file.path(p$DATA_CHOLERA_DAILY, "cholera_surveillance_daily_combined.csv")
     skip_if_not(file.exists(f_w) && file.exists(f_d), "MOSAIC-data processed surveillance not available")
     wk <- data.table::fread(f_w, select = c("iso_code", "date_start", "cases"), data.table = FALSE)
     dy <- data.table::fread(f_d, select = c("iso_code", "date", "cases"), data.table = FALSE)
     wk$date_start <- as.Date(wk$date_start); dy$date <- as.Date(dy$date)
     weekly_of <- function(y, dates) {
          b <- MOSAIC:::.mosaic_week_blocks(dates)
          full <- b$complete & tapply(is.finite(y), b$index, all)
          list(week = b$week_start[full], sum = as.numeric(tapply(y, b$index, sum))[full])
     }
     cand <- c("MOZ", "ETH", "COD", "SOM", "AGO", "MWI", "ZWE", "TZA", "BDI", "KEN")
     for (iso in cand[1:5]) {
          d <- dy[dy$iso_code == iso & dy$date >= as.Date("2023-01-01"), ]
          d <- d[order(d$date), ]
          s <- weekly_of(d$cases, d$date)
          ref <- wk$cases[wk$iso_code == iso][match(s$week, wk$date_start[wk$iso_code == iso])]
          expect_gt(length(s$sum), 50)
          expect_identical(s$sum, round(ref), label = paste(iso, "processed daily vs weekly"))
     }
     cfg <- MOSAIC::config_default
     dates <- as.Date(cfg$date_start) + seq_len(ncol(cfg$reported_cases)) - 1L
     in_sync <- character(0)
     for (iso in cand) {
          y <- as.numeric(cfg$reported_cases[match(iso, cfg$location_name), ])
          d <- dy[dy$iso_code == iso, ]
          ref_d <- d$cases[match(dates, d$date)]
          # in step: every day the config observes is unchanged (weeks appended
          # to the surveillance since the build are not a revision)
          seen <- is.finite(y)
          if (!all(is.finite(ref_d[seen])) || any(y[seen] != ref_d[seen])) next
          in_sync <- c(in_sync, iso)
          s <- weekly_of(y, dates)
          ref <- wk$cases[wk$iso_code == iso][match(s$week, wk$date_start[wk$iso_code == iso])]
          expect_identical(s$sum, round(ref), label = paste(iso, "config_default vs processed weekly"))
     }
     if (!length(in_sync))
          skip("config_default predates the current processed surveillance for every candidate country")
     expect_gte(length(in_sync), 1L)
})
