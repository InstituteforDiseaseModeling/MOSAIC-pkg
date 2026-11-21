# Tests for est_outbreak_onsets(): weekly series from a config, imputed-day handling, population
# lookup, and equality with calc_outbreak_onset() on the same weekly series.

.onset_config <- function() {
     dates <- seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day")   # starts on a Wednesday
     nd <- length(dates)
     day <- seq_len(nd)
     a <- round(2 + 120 * exp(-((day - 500) / 45)^2) + 60 * exp(-((day - 900) / 40)^2))
     tier_a <- rep(1L, nd)
     # the second wave is a reconstruction: tier 3 on whole Monday-Sunday weeks
     mon <- which(format(dates, "%u") == "1")
     imp_days <- unlist(lapply(mon[mon >= 790 & mon <= 1010], function(i) i:(i + 6)))   # 2022-02-28 to 2022-10-09
     tier_a[imp_days] <- 3L
     b <- rep(0, nd)
     b[1:200] <- NA                                                  # unreported start
     list(date_start = dates[1], date_stop = dates[nd], location_name = c("AAA", "BBB"),
          reported_cases = rbind(a, b), reported_tier = rbind(tier_a, ifelse(is.na(b), NA, 1L)))
}
.onset_pop <- data.frame(iso_code = c("AAA", "AAA", "BBB"),
                         date = as.Date(c("2019-01-01", "2021-06-07", "2019-01-01")),
                         population = c(1e6, 2e6, 5e5))

test_that("weeks are the complete Monday-Sunday weeks and sum their days", {
     cfg <- .onset_config()
     x <- est_outbreak_onsets(cfg, population = .onset_pop, imputed = "keep")
     s <- attr(x, "series")
     sa <- s[s$iso_code == "AAA", ]
     expect_equal(sa$week_start[1], as.Date("2020-01-06"))         # the partial first week is dropped
     expect_true(all(format(sa$week_start, "%u") == "1"))
     dates <- seq(as.Date(cfg$date_start), as.Date(cfg$date_stop), by = "day")
     i <- match(sa$week_start[10], dates)
     expect_equal(sa$cases[10], sum(cfg$reported_cases[1, i:(i + 6)]))
     sb <- s[s$iso_code == "BBB", ]
     expect_false(any(sb$observed[sb$week_start < as.Date("2020-07-20")]))   # day 200 is 2020-07-18
     expect_true(all(sb$observed[sb$week_start >= as.Date("2020-07-20")]))
})

test_that("imputed days are unreported by default and counted with imputed = 'keep'", {
     cfg <- .onset_config()
     un <- est_outbreak_onsets(cfg, population = .onset_pop, locations = "AAA")
     kp <- est_outbreak_onsets(cfg, population = .onset_pop, locations = "AAA", imputed = "keep")
     su <- attr(un, "series"); sk <- attr(kp, "series")
     imp_weeks <- su$week_start >= as.Date("2022-02-28") & su$week_start <= as.Date("2022-10-03")
     expect_false(any(su$observed[imp_weeks]))
     expect_true(all(sk$observed[imp_weeks]))
     # with the reconstruction kept, its wave is detected and flagged as imputed
     expect_gt(nrow(kp), nrow(un))
     expect_false(any(un$onset_week > as.Date("2022-01-01")))
     late <- kp$onset_week > as.Date("2022-01-01")
     expect_true(any(late))
     expect_true(all(kp$imputed_share[late] > 0.5))
     expect_true(all(kp$imputed_share[!late] == 0 | is.na(kp$imputed_share[!late])))
})

test_that("each location's result equals calc_outbreak_onset() on its weekly series", {
     cfg <- .onset_config()
     for (m in c("growth", "growth_sensitive", "zheng")) {
          x <- est_outbreak_onsets(cfg, method = m, population = .onset_pop, imputed = "keep")
          s <- attr(x, "series")
          for (iso in c("AAA", "BBB")) {
               si <- s[s$iso_code == iso, ]
               pop <- if (iso == "BBB") 5e5 else ifelse(si$week_start >= as.Date("2021-06-07"), 2e6, 1e6)
               direct <- calc_outbreak_onset(si$cases, si$week_start, pop, observed = si$observed, method = m)
               got <- x[x$iso_code == iso, setdiff(names(x), c("iso_code", "imputed_share"))]
               rownames(got) <- NULL
               expect_equal(got, direct, ignore_attr = TRUE, info = paste(m, iso))
          }
     }
})

test_that("population is the latest value on or before each week, else the earliest", {
     wk <- as.Date(c("2018-12-31", "2019-01-07", "2021-06-07", "2021-06-14"))
     p <- MOSAIC:::.onset_population(.onset_pop, c("AAA", "BBB"), wk)
     expect_equal(p$AAA, c(1e6, 1e6, 2e6, 2e6))
     expect_equal(p$BBB, rep(5e5, 4))
     expect_error(MOSAIC:::.onset_population(.onset_pop, "CCC", wk), "No population for CCC")
     expect_error(MOSAIC:::.onset_population(data.frame(iso = "AAA"), "AAA", wk), "data frame")
})

test_that("real time, rule settings and the imputed share pass through the wrapper", {
     cfg <- .onset_config()
     rt <- est_outbreak_onsets(cfg, mode = "realtime", population = .onset_pop, locations = "AAA", imputed = "keep")
     s <- attr(rt, "series")
     direct <- calc_outbreak_onset(s$cases, s$week_start, ifelse(s$week_start >= as.Date("2021-06-07"), 2e6, 1e6),
                                   observed = s$observed, mode = "realtime")
     expect_equal(rt[, setdiff(names(rt), c("iso_code", "imputed_share"))], direct, ignore_attr = TRUE)
     expect_true(all(!is.na(rt$detection_week)))
     # a setting passed through ... reaches calc_outbreak_onset()
     strict <- est_outbreak_onsets(cfg, population = .onset_pop, locations = "AAA", imputed = "keep",
                                   growth_min = 5)
     expect_equal(nrow(strict), 0L)
     # imputed_share is the imputed share of the config's cases in the onset week +/- 2 weeks
     kp <- est_outbreak_onsets(cfg, population = .onset_pop, locations = "AAA", imputed = "keep")
     dates <- seq(as.Date(cfg$date_start), as.Date(cfg$date_stop), by = "day")
     i <- which(kp$onset_week > as.Date("2022-01-01"))[1]
     win <- dates >= kp$onset_week[i] - 14 & dates <= kp$onset_week[i] + 20
     y <- cfg$reported_cases[1, win]
     expect_equal(kp$imputed_share[i], sum(y[cfg$reported_tier[1, win] == 3L]) / sum(y))
})

test_that("an empty location set is rejected and duplicates run once", {
     cfg <- .onset_config()
     expect_error(est_outbreak_onsets(cfg, population = .onset_pop, locations = character(0)), "empty")
     one <- est_outbreak_onsets(cfg, population = .onset_pop, locations = "AAA")
     two <- est_outbreak_onsets(cfg, population = .onset_pop, locations = c("AAA", "AAA"))
     expect_equal(two, one)
})

test_that("malformed configs and unknown locations are rejected", {
     cfg <- .onset_config()
     expect_error(est_outbreak_onsets(cfg[c("date_start", "location_name")], population = .onset_pop),
                  "missing: date_stop, reported_cases")
     expect_error(est_outbreak_onsets(cfg, population = .onset_pop, locations = "ZZZ"), "not in the config")
     bad <- cfg; bad$reported_cases <- bad$reported_cases[, -1]
     expect_error(est_outbreak_onsets(bad, population = .onset_pop), "locations x days")
     bad <- cfg; bad$reported_tier <- bad$reported_tier[1, , drop = FALSE]
     expect_error(est_outbreak_onsets(bad, population = .onset_pop), "reported_tier")
})

test_that("a config without reported_tier treats every reported day as observed", {
     cfg <- .onset_config()
     cfg$reported_tier <- NULL
     a <- est_outbreak_onsets(cfg, population = .onset_pop, locations = "AAA")
     b <- est_outbreak_onsets(.onset_config(), population = .onset_pop, locations = "AAA", imputed = "keep")
     expect_equal(a[, names(a) != "imputed_share"], b[, names(b) != "imputed_share"], ignore_attr = TRUE)
     expect_true(all(a$imputed_share == 0 | is.na(a$imputed_share)))
})
