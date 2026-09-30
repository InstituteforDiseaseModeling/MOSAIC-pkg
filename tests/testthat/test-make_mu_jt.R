# =============================================================================
# test-make_mu_jt.R
#
# make_mu_jt() expands per-location, per-year reported CFR estimates into the
# daily [location x day] matrix the engine reads as config$mu_jt: linear on the
# logit scale between 1 July anchors, flat before the first and after the last
# estimated year.
# =============================================================================

.est <- data.frame(iso_code = rep(c("AAA", "BBB"), each = 3),
                   year = rep(2023:2025, 2),
                   cfr_estimate = c(0.02, 0.03, 0.025, 0.01, 0.01, 0.012))

test_that("values are exact at 1 July and logit-linear in between", {
  mu <- make_mu_jt(.est, c("AAA", "BBB"), "2023-01-01", "2025-12-31")
  d <- seq(as.Date("2023-01-01"), as.Date("2025-12-31"), by = "day")
  expect_identical(dim(mu), c(2L, length(d)))
  j1 <- match(as.Date(c("2023-07-01", "2024-07-01", "2025-07-01")), d)
  expect_equal(mu[1, j1], c(0.02, 0.03, 0.025), tolerance = 1e-12)
  expect_equal(mu[2, j1], c(0.01, 0.01, 0.012), tolerance = 1e-12)
  # Midpoint of the 2023-07-01 -> 2024-07-01 segment on the day axis.
  a <- as.numeric(as.Date("2023-07-01")); b <- as.numeric(as.Date("2024-07-01"))
  jm <- match(as.Date("2023-12-31"), d); w <- (as.numeric(d[jm]) - a) / (b - a)
  expect_equal(qlogis(mu[1, jm]), (1 - w) * qlogis(0.02) + w * qlogis(0.03), tolerance = 1e-12)
})

test_that("values are held flat before the first and after the last anchor", {
  mu <- make_mu_jt(.est, c("AAA", "BBB"), "2022-01-01", "2027-06-30")
  d <- seq(as.Date("2022-01-01"), as.Date("2027-06-30"), by = "day")
  expect_true(all(abs(mu[1, d <= as.Date("2023-07-01")] - 0.02) < 1e-12))
  expect_true(all(abs(mu[1, d >= as.Date("2025-07-01")] - 0.025) < 1e-12))
})

test_that("step interpolation uses each calendar year's value", {
  mu <- make_mu_jt(.est, "AAA", "2023-01-01", "2026-12-31", interpolation = "step")
  d <- seq(as.Date("2023-01-01"), as.Date("2026-12-31"), by = "day")
  yr <- as.integer(format(d, "%Y"))
  expect_equal(as.numeric(mu[1, ]), c(`2023` = 0.02, `2024` = 0.03, `2025` = 0.025, `2026` = 0.025)[as.character(yr)],
               tolerance = 1e-12, ignore_attr = TRUE)
})

test_that("logit_mean input is used in preference to cfr_estimate and rows follow location order", {
  est <- .est; est$logit_mean <- qlogis(est$cfr_estimate * 2); est$cfr_estimate <- NULL
  mu <- make_mu_jt(est, c("BBB", "AAA"), "2024-07-01", "2024-07-01")
  expect_equal(as.numeric(mu), c(0.02, 0.06), tolerance = 1e-12)
})

test_that("a single estimated year gives a constant row", {
  est <- data.frame(iso_code = "AAA", year = 2024L, cfr_estimate = 0.017)
  mu <- make_mu_jt(est, "AAA", "2023-01-01", "2025-12-31")
  expect_true(all(abs(mu - 0.017) < 1e-12))
})

test_that("bad inputs are refused", {
  expect_error(make_mu_jt(.est, "ZZZ", "2023-01-01", "2023-12-31"), "no rows for: ZZZ")
  expect_error(make_mu_jt(rbind(.est, .est[1, ]), "AAA", "2023-01-01", "2023-12-31"), "duplicate years")
  bad <- .est; bad$cfr_estimate[1] <- 0
  expect_error(make_mu_jt(bad, "AAA", "2023-01-01", "2023-12-31"), "in \\(0, 1\\)")
  expect_error(make_mu_jt(.est, "AAA", "2023-12-31", "2023-01-01"), "date_stop >= date_start")
  expect_error(make_mu_jt(.est[, c("iso_code", "year")], "AAA", "2023-01-01", "2023-12-31"),
               "logit_mean or a cfr_estimate")
})
