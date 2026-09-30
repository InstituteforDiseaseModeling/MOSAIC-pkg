# est_epidemic_peaks() detects peaks on observed surveillance weeks only.

make_peaks_fixture <- function(cases, method) {
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  dates <- seq(as.Date("2015-01-05"), by = "day", length.out = length(cases))
  daily <- data.frame(country = "Botswana", iso_code = "BWA", date = as.character(dates),
                      cases = cases, deaths = 0, source = ifelse(is.na(method), "WHO", "AI"),
                      disaggregation_method = method,
                      confidence_weight = ifelse(is.na(method), 1, 0.5),
                      stringsAsFactors = FALSE)
  dir.create(file.path(tmp, "daily")); dir.create(file.path(tmp, "input"))
  utils::write.csv(daily, file.path(tmp, "daily", "cholera_surveillance_daily_combined.csv"),
                   row.names = FALSE)
  list(DATA_CHOLERA_DAILY = file.path(tmp, "daily"), MODEL_INPUT = file.path(tmp, "input"))
}
# est_epidemic_peaks() appends hand-curated peaks for other countries whenever
# any peak is detected, so assertions look at the fixture country only (BWA has
# no hand-curated peaks).
bwa_peaks <- function(P) { p <- suppressMessages(est_epidemic_peaks(P)); p[p$iso_code == "BWA", ] }
bump <- function(n, centre, height, width) round(height * exp(-((seq_len(n) - centre) / width)^2))

test_that("peaks on Fourier-reconstructed weeks are not detected", {
  n <- 730
  obs  <- bump(n, 150, 200, 20)              # observed outbreak in year 1
  four <- bump(n, 550, 120, 30)              # annual Fourier bump in year 2
  method <- ifelse(seq_len(n) > 365, "fourier_country_k1", NA)
  P <- make_peaks_fixture(ifelse(is.na(method), obs, four), method)
  peaks <- bwa_peaks(P)
  expect_equal(nrow(peaks), 1L)
  expect_true(as.Date(peaks$peak_date) < as.Date("2016-01-01"))
  # Without the method tag the same year-2 bump is detected, so the fixture is live.
  P2 <- make_peaks_fixture(ifelse(is.na(method), obs, four), rep(NA_character_, n))
  expect_equal(nrow(bwa_peaks(P2)), 2L)
})

test_that("a detected peak whose window is at least half imputed days is dropped", {
  n <- 400
  cases <- bump(n, 200, 90, 25)
  method <- rep("fourier_country_k2", n)
  method[196:204] <- "observed"              # a 9-day observed stretch inside imputed weeks
  cases[196:204] <- 400
  P <- make_peaks_fixture(cases, method)
  expect_message(peaks <- est_epidemic_peaks(P), "at least half imputed")
  expect_equal(sum(peaks$iso_code == "BWA"), 0L)
  # The same observed stretch amid observed zeros is kept.
  cases0 <- rep(0, n); cases0[196:204] <- 400
  P2 <- make_peaks_fixture(cases0, rep(NA_character_, n))
  expect_equal(nrow(bwa_peaks(P2)), 1L)
})

test_that("a flagged plateau day moves to the centre of its flat stretch", {
  f <- MOSAIC:::.epidemic_peaks_centre_plateaus
  sm <- c(0, 1, 5, 5, 5, 5, 5, 2, 0)
  expect_equal(which(f(sm, seq_along(sm) == 7)), 5L)   # last plateau day -> centre
  expect_equal(which(f(c(0, 3, 1), c(FALSE, TRUE, FALSE))), 2L)  # lone max unchanged
})

test_that("an isolated observed week peaks inside that week, not in the zeros after it", {
  # One week of 70 cases/day amid missing weeks: the 28-day smoother is flat for
  # ~22 days, and the old last-plateau-day rule put the peak ~11 days after the
  # observed week, on a day with 0 cases.
  n <- 300
  cases <- rep(NA_real_, n); cases[148:154] <- 70
  P <- make_peaks_fixture(cases, rep(NA_character_, n))
  pk <- bwa_peaks(P)
  expect_equal(nrow(pk), 1L)
  day <- as.integer(as.Date(pk$peak_date) - as.Date("2015-01-05")) + 1L
  expect_true(day >= 148 && day <= 154)
  expect_gt(pk$reported_cases, 0)
})
