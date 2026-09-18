# Tests for score_psi_arm(). An untested scorer is the single most dangerous
# component of an autonomous search: a bug here flatters every arm equally and
# the negative control is the only thing that would catch it. Catch it here.
suppressMessages(pkgload::load_all("/Users/johngiles/MOSAIC/MOSAIC-pkg", quiet = TRUE))
library(testthat)
DIR <- "/Users/johngiles/MOSAIC/MOSAIC-pkg/claude/psi_evolve"
source(file.path(DIR, "score_psi_arm.R"))

set.seed(42)
isos  <- c("COD","NGA","MOZ","ETH","ZWE")
dates <- seq(as.Date("2020-01-01"), as.Date("2026-06-30"), by = "week")
obs <- do.call(rbind, lapply(isos, function(i)
  data.frame(iso_code = i, date = dates,
             observed = pmin(0.95, pmax(0.01,
               0.3 + 0.25*sin(2*pi*as.numeric(format(dates,"%j"))/365) + rnorm(length(dates), 0, .05))),
             stringsAsFactors = FALSE)))

folds <- data.frame(fold = 1:6,
  test_start = as.Date(c("2023-01-02","2023-07-03","2024-01-01","2024-07-01","2025-01-06","2025-07-07")))
folds$test_end <- folds$test_start + 83

mk_pred <- function(fn) do.call(rbind, lapply(seq_len(nrow(folds)), function(k) {
  d <- dates[dates >= folds$test_start[k] & dates <= folds$test_end[k]]
  do.call(rbind, lapply(isos, function(i) {
    o <- obs$observed[obs$iso_code == i & obs$date %in% d]
    p <- fn(o)
    data.frame(iso_code = i, date = d, fold = folds$fold[k], psi = p,
               q025 = pmax(0, p - .12), q25 = pmax(0, p - .05),
               q75 = pmin(1, p + .05), q975 = pmin(1, p + .12), stringsAsFactors = FALSE)
  }))
}))

test_that("a near-perfect arm scores high and a pure-noise arm scores low", {
  good <- score_psi_arm("GOOD", mk_pred(function(o) o), obs, folds, "selection", verbose = FALSE)
  bad  <- score_psi_arm("BAD",  mk_pred(function(o) rep(0.5, length(o))), obs, folds, "selection", verbose = FALSE)
  expect_gt(good$S, bad$S)
  expect_gt(good$S, 0)
  expect_equal(good$n_beat, good$n_scored)
})

test_that("mode enforces the fold split and the caller cannot override it", {
  p <- mk_pred(function(o) o)
  sel <- score_psi_arm("S", p, obs, folds, "selection",    verbose = FALSE)
  con <- score_psi_arm("S", p, obs, folds, "confirmation", verbose = FALSE)
  expect_equal(sel$n_folds, 4L)   # 2023-01, 2023-07, 2024-01, 2024-07
  expect_equal(con$n_folds, 2L)   # 2025-01, 2025-07
  expect_true(all(sel$cells$fold %in% 1:4))
  expect_true(all(con$cells$fold %in% 5:6))
})

test_that("countries outside the frozen pool are dropped, not silently scored", {
  p <- mk_pred(function(o) o)
  p$iso_code[p$iso_code == "ZWE"] <- "XXX"
  r <- score_psi_arm("POOL", p, obs, folds, "selection", verbose = FALSE)
  expect_false("XXX" %in% r$per_iso$iso_code)
  expect_equal(r$n_scored, 4L)
})

test_that("the no-regression guard fires on a top-10 country", {
  p <- mk_pred(function(o) o)
  r0 <- score_psi_arm("BASE", p, obs, folds, "selection", verbose = FALSE)
  inc <- setNames(r0$per_iso$wis_skill, r0$per_iso$iso_code)
  inc["COD"] <- inc["COD"] + 0.50           # incumbent was much better on COD
  r1 <- score_psi_arm("ARM", p, obs, folds, "selection", incumbent = inc, verbose = FALSE)
  expect_lt(r1$top10_worst, -0.02)
  expect_false(r1$guard_ok)
})

test_that("a tampered weights file is refused", {
  tmp <- tempfile("pe"); dir.create(tmp)
  w <- read.csv(file.path(DIR, "weights_frozen.csv")); w$w_sqrt[1] <- w$w_sqrt[1] * 2
  write.csv(w, file.path(tmp, "weights_frozen.csv"), row.names = FALSE)
  expect_error(score_psi_arm("T", mk_pred(function(o) o), obs, folds, "selection", dir = tmp),
               "FROZEN OBJECTIVE VIOLATION")
})

test_that("S excluding NGA is reported and differs when NGA is an outlier", {
  p <- mk_pred(function(o) o)
  p$psi[p$iso_code == "NGA"] <- 0.5        # make NGA uninformative only
  r <- score_psi_arm("NGA", p, obs, folds, "selection", verbose = FALSE)
  expect_true(is.finite(r$S_exNGA))
  expect_gt(r$S_exNGA, r$S)
})
