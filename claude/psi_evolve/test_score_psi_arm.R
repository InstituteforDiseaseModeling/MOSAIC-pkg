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
# train_end is the model's information cut-off; the baseline is anchored on it
# (D1). The real driver supplies it as the evaluation grid's cutoff = test_start
# minus the 14-day embargo.
folds$train_end <- folds$test_start - 14

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

test_that("a degenerate (zero-width) interval set is refused, not scored", {
  # A single-seed fit gives q025 == q975 on every row. WIS would then charge the
  # model the full interval penalty against a baseline that has real intervals.
  p <- mk_pred(function(o) o)
  p$q025 <- p$psi; p$q25 <- p$psi; p$q75 <- p$psi; p$q975 <- p$psi
  expect_error(score_psi_arm("DEGEN", p, obs, folds, "selection", verbose = FALSE),
               "ZERO-WIDTH")
})

test_that("a small fraction of zero-width rows warns but still scores", {
  p <- mk_pred(function(o) o)
  i <- seq_len(round(0.05 * nrow(p)))
  p$q025[i] <- p$psi[i]; p$q975[i] <- p$psi[i]
  expect_warning(r <- score_psi_arm("W", p, obs, folds, "selection", verbose = FALSE),
                 "zero-width")
  expect_true(is.finite(r$S))
})

test_that("a `folds` frame without train_end is refused, not silently emptied", {
  # Before D1 the baseline was cut at test_start and train_end was unused, so a
  # folds frame lacking it scored fine. Now it is load-bearing: absent, every
  # is_df is empty and the scorer would report "no scoreable cells", which reads
  # as a data problem rather than a malformed argument.
  bad <- folds[, c("fold", "test_start", "test_end")]
  expect_error(score_psi_arm("NF", mk_pred(function(o) o), obs, bad, "selection"),
               "missing required column")
})

test_that("the no-regression guard FAILS when top-10 countries are unscored", {
  # D3: previously a top-10 country absent from the arm or from `incumbent` was
  # silently exempt, and an all-missing set returned min(NA, na.rm=TRUE) = Inf,
  # which passed the >= -0.02 test. The pool here has 5 of the 10 top-10
  # countries, so the guard cannot be evaluated and must report FAIL.
  p  <- mk_pred(function(o) o)
  r0 <- score_psi_arm("B", p, obs, folds, "selection", verbose = FALSE)
  inc <- setNames(r0$per_iso$wis_skill, r0$per_iso$iso_code)
  expect_warning(r <- score_psi_arm("A", p, obs, folds, "selection",
                                    incumbent = inc, verbose = FALSE),
                 "guard CANNOT be evaluated")
  expect_false(r$guard_ok)
  expect_match(r$guard_note, "unscored")
})

test_that("residual interval mode scores, and pred_pre widens its estimation sample", {
  # The review found ZERO tests touched interval_mode = "residual" -- the path that
  # produced B-CAL's +0.117, the NC2 finding, and every residual-mode number in the
  # ledger. It also contained D2: without `pred_pre` the residual sample is the
  # block itself, which is empty for the first fold, silently dropping it.
  p <- mk_pred(function(o) o)
  r_seed <- score_psi_arm("S", p, obs, folds, "selection", verbose = FALSE,
                          interval_mode = "seed")
  r_res  <- score_psi_arm("R", p, obs, folds, "selection", verbose = FALSE,
                          interval_mode = "residual")
  expect_true(is.finite(r_res$S))
  expect_identical(r_res$interval_mode, "residual")
  # without pred_pre the first fold cannot form residuals -> fewer cells than seed
  expect_lt(r_res$n_cells, r_seed$n_cells)

  # with pred_pre (pre-block history per fold) every fold is scoreable again
  pre <- do.call(rbind, lapply(seq_len(nrow(folds)), function(k) {
    d <- dates[dates <= folds$train_end[k]]
    do.call(rbind, lapply(isos, function(i) data.frame(
      iso_code = i, date = d, fold = folds$fold[k],
      psi = obs$observed[obs$iso_code == i & obs$date %in% d],
      stringsAsFactors = FALSE)))
  }))
  r_pp <- score_psi_arm("P", p, obs, folds, "selection", verbose = FALSE,
                        interval_mode = "residual", pred_pre = pre)
  expect_equal(r_pp$n_cells, r_seed$n_cells)
})

test_that("pred_pre is validated rather than silently ignored", {
  p <- mk_pred(function(o) o)
  expect_error(score_psi_arm("V", p, obs, folds, "selection", interval_mode = "residual",
                             pred_pre = data.frame(iso_code = "COD", date = Sys.Date())),
               "needs iso_code, date, fold")
})
