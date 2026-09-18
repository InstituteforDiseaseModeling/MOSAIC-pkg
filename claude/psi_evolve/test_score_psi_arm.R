# Tests for score_psi_arm(). An untested scorer is the single most dangerous
# component of an autonomous search: a bug here flatters every arm equally and
# the negative control is the only thing that would catch it. Catch it here.
#
# v3: the scorer is GRID-BOUND -- it reads EVAL_GRID.csv, derives the
# selection/confirmation split from it, and refuses a `folds` frame whose
# geometry disagrees. The fixture therefore uses the real frozen grid rather than
# invented dates, which is also a test that the shipped grid is scoreable.
suppressMessages(pkgload::load_all("/Users/johngiles/MOSAIC/MOSAIC-pkg", quiet = TRUE))
library(testthat)
DIR <- "/Users/johngiles/MOSAIC/MOSAIC-pkg/claude/psi_evolve"
source(file.path(DIR, "score_psi_arm.R"))

G <- .pe_grid(DIR)
folds <- data.frame(fold = G$block, train_end = G$cutoff,
                    test_start = G$test_start, test_end = G$test_end)
N_SEL <- sum(G$split == "selection"); N_CON <- sum(G$split == "confirmation")

set.seed(42)
isos  <- c("COD","NGA","MOZ","ETH","ZWE")
# Observed must span >= 2 years before the first cutoff so the `seasonal`
# baseline (week-of-year climatology, >= 730 d of IS history) is computable, and
# far enough past the last block end for every cell to have observations.
dates <- seq(as.Date("2018-01-07"), as.Date("2026-06-28"), by = "week")
obs <- do.call(rbind, lapply(isos, function(i)
  data.frame(iso_code = i, date = dates,
             observed = pmin(0.95, pmax(0.01,
               0.3 + 0.25*sin(2*pi*as.numeric(format(dates,"%j"))/365) + rnorm(length(dates), 0, .05))),
             stringsAsFactors = FALSE)))

# Predictions over a block, plus the pre-cutoff history residual mode needs.
mk_pred <- function(fn, pre = FALSE) do.call(rbind, lapply(seq_len(nrow(folds)), function(k) {
  d <- if (pre) dates[dates <= folds$train_end[k]]
       else      dates[dates >= folds$test_start[k] & dates <= folds$test_end[k]]
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

test_that("mode enforces the split FROM THE GRID and the caller cannot override it", {
  p <- mk_pred(function(o) o)
  sel <- score_psi_arm("S", p, obs, folds, "selection",    verbose = FALSE)
  con <- score_psi_arm("S", p, obs, folds, "confirmation", verbose = FALSE)
  expect_equal(sel$n_folds, N_SEL)
  expect_equal(con$n_folds, N_CON)
  expect_true(all(sel$cells$fold %in% G$block[G$split == "selection"]))
  expect_true(all(con$cells$fold %in% G$block[G$split == "confirmation"]))
  expect_equal(sel$objective_version, 3L)
})

test_that("a folds frame that disagrees with the frozen grid is REFUSED", {
  # v3 guard: at v2 the geometry was never checked against the grid, so a driver
  # that mis-built `folds` would have produced a clean-looking score on the wrong
  # blocks -- the same silent-wrong-number class as the wave-4 stride bug.
  p  <- mk_pred(function(o) o)
  f2 <- folds; f2$test_end <- f2$test_end + 30L
  expect_error(score_psi_arm("BAD", p, obs, f2, "selection", verbose = FALSE),
               "does not match the frozen grid")
  f3 <- folds; f3$train_end <- f3$train_end - 7L
  expect_error(score_psi_arm("BAD", p, obs, f3, "selection", verbose = FALSE),
               "does not match the frozen grid")
  f4 <- folds; f4$fold[1] <- 99L
  expect_error(score_psi_arm("BAD", p, obs, f4, "selection", verbose = FALSE),
               "not in the frozen grid")
})

test_that("an objective_version mismatch is refused", {
  tmp <- file.path(tempdir(), "obj_v_mismatch"); dir.create(tmp, showWarnings = FALSE)
  file.copy(file.path(DIR, c("weights_frozen.csv", "EVAL_GRID.csv")), tmp, overwrite = TRUE)
  writeLines(c("# fake", "objective_version: 99"), file.path(tmp, "OBJECTIVE.md"))
  expect_error(score_psi_arm("V", mk_pred(function(o) o), obs, folds, "selection",
                             dir = tmp, verbose = FALSE),
               "objective mismatch")
})

test_that("countries outside the frozen pool are dropped, not silently scored", {
  p <- mk_pred(function(o) o)
  p$iso_code[p$iso_code == "ZWE"] <- "XXX"
  r <- score_psi_arm("POOL", p, obs, folds, "selection", verbose = FALSE)
  expect_false("XXX" %in% r$per_iso$iso_code)
  expect_equal(r$n_scored, 4L)
})

test_that("the no-regression guard fires on a top-10 country", {
  r0 <- score_psi_arm("A", mk_pred(function(o) o), obs, folds, "selection", verbose = FALSE)
  inc <- stats::setNames(r0$per_iso$wis_skill, r0$per_iso$iso_code)
  inc["MOZ"] <- inc["MOZ"] + 0.5          # incumbent was much better in MOZ
  # The 5-country fixture leaves 5 top-10 countries unscored, which correctly
  # raises the D3 "guard cannot be evaluated" warning; this test is about the
  # MOZ regression, so the warning is expected and suppressed here.
  r1 <- suppressWarnings(score_psi_arm("B", mk_pred(function(o) o), obs, folds,
                                       "selection", incumbent = inc, verbose = FALSE))
  expect_false(isTRUE(r1$guard_ok))
  expect_lt(r1$top10_worst, -0.02)
})

test_that("the no-regression guard FAILS when top-10 countries are unscored", {
  # D3: a missing top-10 country used to make min(NA, na.rm=TRUE) = Inf, and
  # Inf >= -0.02 is TRUE -- i.e. an unevaluable guard PASSED.
  r0 <- score_psi_arm("A", mk_pred(function(o) o), obs, folds, "selection", verbose = FALSE)
  inc <- stats::setNames(r0$per_iso$wis_skill, r0$per_iso$iso_code)
  expect_warning(
    r1 <- score_psi_arm("B", mk_pred(function(o) o), obs, folds, "selection",
                        incumbent = inc, verbose = FALSE),
    "guard CANNOT be evaluated")
  expect_false(isTRUE(r1$guard_ok))
  expect_match(r1$guard_note, "unscored")
})

test_that("a tampered weights file is refused", {
  tmp <- file.path(tempdir(), "tampered"); dir.create(tmp, showWarnings = FALSE)
  file.copy(file.path(DIR, c("OBJECTIVE.md", "EVAL_GRID.csv")), tmp, overwrite = TRUE)
  w <- utils::read.csv(file.path(DIR, "weights_frozen.csv")); w$w_sqrt[1] <- 0.99
  utils::write.csv(w, file.path(tmp, "weights_frozen.csv"), row.names = FALSE)
  expect_error(score_psi_arm("T", mk_pred(function(o) o), obs, folds, "selection",
                             dir = tmp, verbose = FALSE),
               "FROZEN OBJECTIVE VIOLATION")
})

test_that("S excluding NGA is reported and differs when NGA is an outlier", {
  p <- mk_pred(function(o) o)
  p$psi[p$iso_code == "NGA"] <- 0.5
  r <- score_psi_arm("N", p, obs, folds, "selection", verbose = FALSE)
  expect_true(is.finite(r$S_exNGA))
  expect_gt(r$S_exNGA, r$S)
})

test_that("a degenerate (zero-width) interval set is refused, not scored", {
  p <- mk_pred(function(o) o)
  p$q025 <- p$psi; p$q25 <- p$psi; p$q75 <- p$psi; p$q975 <- p$psi
  expect_error(score_psi_arm("Z", p, obs, folds, "selection", verbose = FALSE),
               "ZERO-WIDTH")
})

test_that("a small fraction of zero-width rows warns but still scores", {
  p <- mk_pred(function(o) o)
  k <- sample(nrow(p), ceiling(0.03 * nrow(p)))
  p$q025[k] <- p$psi[k]; p$q975[k] <- p$psi[k]; p$q25[k] <- p$psi[k]; p$q75[k] <- p$psi[k]
  expect_warning(r <- score_psi_arm("W", p, obs, folds, "selection", verbose = FALSE),
                 "zero-width")
  expect_true(is.finite(r$S))
})

test_that("a `folds` frame without train_end is refused, not silently emptied", {
  p <- mk_pred(function(o) o)
  expect_error(score_psi_arm("NOTE", p, obs, folds[, c("fold","test_start","test_end")],
                             "selection", verbose = FALSE),
               "missing required column")
})

test_that("residual interval mode scores, and pred_pre widens its estimation sample", {
  # D2: residual intervals were estimated from `pred`, which the driver has
  # already truncated to the blocks -- so the sample was the block itself and the
  # earliest block was dropped entirely. pred_pre restores the pre-cutoff history.
  p  <- mk_pred(function(o) o)
  pp <- mk_pred(function(o) o, pre = TRUE)
  r_no <- score_psi_arm("R0", p, obs, folds, "selection", interval_mode = "residual",
                        verbose = FALSE)
  r_pp <- score_psi_arm("R1", p, obs, folds, "selection", interval_mode = "residual",
                        pred_pre = pp, verbose = FALSE)
  expect_true(is.finite(r_no$S) && is.finite(r_pp$S))
  expect_gt(r_pp$n_cells, r_no$n_cells)
  seed_cells <- score_psi_arm("R2", p, obs, folds, "selection", verbose = FALSE)$n_cells
  expect_equal(r_pp$n_cells, seed_cells)
})

test_that("pred_pre is validated rather than silently ignored", {
  p  <- mk_pred(function(o) o)
  pp <- mk_pred(function(o) o, pre = TRUE); pp$psi <- NULL
  expect_error(score_psi_arm("R", p, obs, folds, "selection", interval_mode = "residual",
                             pred_pre = pp, verbose = FALSE),
               "needs iso_code, date, fold")
})

test_that("all three baselines are scored on identical cells, and A6 is reported", {
  r <- score_psi_arm("B", mk_pred(function(o) o), obs, folds, "selection", verbose = FALSE)
  expect_named(r$S_by_baseline, c("persistence", "seasonal", "persistence_last"))
  expect_equal(r$S, r$S_by_baseline$persistence$S)     # the objective IS persistence
  expect_true(is.finite(r$S_by_baseline$seasonal$S))   # >= 2 y of IS history exists
  expect_type(r$beats_seasonal, "logical")
  # A near-perfect arm must beat a week-of-year climatology.
  expect_true(r$beats_seasonal)
  # every cell carries every baseline
  for (bn in c("persistence", "seasonal", "persistence_last"))
    expect_true(paste0("skill_", bn) %in% names(r$cells))
})

test_that("the per-horizon decomposition is on the same cells and S uses only the pooled ones", {
  r <- score_psi_arm("H", mk_pred(function(o) o), obs, folds, "selection", verbose = FALSE)
  expect_named(r$per_horizon, c("h1mo", "h2mo", "h3mo"))
  expect_setequal(unique(r$cells$horizon), c("all", "h1mo", "h2mo", "h3mo"))
  # n_cells counts the POOLED cells only -- not 4x them.
  expect_equal(r$n_cells, sum(r$cells$horizon == "all"))
  # each bucket's rows partition the pooled cell's rows
  agg <- stats::aggregate(n_days ~ iso_code + fold,
                          r$cells[r$cells$horizon != "all", ], sum)
  pooled <- r$cells[r$cells$horizon == "all", c("iso_code", "fold", "n_days")]
  j <- merge(agg, pooled, by = c("iso_code", "fold"), suffixes = c("_buckets", "_pooled"))
  expect_equal(j$n_days_buckets, j$n_days_pooled)
})

test_that("a skill claim about a horizon bucket cannot be read off the pooled S", {
  # Wave 17: the estimand is median-then-weight, so it is NOT additive across
  # subsets -- a uniform within-subset win pooled to a loss and produced a
  # retracted headline. Assert the non-additivity explicitly so nobody assumes it.
  r <- score_psi_arm("H", mk_pred(function(o) o), obs, folds, "selection", verbose = FALSE)
  bucket_mean <- mean(vapply(r$per_horizon, function(z) z$S, numeric(1)))
  expect_false(isTRUE(all.equal(r$S, bucket_mean, tolerance = 1e-9)))
})
