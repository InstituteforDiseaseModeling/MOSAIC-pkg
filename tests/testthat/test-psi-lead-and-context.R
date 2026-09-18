# HA-01 part 2: forecast lead + validation-window input context.
#
# Two coupled changes. (1) `.psi_build_sequences(lead = h)` anchors the target h
# weeks AFTER the input window instead of at its end, so the model is trained to
# forecast rather than to nowcast. (2) `.psi_slice_rw_step()` widens the
# validation slice backwards by (timesteps - 1 + lead) weeks of input context and
# then keeps only sequences whose TARGET lies inside the block -- without which an
# 84-day (12-week) block cannot build a single 13-timestep sequence.

mk <- function(n_weeks = 200L, isos = c("AAA", "BBB")) {
     d <- seq(as.Date("2020-01-06"), by = "week", length.out = n_weeks)
     do.call(rbind, lapply(isos, function(i)
          data.frame(iso_code = i, date = d,
                     y = as.numeric(seq_along(d)) / 1000,
                     stringsAsFactors = FALSE)))
}

test_that("lead = 0 is bit-identical to the historical concurrent mapping", {
     p  <- mk()
     X  <- matrix(p$y, ncol = 1)
     s0 <- MOSAIC:::.psi_build_sequences(X, p$y, p$iso_code, p$date, timesteps = 13L)
     s0b <- MOSAIC:::.psi_build_sequences(X, p$y, p$iso_code, p$date, timesteps = 13L,
                                          lead = 0L)
     expect_identical(s0$y, s0b$y)
     expect_identical(s0$dates, s0b$dates)
     expect_identical(dim(s0$X), dim(s0b$X))
     # target is the LAST timestep of its own window
     expect_equal(s0$y[1], p$y[13])
     expect_equal(s0$dates[1], p$date[13])
})

test_that("lead = 12 anchors the target 12 weeks after the input window ends", {
     p <- mk()
     X <- matrix(p$y, ncol = 1)
     s <- MOSAIC:::.psi_build_sequences(X, p$y, p$iso_code, p$date, timesteps = 13L,
                                        lead = 12L)
     # first window covers rows 1..13; its target is row 13 + 12 = 25
     expect_equal(s$y[1], p$y[25])
     expect_equal(s$dates[1], p$date[25])
     # 12 fewer sequences per country than the concurrent mapping
     s0 <- MOSAIC:::.psi_build_sequences(X, p$y, p$iso_code, p$date, timesteps = 13L)
     expect_equal(length(s0$y) - length(s$y), 12L * 2L)
})

test_that("an 84-day validation block builds ZERO sequences without context, and works with it", {
     p  <- mk()
     X  <- matrix(p$y, ncol = 1)
     ts <- as.Date("2022-01-03"); te <- ts + 83      # 84 days = 12 weekly rows
     inblock <- p$date >= ts & p$date <= te
     expect_equal(sum(inblock) / 2L, 12L)            # 12 weekly rows per country

     # OLD behaviour: build from in-block rows only -> not enough for 13 timesteps
     expect_error(
          MOSAIC:::.psi_build_sequences(X[inblock, , drop = FALSE], p$y[inblock],
                                        p$iso_code[inblock], p$date[inblock],
                                        timesteps = 13L),
          "no valid sequences")

     # NEW behaviour: the slice carries (timesteps - 1) weeks of context
     bundle <- list(
          pool_data  = list(X = X, intensity = p$y, countries = p$iso_code, dates = p$date,
                            cw = rep(1, nrow(p))),
          seq_params = list(timesteps = 13L, max_gap_days = 14L, lead = 0L),
          encoders   = list(country_to_id = list(AAA = 1L, BBB = 2L),
                            region_for_country = list(AAA = 1L, BBB = 1L)),
          use_confidence_weight = FALSE)
     step <- list(step = 1L, train_end = ts - 14L, test_start = ts, test_end = te)
     sl <- MOSAIC:::.psi_slice_rw_step(bundle, step)
     expect_gt(sl$n_val, 0L)
})

test_that("no validation target ever falls outside the block", {
     p <- mk(); X <- matrix(p$y, ncol = 1)
     ts <- as.Date("2022-01-03"); te <- ts + 83
     for (ld in c(0L, 12L)) {
          bundle <- list(
               pool_data  = list(X = X, intensity = p$y, countries = p$iso_code,
                                 dates = p$date, cw = rep(1, nrow(p))),
               seq_params = list(timesteps = 13L, max_gap_days = 14L, lead = ld),
               encoders   = list(country_to_id = list(AAA = 1L, BBB = 2L),
                                 region_for_country = list(AAA = 1L, BBB = 1L)),
               use_confidence_weight = FALSE)
          step <- list(step = 1L, train_end = ts - 14L, test_start = ts, test_end = te)
          sl <- MOSAIC:::.psi_slice_rw_step(bundle, step)
          expect_gt(sl$n_val, 0L)
          # y_val must equal the observed series at in-block dates only
          expect_true(all(sl$y_val >= min(p$y[p$date >= ts & p$date <= te])))
          expect_true(all(sl$y_val <= max(p$y[p$date >= ts & p$date <= te])))
     }
})

test_that("training targets never cross train_end (target-anchored by construction)", {
     p <- mk(); X <- matrix(p$y, ncol = 1)
     te_train <- as.Date("2021-06-07")
     bundle <- list(
          pool_data  = list(X = X, intensity = p$y, countries = p$iso_code,
                            dates = p$date, cw = rep(1, nrow(p))),
          seq_params = list(timesteps = 13L, max_gap_days = 14L, lead = 12L),
          encoders   = list(country_to_id = list(AAA = 1L, BBB = 2L),
                            region_for_country = list(AAA = 1L, BBB = 1L)),
          use_confidence_weight = FALSE)
     step <- list(step = 1L, train_end = te_train,
                  test_start = te_train + 14L, test_end = te_train + 14L + 83L)
     sl <- MOSAIC:::.psi_slice_rw_step(bundle, step)
     # every training target value must exist at a date <= train_end
     expect_true(all(sl$y_train <= max(p$y[p$date <= te_train])))
})

test_that("a day-based stride is not multiplied by rw_subsample (HA-01 launch bug)", {
     # `step_days` and `subsample` both thin the grid; applying both multiplies
     # them. With step_days = 84 and the B4 fixture's rw_subsample = 6 still in
     # force, the effective stride was 504 days and a 12-fold grid became 2.
     args <- list(fit_date_start = "2015-01-01", cutoff_date = "2022-01-01",
                  step_days = 84L, test_days = 84L, gap_weeks = 2L,
                  min_test_days = 84L, min_train_years = 4)
     n_ss1 <- length(do.call(MOSAIC:::.psi_make_rw_cv_steps, c(args, subsample = 1L)))
     n_ss6 <- length(suppressMessages(
          do.call(MOSAIC:::.psi_make_rw_cv_steps, c(args, subsample = 6L))))
     expect_equal(n_ss1, 12L)
     expect_equal(n_ss6, n_ss1)          # subsample must be ignored, not multiplied
     # and the month-based path must still honour subsample
     m1 <- length(MOSAIC:::.psi_make_rw_cv_steps("2010-01-01", "2026-10-29", subsample = 1L))
     m5 <- length(MOSAIC:::.psi_make_rw_cv_steps("2010-01-01", "2026-10-29", subsample = 5L))
     expect_lt(m5, m1)
})
