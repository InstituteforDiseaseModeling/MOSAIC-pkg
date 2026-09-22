# Regression: the inner CV must report the epoch whose weights the model is
# HOLDING, not the epoch training stopped at.
#
# THE BUG. `.psi_keras_fit_and_eval()` returned `length(history$metrics$loss)`.
# With `callback_early_stopping(patience = 10, restore_best_weights = TRUE)`
# that is `best + patience`. The value does not stay local: the caller records
# it as `best_epochs` (.psi_fit_predict_rw_cv), takes round(median(.)), and
# refits on the full in-sample data at that many epochs with NO early stopping
# and NO best-weight restoration. So the DEPLOYED model was trained up to
# `patience` epochs past the optimum -- a 40-60% overshoot on the 16-26 epoch
# schedule the production folds produce.
#
# No TensorFlow is needed: the rule is a pure function of the fit history.

.hist <- function(n, val_loss = NULL) {
     list(metrics = list(loss = seq_len(n) / n, val_loss = val_loss))
}

test_that("with restore_best_weights, the reported epoch is argmin(val_loss)", {
     # 25 epochs run, val_loss bottoms at 15, stops 10 later. 15 is the answer.
     vl <- c(seq(1.0, 0.5, length.out = 15L), seq(0.51, 0.60, length.out = 10L))
     expect_equal(which.min(vl), 15L)                      # fixture sanity
     expect_equal(MOSAIC:::.psi_epoch_from_history(.hist(25L, vl), TRUE), 15L)
})

test_that("the old behaviour is what we are NOT doing any more", {
     vl <- c(seq(1.0, 0.5, length.out = 15L), seq(0.51, 0.60, length.out = 10L))
     h  <- .hist(25L, vl)
     # the stop epoch, i.e. the value that used to be returned
     expect_equal(length(h$metrics$loss), 25L)
     expect_false(MOSAIC:::.psi_epoch_from_history(h, TRUE) ==
                       length(h$metrics$loss))
})

test_that("without restore_best_weights the stop epoch IS correct", {
     # the model keeps the LAST epoch's weights, so that is the epoch to refit at
     vl <- c(seq(1.0, 0.5, length.out = 15L), seq(0.51, 0.60, length.out = 10L))
     expect_equal(MOSAIC:::.psi_epoch_from_history(.hist(25L, vl), FALSE), 25L)
})

test_that("a fit that never early-stops is unaffected", {
     # val_loss still improving at the budget: argmin is the last epoch anyway
     vl <- seq(1.0, 0.4, length.out = 30L)
     expect_equal(MOSAIC:::.psi_epoch_from_history(.hist(30L, vl), TRUE), 30L)
})

test_that("missing or unusable val_loss falls back to the stop epoch", {
     expect_equal(MOSAIC:::.psi_epoch_from_history(.hist(12L, NULL), TRUE), 12L)
     expect_equal(MOSAIC:::.psi_epoch_from_history(.hist(12L, rep(NA_real_, 12L)), TRUE), 12L)
     expect_equal(MOSAIC:::.psi_epoch_from_history(.hist(12L, numeric(0)), TRUE), 12L)
})

test_that("the returned epoch is an integer, as the refit path requires", {
     vl <- c(0.9, 0.4, 0.5, 0.6)
     out <- MOSAIC:::.psi_epoch_from_history(.hist(4L, vl), TRUE)
     expect_type(out, "integer")
     expect_equal(out, 2L)
})
