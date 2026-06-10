# CPU-only coverage of the lstm_v2 path's .psi_configure_loss (review Batch 4 /
# B2-10). test-suitability_helpers.R already pins the balanced_uniform ratio and
# the confidence-weight overlay; this file covers the remaining branches: the
# bce/mse_logit target+prediction transforms, the linear/quadratic weight
# schemes, the empty-class edge, the country-balance overlay, and the error
# paths. Pure R — no keras/TF (matches the suite's no-TF-in-CI policy).

configure_loss <- getFromNamespace(".psi_configure_loss", "MOSAIC")

test_that("bce config uses sigmoid+BCE and clamps the target/prediction to [0,1]", {
  lc <- configure_loss(c(-0.2, 0.5, 1.3), loss_kind = "bce", sample_weights = "none")
  expect_equal(lc$activation, "sigmoid")
  expect_equal(lc$loss, "binary_crossentropy")
  expect_equal(lc$y_train, c(0, 0.5, 1))                       # target clamped
  expect_equal(lc$transform_pred(c(-1, 0.4, 2)), c(0, 0.4, 1)) # prediction clamped
})

test_that("mse_logit config uses linear+MSE and round-trips logit<->plogis", {
  y  <- c(0.2, 0.5, 0.8)
  lc <- configure_loss(y, loss_kind = "mse_logit", sample_weights = "none")
  expect_equal(lc$activation, "linear")
  expect_equal(lc$loss, "mse")
  expect_equal(lc$y_train, stats::qlogis(y), tolerance = 1e-8)      # logit target
  expect_equal(lc$transform_pred(stats::qlogis(y)), y, tolerance = 1e-6)  # inverse
})

test_that("linear and quadratic sample weights apply the documented offsets/floors", {
  y   <- c(0, 0.5, 1)
  lin <- configure_loss(y, sample_weights = "linear",
                        sw_offset = 0.1, sw_min = 0.1)
  expect_equal(lin$sample_weight_train, pmax(0.1, y + 0.1))       # 0.1, 0.6, 1.1
  quad <- configure_loss(y, sample_weights = "quadratic",
                         sw_offset_quad = 0.05, sw_min_quad = 0.05)
  expect_equal(quad$sample_weight_train, pmax(0.05, y^2 + 0.05))  # 0.05, 0.3, 1.05
})

test_that("balanced_uniform falls back to uniform weights when a class is empty", {
  expect_equal(
    configure_loss(rep(0, 5), sample_weights = "balanced_uniform")$sample_weight_train,
    rep(1, 5))
  expect_equal(
    configure_loss(runif(5, 0.1, 1), sample_weights = "balanced_uniform")$sample_weight_train,
    rep(1, 5))
})

test_that("country_balance overlay equalizes total weight across countries", {
  y    <- c(rep(0, 4), runif(4, 0.1, 1))
  ctry <- c("A", "A", "A", "A", "A", "B", "B", "B")   # 5 A vs 3 B (unbalanced)
  lc <- configure_loss(y, sample_weights = "balanced_uniform",
                       country_balance = TRUE, country_train = ctry)
  w <- lc$sample_weight_train
  expect_equal(sum(w[ctry == "A"]), sum(w[ctry == "B"]), tolerance = 1e-8)
  expect_true(lc$country_balance)
})

test_that("configure_loss errors on unknown loss_kind and unknown sample_weights", {
  expect_error(configure_loss(c(0, 1), loss_kind = "huber"), "unknown loss_kind")
  expect_error(configure_loss(c(0, 1), sample_weights = "magic"), "Unknown sample_weights")
})
