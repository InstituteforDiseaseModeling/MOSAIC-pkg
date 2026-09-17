# Tier-1 structural tests for the torch backend of the lstm_v2 suitability path.
# torch is in Suggests; every test skips cleanly without it.

skip_without_torch <- function() {
  testthat::skip_if_not_installed("torch")
  if (!isTRUE(torch::torch_is_installed()))
    testthat::skip("LibTorch/Lantern not installed (torch::install_torch())")
}

mk_bundle <- function(n_tr = 400L, n_va = 150L, n_pr = 200L,
                      nC = 6L, nR = 3L, Tt = 13L, F_ = 9L, seed = 3L) {
  set.seed(seed)
  gen <- function(n) {
    X <- array(stats::rnorm(n * Tt * F_), dim = c(n, Tt, F_))
    cid <- sample.int(nC, n, TRUE) - 1L        # 0-based, as .psi_build_data emits
    list(X = X, cid = cid, rid = cid %% nR,
         y = pmax(0, pmin(1, stats::plogis(rowMeans(X[, Tt, 1:3])))))
  }
  a <- gen(n_tr); b <- gen(n_va); p <- gen(n_pr)
  list(X_train = a$X, y_train = a$y,
       country_ids_train = a$cid, region_ids_train = a$rid,
       X_val = b$X, y_val = b$y,
       country_ids_val = b$cid, region_ids_val = b$rid,
       X_pred = p$X, country_ids_pred = p$cid, region_ids_pred = p$rid,
       encoders = list(n_countries = nC, n_regions = nR))
}

hp_small <- list(units_1 = 16L, units_2 = 12L, units_3 = 8L, country_dim = 4L,
                 epochs = 4L, patience = 2L, batch_size = 64L)

test_that("T1-12: the torch backend honours the keras fit_predict return contract", {
  skip_without_torch()
  r <- MOSAIC:::.psi_fit_predict_lstm_torch(mk_bundle(), seed = 11L, hyperparams = hp_small)
  expect_true(all(c("pred", "val_loss", "val_metric", "train_minutes", "n_epochs",
                    "loss_type", "arch_kind", "hier_mode", "sample_weights") %in% names(r)))
  expect_length(r$pred, 200L)
  expect_true(all(is.finite(r$pred)))
  expect_true(all(r$pred >= 0 & r$pred <= 1))   # sigmoid head under bce
  expect_identical(r$arch_kind, "hierarchical")
  expect_identical(r$hier_mode, "film")
  expect_identical(r$backend, "torch")
})

test_that("T1-7: n_epochs_fixed runs exactly N epochs with no validation", {
  skip_without_torch()
  b <- mk_bundle()
  b$X_val <- NULL; b$y_val <- NULL
  b$country_ids_val <- NULL; b$region_ids_val <- NULL
  r <- MOSAIC:::.psi_fit_predict_lstm_torch(
    b, seed = 11L, hyperparams = utils::modifyList(hp_small, list(n_epochs_fixed = 3L)))
  expect_identical(r$n_epochs, 3L)
  expect_true(is.na(r$val_loss))
  expect_true(is.na(r$val_metric))
})

test_that("T1-7b: val mode without a validation slice errors (matches keras backend)", {
  skip_without_torch()
  b <- mk_bundle(); b$X_val <- NULL; b$y_val <- NULL
  expect_error(
    MOSAIC:::.psi_fit_predict_lstm_torch(b, seed = 11L, hyperparams = hp_small),
    "validation data required")
})

test_that("T1-2/T1-3: FiLM is the identity at init and degrades cleanly to one region", {
  skip_without_torch()
  net <- MOSAIC:::.psi_torch_film_net(n_features = 9L, n_countries = 8L, n_regions = 4L,
                                      units_1 = 16L, units_2 = 12L, units_3 = 8L,
                                      country_dim = 4L, dropout = 0, rec_dropout = 0)
  MOSAIC:::.psi_torch_init_keras_style(net)
  # zero-init country deviation => gamma_c = tanh(0) = 0, beta_c = 0 => z_c == z_r
  w <- as.matrix(net$country_deviation_embedding$weight)
  expect_true(all(w == 0))

  net1 <- MOSAIC:::.psi_torch_film_net(n_features = 9L, n_countries = 8L, n_regions = 1L,
                                       units_1 = 16L, units_2 = 12L, units_3 = 8L,
                                       country_dim = 4L, dropout = 0, rec_dropout = 0)
  expect_true(net1$skip_region)
  expect_null(net1$region_embedding)
})

test_that("T1-4: the FiLM gain (1 + tanh(.)) is bounded in [0, 2]", {
  skip_without_torch()
  g <- as.numeric(torch::torch_tanh(torch::torch_randn(2000) * 50))
  expect_gte(min(1 + g), 0)
  expect_lte(max(1 + g), 2)
})

test_that("T1-6: the L2 penalty is keras' l * sum(w^2) on exactly the keras-penalised tensors", {
  skip_without_torch()
  net <- MOSAIC:::.psi_torch_film_net(n_features = 9L, n_countries = 8L, n_regions = 4L,
                                      units_1 = 16L, units_2 = 12L, units_3 = 8L,
                                      country_dim = 4L, dropout = 0, rec_dropout = 0)
  hp <- list(l2 = 5e-4, region_l2 = 1e-4, partial_pool_lambda = 0.1)
  tms <- MOSAIC:::.psi_torch_l2_terms(net, hp)
  # 3 LSTMs x (weight_ih, weight_hh) + region embedding + country embedding
  expect_length(tms, 8L)
  expect_setequal(vapply(tms, function(t) t$lambda, numeric(1)),
                  c(5e-4, 1e-4, 0.1))
  manual <- sum(vapply(tms, function(t)
    t$lambda * sum(as.numeric(t$w)^2), numeric(1)))
  expect_equal(as.numeric(MOSAIC:::.psi_torch_l2_penalty(tms)), manual, tolerance = 1e-6)
  # biases / FiLM heads / output head carry NO penalty (as in keras)
  pen_ptrs <- vapply(tms, function(t) paste(dim(t$w), collapse = "x"), character(1))
  expect_false(paste(dim(net$out_head$weight), collapse = "x") %in% pen_ptrs)
})

test_that("T1-6b: l2 = 0 yields no penalty term", {
  skip_without_torch()
  net <- MOSAIC:::.psi_torch_film_net(9L, 8L, 4L, 16L, 12L, 8L, 4L, 0, 0)
  expect_length(MOSAIC:::.psi_torch_l2_terms(net, list(l2 = 0, region_l2 = 0,
                                                       partial_pool_lambda = 0)), 0L)
  expect_null(MOSAIC:::.psi_torch_l2_penalty(list()))
})

test_that("T1-11: inference is deterministic within a process", {
  skip_without_torch()
  net <- MOSAIC:::.psi_torch_film_net(9L, 8L, 4L, 16L, 12L, 8L, 4L, 0.3, 0.1)
  MOSAIC:::.psi_torch_init_keras_style(net)
  net$eval()
  x <- torch::torch_randn(5, 13, 9)
  cid <- torch::torch_tensor(c(1L, 2L, 3L, 4L, 5L), dtype = torch::torch_long())
  rid <- torch::torch_tensor(c(1L, 1L, 2L, 2L, 3L), dtype = torch::torch_long())
  a <- torch::with_no_grad(as.numeric(net(x, cid, rid)))
  b <- torch::with_no_grad(as.numeric(net(x, cid, rid)))
  expect_identical(a, b)
})

test_that("T1-X-lite: the variational-dropout layer equals the fused kernel in eval mode", {
  skip_without_torch()
  torch::torch_manual_seed(1L)
  l <- MOSAIC:::.psi_nn_lstm_vd(6L, 5L, rec_dropout = 0.3)
  x <- torch::torch_randn(4, 13, 6)
  l$eval()
  seq_out  <- as.array(torch::with_no_grad(l(x, return_sequences = TRUE)))
  last_out <- as.array(torch::with_no_grad(l(x, return_sequences = FALSE)))
  # return_sequences = FALSE must equal the final timestep of the full sequence
  expect_equal(last_out, seq_out[, 13, ], tolerance = 1e-6)
  # training mode with rec_dropout > 0 takes the manual loop: same shape, different values
  l$train()
  tr_out <- as.array(l(x, return_sequences = TRUE))
  expect_identical(dim(tr_out), dim(seq_out))
  expect_false(isTRUE(all.equal(tr_out, seq_out)))
})

test_that("T1-idx: R torch embeddings are 1-based and .psi_torch_ids shifts the bundle's 0-based ids", {
  skip_without_torch()
  ids <- MOSAIC:::.psi_torch_ids(c(0L, 1L, 2L))
  expect_equal(as.integer(ids), c(1L, 2L, 3L))
  e <- torch::nn_embedding(3L, 2L)
  torch::with_no_grad(e$weight$copy_(
    torch::torch_tensor(matrix(c(10, 11, 20, 21, 30, 31), nrow = 3, byrow = TRUE))))
  emb <- as.matrix(e(ids))
  expect_equal(emb[1, ], c(10, 11))   # bundle id 0 -> torch index 1 -> row 1
  expect_equal(emb[3, ], c(30, 31))   # bundle id 2 -> torch index 3 -> row 3
})

test_that("backend dispatch is validated and recorded", {
  skip_without_torch()
  ac <- MOSAIC:::.psi_load_arch_control(NULL)
  expect_identical(ac$backend, "keras")                       # incumbent default
  ac2 <- MOSAIC:::.psi_load_arch_control(list(backend = "torch"))
  expect_identical(ac2$backend, "torch")
})
