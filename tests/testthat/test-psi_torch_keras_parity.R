# T1-X: weight-transplant forward parity between the keras3 and torch graphs.
#
# This is a MIGRATION test. It proves the torch module implements the same
# function as the keras graph, independently of any training stochasticity, by
# transplanting keras' trained weights into the torch module and comparing
# inference-mode forward passes. Delete it together with the keras backend.
#
# Needs BOTH stacks, so it is gated like the other keras end-to-end tests.

skip_without_both_backends <- function() {
  testthat::skip_on_cran(); testthat::skip_on_ci()
  testthat::skip_if_not_installed("torch")
  testthat::skip_if_not_installed("keras3")
  testthat::skip_if_not_installed("reticulate")
  if (!isTRUE(torch::torch_is_installed()))
    testthat::skip("LibTorch/Lantern not installed")
  if (!nzchar(Sys.getenv("MOSAIC_RUN_TORCH_PARITY")))
    testthat::skip("set MOSAIC_RUN_TORCH_PARITY=1 to run the keras<->torch parity test")
}

# keras kernel (in, out) -> torch weight (out, in); keras' single LSTM bias maps
# to b_ih with b_hh zeroed (torch sums them). Gate order is [i, f, c/g, o] in
# both, which this test verifies empirically rather than on faith.
transplant_keras_to_torch <- function(net, model) {
  cp <- function(param, arr)
    torch::with_no_grad(param$copy_(torch::torch_tensor(arr, dtype = torch::torch_float())))
  for (k in 1:3) {
    kl <- keras3::get_layer(model, paste0("lstm", k))$get_weights()
    tl <- net[[paste0("lstm", k)]]$lstm
    cp(tl$weight_ih_l1, t(kl[[1]]))
    cp(tl$weight_hh_l1, t(kl[[2]]))
    cp(tl$bias_ih_l1,   kl[[3]])
    torch::with_no_grad(tl$bias_hh_l1$zero_())
  }
  dense <- c("film_gamma_country", "film_beta_country", "out_head")
  if (!net$skip_region) dense <- c(dense, "film_gamma_region", "film_beta_region")
  for (nm in dense) {
    kw <- keras3::get_layer(model, nm)$get_weights()
    cp(net[[nm]]$weight, t(kw[[1]])); cp(net[[nm]]$bias, kw[[2]])
  }
  if (!net$skip_region)
    cp(net$region_embedding$weight,
       keras3::get_layer(model, "region_embedding")$get_weights()[[1]])
  cp(net$country_deviation_embedding$weight,
     keras3::get_layer(model, "country_deviation_embedding")$get_weights()[[1]])
  invisible(net)
}

parity_max_abs_diff <- function(nC, nR, activation, B = 7L, Tt = 13L, F_ = 9L) {
  hp <- list(units_1 = 24L, units_2 = 16L, units_3 = 8L, country_dim = 5L,
             dropout = 0.3, rec_dropout = 0.10, l2 = 5e-4, region_l2 = 1e-4,
             partial_pool_lambda = 0.1)
  enc <- list(n_countries = nC, n_regions = nR)
  model <- MOSAIC:::.psi_build_keras_model(hp, enc, Tt, F_, activation = activation)
  net   <- MOSAIC:::.psi_torch_film_net(F_, nC, nR, hp$units_1, hp$units_2, hp$units_3,
                                        hp$country_dim, hp$dropout, hp$rec_dropout,
                                        activation)
  # Both graphs zero-init the country deviation, which would make the country
  # FiLM an identity and hide a bug there: randomise it before transplanting.
  keras3::get_layer(model, "country_deviation_embedding")$set_weights(
    list(array(stats::rnorm(nC * hp$country_dim, sd = 0.3), dim = c(nC, hp$country_dim))))
  transplant_keras_to_torch(net, model)

  X   <- array(stats::rnorm(B * Tt * F_), dim = c(B, Tt, F_))
  cid <- sample.int(nC, B, TRUE) - 1L
  rid <- sample.int(nR, B, TRUE) - 1L
  ok <- stats::predict(model, list(X, matrix(cid, ncol = 1L), matrix(rid, ncol = 1L)),
                       verbose = 0L)
  net$eval()
  ot <- torch::with_no_grad(as.numeric(net(
    torch::torch_tensor(X, dtype = torch::torch_float()),
    torch::torch_tensor(cid + 1L, dtype = torch::torch_long()),
    torch::torch_tensor(rid + 1L, dtype = torch::torch_long()))))
  max(abs(as.numeric(ok) - ot))
}

test_that("T1-X: transplanted keras weights give the same forward pass in torch", {
  skip_without_both_backends()
  set.seed(42); torch::torch_manual_seed(42L)
  expect_lt(parity_max_abs_diff(40L, 5L, "sigmoid"), 1e-5)   # production shape
  expect_lt(parity_max_abs_diff(6L,  1L, "sigmoid"), 1e-5)   # single region
  expect_lt(parity_max_abs_diff(12L, 4L, "linear"),  1e-5)   # mse_logit head
})

test_that("T1-1: parameter inventory matches keras up to torch's split LSTM biases", {
  skip_without_both_backends()
  hp <- list(units_1 = 24L, units_2 = 16L, units_3 = 8L, country_dim = 5L,
             dropout = 0.3, rec_dropout = 0.10, l2 = 5e-4, region_l2 = 1e-4,
             partial_pool_lambda = 0.1)
  model <- MOSAIC:::.psi_build_keras_model(hp, list(n_countries = 40L, n_regions = 5L),
                                           13L, 9L, "sigmoid")
  net <- MOSAIC:::.psi_torch_film_net(9L, 40L, 5L, hp$units_1, hp$units_2, hp$units_3,
                                      hp$country_dim, hp$dropout, hp$rec_dropout, "sigmoid")
  nk <- sum(vapply(model$weights, function(w) prod(dim(w)), numeric(1)))
  nt <- sum(vapply(net$parameters, function(p) prod(dim(p)), numeric(1)))
  # PyTorch LSTMs carry b_ih AND b_hh where keras carries one bias vector.
  extra <- sum(vapply(1:3, function(k) 4 * net[[paste0("lstm", k)]]$lstm$hidden_size,
                      numeric(1)))
  expect_identical(nk + extra, nt)
})
