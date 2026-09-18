# COUNTRY-VARIABILITY CAPACITY (psi_evolve `N` arms N5/N6).
#
# WHY THESE EXIST. Production conditions only the trunk's 32-dim OUTPUT --
# z_r = (1+g_r)z + b_r then z_c = (1+g_c)z_r + b_c -- while the recurrent
# weights that process the 13-week sequence are IDENTICAL for all 40 countries.
# So no country can have its own lag structure, response timing or persistence,
# yet measured on the production grid the horizon decay ratio spans 0.083 (ZWE)
# to 1.387 (MWI) and every arm's benefit splits along the snf_k5 region map
# (snf_1 weighted -0.259 with every member non-positive vs snf_2 +0.498).
#
#   N5 `film_input`  -- condition the trunk's INPUTS, so a country reweights
#                       which covariates the shared dynamics see.
#   N6 `gamma_scale` -- widen country modulation past tanh's (0,2) multiplier so
#                       a country can REVERSE a feature's sign, not only damp it.
#
# Both default to production behaviour.

.cv_bundle <- function(ts = 13L, nf = 6L) {
     rf <- function(n) array(stats::runif(n * ts * nf), dim = c(n, ts, nf))
     list(X_train = rf(48), y_train = stats::runif(48),
          country_ids_train = rep(0:1, length.out = 48), region_ids_train = rep(0L, 48),
          X_val = rf(16), y_val = stats::runif(16),
          country_ids_val = rep(0:1, length.out = 16), region_ids_val = rep(0L, 16),
          X_pred = rf(20), country_ids_pred = rep(0:1, length.out = 20),
          region_ids_pred = rep(0L, 20),
          confidence_weight_train = NULL, confidence_weight_val = NULL,
          encoders = list(n_countries = 2L, n_regions = 1L,
                          country_to_id = list(A = 0L, B = 1L),
                          region_for_country = list(A = 0L, B = 0L)))
}
.cv_hp <- list(units_1 = 8L, units_2 = 6L, units_3 = 4L, epochs = 1L,
               n_epochs_fixed = 1L, batch_size = 16L, patience = 1L,
               country_dim = 2L, rec_dropout = 0, dropout = 0)

test_that("the defaults are production: no input-FiLM, tanh-width country gamma", {
     ac <- MOSAIC:::.psi_load_arch_control(NULL)
     expect_false(isTRUE(ac$film_input))
     expect_null(ac$gamma_scale)
})

test_that("arch_control threads film_input and gamma_scale", {
     ac <- MOSAIC:::.psi_load_arch_control(list(film_input = TRUE, gamma_scale = 2))
     expect_true(isTRUE(ac$film_input))
     expect_equal(ac$gamma_scale, 2)
})

test_that("a zero-init embedding feeding a default-bias dense is exactly zero", {
     # This is the mechanism the N5 identity-at-init claim rests on: the FiLM
     # generator's embedding is zero-initialised and Keras defaults
     # bias_initializer to zeros, so dense(0) = 0 and tanh(0) = 0, making
     # (1 + gamma_in) * X + beta_in == X exactly at initialisation. Zeroing the
     # DENSE as well would instead make the branch permanently dead (a zero
     # input gives a zero weight gradient and a zero weight gives a zero
     # embedding gradient), which is why only the embedding is zeroed.
     skip_on_cran(); skip_if_not_installed("keras3")
     skip_if_not(reticulate::py_module_available("tensorflow"), "TensorFlow unavailable")
     inp <- keras3::layer_input(shape = 1L, dtype = "int32")
     e <- inp |>
          keras3::layer_embedding(input_dim = 2L, output_dim = 3L,
                                  embeddings_initializer = "zeros") |>
          keras3::layer_flatten()
     g <- keras3::layer_dense(e, units = 4L, activation = "tanh")
     m <- keras3::keras_model(inputs = inp, outputs = g)
     out <- as.numeric(stats::predict(m, matrix(c(0L, 1L), ncol = 1), verbose = 0))
     expect_true(all(abs(out) < 1e-7))
})

test_that("N5 input-FiLM fits and predicts, and records itself in the result", {
     skip_on_cran(); skip_if_not_installed("keras3")
     skip_if_not(reticulate::py_module_available("tensorflow"), "TensorFlow unavailable")
     b <- .cv_bundle()
     r <- MOSAIC:::.psi_fit_predict_lstm(b, seed = 11L,
            hyperparams = utils::modifyList(.cv_hp, list(film_input = TRUE)))
     p <- as.numeric(r$pred)
     expect_equal(length(p), dim(b$X_pred)[1])
     expect_true(all(is.finite(p)))
     expect_identical(r$trunk, "lstm")          # N5 is orthogonal to the trunk
})

test_that("N6 sign-permissive gamma fits and predicts at scales > 1", {
     skip_on_cran(); skip_if_not_installed("keras3")
     skip_if_not(reticulate::py_module_available("tensorflow"), "TensorFlow unavailable")
     b <- .cv_bundle()
     for (gs in c(1, 2, 3)) {
          r <- MOSAIC:::.psi_fit_predict_lstm(b, seed = 11L,
                 hyperparams = utils::modifyList(.cv_hp, list(gamma_scale = gs)))
          expect_true(all(is.finite(as.numeric(r$pred))), info = paste("gamma_scale:", gs))
     }
})

test_that("N5 and N6 compose, and compose with a non-default trunk", {
     skip_on_cran(); skip_if_not_installed("keras3")
     skip_if_not(reticulate::py_module_available("tensorflow"), "TensorFlow unavailable")
     b <- .cv_bundle()
     r <- MOSAIC:::.psi_fit_predict_lstm(b, seed = 11L,
            hyperparams = utils::modifyList(.cv_hp,
              list(film_input = TRUE, gamma_scale = 2, trunk = "tcn")))
     expect_true(all(is.finite(as.numeric(r$pred))))
     expect_identical(r$trunk, "tcn")
})
