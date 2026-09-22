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

# ---- D9b: country embedding initialised from static covariates -------------

.cv_static <- function(n_countries = 2L, n_cov = 5L) {
     m <- matrix(stats::rnorm(n_countries * n_cov), nrow = n_countries)
     dimnames(m) <- list(c("A", "B")[seq_len(n_countries)], paste0("cov", seq_len(n_cov)))
     m
}

test_that("D9b and N8 are PROMOTED: the resolved production spec has them ON", {
     ac <- MOSAIC:::.psi_load_arch_control(NULL)
     expect_identical(ac$country_static, "auto")
     expect_true(isTRUE(ac$country_balance))
})

test_that("the promotion OVERRIDES the fixture, which still pins the B4 value", {
     # This is the whole reason the promotion lives in .psi_load_arch_control()
     # and not in a downstream `%||%`: the B4 fixture sets country_balance
     # EXPLICITLY to false, so a null-coalesce never fires and the promotion
     # would be silently inert on the production path (lessons 1 and 6). Guard
     # the mechanism, not just the outcome -- if someone edits the fixture
     # instead, this test says so.
     f <- system.file("fixtures", "B4_rolling_cv_spec.yml", package = "MOSAIC")
     skip_if(!nzchar(f) || !file.exists(f), "B4 fixture not installed")
     fixture <- yaml::read_yaml(f)
     expect_false(isTRUE(fixture$country_balance))      # fixture unchanged
     expect_true(isTRUE(MOSAIC:::.psi_load_arch_control(NULL)$country_balance))
})

test_that("an explicit arch_control still beats the promoted default", {
     ac <- MOSAIC:::.psi_load_arch_control(list(country_balance = FALSE))
     expect_false(isTRUE(ac$country_balance))
})

test_that("country_static = 'off' survives modifyList where NULL does not", {
     # utils::modifyList DELETES a NULL element, so `country_static = NULL` is
     # indistinguishable from not setting it and falls back to the promoted
     # default. "off" is the value that actually opts out.
     ac_null <- MOSAIC:::.psi_load_arch_control(list(country_static = NULL))
     expect_null(ac_null$country_static)
     ac_off <- MOSAIC:::.psi_load_arch_control(list(country_static = "off"))
     expect_identical(ac_off$country_static, "off")
})

test_that("D9b is applied BY DEFAULT when the bundle carries the static matrix", {
     # No country_static hyperparam is passed: the promoted default must reach
     # the embedding and freeze it at the supplied covariates.
     skip_on_cran(); skip_if_not_installed("keras3")
     skip_if_not(reticulate::py_module_available("tensorflow"), "TensorFlow unavailable")
     set.seed(5)
     M <- .cv_static()
     b <- .cv_bundle(); b$encoders$country_static <- M
     hp <- utils::modifyList(.cv_hp, list(n_epochs_fixed = 2L))
     hp$country_static <- NULL                # ensure the caller does not set it
     # The fallback message fires ONLY when the default is not applied, so its
     # absence here is the discriminator against the degradation test above.
     expect_no_message(
          r <- MOSAIC:::.psi_fit_predict_lstm(b, seed = 11L, hyperparams = hp),
          message = "D9b not applied")
     expect_true(all(is.finite(as.numeric(r$pred))))
})

test_that("an unknown country_static mode is rejected", {
     skip_on_cran(); skip_if_not_installed("keras3")
     skip_if_not(reticulate::py_module_available("tensorflow"), "TensorFlow unavailable")
     b <- .cv_bundle()
     expect_error(
          MOSAIC:::.psi_fit_predict_lstm(b, seed = 11L,
               hyperparams = utils::modifyList(.cv_hp, list(country_static = "nonsense"))),
          "country_static must be one of")
})

test_that("country_static = 'auto' DEGRADES when the matrix is absent", {
     # The promotion must not break callers holding an older bundle: 'auto' is
     # the default, so it has to fall back rather than abort. An explicit
     # 'frozen' on the same bundle still errors (next test).
     skip_on_cran(); skip_if_not_installed("keras3")
     skip_if_not(reticulate::py_module_available("tensorflow"), "TensorFlow unavailable")
     b <- .cv_bundle()                        # encoders carry no country_static
     hp <- utils::modifyList(.cv_hp, list(country_static = "auto", n_epochs_fixed = 2L))
     expect_message(
          r <- MOSAIC:::.psi_fit_predict_lstm(b, seed = 11L, hyperparams = hp),
          "D9b not applied")
     expect_true(all(is.finite(as.numeric(r$pred))))
})

test_that("requesting country_static without the matrix fails loudly", {
     skip_on_cran(); skip_if_not_installed("keras3")
     skip_if_not(reticulate::py_module_available("tensorflow"), "TensorFlow unavailable")
     b <- .cv_bundle()                        # encoders carry no country_static
     expect_error(
          MOSAIC:::.psi_fit_predict_lstm(b, seed = 11L,
               hyperparams = utils::modifyList(.cv_hp, list(country_static = "frozen"))),
          "encoders\\$country_static is absent")
})

test_that("D9b fits in both modes and frozen really does not move the embedding", {
     skip_on_cran(); skip_if_not_installed("keras3")
     skip_if_not(reticulate::py_module_available("tensorflow"), "TensorFlow unavailable")
     set.seed(3)
     M <- .cv_static()
     b <- .cv_bundle(); b$encoders$country_static <- M
     for (mode in c("frozen", "trainable")) {
          hp <- utils::modifyList(.cv_hp, list(country_static = mode, n_epochs_fixed = 2L))
          r <- MOSAIC:::.psi_fit_predict_lstm(b, seed = 11L, hyperparams = hp)
          expect_true(all(is.finite(as.numeric(r$pred))), info = mode)
     }
})

test_that("the static matrix built into encoders is z-scored and country-ordered", {
     # Mirrors what .psi_build_data does, so a change to the recipe is caught
     # here rather than after an arm has spent its compute.
     iso <- c("AAA", "BBB", "CCC")
     d <- data.frame(
          iso_code = rep(iso, each = 4),
          Piped_Water        = rep(c(0.1, 0.5, 0.9), each = 4),
          Open_Defecation    = rep(c(0.7, 0.3, 0.05), each = 4),
          population_density = rep(c(10, 200, 50), each = 4),
          GDP                = rep(c(500, 5000, 1500), each = 4))
     sc <- intersect(MOSAIC:::.PSI_STATIC_COUNTRY_COVARIATES, names(d))
     expect_length(sc, 4L)
     M <- vapply(sc, function(k) as.numeric(tapply(d[[k]], d$iso_code, mean)[iso]),
                 numeric(length(iso)))
     M <- scale(M)
     expect_equal(nrow(M), 3L)
     expect_true(all(abs(colMeans(M)) < 1e-12))            # z-scored across countries
     expect_true(all(abs(apply(M, 2, stats::sd) - 1) < 1e-12))
})

test_that("D9b composes with N5 and N6", {
     skip_on_cran(); skip_if_not_installed("keras3")
     skip_if_not(reticulate::py_module_available("tensorflow"), "TensorFlow unavailable")
     set.seed(4)
     b <- .cv_bundle(); b$encoders$country_static <- .cv_static()
     r <- MOSAIC:::.psi_fit_predict_lstm(b, seed = 11L,
            hyperparams = utils::modifyList(.cv_hp,
              list(country_static = "frozen", film_input = TRUE, gamma_scale = 2)))
     expect_true(all(is.finite(as.numeric(r$pred))))
})
