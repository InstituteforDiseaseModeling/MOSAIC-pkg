# TRUNK REGISTRY (psi_evolve `N` arms).
#
# `arch_control$trunk` selects the sequence encoder while the hierarchical-FiLM
# conditioning, the head, the loss and the feature pipeline are held fixed -- so
# an architecture arm is ONE registered change (psi_evolve PROTOCOL section 2)
# rather than a confounded bundle. Every variant must return (B, units_3), which
# is the contract the FiLM stack depends on.
#
# Why this matters: the April 2026 bake-off ranked six trunks against the
# THEN-production 3-layer LSTM, and the hierarchical-FiLM model landed after it,
# so no alternative trunk has ever been compared against the model in production.

.mk_tiny_bundle <- function(n_tr = 48L, n_va = 16L, n_pr = 20L,
                            timesteps = 13L, n_features = 4L) {
     rf <- function(n) array(stats::runif(n * timesteps * n_features),
                             dim = c(n, timesteps, n_features))
     list(
          X_train = rf(n_tr), y_train = stats::runif(n_tr),
          # ids are 0-BASED: .psi_build_sequences() encodes with
          # seq_along(iso_list) - 1L, and the embeddings use input_dim =
          # n_countries, so a 1-based id overflows the table.
          country_ids_train = rep(0:1, length.out = n_tr),
          region_ids_train  = rep(0L, n_tr),
          X_val = rf(n_va), y_val = stats::runif(n_va),
          country_ids_val = rep(0:1, length.out = n_va),
          region_ids_val  = rep(0L, n_va),
          X_pred = rf(n_pr),
          country_ids_pred = rep(0:1, length.out = n_pr),
          region_ids_pred  = rep(0L, n_pr),
          confidence_weight_train = NULL, confidence_weight_val = NULL,
          encoders = list(n_countries = 2L, n_regions = 1L,
                          country_to_id = list(AAA = 0L, BBB = 1L),
                          region_for_country = list(AAA = 0L, BBB = 0L)))
}

.tiny_hp <- list(units_1 = 8L, units_2 = 6L, units_3 = 4L, epochs = 1L,
                 n_epochs_fixed = 1L, batch_size = 16L, patience = 1L,
                 country_dim = 2L, rec_dropout = 0)

test_that("every registered trunk fits and predicts through the FiLM head", {
     skip_on_cran()
     skip_if_not_installed("keras3")
     skip_if_not_installed("reticulate")
     skip_if_not(reticulate::py_module_available("tensorflow"),
                 "TensorFlow not available in this environment")
     b <- .mk_tiny_bundle()
     for (tk in c("lstm", "gru", "tcn", "dlinear")) {
          hp <- utils::modifyList(.tiny_hp, list(trunk = tk))
          r <- MOSAIC:::.psi_fit_predict_lstm(b, seed = 11L, hyperparams = hp)
          expect_equal(length(as.numeric(r$pred)), dim(b$X_pred)[1],
                       info = paste("trunk:", tk))
          expect_true(all(is.finite(as.numeric(r$pred))), info = paste("trunk:", tk))
          # provenance: the artefact must say which trunk produced it, or two
          # psi files from different architectures are indistinguishable.
          expect_identical(r$trunk, tk)
          expect_identical(r$hier_mode, "film")
     }
})

test_that("the default trunk is the production LSTM", {
     skip_on_cran()
     skip_if_not_installed("keras3")
     skip_if_not(reticulate::py_module_available("tensorflow"),
                 "TensorFlow not available in this environment")
     r <- MOSAIC:::.psi_fit_predict_lstm(.mk_tiny_bundle(), seed = 11L,
                                         hyperparams = .tiny_hp)
     expect_identical(r$trunk, "lstm")
})

test_that("an unknown trunk is refused rather than silently falling back", {
     skip_on_cran()
     skip_if_not_installed("keras3")
     skip_if_not(reticulate::py_module_available("tensorflow"),
                 "TensorFlow not available in this environment")
     hp <- utils::modifyList(.tiny_hp, list(trunk = "mamba"))
     expect_error(MOSAIC:::.psi_fit_predict_lstm(.mk_tiny_bundle(), seed = 11L,
                                                 hyperparams = hp),
                  "unknown trunk")
})

test_that("the TCN trunk validates its dilation spec", {
     skip_on_cran()
     skip_if_not_installed("keras3")
     skip_if_not(reticulate::py_module_available("tensorflow"),
                 "TensorFlow not available in this environment")
     hp <- utils::modifyList(.tiny_hp, list(trunk = "tcn", tcn_dilations = c(1L, 2L)))
     expect_error(MOSAIC:::.psi_fit_predict_lstm(.mk_tiny_bundle(), seed = 11L,
                                                 hyperparams = hp),
                  "tcn_dilations must have length 3")
})

test_that("arch_control threads the trunk into the model hyperparameters", {
     # A knob that never reaches the model is the package's recurring failure
     # (CLAUDE.md lessons 1 and 6: functions created but never wired in). Assert
     # the resolved arch_control carries it rather than trusting the plumbing.
     ac <- MOSAIC:::.psi_load_arch_control(list(trunk = "tcn", tcn_kernel = 5,
                                                 epoch_select_seeds = 2))
     expect_identical(ac$trunk, "tcn")
     expect_identical(ac$tcn_kernel, 5L)            # coerced to integer
     expect_identical(ac$epoch_select_seeds, 2L)
     # and the default resolves to NULL so the historical path is unchanged
     ac0 <- MOSAIC:::.psi_load_arch_control(NULL)
     expect_null(ac0$trunk)
     expect_null(ac0$epoch_select_seeds)
})
