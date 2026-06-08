# =============================================================================
# lstm_film_suitability.R — The gauge_A LSTM for the lstm_v2 suitability path:
# a 3-stack LSTM trunk (128 -> 64 -> 32) with hierarchical FiLM modulation
# (region FiLM then country-deviation FiLM, tanh gamma). Ported from the
# MOSAIC-Mozambique sandbox (archs/arch_lstm.R), GAUGE_A ONLY — the flat,
# concat, input-FiLM, hypernet, and exp-gamma ("gauge_B") branches are dropped.
#
# Pipeline:
#   features (B, T, F) --LSTM stack--> z (units_3-dim shared climate response)
#   region_id  --Embedding(R,16)--> r_vec --Dense--> (gamma_r, beta_r)
#   country_id --Embedding(C,16, init=0)--> c_dev --Dense--> (gamma_c, beta_c)
#     Region FiLM:  z_r = (1 + gamma_r) * z   + beta_r
#     Country FiLM: z_c = (1 + gamma_c) * z_r + beta_c
#     Output:       psi = sigmoid(Dense(1)(z_c))
#   gamma_r, gamma_c use tanh so the multiplicative gain (1 + gamma) lies in
#   [0, 2] (bounded; identity at initialization). The country embedding is a
#   zero-initialized *deviation*, so countries with no training data inherit
#   pure regional modulation by construction; an L2 partial-pool penalty
#   (partial_pool_lambda) shrinks data-sparse countries back toward their region.
#
# This tanh hierarchical-FiLM configuration is the empirically-selected B4 path
# (it dominates the alternatives on amplitude/bias — see plan_v034_RECOMMENDED).
# When n_regions == 1 the region branch is skipped (a frozen zero "tap" keeps
# input_region in the graph) and the model degrades to country-only FiLM.
#
# keras3 is in Suggests; this runs only inside the lstm_v2 path. `%||%` is
# provided package-wide by R/aaa_utils.R.
# =============================================================================

#' Fit the gauge_A hierarchical-FiLM LSTM and predict over X_pred.
#' @keywords internal
#' @noRd
.psi_fit_predict_lstm <- function(data_bundle, seed = 11L, hyperparams = list()) {
     set.seed(seed)
     tf <- reticulate::import("tensorflow", convert = FALSE)
     tf$random$set_seed(as.integer(seed))
     np <- reticulate::import("numpy", convert = FALSE)
     np$random$seed(as.integer(seed))

     hp <- utils::modifyList(list(
          units_1       = 128L, units_2 = 64L, units_3 = 32L,
          dropout       = 0.3,
          rec_dropout   = 0.10,
          l2            = 5e-4,
          lr            = 0.001,
          batch_size    = 128L,
          epochs        = 200L,
          patience      = 10L,
          rlr_factor    = 0.5,
          rlr_patience  = 8L,
          min_lr        = 1e-6,
          restore_best_weights = TRUE,
          n_epochs_fixed = NULL,
          country_dim          = 16L,
          partial_pool_lambda  = 0.1,
          region_l2            = 1e-4,
          sample_weights       = "balanced_uniform",
          balance_R            = 1.0,
          loss_kind            = "bce",
          # Loss internals (passive under bce + balanced_uniform; defaults match
          # the B4 fixture pins). Threaded so a research override is honored.
          logit_eps            = 1e-6, logit_clip = 6,
          sw_offset            = 0.1,  sw_min      = 0.1,
          sw_offset_quad       = 0.05, sw_min_quad = 0.05
     ), hyperparams)

     enc <- data_bundle$encoders
     if (is.null(enc))
          stop(".psi_fit_predict_lstm: gauge_A requires data_bundle$encoders")

     lc <- .psi_configure_loss(
          data_bundle$y_train, data_bundle$y_val,
          sample_weights  = hp$sample_weights,
          loss_kind       = hp$loss_kind,
          balance_R       = hp$balance_R %||% 1.0,
          logit_eps       = hp$logit_eps %||% 1e-6,
          logit_clip      = hp$logit_clip %||% 6,
          sw_offset       = hp$sw_offset %||% 0.1,
          sw_min          = hp$sw_min %||% 0.1,
          sw_offset_quad  = hp$sw_offset_quad %||% 0.05,
          sw_min_quad     = hp$sw_min_quad %||% 0.05,
          country_balance = isTRUE(hp$country_balance),
          country_train   = data_bundle$country_ids_train,
          country_val     = data_bundle$country_ids_val,
          confidence_weight_train = data_bundle$confidence_weight_train,
          confidence_weight_val   = data_bundle$confidence_weight_val)

     timesteps  <- dim(data_bundle$X_train)[2]
     n_features <- dim(data_bundle$X_train)[3]

     # ---- Shared LSTM trunk ------------------------------------------------
     build_trunk <- function(input_feat) {
          x <- keras3::layer_lstm(input_feat,
               units = hp$units_1, return_sequences = TRUE,
               kernel_regularizer = keras3::regularizer_l2(hp$l2),
               recurrent_dropout  = hp$rec_dropout, name = "lstm1")
          x <- keras3::layer_dropout(x, rate = hp$dropout, name = "drop1")
          x <- keras3::layer_lstm(x,
               units = hp$units_2, return_sequences = TRUE,
               kernel_regularizer = keras3::regularizer_l2(hp$l2),
               recurrent_dropout  = hp$rec_dropout, name = "lstm2")
          x <- keras3::layer_dropout(x, rate = hp$dropout, name = "drop2")
          x <- keras3::layer_lstm(x,
               units = hp$units_3, return_sequences = FALSE,
               kernel_regularizer = keras3::regularizer_l2(hp$l2),
               recurrent_dropout  = hp$rec_dropout, name = "lstm3")
          x <- keras3::layer_dropout(x, rate = hp$dropout, name = "drop3")
          x
     }

     skip_region      <- enc$n_regions <= 1L
     use_partial_pool <- isTRUE(hp$partial_pool_lambda > 0)
     film_dim         <- as.integer(hp$units_3)

     input_feat    <- keras3::layer_input(shape = c(timesteps, n_features),
                                          name = "features")
     input_country <- keras3::layer_input(shape = 1L, dtype = "int32",
                                          name = "country_id")
     input_region  <- keras3::layer_input(shape = 1L, dtype = "int32",
                                          name = "region_id")

     z <- build_trunk(input_feat)                       # (B, units_3)

     # ---- Region FiLM (skipped via frozen zero-tap when only one region) ----
     if (skip_region) {
          r_tap <- input_region |>
               keras3::layer_embedding(
                    input_dim              = max(enc$n_regions, 1L),
                    output_dim             = film_dim,
                    embeddings_initializer = "zeros",
                    trainable              = FALSE,
                    name = "region_noop_tap") |>
               keras3::layer_flatten(name = "region_noop_flat")
          z_r <- keras3::op_add(z, r_tap)
     } else {
          r_vec <- input_region |>
               keras3::layer_embedding(
                    input_dim  = enc$n_regions,
                    output_dim = hp$country_dim,
                    embeddings_regularizer = keras3::regularizer_l2(hp$region_l2),
                    name = "region_embedding") |>
               keras3::layer_flatten(name = "region_flat")
          # tanh gamma_r bounds (1 + gamma_r) in [0, 2]; identity at init.
          gamma_r <- keras3::layer_dense(r_vec, units = film_dim,
                                         activation = "tanh",
                                         name = "film_gamma_region")
          beta_r  <- keras3::layer_dense(r_vec, units = film_dim,
                                         activation = "linear",
                                         name = "film_beta_region")
          one_r <- keras3::op_ones_like(gamma_r)
          z_r <- keras3::op_add(
               keras3::op_multiply(z, keras3::op_add(one_r, gamma_r)),
               beta_r)
     }

     # ---- Country-deviation FiLM (zero-init; partial pooling = L2) ----------
     # c_dev is the deviation of each country's modulation from its region's.
     # Zero-init => data-sparse countries inherit pure regional modulation; the
     # L2 penalty (partial_pool_lambda) pulls them back toward zero deviation.
     country_dev_layer <- keras3::layer_embedding(
          input_dim                = enc$n_countries,
          output_dim               = hp$country_dim,
          embeddings_initializer   = "zeros",
          embeddings_regularizer   = if (use_partial_pool)
               keras3::regularizer_l2(hp$partial_pool_lambda) else NULL,
          name = "country_deviation_embedding")
     c_dev <- input_country |> country_dev_layer() |>
          keras3::layer_flatten(name = "country_dev_flat")

     # tanh gamma_c bounds country deviation scaling to (1 + gamma_c) in [0, 2];
     # with zero-init c_dev the country FiLM is identity at init and departs from
     # it only where data supports it.
     gamma_c <- keras3::layer_dense(c_dev, units = film_dim,
                                    activation = "tanh",
                                    name = "film_gamma_country")
     beta_c  <- keras3::layer_dense(c_dev, units = film_dim,
                                    activation = "linear",
                                    name = "film_beta_country")
     one_c <- keras3::op_ones_like(gamma_c)
     z_c <- keras3::op_add(
          keras3::op_multiply(z_r, keras3::op_add(one_c, gamma_c)),
          beta_c)

     out <- keras3::layer_dense(z_c, units = 1, activation = lc$activation,
                                name = "out_head")
     model <- keras3::keras_model(
          inputs  = list(input_feat, input_country, input_region),
          outputs = out)

     keras3::compile(model,
          optimizer = keras3::optimizer_adam(learning_rate = hp$lr),
          loss      = lc$loss,
          metrics   = list(lc$metric))

     result <- .psi_keras_fit_and_eval(model, lc, hp, data_bundle,
                                       hierarchical = TRUE)
     result$loss_type      <- hp$loss_kind %||% "bce"
     result$arch_kind      <- "hierarchical"
     result$hier_mode      <- "film"
     result$sample_weights <- hp$sample_weights
     result
}
