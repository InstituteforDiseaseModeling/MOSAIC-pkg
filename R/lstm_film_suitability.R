# =============================================================================
# lstm_film_suitability.R — The gauge_A sequence model for the lstm_v2
# suitability path: a 3-stack trunk (128 -> 64 -> 32; LSTM by default, with GRU
# and dilated-causal-TCN variants selectable via
# `arch_control$trunk`) with hierarchical FiLM modulation
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
     # Focus TF's intra/inter-op thread pools (env-driven) BEFORE any op is built
     # so N parallel seed workers don't each size their pool to the whole box --
     # the OMP/BLAS pin does NOT govern TF's Eigen intra-op pool. The per-worker
     # budget is set by .psi_fit_seeds_parallel(); a serial fit reads the whole-
     # process budget. tryCatch is silent because the call errors (no-op) once the
     # runtime is initialized, e.g. a 2nd seed fit in the same process.
     .tf_intra <- suppressWarnings(as.integer(Sys.getenv("MOSAIC_PSI_TF_INTRAOP", "")))
     .tf_inter <- suppressWarnings(as.integer(Sys.getenv("MOSAIC_PSI_TF_INTEROP", "")))
     if (!is.na(.tf_intra) && .tf_intra > 0L)
          try(tf$config$threading$set_intra_op_parallelism_threads(.tf_intra), silent = TRUE)
     if (!is.na(.tf_inter) && .tf_inter > 0L)
          try(tf$config$threading$set_inter_op_parallelism_threads(.tf_inter), silent = TRUE)
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
          sw_offset_quad       = 0.05, sw_min_quad = 0.05,
          # Trunk registry (psi_evolve `N` arms). "lstm" is the production trunk
          # and is byte-identical to the pre-registry code path.
          trunk                = "lstm",
          tcn_kernel           = 3L,
          tcn_dilations        = c(1L, 2L, 4L),
          # Country-variability capacity (psi_evolve `N` arms). Both default to
          # the production behaviour, so an unset spec is byte-identical.
          film_input           = FALSE,   # N5: condition the trunk's INPUTS
          gamma_scale          = 1        # N6: >1 lets country modulation flip sign
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

     # ---- Shared sequence trunk (registry: lstm | gru | tcn) ---------------
     # The trunk is the ONE thing an architecture arm varies. Every variant must
     # return (B, units_3) so the hierarchical-FiLM stack, the head, the loss and
     # the feature pipeline below are untouched -- that is what makes a trunk swap
     # a single registered change (psi_evolve PROTOCOL section 2) rather than a
     # confounded bundle. `trunk = "lstm"` reproduces the production trunk
     # exactly; the other two are unreachable unless asked for by name.
     #
     # WHY THIS EXISTS. The April 2026 architecture bake-off
     # (claude/psi_arch_bench/) ranked six trunks, and `tcn_v1` led at both 4 and
     # 13 weeks -- but its LSTM entry was the THEN-production 3-layer LSTM, and
     # this hierarchical-FiLM model landed 2026-06-07, after that bake-off. So no
     # alternative trunk has ever been compared against the model now in
     # production. This registry is how that comparison becomes possible without
     # also changing the FiLM conditioning, the head or the features.
     trunk_kind <- tolower(as.character(hp$trunk %||% "lstm"))
     if (!trunk_kind %in% c("lstm", "gru", "tcn"))
          stop(".psi_fit_predict_lstm: unknown trunk '", trunk_kind,
               "'. Supported: 'lstm' (production), 'gru', 'tcn'.", call. = FALSE)

     build_trunk <- function(input_feat) {
          if (trunk_kind == "lstm") {
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
               return(x)
          }

          if (trunk_kind == "gru") {
               # Same depth, widths, regularisation and dropout schedule as the
               # LSTM trunk; only the recurrent cell differs.
               x <- keras3::layer_gru(input_feat,
                    units = hp$units_1, return_sequences = TRUE,
                    kernel_regularizer = keras3::regularizer_l2(hp$l2),
                    recurrent_dropout  = hp$rec_dropout, name = "gru1")
               x <- keras3::layer_dropout(x, rate = hp$dropout, name = "drop1")
               x <- keras3::layer_gru(x,
                    units = hp$units_2, return_sequences = TRUE,
                    kernel_regularizer = keras3::regularizer_l2(hp$l2),
                    recurrent_dropout  = hp$rec_dropout, name = "gru2")
               x <- keras3::layer_dropout(x, rate = hp$dropout, name = "drop2")
               x <- keras3::layer_gru(x,
                    units = hp$units_3, return_sequences = FALSE,
                    kernel_regularizer = keras3::regularizer_l2(hp$l2),
                    recurrent_dropout  = hp$rec_dropout, name = "gru3")
               x <- keras3::layer_dropout(x, rate = hp$dropout, name = "drop3")
               return(x)
          }

          # TCN: dilated CAUSAL convolutions. `padding = "causal"` is what makes
          # this a forecaster rather than a smoother -- timestep t sees only
          # t' <= t, so no target-adjacent covariate can leak backwards through
          # the receptive field. Causal padding preserves sequence length, so the
          # stack ends by taking the LAST timestep (the window's target anchor,
          # matching return_sequences = FALSE on the recurrent trunks) via a crop
          # + flatten rather than a pooling layer, which would average away the
          # recency the recurrent trunks keep.
          dil <- as.integer(hp$tcn_dilations %||% c(1L, 2L, 4L))
          if (length(dil) != 3L)
               stop(".psi_fit_predict_lstm: tcn_dilations must have length 3 (one per stack level).",
                    call. = FALSE)
          units <- c(as.integer(hp$units_1), as.integer(hp$units_2), as.integer(hp$units_3))
          x <- input_feat
          for (i in 1:3) {
               x <- keras3::layer_conv_1d(x,
                    filters = units[i], kernel_size = as.integer(hp$tcn_kernel %||% 3L),
                    padding = "causal", dilation_rate = dil[i], activation = "relu",
                    kernel_regularizer = keras3::regularizer_l2(hp$l2),
                    name = sprintf("tcn%d", i))
               x <- keras3::layer_dropout(x, rate = hp$dropout,
                                          name = sprintf("drop%d", i))
          }
          # (B, timesteps, units_3) -> (B, 1, units_3) -> (B, units_3)
          x <- keras3::layer_cropping_1d(x, cropping = c(as.integer(timesteps - 1L), 0L),
                                         name = "tcn_last_step")
          keras3::layer_flatten(x, name = "tcn_flat")
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

     # ---- N5: INPUT-FiLM (optional) ---------------------------------------
     # WHY. The production model conditions only the trunk's 32-dim OUTPUT:
     # z_r = (1+g_r)z + b_r then z_c = (1+g_c)z_r + b_c. The recurrent weights
     # that process the 13-week sequence are IDENTICAL for all 40 countries, so
     # no country can have its own lag structure, response timing or
     # persistence -- and those are exactly what differ. Measured on the
     # production grid: the horizon decay ratio spans 0.083 (ZWE) to 1.387
     # (MWI), a 17x spread in the one property the architecture forces to be
     # common, and every arm's benefit splits along the snf_k5 region map
     # (snf_1 weighted -0.259 with every member non-positive, snf_2 +0.498).
     #
     # Conditioning the INPUTS instead lets a country reweight WHICH covariates
     # the shared dynamics see -- country-specific effective dynamics without
     # per-country recurrent weights. This restores the `input-FiLM` branch that
     # the gauge_A-only port dropped (see this file's header).
     #
     # Zero-init the EMBEDDING, default-init the DENSE: at init the embedding is
     # 0 so gamma_in = tanh(0) = 0 and beta_in = 0, making this an exact
     # identity -- the arm starts at the production model. Gradients still flow,
     # because the dense weights are nonzero. Zero-initialising BOTH would make
     # the branch permanently dead (zero input to the dense gives a zero weight
     # gradient, and a zero dense weight gives a zero embedding gradient); this
     # mirrors the country-deviation FiLM below, which is built the same way.
     feat_in <- input_feat
     if (isTRUE(hp$film_input)) {
          c_in <- input_country |>
               keras3::layer_embedding(
                    input_dim  = enc$n_countries,
                    output_dim = as.integer(hp$country_dim),
                    embeddings_initializer = "zeros",
                    embeddings_regularizer = if (use_partial_pool)
                         keras3::regularizer_l2(hp$partial_pool_lambda) else NULL,
                    name = "film_in_country_embedding") |>
               keras3::layer_flatten(name = "film_in_country_flat")
          g_in <- keras3::layer_dense(c_in, units = n_features, activation = "tanh",
                                      name = "film_in_gamma")
          b_in <- keras3::layer_dense(c_in, units = n_features, activation = "linear",
                                      name = "film_in_beta")
          g_in <- keras3::layer_reshape(g_in, target_shape = c(1L, n_features),
                                        name = "film_in_gamma_rs")
          b_in <- keras3::layer_reshape(b_in, target_shape = c(1L, n_features),
                                        name = "film_in_beta_rs")
          # (B, T, F) * (B, 1, F) broadcasts over the time axis
          feat_in <- keras3::op_add(
               keras3::op_multiply(input_feat,
                                   keras3::op_add(keras3::op_ones_like(g_in), g_in)),
               b_in)
     }

     z <- build_trunk(feat_in)                          # (B, units_3)

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
     # N6: `gamma_scale` > 1 widens the country modulation beyond tanh's (0,2)
     # multiplier so a country can REVERSE a trunk feature's sign, not merely
     # damp or double it. At the default 1 this is the production tanh exactly.
     # Production cannot express a covariate whose effect is opposite in two
     # regimes: the shared trunk averages them and only beta_c can compensate.
     .gs <- as.numeric(hp$gamma_scale %||% 1)
     gamma_c <- keras3::layer_dense(c_dev, units = film_dim,
                                    activation = if (.gs == 1) "tanh" else
                                         function(x) .gs * keras3::op_tanh(x),
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
     result$trunk          <- trunk_kind
     result$sample_weights <- hp$sample_weights
     result
}
