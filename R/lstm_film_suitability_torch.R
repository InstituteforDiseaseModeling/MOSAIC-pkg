# =============================================================================
# lstm_film_suitability_torch.R -- R `torch` implementation of the gauge_A
# hierarchical-FiLM LSTM used by the lstm_v2 suitability path. Backend-parallel
# to R/lstm_film_suitability.R (keras3/TensorFlow); selected via
# arch_control$backend = "torch".
#
# Architecture is identical to the keras graph (see the sibling file):
#   features (B, T, F) --LSTM 128->64->32--> z
#   region_id  --Embedding(R, 16)--> Dense --> (gamma_r, beta_r)   [tanh gamma]
#   country_id --Embedding(C, 16, zeros)--> Dense --> (gamma_c, beta_c)
#     z_r = (1 + gamma_r) * z   + beta_r
#     z_c = (1 + gamma_c) * z_r + beta_c
#     out = sigmoid(Dense(1)(z_c))
#
# Three fidelity details that differ from torch defaults and are handled here:
#
#  (1) recurrent_dropout. PyTorch/LibTorch has no variational recurrent dropout
#      (nn_lstm's `dropout` is BETWEEN stacked layers). .psi_nn_lstm_vd wraps a
#      real nn_lstm and, ONLY when rec_dropout > 0 AND training, runs a manual
#      per-timestep loop applying a single dropout mask to h_{t-1} held fixed
#      across t (Gal & Ghahramani; this is what keras does). Otherwise it
#      delegates to the fused kernel. The parameters are the nn_lstm's own
#      either way, so inference is always fast and weight transplant works.
#
#  (2) Initialization. keras uses glorot_uniform (input kernel), orthogonal
#      (recurrent kernel) and unit_forget_bias; torch defaults to
#      U(-1/sqrt(H), 1/sqrt(H)) everywhere with no forget-bias trick. That is a
#      different model, not a different framework, so .psi_torch_init_keras_style
#      replicates the keras scheme.
#
#  (3) L2. keras regularizer_l2(l) adds l * sum(w^2) to the LOSS (gradient 2lw);
#      torch's optimizer weight_decay adds wd*w to the gradient, so matching
#      needs wd = 2l -- a silent factor of two. We instead add the penalty to
#      the loss explicitly (.psi_torch_l2_penalty), which is identical to keras
#      by construction and needs no param-group bookkeeping.
#
# torch is in Suggests; nothing here runs unless backend = "torch".
# `%||%` is provided package-wide by R/aaa_utils.R.
# =============================================================================

# ---- LSTM layer with optional variational recurrent dropout -----------------
#' @keywords internal
#' @noRd
.psi_nn_lstm_vd <- function(input_size, hidden_size, rec_dropout = 0) {
     torch::nn_module(
          "psi_nn_lstm_vd",
          initialize = function(input_size, hidden_size, rec_dropout) {
               self$hidden_size <- hidden_size
               self$rec_dropout <- rec_dropout
               self$lstm <- torch::nn_lstm(input_size, hidden_size,
                                           batch_first = TRUE)
          },
          forward = function(x, return_sequences = TRUE) {
               # Fast path: no recurrent dropout, or inference. Uses the fused
               # LibTorch kernel; h_n of a 1-layer unidirectional LSTM IS the
               # last output step, so return_sequences = FALSE is exact.
               if (self$rec_dropout <= 0 || !self$training) {
                    res <- self$lstm(x)
                    if (return_sequences) return(res[[1]])
                    return(res[[2]][[1]]$squeeze(1))
               }
               # Variational path. The input projection is precomputed for all
               # timesteps in ONE matmul (as keras does), so the R-level loop
               # only carries the recurrent matmul and the gate nonlinearities.
               # NOTE: R torch numbers RNN layers from 1 (weight_ih_l1), unlike
               # PyTorch's weight_ih_l0.
               W_ih <- self$lstm$weight_ih_l1
               W_hh <- self$lstm$weight_hh_l1
               b_ih <- self$lstm$bias_ih_l1
               b_hh <- self$lstm$bias_hh_l1
               B  <- x$size(1); Tn <- x$size(2); H <- self$hidden_size
               gx <- torch::torch_matmul(x, W_ih$t()) + b_ih          # (B,T,4H)
               h <- torch::torch_zeros(B, H, dtype = x$dtype, device = x$device)
               cc <- torch::torch_zeros(B, H, dtype = x$dtype, device = x$device)
               keep <- 1 - self$rec_dropout
               # ONE mask, drawn per forward call, held fixed across timesteps
               # (variational); inverted scaling, matching keras.
               mask <- (torch::torch_rand(B, H, dtype = x$dtype, device = x$device) < keep)
               mask <- mask$to(dtype = x$dtype) / keep
               outs <- vector("list", Tn)
               for (tt in seq_len(Tn)) {
                    g <- gx[ , tt, ] + torch::torch_matmul(h * mask, W_hh$t()) + b_hh
                    ch <- torch::torch_chunk(g, 4L, dim = 2L)   # PyTorch order i,f,g,o
                    i_g <- torch::torch_sigmoid(ch[[1]])
                    f_g <- torch::torch_sigmoid(ch[[2]])
                    g_g <- torch::torch_tanh(ch[[3]])
                    o_g <- torch::torch_sigmoid(ch[[4]])
                    cc <- f_g * cc + i_g * g_g
                    h  <- o_g * torch::torch_tanh(cc)
                    outs[[tt]] <- h
               }
               if (!return_sequences) return(h)
               torch::torch_stack(outs, dim = 2L)
          }
     )(input_size, hidden_size, rec_dropout)
}

# ---- The hierarchical-FiLM network ------------------------------------------
#' @keywords internal
#' @noRd
.psi_torch_film_net <- function(n_features, n_countries, n_regions,
                                units_1 = 128L, units_2 = 64L, units_3 = 32L,
                                country_dim = 16L, dropout = 0.3,
                                rec_dropout = 0.10, activation = "sigmoid") {
     torch::nn_module(
          "psi_film_net",
          initialize = function() {
               self$skip_region <- n_regions <= 1L
               self$activation  <- activation
               self$lstm1 <- .psi_nn_lstm_vd(n_features, units_1, rec_dropout)
               self$lstm2 <- .psi_nn_lstm_vd(units_1,    units_2, rec_dropout)
               self$lstm3 <- .psi_nn_lstm_vd(units_2,    units_3, rec_dropout)
               self$drop1 <- torch::nn_dropout(dropout)
               self$drop2 <- torch::nn_dropout(dropout)
               self$drop3 <- torch::nn_dropout(dropout)
               if (!self$skip_region) {
                    self$region_embedding   <- torch::nn_embedding(n_regions, country_dim)
                    self$film_gamma_region  <- torch::nn_linear(country_dim, units_3)
                    self$film_beta_region   <- torch::nn_linear(country_dim, units_3)
               }
               self$country_deviation_embedding <- torch::nn_embedding(n_countries, country_dim)
               self$film_gamma_country <- torch::nn_linear(country_dim, units_3)
               self$film_beta_country  <- torch::nn_linear(country_dim, units_3)
               self$out_head <- torch::nn_linear(units_3, 1L)
          },
          forward = function(x, cid, rid) {
               z <- self$drop1(self$lstm1(x, return_sequences = TRUE))
               z <- self$drop2(self$lstm2(z, return_sequences = TRUE))
               z <- self$drop3(self$lstm3(z, return_sequences = FALSE))   # (B, U3)
               if (self$skip_region) {
                    z_r <- z          # frozen zero tap in keras == identity here
               } else {
                    r_vec   <- self$region_embedding(rid)$squeeze(2)
                    gamma_r <- torch::torch_tanh(self$film_gamma_region(r_vec))
                    beta_r  <- self$film_beta_region(r_vec)
                    z_r     <- z * (1 + gamma_r) + beta_r
               }
               c_dev   <- self$country_deviation_embedding(cid)$squeeze(2)
               gamma_c <- torch::torch_tanh(self$film_gamma_country(c_dev))
               beta_c  <- self$film_beta_country(c_dev)
               z_c     <- z_r * (1 + gamma_c) + beta_c
               out <- self$out_head(z_c)
               if (identical(self$activation, "sigmoid")) torch::torch_sigmoid(out) else out
          }
     )()
}

# ---- keras-style initialization ---------------------------------------------
# keras: LSTM kernel glorot_uniform, recurrent_kernel orthogonal, bias zeros
# with unit_forget_bias (forget slice = 1); Dense glorot_uniform + zero bias;
# Embedding RandomUniform(-0.05, 0.05). torch defaults to none of these.
#' @keywords internal
#' @noRd
.psi_torch_init_keras_style <- function(net) {
     torch::with_no_grad({
          for (nm in c("lstm1", "lstm2", "lstm3")) {
               l <- net[[nm]]$lstm
               H <- l$hidden_size
               torch::nn_init_xavier_uniform_(l$weight_ih_l1)
               torch::nn_init_orthogonal_(l$weight_hh_l1)
               torch::nn_init_zeros_(l$bias_ih_l1)
               torch::nn_init_zeros_(l$bias_hh_l1)
               # PyTorch gate order (i, f, g, o): forget slice is rows H+1..2H.
               l$bias_ih_l1[(H + 1):(2 * H)] <- 1
          }
          dense <- c("film_gamma_country", "film_beta_country", "out_head")
          if (!net$skip_region)
               dense <- c(dense, "film_gamma_region", "film_beta_region")
          for (nm in dense) {
               torch::nn_init_xavier_uniform_(net[[nm]]$weight)
               torch::nn_init_zeros_(net[[nm]]$bias)
          }
          if (!net$skip_region)
               torch::nn_init_uniform_(net$region_embedding$weight, -0.05, 0.05)
          # Country deviation is a ZERO-init deviation from the region: countries
          # with no data inherit pure regional modulation by construction.
          torch::nn_init_zeros_(net$country_deviation_embedding$weight)
     })
     invisible(net)
}

# ---- Explicit L2 penalty (keras semantics: l * sum(w^2) added to the loss) ---
#' @keywords internal
#' @noRd
.psi_torch_l2_terms <- function(net, hp) {
     terms <- list()
     if ((hp$l2 %||% 0) > 0) {
          for (nm in c("lstm1", "lstm2", "lstm3")) {
               l <- net[[nm]]$lstm
               terms[[length(terms) + 1L]] <- list(w = l$weight_ih_l1, lambda = hp$l2)
               terms[[length(terms) + 1L]] <- list(w = l$weight_hh_l1, lambda = hp$l2)
          }
     }
     if (!net$skip_region && (hp$region_l2 %||% 0) > 0)
          terms[[length(terms) + 1L]] <- list(w = net$region_embedding$weight,
                                              lambda = hp$region_l2)
     if ((hp$partial_pool_lambda %||% 0) > 0)
          terms[[length(terms) + 1L]] <- list(w = net$country_deviation_embedding$weight,
                                              lambda = hp$partial_pool_lambda)
     terms
}

#' @keywords internal
#' @noRd
.psi_torch_l2_penalty <- function(terms) {
     if (length(terms) == 0L) return(NULL)
     p <- NULL
     for (tm in terms) {
          v <- tm$lambda * torch::torch_sum(tm$w^2)
          p <- if (is.null(p)) v else p + v
     }
     p
}

# ---- Fit + predict (the backend contract) -----------------------------------
#' Fit the torch hierarchical-FiLM LSTM and predict over X_pred.
#'
#' Signature and return value match .psi_fit_predict_lstm() (the keras backend)
#' exactly, so .psi_fit_predict_rw_cv() / .psi_run_seed_ensemble() are unchanged.
#' @keywords internal
#' @noRd
.psi_fit_predict_lstm_torch <- function(data_bundle, seed = 11L, hyperparams = list()) {
     set.seed(seed)
     torch::torch_manual_seed(as.integer(seed))
     # Per-worker thread slice (mirrors the keras backend's TF intra-op cap, but
     # torch honours this at any time -- there is no "must precede the first op"
     # hazard).
     .n_intra <- suppressWarnings(as.integer(Sys.getenv("MOSAIC_PSI_TORCH_THREADS", "")))
     if (!is.na(.n_intra) && .n_intra > 0L)
          try(torch::torch_set_num_threads(.n_intra), silent = TRUE)

     hp <- utils::modifyList(list(
          units_1       = 128L, units_2 = 64L, units_3 = 32L,
          dropout       = 0.3,
          rec_dropout   = 0.10,
          l2            = 5e-4,
          lr            = 0.001,
          adam_eps      = 1e-7,   # keras optimizer_adam default (torch's is 1e-8)
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
          logit_eps            = 1e-6, logit_clip = 6,
          sw_offset            = 0.1,  sw_min      = 0.1,
          sw_offset_quad       = 0.05, sw_min_quad = 0.05
     ), hyperparams)

     enc <- data_bundle$encoders
     if (is.null(enc))
          stop(".psi_fit_predict_lstm_torch: gauge_A requires data_bundle$encoders")

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

     n_features <- dim(data_bundle$X_train)[3]

     net <- .psi_torch_film_net(
          n_features  = n_features,
          n_countries = enc$n_countries,
          n_regions   = enc$n_regions,
          units_1     = as.integer(hp$units_1),
          units_2     = as.integer(hp$units_2),
          units_3     = as.integer(hp$units_3),
          country_dim = as.integer(hp$country_dim),
          dropout     = hp$dropout,
          rec_dropout = hp$rec_dropout,
          activation  = lc$activation)
     .psi_torch_init_keras_style(net)

     result <- .psi_torch_fit_and_eval(net, lc, hp, data_bundle)
     result$loss_type      <- hp$loss_kind %||% "bce"
     result$arch_kind      <- "hierarchical"
     result$hier_mode      <- "film"
     result$sample_weights <- hp$sample_weights
     result$backend        <- "torch"
     result
}
