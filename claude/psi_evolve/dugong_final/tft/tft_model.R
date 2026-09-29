# =============================================================================
# tft_model.R -- Temporal Fusion Transformer (Lim et al. 2019, arXiv:1912.09363)
# in keras3-R, as a STANDALONE psi model.
#
# WHY STANDALONE. It is not a trunk. TFT's whole point is routing three input
# types differently -- static, known-future, observed-past -- so it subsumes the
# FiLM head rather than slotting into it. Built independently, it touches no
# production code; comparability comes from emitting the SAME artifact the
# existing arms do (iso_code, date, psi, q025, q25, q75, q975 per cutoff).
#
# WHY IT MATTERS HERE. Two of phase 1's findings are structural, and TFT
# addresses both by construction rather than by tuning:
#   * lead = 0 -- the production model is a NOWCASTER and its "forecast" is a
#     side-effect of being fed covariates that extend past the cutoff. TFT is
#     multi-horizon by design: it predicts H steps ahead from an origin, with
#     known-future covariates as a FIRST-CLASS input type.
#   * intervals -- psi's q025/q25/q75/q975 are currently bolted on. TFT emits
#     them natively from a pinball (quantile) loss.
#
# No new dependency: everything below is keras3 1.5.1 primitives.
# =============================================================================
suppressMessages(library(keras3))

`%||%` <- function(a, b) if (is.null(a)) b else a

# KerasTensor shape helpers. dim() does NOT work on a KerasTensor -- it returns
# zero-length, which silently turns a shape test into `if (logical(0))`.
.ndim    <- function(x) length(x$shape)
.lastdim <- function(x) { s <- x$shape; as.integer(s[[length(s)]]) }
.timedim <- function(x) { s <- x$shape; as.integer(s[[2L]]) }

# ---- Gated Residual Network: TFT's basic block ------------------------------
# GRN(a, c) = LayerNorm( a + GLU( W2 * ELU(W1 a + W3 c) ) ). The gate lets the
# network suppress the block entirely, which is what makes a deep stack safe on
# small data -- the relevant property for ~20k sequences.
.grn <- function(x, units, context = NULL, dropout = 0.1, name) {
     h <- keras3::layer_dense(x, units = units, name = paste0(name, "_w1"))
     if (!is.null(context)) {
          cc <- keras3::layer_dense(context, units = units, use_bias = FALSE,
                                    name = paste0(name, "_wc"))
          # context is (B, units); x may be (B, T, units) -> broadcast over time
          if (.ndim(x) == 3L)
               cc <- keras3::layer_reshape(cc, target_shape = c(1L, units),
                                           name = paste0(name, "_wc_rs"))
          h <- keras3::op_add(h, cc)
     }
     h <- keras3::layer_activation(h, activation = "elu", name = paste0(name, "_elu"))
     h <- keras3::layer_dense(h, units = units, name = paste0(name, "_w2"))
     h <- keras3::layer_dropout(h, rate = dropout, name = paste0(name, "_do"))
     g <- keras3::layer_dense(h, units = units, activation = "sigmoid",
                              name = paste0(name, "_glu_g"))
     v <- keras3::layer_dense(h, units = units, name = paste0(name, "_glu_v"))
     h <- keras3::op_multiply(g, v)
     skip <- if (.lastdim(x) != units)
          keras3::layer_dense(x, units = units, name = paste0(name, "_skip")) else x
     keras3::layer_layer_normalization(keras3::op_add(skip, h),
                                       name = paste0(name, "_ln"))
}

# ---- Variable Selection Network ---------------------------------------------
# Learns a softmax weight per INPUT VARIABLE (optionally conditioned on static
# context), then returns the weighted sum of per-variable GRN transforms. This
# is the piece that tells us WHICH of the 38 covariates the model uses -- the
# interpretability we have never had, and directly relevant to the open question
# of whether the covariates carry 12-week signal at all.
#   x: (B, T, n_vars) for temporal, (B, n_vars) for static; each var is scalar.
.vsn <- function(x, n_vars, units, context = NULL, dropout = 0.1, name) {
     temporal <- .ndim(x) == 3L
     # per-variable transform: project each scalar var to `units`
     flat <- if (temporal) x else x
     sel <- .grn(flat, units = n_vars, context = context, dropout = dropout,
                 name = paste0(name, "_sel"))
     w <- keras3::layer_activation(sel, activation = "softmax",
                                   name = paste0(name, "_softmax"))
     w <- if (temporal)
          keras3::layer_reshape(w, target_shape = c(.timedim(x), n_vars, 1L),
                                name = paste0(name, "_w_rs"))
          else keras3::layer_reshape(w, target_shape = c(n_vars, 1L),
                                     name = paste0(name, "_w_rs"))
     # each variable gets its own GRN on its scalar value
     xs <- if (temporal)
          keras3::layer_reshape(x, target_shape = c(.timedim(x), n_vars, 1L),
                                name = paste0(name, "_x_rs"))
          else keras3::layer_reshape(x, target_shape = c(n_vars, 1L),
                                     name = paste0(name, "_x_rs"))
     xt <- .grn(xs, units = units, dropout = dropout, name = paste0(name, "_var"))
     # weighted sum over the variable axis
     keras3::op_sum(keras3::op_multiply(xt, w), axis = if (temporal) -2L else -2L)
}

# ---- the model ---------------------------------------------------------------
#' @param n_static   number of static (country) covariates
#' @param n_past     number of observed-past variables (target history + covars)
#' @param n_future   number of known-future variables
#' @param lookback   encoder length L (weeks)
#' @param horizon    decoder length H (weeks) -- the forecast the model is TRAINED on
#' @param quantiles  output quantiles; the median is psi
build_tft <- function(n_static, n_past, n_future, lookback, horizon,
                      n_countries, units = 32L, n_heads = 4L, dropout = 0.1,
                      quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975)) {

     in_static  <- keras3::layer_input(shape = n_static,             name = "static")
     in_country <- keras3::layer_input(shape = 1L, dtype = "int32",  name = "country")
     in_past    <- keras3::layer_input(shape = c(lookback, n_past),  name = "past")
     in_future  <- keras3::layer_input(shape = c(horizon,  n_future),name = "future")

     # country identity as an extra static variable block
     c_emb <- in_country |>
          keras3::layer_embedding(input_dim = n_countries, output_dim = units,
                                  name = "country_emb") |>
          keras3::layer_flatten(name = "country_flat")

     # ---- static encoders: four contexts, exactly as in the paper ------------
     s_vec <- .vsn(in_static, n_vars = n_static, units = units,
                   dropout = dropout, name = "vsn_static")
     s_vec <- keras3::layer_layer_normalization(
                  keras3::op_add(s_vec, c_emb), name = "static_plus_country")
     c_sel <- .grn(s_vec, units, dropout = dropout, name = "ctx_sel")   # variable selection
     c_enr <- .grn(s_vec, units, dropout = dropout, name = "ctx_enr")   # static enrichment
     c_h   <- .grn(s_vec, units, dropout = dropout, name = "ctx_h")     # LSTM h0
     c_c   <- .grn(s_vec, units, dropout = dropout, name = "ctx_c")     # LSTM c0

     # ---- temporal variable selection, conditioned on static -----------------
     past_sel   <- .vsn(in_past,   n_past,   units, context = c_sel,
                        dropout = dropout, name = "vsn_past")
     future_sel <- .vsn(in_future, n_future, units, context = c_sel,
                        dropout = dropout, name = "vsn_future")

     # ---- seq2seq LSTM, initial state from the static encoders ---------------
     enc_layer <- keras3::layer_lstm(units = units, return_sequences = TRUE,
                                     return_state = TRUE, name = "enc_lstm")
     enc_out <- enc_layer(past_sel, initial_state = list(c_h, c_c))
     enc_seq <- enc_out[[1]]; h_n <- enc_out[[2]]; c_n <- enc_out[[3]]
     dec_layer <- keras3::layer_lstm(units = units, return_sequences = TRUE,
                                     name = "dec_lstm")
     dec_seq <- dec_layer(future_sel, initial_state = list(h_n, c_n))
     temporal <- keras3::layer_concatenate(list(enc_seq, dec_seq), axis = 2L,
                                           name = "enc_dec")
     skip_in  <- keras3::layer_concatenate(list(past_sel, future_sel), axis = 2L,
                                           name = "skip_in")
     temporal <- keras3::layer_layer_normalization(
                     keras3::op_add(temporal, skip_in), name = "seq_ln")

     # ---- static enrichment + interpretable multi-head attention -------------
     enriched <- .grn(temporal, units, context = c_enr, dropout = dropout,
                      name = "enrich")
     att <- keras3::layer_multi_head_attention(
                 num_heads = n_heads, key_dim = max(1L, units %/% n_heads),
                 dropout = dropout, name = "attn")(enriched, enriched, enriched,
                 use_causal_mask = TRUE)
     att <- keras3::layer_layer_normalization(
                 keras3::op_add(att, enriched), name = "attn_ln")
     ff  <- .grn(att, units, dropout = dropout, name = "ff")
     out <- keras3::layer_layer_normalization(
                 keras3::op_add(ff, temporal), name = "out_ln")

     # ---- decoder positions only, quantile head ------------------------------
     # take the decoder positions. op_slice is 0-indexed on the backend, so the
     # decoder block starts at `lookback`.
     dec_out <- keras3::layer_lambda(
          out,
          f = function(z) keras3::op_slice(
                    z, start_indices = c(0L, as.integer(lookback), 0L),
                    shape = c(-1L, as.integer(horizon), as.integer(units))),
          name = "take_decoder")
     yhat <- keras3::layer_dense(dec_out, units = length(quantiles),
                                 name = "quantiles")       # (B, H, n_q)

     keras3::keras_model(
          inputs  = list(static = in_static, country = in_country,
                         past = in_past, future = in_future),
          outputs = yhat, name = "tft_psi")
}

# ---- pinball (quantile) loss -------------------------------------------------
# Sum of pinball losses over the quantile set. This is what makes the intervals
# a TRAINED property rather than a residual assumption bolted on afterwards.
make_quantile_loss <- function(quantiles) {
     q <- keras3::op_convert_to_tensor(as.numeric(quantiles), dtype = "float32")
     function(y_true, y_pred) {
          # y_true (B, H, 1) broadcast against y_pred (B, H, n_q)
          e <- keras3::op_subtract(y_true, y_pred)
          keras3::op_mean(keras3::op_maximum(
               keras3::op_multiply(q, e),
               keras3::op_multiply(keras3::op_subtract(q, 1), e)))
     }
}
