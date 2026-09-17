# =============================================================================
# loss_suitability_torch.R -- training loop for the torch backend of the lstm_v2
# suitability path. Mirrors .psi_keras_fit_and_eval() (R/loss_suitability.R)
# one-for-one: same two modes, same callbacks, same weighted loss, same return
# contract. .psi_configure_loss() is shared, unchanged, and pure R.
#
# keras `fit()` semantics reproduced deliberately:
#   * shuffle the training set every epoch; do NOT drop the last partial batch
#   * loss  = sum(w_i * l_i) / N        (keras "sum_over_batch_size")
#   * metric= sum(w_i * v_i) / sum(w_i) (keras Mean metric with sample_weight --
#              a DIFFERENT normalisation from the loss; not a typo)
#   * validation is evaluated at epoch end, unshuffled, sample-weighted
#   * EarlyStopping(min_delta = 0) and ReduceLROnPlateau(min_delta = 1e-4,
#     ABSOLUTE threshold) are hand-rolled so they match keras' comparison rule
#     rather than torch's relative-threshold default.
#
# Validation loss is accumulated over batches as sum(w*l) then divided by N once
# (the global form). keras instead averages per-batch losses, which differs only
# when the last batch is short. The global form is the stabler statistic and the
# backends are not bitwise comparable anyway.
#
# torch is in Suggests; nothing here runs unless backend = "torch".
# =============================================================================

# R torch's nn_embedding is 1-BASED. The data bundle carries 0-based ids
# (country_to_id = seq_along(iso) - 1L, for keras), so every id crossing into a
# torch embedding must be shifted. Getting this wrong silently off-by-ones every
# country's FiLM modulation.
#' @keywords internal
#' @noRd
.psi_torch_ids <- function(ids) {
     torch::torch_tensor(as.integer(ids) + 1L, dtype = torch::torch_long())
}

#' @keywords internal
#' @noRd
.psi_torch_x <- function(X) torch::torch_tensor(X, dtype = torch::torch_float())

#' @keywords internal
#' @noRd
.psi_torch_y <- function(y) {
     torch::torch_tensor(matrix(as.numeric(y), ncol = 1L),
                         dtype = torch::torch_float())
}

# Weighted loss + weighted metric for one batch, returning tensors so the caller
# can both backprop and accumulate. w may be NULL (unweighted).
#' @keywords internal
#' @noRd
.psi_torch_batch_loss <- function(pred, y, w, loss_kind) {
     if (identical(loss_kind, "bce")) {
          # clamp keeps log() finite if a sigmoid saturates
          p <- torch::torch_clamp(pred, 1e-7, 1 - 1e-7)
          l <- -(y * torch::torch_log(p) + (1 - y) * torch::torch_log(1 - p))
     } else {
          l <- (pred - y)^2
     }
     v <- torch::torch_abs(pred - y)                      # mae
     if (!is.null(w)) { l <- l * w; v <- v * w }
     list(loss_sum = torch::torch_sum(l), metric_sum = torch::torch_sum(v))
}

#' Train a torch FiLM net on data_bundle and predict on X_pred.
#'
#' val mode (default): early stopping + ReduceLROnPlateau on X_val/y_val.
#' fixed-epochs mode: hp$n_epochs_fixed set -> exactly that many epochs, no
#' validation, no early stopping (the RW-CV final refit).
#' @keywords internal
#' @noRd
.psi_torch_fit_and_eval <- function(net, lc, hp, data_bundle) {

     use_fixed <- !is.null(hp$n_epochs_fixed)
     bs        <- as.integer(hp$batch_size %||% 128L)
     loss_kind <- lc$loss_kind %||% "bce"

     xt  <- .psi_torch_x(data_bundle$X_train)
     yt  <- .psi_torch_y(lc$y_train)
     ct  <- .psi_torch_ids(data_bundle$country_ids_train)
     rt  <- .psi_torch_ids(data_bundle$region_ids_train)
     swt <- if (!is.null(lc$sample_weight_train))
          torch::torch_tensor(matrix(as.numeric(lc$sample_weight_train), ncol = 1L),
                              dtype = torch::torch_float()) else NULL
     n_train <- dim(data_bundle$X_train)[1]

     have_val <- !use_fixed
     if (have_val) {
          if (is.null(data_bundle$X_val) || is.null(lc$y_val))
               stop(".psi_torch_fit_and_eval: validation data required when n_epochs_fixed is NULL")
          xv  <- .psi_torch_x(data_bundle$X_val)
          yv  <- .psi_torch_y(lc$y_val)
          cv  <- .psi_torch_ids(data_bundle$country_ids_val)
          rv  <- .psi_torch_ids(data_bundle$region_ids_val)
          swv <- if (!is.null(lc$sample_weight_val))
               torch::torch_tensor(matrix(as.numeric(lc$sample_weight_val), ncol = 1L),
                                   dtype = torch::torch_float()) else NULL
          n_val <- dim(data_bundle$X_val)[1]
     }

     opt      <- torch::optim_adam(net$parameters, lr = hp$lr %||% 0.001)
     l2_terms <- .psi_torch_l2_terms(net, hp)

     # ---- epoch-end evaluation over a held-out set (no grad) ---------------
     eval_set <- function(x, y, cid, rid, w, n) {
          net$eval()
          ls <- 0; ms <- 0; wsum <- 0
          torch::with_no_grad({
               for (st in seq(1L, n, by = bs)) {
                    en <- min(st + bs - 1L, n)
                    idx <- torch::torch_tensor(as.integer(st:en), dtype = torch::torch_long())
                    pb <- net(x[idx, , ], cid[idx], rid[idx])
                    wb <- if (is.null(w)) NULL else w[idx, ]
                    r  <- .psi_torch_batch_loss(pb, y[idx, ], wb, loss_kind)
                    ls <- ls + as.numeric(r$loss_sum)
                    ms <- ms + as.numeric(r$metric_sum)
                    wsum <- wsum + if (is.null(w)) (en - st + 1L) else
                         as.numeric(torch::torch_sum(w[idx, ]))
               }
          })
          net$train()
          # loss: /N (keras sum_over_batch_size); metric: /sum(w) (keras Mean)
          list(loss = ls / n, metric = ms / max(wsum, .Machine$double.eps))
     }

     # ---- callback state ----------------------------------------------------
     patience     <- as.integer(hp$patience %||% 10L)
     rlr_factor   <- hp$rlr_factor %||% 0.5
     rlr_patience <- as.integer(hp$rlr_patience %||% 8L)
     min_lr       <- hp$min_lr %||% 1e-6
     restore_best <- isTRUE(hp$restore_best_weights %||% TRUE)
     max_epochs   <- if (use_fixed) as.integer(hp$n_epochs_fixed)
                     else as.integer(hp$epochs %||% 150L)

     best_loss <- Inf; best_state <- NULL; best_epoch <- max_epochs
     wait_es <- 0L; wait_lr <- 0L; rlr_best <- Inf
     epochs_run <- 0L

     t0 <- proc.time()
     net$train()
     for (ep in seq_len(max_epochs)) {
          epochs_run <- ep
          perm <- sample.int(n_train)                       # keras shuffle=TRUE
          for (st in seq(1L, n_train, by = bs)) {           # last batch NOT dropped
               en  <- min(st + bs - 1L, n_train)
               idx <- torch::torch_tensor(perm[st:en], dtype = torch::torch_long())
               opt$zero_grad()
               pb <- net(xt[idx, , ], ct[idx], rt[idx])
               wb <- if (is.null(swt)) NULL else swt[idx, ]
               r  <- .psi_torch_batch_loss(pb, yt[idx, ], wb, loss_kind)
               loss <- r$loss_sum / (en - st + 1L)
               pen  <- .psi_torch_l2_penalty(l2_terms)
               if (!is.null(pen)) loss <- loss + pen
               loss$backward()
               opt$step()
          }
          if (!have_val) next

          ev <- eval_set(xv, yv, cv, rv, swv, n_val)
          vl <- ev$loss

          if (vl < best_loss) {                              # EarlyStopping, min_delta=0
               best_loss  <- vl
               best_epoch <- ep
               wait_es    <- 0L
               if (restore_best)
                    best_state <- lapply(net$state_dict(), function(z) z$clone())
          } else {
               wait_es <- wait_es + 1L
               if (wait_es >= patience) break
          }
          if (vl < rlr_best - 1e-4) {                        # ReduceLROnPlateau, ABS 1e-4
               rlr_best <- vl; wait_lr <- 0L
          } else {
               wait_lr <- wait_lr + 1L
               if (wait_lr >= rlr_patience) {
                    cur <- opt$param_groups[[1]]$lr
                    new <- max(cur * rlr_factor, min_lr)
                    if (new < cur) opt$param_groups[[1]]$lr <- new
                    wait_lr <- 0L
               }
          }
     }
     if (have_val && restore_best && !is.null(best_state))
          net$load_state_dict(best_state)
     train_minutes <- round((proc.time() - t0)["elapsed"] / 60, 2)

     if (use_fixed) {
          val_loss <- NA_real_; val_metric <- NA_real_; n_epochs <- max_epochs
     } else {
          ev <- eval_set(xv, yv, cv, rv, swv, n_val)
          val_loss <- ev$loss; val_metric <- ev$metric
          n_epochs <- epochs_run
     }

     # ---- predict -----------------------------------------------------------
     xp <- .psi_torch_x(data_bundle$X_pred)
     cp <- .psi_torch_ids(data_bundle$country_ids_pred)
     rp <- .psi_torch_ids(data_bundle$region_ids_pred)
     np_ <- dim(data_bundle$X_pred)[1]
     net$eval()
     out <- numeric(np_)
     pbs <- max(bs, 2048L)
     torch::with_no_grad({
          for (st in seq(1L, np_, by = pbs)) {
               en  <- min(st + pbs - 1L, np_)
               idx <- torch::torch_tensor(as.integer(st:en), dtype = torch::torch_long())
               out[st:en] <- as.numeric(net(xp[idx, , ], cp[idx], rp[idx]))
          }
     })
     pred <- lc$transform_pred(out)

     list(pred          = pred,
          val_loss      = val_loss,
          val_metric    = val_metric,
          train_minutes = unname(train_minutes),
          n_epochs      = n_epochs)
}
