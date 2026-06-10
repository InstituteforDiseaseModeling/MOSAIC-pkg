# =============================================================================
# loss_suitability.R — Loss configuration + keras fit/eval for the lstm_v2
# ("gauge_A") suitability path. Ported from the MOSAIC-Mozambique sandbox
# (loss.R), gauge_A defaults (bce + balanced_uniform + confidence-weight
# overlay). Correction vs sandbox: the ReduceLROnPlateau floor is threaded from
# hp$min_lr (the sandbox hardcoded 1e-6).
#
# keras3 is in Suggests; these functions only run inside the lstm_v2 path, which
# the dispatcher reaches only when keras3 is available (the orchestrator checks).
# `%||%` is provided package-wide by R/aaa_utils.R.
# =============================================================================

#' Build a loss-and-target configuration with optional sample weights.
#'
#' Two loss kinds: "bce" (default; sigmoid output, BCE on raw \[0,1\] target — the
#' gauge_A default) and "mse_logit" (linear output, MSE on logit-transformed
#' target). Sample weighting addresses the zero-week class imbalance (~72% of
#' all-MOSAIC training weeks are zero, ratio ~2.5:1; higher for individual
#' high-incidence countries, e.g. ~88% for MOZ-only):
#' "balanced_uniform" (default) equalises aggregate loss from zero vs nonzero
#' rows (ratio 1:balance_R). A per-row confidence-weight overlay is applied LAST,
#' total-weight-preserving, so AI-mined rows (cw < 1) contribute proportionally
#' less gradient without changing the overall loss scale.
#' @keywords internal
#' @noRd
.psi_configure_loss <- function(y_train_raw, y_val_raw = NULL,
                                sample_weights = "balanced_uniform",
                                sw_offset = 0.1,
                                sw_min    = 0.1,
                                sw_offset_quad = 0.05,
                                sw_min_quad    = 0.05,
                                balance_R      = 1.0,
                                loss_kind      = "bce",
                                logit_eps      = 1e-6,
                                logit_clip     = 6,
                                country_balance  = FALSE,
                                country_train    = NULL,
                                country_val      = NULL,
                                confidence_weight_train = NULL,
                                confidence_weight_val   = NULL) {
     if (identical(loss_kind, "bce")) {
          transform_y <- function(y) if (is.null(y)) NULL else pmax(0, pmin(1, y))
          cfg <- list(
               activation     = "sigmoid",
               loss           = "binary_crossentropy",
               metric         = "mae",
               transform_pred = function(p) pmax(0, pmin(1, p))
          )
     } else if (identical(loss_kind, "mse_logit")) {
          transform_y <- function(y) {
               if (is.null(y)) return(NULL)
               yc <- pmax(logit_eps, pmin(1 - logit_eps, pmax(0, pmin(1, y))))
               stats::qlogis(yc)
          }
          cfg <- list(
               activation     = "linear",
               loss           = "mse",
               metric         = "mae",
               transform_pred = function(p) stats::plogis(pmax(-logit_clip,
                                                               pmin(logit_clip, p)))
          )
     } else {
          stop(".psi_configure_loss: unknown loss_kind '", loss_kind,
               "' (expected 'bce' or 'mse_logit')")
     }
     cfg$y_train <- transform_y(y_train_raw)
     cfg$y_val   <- transform_y(y_val_raw)
     cfg$loss_kind <- loss_kind

     # Sample weighting (computed from RAW intensity y, not the transformed
     # target). See the architecture roxygen for the balanced_uniform rationale.
     make_sw <- switch(sample_weights,
          "none"      = function(y) NULL,
          "linear"    = function(y) if (is.null(y)) NULL else
               pmax(sw_min, pmax(0, pmin(1, y)) + sw_offset),
          "quadratic" = function(y) if (is.null(y)) NULL else
               pmax(sw_min_quad, pmax(0, pmin(1, y))^2 + sw_offset_quad),
          "balanced_uniform" = function(y) {
               if (is.null(y)) return(NULL)
               y_clamped <- pmax(0, pmin(1, y))
               is_zero <- y_clamped == 0
               n_zero  <- sum(is_zero)
               n_nz    <- sum(!is_zero)
               if (n_zero == 0L || n_nz == 0L) {
                    return(rep(1, length(y)))
               }
               w_zero <- 1
               w_nz   <- balance_R * n_zero / n_nz
               ifelse(is_zero, w_zero, w_nz)
          },
          stop("Unknown sample_weights: ", sample_weights,
               " (expected one of: none, linear, quadratic, balanced_uniform)")
     )
     cfg$sample_weight_train <- make_sw(y_train_raw)
     cfg$sample_weight_val   <- make_sw(y_val_raw)
     cfg$sample_weights      <- sample_weights

     # ---- Country-balance overlay (multiply weights by 1/sum-per-country) ---
     apply_country_balance <- function(w, c_vec) {
          if (is.null(w) || is.null(c_vec)) return(w)
          c_vec <- as.character(c_vec)
          sums <- tapply(w, c_vec, sum, na.rm = TRUE)
          sums[!is.finite(sums) | sums <= 0] <- 1
          n_countries <- length(sums)
          target <- sum(w, na.rm = TRUE) / n_countries
          w * (target / sums[c_vec])
     }
     if (isTRUE(country_balance)) {
          cfg$sample_weight_train <- apply_country_balance(
               cfg$sample_weight_train, country_train)
          cfg$sample_weight_val <- apply_country_balance(
               cfg$sample_weight_val, country_val)
     }
     cfg$country_balance <- isTRUE(country_balance)

     # ---- Confidence-weight overlay (applied LAST, total-weight-preserving) --
     # NOTE (B4-faithful behavior, flagged for the Phase-3 amplitude guard): the
     # overlay preserves the GLOBAL total weight, so when per-row cw correlates
     # with the zero/nonzero class it shifts weight BETWEEN classes and partially
     # offsets balanced_uniform. In the production data AI-mined rows (cw<1) are
     # commoner on outbreak weeks (mean cw ~0.77 nonzero vs ~0.94 zero), so CW=ON
     # shifts aggregate weight TOWARD the zero class — it DOWN-weights the outbreak
     # (nonzero) class, pulling the realized nonzero:zero ratio BELOW balance_R
     # (zero:nonzero rises ~1.0 -> ~1.2). This matches the sandbox B4 run; the
     # §8.2 amplitude guard checks pred_sd with CW on vs off.
     apply_cw_overlay <- function(w, cw) {
          if (is.null(w) || is.null(cw)) return(w)
          if (length(w) != length(cw))
               stop(sprintf(".psi_configure_loss: confidence_weight length mismatch (w=%d, cw=%d)",
                            length(w), length(cw)))
          sum_w <- sum(w, na.rm = TRUE)
          combined <- w * cw
          sum_c <- sum(combined, na.rm = TRUE)
          if (!is.finite(sum_c) || sum_c <= 0) return(w)
          combined * (sum_w / sum_c)
     }
     cfg$sample_weight_train <- apply_cw_overlay(
          cfg$sample_weight_train, confidence_weight_train)
     cfg$sample_weight_val <- apply_cw_overlay(
          cfg$sample_weight_val, confidence_weight_val)
     cfg$use_confidence_weight <- !is.null(confidence_weight_train)

     cfg
}

#' Reshape a 1D numeric target for keras fit() as (n, 1).
#' @keywords internal
#' @noRd
.psi_as_target_array <- function(y) array(y, dim = c(length(y), 1L))

#' Train a keras model on data_bundle and predict on X_pred.
#'
#' val mode (default): early stopping + ReduceLR on data_bundle$X_val/y_val.
#' fixed-epochs mode: set hp$n_epochs_fixed to an integer; trains exactly that
#' many epochs with no validation/early stopping (the RW-CV final refit).
#' @keywords internal
#' @noRd
.psi_keras_fit_and_eval <- function(model, lc, hp, data_bundle,
                                    hierarchical = FALSE) {
     use_fixed <- !is.null(hp$n_epochs_fixed)

     # hierarchical models expect (features, country_id, region_id).
     make_x <- function(X, c_ids, r_ids) {
          if (hierarchical) list(X, as.integer(c_ids), as.integer(r_ids)) else X
     }

     x_train <- make_x(data_bundle$X_train,
                       data_bundle$country_ids_train,
                       data_bundle$region_ids_train)
     y_train_arr <- .psi_as_target_array(lc$y_train)

     if (use_fixed) {
          callbacks  <- list()
          fit_epochs <- as.integer(hp$n_epochs_fixed)
          val_data   <- NULL
     } else {
          if (is.null(data_bundle$X_val) || is.null(lc$y_val))
               stop(".psi_keras_fit_and_eval: validation data required when n_epochs_fixed is NULL")
          callbacks  <- list(
               keras3::callback_early_stopping(monitor = "val_loss",
                    patience = hp$patience %||% 10L,
                    restore_best_weights = isTRUE(hp$restore_best_weights %||% TRUE),
                    verbose = 0),
               keras3::callback_reduce_lr_on_plateau(monitor = "val_loss",
                    factor = hp$rlr_factor %||% 0.5,
                    patience = hp$rlr_patience %||% 5L,
                    min_lr = hp$min_lr %||% 1e-6, verbose = 0)
          )
          fit_epochs <- hp$epochs %||% 150L
          x_val <- make_x(data_bundle$X_val,
                          data_bundle$country_ids_val,
                          data_bundle$region_ids_val)
          val_data <- list(x_val, .psi_as_target_array(lc$y_val))
     }

     sw_train <- lc$sample_weight_train
     sw_val   <- lc$sample_weight_val

     # keras expects validation_data as a 3-tuple when val sample weights exist.
     if (!use_fixed && !is.null(sw_val)) {
          val_data <- list(x_val, .psi_as_target_array(lc$y_val), sw_val)
     }

     t0 <- proc.time()
     history <- keras3::fit(model,
          x = x_train,
          y = y_train_arr,
          sample_weight   = sw_train,
          validation_data = val_data,
          epochs     = fit_epochs,
          batch_size = hp$batch_size %||% 128L,
          callbacks  = callbacks,
          verbose    = 0L)
     train_minutes <- round((proc.time() - t0)["elapsed"] / 60, 2)

     if (use_fixed) {
          val_loss   <- NA_real_
          val_metric <- NA_real_
          n_epochs   <- fit_epochs
     } else {
          x_val <- make_x(data_bundle$X_val,
                          data_bundle$country_ids_val,
                          data_bundle$region_ids_val)
          score <- keras3::evaluate(model,
               x_val, .psi_as_target_array(lc$y_val),
               sample_weight = sw_val,
               verbose = 0L, return_dict = TRUE)
          val_loss   <- as.numeric(score$loss)
          val_metric <- as.numeric(score[[lc$metric]])
          n_epochs   <- length(history$metrics$loss)
     }

     x_pred <- make_x(data_bundle$X_pred,
                      data_bundle$country_ids_pred,
                      data_bundle$region_ids_pred)
     raw_pred <- as.numeric(stats::predict(model, x_pred, verbose = 0L))
     pred     <- lc$transform_pred(raw_pred)

     list(
          pred          = pred,
          val_loss      = val_loss,
          val_metric    = val_metric,
          train_minutes = unname(train_minutes),
          n_epochs      = n_epochs
     )
}
