# =============================================================================
# tft_data.R -- build TFT's four input streams from the MOSAIC suitability panel.
#
# DESIGN RULE: reuse `.psi_build_data()` for everything it already does --
# panel loading, TRAIN-ONLY feature scaling, the intensity response recipe,
# categorical encoders, the static country covariates, and the rolling-CV step
# grid. Only the final tensor assembly is new, because TFT needs a shape
# `.psi_build_sequences()` does not produce.
#
# Writing a parallel data path would be the exact failure this repo keeps
# hitting (lesson 11: N parallel functions that must be updated in lockstep).
# So the leakage-sensitive parts have ONE implementation, shared with production.
#
# WHAT TFT NEEDS, per forecast origin i for a country:
#   static  (n_static)          the country's z-scored static covariates
#   country (1)                 integer id, for the embedding
#   past    (L, n_past)         covariates over the lookback, and -- if
#                               include_target_history -- the observed target
#                               history as an extra channel
#   future  (H, n_future)       covariates over the FORECAST window. These are
#                               genuinely available at the origin (climate fields
#                               run ahead), which is the whole point: TFT treats
#                               them as known-future instead of quietly consuming
#                               them like a nowcaster.
#   y       (H)                 the target over the forecast window
#
# CONFOUND TO DECLARE: `include_target_history = TRUE` gives TFT something the
# production psi model NEVER sees -- past values of its own target. That is
# standard for a forecaster and TFT assumes it, but it is an information
# advantage unrelated to architecture. Set FALSE for a covariates-only
# comparison. Recorded in tft/README.md.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Assemble TFT tensors for one forecast cutoff.
#'
#' @param bundle the list returned by MOSAIC:::.psi_build_data()
#' @param lookback L, encoder length in weeks
#' @param horizon  H, decoder length in weeks (the trained forecast)
#' @param include_target_history add the observed target as a past channel
#' @param max_gap_days reject windows spanning a larger date gap (matches the
#'   production sequence builder's contract)
tft_build_tensors <- function(bundle, lookback = 52L, horizon = 12L,
                              include_target_history = TRUE,
                              max_gap_days = 14L) {
     pd  <- bundle$pool_data
     enc <- bundle$encoders
     cs  <- enc$country_static
     if (is.null(cs))
          stop("tft_build_tensors: encoders$country_static is NULL -- the panel is ",
               "missing the static country covariates this arm depends on.")
     # `pool_data$country_ids` are ZERO-BASED (range 0..n_countries-1), while
     # `country_static` is a matrix R indexes from 1. Getting this wrong is
     # silent and severe: cs[0, ] returns a ZERO-ROW matrix (so rbind drops the
     # sample and destroys row correspondence between streams), and every other
     # id returns the WRONG country's covariates, shifted by one. Verified:
     # country_ids 0..39 against a 40 x 12 matrix.
     if (!is.matrix(cs))
          stop("tft_build_tensors: country_static must be a matrix, got ", class(cs)[1])
     id_rng <- range(pd$country_ids, na.rm = TRUE)
     if (id_rng[1] != 0L || id_rng[2] != nrow(cs) - 1L)
          stop(sprintf(paste0("tft_build_tensors: country_ids are %d..%d but ",
               "country_static has %d rows. The zero-based/one-based mapping this ",
               "function relies on no longer holds -- refusing rather than ",
               "silently misaligning static covariates."),
               id_rng[1], id_rng[2], nrow(cs)))
     L <- as.integer(lookback); H <- as.integer(horizon)
     nfeat <- ncol(pd$X)
     n_past <- nfeat + as.integer(isTRUE(include_target_history))

     out_static <- list(); out_country <- list(); out_past <- list()
     out_future <- list(); out_y <- list(); out_meta <- list()

     for (iso in unique(pd$countries)) {
          m  <- which(pd$countries == iso)
          o  <- m[order(pd$dates[m])]
          di <- pd$dates[o]; Xi <- pd$X[o, , drop = FALSE]; yi <- pd$intensity[o]
          cid <- pd$country_ids[o][1]
          n <- length(o)
          if (n < L + H) next
          # origin index i = last timestep of the lookback; forecast covers i+1..i+H
          for (i in L:(n - H)) {
               pw <- (i - L + 1L):i
               fw <- (i + 1L):(i + H)
               # reject windows with a date gap larger than the production contract
               if (any(as.numeric(diff(di[c(pw, fw)])) > max_gap_days)) next
               # The FUTURE target may be unobserved -- those origins are exactly
               # the ones we want to FORECAST. Requiring finite y here (the first
               # version did) silently excluded them, leaving 82 prediction
               # samples across 40 countries. y is carried with NAs and the
               # SPLITTER decides: training needs all-finite y, prediction does
               # not. What prediction does need is the covariates, and the PAST
               # target history below -- which is legitimate, since the history
               # is known at the origin.
               yy <- yi[fw]
               if (any(!is.finite(Xi[c(pw, fw), ]))) next
               past <- Xi[pw, , drop = FALSE]
               if (include_target_history) {
                    th <- yi[pw]
                    # the target history must be observed; a partially-missing
                    # lookback would silently train on zeros
                    if (!all(is.finite(th))) next
                    past <- cbind(past, th)
               }
               k <- length(out_y) + 1L
               # cid is 0-based; cs is 1-based -> +1. And cid needs NO shift for
               # keras, which wants 0-based embedding indices and already has them.
               out_static[[k]]  <- as.numeric(cs[cid + 1L, ])
               out_country[[k]] <- as.integer(cid)
               out_past[[k]]    <- past
               out_future[[k]]  <- Xi[fw, , drop = FALSE]
               out_y[[k]]       <- yy
               out_meta[[k]]    <- data.frame(iso_code = iso,
                                              origin_date = di[i],
                                              stringsAsFactors = FALSE)
          }
     }
     n <- length(out_y)
     if (!n) stop("tft_build_tensors: produced 0 samples -- check lookback/horizon ",
                  "against the panel's per-country series length.")
     arr3 <- function(lst, d2, d3) {
          a <- array(NA_real_, c(length(lst), d2, d3))
          for (i in seq_along(lst)) a[i, , ] <- lst[[i]]
          a
     }
     # Build static by PREALLOCATION, not rbind. rbind silently drops a
     # zero-length entry, which is how the 0-based indexing bug above produced a
     # static stream 351 rows shorter than the others -- misaligning every
     # subsequent row rather than failing.
     stat <- matrix(NA_real_, n, ncol(cs))
     for (i in seq_len(n)) {
          v <- out_static[[i]]
          if (length(v) != ncol(cs))
               stop(sprintf("tft_build_tensors: static row %d has length %d, expected %d",
                            i, length(v), ncol(cs)))
          stat[i, ] <- v
     }
     x <- list(static  = stat,
               country = matrix(unlist(out_country), ncol = 1L),
               past    = arr3(out_past,   L, n_past),
               future  = arr3(out_future, H, nfeat))
     # every stream must agree on the sample axis, or rows do not correspond
     nn <- vapply(x, function(z) dim(z)[1], integer(1))
     if (length(unique(c(nn, n))) != 1L)
          stop("tft_build_tensors: sample-axis mismatch across streams: ",
               paste(names(nn), nn, sep = "=", collapse = " "), " vs n=", n)
     list(
          x = x,
          y    = array(do.call(rbind, out_y), c(n, H, 1L)),
          meta = do.call(rbind, out_meta),
          dims = list(n_static = ncol(cs), n_past = n_past, n_future = nfeat,
                      lookback = L, horizon = H, n_countries = enc$n_countries),
          # target dates implied by each sample: origin + 1..H weeks
          target_dates = lapply(seq_len(n), function(i)
               out_meta[[i]]$origin_date + 7L * seq_len(H))
     )
}

#' Split assembled tensors into train / predict by the forecast cutoff.
#'
#' TRAIN uses only origins whose ENTIRE forecast window lies before the cutoff,
#' so no training sample contains a target value at or after the origin of
#' interest. PREDICT uses origins whose window lies at or after the cutoff.
tft_split_by_cutoff <- function(tt, cutoff_date) {
     cutoff_date <- as.Date(cutoff_date)
     last_target <- as.Date(vapply(tt$target_dates, function(d) as.numeric(max(d)),
                                   numeric(1)), origin = "1970-01-01")
     first_target <- as.Date(vapply(tt$target_dates, function(d) as.numeric(min(d)),
                                    numeric(1)), origin = "1970-01-01")
     # TRAIN additionally requires the full horizon OBSERVED; PREDICT does not
     # (an unobserved future is the thing being forecast).
     y_complete <- apply(tt$y[, , 1, drop = FALSE], 1, function(v) all(is.finite(v)))
     is_train <- last_target < cutoff_date & y_complete
     is_pred  <- first_target >= cutoff_date
     sub <- function(idx) list(
          x = list(static  = tt$x$static[idx, , drop = FALSE],
                   country = tt$x$country[idx, , drop = FALSE],
                   past    = tt$x$past[idx, , , drop = FALSE],
                   future  = tt$x$future[idx, , , drop = FALSE]),
          y = tt$y[idx, , , drop = FALSE],
          meta = tt$meta[idx, , drop = FALSE],
          target_dates = tt$target_dates[idx])
     list(train = sub(which(is_train)), pred = sub(which(is_pred)),
          n_train = sum(is_train), n_pred = sum(is_pred))
}
