# =============================================================================
# score_psi_arm.R -- the ONE scorer for the psi 12-week evolution process.
#
# Implements OBJECTIVE.md v2 exactly:
#   S(arm) = sum_j w_j * median_over_folds( wis_skill_j )
# where wis_skill is vs the PERSISTENCE baseline on held-out 84-day blocks,
# w_j is the frozen sqrt-burden weight, over the 16-country scoring pool.
#
# Guards that make the number trustworthy (PROTOCOL.md sections 5.1, 5.3):
#   * the frozen weights file is sha256-checked on every call;
#   * `mode` is explicit and the fold split is enforced here, not by the caller,
#     so confirmation folds cannot leak into a selection score;
#   * countries outside the frozen pool are dropped, loudly.
#
# WIS and the baseline come from the package's own internals so the model and
# the baseline are scored by identical code (forecast-CV finding FCV-07 was the
# opposite mistake: baseline scored on different cells from the model).
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

# Directory of this file when sourced; falls back to the working directory.
.PSI_EVOLVE_DIR <- local({
     of <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
     dirname(normalizePath(of %||% "score_psi_arm.R", mustWork = FALSE))
})

.pe_weights <- function(dir = .PSI_EVOLVE_DIR,
                        expect_sha = "255078783c0b8bc582e5128e8f7f8d6d170ccd7d1b0867adf981f8baa5a89a81") {
     f <- file.path(dir, "weights_frozen.csv")
     if (!file.exists(f)) stop("score_psi_arm: weights_frozen.csv not found at ", f, call. = FALSE)
     sha <- tryCatch(
          system2("shasum", c("-a", "256", shQuote(f)), stdout = TRUE),
          error = function(e) NA_character_)
     sha <- if (length(sha) && !is.na(sha[1])) sub(" .*$", "", sha[1]) else NA_character_
     if (!is.na(sha) && !identical(sha, expect_sha)) {
          stop(sprintf(paste0("score_psi_arm: FROZEN OBJECTIVE VIOLATION. weights_frozen.csv sha256 is\n",
                              "  %s\nbut OBJECTIVE.md pins\n  %s\n",
                              "Scores under a changed objective are not comparable. Bump objective_version ",
                              "and re-score the incumbent, or restore the file."), sha, expect_sha),
               call. = FALSE)
     }
     w <- utils::read.csv(f, stringsAsFactors = FALSE)
     w$w <- w$w_sqrt / sum(w$w_sqrt)
     w
}

# Top-10 burden countries -- the no-regression guard set (OBJECTIVE.md section 2)
.PE_TOP10 <- c("COD","NGA","SSD","ETH","MOZ","SOM","MWI","AGO","ZWE","ZMB")
.PE_SPLIT_DATE <- as.Date("2025-01-01")   # selection < this <= confirmation

#' Score one arm.
#'
#' @param arm_id character. Registry id.
#' @param pred data.frame: iso_code, date, fold, psi, q025, q25, q75, q975.
#'   One row per held-out day per country per fold.
#' @param obs data.frame: iso_code, date, observed (transmission intensity).
#' @param folds data.frame: fold, train_end, test_start, test_end.
#' @param mode "selection" (folds with test_start <  2025-01-01) or
#'   "confirmation" (test_start >= 2025-01-01). PROTOCOL 5.3: confirmation is
#'   read once per arm, only after a selection win.
#' @param incumbent optional named numeric of per-country skill for the current
#'   incumbent; enables the no-regression guard and S_delta.
score_psi_arm <- function(arm_id, pred, obs, folds,
                          mode = c("selection", "confirmation"),
                          incumbent = NULL, dir = .PSI_EVOLVE_DIR, verbose = TRUE) {
     mode <- match.arg(mode)
     stopifnot(is.data.frame(pred), is.data.frame(obs), is.data.frame(folds))
     W <- .pe_weights(dir)
     pool <- W$iso_code

     # ---- enforce the split HERE, never trust the caller ---------------------
     folds$test_start <- as.Date(folds$test_start)
     keep <- if (mode == "selection") folds$test_start <  .PE_SPLIT_DATE
             else                     folds$test_start >= .PE_SPLIT_DATE
     folds <- folds[keep, , drop = FALSE]
     if (nrow(folds) == 0L)
          stop(sprintf("score_psi_arm: no %s folds (split at %s)", mode, .PE_SPLIT_DATE), call. = FALSE)
     pred <- pred[pred$fold %in% folds$fold, , drop = FALSE]

     # ---- restrict to the frozen pool, loudly --------------------------------
     dropped <- setdiff(unique(pred$iso_code), pool)
     if (length(dropped) && verbose)
          message("score_psi_arm: dropping ", length(dropped),
                  " country(ies) outside the frozen pool: ", paste(dropped, collapse = ", "))
     pred <- pred[pred$iso_code %in% pool, , drop = FALSE]
     pred$date <- as.Date(pred$date); obs$date <- as.Date(obs$date)

     wis_fn <- getFromNamespace(".rcv_wis", "MOSAIC")
     bl_fn  <- getFromNamespace(".rcv_baseline", "MOSAIC")

     # DEGENERATE-INTERVAL GUARD. The model's prediction intervals are seed
     # dispersion quantiles, so a single-seed fit yields q025 == q975 on every
     # row. WIS would then score a POINT forecast against a baseline that gets
     # real residual-quantile intervals -- the model is charged the full interval
     # penalty with no interval to earn it back, and every low-seed arm looks
     # systematically worse for a reason unrelated to its quality. Measured on a
     # 1-seed smoke: 100% of rows zero-width, S = -0.88, which is an artifact and
     # not a result. Refuse rather than return a number that reads as a score.
     zw <- mean(abs(pred$q975 - pred$q025) < 1e-12, na.rm = TRUE)
     if (is.finite(zw) && zw > 0.5) {
          stop(sprintf(paste0("score_psi_arm: %.1f%% of prediction rows have a ZERO-WIDTH 95%% ",
                              "interval (q025 == q975). This is what a single-seed fit produces, ",
                              "and WIS would compare a point forecast against an interval ",
                              "baseline. Refit with n_seeds >= 3, or score a point metric instead."),
                      100 * zw), call. = FALSE)
     }
     if (is.finite(zw) && zw > 0.01)
          warning(sprintf("score_psi_arm: %.2f%% of rows have zero-width intervals.", 100 * zw),
                  call. = FALSE)

     # ---- per (country, fold) skill -----------------------------------------
     cells <- list()
     for (i in seq_len(nrow(folds))) {
          fd <- folds[i, ]
          for (iso in pool) {
               p <- pred[pred$iso_code == iso & pred$fold == fd$fold, , drop = FALSE]
               if (nrow(p) == 0L) next
               o <- obs[obs$iso_code == iso, , drop = FALSE]
               m <- merge(p, o[, c("date", "observed")], by = "date")
               m <- m[is.finite(m$observed) & is.finite(m$psi), , drop = FALSE]
               if (nrow(m) < 4L) next

               wis_model <- mean(wis_fn(m$observed, m$psi, m$q25, m$q75, m$q025, m$q975), na.rm = TRUE)

               # baseline fitted on this country's history strictly BEFORE the block
               is_df <- o[o$date < as.Date(fd$test_start) & is.finite(o$observed), , drop = FALSE]
               if (nrow(is_df) < 8L) next
               b <- bl_fn(is_df, m$date, "persistence")
               if (!any(is.finite(b$point))) next
               wis_base <- mean(wis_fn(m$observed, b$point, b$pi50_lo, b$pi50_hi,
                                       b$pi95_lo, b$pi95_hi), na.rm = TRUE)
               if (!is.finite(wis_base) || wis_base <= 0) next

               cells[[length(cells) + 1L]] <- data.frame(
                    iso_code = iso, fold = fd$fold, n_days = nrow(m),
                    wis_model = wis_model, wis_base = wis_base,
                    wis_skill = 1 - wis_model / wis_base, stringsAsFactors = FALSE)
          }
     }
     if (!length(cells)) stop("score_psi_arm: no scoreable (country, fold) cells", call. = FALSE)
     cells <- do.call(rbind, cells)

     # ---- aggregate: median over folds within country, then burden-weighted --
     per_iso <- stats::aggregate(wis_skill ~ iso_code, cells, stats::median)
     per_iso <- merge(per_iso, W[, c("iso_code", "w")], by = "iso_code")
     per_iso$w <- per_iso$w / sum(per_iso$w)          # renormalise over scored countries
     S <- sum(per_iso$w * per_iso$wis_skill)
     n_beat <- sum(per_iso$wis_skill > 0)

     # ---- no-regression guard (OBJECTIVE section 2) --------------------------
     top10_worst <- NA_real_; guard_ok <- NA; S_delta <- NA_real_
     if (!is.null(incumbent)) {
          t10 <- intersect(per_iso$iso_code, .PE_TOP10)
          d   <- per_iso$wis_skill[match(t10, per_iso$iso_code)] - incumbent[t10]
          top10_worst <- if (length(d)) min(d, na.rm = TRUE) else NA_real_
          guard_ok <- is.na(top10_worst) || top10_worst >= -0.02
          inc_v <- incumbent[per_iso$iso_code]
          S_delta <- S - sum(per_iso$w * inc_v, na.rm = TRUE)
     }

     # ---- S excluding NGA (PROTOCOL section 4: a win must survive it) --------
     pn <- per_iso[per_iso$iso_code != "NGA", ]
     S_exNGA <- if (nrow(pn)) sum(pn$w / sum(pn$w) * pn$wis_skill) else NA_real_

     out <- list(arm_id = arm_id, mode = mode, objective_version = 2L,
                 S = S, S_delta = S_delta, S_exNGA = S_exNGA,
                 n_beat = n_beat, n_scored = nrow(per_iso),
                 top10_worst = top10_worst, guard_ok = guard_ok,
                 n_folds = nrow(folds), n_cells = nrow(cells),
                 per_iso = per_iso, cells = cells)
     if (verbose) {
          message(sprintf("[%s | %s] S = %.4f  (exNGA %.4f)  n_beat = %d/%d  folds = %d  cells = %d",
                          arm_id, mode, S, S_exNGA, n_beat, nrow(per_iso), nrow(folds), nrow(cells)))
          if (!is.na(top10_worst))
               message(sprintf("   top-10 worst delta = %+.4f  -> guard %s",
                               top10_worst, if (isTRUE(guard_ok)) "PASS" else "FAIL"))
     }
     out
}
