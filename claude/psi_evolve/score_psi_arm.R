# =============================================================================
# score_psi_arm.R -- the ONE scorer for the psi 12-week evolution process.
#
# Implements OBJECTIVE.md v3 exactly:
#   S(arm) = sum_j w_j * median_over_blocks( wis_skill_j )
# where wis_skill is vs the PERSISTENCE baseline on the held-out 92-day block of
# each of the 9 PRODUCTION VALIDATION cutoffs (the OCV-4 grid), w_j is the frozen
# sqrt-burden weight, over the 16-country scoring pool.
#
# Guards that make the number trustworthy (PROTOCOL.md sections 5.1, 5.3):
#   * the frozen weights file is sha256-checked on every call;
#   * the declared objective_version in OBJECTIVE.md must match this scorer;
#   * `mode` is explicit and the split is enforced here from EVAL_GRID.csv, never
#     taken from the caller, so confirmation blocks cannot leak into a selection
#     score AND a caller passing the wrong block geometry is rejected;
#   * countries outside the frozen pool are dropped, loudly.
#
# v3 additions:
#   * all three baselines scored on identical cells (persistence = the objective's
#     denominator; `seasonal` = the A6 must-beat; persistence_last = reported);
#   * per-horizon decomposition (h1mo/h2mo/h3mo) on the same cells, so "is the
#     12-week loss a 12-week problem or a compounding 4-week one" is answerable
#     without a second run.
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
     # D4 (red-team, wave 13): if `shasum` is absent or errors, `sha` was NA and the
     # check was skipped SILENTLY -- section 5.1 says hash-checked at every scoring run.
     if (is.na(sha))
          stop("score_psi_arm: cannot compute the weights sha256 (is `shasum` available?). ",
               "PROTOCOL 5.1 requires the frozen objective to be hash-checked on every ",
               "scoring run; refusing to score unverified.", call. = FALSE)
     if (!identical(sha, expect_sha)) {
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
.PE_OBJECTIVE_VERSION <- 3L

# The objective version this scorer implements must match the one OBJECTIVE.md
# declares. Code and objective drifting apart silently is the failure mode
# PROTOCOL 5.1 exists to prevent, and the weights sha cannot catch it: v3 reuses
# v1/v2's weights file unchanged, so the hash matches across a grid change that
# makes every prior score incomparable.
.pe_assert_objective_version <- function(dir = .PSI_EVOLVE_DIR) {
     f <- file.path(dir, "OBJECTIVE.md")
     if (!file.exists(f))
          stop("score_psi_arm: OBJECTIVE.md not found at ", f, call. = FALSE)
     ln <- grep("^objective_version:", readLines(f, warn = FALSE), value = TRUE)
     if (!length(ln))
          stop("score_psi_arm: OBJECTIVE.md declares no `objective_version:`.", call. = FALSE)
     v <- as.integer(trimws(sub("^objective_version:", "", ln[1])))
     if (!identical(v, .PE_OBJECTIVE_VERSION))
          stop(sprintf(paste0("score_psi_arm: objective mismatch. OBJECTIVE.md declares v%d but this ",
                              "scorer implements v%d. Scores across versions are not comparable."),
                       v, .PE_OBJECTIVE_VERSION), call. = FALSE)
     invisible(v)
}

# The frozen evaluation grid. Read here rather than trusted from the caller: the
# split, the block geometry and the horizon boundaries are objective parameters
# (OBJECTIVE 4a), and at v2 nothing verified that the driver's `folds` frame
# actually matched them.
.pe_grid <- function(dir = .PSI_EVOLVE_DIR) {
     f <- file.path(dir, "EVAL_GRID.csv")
     if (!file.exists(f)) stop("score_psi_arm: EVAL_GRID.csv not found at ", f, call. = FALSE)
     g <- utils::read.csv(f, stringsAsFactors = FALSE)
     need <- c("block", "cutoff", "h1mo_end", "h2mo_end", "test_start", "test_end",
               "split", "grid")
     miss <- setdiff(need, names(g))
     if (length(miss))
          stop("score_psi_arm: EVAL_GRID.csv is missing column(s): ",
               paste(miss, collapse = ", "), call. = FALSE)
     g <- g[g$grid == "prod", , drop = FALSE]
     for (k in c("cutoff", "h1mo_end", "h2mo_end", "test_start", "test_end"))
          g[[k]] <- as.Date(g[[k]])
     if (!all(g$split %in% c("selection", "confirmation")))
          stop("score_psi_arm: EVAL_GRID.csv `split` must be selection|confirmation.",
               call. = FALSE)
     g
}

# Baselines scored on every cell. `persistence` is the objective's denominator;
# `seasonal` is the A6 must-beat (the cheapest baseline that actually VARIES in
# time -- persistence is a flat local constant, so beating it says nothing about
# whether psi's dynamics are informative); `persistence_last` is reported.
.PE_BASELINES <- c("persistence", "seasonal", "persistence_last")

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
#' @param interval_mode How the MODEL's prediction intervals are formed.
#'   \code{"seed"} (the objective-v2 default) uses the seed-dispersion quantiles
#'   the psi pipeline emits. \code{"residual"} instead builds empirical
#'   residual-quantile intervals from the model's own pre-block errors, exactly as
#'   the persistence baseline's are built.
#'
#'   WHY THIS OPTION EXISTS. Seed dispersion measures how much the FIT wobbles
#'   across random seeds; it is not predictive uncertainty. Measured on A000 over
#'   5,952 paired cells: psi's 95% interval is 49x NARROWER than the baseline's
#'   and covers 18.0% of outcomes against the baseline's 92.4%. WIS therefore
#'   charges psi for being under-dispersed rather than for being wrong, and an
#'   arm's score becomes partly a function of its seed count. \code{"residual"}
#'   makes the two sides symmetric.
#'
#'   This is a REPORTED diagnostic, not the objective: PROTOCOL section 5.1
#'   forbids an agent from enacting an objective change. Switching the default
#'   requires a human decision and an \code{objective_version} bump.
score_psi_arm <- function(arm_id, pred, obs, folds,
                          mode = c("selection", "confirmation"),
                          incumbent = NULL, dir = .PSI_EVOLVE_DIR, verbose = TRUE,
                          interval_mode = c("seed", "residual"),
                          psi_column = "psi",
                          pred_pre = NULL) {
     mode <- match.arg(mode)
     interval_mode <- match.arg(interval_mode)
     # `train_end` is load-bearing: it is the model's information cut-off and the
     # baseline is anchored on it (D1). Absent, `fd$train_end` is NULL, every
     # `is_df` comes back empty and the scorer reports "no scoreable cells" --
     # which reads like a data problem rather than a malformed `folds`. Say so.
     req_folds <- c("fold", "train_end", "test_start", "test_end")
     miss_f <- setdiff(req_folds, names(folds))
     if (length(miss_f))
          stop("score_psi_arm: `folds` is missing required column(s): ",
               paste(miss_f, collapse = ", "),
               ". `train_end` is the model's information cut-off and anchors the baseline.",
               call. = FALSE)
     # `psi_column` selects which series is scored as the point forecast. "psi"
     # (== pred_bias_corrected) is what the engine consumes; "pred_smooth" is the
     # same seed ensemble BEFORE calibrate_psi_predictions()'s per-country affine.
     # NOTE the shipped q025/q25/q75/q975 are the seed-dispersion quantiles of
     # pred_smooth, so pairing them with the bias-corrected psi leaves the
     # intervals not centred on the point -- a second reason to compare the two.
     if (!psi_column %in% names(pred))
          stop("score_psi_arm: psi_column '", psi_column, "' not in `pred`.", call. = FALSE)
     if (!identical(psi_column, "psi")) pred$psi <- pred[[psi_column]]
     if (!is.null(pred_pre)) {
          if (!all(c("iso_code", "date", "fold", psi_column) %in% names(pred_pre)))
               stop("score_psi_arm: `pred_pre` needs iso_code, date, fold and '", psi_column, "'.",
                    call. = FALSE)
          if (!identical(psi_column, "psi")) pred_pre$psi <- pred_pre[[psi_column]]
          pred_pre$date <- as.Date(pred_pre$date)
     }
     stopifnot(is.data.frame(pred), is.data.frame(obs), is.data.frame(folds))
     .pe_assert_objective_version(dir)
     W <- .pe_weights(dir)
     pool <- W$iso_code
     G <- .pe_grid(dir)

     # ---- enforce the split AND the geometry HERE, never trust the caller ----
     # v3: the split comes from EVAL_GRID.csv's `split` column, and every fold the
     # caller supplies must match the frozen grid on all four dates. At v2 the
     # split was a date constant and the geometry was unverified, so a driver that
     # built `folds` incorrectly would have produced a clean-looking score on the
     # wrong blocks -- the same silent-wrong-number class as the wave-4 stride bug.
     folds$fold       <- as.integer(folds$fold)
     folds$train_end  <- as.Date(folds$train_end)
     folds$test_start <- as.Date(folds$test_start)
     folds$test_end   <- as.Date(folds$test_end)
     unknown <- setdiff(folds$fold, G$block)
     if (length(unknown))
          stop("score_psi_arm: fold(s) not in the frozen grid: ",
               paste(unknown, collapse = ", "), call. = FALSE)
     gi <- match(folds$fold, G$block)
     bad <- which(folds$train_end  != G$cutoff[gi] |
                  folds$test_start != G$test_start[gi] |
                  folds$test_end   != G$test_end[gi])
     if (length(bad))
          stop(sprintf(paste0("score_psi_arm: fold geometry does not match the frozen grid for ",
                              "block(s) %s. Expected block %d = cutoff %s, test [%s, %s]; got ",
                              "train_end %s, test [%s, %s]."),
                       paste(folds$fold[bad], collapse = ","), folds$fold[bad[1]],
                       G$cutoff[gi[bad[1]]], G$test_start[gi[bad[1]]], G$test_end[gi[bad[1]]],
                       folds$train_end[bad[1]], folds$test_start[bad[1]], folds$test_end[bad[1]]),
               call. = FALSE)
     folds$split <- G$split[gi]
     folds$h1mo_end <- G$h1mo_end[gi]
     folds$h2mo_end <- G$h2mo_end[gi]
     folds <- folds[folds$split == mode, , drop = FALSE]
     if (nrow(folds) == 0L)
          stop(sprintf("score_psi_arm: no %s blocks present in `folds` (grid has %d)",
                       mode, sum(G$split == mode)), call. = FALSE)
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
     # Under "residual" the emitted seed quantiles are replaced, so the
     # zero-width check below is only meaningful for "seed".
     zw <- if (identical(interval_mode, "seed"))
          mean(abs(pred$q975 - pred$q025) < 1e-12, na.rm = TRUE) else 0
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

               # D1 (red-team, wave 13): the baseline must share the MODEL's
               # information set. This previously cut at `test_start` = cutoff + 14d,
               # but the model sees nothing past `cutoff`. `.rcv_baseline("persistence")`
               # is `mean(tail(observed, 4))`, so TWO of its four anchor weeks were
               # post-cutoff: it was effectively a 1-to-12-week-ahead forecast against
               # the model's 3-to-14-week-ahead one. `fd$train_end` was passed in by the
               # driver and never used. Consequence: every "psi loses to persistence by
               # X" computed before this fix is an UPPER BOUND on X.
               is_df <- o[o$date <= as.Date(fd$train_end) & is.finite(o$observed), , drop = FALSE]
               if (nrow(is_df) < 8L) next

               if (identical(interval_mode, "seed")) {
                    lo50 <- m$q25; hi50 <- m$q75; lo95 <- m$q025; hi95 <- m$q975
               } else {
                    # Symmetric with the baseline: empirical quantiles of the
                    # model's OWN residuals on this country's pre-block history.
                    #
                    # D2 (red-team, wave 13): this used to read `pred`, which the
                    # driver has already truncated to the evaluation blocks. So the
                    # residual sample was 12 out-of-sample points for the second fold
                    # and ZERO for the first -- which silently dropped the earliest
                    # block from every country (A000: 199 -> 183 cells = exactly one
                    # fold x 16 countries), and made the interval estimator's quality
                    # depend on the fold index. `quantile(r, 0.025)` on 12 points is
                    # essentially the sample minimum (~85% coverage, not 95%).
                    #
                    # The fix costs nothing: each cached psi file already spans the
                    # full prediction window from `pred_date_start`, so ~3,230
                    # pre-cutoff rows per country exist and the driver was discarding
                    # them. Supplied via `pred_pre`, the model's residual sample is
                    # now in-sample pre-cutoff history -- the same quantity, on the
                    # same scale, as the baseline's.
                    src <- if (!is.null(pred_pre)) pred_pre else pred
                    pre <- src[src$iso_code == iso & src$date <= as.Date(fd$train_end), ,
                               drop = FALSE]
                    if (!is.null(pred_pre)) pre <- pre[pre$fold == fd$fold, , drop = FALSE]
                    pm  <- merge(pre[, c("date", "psi")], o[, c("date", "observed")], by = "date")
                    r   <- pm$observed - pm$psi
                    r   <- r[is.finite(r)]
                    if (length(r) < 8L) next
                    q   <- stats::quantile(r, c(0.025, 0.25, 0.75, 0.975), names = FALSE)
                    lo95 <- m$psi + q[1]; lo50 <- m$psi + q[2]
                    hi50 <- m$psi + q[3]; hi95 <- m$psi + q[4]
               }
               # Per-ROW WIS for the model and for every baseline, computed once.
               # The horizon decomposition then aggregates the same rows into
               # buckets -- so a bucket score and the pooled score are guaranteed
               # to be the same quantity on the same cells, rather than two
               # separate runs that could diverge.
               wis_m_row <- wis_fn(m$observed, m$psi, lo50, hi50, lo95, hi95)
               bl_row <- list()
               for (bn in .PE_BASELINES) {
                    b <- bl_fn(is_df, m$date, bn)
                    bl_row[[bn]] <- if (any(is.finite(b$point)))
                         wis_fn(m$observed, b$point, b$pi50_lo, b$pi50_hi,
                                b$pi95_lo, b$pi95_hi)
                    else rep(NA_real_, nrow(m))
               }
               # The objective's denominator must exist for the cell to count.
               wb_all <- mean(bl_row[["persistence"]], na.rm = TRUE)
               if (!is.finite(wb_all) || wb_all <= 0) next

               # Horizon buckets, identical to MOSAIC:::.rolling_cv_label():
               # h1mo = oos0+1..oos0+31, h2mo = ..+61, h3mo = ..+92.
               hb <- ifelse(m$date <= fd$h1mo_end, "h1mo",
                            ifelse(m$date <= fd$h2mo_end, "h2mo", "h3mo"))
               idx_list <- c(list(all = rep(TRUE, nrow(m))),
                             stats::setNames(lapply(c("h1mo", "h2mo", "h3mo"),
                                                    function(h) hb == h),
                                             c("h1mo", "h2mo", "h3mo")))
               for (hn in names(idx_list)) {
                    idx <- idx_list[[hn]]
                    # A bucket needs >= 2 observed weeks to mean anything; the
                    # pooled cell keeps the historical >= 4 floor via nrow(m).
                    if (sum(idx) < if (hn == "all") 4L else 2L) next
                    wm <- mean(wis_m_row[idx], na.rm = TRUE)
                    row <- data.frame(iso_code = iso, fold = fd$fold, horizon = hn,
                                      n_days = sum(idx), wis_model = wm,
                                      stringsAsFactors = FALSE)
                    for (bn in .PE_BASELINES) {
                         wb <- mean(bl_row[[bn]][idx], na.rm = TRUE)
                         row[[paste0("wis_base_", bn)]]  <- wb
                         row[[paste0("skill_", bn)]] <-
                              if (is.finite(wb) && wb > 0) 1 - wm / wb else NA_real_
                    }
                    # Back-compatible names: the objective's baseline.
                    row$wis_base  <- row$wis_base_persistence
                    row$wis_skill <- row$skill_persistence
                    cells[[length(cells) + 1L]] <- row
               }
          }
     }
     if (!length(cells)) stop("score_psi_arm: no scoreable (country, fold) cells", call. = FALSE)
     cells <- do.call(rbind, cells)
     cells_all <- cells[cells$horizon == "all", , drop = FALSE]
     if (!nrow(cells_all))
          stop("score_psi_arm: no scoreable (country, block) cells at the pooled horizon",
               call. = FALSE)

     # ---- aggregate: median over blocks within country, then burden-weighted -
     # One helper for every skill column, so the objective's S, the baseline
     # comparisons and the per-horizon numbers are computed by IDENTICAL code.
     # NOTE this estimand is median-then-weight, so it is NOT additive across
     # subsets: the pooled S is not a weighted average of the per-horizon S
     # values, and a uniform within-subset win can pool to a loss (wave 17).
     # PROTOCOL section 4 requires subset claims to be computed, never inferred.
     agg_S <- function(df, col) {
          d <- df[is.finite(df[[col]]), c("iso_code", col), drop = FALSE]
          if (!nrow(d))
               return(list(per_iso = NULL, S = NA_real_, n_beat = NA_integer_, n_scored = 0L))
          names(d)[2] <- "skill"
          pi <- stats::aggregate(skill ~ iso_code, d, stats::median)
          pi <- merge(pi, W[, c("iso_code", "w")], by = "iso_code")
          pi$w <- pi$w / sum(pi$w)                    # renormalise over scored countries
          list(per_iso = pi, S = sum(pi$w * pi$skill),
               n_beat = sum(pi$skill > 0), n_scored = nrow(pi))
     }

     primary <- agg_S(cells_all, "skill_persistence")
     per_iso <- primary$per_iso
     names(per_iso)[names(per_iso) == "skill"] <- "wis_skill"
     S <- primary$S
     n_beat <- primary$n_beat

     # A6 (OBJECTIVE 3b): the same arm, same cells, against each baseline.
     S_by_baseline <- lapply(.PE_BASELINES, function(bn) {
          a <- agg_S(cells_all, paste0("skill_", bn))
          list(S = a$S, n_beat = a$n_beat, n_scored = a$n_scored)
     })
     names(S_by_baseline) <- .PE_BASELINES

     # Per-horizon decomposition, vs the objective's baseline, on the same cells.
     per_horizon <- lapply(c("h1mo", "h2mo", "h3mo"), function(hn) {
          a <- agg_S(cells[cells$horizon == hn, , drop = FALSE], "skill_persistence")
          list(horizon = hn, S = a$S, n_beat = a$n_beat, n_scored = a$n_scored,
               n_cells = sum(cells$horizon == hn))
     })
     names(per_horizon) <- c("h1mo", "h2mo", "h3mo")

     # ---- no-regression guard (OBJECTIVE section 2) --------------------------
     top10_worst <- NA_real_; guard_ok <- NA; S_delta <- NA_real_; guard_note <- NA_character_
     if (!is.null(incumbent)) {
          # D3 (red-team, wave 13): this guard FAILED OPEN in four ways -- a top-10
          # country missing from `incumbent` gave NA which `min(na.rm=TRUE)` discarded
          # (silently exempt); all-missing gave `min(NA, na.rm=TRUE) = Inf` and
          # `Inf >= -0.02` is TRUE, i.e. a PASS; a top-10 country that failed to score
          # was dropped by intersect() and went unguarded; and S_delta counted a missing
          # incumbent country's skill as 0 while keeping its weight in S. Same shape as
          # `.drop_filled_prediction_tail()`, fixed one wave earlier with the note that a
          # guard against silent corruption must not itself fail silently.
          t10_scored  <- intersect(per_iso$iso_code, .PE_TOP10)
          t10_missing <- setdiff(.PE_TOP10, t10_scored)
          inc_missing <- t10_scored[!t10_scored %in% names(incumbent) |
                                    is.na(incumbent[t10_scored])]
          if (length(t10_missing) || length(inc_missing)) {
               guard_ok <- FALSE
               guard_note <- sprintf(paste0("guard CANNOT be evaluated: %d top-10 country(ies) ",
                                            "unscored [%s]; %d lack an incumbent value [%s]"),
                                     length(t10_missing), paste(t10_missing, collapse = ","),
                                     length(inc_missing), paste(inc_missing, collapse = ","))
               warning("score_psi_arm: ", guard_note, ". Treating as FAIL.", call. = FALSE)
          }
          d <- per_iso$wis_skill[match(t10_scored, per_iso$iso_code)] - incumbent[t10_scored]
          d <- d[is.finite(d)]
          top10_worst <- if (length(d)) min(d) else NA_real_
          if (is.na(guard_ok))
               guard_ok <- is.finite(top10_worst) && top10_worst >= -0.02
          # S_delta only where BOTH arms scored the country, so a missing incumbent
          # cannot be silently counted as zero skill while keeping its weight.
          both <- per_iso$iso_code[per_iso$iso_code %in% names(incumbent) &
                                   is.finite(incumbent[per_iso$iso_code])]
          if (length(both)) {
               pb <- per_iso[per_iso$iso_code %in% both, ]
               wb <- pb$w / sum(pb$w)
               S_delta <- sum(wb * pb$wis_skill) - sum(wb * incumbent[pb$iso_code])
          }
     }

     # ---- S excluding NGA (PROTOCOL section 4: a win must survive it) --------
     pn <- per_iso[per_iso$iso_code != "NGA", ]
     S_exNGA <- if (nrow(pn)) sum(pn$w / sum(pn$w) * pn$wis_skill) else NA_real_

     out <- list(arm_id = arm_id, mode = mode,
                 objective_version = .PE_OBJECTIVE_VERSION,
                 interval_mode = interval_mode, psi_column = psi_column,
                 S = S, S_delta = S_delta, S_exNGA = S_exNGA,
                 n_beat = n_beat, n_scored = nrow(per_iso),
                 top10_worst = top10_worst, guard_ok = guard_ok, guard_note = guard_note,
                 n_folds = nrow(folds), n_cells = nrow(cells_all),
                 blocks = sort(unique(folds$fold)),
                 S_by_baseline = S_by_baseline,
                 beats_seasonal = isTRUE(is.finite(S_by_baseline$seasonal$S) &&
                                         S_by_baseline$seasonal$S > 0),
                 per_horizon = per_horizon,
                 per_iso = per_iso, cells = cells)
     if (verbose) {
          message(sprintf("[%s | %s | obj v%d] S = %.4f  (exNGA %.4f)  n_beat = %d/%d  blocks = %d  cells = %d",
                          arm_id, mode, .PE_OBJECTIVE_VERSION, S, S_exNGA, n_beat,
                          nrow(per_iso), nrow(folds), nrow(cells_all)))
          message(sprintf("   vs baselines: %s",
                          paste(sprintf("%s %+.4f (n_beat %s)", .PE_BASELINES,
                                        vapply(S_by_baseline, function(z) z$S, numeric(1)),
                                        vapply(S_by_baseline, function(z)
                                               as.character(z$n_beat), character(1))),
                                collapse = "  |  ")))
          message(sprintf("   A6 (must beat `seasonal`): %s",
                          if (out$beats_seasonal) "PASS" else "FAIL"))
          message(sprintf("   per horizon: %s",
                          paste(sprintf("%s %+.4f", names(per_horizon),
                                        vapply(per_horizon, function(z) z$S, numeric(1))),
                                collapse = "  ")))
          if (!is.na(top10_worst))
               message(sprintf("   top-10 worst delta = %+.4f  -> guard %s",
                               top10_worst, if (isTRUE(guard_ok)) "PASS" else "FAIL"))
     }
     out
}
