#' Optimize Ensemble Subset Size
#'
#' @description
#' Post-ensemble optimization that evaluates top-N subsets (ranked by
#' likelihood) and selects the N that maximizes prediction quality. Produces a
#' separate \code{mosaic_ensemble} object at the optimal subset size, leaving the
#' original ensemble untouched.
#'
#' For each candidate size N (from \code{min_n} to the full ensemble), the
#' function re-computes Gibbs weights within the top-N subset, re-computes
#' weighted median predictions from the 4D arrays, and scores with the selected
#' objective function.
#'
#' @details
#' The evaluation loop is organized \emph{cell-outer}: each
#' \code{[location, time]} cell's finite predictions are gathered once and then
#' scored across every candidate N (the gather does not depend on N), filling a
#' per-cell vector of central tendencies. The default call path (\code{stride = 1L},
#' \code{cl = NULL}) is exhaustive and serial and is guaranteed bit-for-bit
#' identical to the historical implementation (Tier-2 parity).
#'
#' Two opt-in speed levers are available:
#' \itemize{
#'   \item \strong{PSOCK parallelism} (\code{cl}): the \code{n_locations *
#'     n_time_points} cells are split across the supplied cluster's workers. The
#'     result is bit-for-bit identical to the serial path (each worker runs the
#'     same kernel on a disjoint block of cells; the master scatters the per-cell
#'     central/quantile values back into the same \code{[i, j]} positions). FORK
#'     clusters are unsafe here (native reticulate/numba threads are loaded by
#'     this post-calibration point); pass a \code{"PSOCK"} cluster.
#'   \item \strong{Stride-then-refine} (\code{stride}): with \code{stride > 1L},
#'     a coarse grid \code{seq(min_n, max_n, by = stride)} is evaluated first to
#'     locate the best N*, then a refinement bracket
#'     \code{(N*-stride):(N*+stride)} is evaluated, and the selection logic runs
#'     over the combined (coarse + refine) rows. This is \strong{not} bit-identical
#'     to the exhaustive path: \code{evaluation_table} contains only the evaluated
#'     N's (fewer rows), and the selected \code{optimal_n} may differ slightly from
#'     the exhaustive optimum if the score profile is non-monotone between coarse
#'     points. The trade is roughly an \code{O(stride)} reduction in the number of
#'     N's evaluated.
#' }
#'
#' @param ensemble A \code{mosaic_ensemble} object returned by
#'   \code{\link{calc_model_ensemble}}.
#' @param likelihoods Numeric vector of log-likelihoods, one per parameter set
#'   in the ensemble. Must have length \code{ensemble$n_param_sets}.
#' @param seeds Optional numeric/integer vector of simulation seeds aligned
#'   with \code{likelihoods}. When supplied, the function returns
#'   \code{optimal_seeds} so callers can map the optimized subset back to the
#'   original \code{samples.parquet} rows without re-deriving the internal sort.
#'   Separately, if \code{ensemble$seeds} is present (the per-member seeds carried
#'   by \code{calc_model_ensemble}, aligned with \code{cases_array} by
#'   construction), it is carried through the same sort/slice and exposed as
#'   \code{ensemble_optimized$seeds} for cases_array-aligned consumers such as
#'   medoid selection. This argument and \code{ensemble$seeds} serve different
#'   roles and are kept independent.
#' @param min_n Minimum subset size to evaluate. Default \code{30L} to guard
#'   against small-subset KDE degeneracy when the optimized subset drives
#'   posterior artifacts. Fox et al. (2024) report 4 as a statistical minimum
#'   for accuracy, but posterior density estimation needs larger N.
#' @param objective Scoring function: \code{"mae"} (default, normalized MAE),
#'   \code{"r2_bias"} (R-squared plus bias penalty), or \code{"wis"} (normalized
#'   Weighted Interval Score).
#' @param central_method Central tendency used to summarize each per-cell
#'   ensemble distribution for the \code{"mae"} and \code{"r2_bias"} objectives
#'   and for the recorded R^2/bias/MAE diagnostics: \code{"median"} (default,
#'   reproduces historical selection) or \code{"mean"} (weighted mean, unbiased
#'   for expected counts). Scalar or per-channel \code{c(cases=, deaths=)}. The
#'   \code{"wis"} objective is quantile-based and \strong{unaffected} by this
#'   setting (its point forecast remains the weighted median). The default
#'   \code{"median"} here is deliberate -- it preserves historical direct-call
#'   selection and the Tier-2 bit-for-bit parity guarantee; \code{run_MOSAIC()}
#'   passes the resolved \code{control$predictions$central_method} (package
#'   default \code{"median"} as of v0.46.1) explicitly.
#' @param stride Integer >= 1. \code{1L} (default) evaluates every N in
#'   \code{min_n:max_n} (exhaustive, bit-identical parity path). \code{> 1L}
#'   enables a coarse-then-refine two-stage search (see Details). \strong{Opt-in};
#'   changes which N's are evaluated and may select a slightly different
#'   \code{optimal_n} than the exhaustive search.
#' @param cl Optional parallel cluster (from
#'   \code{\link{make_mosaic_cluster}} or \code{parallel::makeCluster}) used to
#'   split the per-cell evaluation kernel across workers. \code{NULL} (default)
#'   runs serially. When supplied, the result is bit-for-bit identical to the
#'   serial path. Must be a \code{"PSOCK"} cluster (FORK is unsafe at this point
#'   in the pipeline). For trivially small problems the function falls back to
#'   serial regardless of \code{cl}.
#' @param verbose Logical; if \code{TRUE}, emit progress messages.
#'
#' @return An S3 object of class \code{mosaic_subset_optimization} containing:
#'   \describe{
#'     \item{evaluation_table}{Data frame with one row per N evaluated. With
#'       \code{stride > 1L} this contains only the coarse + refine N's, sorted by
#'       N, not every N in \code{min_n:max_n}.}
#'     \item{optimal_n}{Selected subset size.}
#'     \item{optimal_score}{Score at optimal N.}
#'     \item{optimal_weights}{Re-computed Gibbs weights for the optimal subset.}
#'     \item{optimal_indices}{Integer indices into the original ensemble arrays.}
#'     \item{optimal_seeds}{Simulation seeds of the optimal subset in
#'       likelihood-sorted order, aligned with \code{optimal_weights} (when
#'       \code{seeds} supplied), else \code{NULL}.}
#'     \item{ensemble_optimized}{Complete \code{mosaic_ensemble} object at optimal
#'       N. Its \code{$seeds} field is the per-member seed aligned with its
#'       \code{cases_array} (member-to-seed aligned) when \code{ensemble$seeds} was
#'       present, else it falls back to \code{optimal_seeds}.}
#'     \item{stability_flag}{TRUE if score profile was flat.}
#'     \item{diagnostics_n}{Original diagnostics-selected N.}
#'     \item{diagnostics_score}{Score at the diagnostics-selected N.}
#'     \item{objective}{Which objective was used.}
#'     \item{central_method}{Resolved per-channel central tendency used for
#'       \code{"mae"}/\code{"r2_bias"} scoring and the recorded diagnostics.}
#'   }
#'
#' @references
#' Bracher J et al. (2021). Evaluating epidemic forecasts in an interval format.
#' \emph{PLOS Computational Biology}, 17(2), e1008618.
#'
#' Gneiting T & Raftery AE (2007). Strictly Proper Scoring Rules, Prediction,
#' and Estimation. \emph{JASA}, 102(477), 359--378.
#'
#' @seealso \code{\link{make_mosaic_cluster}} for building a PSOCK cluster to
#'   pass to \code{cl}.
#'
#' @export
optimize_ensemble_subset <- function(ensemble,
                                     likelihoods,
                                     seeds = NULL,
                                     min_n = 30L,
                                     objective = c("mae", "r2_bias", "wis"),
                                     central_method = "median",
                                     stride = 1L,
                                     cl = NULL,
                                     verbose = TRUE) {

  objective <- match.arg(objective)
  central_method <- .mosaic_resolve_central_method(central_method)
  stride <- max(as.integer(stride), 1L)

  # ── 1. VALIDATE INPUTS ────────────────────────────────────────────────

  if (!inherits(ensemble, "mosaic_ensemble")) {
    stop("'ensemble' must be a mosaic_ensemble object.", call. = FALSE)
  }

  n_params <- ensemble$n_param_sets
  n_stoch  <- ensemble$n_simulations_per_config
  n_locs   <- ensemble$n_locations
  n_times  <- ensemble$n_time_points

  # Per-member seeds carried ON the ensemble (aligned with cases_array by
  # construction in calc_model_ensemble: member i <-> ens_member_seeds[i]). These
  # are carried through the sort/slice below in lockstep with cases_array and
  # exposed as ensemble_optimized$seeds, giving medoid selection a seed vector
  # that cannot drift relative to the array. This is deliberately SEPARATE from
  # `seeds`/`optimal_seeds`, which stay aligned with `likelihoods`/`optimal_weights`
  # for the downstream seed->weight mapping. NULL when the ensemble carries none.
  ens_member_seeds <- if (!is.null(ensemble$seeds) && length(ensemble$seeds) == n_params) {
    ensemble$seeds
  } else NULL

  if (length(likelihoods) != n_params) {
    stop(sprintf("Length of 'likelihoods' (%d) must match ensemble$n_param_sets (%d).",
                 length(likelihoods), n_params), call. = FALSE)
  }
  if (any(!is.finite(likelihoods))) {
    stop("'likelihoods' must contain only finite values.", call. = FALSE)
  }

  if (!is.null(seeds)) {
    if (length(seeds) != n_params) {
      stop(sprintf("Length of 'seeds' (%d) must match ensemble$n_param_sets (%d).",
                   length(seeds), n_params), call. = FALSE)
    }
  }

  min_n <- max(as.integer(min_n), 1L)
  if (min_n > n_params) {
    warning(sprintf("min_n (%d) exceeds n_param_sets (%d); clamping to %d.",
                    min_n, n_params, n_params), call. = FALSE)
    min_n <- n_params
  }

  # ── 2. SORT BY LIKELIHOOD DESCENDING ──────────────────────────────────

  sort_order <- order(likelihoods, decreasing = TRUE)
  needs_sort <- !identical(sort_order, seq_len(n_params))

  if (needs_sort) {
    likelihoods  <- likelihoods[sort_order]
    cases_array  <- ensemble$cases_array[, , sort_order, , drop = FALSE]
    deaths_array <- ensemble$deaths_array[, , sort_order, , drop = FALSE]
    if (!is.null(seeds)) seeds <- seeds[sort_order]
    # Carried in lockstep with cases_array so member<->seed alignment survives.
    if (!is.null(ens_member_seeds)) ens_member_seeds <- ens_member_seeds[sort_order]
  } else {
    cases_array  <- ensemble$cases_array
    deaths_array <- ensemble$deaths_array
  }

  # ── 3. PRE-COMPUTE CONSTANTS ──────────────────────────────────────────

  obs_c_flat <- as.numeric(ensemble$obs_cases)
  obs_d_flat <- as.numeric(ensemble$obs_deaths)
  mean_obs_c <- mean(obs_c_flat, na.rm = TRUE)
  mean_obs_d <- mean(obs_d_flat, na.rm = TRUE)

  # Guard against zero means for normalization
  if (!is.finite(mean_obs_c) || mean_obs_c == 0) mean_obs_c <- 1
  if (!is.finite(mean_obs_d) || mean_obs_d == 0) mean_obs_d <- 1

  envelope_quantiles <- ensemble$envelope_quantiles %||% c(0.025, 0.25, 0.75, 0.975)
  n_ci_pairs <- length(envelope_quantiles) / 2L
  wis_probs  <- c(0.025, 0.25, 0.5, 0.75, 0.975)

  max_n <- n_params

  # Candidate N grid: exhaustive when stride == 1 (parity path), coarse-then-
  # refine handled in section 4. `ns_exhaustive` is the full min_n:max_n.
  ns_exhaustive <- seq.int(min_n, max_n)

  if (verbose) message(sprintf("Optimizing ensemble subset: N = %d to %d (%s objective%s)...",
                               min_n, max_n, objective,
                               if (stride > 1L) sprintf(", stride %d", stride) else ""))

  # ── 3b. PER-CELL SORTED TABLES (sort once, reuse across all N) ────────
  # Top-N subsets are nested in param index, and each (param, stoch) prediction
  # carries its OWN param's weight (param-fastest alignment). So sort each cell's
  # finite predictions over the full param block ONCE; for each N, mask to
  # param_id <= N (a sorted subsequence) and weight by the per-N Gibbs weights.
  # Bit-identical to re-sorting per N (a sorted subsequence equals sorting the
  # subset; ties break identically because both layouts are param-fastest with
  # param <= N). Removes ~all the O(m log m) re-sorts from the N loop.
  #
  # MEMORY (R-7): store only the per-cell sorted FINITE-INDEX vector (integers),
  # not the sorted values. The values are re-derived per use by gathering
  # as.vector(arr[i,j,,])[ord]; since ord = which(is.finite(v))[order(v[finite])],
  # v[ord] equals the old v[fin][o] and pid_full[ord] equals the old pid_full[fin][o]
  # exactly -- so weighted_quantiles_presorted receives identical inputs and the
  # result is BIT-IDENTICAL (guarded by test-tier2_parity.R). This drops the
  # dominant memory term (a full doubles copy of both arrays) from the optimize
  # step's peak (~40% less), trading a per-cell gather in the N loop for the RAM
  # -- the value tables dominated memory at the 40K-sim target.
  #
  # Cell enumeration index ci = (i-1)*n_times + j (i outer, j inner); decode
  # i = ((ci-1) %/% n_times) + 1, j = ((ci-1) %% n_times) + 1. This ordering is
  # the contract shared with the parallel cell-split kernel below.
  pid_full <- rep(seq_len(max_n), times = n_stoch)   # param id of each as.vector() element
  ord_c <- .presort_cell_orders(cases_array, n_locs, n_times)
  ord_d <- .presort_cell_orders(deaths_array, n_locs, n_times)

  # Per-channel central tendency for scoring/diagnostics. The median path is
  # always computed (it is the WIS point forecast and the historical selection
  # series); the weighted mean is computed only when requested. The mean mirrors
  # calc_model_ensemble()'s mean exactly (drop non-finite/zero-weight, then
  # weight-renormalize) so selection and the final ensemble report agree.
  need_mean_c <- central_method[["cases"]]  == "mean"
  need_mean_d <- central_method[["deaths"]] == "mean"

  # ── 4. EVALUATION LOOP ───────────────────────────────────────────────
  # `.eval_ns()` evaluates a given vector of candidate N's and returns one
  # eval_table row per N. It uses the cell-outer kernel (gather once per cell,
  # loop over N inside the cell) which is the unit of work parallelized by `cl`.

  .eval_ns <- function(ns) {

    ns <- as.integer(ns)
    n_grid <- length(ns)

    # 4a. Per-N Gibbs weights and per-param weight shares (weight / n_stoch),
    #     computed once up front so the per-cell kernel only does the masked
    #     weighted quantile/mean. `weights_by_n` is indexed by grid position.
    weights_by_n      <- vector("list", n_grid)  # full per-param weights (for ESS)
    w_per_param_by_n  <- vector("list", n_grid)  # weights / n_stoch (kernel input)
    for (g in seq_len(n_grid)) {
      n       <- ns[g]
      ll_n    <- likelihoods[1:n]
      aic_n   <- -2 * ll_n
      delta_n <- aic_n - min(aic_n)
      actual_range <- diff(range(delta_n))
      if (actual_range < .Machine$double.eps) {
        w_n <- rep(1 / n, n)
      } else {
        eta_n <- 0.5 * (4.0 / actual_range)
        w_n <- calc_model_weights_gibbs(x = delta_n, eta = eta_n)
      }
      weights_by_n[[g]]     <- w_n
      w_per_param_by_n[[g]] <- w_n / n_stoch
    }

    # 4b. Run the per-cell kernel over all cells (serial or PSOCK cell-split).
    #     Returns a list with per-(cell, N) central matrices and (for wis) the
    #     per-(cell, N) quantile matrices, all in cell-major [ci x grid] layout.
    n_cells <- n_locs * n_times
    use_parallel <- !is.null(cl) &&
                    inherits(cl, "cluster") &&
                    n_cells >= 2L &&
                    length(cl) >= 1L

    if (use_parallel) {
      cell_blocks <- parallel::splitIndices(n_cells, min(length(cl), n_cells))
      cell_blocks <- Filter(function(b) length(b) > 0L, cell_blocks)
      block_res <- parallel::parLapply(cl, cell_blocks, function(cells) {
        MOSAIC:::.optimize_eval_cell_block(
          cells, ns, w_per_param_by_n, .GLOBAL_cases_array, .GLOBAL_deaths_array,
          .GLOBAL_ord_c, .GLOBAL_ord_d, .GLOBAL_pid_full, n_times, objective,
          wis_probs, need_mean_c, need_mean_d)
      })
      kern <- .optimize_combine_cell_blocks(block_res, n_cells, n_grid,
                                            objective, need_mean_c, need_mean_d)
    } else {
      kern <- .optimize_eval_cell_block(
        seq_len(n_cells), ns, w_per_param_by_n, cases_array, deaths_array,
        ord_c, ord_d, pid_full, n_times, objective,
        wis_probs, need_mean_c, need_mean_d)
    }

    # 4c. For each N, scatter the per-cell central/quantile vectors into
    #     [n_locs x n_times] matrices (column-major via as.numeric), score, and
    #     record all diagnostics. Cell index ci -> [i, j] with i = ((ci-1) %/%
    #     n_times) + 1, j = ((ci-1) %% n_times) + 1, matching .presort_cell_orders.
    rows <- vector("list", n_grid)
    for (g in seq_len(n_grid)) {
      n <- ns[g]

      median_c <- .scatter_cells(kern$median_c[, g], n_locs, n_times)
      median_d <- .scatter_cells(kern$median_d[, g], n_locs, n_times)
      cen_c_flat <- if (need_mean_c) as.numeric(.scatter_cells(kern$mean_c[, g], n_locs, n_times)) else as.numeric(median_c)
      cen_d_flat <- if (need_mean_d) as.numeric(.scatter_cells(kern$mean_d[, g], n_locs, n_times)) else as.numeric(median_d)
      med_c_flat <- as.numeric(median_c)
      med_d_flat <- as.numeric(median_d)

      score_n <- switch(objective,
        "mae" = {
          mae_c <- mean(abs(cen_c_flat - obs_c_flat), na.rm = TRUE)
          mae_d <- mean(abs(cen_d_flat - obs_d_flat), na.rm = TRUE)
          -(mae_c / mean_obs_c + mae_d / mean_obs_d)
        },
        "r2_bias" = {
          r2_c   <- calc_model_R2(obs_c_flat, cen_c_flat)
          r2_d   <- calc_model_R2(obs_d_flat, cen_d_flat)
          bias_c <- calc_bias_ratio(obs_c_flat, cen_c_flat)
          bias_d <- calc_bias_ratio(obs_d_flat, cen_d_flat)
          lbc <- if (is.finite(bias_c) && bias_c > 0) abs(log(bias_c)) else 10
          lbd <- if (is.finite(bias_d) && bias_d > 0) abs(log(bias_d)) else 10
          r2_c + r2_d - 0.5 * (lbc + lbd)
        },
        "wis" = {
          q025_c <- as.numeric(.scatter_cells(kern$q025_c[, g], n_locs, n_times))
          q25_c  <- as.numeric(.scatter_cells(kern$q25_c[, g],  n_locs, n_times))
          q75_c  <- as.numeric(.scatter_cells(kern$q75_c[, g],  n_locs, n_times))
          q975_c <- as.numeric(.scatter_cells(kern$q975_c[, g], n_locs, n_times))
          q025_d <- as.numeric(.scatter_cells(kern$q025_d[, g], n_locs, n_times))
          q25_d  <- as.numeric(.scatter_cells(kern$q25_d[, g],  n_locs, n_times))
          q75_d  <- as.numeric(.scatter_cells(kern$q75_d[, g],  n_locs, n_times))
          q975_d <- as.numeric(.scatter_cells(kern$q975_d[, g], n_locs, n_times))
          wis_c <- .compute_wis_from_quantiles(obs_c_flat,
                     q025_c, q25_c, med_c_flat, q75_c, q975_c)
          wis_d <- .compute_wis_from_quantiles(obs_d_flat,
                     q025_d, q25_d, med_d_flat, q75_d, q975_d)
          -(wis_c / mean_obs_c + wis_d / mean_obs_d)
        }
      )

      mae_c_n  <- mean(abs(cen_c_flat - obs_c_flat), na.rm = TRUE)
      mae_d_n  <- mean(abs(cen_d_flat - obs_d_flat), na.rm = TRUE)
      r2_c_n   <- calc_model_R2(obs_c_flat, cen_c_flat)
      r2_d_n   <- calc_model_R2(obs_d_flat, cen_d_flat)
      bias_c_n <- calc_bias_ratio(obs_c_flat, cen_c_flat)
      bias_d_n <- calc_bias_ratio(obs_d_flat, cen_d_flat)
      ess_n    <- calc_model_ess(weights_by_n[[g]])

      rows[[g]] <- list(
        n = n, r2_cases = r2_c_n, r2_deaths = r2_d_n,
        bias_cases = bias_c_n, bias_deaths = bias_d_n,
        mae_cases = mae_c_n, mae_deaths = mae_d_n,
        ess = ess_n, score = score_n
      )
    }

    tab <- data.frame(
      n          = vapply(rows, `[[`, numeric(1), "n"),
      r2_cases   = vapply(rows, `[[`, numeric(1), "r2_cases"),
      r2_deaths  = vapply(rows, `[[`, numeric(1), "r2_deaths"),
      bias_cases = vapply(rows, `[[`, numeric(1), "bias_cases"),
      bias_deaths = vapply(rows, `[[`, numeric(1), "bias_deaths"),
      mae_cases  = vapply(rows, `[[`, numeric(1), "mae_cases"),
      mae_deaths = vapply(rows, `[[`, numeric(1), "mae_deaths"),
      ess        = vapply(rows, `[[`, numeric(1), "ess"),
      score      = vapply(rows, `[[`, numeric(1), "score"),
      stringsAsFactors = FALSE
    )
    tab$n <- as.integer(tab$n)
    tab
  }

  # When parallel, export the heavy objects ONCE (not per task) so each
  # parLapply call ships only the light per-task args (cells, weights). We avoid
  # closing over `ensemble`/the optimize frame: the worker function references
  # only the specific exported globals by name.
  if (!is.null(cl) && inherits(cl, "cluster") && (n_locs * n_times) >= 2L) {
    .GLOBAL_cases_array  <- cases_array
    .GLOBAL_deaths_array <- deaths_array
    .GLOBAL_ord_c <- ord_c
    .GLOBAL_ord_d <- ord_d
    .GLOBAL_pid_full <- pid_full
    parallel::clusterExport(cl,
      varlist = c(".GLOBAL_cases_array", ".GLOBAL_deaths_array",
                  ".GLOBAL_ord_c", ".GLOBAL_ord_d", ".GLOBAL_pid_full"),
      envir = environment())
  }

  # Stride dispatch: stride == 1 -> exhaustive (parity); stride > 1 ->
  # coarse-then-refine. In both cases eval_table rows are sorted ascending by N.
  if (stride == 1L) {
    eval_table <- .eval_ns(ns_exhaustive)
  } else {
    ns_coarse <- unique(c(seq.int(min_n, max_n, by = stride), max_n))
    coarse_tab <- .eval_ns(ns_coarse)
    n_star <- coarse_tab$n[which.max(coarse_tab$score)]
    ns_refine <- setdiff(intersect(seq.int(n_star - stride, n_star + stride),
                                   seq.int(min_n, max_n)),
                         ns_coarse)
    if (length(ns_refine) > 0L) {
      refine_tab <- .eval_ns(ns_refine)
      eval_table <- rbind(coarse_tab, refine_tab)
    } else {
      eval_table <- coarse_tab
    }
    eval_table <- eval_table[order(eval_table$n), , drop = FALSE]
    rownames(eval_table) <- NULL
  }

  n_evals <- nrow(eval_table)

  # ── 5. SELECT OPTIMAL N ──────────────────────────────────────────────

  stability_tol  <- 0.02
  scores         <- eval_table$score
  max_score      <- max(scores, na.rm = TRUE)
  score_range    <- max_score - min(scores, na.rm = TRUE)
  stability_flag <- (score_range <= stability_tol)

  if (stability_flag) {
    # Flat profile: pick largest N (regularization tiebreaker)
    optimal_idx <- n_evals
  } else {
    optimal_idx <- which.max(scores)
  }

  optimal_n       <- eval_table$n[optimal_idx]
  optimal_score   <- eval_table$score[optimal_idx]
  diagnostics_n   <- max_n
  diagnostics_score <- eval_table$score[n_evals]

  if (verbose) {
    message(sprintf("  Optimal N = %d (score = %.4f), diagnostics N = %d (score = %.4f)%s",
                    optimal_n, optimal_score, diagnostics_n, diagnostics_score,
                    if (stability_flag) " [flat profile]" else ""))
  }

  # ── 6. BUILD OPTIMIZED ENSEMBLE ──────────────────────────────────────

  optimal_indices <- 1:optimal_n
  optimal_seeds   <- if (!is.null(seeds)) seeds[optimal_indices] else NULL
  # Member-aligned seeds for the optimized subset (for medoid selection). Falls
  # back to optimal_seeds when the ensemble carried no per-member seeds.
  opt_member_seeds <- if (!is.null(ens_member_seeds)) ens_member_seeds[optimal_indices] else optimal_seeds

  # Re-compute Gibbs weights at optimal N
  ll_opt    <- likelihoods[optimal_indices]
  aic_opt   <- -2 * ll_opt
  delta_opt <- aic_opt - min(aic_opt)
  range_opt <- diff(range(delta_opt))

  if (range_opt < .Machine$double.eps) {
    optimal_weights <- rep(1 / optimal_n, optimal_n)
  } else {
    eta_opt <- 0.5 * (4.0 / range_opt)
    optimal_weights <- calc_model_weights_gibbs(x = delta_opt, eta = eta_opt)
  }

  # Slice arrays
  opt_cases  <- cases_array[, , optimal_indices, , drop = FALSE]
  opt_deaths <- deaths_array[, , optimal_indices, , drop = FALSE]

  # Re-compute all statistics (medians, means, CIs)
  # `times` (NOT `each`): param-fastest weights to match as.vector(opt_cases[i, j, , ]).
  sim_weights_opt <- rep(optimal_weights, times = n_stoch) / n_stoch

  cases_mean_m   <- matrix(NA_real_, n_locs, n_times)
  cases_median_m <- matrix(NA_real_, n_locs, n_times)
  deaths_mean_m   <- matrix(NA_real_, n_locs, n_times)
  deaths_median_m <- matrix(NA_real_, n_locs, n_times)

  cases_ci <- lapply(seq_len(n_ci_pairs), function(ci_idx) {
    list(lower = matrix(NA_real_, n_locs, n_times),
         upper = matrix(NA_real_, n_locs, n_times))
  })
  deaths_ci <- lapply(seq_len(n_ci_pairs), function(ci_idx) {
    list(lower = matrix(NA_real_, n_locs, n_times),
         upper = matrix(NA_real_, n_locs, n_times))
  })

  for (i in seq_len(n_locs)) {
    for (j in seq_len(n_times)) {
      vals_c <- as.vector(opt_cases[i, j, , ])
      vals_d <- as.vector(opt_deaths[i, j, , ])

      # Renormalize over surviving (non-NA) sims so a failed cell doesn't bias
      # the mean toward 0. Matches calc_model_ensemble()'s mean handling and the
      # weighted-median path (which renormalizes inside weighted_quantiles), so
      # the two stats stay consistent. No-op when no sim failed (weights sum to 1).
      valid_c <- is.finite(vals_c) & is.finite(sim_weights_opt) & sim_weights_opt > 0
      wsum_c  <- if (any(valid_c)) sum(sim_weights_opt[valid_c]) else 0
      cases_mean_m[i, j]    <- if (wsum_c > 0) sum(vals_c[valid_c] * sim_weights_opt[valid_c]) / wsum_c else NA_real_
      valid_d <- is.finite(vals_d) & is.finite(sim_weights_opt) & sim_weights_opt > 0
      wsum_d  <- if (any(valid_d)) sum(sim_weights_opt[valid_d]) else 0
      deaths_mean_m[i, j]   <- if (wsum_d > 0) sum(vals_d[valid_d] * sim_weights_opt[valid_d]) / wsum_d else NA_real_

      # One weighted_quantiles call per metric for median + envelope (single sort)
      qc <- weighted_quantiles(vals_c, sim_weights_opt, c(0.5, envelope_quantiles))
      qd <- weighted_quantiles(vals_d, sim_weights_opt, c(0.5, envelope_quantiles))
      cases_median_m[i, j]  <- qc[1]; all_q_c <- qc[-1]
      deaths_median_m[i, j] <- qd[1]; all_q_d <- qd[-1]

      for (ci_idx in seq_len(n_ci_pairs)) {
        lower_idx <- ci_idx
        upper_idx <- length(envelope_quantiles) - ci_idx + 1L
        cases_ci[[ci_idx]]$lower[i, j]  <- all_q_c[lower_idx]
        cases_ci[[ci_idx]]$upper[i, j]  <- all_q_c[upper_idx]
        deaths_ci[[ci_idx]]$lower[i, j] <- all_q_d[lower_idx]
        deaths_ci[[ci_idx]]$upper[i, j] <- all_q_d[upper_idx]
      }
    }
  }

  ensemble_optimized <- structure(
    list(
      cases_array               = opt_cases,
      deaths_array              = opt_deaths,
      cases_mean                = cases_mean_m,
      cases_median              = cases_median_m,
      deaths_mean               = deaths_mean_m,
      deaths_median             = deaths_median_m,
      ci_bounds                 = list(cases = cases_ci, deaths = deaths_ci),
      obs_cases                 = ensemble$obs_cases,
      obs_deaths                = ensemble$obs_deaths,
      parameter_weights         = optimal_weights,
      seeds                     = opt_member_seeds,
      n_param_sets              = as.integer(optimal_n),
      n_simulations_per_config  = n_stoch,
      n_successful              = as.integer(optimal_n * n_stoch),
      location_names            = ensemble$location_names,
      n_locations               = n_locs,
      n_time_points             = n_times,
      date_start                = ensemble$date_start,
      date_stop                 = ensemble$date_stop,
      envelope_quantiles        = envelope_quantiles
    ),
    class = "mosaic_ensemble"
  )

  # ── 7. RETURN ─────────────────────────────────────────────────────────

  structure(
    list(
      evaluation_table  = eval_table,
      optimal_n         = as.integer(optimal_n),
      optimal_score     = optimal_score,
      optimal_weights   = optimal_weights,
      optimal_indices   = optimal_indices,
      optimal_seeds     = optimal_seeds,
      ensemble_optimized = ensemble_optimized,
      stability_flag    = stability_flag,
      diagnostics_n     = as.integer(diagnostics_n),
      diagnostics_score = diagnostics_score,
      objective         = objective,
      central_method    = central_method
    ),
    class = "mosaic_subset_optimization"
  )
}


# ── Internal: per-cell presort ──────────────────────────────────────────
#
# Sort each [location, time] cell's finite predictions ONCE over the full param
# block. Returns a list (length n_locs * n_times) of integer finite-element
# indices in ascending value order. Cell index ci = (i-1)*n_times + j (i outer,
# j inner) -- the contract shared with the evaluation kernel and the scatter.
# @keywords internal
.presort_cell_orders <- function(arr, n_locs, n_times) {
  out <- vector("list", n_locs * n_times); ci <- 0L
  for (i in seq_len(n_locs)) for (j in seq_len(n_times)) {
    ci <- ci + 1L
    v   <- as.vector(arr[i, j, , ]); fin <- which(is.finite(v))
    out[[ci]] <- fin[order(v[fin])]   # finite element indices, ascending value order
  }
  out
}


# ── Internal: cell-mean (weighted, surviving-sim renormalized) ──────────
# Drop non-finite values / zero-weight sims, then weight-renormalize -- mirrors
# calc_model_ensemble()'s mean exactly so selection and final report agree.
# @keywords internal
.cell_mean <- function(vals, wts) {
  ok <- is.finite(vals) & is.finite(wts) & wts > 0
  if (!any(ok)) return(NA_real_)
  sum(vals[ok] * wts[ok]) / sum(wts[ok])
}


# ── Internal: scatter a per-cell vector into an [n_locs x n_times] matrix ──
# The per-cell vector is in ci = (i-1)*n_times + j order (i outer, j inner).
# Filling a matrix(byrow = TRUE) with that vector places cell ci at [i, j], then
# as.numeric() of the matrix flattens column-major -- matching the historical
# as.numeric(median_c) used by the scorer. (Equivalently: matrix(v, n_locs,
# n_times, byrow = TRUE).)
# @keywords internal
.scatter_cells <- function(v, n_locs, n_times) {
  matrix(v, nrow = n_locs, ncol = n_times, byrow = TRUE)
}


# ── Internal: per-cell evaluation kernel (Lever A + B unit of work) ─────
#
# For each cell in `cells` (cell index ci = (i-1)*n_times + j), gather the cell's
# finite predictions ONCE (xc/xd via the presorted order), then loop over the N
# grid computing the masked (param_id <= n) weighted median (and, for wis, the
# 5 wis quantiles) and optional weighted mean. Returns matrices [length(cells) x
# length(ns)] keyed by ROW = position within `cells` (NOT global ci). The caller
# maps row -> ci via `cells`. This is the exact per-cell math of the historical
# loop, just reorganized cell-outer; it is the unit parallelized across workers.
#
# @param cells Integer vector of global cell indices to evaluate.
# @param ns Integer vector of candidate subset sizes.
# @param w_per_param_by_n List (length(ns)) of per-param weight-share vectors.
# @param cases_array,deaths_array The 4D prediction arrays (sorted by likelihood).
# @param ord_c,ord_d Per-cell presorted finite-index lists (global ci order).
# @param pid_full Param id of each as.vector() element (param-fastest).
# @param n_times Number of time points (to decode ci -> i, j).
# @param objective One of "mae","r2_bias","wis".
# @param wis_probs The 5 wis probabilities.
# @param need_mean_c,need_mean_d Whether to compute the weighted mean per channel.
# @keywords internal
.optimize_eval_cell_block <- function(cells, ns, w_per_param_by_n,
                                      cases_array, deaths_array,
                                      ord_c, ord_d, pid_full, n_times,
                                      objective, wis_probs,
                                      need_mean_c, need_mean_d) {
  n_cell <- length(cells)
  n_grid <- length(ns)

  median_c <- matrix(NA_real_, n_cell, n_grid)
  median_d <- matrix(NA_real_, n_cell, n_grid)
  mean_c   <- if (need_mean_c) matrix(NA_real_, n_cell, n_grid) else NULL
  mean_d   <- if (need_mean_d) matrix(NA_real_, n_cell, n_grid) else NULL

  do_wis <- identical(objective, "wis")
  if (do_wis) {
    q025_c <- q25_c <- q75_c <- q975_c <- matrix(NA_real_, n_cell, n_grid)
    q025_d <- q25_d <- q75_d <- q975_d <- matrix(NA_real_, n_cell, n_grid)
  }

  for (r in seq_len(n_cell)) {
    ci <- cells[r]
    i  <- ((ci - 1L) %/% n_times) + 1L
    j  <- ((ci - 1L) %%  n_times) + 1L

    # Gather once per cell (independent of N) -- this is the Lever A hoist.
    oc <- ord_c[[ci]]; pc <- pid_full[oc]; xc <- as.vector(cases_array[i, j, , ])[oc]
    od <- ord_d[[ci]]; pd <- pid_full[od]; xd <- as.vector(deaths_array[i, j, , ])[od]

    for (g in seq_len(n_grid)) {
      n   <- ns[g]
      wpp <- w_per_param_by_n[[g]]
      kc  <- pc <= n
      kd  <- pd <= n
      xck <- xc[kc]; wck <- wpp[pc[kc]]
      xdk <- xd[kd]; wdk <- wpp[pd[kd]]

      if (do_wis) {
        qc <- weighted_quantiles_presorted(xck, wck, wis_probs)
        qd <- weighted_quantiles_presorted(xdk, wdk, wis_probs)
        q025_c[r, g] <- qc[1]; q25_c[r, g] <- qc[2]; median_c[r, g] <- qc[3]
        q75_c[r, g]  <- qc[4]; q975_c[r, g] <- qc[5]
        q025_d[r, g] <- qd[1]; q25_d[r, g] <- qd[2]; median_d[r, g] <- qd[3]
        q75_d[r, g]  <- qd[4]; q975_d[r, g] <- qd[5]
      } else {
        median_c[r, g] <- weighted_quantiles_presorted(xck, wck, 0.5)
        median_d[r, g] <- weighted_quantiles_presorted(xdk, wdk, 0.5)
      }
      if (need_mean_c) mean_c[r, g] <- .cell_mean(xck, wck)
      if (need_mean_d) mean_d[r, g] <- .cell_mean(xdk, wdk)
    }
  }

  out <- list(cells = cells, median_c = median_c, median_d = median_d,
              mean_c = mean_c, mean_d = mean_d)
  if (do_wis) {
    out$q025_c <- q025_c; out$q25_c <- q25_c; out$q75_c <- q75_c; out$q975_c <- q975_c
    out$q025_d <- q025_d; out$q25_d <- q25_d; out$q75_d <- q75_d; out$q975_d <- q975_d
  }
  out
}


# ── Internal: stitch per-worker cell blocks into global [n_cells x n_grid] ──
# Each block carries its `cells` (global ci) and row-aligned matrices. Scatter
# each block's rows into the global matrices at the block's global cell indices,
# yielding the SAME [n_cells x n_grid] matrices the serial kernel produces.
# @keywords internal
.optimize_combine_cell_blocks <- function(block_res, n_cells, n_grid,
                                          objective, need_mean_c, need_mean_d) {
  median_c <- matrix(NA_real_, n_cells, n_grid)
  median_d <- matrix(NA_real_, n_cells, n_grid)
  mean_c   <- if (need_mean_c) matrix(NA_real_, n_cells, n_grid) else NULL
  mean_d   <- if (need_mean_d) matrix(NA_real_, n_cells, n_grid) else NULL
  do_wis <- identical(objective, "wis")
  if (do_wis) {
    q025_c <- q25_c <- q75_c <- q975_c <- matrix(NA_real_, n_cells, n_grid)
    q025_d <- q25_d <- q75_d <- q975_d <- matrix(NA_real_, n_cells, n_grid)
  }

  for (b in block_res) {
    rows <- b$cells
    median_c[rows, ] <- b$median_c
    median_d[rows, ] <- b$median_d
    if (need_mean_c) mean_c[rows, ] <- b$mean_c
    if (need_mean_d) mean_d[rows, ] <- b$mean_d
    if (do_wis) {
      q025_c[rows, ] <- b$q025_c; q25_c[rows, ] <- b$q25_c
      q75_c[rows, ]  <- b$q75_c;  q975_c[rows, ] <- b$q975_c
      q025_d[rows, ] <- b$q025_d; q25_d[rows, ] <- b$q25_d
      q75_d[rows, ]  <- b$q75_d;  q975_d[rows, ] <- b$q975_d
    }
  }

  out <- list(median_c = median_c, median_d = median_d,
              mean_c = mean_c, mean_d = mean_d)
  if (do_wis) {
    out$q025_c <- q025_c; out$q25_c <- q25_c; out$q75_c <- q75_c; out$q975_c <- q975_c
    out$q025_d <- q025_d; out$q25_d <- q25_d; out$q75_d <- q75_d; out$q975_d <- q975_d
  }
  out
}


# ── Internal: WIS from empirical quantiles ──────────────────────────────
#
# Implements the Bracher et al. (2021) Weighted Interval Score formula
# using pre-computed empirical quantiles from the ensemble.
#
# @param obs_flat Flattened observed values.
# @param q025_flat,q25_flat,q50_flat,q75_flat,q975_flat Flattened quantile vectors.
# @return Scalar WIS (lower is better).
# @keywords internal
.compute_wis_from_quantiles <- function(obs_flat, q025_flat, q25_flat,
                                        q50_flat, q75_flat, q975_flat) {
  # Bracher et al. 2021: WIS = (1/(K+0.5)) * [0.5*MAE + sum_k (alpha_k/2)*IS_k]
  # K = 2 interval pairs: 50% (q25-q75) and 95% (q025-q975)

  # MAE term (0.5 coefficient per Bracher)
  mae_term <- 0.5 * mean(abs(obs_flat - q50_flat), na.rm = TRUE)

  # 50% interval (alpha = 0.5)
  alpha_50 <- 0.5
  width_50 <- q75_flat - q25_flat
  pen_lower_50 <- (2 / alpha_50) * pmax(q25_flat - obs_flat, 0)
  pen_upper_50 <- (2 / alpha_50) * pmax(obs_flat - q75_flat, 0)
  is_50 <- mean(width_50 + pen_lower_50 + pen_upper_50, na.rm = TRUE)

  # 95% interval (alpha = 0.05)
  alpha_95 <- 0.05
  width_95 <- q975_flat - q025_flat
  pen_lower_95 <- (2 / alpha_95) * pmax(q025_flat - obs_flat, 0)
  pen_upper_95 <- (2 / alpha_95) * pmax(obs_flat - q975_flat, 0)
  is_95 <- mean(width_95 + pen_lower_95 + pen_upper_95, na.rm = TRUE)

  K <- 2
  (mae_term + (alpha_50 / 2) * is_50 + (alpha_95 / 2) * is_95) / (K + 0.5)
}
