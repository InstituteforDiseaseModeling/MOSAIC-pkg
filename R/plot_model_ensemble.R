#' Assemble the masked prediction table from a mosaic_ensemble object
#'
#' Pure helper extracted from \code{plot_model_ensemble()}. Builds the tidy
#' per-location prediction table (central series + dynamic CI pairs + observed)
#' and applies the DISPLAY-ONLY boundary-artifact mask. This is the single
#' source of truth for both the exported \code{predictions_*.csv} files and the
#' lines drawn by \code{plot_model_ensemble()}, so the CSV and the plot are
#' guaranteed identical.
#'
#' The returned data.frame schema (verbatim, in order) is:
#' \code{location, date, metric, observed, predicted_central, predicted_mean,
#' predicted_median, central_method, ci_<k>_lower, ci_<k>_upper} (dynamic CI
#' pairs, one per envelope quantile pair). \code{metric} is a factor with levels
#' \code{c("Suspected Cases", "Deaths")}.
#'
#' DISPLAY ONLY: the raw ensemble arrays / \code{*_mean} / \code{*_median}
#' matrices on \code{ensemble} are never mutated, so any R2/bias/likelihood
#' computed upstream from the raw object is unaffected by the mask.
#'
#' @param ensemble A \code{mosaic_ensemble} object from \code{calc_model_ensemble()}.
#' @param central_method Central tendency for \code{predicted_central}. Scalar
#'   or per-channel \code{c(cases=, deaths=)}; resolved via
#'   \code{.mosaic_resolve_central_method()}. Default \code{"median"}.
#' @param n_cases_warmup_mask Integer. Leading Suspected-Cases timesteps blanked
#'   to \code{NA}. Default \code{2L}.
#' @param mask_final_deaths_step Logical. Blank the final Deaths timestep.
#'   Default \code{TRUE}.
#' @param score_idx_cases,score_idx_deaths Integer (1-based) scored-window start
#'   per channel. \code{NULL} (default) reads from \code{ensemble$artifact_mask}.
#'
#' @return A data.frame with the schema above.
#' @noRd
.mosaic_assemble_prediction_table <- function(ensemble,
                                              central_method         = "median",
                                              n_cases_warmup_mask    = 2L,
                                              mask_final_deaths_step = TRUE,
                                              score_idx_cases        = NULL,
                                              score_idx_deaths       = NULL) {

  if (!inherits(ensemble, "mosaic_ensemble"))
    stop("ensemble must be a mosaic_ensemble object from calc_model_ensemble()")

  central_method <- .mosaic_resolve_central_method(central_method)

  cases_median  <- ensemble$cases_median
  deaths_median <- ensemble$deaths_median
  cases_mean    <- ensemble$cases_mean
  deaths_mean   <- ensemble$deaths_mean
  if (is.null(cases_mean))  cases_mean  <- cases_median
  if (is.null(deaths_mean)) deaths_mean <- deaths_median
  cases_central  <- if (central_method[["cases"]]  == "mean") cases_mean  else cases_median
  deaths_central <- if (central_method[["deaths"]] == "mean") deaths_mean else deaths_median

  obs_cases          <- ensemble$obs_cases
  obs_deaths         <- ensemble$obs_deaths
  location_names     <- ensemble$location_names
  n_locations        <- ensemble$n_locations
  n_time_points      <- ensemble$n_time_points
  envelope_quantiles <- ensemble$envelope_quantiles
  date_start         <- ensemble$date_start
  date_stop          <- ensemble$date_stop
  ci_bounds_cases    <- ensemble$ci_bounds$cases
  ci_bounds_deaths   <- ensemble$ci_bounds$deaths

  # Date axis (matches plot_model_ensemble()'s resolution exactly).
  if (!is.null(date_start) && !is.null(date_stop)) {
    dates <- seq(as.Date(date_start), as.Date(date_stop), length.out = n_time_points)
  } else if (!is.null(date_start)) {
    dates <- seq(as.Date(date_start), length.out = n_time_points, by = "week")
  } else {
    dates <- seq_len(n_time_points)
  }

  .extract_loc <- function(data, i) if (is.matrix(data)) data[i, ] else data

  n_ci_pairs <- length(envelope_quantiles) / 2L

  plot_data <- do.call(rbind, lapply(seq_len(n_locations), function(i) {
    loc_df <- data.frame(
      location          = location_names[i],
      date              = rep(dates, 2L),
      metric            = c(rep("Suspected Cases", n_time_points),
                            rep("Deaths",          n_time_points)),
      observed          = c(.extract_loc(obs_cases,    i),
                            .extract_loc(obs_deaths,   i)),
      predicted_central = c(.extract_loc(cases_central, i),
                            .extract_loc(deaths_central, i)),
      predicted_mean    = c(.extract_loc(cases_mean,   i),
                            .extract_loc(deaths_mean,  i)),
      predicted_median  = c(.extract_loc(cases_median, i),
                            .extract_loc(deaths_median, i)),
      central_method    = c(rep(central_method[["cases"]],  n_time_points),
                            rep(central_method[["deaths"]], n_time_points)),
      stringsAsFactors = FALSE
    )

    for (ci_idx in seq_len(n_ci_pairs)) {
      lower_col <- paste0("ci_", ci_idx, "_lower")
      upper_col <- paste0("ci_", ci_idx, "_upper")
      loc_df[[lower_col]] <- c(ci_bounds_cases[[ci_idx]]$lower[i, ],
                                ci_bounds_deaths[[ci_idx]]$lower[i, ])
      loc_df[[upper_col]] <- c(ci_bounds_cases[[ci_idx]]$upper[i, ],
                                ci_bounds_deaths[[ci_idx]]$upper[i, ])
    }
    loc_df
  }))

  plot_data$metric <- factor(plot_data$metric,
                              levels = c("Suspected Cases", "Deaths"))

  # ---------------------------------------------------------------------------
  # Boundary-artifact mask (DISPLAY ONLY)
  # ---------------------------------------------------------------------------
  # Artifact 1 (mask_final_deaths_step): reported_deaths is written at [tick] on
  #   an array of length nticks+1, so the final slot is never written and reads
  #   as a drop-to-zero. Cases write at [tick+1] and are fine.
  # Artifact 2 (n_cases_warmup_mask): the first ~1-2 reported cases steps are an
  #   IC warm-up transient. The legitimate leading reporting-lag zeros in Deaths
  #   (delta_reporting_deaths) are REAL and are deliberately NOT masked.

  n_cases_warmup_mask <- as.integer(n_cases_warmup_mask)
  if (length(n_cases_warmup_mask) != 1L || is.na(n_cases_warmup_mask) ||
      n_cases_warmup_mask < 0L)
    stop("n_cases_warmup_mask must be a single non-negative integer")

  .resolve_score_idx <- function(arg, field) {
    if (!is.null(arg)) {
      v <- as.integer(arg)
    } else {
      v <- tryCatch(as.integer(ensemble$artifact_mask[[field]]), error = function(e) NA_integer_)
    }
    if (length(v) != 1L || is.na(v) || v < 1L) 1L else v
  }
  score_idx_cases  <- .resolve_score_idx(score_idx_cases,  "score_idx_cases")
  score_idx_deaths <- .resolve_score_idx(score_idx_deaths, "score_idx_deaths")

  if (isTRUE(mask_final_deaths_step) || n_cases_warmup_mask > 0L ||
      score_idx_cases > 1L || score_idx_deaths > 1L) {
    pred_cols <- c("predicted_central", "predicted_mean", "predicted_median")
    ci_cols   <- grep("^ci_[0-9]+_(lower|upper)$", names(plot_data), value = TRUE)
    mask_cols <- intersect(c(pred_cols, ci_cols), names(plot_data))

    is_cases  <- plot_data$metric == "Suspected Cases"
    is_deaths <- plot_data$metric == "Deaths"

    rows_to_mask <- logical(nrow(plot_data))

    cases_head <- max(n_cases_warmup_mask, score_idx_cases - 1L)
    if (cases_head > 0L) {
      k <- min(cases_head, n_time_points)
      warmup_pos <- seq_len(k)
      for (loc_i in location_names) {
        sel <- which(plot_data$location == loc_i & is_cases)
        if (length(sel) >= 1L) rows_to_mask[sel[warmup_pos]] <- TRUE
      }
    }

    deaths_head <- score_idx_deaths - 1L
    if (deaths_head > 0L) {
      k <- min(deaths_head, n_time_points)
      deaths_pos <- seq_len(k)
      for (loc_i in location_names) {
        sel <- which(plot_data$location == loc_i & is_deaths)
        if (length(sel) >= 1L) rows_to_mask[sel[deaths_pos]] <- TRUE
      }
    }

    if (isTRUE(mask_final_deaths_step) && n_time_points >= 1L) {
      for (loc_i in location_names) {
        sel <- which(plot_data$location == loc_i & is_deaths)
        if (length(sel) >= 1L) rows_to_mask[sel[length(sel)]] <- TRUE
      }
    }

    if (any(rows_to_mask)) {
      for (cc in mask_cols) plot_data[rows_to_mask, cc] <- NA_real_
    }
  }

  plot_data
}

#' Write per-location ensemble prediction CSVs
#'
#' Writes the assembled prediction table (one CSV per location) into
#' \code{data_dir} as \code{predictions_<file_prefix>_<LOC>.csv}. This is the
#' unconditional data-write path used by \code{run_MOSAIC()} (independent of
#' plotting). The table is produced by \code{.mosaic_assemble_prediction_table()}.
#'
#' @param prediction_table data.frame from \code{.mosaic_assemble_prediction_table()}.
#' @param data_dir Directory to write CSVs into (created if absent).
#' @param file_prefix Filename prefix (e.g. \code{"ensemble"}, \code{"medoid"}).
#' @param verbose Logical; print progress messages.
#' @return Invisibly, the character vector of written file paths.
#' @noRd
.mosaic_write_prediction_csvs <- function(prediction_table, data_dir,
                                          file_prefix = "ensemble",
                                          verbose = TRUE) {
  if (is.null(prediction_table) || nrow(prediction_table) == 0L)
    return(invisible(character(0)))
  if (!dir.exists(data_dir))
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  locs <- unique(as.character(prediction_table$location))
  written <- character(0)
  for (loc in locs) {
    loc_df  <- prediction_table[as.character(prediction_table$location) == loc, ]
    csv_out <- file.path(data_dir, paste0("predictions_", file_prefix, "_", loc, ".csv"))
    utils::write.csv(loc_df, csv_out, row.names = FALSE)
    written <- c(written, csv_out)
    if (verbose) message("  Saved: ", csv_out)
  }
  invisible(written)
}

#' Plot Ensemble Predictions from a mosaic_ensemble Object
#'
#' @description
#' Renders time-series plots from a \code{mosaic_ensemble} object produced by
#' \code{\link{calc_model_ensemble}}. Shows the weighted median prediction line
#' with confidence interval ribbons and observed data points. Optionally saves
#' per-location prediction CSVs for downstream use.
#'
#' @param ensemble A \code{mosaic_ensemble} object returned by
#'   \code{\link{calc_model_ensemble}}.
#' @param output_dir Character. Directory where plots are saved (and CSVs,
#'   when \code{data_dir} is not provided). Created if it does not exist.
#' @param data_dir Character. Retained for back-compat. Formerly the directory
#'   where per-location prediction CSVs were written; CSV writing has moved out
#'   of this function (see \code{save_predictions}). Ignored.
#' @param file_prefix Character. Prefix used in output filenames:
#'   \code{predictions_<prefix>_<LOC>.pdf/csv} for per-location outputs and
#'   \code{predictions_<prefix>_cases_all.pdf} / \code{_deaths_all.pdf} for
#'   multi-location overview plots. Default \code{"ensemble"}.
#' @param title_label Character. Leading label used in plot titles
#'   (\code{"<title_label>: <LOC>"}). Default \code{"Posterior Ensemble"}.
#' @param save_predictions Logical. \strong{Deprecated} and now a no-op.
#'   Prediction CSVs are written unconditionally by \code{run_MOSAIC()} via
#'   \code{.mosaic_assemble_prediction_table()} /
#'   \code{.mosaic_write_prediction_csvs()} (independent of plotting) and can be
#'   regenerated by \code{\link{render_MOSAIC_figures}}. Passing \code{TRUE}
#'   emits a one-time deprecation warning. Default \code{FALSE}.
#' @param central_method Central tendency for the plotted/scored line:
#'   \code{"median"} (default; lower calibration bias) or \code{"mean"}
#'   (unbiased for expected counts, never collapses on sparse deaths, unmasks
#'   implied-CFR bias). Scalar or per-channel \code{c(cases=, deaths=)}.
#' @param mask_final_deaths_step Logical. If \code{TRUE} (default), blank the
#'   FINAL timestep of every Deaths prediction (set the predicted/CI cells to
#'   \code{NA}) in the exported CSV and the rendered lines. This masks a
#'   laser-cholera engine off-by-one in which \code{reported_deaths} is written
#'   at \code{[tick]} on an array of length \code{nticks + 1}, so the final slot
#'   is never written and reads as an artificial drop-to-zero. DISPLAY ONLY:
#'   the underlying ensemble arrays are untouched, so any R2/bias/likelihood
#'   computed upstream from the raw object is unaffected. Cases are written at
#'   \code{[tick + 1]} and are not affected.
#' @param n_cases_warmup_mask Integer. Number of LEADING timesteps of every
#'   Suspected Cases prediction to blank (set to \code{NA}) in the exported CSV
#'   and the rendered lines. Default \code{2L}. This masks the initial-condition
#'   warm-up transient (seeded E/I progressing into new_symptomatic before the
#'   SEIR dynamics settle), which is visually dominant for low-count countries.
#'   DISPLAY ONLY (raw arrays untouched). The legitimate leading
#'   reporting-lag zeros in Deaths (from \code{delta_reporting_deaths}) are REAL
#'   and are NOT masked by this argument. Set to \code{0L} to disable.
#' @param score_idx_cases,score_idx_deaths Integer (1-based). Per-channel scored
#'   time-window START index (burn-in / deaths-era start). Leading timesteps
#'   strictly BEFORE the index are blanked (set to \code{NA}) in the exported CSV
#'   and the rendered lines so the plot shows only the scored window. Default
#'   \code{1L} (no blanking). When \code{NULL} (the typical caller pattern) the
#'   value is read from \code{ensemble$artifact_mask$score_idx_*} so plots
#'   automatically track the scored window the ensemble was built with. DISPLAY
#'   ONLY (raw arrays untouched).
#' @param prediction_table Optional precomputed prediction table (a data.frame
#'   from \code{.mosaic_assemble_prediction_table()}). When supplied, the plotter
#'   uses it directly instead of re-assembling from \code{ensemble}, guaranteeing
#'   the rendered lines match an already-written CSV (the renderer path). When
#'   \code{NULL} (default) the table is assembled internally with the masking
#'   arguments above.
#' @param verbose Logical. Print progress messages. Default \code{TRUE}.
#'
#' @return Invisibly returns a list with:
#' \describe{
#'   \item{individual}{Named list of ggplot objects, one per location.}
#'   \item{cases_faceted}{Faceted cases plot (multi-location only).}
#'   \item{deaths_faceted}{Faceted deaths plot (multi-location only).}
#'   \item{simulation_stats}{Simulation metadata from the ensemble object.}
#' }
#'
#' @seealso \code{\link{calc_model_ensemble}} to compute the ensemble.
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_point geom_line facet_grid
#'   facet_wrap scale_color_manual scale_fill_manual scale_y_continuous
#'   scale_x_date theme_minimal theme element_text element_blank labs ggsave
#' @importFrom dplyr filter mutate
#' @importFrom scales comma
plot_model_ensemble <- function(ensemble,
                                output_dir,
                                data_dir         = NULL,
                                file_prefix      = "ensemble",
                                title_label      = "Posterior Ensemble",
                                save_predictions = FALSE,
                                central_method   = "median",
                                mask_final_deaths_step = TRUE,
                                n_cases_warmup_mask    = 2L,
                                score_idx_cases        = NULL,
                                score_idx_deaths       = NULL,
                                prediction_table       = NULL,
                                verbose          = TRUE) {

  # ---------------------------------------------------------------------------
  # Validate inputs
  # ---------------------------------------------------------------------------

  if (!inherits(ensemble, "mosaic_ensemble"))
    stop("ensemble must be a mosaic_ensemble object from calc_model_ensemble()")

  if (missing(output_dir) || is.null(output_dir))
    stop("output_dir is required")

  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # save_predictions is deprecated: prediction CSVs are now written
  # unconditionally by run_MOSAIC() via .mosaic_assemble_prediction_table() /
  # .mosaic_write_prediction_csvs(), independent of plotting. The argument is
  # retained as a no-op for back-compat; passing TRUE warns once.
  if (isTRUE(save_predictions)) {
    warning("`save_predictions` is deprecated and now a no-op. Prediction CSVs ",
            "are written unconditionally by run_MOSAIC() (and can be regenerated ",
            "by render_MOSAIC_figures()); plot_model_ensemble() no longer writes ",
            "them. See .mosaic_assemble_prediction_table().", call. = FALSE)
  }

  # Unpack ensemble fields
  central_method     <- .mosaic_resolve_central_method(central_method)
  cases_median       <- ensemble$cases_median
  deaths_median      <- ensemble$deaths_median
  cases_mean         <- ensemble$cases_mean
  deaths_mean        <- ensemble$deaths_mean
  if (is.null(cases_mean))  cases_mean  <- cases_median
  if (is.null(deaths_mean)) deaths_mean <- deaths_median
  cases_central      <- if (central_method[["cases"]]  == "mean") cases_mean  else cases_median
  deaths_central     <- if (central_method[["deaths"]] == "mean") deaths_mean else deaths_median
  obs_cases          <- ensemble$obs_cases
  obs_deaths         <- ensemble$obs_deaths
  location_names     <- ensemble$location_names
  n_locations        <- ensemble$n_locations
  n_time_points      <- ensemble$n_time_points
  n_successful       <- ensemble$n_successful
  n_param_sets       <- ensemble$n_param_sets
  n_stoch_per        <- ensemble$n_simulations_per_config
  envelope_quantiles <- ensemble$envelope_quantiles
  date_start         <- ensemble$date_start
  date_stop          <- ensemble$date_stop

  # ---------------------------------------------------------------------------
  # Handle dates
  # ---------------------------------------------------------------------------

  if (!is.null(date_start) && !is.null(date_stop)) {
    dates <- seq(as.Date(date_start), as.Date(date_stop), length.out = n_time_points)
  } else if (!is.null(date_start)) {
    dates <- seq(as.Date(date_start), length.out = n_time_points, by = "week")
  } else {
    dates <- seq_len(n_time_points)
    if (verbose) message("Warning: no date info in ensemble. Using numeric time points.")
  }

  use_date_axis <- inherits(dates, "Date")

  # ---------------------------------------------------------------------------
  # Helper: extract data for a single location
  # ---------------------------------------------------------------------------

  .extract_loc <- function(data, i) if (is.matrix(data)) data[i, ] else data

  # ---------------------------------------------------------------------------
  # Build (or reuse) tidy plot_data frame
  # ---------------------------------------------------------------------------
  # The masked prediction table is assembled by the pure helper
  # .mosaic_assemble_prediction_table() so the plotted lines and the exported
  # CSVs are guaranteed identical (single source of truth). A precomputed table
  # may be passed via `prediction_table` (the renderer path); otherwise it is
  # built here from the ensemble with the same masking arguments.

  if (verbose) message("Building plot data...")

  if (!is.null(prediction_table)) {
    plot_data <- prediction_table
    plot_data$metric <- factor(as.character(plot_data$metric),
                               levels = c("Suspected Cases", "Deaths"))
  } else {
    plot_data <- .mosaic_assemble_prediction_table(
      ensemble               = ensemble,
      central_method         = central_method,
      n_cases_warmup_mask    = n_cases_warmup_mask,
      mask_final_deaths_step = mask_final_deaths_step,
      score_idx_cases        = score_idx_cases,
      score_idx_deaths       = score_idx_deaths
    )
  }

  n_ci_pairs <- length(envelope_quantiles) / 2L

  # Re-derive the per-channel scored-window starts for the in-plot annotation
  # masking below (the table itself is already masked by the helper).
  .resolve_score_idx <- function(arg, field) {
    if (!is.null(arg)) {
      v <- as.integer(arg)
    } else {
      v <- tryCatch(as.integer(ensemble$artifact_mask[[field]]), error = function(e) NA_integer_)
    }
    if (length(v) != 1L || is.na(v) || v < 1L) 1L else v
  }
  score_idx_cases  <- .resolve_score_idx(score_idx_cases,  "score_idx_cases")
  score_idx_deaths <- .resolve_score_idx(score_idx_deaths, "score_idx_deaths")

  # ---------------------------------------------------------------------------
  # Plotting helpers
  # ---------------------------------------------------------------------------

  # Window-length-adaptive x-axis breaks: a fixed "3 months" produces ~44
  # unreadable ticks over an 11-year (2015) window. Choose a break interval that
  # targets ~10-15 ticks given the actual date span, walking a ladder of
  # human-friendly intervals (month -> 3 months -> 6 months -> year -> multi-year).
  # Returns the chosen break interval IN MONTHS (integer). Targets ~10-15 ticks
  # over the actual date span, walking a ladder of human-friendly intervals
  # (month -> 3 months -> 6 months -> year -> multi-year).
  .date_break_months <- function(d) {
    if (!inherits(d, "Date") || length(d) < 2L) return(3L)
    span_mo    <- as.numeric(max(d) - min(d)) / 30.4375
    target     <- 12  # aim for ~10-15 ticks
    candidates <- c(1, 3, 6, 12, 24, 36, 60, 120)  # interval lengths in months
    # smallest interval that yields <= target ticks
    pick <- candidates[which((span_mo / candidates) <= target)[1L]]
    if (is.na(pick)) pick <- candidates[length(candidates)]
    pick
  }

  .add_date_scale <- function(p) {
    if (!use_date_axis) return(p)
    m   <- .date_break_months(dates)
    brk <- if (m %% 12 == 0) {
             yrs <- m %/% 12
             if (yrs == 1L) "1 year" else sprintf("%d years", yrs)
           } else sprintf("%d months", m)
    # Label format is keyed to the BREAK INTERVAL, not the overall span. With
    # sub-annual breaks (e.g. 6 months over a multi-year window) a year-only
    # label renders two identical year ticks per year ("2026", "2026"), which
    # reads as a duplicate/inaccurate axis. So show the month whenever breaks are
    # finer than a year; use year-only for annual-or-coarser breaks.
    lbl <- if (m >= 12) "%Y" else "%b %Y"
    p + ggplot2::scale_x_date(date_breaks = brk, date_labels = lbl)
  }

  ribbon_alphas <- seq(0.2, 0.5, length.out = n_ci_pairs)
  total_sims    <- n_param_sets * n_stoch_per

  plot_list <- list(individual = list())

  # ---------------------------------------------------------------------------
  # 1. Individual location plots
  # ---------------------------------------------------------------------------

  if (verbose) message("Generating individual location plots...")

  for (i in seq_len(n_locations)) {

    loc      <- location_names[i]
    loc_data <- plot_data[plot_data$location == loc, ]

    obs_c  <- .extract_loc(obs_cases,    i)
    obs_d  <- .extract_loc(obs_deaths,   i)
    pred_c <- .extract_loc(cases_central, i)
    pred_d <- .extract_loc(deaths_central, i)

    # Blank the unscored head of the central series so the displayed R2/bias
    # annotations match the scored window (NA dropped pairwise downstream).
    if (score_idx_cases > 1L)
      pred_c[seq_len(min(score_idx_cases - 1L, length(pred_c)))] <- NA_real_
    if (score_idx_deaths > 1L)
      pred_d[seq_len(min(score_idx_deaths - 1L, length(pred_d)))] <- NA_real_

    r2_c   <- tryCatch(round(calc_model_R2(obs_c, pred_c), 3L), error = function(e) NA)
    r2_d   <- tryCatch(round(calc_model_R2(obs_d, pred_d), 3L), error = function(e) NA)
    bias_c <- tryCatch(round(calc_bias_ratio(obs_c, pred_c), 2L), error = function(e) NA)
    bias_d <- tryCatch(round(calc_bias_ratio(obs_d, pred_d), 2L), error = function(e) NA)

    loc_data_points <- loc_data[!is.na(loc_data$observed), ]

    p <- ggplot2::ggplot(loc_data, ggplot2::aes(x = date))

    # Add CI ribbons from widest to narrowest
    for (ci_idx in seq_len(n_ci_pairs)) {
      lower_col <- paste0("ci_", ci_idx, "_lower")
      upper_col <- paste0("ci_", ci_idx, "_upper")
      p <- p +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[lower_col]],
                                           ymax = .data[[upper_col]],
                                           fill = metric),
                             alpha = ribbon_alphas[ci_idx])
    }

    p <- p +
      ggplot2::geom_point(data = loc_data_points, ggplot2::aes(y = observed),
                          color = mosaic_colors("data"), size = 1.5, alpha = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = predicted_central, color = metric),
                         linewidth = 0.75) +
      ggplot2::facet_grid(metric ~ ., scales = "free_y", switch = "y") +
      ggplot2::scale_color_manual(
        values = c("Suspected Cases" = unname(mosaic_colors("cases")),
                   "Deaths"          = unname(mosaic_colors("deaths"))),
        guide = "none"
      ) +
      ggplot2::scale_fill_manual(
        values = c("Suspected Cases" = unname(mosaic_colors("cases")),
                   "Deaths"          = unname(mosaic_colors("deaths"))),
        guide = "none"
      ) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      theme_mosaic(base_size = 10) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                     strip.placement = "outside") +
      ggplot2::labs(
        x = if (use_date_axis) "Date" else "Time",
        y = NULL,
        title = paste0(title_label, ": ", loc),
        subtitle = if (n_param_sets == 1L) {
          paste0(n_stoch_per, " stochastic reruns from single parameter set")
        } else {
          paste0(
            n_param_sets, " parameter sets \u00d7 ",
            n_stoch_per, " stochastic = ",
            total_sims, " total simulations"
          )
        },
        caption = paste0(
          "Ribbons show ", paste(
            paste0(round(envelope_quantiles[seq(1, length(envelope_quantiles), by = 2)] * 100), "-",
                   round(envelope_quantiles[seq(2, length(envelope_quantiles), by = 2)] * 100), "%"),
            collapse = " and "
          ), " confidence intervals | Central: cases=", central_method[["cases"]],
          ", deaths=", central_method[["deaths"]], "\n",
          "Cases: Obs = ", format(round(sum(obs_c, na.rm = TRUE)), big.mark = ","),
          ", Pred = ",     format(round(sum(pred_c, na.rm = TRUE)), big.mark = ","),
          ", R\u00b2 = ", ifelse(is.na(r2_c), "NA", r2_c),
          ", Bias = ",    ifelse(is.na(bias_c), "NA", bias_c),
          " | Deaths: Obs = ", format(round(sum(obs_d, na.rm = TRUE)), big.mark = ","),
          ", Pred = ",         format(round(sum(pred_d, na.rm = TRUE)), big.mark = ","),
          ", R\u00b2 = ", ifelse(is.na(r2_d), "NA", r2_d),
          ", Bias = ",    ifelse(is.na(bias_d), "NA", bias_d)
        )
      )

    p <- .add_date_scale(p)

    plot_list$individual[[loc]] <- p
    if (verbose) print(p)

    out_file <- file.path(output_dir, paste0("predictions_", file_prefix, "_", loc, ".pdf"))
    ggplot2::ggsave(out_file, plot = p, width = 10, height = 6, dpi = 300)
    if (verbose) message("  Saved: ", out_file)
  }

  # ---------------------------------------------------------------------------
  # 2. Faceted plots (multi-location only)
  # ---------------------------------------------------------------------------

  if (n_locations > 1L) {

    # ----- Faceted cases plot -------------------------------------------------

    if (verbose) message("Generating faceted cases plot...")

    cases_data <- plot_data[plot_data$metric == "Suspected Cases", ]
    all_obs_c  <- as.numeric(obs_cases)
    all_pred_c <- as.numeric(cases_central)
    r2_c_all   <- tryCatch(round(calc_model_R2(all_obs_c, all_pred_c), 3L),
                            error = function(e) NA)
    bias_c_all <- tryCatch(round(calc_bias_ratio(all_obs_c, all_pred_c), 2L),
                            error = function(e) NA)

    p_cases <- ggplot2::ggplot(cases_data, ggplot2::aes(x = date))

    for (ci_idx in seq_len(n_ci_pairs)) {
      lower_col <- paste0("ci_", ci_idx, "_lower")
      upper_col <- paste0("ci_", ci_idx, "_upper")
      p_cases <- p_cases +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[lower_col]],
                                           ymax = .data[[upper_col]]),
                             fill  = mosaic_color_variant(unname(mosaic_colors("cases")), "lighten", 0.3),
                             alpha = ribbon_alphas[ci_idx])
    }

    cases_data_points <- cases_data[!is.na(cases_data$observed), ]

    p_cases <- p_cases +
      ggplot2::geom_point(data = cases_data_points, ggplot2::aes(y = observed),
                          color = mosaic_colors("data"), size = 1.5, alpha = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = predicted_central),
                         color = mosaic_colors("cases"), linewidth = 0.8) +
      ggplot2::facet_wrap(~ location, scales = "free_y",
                          ncol = min(3L, n_locations)) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      theme_mosaic(base_size = 10) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8)) +
      ggplot2::labs(
        x = if (use_date_axis) "Date" else "Time", y = "Suspected Cases",
        title    = paste0(title_label, ": Suspected Cases by Location"),
        subtitle = if (n_param_sets == 1L) {
          paste0(n_stoch_per, " stochastic reruns from single parameter set | ",
                 n_successful, " successful sims")
        } else {
          paste0(n_param_sets, " parameter sets \u00d7 ", n_stoch_per,
                 " stochastic | ", n_successful, " successful sims")
        },
        caption  = paste0(
          "Total: Obs = ",    format(round(sum(obs_cases, na.rm = TRUE)), big.mark = ","),
          ", Pred = ",         format(round(sum(cases_central, na.rm = TRUE)), big.mark = ","),
          ", R\u00b2 = ", ifelse(is.na(r2_c_all), "NA", r2_c_all),
          ", Bias = ",    ifelse(is.na(bias_c_all), "NA", bias_c_all),
          " (central: ", central_method[["cases"]], ")",
          "\nGenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      )

    p_cases <- .add_date_scale(p_cases)
    plot_list$cases_faceted <- p_cases
    if (verbose) print(p_cases)

    plot_w <- if (n_locations <= 3L) 12 else if (n_locations <= 6L) 14 else 16
    plot_h <- if (n_locations <= 3L) 5  else if (n_locations <= 6L) 8  else
              max(10, ceiling(n_locations / 3L) * 3L)

    out_file <- file.path(output_dir, paste0("predictions_", file_prefix, "_cases_all.pdf"))
    ggplot2::ggsave(out_file, plot = p_cases,
                    width = plot_w, height = plot_h, dpi = 300, limitsize = FALSE)
    if (verbose) message("  Saved: ", out_file)

    # ----- Faceted deaths plot ------------------------------------------------

    if (verbose) message("Generating faceted deaths plot...")

    deaths_data <- plot_data[plot_data$metric == "Deaths", ]
    all_obs_d   <- as.numeric(obs_deaths)
    all_pred_d  <- as.numeric(deaths_central)
    r2_d_all    <- tryCatch(round(calc_model_R2(all_obs_d, all_pred_d), 3L),
                            error = function(e) NA)
    bias_d_all  <- tryCatch(round(calc_bias_ratio(all_obs_d, all_pred_d), 2L),
                            error = function(e) NA)

    p_deaths <- ggplot2::ggplot(deaths_data, ggplot2::aes(x = date))

    for (ci_idx in seq_len(n_ci_pairs)) {
      lower_col <- paste0("ci_", ci_idx, "_lower")
      upper_col <- paste0("ci_", ci_idx, "_upper")
      p_deaths <- p_deaths +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[lower_col]],
                                           ymax = .data[[upper_col]]),
                             fill  = mosaic_color_variant(unname(mosaic_colors("deaths")), "lighten", 0.3),
                             alpha = ribbon_alphas[ci_idx])
    }

    deaths_data_points <- deaths_data[!is.na(deaths_data$observed), ]

    p_deaths <- p_deaths +
      ggplot2::geom_point(data = deaths_data_points, ggplot2::aes(y = observed),
                          color = mosaic_colors("data"), size = 1.5, alpha = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = predicted_central),
                         color = mosaic_colors("deaths"), linewidth = 0.8) +
      ggplot2::facet_wrap(~ location, scales = "free_y",
                          ncol = min(3L, n_locations)) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      theme_mosaic(base_size = 10) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8)) +
      ggplot2::labs(
        x = if (use_date_axis) "Date" else "Time", y = "Deaths",
        title    = paste0(title_label, ": Deaths by Location"),
        subtitle = if (n_param_sets == 1L) {
          paste0(n_stoch_per, " stochastic reruns from single parameter set | ",
                 n_successful, " successful sims")
        } else {
          paste0(n_param_sets, " parameter sets \u00d7 ", n_stoch_per,
                 " stochastic | ", n_successful, " successful sims")
        },
        caption  = paste0(
          "Total: Obs = ",    format(round(sum(obs_deaths, na.rm = TRUE)), big.mark = ","),
          ", Pred = ",         format(round(sum(deaths_central, na.rm = TRUE)), big.mark = ","),
          ", R\u00b2 = ", ifelse(is.na(r2_d_all), "NA", r2_d_all),
          ", Bias = ",    ifelse(is.na(bias_d_all), "NA", bias_d_all),
          " (central: ", central_method[["deaths"]], ")",
          "\nGenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      )

    p_deaths <- .add_date_scale(p_deaths)
    plot_list$deaths_faceted <- p_deaths
    if (verbose) print(p_deaths)

    out_file <- file.path(output_dir, paste0("predictions_", file_prefix, "_deaths_all.pdf"))
    ggplot2::ggsave(out_file, plot = p_deaths,
                    width = plot_w, height = plot_h, dpi = 300, limitsize = FALSE)
    if (verbose) message("  Saved: ", out_file)
  }

  # ---------------------------------------------------------------------------
  # Return
  # ---------------------------------------------------------------------------

  plot_list$simulation_stats <- list(
    n_param_sets              = n_param_sets,
    n_simulations_per_config  = n_stoch_per,
    n_successful              = n_successful,
    envelope_quantiles        = envelope_quantiles
  )

  if (verbose) message("plot_model_ensemble complete.")
  invisible(plot_list)
}
