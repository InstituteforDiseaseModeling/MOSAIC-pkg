#' Assemble the masked prediction table from a mosaic_ensemble object
#'
#' Pure helper extracted from \code{plot_model_ensemble()}. Builds the tidy
#' per-location prediction table (central series + dynamic CI pairs + observed)
#' and applies the boundary-artifact mask. This is the single source of truth
#' for the exported \code{predictions_*.csv} files (whose unscored head stays
#' \code{NA}) and for every line \code{plot_model_ensemble()} draws: with
#' \code{show_burn_in = TRUE} the plotter draws this same assembly with the
#' head masks switched off (\code{.mosaic_display_prediction_table()}), so the
#' scored-window cells it draws are the CSV's cells.
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
#' \code{predicted_central} and \code{predicted_mean} summarise the engine-level
#' member trajectories. The \code{ci_*} columns are the ensemble's
#' \code{ci_bounds}, observation-level posterior predictive intervals for an
#' ensemble built with an observation model (\code{run_MOSAIC()} since
#' v0.101.0), and \code{predicted_median} is the median of the same draws:
#' the ensemble's \code{predictive_median} for a channel that received
#' observation noise (\code{ensemble$observation_model}), else the engine
#' median, which then shares its draws with the engine-level \code{ci_*}. So
#' every row's quantiles nest (\code{ci_1_lower <= ci_2_lower <=
#' predicted_median <= ci_2_upper <= ci_1_upper} for the default envelope) and
#' a weighted interval score built from them is proper. Where the reporting
#' dispersion is small the observation-level median falls far below the
#' engine-level central line, which is why the central line is not taken from
#' it.
#'
#' @param ensemble A \code{mosaic_ensemble} object from \code{calc_model_ensemble()}.
#' @param central_method Central tendency for \code{predicted_central}. Scalar
#'   or per-channel \code{c(cases=, deaths=)}; resolved via
#'   \code{.mosaic_resolve_central_method()}. Default
#'   \code{c(cases = "median", deaths = "mean")}.
#' @param n_cases_warmup_mask Integer. Leading Suspected-Cases timesteps blanked
#'   to \code{NA}. Default \code{2L}.
#' @param mask_final_deaths_step Logical. Blank the final Deaths timestep.
#'   Default \code{FALSE} (see \code{\link{plot_model_ensemble}}).
#' @param score_idx_cases,score_idx_deaths Integer (1-based) scored-window start
#'   per channel. \code{NULL} (default) reads from \code{ensemble$artifact_mask}.
#'
#' @return A data.frame with the schema above.
#' @noRd
.mosaic_assemble_prediction_table <- function(ensemble,
                                              central_method         = c(cases = "median",
                                                                         deaths = "mean"),
                                              n_cases_warmup_mask    = 2L,
                                              mask_final_deaths_step = FALSE,
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
  # The median of the draws the ci_* columns come from (see the details above).
  .quantile_median <- function(chan, engine_median) {
    pm <- ensemble$predictive_median[[chan]]
    if (isTRUE(ensemble$observation_model[[chan]]) && !is.null(pm)) pm else engine_median
  }
  cases_qmedian  <- .quantile_median("cases",  cases_median)
  deaths_qmedian <- .quantile_median("deaths", deaths_median)

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
      predicted_median  = c(.extract_loc(cases_qmedian,  i),
                            .extract_loc(deaths_qmedian, i)),
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
  # Artifact 1 (mask_final_deaths_step, off by default since v0.96.0): the
  #   laser-cholera engine wrote reported_deaths at [tick] on an array of length
  #   nticks+1, so its final slot was never written and read as a drop-to-zero.
  #   The R engine reports deaths on the cases' row, so the final slot is real.
  # Artifact 2 (n_cases_warmup_mask): the first ~1-2 reported cases steps are an
  #   IC warm-up transient. The legitimate leading reporting-lag zeros in Deaths
  #   (delta_reporting_cases; deaths are reported on the case lag) are REAL and
  #   are deliberately NOT masked.

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

#' Prediction table drawn by plot_model_ensemble() when the burn-in is shown
#'
#' The exported CSV blanks every time step before the scored window. For
#' display, \code{.mosaic_assemble_prediction_table()} is run with the head
#' masks off (no cases warm-up, scored-window starts of 1), so the cells inside
#' the scored window are exactly the CSV's cells and the unscored head carries
#' the ensemble's own central and interval series. \code{mask_final_deaths_step}
#' is an engine artifact, not part of the head, and is kept.
#'
#' When a precomputed \code{prediction_table} is supplied it is drawn verbatim,
#' except that its blank cells in the unscored head (the first
#' \code{head_cases}/\code{head_deaths} steps, matched to the ensemble by
#' location, date and metric) are filled from that unmasked assembly; the
#' central column follows the table's own \code{central_method} column and is
#' filled with the engine-level weighted median or mean, never with
#' \code{predicted_median} (the observation-level predictive median for an
#' observation-model ensemble). If the ensemble cannot be assembled, the table
#' is returned unfilled with a warning and its head stays blank.
#'
#' @param ensemble A \code{mosaic_ensemble} object.
#' @param central_method Resolved per-channel central method.
#' @param mask_final_deaths_step Logical; passed through.
#' @param head_cases,head_deaths Integer; number of leading unscored steps.
#' @param prediction_table Optional table from
#'   \code{.mosaic_assemble_prediction_table()}.
#' @return A data.frame with the prediction-table schema.
#' @noRd
.mosaic_display_prediction_table <- function(ensemble, central_method,
                                             mask_final_deaths_step,
                                             head_cases, head_deaths,
                                             prediction_table = NULL) {
  .assemble_unmasked <- function(cm = central_method) .mosaic_assemble_prediction_table(
    ensemble               = ensemble,
    central_method         = cm,
    n_cases_warmup_mask    = 0L,
    mask_final_deaths_step = mask_final_deaths_step,
    score_idx_cases        = 1L,
    score_idx_deaths       = 1L
  )

  if (is.null(prediction_table)) return(.assemble_unmasked())

  tbl <- prediction_table
  tbl$metric <- factor(as.character(tbl$metric),
                       levels = c("Suspected Cases", "Deaths"))
  # `full` supplies the interval, mean and median cells; the engine-level
  # weighted median for a median central line comes from a median assembly's
  # predicted_central, because predicted_median holds the observation-level
  # predictive median for an observation-model ensemble.
  full <- tryCatch({
    f <- .assemble_unmasked()
    f$.engine_median <- .assemble_unmasked("median")$predicted_central
    f
  }, error = function(e) {
    warning("plot_model_ensemble: the burn-in could not be drawn from the ",
            "ensemble (", conditionMessage(e), "); drawing prediction_table ",
            "as supplied, with its unscored head blank.", call. = FALSE)
    NULL
  })
  if (is.null(full)) return(tbl)

  # Time index of every row of `full`, in the assembly's construction order
  # (per location: the cases block, then the deaths block).
  full_t <- rep(rep(seq_len(ensemble$n_time_points), 2L), ensemble$n_locations)
  .key <- function(d) paste(as.character(d$location), as.character(d$date),
                            as.character(d$metric), sep = "\r")
  idx      <- match(.key(tbl), .key(full))
  is_cases <- as.character(tbl$metric) == "Suspected Cases"
  in_head  <- !is.na(idx) &
    ((is_cases & full_t[idx] <= head_cases) | (!is_cases & full_t[idx] <= head_deaths))
  rows <- which(in_head)
  if (!length(rows)) return(tbl)
  src <- idx[rows]

  chan_default <- ifelse(is_cases[rows], central_method[["cases"]],
                         central_method[["deaths"]])
  cen <- if ("central_method" %in% names(tbl)) as.character(tbl$central_method[rows]) else chan_default
  bad <- is.na(cen) | !cen %in% c("mean", "median")
  cen[bad] <- chan_default[bad]

  fill <- list(predicted_central = ifelse(cen == "median",
                                          full$.engine_median[src],
                                          full$predicted_mean[src]))
  for (cc in intersect(c("predicted_mean", "predicted_median",
                         grep("^ci_[0-9]+_(lower|upper)$", names(full), value = TRUE)),
                       names(tbl))) {
    fill[[cc]] <- full[[cc]][src]
  }
  for (cc in intersect(names(fill), names(tbl))) {
    cur <- tbl[[cc]][rows]
    na  <- is.na(cur)
    cur[na] <- fill[[cc]][na]
    tbl[[cc]][rows] <- cur
  }
  tbl
}

#' Per-channel central method a prediction table was assembled with
#'
#' Reads the table's \code{central_method} column: a channel whose rows carry a
#' single valid value (\code{"mean"} or \code{"median"}) takes it; a channel
#' with none (column absent, blank, mixed or invalid) keeps \code{fallback}.
#'
#' @param tbl A prediction table from \code{.mosaic_assemble_prediction_table()}.
#' @param fallback Resolved per-channel central method.
#' @return Named character vector \code{c(cases = , deaths = )}.
#' @noRd
.mosaic_table_central_method <- function(tbl, fallback) {
  out <- fallback
  if (!"central_method" %in% names(tbl)) return(out)
  metric <- as.character(tbl$metric)
  for (ch in c("cases", "deaths")) {
    rows <- metric == if (ch == "cases") "Suspected Cases" else "Deaths"
    v <- unique(stats::na.omit(as.character(tbl$central_method[rows])))
    if (length(v) == 1L && v %in% c("mean", "median")) out[[ch]] <- v
  }
  out
}

#' Unscored spans of a prediction figure, one row per channel
#'
#' The steps before each channel's scoring window (the cases warm-up and the
#' burn-in) as a rectangle from the panel edge to the scoring-window start,
#' plus the "scored from YYYY-MM-DD" label for the marker drawn at that start:
#' where the caption metrics and the calibration's scoring window begin (under
#' \code{cases_scoring = "weekly"} the likelihood's first complete reporting
#' week can start up to six days later).
#'
#' @param dates The figure's time axis (Date, or numeric when undated).
#' @param head_cases,head_deaths Integer; number of leading unscored steps.
#' @return A data.frame (\code{metric, xmin, xmax, ymin, ymax, ytop, label}) or
#'   \code{NULL} when neither channel has an unscored head. \code{xmax} is the
#'   scored-window start, or \code{Inf} (and \code{label} \code{NA}) when the
#'   whole series is unscored.
#' @noRd
.mosaic_unscored_spans <- function(dates, head_cases, head_deaths) {
  n     <- length(dates)
  heads <- c("Suspected Cases" = head_cases, "Deaths" = head_deaths)
  heads <- heads[heads > 0L]
  if (!length(heads) || n < 1L) return(NULL)

  is_date <- inherits(dates, "Date")
  start <- vapply(heads, function(h) if (h < n) as.numeric(dates[h + 1L]) else Inf,
                  numeric(1L))
  label <- rep(NA_character_, length(start))
  ok    <- is.finite(start)
  label[ok] <- paste0("scored from ",
                      if (is_date) format(as.Date(start[ok], origin = "1970-01-01"))
                      else paste0("t = ", start[ok]))
  xmin <- rep(-Inf, length(start))
  xmax <- unname(start)
  if (is_date) {
    xmin <- as.Date(xmin, origin = "1970-01-01")
    xmax <- as.Date(xmax, origin = "1970-01-01")
  }
  data.frame(metric = factor(names(heads), levels = c("Suspected Cases", "Deaths")),
             xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, ytop = Inf,
             label = label, stringsAsFactors = FALSE)
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
#' \code{\link{calc_model_ensemble}}. Shows the central prediction line (by
#' default the weighted median for cases and the weighted mean for deaths;
#' \code{central_method}) with interval ribbons and observed data points. The
#' line is the model's central trajectory: the weighted median (or mean) of the
#' engine-level member trajectories, before observation noise. The ribbons are
#' the ensemble's \code{ci_bounds}, observation-level posterior predictive
#' intervals when the ensemble was built with an observation model; the caption
#' then says so, and notes that the line can lie above the 50% band where the
#' reporting dispersion is small (a small weekly negative binomial size
#' \eqn{k} skews the observation-level predictive toward zero, so its upper
#' 50% bound can fall below the engine-level median). By default the
#' predictions are drawn from the first time step, and the steps before the
#' scoring window (burn-in and cases warm-up) are shaded grey, with a dashed
#' line at the scoring-window start labelled with its date (e.g. "scored from
#' 2023-02-15"; \code{show_burn_in}). That is where the caption metrics (R2,
#' bias and totals) and the calibration's scoring window start; under
#' \code{cases_scoring = "weekly"} the cases likelihood scores the complete
#' reporting weeks inside that window, so its first scored week can begin up
#' to six days after the marker.
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
#'   \code{"mean"} (the expected count, which never collapses to zero on sparse
#'   deaths) or \code{"median"} (the typical trajectory). Scalar or per-channel
#'   \code{c(cases=, deaths=)}; default \code{c(cases = "median", deaths = "mean")}
#'   (both mean from v0.98.0 to v0.100.x, both median from v0.46.1 to v0.97.x).
#' @param mask_final_deaths_step Logical. If \code{TRUE}, blank the FINAL
#'   timestep of every Deaths prediction (set the predicted/CI cells to
#'   \code{NA}) in the exported CSV and the rendered lines. This masked a
#'   laser-cholera engine off-by-one in which \code{reported_deaths} was written
#'   at \code{[tick]} on an array of length \code{nticks + 1}, so the final slot
#'   was never written and read as an artificial drop-to-zero. Since v0.96.0 the
#'   R engine reports deaths on the same row as cases, so the default is
#'   \code{FALSE}; set \code{TRUE} for an ensemble from the laser-cholera engine.
#'   DISPLAY ONLY: the underlying ensemble arrays are untouched, so any
#'   R2/bias/likelihood computed upstream from the raw object is unaffected.
#' @param n_cases_warmup_mask Integer. Number of LEADING timesteps of every
#'   Suspected Cases prediction excluded from the caption metrics. Default
#'   \code{2L}. This covers the initial-condition warm-up transient (seeded E/I
#'   progressing into new_symptomatic before the SEIR dynamics settle). With
#'   \code{show_burn_in = TRUE} these steps are drawn inside the grey unscored
#'   span; with \code{FALSE} they are blanked, as in the exported CSV. The raw
#'   arrays are untouched. The legitimate leading reporting-lag zeros in Deaths
#'   (from \code{delta_reporting_cases}, the lag deaths share with cases) are
#'   REAL and are NOT masked by this argument. Set to \code{0L} to disable.
#' @param score_idx_cases,score_idx_deaths Integer (1-based). Per-channel scored
#'   time-window START index (burn-in / deaths-era start). Timesteps strictly
#'   BEFORE the index are excluded from the caption metrics and, like the cases
#'   warm-up, shaded (\code{show_burn_in = TRUE}) or blanked
#'   (\code{show_burn_in = FALSE}). When \code{NULL} (default, the typical caller
#'   pattern) the value is read from \code{ensemble$artifact_mask$score_idx_*}
#'   so plots track the scored window the ensemble was built with; \code{1}
#'   means no burn-in. The raw arrays are untouched.
#' @param prediction_table Optional precomputed prediction table (a data.frame
#'   from \code{.mosaic_assemble_prediction_table()}). When supplied, its lines
#'   are drawn as given, so they match an already-written CSV; with
#'   \code{show_burn_in = TRUE} its blank unscored head is filled from
#'   \code{ensemble}'s own central and interval series (if \code{ensemble}
#'   cannot supply them, a warning is given and the head is left blank). The
#'   table's \code{central_method} column, when present, names the line drawn,
#'   so the caption's central label and its R2, bias and totals follow it per
#'   channel; an explicitly supplied \code{central_method} that disagrees with
#'   it draws a warning. When \code{NULL} (default) the table is assembled from
#'   \code{ensemble}.
#' @param show_burn_in Logical. If \code{TRUE} (default), draw the predicted
#'   central line and interval ribbons from the first time step, shade the
#'   steps before each channel's scoring window (burn-in and cases warm-up) in
#'   light grey, and mark the scoring-window start with a dashed line labelled
#'   with its date, e.g. "scored from 2023-02-15" (one label per channel when
#'   the starts differ). The marker is where the caption metrics and the
#'   calibration's scoring window start; under \code{cases_scoring = "weekly"}
#'   the cases likelihood scores the complete reporting weeks inside the window,
#'   so its first scored week can begin up to six days later. If \code{FALSE},
#'   blank the predictions before the scoring window, as in the exported CSV.
#'   Display only: the caption metrics are computed on the scoring window either
#'   way, and \code{mask_final_deaths_step} applies either way.
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
                                central_method   = c(cases = "median", deaths = "mean"),
                                mask_final_deaths_step = FALSE,
                                n_cases_warmup_mask    = 2L,
                                score_idx_cases        = NULL,
                                score_idx_deaths       = NULL,
                                prediction_table       = NULL,
                                show_burn_in           = TRUE,
                                verbose          = TRUE) {

  # ---------------------------------------------------------------------------
  # Validate inputs
  # ---------------------------------------------------------------------------

  if (!inherits(ensemble, "mosaic_ensemble"))
    stop("ensemble must be a mosaic_ensemble object from calc_model_ensemble()")

  if (missing(output_dir) || is.null(output_dir))
    stop("output_dir is required")

  if (!is.logical(show_burn_in) || length(show_burn_in) != 1L || is.na(show_burn_in))
    stop("show_burn_in must be TRUE or FALSE")

  # Whether the caller chose the central line (read before the argument is
  # reassigned below; see prediction_table).
  central_supplied <- !missing(central_method)

  warmup_n <- suppressWarnings(as.integer(n_cases_warmup_mask))
  if (length(warmup_n) != 1L || is.na(warmup_n) || warmup_n < 0L)
    stop("n_cases_warmup_mask must be a single non-negative integer")

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
  # A supplied table is drawn as given, so its own central_method column names
  # the line on the figure; the caption label and scored series follow it.
  if (!is.null(prediction_table)) {
    table_central <- .mosaic_table_central_method(prediction_table, central_method)
    if (central_supplied && !identical(table_central, central_method))
      warning(sprintf(paste0(
        "plot_model_ensemble: prediction_table carries central_method cases=%s, deaths=%s; ",
        "its line is drawn and the caption is labelled and scored by it, not by ",
        "central_method = c(cases = \"%s\", deaths = \"%s\")."),
        table_central[["cases"]], table_central[["deaths"]],
        central_method[["cases"]], central_method[["deaths"]]), call. = FALSE)
    central_method <- table_central
  }
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
  # The ribbons are predictive intervals for the observed counts when the
  # ensemble drew observation noise; say so, since the line is engine-level.
  # A small weekly NB size k skews that predictive toward zero, so its upper
  # 50% bound can fall below the engine-level line; the caption says that too.
  obs_level          <- isTRUE(ensemble$observation_model$cases)
  interval_kind      <- if (obs_level) " observation-level predictive" else ""
  line_note_k        <- "can lie above the 50% band where the reporting dispersion k is small"
  line_note          <- if (obs_level) paste0(
    "Line: engine-level central trajectory, before observation noise; it ", line_note_k)
  .faceted_note      <- function(where) if (obs_level) paste0(
    "\nRibbons: ", .mosaic_interval_label(envelope_quantiles),
    " observation-level predictive intervals; line: engine-level central ",
    "trajectory, before observation noise, which ", where)

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
  # Per-channel scored-window starts and unscored heads
  # ---------------------------------------------------------------------------
  # Resolved exactly as .mosaic_assemble_prediction_table() resolves them, so
  # the unscored head is the span the exported CSV blanks: the first
  # max(warm-up, score_idx_cases - 1) cases steps and score_idx_deaths - 1
  # deaths steps.
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
  head_cases  <- min(max(warmup_n, score_idx_cases - 1L), n_time_points)
  head_deaths <- min(score_idx_deaths - 1L, n_time_points)

  # ---------------------------------------------------------------------------
  # Build (or reuse) tidy plot_data frame
  # ---------------------------------------------------------------------------
  # Every table drawn here comes from the pure helper
  # .mosaic_assemble_prediction_table(), the single source of the exported
  # CSVs. With show_burn_in = TRUE the head masks are switched off for drawing
  # (.mosaic_display_prediction_table()), so the scored window is drawn from
  # the CSV's own cells and the unscored head from the ensemble; with FALSE the
  # masked table (or a supplied `prediction_table`) is drawn as is.

  if (verbose) message("Building plot data...")

  if (show_burn_in) {
    plot_data <- .mosaic_display_prediction_table(
      ensemble               = ensemble,
      central_method         = central_method,
      mask_final_deaths_step = mask_final_deaths_step,
      head_cases             = head_cases,
      head_deaths            = head_deaths,
      prediction_table       = prediction_table
    )
  } else if (!is.null(prediction_table)) {
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

  # Grey span over each channel's unscored head, ending at the scored-window
  # start (NULL when nothing is unscored or the burn-in is not shown).
  unscored <- if (show_burn_in) .mosaic_unscored_spans(dates, head_cases, head_deaths)

  # Scored central series for the caption R2/bias/totals. Built with the same
  # masking helper run_MOSAIC() uses for summary.json, driven by the same
  # warm-up / final-deaths / scored-window settings that define the unscored
  # head, so the caption scores exactly the scored window, whether or not the
  # head is drawn.
  caption_mask <- list(
    cases_warmup     = as.integer(n_cases_warmup_mask),
    deaths_final     = isTRUE(mask_final_deaths_step),
    score_idx_cases  = score_idx_cases,
    score_idx_deaths = score_idx_deaths
  )
  cases_scored  <- .mosaic_mask_central_for_scoring(cases_central,  "cases",  caption_mask)
  deaths_scored <- .mosaic_mask_central_for_scoring(deaths_central, "deaths", caption_mask)

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

  # Unscored-span layers (show_burn_in = TRUE): the grey span under the
  # ribbons, the dashed scored-window marker over the ribbons but under the
  # observed points and central line, and the "scored from" label on top.
  # Span rows carry `metric`, which places them in the per-location metric
  # facets; in the location facets of the per-channel plots they carry no
  # facet variable and so repeat in every panel.
  unscored_col  <- unname(mosaic_colors("reference"))
  unscored_text <- mosaic_color_variant(unscored_col, "darken", 0.35)

  .add_unscored_shade <- function(p, spans) {
    if (is.null(spans) || !nrow(spans)) return(p)
    p + ggplot2::geom_rect(
      data = spans,
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                   ymin = .data$ymin, ymax = .data$ymax),
      inherit.aes = FALSE, fill = unscored_col, alpha = 0.18)
  }

  .add_unscored_marker <- function(p, spans) {
    if (is.null(spans)) return(p)
    spans <- spans[is.finite(spans$xmax), , drop = FALSE]
    if (!nrow(spans)) return(p)
    p + ggplot2::geom_vline(
      data = spans, ggplot2::aes(xintercept = .data$xmax),
      inherit.aes = FALSE, linetype = "dashed", colour = unscored_col,
      linewidth = 0.45)
  }

  .add_unscored_label <- function(p, labels) {
    if (is.null(labels) || !nrow(labels)) return(p)
    labels$label <- paste0(" ", labels$label)
    p + ggplot2::geom_text(
      data = labels,
      ggplot2::aes(x = .data$xmax, y = .data$ytop, label = .data$label),
      inherit.aes = FALSE, hjust = 0, vjust = 1.4, size = 2.6,
      colour = unscored_text)
  }

  # Per-location figure: one label per channel, or a single label on the
  # cases panel when both channels' scored windows start on the same step.
  loc_labels <- NULL
  if (!is.null(unscored)) {
    loc_labels <- unscored[!is.na(unscored$label), , drop = FALSE]
    if (nrow(loc_labels) == 2L && identical(loc_labels$label[1L], loc_labels$label[2L]))
      loc_labels <- loc_labels[loc_labels$metric == "Suspected Cases", , drop = FALSE]
  }

  # Per-channel location facets: the channel's span in every panel, its label
  # in the first panel only (facet_wrap orders a character facet alphabetically).
  .channel_spans <- function(metric) {
    if (is.null(unscored)) return(NULL)
    unscored[unscored$metric == metric, , drop = FALSE]
  }
  .channel_label <- function(spans, data) {
    if (is.null(spans)) return(NULL)
    lab <- spans[!is.na(spans$label), , drop = FALSE]
    if (!nrow(lab)) return(NULL)
    panels <- if (is.factor(data$location)) levels(droplevels(data$location))
              else sort(unique(as.character(data$location)))
    lab$location <- panels[1L]
    lab
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
    # Scored (masked) central series: NA steps are dropped pairwise downstream.
    pred_c <- .extract_loc(cases_scored,  i)
    pred_d <- .extract_loc(deaths_scored, i)

    r2_c   <- tryCatch(round(calc_model_R2(obs_c, pred_c), 3L), error = function(e) NA)
    r2_d   <- tryCatch(round(calc_model_R2(obs_d, pred_d), 3L), error = function(e) NA)
    bias_c <- tryCatch(round(calc_bias_ratio(obs_c, pred_c), 2L), error = function(e) NA)
    bias_d <- tryCatch(round(calc_bias_ratio(obs_d, pred_d), 2L), error = function(e) NA)

    loc_data_points <- loc_data[!is.na(loc_data$observed), ]

    p <- ggplot2::ggplot(loc_data, ggplot2::aes(x = date))
    p <- .add_unscored_shade(p, unscored)

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

    p <- .add_unscored_marker(p, unscored)
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
          "Ribbons show ", .mosaic_interval_label(envelope_quantiles), interval_kind,
          " intervals | Central: cases=", central_method[["cases"]],
          ", deaths=", central_method[["deaths"]], "\n",
          if (obs_level) paste0(line_note, "\n"),
          "Cases: Obs = ", format(round(.paired_total(obs_c, pred_c)[["obs"]]), big.mark = ","),
          ", Pred = ",     format(round(.paired_total(obs_c, pred_c)[["pred"]]), big.mark = ","),
          ", R\u00b2 = ", ifelse(is.na(r2_c), "NA", r2_c),
          ", Bias = ",    ifelse(is.na(bias_c), "NA", bias_c),
          " | Deaths: Obs = ", format(round(.paired_total(obs_d, pred_d)[["obs"]]), big.mark = ","),
          ", Pred = ",         format(round(.paired_total(obs_d, pred_d)[["pred"]]), big.mark = ","),
          ", R\u00b2 = ", ifelse(is.na(r2_d), "NA", r2_d),
          ", Bias = ",    ifelse(is.na(bias_d), "NA", bias_d)
        )
      )

    p <- .add_unscored_label(p, loc_labels)
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
    all_pred_c <- as.numeric(cases_scored)
    r2_c_all   <- tryCatch(round(calc_model_R2(all_obs_c, all_pred_c), 3L),
                            error = function(e) NA)
    bias_c_all <- tryCatch(round(calc_bias_ratio(all_obs_c, all_pred_c), 2L),
                            error = function(e) NA)

    cases_spans <- .channel_spans("Suspected Cases")
    p_cases <- ggplot2::ggplot(cases_data, ggplot2::aes(x = date))
    p_cases <- .add_unscored_shade(p_cases, cases_spans)

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

    p_cases <- .add_unscored_marker(p_cases, cases_spans)
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
          "Total: Obs = ",    format(round(.paired_total(all_obs_c, all_pred_c)[["obs"]]), big.mark = ","),
          ", Pred = ",         format(round(.paired_total(all_obs_c, all_pred_c)[["pred"]]), big.mark = ","),
          ", R\u00b2 = ", ifelse(is.na(r2_c_all), "NA", r2_c_all),
          ", Bias = ",    ifelse(is.na(bias_c_all), "NA", bias_c_all),
          " (central: ", central_method[["cases"]], ")",
          .faceted_note(line_note_k),
          "\nGenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      )

    p_cases <- .add_unscored_label(p_cases, .channel_label(cases_spans, cases_data))
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
    all_pred_d  <- as.numeric(deaths_scored)
    r2_d_all    <- tryCatch(round(calc_model_R2(all_obs_d, all_pred_d), 3L),
                            error = function(e) NA)
    bias_d_all  <- tryCatch(round(calc_bias_ratio(all_obs_d, all_pred_d), 2L),
                            error = function(e) NA)

    deaths_spans <- .channel_spans("Deaths")
    p_deaths <- ggplot2::ggplot(deaths_data, ggplot2::aes(x = date))
    p_deaths <- .add_unscored_shade(p_deaths, deaths_spans)

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

    p_deaths <- .add_unscored_marker(p_deaths, deaths_spans)
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
          "Total: Obs = ",    format(round(.paired_total(all_obs_d, all_pred_d)[["obs"]]), big.mark = ","),
          ", Pred = ",         format(round(.paired_total(all_obs_d, all_pred_d)[["pred"]]), big.mark = ","),
          ", R\u00b2 = ", ifelse(is.na(r2_d_all), "NA", r2_d_all),
          ", Bias = ",    ifelse(is.na(bias_d_all), "NA", bias_d_all),
          " (central: ", central_method[["deaths"]], ")",
          .faceted_note("can lie above the 50% band where deaths are sparse or overdispersed"),
          "\nGenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      )

    p_deaths <- .add_unscored_label(p_deaths, .channel_label(deaths_spans, deaths_data))
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


# Observed and predicted totals over the SAME cells -- those where both are
# finite -- so a caption's "Obs", "Pred" and "Bias" describe one window. Summing
# each series over its own non-missing cells put the unobserved forecast tail in
# "Pred" and the unscored head in "Obs".
.paired_total <- function(obs, pred) {
  obs <- as.numeric(obs); pred <- as.numeric(pred)
  ok <- is.finite(obs) & is.finite(pred)
  c(obs = sum(obs[ok]), pred = sum(pred[ok]))
}

# Name the nested central intervals an envelope's quantiles form, pairing each
# lower quantile with its mirror (q_i with q_{n-i+1}): c(0.025, 0.25, 0.75, 0.975)
# gives "95% and 50%".
.mosaic_interval_label <- function(q) {
  q <- sort(as.numeric(q)); n <- length(q)
  if (n < 2L) return("no")
  k <- seq_len(n %/% 2L)
  paste0(paste0(sprintf("%g", round((q[n - k + 1L] - q[k]) * 100, 1)), "%"), collapse = " and ")
}
