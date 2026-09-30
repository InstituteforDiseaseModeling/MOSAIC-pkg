#' Assemble the model convergence-status table
#'
#' Reads the calibration convergence diagnostics JSON from a results directory
#' and assembles the metric/value/target/status table that
#' \code{\link{plot_model_convergence_status}} renders. The derived
#' \code{convergence_status.csv} (distinct from the raw
#' \code{convergence_results.parquet}) is written here so it is produced
#' independently of plotting, and the plot consumes the returned table.
#'
#' Auto-detects the diagnostics file in this order:
#' \code{convergence_diagnostics.json} (likelihood-based) then
#' \code{convergence_diagnostics_loss.json} (loss-based).
#'
#' @param results_dir Path to the results directory containing the convergence
#'   diagnostics JSON (and, optionally, \code{parameter_ess.csv}).
#' @param output_dir Optional directory to write \code{convergence_status.csv}.
#'   When \code{NULL} (default) no file is written.
#' @param verbose Logical; print progress messages.
#'
#' @return A list with: \code{metrics_data} (data.frame with columns
#'   \code{Metric}, \code{Description}, \code{Target}, \code{Value},
#'   \code{Status}), \code{metric_expressions} (list of plotmath expressions,
#'   aligned row-for-row with \code{metrics_data}), \code{diagnostics} (the
#'   parsed JSON), \code{param_ess_data} (data.frame or \code{NULL}),
#'   \code{n_params}, \code{n_pass}, and \code{target_ess_param}. Returns
#'   \code{NULL} if no diagnostics file is found or no metrics could be assembled.
#'
#' @export
calc_model_convergence_status <- function(results_dir,
                                          output_dir = NULL,
                                          verbose = TRUE) {

  if (!dir.exists(results_dir)) stop("results_dir does not exist: ", results_dir)

  diagnostics_files <- c(
    file.path(results_dir, "convergence_diagnostics.json"),      # Likelihood-based
    file.path(results_dir, "convergence_diagnostics_loss.json")  # Loss-based
  )

  diagnostics_file <- NULL
  for (file in diagnostics_files) {
    if (file.exists(file)) {
      diagnostics_file <- file
      break
    }
  }

  if (is.null(diagnostics_file)) {
    if (verbose) message("No convergence diagnostics file found in ", results_dir)
    return(invisible(NULL))
  }

  if (verbose) message("Reading convergence diagnostics from: ", diagnostics_file)

  diagnostics <- jsonlite::read_json(diagnostics_file)

  # --- Prepare data for table -------------------------------------------------
  metrics_data <- data.frame(
    Metric = character(),
    Description = character(),
    Target = character(),
    Value = character(),
    Status = character(),
    stringsAsFactors = FALSE
  )
  metric_expressions <- list()

  # Row 1: N_sim
  n_valid <- if (!is.null(diagnostics$summary$total_simulations_original)) {
    if (!is.null(diagnostics$summary$n_successful)) {
      diagnostics$summary$n_successful
    } else {
      diagnostics$summary$total_simulations_original
    }
  } else {
    NA
  }

  if (!is.na(n_valid)) {
    metrics_data <- rbind(metrics_data, data.frame(
      Metric = "N_sim",
      Description = "Total number of simulations that completed successfully",
      Target = "-",
      Value = format(n_valid, big.mark = ","),
      Status = "info",
      stringsAsFactors = FALSE
    ))
    metric_expressions[[length(metric_expressions) + 1]] <- expression(bold(N[sim]))
  }

  # Row 2: N_retained
  if (!is.null(diagnostics$summary$retained_simulations)) {
    n_retained <- diagnostics$summary$retained_simulations
    metrics_data <- rbind(metrics_data, data.frame(
      Metric = "N_retained",
      Description = "Number of simulations retained after removing non-finite and outliers",
      Target = "-",
      Value = format(n_retained, big.mark = ","),
      Status = "-",
      stringsAsFactors = FALSE
    ))
    metric_expressions[[length(metric_expressions) + 1]] <- expression(bold(N[retained]))
  }

  # Subset selection row: keyed off summary$percentile_used directly. It is
  # reported, not gated (calc_convergence_diagnostics() gates the same
  # information through B_size_upper), so it carries status "info".
  if (!is.null(diagnostics$summary$percentile_used)) {
    percentile_val <- as.numeric(diagnostics$summary$percentile_used)
    target_percentile <- if (!is.null(diagnostics$targets$percentile_max$value)) {
      as.numeric(diagnostics$targets$percentile_max$value)
    } else if (!is.null(diagnostics$targets$max_best_subset$value) &&
               !is.null(diagnostics$summary$total_simulations_original)) {
      as.numeric(diagnostics$targets$max_best_subset$value) /
        as.numeric(diagnostics$summary$total_simulations_original) * 100
    } else {
      NA_real_
    }
    metrics_data <- rbind(metrics_data, data.frame(
      Metric = "Subset Selection",
      Description = "Best subset as % of all draws (not gated)",
      Target = if (is.finite(target_percentile)) sprintf("<=%.1f%%", target_percentile) else "-",
      Value = sprintf("%.1f%%", percentile_val),
      Status = "info",
      stringsAsFactors = FALSE
    ))
    metric_expressions[[length(metric_expressions) + 1]] <- expression(bold("Subset Selection"))
  }

  # Gated best-subset metrics, in display order
  if (verbose) message("Processing metrics in specified order...")
  metric_order <- c("B_size", "B_size_upper", "ess_best", "A_B", "cvw_B")

  for (metric_name in metric_order) {
    if (!(metric_name %in% names(diagnostics$metrics))) {
      if (verbose) message("  Metric not found, skipping: ", metric_name)
      next
    }

    metric <- diagnostics$metrics[[metric_name]]
    if (verbose) message("  Processing metric: ", metric_name)

    if (is.null(metric) || (!is.list(metric) && !is.atomic(metric))) {
      if (verbose) message("    Skipping metric with invalid structure: ", metric_name)
      next
    }

    if (is.null(metric$value)) {
      if (verbose) message("    Metric missing value field, using metric as value: ", metric_name)
      metric <- list(value = metric, status = "info", description = metric_name)
    }

    target_value <- switch(metric_name,
      "ess_best"     = paste(">=", diagnostics$targets$ess_best$value),
      "A_B"          = paste(">=", diagnostics$targets$A_best$value),
      "cvw_B"        = paste("<=", diagnostics$targets$cvw_best$value),
      "B_size"       = paste(">=", diagnostics$targets$ess_best$value),
      "B_size_upper" = paste("<=", metric$target %||% diagnostics$targets$max_best_subset$value),
      "-"
    )

    formatted_value <- .mosaic_format_status_value(metric$value)

    display_name <- switch(metric_name,
      "ess_best" = "ESS_B",
      "A_B" = "A_B",
      "cvw_B" = "CV_B",
      "B_size" = "Best Subset (B)",
      "B_size_upper" = "Best Subset (B) cap",
      metric_name
    )

    display_expression <- switch(metric_name,
      "ess_best" = expression(bold(ESS[B])),
      "A_B" = expression(bold(A[B])),
      "cvw_B" = expression(bold(CV[B])),
      "B_size" = expression(bold("Best Subset (B)")),
      "B_size_upper" = expression(bold("Best Subset (B) cap")),
      NULL
    )

    better_description <- switch(metric_name,
      "ess_best" = "Effective sample size in best subset",
      "A_B" = "Agreement between simulations in best subset",
      "cvw_B" = "Variability of weights in best subset",
      "B_size" = "Number of simulations in best performing subset (lower bound)",
      "B_size_upper" = "Best subset size must not exceed max_best_subset (upper cap)",
      if (!is.null(metric$description)) metric$description else metric_name
    )

    if (is.null(target_value) || length(target_value) == 0) target_value <- "-"
    if (is.null(metric$status) || length(metric$status) == 0) metric$status <- "info"

    metrics_data <- rbind(metrics_data, data.frame(
      Metric = display_name,
      Description = better_description,
      Target = target_value,
      Value = formatted_value,
      Status = metric$status,
      stringsAsFactors = FALSE
    ))
    metric_expressions[[length(metric_expressions) + 1]] <- if (!is.null(display_expression)) {
      display_expression
    } else {
      as.expression(bquote(bold(.(display_name))))
    }
  }

  # Parameter ESS summary row
  param_ess_data <- NULL
  n_params <- NA
  n_pass <- NA
  target_ess_param <- NA

  if (!is.null(diagnostics$metrics$param_ess)) {
    param_metric <- diagnostics$metrics$param_ess
    target_ess_param <- diagnostics$targets$ess_param$value
    target_ess_param_prop <- diagnostics$targets$ess_param_prop$value

    pct_pass_display <- sprintf("%.0f%% (%d/%d >= %.0f)",
                               param_metric$value * 100,
                               param_metric$n_pass,
                               param_metric$n_total,
                               target_ess_param)
    param_ess_status <- param_metric$status

    param_ess_file <- file.path(results_dir, "parameter_ess.csv")
    if (file.exists(param_ess_file)) {
      param_ess_data <- utils::read.csv(param_ess_file, stringsAsFactors = FALSE)
      n_params <- param_metric$n_total
      n_pass <- param_metric$n_pass
    }

    metrics_data <- rbind(metrics_data, data.frame(
      Metric = "Parameter ESS",
      Description = "Proportion of parameters with adequate effective sample size",
      Target = sprintf(">=%.0f%%", target_ess_param_prop * 100),
      Value = pct_pass_display,
      Status = param_ess_status,
      stringsAsFactors = FALSE
    ))
    metric_expressions[[length(metric_expressions) + 1]] <- expression(bold("Parameter ESS"))

    if (verbose) message("Added Parameter ESS summary from diagnostics: ", pct_pass_display)

  } else {
    param_ess_file <- file.path(results_dir, "parameter_ess.csv")
    if (file.exists(param_ess_file)) {
      param_ess_data <- utils::read.csv(param_ess_file, stringsAsFactors = FALSE)

      target_ess_param <- if (!is.null(diagnostics$targets$ess_param$value)) {
        diagnostics$targets$ess_param$value
      } else {
        100
      }
      target_ess_param_prop <- if (!is.null(diagnostics$targets$ess_param_prop$value)) {
        diagnostics$targets$ess_param_prop$value
      } else {
        0.90
      }

      n_params <- nrow(param_ess_data)
      n_pass <- sum(param_ess_data$ess_marginal >= target_ess_param, na.rm = TRUE)
      pct_pass <- (n_pass / n_params) * 100

      param_ess_display <- sprintf("%.0f%% (%d/%d >= %.0f)",
                                  pct_pass, n_pass, n_params, target_ess_param)

      param_ess_status <- if (pct_pass/100 >= target_ess_param_prop) "pass"
                         else if (pct_pass/100 >= target_ess_param_prop * 0.8) "warn"
                         else "fail"

      metrics_data <- rbind(metrics_data, data.frame(
        Metric = "Parameter ESS",
        Description = "Proportion of parameters with adequate effective sample size",
        Target = sprintf(">=%.0f%%", target_ess_param_prop * 100),
        Value = param_ess_display,
        Status = param_ess_status,
        stringsAsFactors = FALSE
      ))
      metric_expressions[[length(metric_expressions) + 1]] <- expression(bold("Parameter ESS"))

      if (verbose) message("Added Parameter ESS summary from file (legacy): ", param_ess_display)
    } else {
      if (verbose) message("Parameter ESS not found in diagnostics or file")
    }
  }

  # Exact importance-sampling diagnostics: reported, never gated. The docs
  # require these to be read alongside ESS_B, so they belong in the table.
  is_all  <- diagnostics$importance_sampling$all_draws
  is_best <- diagnostics$importance_sampling$best_subset
  if (!is.null(is_all) || !is.null(is_best)) {
    .is_row <- function(metric, description, value, target, expr) {
      metrics_data <<- rbind(metrics_data, data.frame(
        Metric = metric, Description = description, Target = target,
        Value = value, Status = "info", stringsAsFactors = FALSE
      ))
      metric_expressions[[length(metric_expressions) + 1]] <<- expr
    }
    .ess_is_value <- function(d) {
      v <- .mosaic_format_status_value(d$ess_is)
      if (!is.null(d$n)) paste0(v, " of ", format(d$n, big.mark = ",")) else v
    }
    if (!is.null(is_all)) {
      .is_row("ESS_IS (all)", "Exact IS ESS, all draws (not gated)",
              .ess_is_value(is_all), "-", expression(bold(ESS[IS]~"(all)")))
    }
    if (!is.null(is_best)) {
      .is_row("ESS_IS (B)", "Exact IS ESS, best subset (not gated)",
              .ess_is_value(is_best), "-", expression(bold(ESS[IS]~"(B)")))
    }
    if (!is.null(is_all)) {
      # khat_status is the status of the tail fit ("ok", "insufficient
      # draws", ...), not a reliability verdict, so it is shown only when the
      # fit did not succeed; the reliability reading is khat against 0.7.
      khat_desc <- "Pareto k-hat, all draws (not gated)"
      fit_status <- is_all$khat_status
      if (length(fit_status) == 1L && !is.na(fit_status) &&
          !identical(as.character(fit_status), "ok")) {
        khat_desc <- paste0(khat_desc, "; fit: ", fit_status)
      } else {
        khat_num <- suppressWarnings(as.numeric(is_all$khat))
        if (length(khat_num) == 1L && is.finite(khat_num) && khat_num >= 0.7)
          khat_desc <- paste0(khat_desc, "; IS estimate unreliable")
      }
      .is_row("Pareto k-hat", khat_desc, .mosaic_format_status_value(is_all$khat),
              "< 0.7", expression(bold(hat(k))))
    }
  }

  # Overall verdict (the aggregate of the gated statuses)
  overall <- diagnostics$summary$convergence_status
  if (!is.null(overall) && length(overall) == 1L && nzchar(overall)) {
    metrics_data <- rbind(metrics_data, data.frame(
      Metric = "Overall",
      Description = "Worst of the gated metrics",
      Target = "PASS",
      Value = as.character(overall),
      Status = tolower(as.character(overall)),
      stringsAsFactors = FALSE
    ))
    metric_expressions[[length(metric_expressions) + 1]] <- expression(bold("Overall"))
  }

  if (nrow(metrics_data) == 0) {
    if (verbose) message("No metrics data to assemble.")
    return(invisible(NULL))
  }

  if (verbose) message("Successfully processed ", nrow(metrics_data), " metrics")

  # --- Companion CSV (unconditional when output_dir supplied) -----------------
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir))
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    csv_out <- data.frame(
      metric = metrics_data$Metric,
      description = metrics_data$Description,
      value = metrics_data$Value,
      target = metrics_data$Target,
      status = metrics_data$Status,
      stringsAsFactors = FALSE
    )
    csv_file <- file.path(output_dir, "convergence_status.csv")
    utils::write.csv(csv_out, csv_file, row.names = FALSE)
    if (verbose) message("Saved ", csv_file)
  }

  invisible(list(
    metrics_data       = metrics_data,
    metric_expressions = metric_expressions,
    diagnostics        = diagnostics,
    param_ess_data     = param_ess_data,
    n_params           = n_params,
    n_pass             = n_pass,
    target_ess_param   = target_ess_param
  ))
}


# Format a diagnostics value for the status table; JSON nulls/NA print as "NA".
# @keywords internal
.mosaic_format_status_value <- function(x) {
  if (is.null(x) || length(x) == 0L) return("NA")
  if (is.list(x)) x <- unlist(x)
  if (!is.numeric(x)) return(as.character(x[1]))
  x <- x[1]
  if (!is.finite(x)) return("NA")
  if (abs(x) >= 100) {
    format(round(x, 0), scientific = FALSE)
  } else if (abs(x) >= 1) {
    format(round(x, 2), scientific = FALSE)
  } else {
    format(round(x, 4), scientific = FALSE)
  }
}
