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

  # Process metrics in specific order
  if (verbose) message("Processing metrics in specified order...")
  metric_order <- c("ess_retained", "B_size", "ess_best", "A_B", "cvw_B")

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

    target_value <- NA
    if (metric_name %in% c("ess_all", "ess_retained")) {
      target_value <- "-"
    } else if (metric_name == "ess_best") {
      target_value <- paste(">=", diagnostics$targets$ess_best$value)
    } else if (metric_name == "A_B") {
      target_value <- paste(">=", diagnostics$targets$A_best$value)
    } else if (metric_name == "cvw_B") {
      target_value <- paste("<=", diagnostics$targets$cvw_best$value)
    } else if (metric_name == "B_size") {
      target_value <- paste(">=", diagnostics$targets$ess_best$value)
    } else {
      target_value <- "-"
    }

    formatted_value <- if (is.numeric(metric$value)) {
      if (metric$value >= 100) {
        format(round(metric$value, 0), scientific = FALSE)
      } else if (metric$value >= 1) {
        format(round(metric$value, 2), scientific = FALSE)
      } else {
        format(round(metric$value, 4), scientific = FALSE)
      }
    } else {
      as.character(metric$value)
    }

    display_name <- switch(metric_name,
      "ess_all" = "ESS_retained",
      "ess_retained" = "ESS_retained",
      "ess_best" = "ESS_B",
      "A_B" = "A_B",
      "cvw_B" = "CV_B",
      "B_size" = "Best Subset (B)",
      metric_name
    )

    display_expression <- switch(metric_name,
      "ess_all" = expression(bold(ESS[retained])),
      "ess_retained" = expression(bold(ESS[retained])),
      "ess_best" = expression(bold(ESS[B])),
      "A_B" = expression(bold(A[B])),
      "cvw_B" = expression(bold(CV[B])),
      "B_size" = expression(bold("Best Subset (B)")),
      NULL
    )

    better_description <- switch(metric_name,
      "ess_all" = "Effective sample size across retained simulations",
      "ess_retained" = "Effective sample size across retained simulations",
      "ess_best" = "Effective sample size in best subset",
      "A_B" = "Agreement between simulations in best subset",
      "cvw_B" = "Variability of weights in best subset",
      "B_size" = "Number of simulations in best performing subset (lower bound)",
      if (!is.null(metric$description)) metric$description else metric_name
    )

    if (is.null(display_name) || length(display_name) == 0) display_name <- metric_name
    if (is.null(better_description) || length(better_description) == 0) better_description <- metric_name
    if (is.null(target_value) || length(target_value) == 0) target_value <- "-"
    if (is.null(formatted_value) || length(formatted_value) == 0) formatted_value <- "N/A"
    if (is.null(metric$status) || length(metric$status) == 0) metric$status <- "info"

    tryCatch({
      new_row <- data.frame(
        Metric = display_name,
        Description = better_description,
        Target = target_value,
        Value = formatted_value,
        Status = metric$status,
        stringsAsFactors = FALSE
      )
      metrics_data <- rbind(metrics_data, new_row)
      if (!is.null(display_expression)) {
        metric_expressions[[length(metric_expressions) + 1]] <- display_expression
      }
    }, error = function(e) {
      if (verbose) message("    Error creating row for metric: ", metric_name, " - ", e$message)
    })

    # Subset Selection row after ESS_retained
    if (metric_name == "ess_retained" && !is.null(diagnostics$summary$percentile_used)) {
      percentile_val <- diagnostics$summary$percentile_used

      target_percentile <- if (!is.null(diagnostics$targets$percentile_max$value)) {
        diagnostics$targets$percentile_max$value
      } else if (!is.null(diagnostics$targets$max_percentile$value)) {
        diagnostics$targets$max_percentile$value
      } else if (!is.null(diagnostics$targets$max_best_subset$value)) {
        n_retained <- diagnostics$summary$retained_simulations
        (diagnostics$targets$max_best_subset$value / n_retained) * 100
      } else {
        5.0
      }

      percentile_display <- sprintf("%.1f%%", percentile_val)

      percentile_status <- if (percentile_val <= target_percentile) {
        "pass"
      } else if (percentile_val <= target_percentile * 1.5) {
        "warn"
      } else {
        "fail"
      }

      metrics_data <- rbind(metrics_data, data.frame(
        Metric = "Subset Selection",
        Description = "Percentile of likelihood distribution used for best subset",
        Target = sprintf("<=%.1f%%", target_percentile),
        Value = percentile_display,
        Status = percentile_status,
        stringsAsFactors = FALSE
      ))
      metric_expressions[[length(metric_expressions) + 1]] <- expression(bold("Subset Selection"))
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
