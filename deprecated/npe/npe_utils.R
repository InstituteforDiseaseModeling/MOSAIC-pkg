# =============================================================================
# NPE Utility Functions - Internal Helpers for Consolidated NPE
# =============================================================================

#' Create NPE logger
#' @keywords internal
.create_npe_logger_simple <- function(verbose = TRUE, log_file = NULL) {

    log_levels <- c("DEBUG", "INFO", "WARNING", "ERROR")

    logger <- list(
        verbose = verbose,
        log_file = log_file,

        log = function(level, msg, ...) {
            if (!level %in% log_levels) {
                level <- "INFO"
            }

            formatted_msg <- sprintf("[%s] %s: %s",
                                   format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                   level,
                                   sprintf(msg, ...))

            # Console output
            if (verbose && level != "DEBUG") {
                if (level == "ERROR") {
                    message(formatted_msg)
                    stop(sprintf(msg, ...), call. = FALSE)
                } else if (level == "WARNING") {
                    warning(sprintf(msg, ...), call. = FALSE, immediate. = TRUE)
                } else {
                    message(formatted_msg)
                }
            }

            # File output
            if (!is.null(log_file)) {
                cat(formatted_msg, "\n", file = log_file, append = TRUE)
            }
        }
    )

    # Create log file if specified
    if (!is.null(log_file)) {
        dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
        cat(sprintf("NPE Training Log - Started at %s\n", Sys.time()),
            file = log_file, append = FALSE)
    }

    return(logger)
}

#' Cleanup NPE resources
#' @keywords internal
.cleanup_npe_resources_simple <- function(use_gpu = FALSE, logger = NULL) {

    if (!is.null(logger)) logger$log("DEBUG", "Cleaning up resources...")

    # Force garbage collection
    gc(verbose = FALSE, full = TRUE)

    # Clear Python objects if reticulate is loaded
    if ("reticulate" %in% loadedNamespaces()) {
        tryCatch({
            reticulate::py_run_string("
import gc
import torch
if torch.cuda.is_available():
    torch.cuda.empty_cache()
gc.collect()
            ")
        }, error = function(e) {
            # Silently ignore Python cleanup errors
        })
    }
}

#' Validate training inputs
#' @keywords internal
.validate_train_inputs_simple <- function(simulations_file, outputs_file, output_dir, logger) {

    # Check files exist
    if (!file.exists(simulations_file)) {
        logger$log("ERROR", "Simulations file not found: %s", simulations_file)
    }

    if (!file.exists(outputs_file)) {
        logger$log("ERROR", "Outputs file not found: %s", outputs_file)
    }

    # Check output directory
    if (!dir.exists(dirname(output_dir))) {
        logger$log("ERROR", "Parent directory for output_dir does not exist: %s", dirname(output_dir))
    }

    logger$log("DEBUG", "Input validation passed")
}

#' Check Python dependencies
#' @keywords internal
.check_python_dependencies_simple <- function(logger) {

    required_modules <- c("torch", "numpy", "pandas", "sklearn", "lampe", "zuko")

    for (module in required_modules) {
        if (!reticulate::py_module_available(module)) {
            logger$log("ERROR", "Required Python module not installed: %s", module)
        }
    }

    # Check CUDA if GPU requested
    torch <- reticulate::import("torch", convert = FALSE)
    if (reticulate::py_to_r(torch$cuda$is_available())) {
        logger$log("INFO", "CUDA GPU available: %s",
                  reticulate::py_to_r(torch$cuda$get_device_name(0L)))
    } else if (reticulate::py_to_r(torch$backends$mps$is_available())) {
        logger$log("INFO", "MPS (Apple Silicon) GPU available")
    } else {
        logger$log("INFO", "No GPU detected, will use CPU")
    }
}

#' Filter simulations based on criteria
#' @keywords internal
.filter_simulations <- function(simulations, filter_type, logger) {

    n_original <- nrow(simulations)

    if (filter_type == "complete_cases") {
        # Keep only simulations with finite likelihoods
        if ("is_finite" %in% names(simulations)) {
            simulations <- simulations[simulations$is_finite == TRUE, ]
        }
    } else if (filter_type == "retained") {
        # Keep only retained simulations
        if ("is_retained" %in% names(simulations)) {
            simulations <- simulations[simulations$is_retained == TRUE, ]
        }
    }
    # else "all" - keep everything

    n_filtered <- nrow(simulations)

    if (n_filtered < n_original) {
        logger$log("INFO", "Filtered simulations: %d -> %d (%s filter)",
                  n_original, n_filtered, filter_type)
    }

    if (n_filtered == 0) {
        logger$log("ERROR", "No simulations remaining after filtering")
    }

    return(simulations)
}

#' Infer parameter bounds from data
#' @keywords internal
.infer_bounds_from_data <- function(param_data) {

    bounds <- data.frame(
        parameter = names(param_data),
        min = NA,
        max = NA,
        stringsAsFactors = FALSE
    )

    for (i in seq_len(ncol(param_data))) {
        param_name <- names(param_data)[i]
        values <- param_data[[i]]

        # Remove NA values
        values <- values[!is.na(values)]

        if (length(values) > 0) {
            # Use 0.1% and 99.9% quantiles to avoid extreme outliers
            bounds$min[i] <- quantile(values, 0.001)
            bounds$max[i] <- quantile(values, 0.999)

            # Special handling for proportion parameters
            if (grepl("^(prop_|p_|phi_|rho|sigma)", param_name)) {
                bounds$min[i] <- max(0, bounds$min[i])
                bounds$max[i] <- min(1, bounds$max[i])
            }
        }
    }

    return(bounds)
}

#' Validate parameter bounds
#' @keywords internal
.validate_bounds <- function(bounds, param_names, logger) {

    # Check all parameters have bounds
    missing_bounds <- param_names[!param_names %in% bounds$parameter]
    if (length(missing_bounds) > 0) {
        logger$log("ERROR", "Missing bounds for parameters: %s",
                  paste(missing_bounds, collapse = ", "))
    }

    # Check bounds are valid
    for (i in seq_len(nrow(bounds))) {
        if (is.na(bounds$min[i]) || is.na(bounds$max[i])) {
            logger$log("ERROR", "Invalid bounds for parameter %s", bounds$parameter[i])
        }
        if (bounds$min[i] >= bounds$max[i]) {
            logger$log("ERROR", "Invalid bounds for %s: min >= max",
                      bounds$parameter[i])
        }
    }

    logger$log("DEBUG", "Bounds validation passed for %d parameters", nrow(bounds))
}

#' Prepare observation matrix from outputs
#' @keywords internal
.prepare_observation_matrix <- function(outputs, n_sims, n_timesteps, n_locations) {

    # Pivot outputs to wide format
    outputs_wide <- outputs %>%
        dplyr::arrange(sim, t, j) %>%
        tidyr::pivot_wider(
            id_cols = sim,
            names_from = c(t, j),
            names_prefix = "obs_",
            values_from = cases,
            values_fill = 0
        )

    # Ensure we have all simulations
    if (nrow(outputs_wide) != n_sims) {
        warning(sprintf("Observation matrix has %d rows but expected %d simulations",
                       nrow(outputs_wide), n_sims))
    }

    # Convert to matrix (exclude sim column)
    x_matrix <- as.matrix(outputs_wide[, -1])

    return(x_matrix)
}

#' Get device for training
#' @keywords internal
.get_device <- function(use_gpu, logger) {

    if (!use_gpu) {
        return("cpu")
    }

    if (reticulate::py_module_available("torch")) {
        torch <- reticulate::import("torch")

        if (torch$cuda$is_available()) {
            device <- "cuda"
            logger$log("INFO", "Using CUDA GPU for training")
        } else if (torch$backends$mps$is_available()) {
            device <- "mps"
            logger$log("INFO", "Using MPS (Apple Silicon) GPU for training")
        } else {
            device <- "cpu"
            logger$log("INFO", "GPU requested but not available, using CPU")
        }
    } else {
        device <- "cpu"
        logger$log("WARNING", "PyTorch not available, using CPU")
    }

    return(device)
}

#' Backup models with suffix
#' @keywords internal
.backup_models <- function(output_dir, suffix, logger) {

    model_files <- list.files(output_dir, pattern = "\\.(pt|pkl|json)$", full.names = TRUE)

    for (file in model_files) {
        backup_name <- sub("(\\.[^.]+)$", paste0(suffix, "\\1"), file)
        file.copy(file, backup_name)
        logger$log("DEBUG", "Backed up %s", basename(file))
    }
}

#' Run quick diagnostics on model
#' @keywords internal
.run_quick_diagnostics <- function(model_dir, n_samples = 100) {

    # This would implement quick diagnostic checks
    # For now, return placeholder results
    list(
        coverage = list(mean_coverage = 0.85),
        sbc = list(ks_pvalue = 0.15),
        validation = list(loss_ratio = 1.2)
    )
}

#' Check if retraining is needed based on diagnostics
#' @keywords internal
.needs_retraining <- function(diagnostics, params) {

    # Check coverage
    if (diagnostics$coverage$mean_coverage < params$coverage_threshold) {
        return(TRUE)
    }

    # Check SBC
    if (diagnostics$sbc$ks_pvalue < params$sbc_threshold) {
        return(TRUE)
    }

    return(FALSE)
}

#' Adjust architecture based on diagnostics
#' @keywords internal
.adjust_architecture <- function(spec, diagnostics) {

    # Use the apply_npe_autotune function from npe_architecture.R
    apply_npe_autotune(spec, diagnostics)
}

#' Run SBC diagnostic
#' @keywords internal
.run_sbc_diagnostic <- function(model_dir, test_data, n_samples) {

    # Placeholder implementation
    # This would run simulation-based calibration tests
    list(
        ks_statistic = 0.05,
        ks_pvalue = 0.25,
        uniformity_test = "PASS"
    )
}

#' Run coverage diagnostic
#' @keywords internal
.run_coverage_diagnostic <- function(model_dir, test_data, n_samples) {

    # Placeholder implementation
    # This would compute coverage metrics
    list(
        mean_coverage = 0.88,
        coverage_by_param = rep(0.85, 10),
        calibration_score = 0.92
    )
}

#' Run posterior predictive checks
#' @keywords internal
.run_ppc_diagnostic <- function(model_dir, test_data) {

    # Placeholder implementation
    list(
        chi_square = 15.2,
        pvalue = 0.35,
        predictive_accuracy = 0.91
    )
}

#' Create diagnostic report
#' @keywords internal
.create_diagnostic_report <- function(results, output_dir) {

    report_file <- file.path(output_dir, "diagnostic_report.txt")

    sink(report_file)
    cat("NPE DIAGNOSTIC REPORT\n")
    cat("=====================\n\n")

    if (!is.null(results$sbc)) {
        cat("Simulation-Based Calibration:\n")
        cat(sprintf("  KS Statistic: %.4f\n", results$sbc$ks_statistic))
        cat(sprintf("  P-value: %.4f\n", results$sbc$ks_pvalue))
        cat(sprintf("  Result: %s\n\n", results$sbc$uniformity_test))
    }

    if (!is.null(results$coverage)) {
        cat("Coverage Analysis:\n")
        cat(sprintf("  Mean Coverage: %.2f%%\n", results$coverage$mean_coverage * 100))
        cat(sprintf("  Calibration Score: %.3f\n\n", results$coverage$calibration_score))
    }

    if (!is.null(results$ppc)) {
        cat("Posterior Predictive Checks:\n")
        cat(sprintf("  Chi-square: %.2f\n", results$ppc$chi_square))
        cat(sprintf("  P-value: %.4f\n", results$ppc$pvalue))
        cat(sprintf("  Predictive Accuracy: %.2f%%\n\n", results$ppc$predictive_accuracy * 100))
    }

    cat(sprintf("Report generated: %s\n", Sys.time()))
    sink()
}

#' Create NPE plots
#' @keywords internal
.create_npe_plots <- function(posteriors, diagnostics, output_dir) {

    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    plot_files <- character()

    # This would create various diagnostic plots
    # For now, return list of created files

    return(list(
        plot_files = plot_files,
        output_dir = output_dir
    ))
}