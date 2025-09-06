#' Create Results Schema for Calibration Workflows
#'
#' Creates a standardized schema for managing results matrices in calibration workflows.
#' This eliminates common bugs related to column misalignment, manual index management,
#' and inconsistent parameter extraction.
#'
#' @param config_base A base configuration object used to determine parameter structure
#' @param include_metadata Logical. Whether to include simulation metadata columns (sim, iter, seed)
#'
#' @return A calibration_results_schema object containing:
#'   \itemize{
#'     \item \code{metadata_cols}: Names of metadata columns
#'     \item \code{param_cols}: Names of parameter columns
#'     \item \code{all_cols}: All column names in order
#'     \item \code{n_metadata}: Number of metadata columns
#'     \item \code{n_params}: Number of parameter columns
#'     \item \code{n_total}: Total number of columns
#'     \item \code{param_template}: Template parameter vector for validation
#'   }
#'
#' @details
#' The schema provides a single source of truth for results matrix structure,
#' eliminating manual column counting and index management. It ensures consistent
#' parameter extraction and provides validation for parameter insertion.
#'
#' Metadata columns are always placed first, followed by parameter columns.
#' The 'seed' parameter is automatically excluded from parameter columns if present
#' in the configuration to avoid duplication with metadata.
#'
#' @seealso \code{\link{create_results_matrix}}, \code{\link{insert_parameters}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' config_base <- get_location_config("ETH")
#' schema <- create_results_schema(config_base)
#' print(schema$all_cols)  # See all column names
#' }
create_results_schema <- function(config_base, include_metadata = TRUE) {
    
    # Get canonical parameter template
    param_template <- convert_config_to_matrix(config_base)
    
    # Define metadata columns (always first)
    metadata_cols <- if (include_metadata) {
        c("sim", "iter", "seed", "likelihood")
    } else {
        c("likelihood")
    }
    
    # Define parameter columns (exclude seed if present to avoid duplication)
    param_cols <- names(param_template)
    if ("seed" %in% param_cols) {
        param_cols <- param_cols[param_cols != "seed"]
        param_template <- param_template[param_cols]
    }
    
    # Create schema object
    schema <- list(
        metadata_cols = metadata_cols,
        param_cols = param_cols,
        all_cols = c(metadata_cols, param_cols),
        n_metadata = length(metadata_cols),
        n_params = length(param_cols),
        n_total = length(metadata_cols) + length(param_cols),
        param_template = param_template
    )
    
    class(schema) <- "calibration_results_schema"
    return(schema)
}


#' Create Results Matrix Using Schema
#'
#' Creates a pre-allocated results matrix with proper column structure
#' based on a calibration results schema.
#'
#' @param schema A calibration_results_schema object from \code{create_results_schema()}
#' @param n_rows Number of rows to pre-allocate
#'
#' @return A numeric matrix with proper column names and dimensions
#'
#' @export
#'
#' @examples
#' \dontrun{
#' schema <- create_results_schema(config_base)
#' results_matrix <- create_results_matrix(schema, n_rows = 1000)
#' }
create_results_matrix <- function(schema, n_rows = 1) {
    
    if (!inherits(schema, "calibration_results_schema")) {
        stop("schema must be a calibration_results_schema object")
    }
    
    mat <- matrix(NA_real_, nrow = n_rows, ncol = schema$n_total)
    colnames(mat) <- schema$all_cols
    return(mat)
}


#' Get Column Indices from Schema
#'
#' Helper function to get column indices for specific data types from a schema.
#' This eliminates manual column counting and hard-coded indices.
#'
#' @param schema A calibration_results_schema object
#' @param type Type of columns to retrieve: "metadata", "params", or "likelihood"
#'
#' @return Integer vector of column indices
#'
#' @export
#'
#' @examples
#' \dontrun{
#' schema <- create_results_schema(config_base)
#' param_indices <- get_col_indices(schema, "params")
#' likelihood_idx <- get_col_indices(schema, "likelihood")
#' }
get_col_indices <- function(schema, type = c("metadata", "params", "likelihood")) {
    
    if (!inherits(schema, "calibration_results_schema")) {
        stop("schema must be a calibration_results_schema object")
    }
    
    type <- match.arg(type)
    
    switch(type,
           metadata = 1:schema$n_metadata,
           params = (schema$n_metadata + 1):schema$n_total,
           likelihood = which(schema$all_cols == "likelihood")
    )
}


#' Insert Parameters into Results Row
#'
#' Safely inserts parameter values into a results row using schema validation.
#' This ensures parameter structure consistency and catches mismatches early.
#'
#' @param result_row Numeric vector representing a single results row
#' @param schema A calibration_results_schema object
#' @param params_config Configuration object containing parameters to extract
#'
#' @return Updated result_row with parameters inserted
#'
#' @export
#'
#' @examples
#' \dontrun{
#' schema <- create_results_schema(config_base)
#' result_row <- rep(NA_real_, schema$n_total)
#' params <- sample_parameters(PATHS, config_base, seed = 123)
#' result_row <- insert_parameters(result_row, schema, params)
#' }
insert_parameters <- function(result_row, schema, params_config) {
    
    if (!inherits(schema, "calibration_results_schema")) {
        stop("schema must be a calibration_results_schema object")
    }
    
    # Extract parameters using convert_config_to_matrix
    param_vec <- convert_config_to_matrix(params_config)
    
    # Filter to match schema parameter columns (removes seed, ensures consistent ordering)
    param_vec <- param_vec[schema$param_cols]
    
    # Validate length matches schema
    if (length(param_vec) != schema$n_params) {
        stop(sprintf("Parameter vector length (%d) doesn't match schema (%d)", 
                    length(param_vec), schema$n_params))
    }
    
    # Validate names match schema
    if (!identical(names(param_vec), schema$param_cols)) {
        missing_in_vec <- setdiff(schema$param_cols, names(param_vec))
        extra_in_vec <- setdiff(names(param_vec), schema$param_cols)
        error_msg <- "Parameter names don't match schema"
        if (length(missing_in_vec) > 0) {
            error_msg <- paste0(error_msg, "\n  Missing: ", paste(missing_in_vec, collapse = ", "))
        }
        if (length(extra_in_vec) > 0) {
            error_msg <- paste0(error_msg, "\n  Extra: ", paste(extra_in_vec, collapse = ", "))
        }
        stop(error_msg)
    }
    
    # Insert parameters into result row
    param_indices <- get_col_indices(schema, "params")
    result_row[param_indices] <- as.numeric(param_vec)
    
    return(result_row)
}


#' Insert Metadata into Results Row
#'
#' Safely inserts metadata values into a results row using schema structure.
#'
#' @param result_row Numeric vector representing a single results row
#' @param schema A calibration_results_schema object
#' @param sim_id Simulation ID (optional)
#' @param iter Iteration number (optional)
#' @param seed Random seed (optional)
#' @param likelihood Log-likelihood value (optional)
#'
#' @return Updated result_row with metadata inserted
#'
#' @export
#'
#' @examples
#' \dontrun{
#' schema <- create_results_schema(config_base)
#' result_row <- rep(NA_real_, schema$n_total)
#' result_row <- insert_metadata(result_row, schema, sim_id = 1, iter = 1, seed = 123)
#' }
insert_metadata <- function(result_row, schema, sim_id = NA, iter = NA, seed = NA, likelihood = NA) {
    
    if (!inherits(schema, "calibration_results_schema")) {
        stop("schema must be a calibration_results_schema object")
    }
    
    # Insert each metadata value if column exists in schema
    if ("sim" %in% schema$metadata_cols && !is.na(sim_id)) {
        result_row[which(schema$all_cols == "sim")] <- sim_id
    }
    if ("iter" %in% schema$metadata_cols && !is.na(iter)) {
        result_row[which(schema$all_cols == "iter")] <- iter
    }
    if ("seed" %in% schema$metadata_cols && !is.na(seed)) {
        result_row[which(schema$all_cols == "seed")] <- seed
    }
    if ("likelihood" %in% schema$metadata_cols && !is.na(likelihood)) {
        result_row[which(schema$all_cols == "likelihood")] <- likelihood
    }
    
    return(result_row)
}


#' Print Method for Results Schema
#'
#' @param x A calibration_results_schema object
#' @param ... Additional arguments (unused)
#'
#' @export
print.calibration_results_schema <- function(x, ...) {
    cat("Calibration Results Schema\n")
    cat("==========================\n")
    cat("Total columns:", x$n_total, "\n")
    cat("Metadata columns:", x$n_metadata, paste0("(", paste(x$metadata_cols, collapse = ", "), ")"), "\n")
    cat("Parameter columns:", x$n_params, "\n")
    cat("\nColumn structure:\n")
    for (i in seq_along(x$all_cols)) {
        type <- if (i <= x$n_metadata) "metadata" else "parameter"
        cat(sprintf("  %2d: %-20s [%s]\n", i, x$all_cols[i], type))
    }
    invisible(x)
}