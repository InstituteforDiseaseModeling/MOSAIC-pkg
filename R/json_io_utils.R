#' Standardized JSON I/O utilities for MOSAIC model files
#'
#' These functions ensure consistent JSON formatting across the codebase,
#' particularly for priors and posteriors files.

#' Write priors or posteriors to JSON with consistent formatting
#'
#' This function standardizes JSON output across the MOSAIC package,
#' ensuring consistent formatting with auto_unbox = TRUE to avoid
#' array wrapping of scalar values.
#'
#' @param object List object containing priors or posteriors
#' @param path Output file path
#' @param type Either "priors" or "posteriors" for validation
#' @param validate Logical; whether to validate structure before writing (default TRUE)
#'
#' @return Invisibly returns the output path
#'
#' @examples
#' \dontrun{
#' # Write priors with standardized formatting
#' write_model_json(priors_default, "priors.json", type = "priors")
#'
#' # Write posteriors
#' write_model_json(posteriors, "posteriors.json", type = "posteriors")
#' }
#'
#' @export
write_model_json <- function(object, path, type = c("priors", "posteriors"), validate = TRUE) {
  type <- match.arg(type)

  # Basic structure validation
  if (validate) {
    if (type == "priors" && !all(c("metadata", "parameters_global") %in% names(object))) {
      stop("Invalid priors structure: missing required fields 'metadata' and/or 'parameters_global'")
    }
    if (type == "posteriors" && !"metadata" %in% names(object)) {
      stop("Invalid posteriors structure: missing required field 'metadata'")
    }
  }

  # Ensure directory exists
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }

  # Always use auto_unbox for consistent, clean JSON output
  jsonlite::write_json(
    object,
    path,
    pretty = TRUE,
    auto_unbox = TRUE,  # Critical: prevents array wrapping of scalars
    digits = 10,        # Preserve numerical precision
    na = "null"         # Handle NA consistently
  )

  invisible(path)
}

#' Read priors or posteriors from JSON with consistent parsing
#'
#' This function provides standardized reading of JSON files,
#' with options for simplification.
#'
#' @param path Input file path
#' @param simplify Whether to simplify to vectors (default FALSE for consistency)
#' @param validate Logical; whether to validate structure after reading (default TRUE)
#'
#' @return List object containing the parsed JSON
#'
#' @examples
#' \dontrun{
#' # Read priors
#' priors <- read_model_json("priors.json")
#'
#' # Read with simplification (not recommended for priors/posteriors)
#' data <- read_model_json("data.json", simplify = TRUE)
#' }
#'
#' @export
read_model_json <- function(path, simplify = FALSE, validate = TRUE) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  # Read with controlled simplification
  result <- jsonlite::read_json(
    path,
    simplifyVector = simplify,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )

  # Optional validation
  if (validate && !simplify) {
    # Check for common issues
    if (is.null(result)) {
      warning("JSON file appears to be empty: ", path)
    }
  }

  result
}

#' Validate priors JSON structure
#'
#' Checks that a priors object or file has the correct structure
#' and warns about potential issues like array wrapping.
#'
#' @param priors_object Priors object (list) or file path (character)
#' @param verbose Logical; whether to print validation messages (default TRUE)
#'
#' @return TRUE if valid (invisibly), otherwise stops with error
#'
#' @examples
#' \dontrun{
#' # Validate a priors file
#' validate_priors_json("priors.json")
#'
#' # Validate a priors object
#' validate_priors_json(priors_default)
#' }
#'
#' @export
validate_priors_json <- function(priors_object, verbose = TRUE) {
  # Load from file if needed
  if (is.character(priors_object)) {
    if (verbose) message("Reading priors from: ", priors_object)
    priors <- read_model_json(priors_object, validate = FALSE)
  } else {
    priors <- priors_object
  }

  # Check required top-level fields
  required_fields <- c("metadata", "parameters_global")
  missing_fields <- setdiff(required_fields, names(priors))
  if (length(missing_fields) > 0) {
    stop("Missing required fields: ", paste(missing_fields, collapse = ", "))
  }

  # Check for array wrapping (indicates incorrect format)
  issues_found <- FALSE
  check_unwrapped <- function(obj, path = "") {
    for (name in names(obj)) {
      val <- obj[[name]]
      new_path <- if (path == "") name else paste(path, name, sep = ".")

      # Check if value is unnecessarily wrapped in array
      if (is.list(val) && length(val) == 1 &&
          !is.list(val[[1]]) && !is.null(val[[1]])) {
        if (verbose) {
          warning("Possible array wrapping at: ", new_path,
                  "\n  Consider using write_model_json() or auto_unbox = TRUE",
                  call. = FALSE)
        }
        issues_found <<- TRUE
      }

      # Recurse for nested lists
      if (is.list(val) && length(names(val)) > 0) {
        check_unwrapped(val, new_path)
      }
    }
  }

  check_unwrapped(priors)

  # Validate distribution specifications
  validate_distributions <- function(params_list, prefix = "") {
    for (param_name in names(params_list)) {
      param <- params_list[[param_name]]

      if (!is.null(param$distribution)) {
        # Handle both array-wrapped and clean formats
        dist <- if (is.list(param$distribution)) param$distribution[[1]] else param$distribution

        # Check if distribution is valid
        valid_dists <- c("beta", "gamma", "normal", "lognormal",
                        "uniform", "gompertz", "truncnorm")
        if (!tolower(dist) %in% valid_dists) {
          stop(prefix, param_name, ": Invalid distribution '", dist, "'")
        }

        # Check parameters match distribution
        if (!is.null(param$parameters)) {
          # Map of distribution types to their required parameters
          # Note: lognormal can use either (mean, sd) OR (meanlog, sdlog)
          required_params_map <- list(
            beta = c("shape1", "shape2"),
            gamma = c("shape", "rate"),
            normal = c("mean", "sd"),
            lognormal = list(c("meanlog", "sdlog"), c("mean", "sd")),  # Either form
            uniform = c("min", "max"),
            gompertz = c("b", "eta"),
            truncnorm = c("mean", "sd", "a", "b")
          )

          required_params <- required_params_map[[tolower(dist)]]
          if (is.null(required_params)) required_params <- character()

          # Extract parameter names (handle array wrapping)
          param_names <- names(param$parameters)

          # Special handling for lognormal which can have two forms
          if (tolower(dist) == "lognormal" && is.list(required_params)) {
            # Check if either form is satisfied
            form1_ok <- all(required_params[[1]] %in% param_names)
            form2_ok <- all(required_params[[2]] %in% param_names)
            if (!form1_ok && !form2_ok) {
              stop(prefix, param_name, ": Lognormal distribution requires either ",
                   "(meanlog, sdlog) or (mean, sd) parameters")
            }
            missing <- character()  # No missing if one form is satisfied
          } else {
            # Standard check for other distributions
            if (is.list(required_params)) required_params <- required_params[[1]]
            missing <- setdiff(required_params, param_names)
          }
          if (length(missing) > 0) {
            stop(prefix, param_name, ": Missing parameters for ",
                 dist, " distribution: ", paste(missing, collapse = ", "))
          }
        }
      }

      # Recurse for location-specific parameters
      if (!is.null(param$location)) {
        for (loc in names(param$location)) {
          # Create a named list for recursion
          loc_param <- list()
          loc_param[[paste0(param_name, "_", loc)]] <- param$location[[loc]]
          validate_distributions(
            loc_param,
            paste0(prefix, "location.", loc, ".")
          )
        }
      }
    }
  }

  # Validate global parameters
  if (!is.null(priors$parameters_global)) {
    validate_distributions(priors$parameters_global, "parameters_global.")
  }

  # Validate location parameters
  if (!is.null(priors$parameters_location)) {
    validate_distributions(priors$parameters_location, "parameters_location.")
  }

  if (verbose) {
    if (issues_found) {
      message("⚠ Priors JSON has formatting issues but is structurally valid")
    } else {
      message("✓ Priors JSON structure is valid and well-formatted")
    }
  }

  invisible(TRUE)
}

#' Convert array-wrapped JSON to clean format
#'
#' Reads a JSON file that may have array-wrapped scalar values
#' and rewrites it with proper formatting (auto_unbox = TRUE).
#' This is useful for cleaning up JSON files created without auto_unbox.
#'
#' @param input_path Path to JSON file with potential array wrapping
#' @param output_path Path for cleaned JSON (defaults to overwrite input)
#' @param backup Logical; whether to create backup of original (default TRUE)
#'
#' @return Invisibly returns the output path
#'
#' @examples
#' \dontrun{
#' # Clean a priors file in place (creates .bak backup)
#' clean_priors_json("priors.json")
#'
#' # Clean to a new file without backup
#' clean_priors_json("old_priors.json", "clean_priors.json", backup = FALSE)
#' }
#'
#' @export
clean_priors_json <- function(input_path, output_path = input_path, backup = TRUE) {
  if (!file.exists(input_path)) {
    stop("Input file not found: ", input_path)
  }

  # Create backup if overwriting
  if (backup && input_path == output_path) {
    backup_path <- paste0(input_path, ".bak")
    file.copy(input_path, backup_path, overwrite = TRUE)
    message("Created backup: ", backup_path)
  }

  # Read with simplification to unwrap arrays
  priors <- jsonlite::fromJSON(input_path, simplifyVector = TRUE)

  # Write back with proper formatting
  write_model_json(priors, output_path,
                  type = ifelse(grepl("posterior", basename(input_path)),
                               "posteriors", "priors"),
                  validate = FALSE)

  message("✓ Cleaned JSON written to: ", output_path)

  # Validate the result
  tryCatch({
    validate_priors_json(output_path, verbose = FALSE)
  }, error = function(e) {
    warning("Cleaned file may have structural issues: ", e$message)
  })

  invisible(output_path)
}

#' Batch clean multiple JSON files
#'
#' Applies clean_priors_json to multiple files, useful for
#' cleaning up calibration directories.
#'
#' @param pattern File pattern to match (e.g., "**/priors.json")
#' @param path Base path to search in
#' @param dry_run Logical; if TRUE, only shows what would be cleaned (default FALSE)
#'
#' @return Character vector of cleaned file paths (invisibly)
#'
#' @examples
#' \dontrun{
#' # Dry run to see what would be cleaned
#' batch_clean_json("**/priors.json", "local/calibration", dry_run = TRUE)
#'
#' # Actually clean all priors.json files
#' batch_clean_json("**/priors.json", "local/calibration")
#' }
#'
#' @export
batch_clean_json <- function(pattern, path = ".", dry_run = FALSE) {
  # Find matching files
  files <- list.files(path, pattern = pattern,
                     recursive = TRUE, full.names = TRUE)

  if (length(files) == 0) {
    message("No files found matching pattern: ", pattern)
    return(invisible(character()))
  }

  message("Found ", length(files), " file(s) to clean")

  if (dry_run) {
    message("DRY RUN - would clean:")
    for (f in files) {
      message("  - ", f)
    }
    return(invisible(files))
  }

  # Clean each file
  cleaned <- character()
  for (f in files) {
    tryCatch({
      message("\nCleaning: ", f)
      clean_priors_json(f, backup = TRUE)
      cleaned <- c(cleaned, f)
    }, error = function(e) {
      warning("Failed to clean ", f, ": ", e$message)
    })
  }

  message("\n✓ Successfully cleaned ", length(cleaned), " file(s)")
  invisible(cleaned)
}