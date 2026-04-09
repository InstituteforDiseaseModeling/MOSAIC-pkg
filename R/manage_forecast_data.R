#' Check Forecast Data Freshness
#'
#' Check if ENSO and DMI forecast data needs updating by examining the 
#' expiration dates in the JSON configuration files.
#'
#' @param warn_threshold_days Number of days before expiration to start warning (default: 30)
#'
#' @return List with freshness status for both datasets
#'
#' @details This function reads the JSON configuration files and checks:
#'   - Whether forecast data has already expired
#'   - Whether data expires soon (within warning threshold)
#'   - Provides recommendations for updating
#'
#' @examples
#' \dontrun{
#' # Check current freshness status
#' freshness <- check_forecast_freshness()
#' print(freshness)
#' 
#' # Use shorter warning threshold
#' freshness <- check_forecast_freshness(warn_threshold_days = 14)
#' }
#'
#' @export
check_forecast_freshness <- function(warn_threshold_days = 30) {
  
  # Locate JSON files
  enso_file <- system.file("extdata", "enso_forecast_current.json", package = "MOSAIC")
  dmi_file <- system.file("extdata", "dmi_forecast_current.json", package = "MOSAIC")
  
  if (!file.exists(enso_file) || !file.exists(dmi_file)) {
    stop("Forecast configuration files not found. Expected files:\n",
         "  ", enso_file, "\n",
         "  ", dmi_file)
  }
  
  # Read JSON data
  enso_config <- jsonlite::fromJSON(enso_file)
  dmi_config <- jsonlite::fromJSON(dmi_file)
  
  current_date <- Sys.Date()
  
  # Parse expiration dates
  enso_expires <- as.Date(enso_config$expires_after)
  dmi_expires <- as.Date(dmi_config$expires_after)
  
  # Calculate days until expiration
  enso_days_remaining <- as.numeric(enso_expires - current_date)
  dmi_days_remaining <- as.numeric(dmi_expires - current_date)
  
  # Determine status for each dataset
  enso_status <- get_freshness_status(enso_days_remaining, warn_threshold_days)
  dmi_status <- get_freshness_status(dmi_days_remaining, warn_threshold_days)
  
  # Overall status
  overall_status <- "FRESH"
  if (enso_status$level == "EXPIRED" || dmi_status$level == "EXPIRED") {
    overall_status <- "EXPIRED"
  } else if (enso_status$level == "EXPIRING_SOON" || dmi_status$level == "EXPIRING_SOON") {
    overall_status <- "EXPIRING_SOON"
  }
  
  # Create result
  result <- list(
    overall_status = overall_status,
    enso = list(
      status = enso_status$level,
      message = enso_status$message,
      expires_date = enso_expires,
      days_remaining = enso_days_remaining,
      last_updated = enso_config$last_updated,
      needs_update = enso_status$needs_update
    ),
    dmi = list(
      status = dmi_status$level,
      message = dmi_status$message,
      expires_date = dmi_expires,
      days_remaining = dmi_days_remaining,
      last_updated = dmi_config$last_updated,
      needs_update = dmi_status$needs_update
    )
  )
  
  # Print summary
  cat("=== FORECAST DATA FRESHNESS CHECK ===\n")
  cat(sprintf("Overall Status: %s\n\n", overall_status))
  cat(sprintf("ENSO: %s - %s\n", enso_status$level, enso_status$message))
  cat(sprintf("DMI:  %s - %s\n\n", dmi_status$level, dmi_status$message))
  
  if (overall_status %in% c("EXPIRED", "EXPIRING_SOON")) {
    cat("ACTION REQUIRED:\n")
    cat("1. Visit: http://www.bom.gov.au/climate/ocean/outlooks/\n")
    cat("2. Update JSON files in inst/extdata/\n")
    cat("3. See: inst/extdata/UPDATE_FORECAST_INSTRUCTIONS.md\n")
  }
  
  # Issue warnings if needed
  if (enso_status$needs_update) {
    warning("ENSO forecast data needs updating: ", enso_status$message)
  }
  if (dmi_status$needs_update) {
    warning("DMI forecast data needs updating: ", dmi_status$message)
  }
  
  return(result)
}

#' Get Freshness Status Helper
#' @keywords internal
get_freshness_status <- function(days_remaining, warn_threshold) {
  
  if (days_remaining < 0) {
    list(
      level = "EXPIRED",
      message = sprintf("Expired %d days ago", abs(days_remaining)),
      needs_update = TRUE
    )
  } else if (days_remaining <= warn_threshold) {
    list(
      level = "EXPIRING_SOON", 
      message = sprintf("Expires in %d days", days_remaining),
      needs_update = TRUE
    )
  } else {
    list(
      level = "FRESH",
      message = sprintf("Fresh (%d days until expiration)", days_remaining),
      needs_update = FALSE
    )
  }
}

#' Get DMI Forecast from JSON Configuration
#'
#' Load DMI forecast data from the JSON configuration file instead of 
#' hardcoded values. This replaces the manual text parsing approach.
#'
#' @return A data frame with columns: year, month, month_name, variable, value
#'
#' @details This function reads forecast data from inst/extdata/dmi_forecast_current.json
#'   and converts it to the standard MOSAIC format. It automatically checks for
#'   data freshness and issues warnings if the data is stale.
#'
#' @examples
#' \dontrun{
#' # Get current DMI forecasts  
#' dmi_forecast <- get_DMI_forecast_from_json()
#' head(dmi_forecast)
#' }
#'
#' @export
get_DMI_forecast_from_json <- function() {
  
  # Check freshness first (but don't print full report again if already checked)
  freshness <- suppressMessages(check_forecast_freshness())
  
  # Read JSON configuration
  dmi_file <- system.file("extdata", "dmi_forecast_current.json", package = "MOSAIC")
  
  if (!file.exists(dmi_file)) {
    stop("DMI forecast configuration file not found: ", dmi_file)
  }
  
  message("NOTE: DMI forecast loaded from JSON configuration")
  message("Source: ", dmi_file)
  
  config <- jsonlite::fromJSON(dmi_file)
  
  # Convert JSON structure to data frame
  result_list <- list()
  
  for (variable in names(config$forecasts)) {
    
    forecasts <- config$forecasts[[variable]]
    date_strings <- names(forecasts)
    values <- as.numeric(forecasts)
    
    # Parse dates
    dates <- as.Date(paste0(date_strings, "-01"))
    years <- as.integer(format(dates, "%Y"))
    months <- as.integer(format(dates, "%m"))
    month_names <- month.abb[months]
    
    # Create data frame for this variable
    df <- data.frame(
      year = years,
      month = months,
      month_name = month_names, 
      variable = variable,
      value = values,
      stringsAsFactors = FALSE
    )
    
    result_list[[variable]] <- df
  }
  
  # Combine all variables (though DMI typically only has one)
  combined_df <- do.call(rbind, result_list)
  combined_df <- combined_df[order(combined_df$year, combined_df$month), ]
  
  return(combined_df)
}
