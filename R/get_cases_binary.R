#' Create Binary Environmental Suitability Indicator from Case Data
#'
#' This function creates a binary indicator of environmental suitability for cholera transmission
#' based on case count data. It uses sophisticated temporal logic to identify not only outbreak
#' periods but also the environmental conditions leading up to outbreaks, which may be more
#' predictive of future transmission risk.
#'
#' @param data A data frame containing cholera surveillance data with columns:
#' \itemize{
#'   \item \strong{iso_code}: ISO country code
#'   \item \strong{cases}: Weekly cholera case counts
#'   \item \strong{date_start}: Start date of each week (for ordering)
#' }
#' @param cutoff Numeric threshold for case counts. Weeks with cases >= cutoff are considered
#'   outbreak periods. Default is 1 (any cases).
#'
#' @return The input data frame with an additional \code{cases_binary} column containing
#'   the binary environmental suitability indicator (0 = unsuitable, 1 = suitable).
#'
#' @details
#' The environmental suitability logic works as follows:
#' \enumerate{
#'   \item \strong{Primary identification}: Weeks with cases >= cutoff are marked as suitable (1)
#'   \item \strong{Lead-up period}: The week immediately before any outbreak week is marked as suitable
#'   \item \strong{Extended lead-up}: If two consecutive weeks have cases >= cutoff, then 2-3 weeks before are also marked as suitable
#' }
#'
#' This approach recognizes that environmental conditions favorable for cholera transmission
#' typically develop in the weeks preceding observable outbreaks. By marking these lead-up
#' periods as "suitable," the model can learn to predict environmental suitability before
#' cases are reported, which is more useful for early warning systems.
#'
#' \strong{Missing Data Handling}: Weeks with missing case data (NA) are preserved as NA
#' in the binary indicator and are never retroactively marked as suitable during the
#' lead-up period logic. This prevents false environmental suitability signals during
#' periods with poor surveillance coverage.
#'
#' The function processes each country separately to ensure that temporal relationships
#' are maintained within each location's outbreak history.
#'
#' @examples
#' \dontrun{
#' # Load cholera case data
#' data <- read.csv("cholera_weekly_data.csv")
#' 
#' # Create binary suitability indicator with default cutoff
#' data_with_binary <- get_cases_binary(data)
#' 
#' # Use higher cutoff for more conservative outbreak definition
#' data_with_binary <- get_cases_binary(data, cutoff = 10)
#' }
#'
#' @note
#' The temporal logic requires data to be ordered by date within each country. The function
#' will sort the data appropriately, but for best performance, input data should already
#' be ordered by iso_code and date_start.
#'
#' @seealso
#' \code{\link{process_suitability_data}} for the main data processing pipeline where this function is used.
#'
#' @export
get_cases_binary <- function(data, cutoff = 1) {
    
    # Input validation
    required_cols <- c("iso_code", "cases", "date_start")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
        stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }
    
    # Ensure data is ordered by country and date
    data <- data[order(data$iso_code, as.Date(data$date_start)), ]
    
    # Initialize cases_binary column
    data$cases_binary <- 0
    
    # Split the data by iso_code to process each country separately
    split_data <- split(data, data$iso_code)
    
    # Apply the temporal logic to each country subset
    split_data <- lapply(split_data, function(country_data) {
        
        # Preserve original NA status to prevent false marking of missing data periods
        original_na <- is.na(country_data$cases)
        
        # Step 1: Set cases_binary to 1 where cases >= cutoff (only for non-NA values)
        country_data$cases_binary <- ifelse(is.na(country_data$cases), 
                                          NA, 
                                          as.numeric(country_data$cases >= cutoff))
        
        # Step 2: Mark lead-up periods - one week before any outbreak
        # Only mark previous week as suitable if it has actual data (not originally NA)
        for (i in 2:nrow(country_data)) {
            if (!is.na(country_data$cases_binary[i]) && country_data$cases_binary[i] == 1) {
                # Only mark previous week if it was not originally missing data
                if (!original_na[i - 1]) {
                    country_data$cases_binary[i - 1] <- 1
                }
            }
        }
        
        # Step 3: Extended lead-up periods for sustained outbreaks
        # Only mark 2-3 weeks prior if they were not originally missing data
        # More conservative for high cutoffs to prevent excessive expansion
        for (i in 4:nrow(country_data)) {
            if (!is.na(country_data$cases_binary[i]) && !is.na(country_data$cases_binary[i - 1])) {
                if (country_data$cases_binary[i] == 1 && country_data$cases_binary[i - 1] == 1) {
                    # Check if both weeks had cases above cutoff (not just marked as suitable)
                    actual_outbreak_i <- !is.na(country_data$cases[i]) && country_data$cases[i] >= cutoff
                    actual_outbreak_i1 <- !is.na(country_data$cases[i-1]) && country_data$cases[i-1] >= cutoff
                    
                    # Only extend lead-up for true consecutive outbreaks (not lead-up periods)
                    if (actual_outbreak_i && actual_outbreak_i1) {
                        # Only mark weeks 2-3 prior if they were not originally missing data
                        if (!original_na[i - 2]) {
                            country_data$cases_binary[i - 2] <- 1
                        }
                        if (!original_na[i - 3]) {
                            country_data$cases_binary[i - 3] <- 1
                        }
                    }
                }
            }
        }
        
        return(country_data)
    })
    
    # Reassemble the data back into one data frame
    result <- do.call(rbind, split_data)
    
    # Reset row names
    row.names(result) <- NULL
    
    # Report summary statistics
    n_total <- nrow(result)
    n_suitable <- sum(result$cases_binary == 1, na.rm = TRUE)
    n_countries <- length(unique(result$iso_code))
    n_above_cutoff <- sum(result$cases >= cutoff, na.rm = TRUE)
    n_na_binary <- sum(is.na(result$cases_binary))
    
    message(sprintf("Environmental suitability indicator created:"))
    message(sprintf("  - Total observations: %d", n_total))
    message(sprintf("  - Weeks with cases >= %d: %d", cutoff, n_above_cutoff))
    message(sprintf("  - Suitable periods (including lead-up): %d (%.1f%%)", n_suitable, 100 * n_suitable / n_total))
    message(sprintf("  - Expansion factor: %.1fx (suitable/above cutoff)", n_suitable / max(1, n_above_cutoff)))
    message(sprintf("  - Countries processed: %d", n_countries))
    message(sprintf("  - Missing data periods: %d", n_na_binary))
    
    return(result)
}