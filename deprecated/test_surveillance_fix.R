#' Test script for the updated surveillance data processing function
#'
#' This script tests the fixes to process_cholera_surveillance_data.R
#' Run this to validate that the function works correctly

# Test the updated function
test_surveillance_fix <- function() {
    
    # Load required libraries
    library(MOSAIC)
    
    # Get paths
    PATHS <- get_paths()
    
    # Run the updated function
    cat("Testing updated process_cholera_surveillance_data function...\n")
    
    # Test with JHU as preferred source
    tryCatch({
        process_cholera_surveillance_data(PATHS, keep_source = "JHU")
        cat("✓ Function executed successfully with JHU preference\n")
    }, error = function(e) {
        cat("✗ Error with JHU preference:", e$message, "\n")
    })
    
    # Check output files
    weekly_file <- file.path(PATHS$DATA_CHOLERA_WEEKLY, "cholera_surveillance_weekly_combined.csv")
    daily_file <- file.path(PATHS$DATA_CHOLERA_DAILY, "cholera_surveillance_daily_combined.csv")
    
    if (file.exists(weekly_file)) {
        wk_data <- read.csv(weekly_file, stringsAsFactors = FALSE)
        cat("✓ Weekly output file created with", nrow(wk_data), "rows\n")
        
        # Check for all-NA rows
        na_rows <- sum(apply(wk_data, 1, function(x) all(is.na(x))))
        cat("✓ All-NA rows in weekly data:", na_rows, "(should be 0)\n")
        
        # Check data structure
        countries <- length(unique(wk_data$iso_code[!is.na(wk_data$iso_code)]))
        weeks <- length(unique(paste(wk_data$year, wk_data$week)[!is.na(wk_data$year) & !is.na(wk_data$week)]))
        reported <- sum(!is.na(wk_data$cases))
        missing <- sum(is.na(wk_data$cases))
        
        cat("✓ Countries:", countries, "\n")
        cat("✓ Time periods:", weeks, "\n")  
        cat("✓ Reported observations:", reported, "\n")
        cat("✓ Missing observations:", missing, "\n")
        cat("✓ Total observations:", nrow(wk_data), "\n")
        
        # Check if data is square
        expected_square <- countries * weeks
        cat("✓ Expected square structure:", expected_square, "\n")
        cat("✓ Actual structure:", nrow(wk_data), "\n")
        cat("✓ Square data structure:", ifelse(nrow(wk_data) == expected_square, "YES", "NO"), "\n")
        
    } else {
        cat("✗ Weekly output file not found\n")
    }
    
    if (file.exists(daily_file)) {
        daily_data <- read.csv(daily_file, stringsAsFactors = FALSE)
        cat("✓ Daily output file created with", nrow(daily_data), "rows\n")
    } else {
        cat("✗ Daily output file not found\n")
    }
    
    cat("Test completed.\n")
}

# Run the test
if (interactive()) {
    test_surveillance_fix()
}