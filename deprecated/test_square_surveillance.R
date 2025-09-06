#' Test script for the truly square surveillance data processing function
#'
#' This script tests the truly square data structure implementation
#' Run this to validate that the function creates proper square data

test_square_surveillance <- function() {
    
    # Load required libraries
    library(MOSAIC)
    
    # Get paths
    PATHS <- get_paths()
    
    # Run the updated function
    cat("Testing truly square surveillance data processing...\n")
    
    # Test with JHU as preferred source
    tryCatch({
        process_cholera_surveillance_data(PATHS, keep_source = "JHU")
        cat("✓ Function executed successfully with JHU preference\n")
    }, error = function(e) {
        cat("✗ Error with JHU preference:", e$message, "\n")
        return(FALSE)
    })
    
    # Check output files
    weekly_file <- file.path(PATHS$DATA_CHOLERA_WEEKLY, "cholera_surveillance_weekly_combined.csv")
    
    if (file.exists(weekly_file)) {
        wk_data <- read.csv(weekly_file, stringsAsFactors = FALSE)
        cat("✓ Weekly output file created with", nrow(wk_data), "rows\n")
        
        # Check for all-NA rows
        na_rows <- sum(apply(wk_data, 1, function(x) all(is.na(x))))
        cat("✓ All-NA rows in weekly data:", na_rows, "(should be 0)\n")
        
        # Check data structure
        countries <- length(unique(wk_data$iso_code[!is.na(wk_data$iso_code)]))
        
        # Check if data is truly square (all countries × all weeks from min to max)
        min_date <- min(as.Date(wk_data$date_start), na.rm = TRUE)
        max_date <- max(as.Date(wk_data$date_start), na.rm = TRUE)
        expected_weeks <- length(seq(min_date, max_date, by = "week"))
        expected_square <- countries * expected_weeks
        
        reported <- sum(!is.na(wk_data$cases))
        missing <- sum(is.na(wk_data$cases))
        
        cat("✓ Countries:", countries, "\n")
        cat("✓ Date range:", as.character(min_date), "to", as.character(max_date), "\n")
        cat("✓ Expected weeks:", expected_weeks, "\n")
        cat("✓ Expected square structure:", expected_square, "(", countries, "countries ×", expected_weeks, "weeks )\n")
        cat("✓ Actual structure:", nrow(wk_data), "\n")
        cat("✓ Truly square data structure:", ifelse(nrow(wk_data) == expected_square, "YES", "NO"), "\n")
        
        if (nrow(wk_data) == expected_square) {
            cat("✓ SUCCESS: Data is truly square!\n")
        } else {
            cat("✗ WARNING: Data is not truly square\n")
        }
        
        cat("✓ Reported observations:", reported, sprintf("(%.1f%%)", 100 * reported / nrow(wk_data)), "\n")
        cat("✓ Missing observations:", missing, sprintf("(%.1f%%)", 100 * missing / nrow(wk_data)), "\n")
        
        # Check temporal continuity
        all_weeks_check <- seq(min_date, max_date, by = "week")
        unique_weeks_in_data <- unique(as.Date(wk_data$date_start))
        missing_weeks <- setdiff(all_weeks_check, unique_weeks_in_data)
        
        if (length(missing_weeks) == 0) {
            cat("✓ Temporal continuity: PERFECT (no missing weeks)\n")
        } else {
            cat("✗ Temporal continuity: GAPS (", length(missing_weeks), "missing weeks)\n")
        }
        
        # Check that every country has same number of weeks
        country_week_counts <- table(wk_data$iso_code)
        if (length(unique(country_week_counts)) == 1) {
            cat("✓ Country balance: PERFECT (all countries have", unique(country_week_counts), "weeks)\n")
        } else {
            cat("✗ Country balance: UNEVEN (countries have different week counts)\n")
        }
        
    } else {
        cat("✗ Weekly output file not found\n")
        return(FALSE)
    }
    
    cat("Square surveillance data test completed.\n")
    return(TRUE)
}

# Run the test if called interactively
if (interactive()) {
    test_square_surveillance()
}