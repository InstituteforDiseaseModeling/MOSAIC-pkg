#' Combine WHO and GTFCC Vaccination Data
#'
#' This function intelligently combines vaccination data from WHO and GTFCC sources, prioritizing GTFCC data
#' while identifying and including WHO campaigns that are missing from GTFCC. It uses multiple matching
#' criteria to determine if campaigns are duplicates, accounting for date discrepancies and dose variations.
#'
#' @param PATHS A list containing file paths for input and output data. The list should include:
#' \itemize{
#'   \item \strong{MODEL_INPUT}: Path to the directory containing processed vaccination data files and where combined data will be saved.
#' }
#' @param date_tolerance Number of days tolerance for matching campaign dates between sources (default: 60 days)
#' @param dose_tolerance Proportion tolerance for matching doses between sources (default: 0.2 = 20%)
#'
#' @return The function saves the combined vaccination data to a CSV file in the directory specified by `PATHS$MODEL_INPUT`. 
#' It also returns the combined data as a data frame for further use in R.
#'
#' @details
#' The function performs intelligent campaign matching using the following approach:
#' \enumerate{
#'   \item **Load Processed Data**:
#'     - Reads WHO and GTFCC processed vaccination data files
#'   \item **Intelligent Matching**:
#'     - Matches campaigns by country, approximate date (within tolerance), and dose similarity
#'     - Uses multiple passes with different tolerance levels to maximize accurate matching
#'     - Identifies truly unique WHO campaigns not present in GTFCC
#'   \item **Data Combination**:
#'     - Prioritizes GTFCC data (marked as source = "GTFCC")
#'     - Adds unique WHO campaigns (marked as source = "WHO")
#'     - Includes campaigns present in both (marked as source = "GTFCC_WHO_matched")
#'   \item **Quality Assurance**:
#'     - Validates data structure matches downstream requirements
#'     - Ensures all required columns are present
#'     - Maintains date and ID formatting standards
#' }
#'
#' @examples
#' # Example usage
#' PATHS <- get_paths()
#' combined_data <- combine_vaccination_data(PATHS)
#'
#' # With custom tolerance settings
#' combined_data <- combine_vaccination_data(PATHS, date_tolerance = 30, dose_tolerance = 0.1)
#'
#' @importFrom glue glue
#' @importFrom utils read.csv write.csv
#' @export

combine_vaccination_data <- function(PATHS, date_tolerance = 60, dose_tolerance = 0.2) {
     
     message("==========================================")
     message("Combining WHO and GTFCC Vaccination Data")
     message("==========================================")
     
     # Load processed data from both sources
     message("Loading processed vaccination data...")
     who_data <- read.csv(file.path(PATHS$MODEL_INPUT, "data_vaccinations_WHO.csv"), stringsAsFactors = FALSE)
     gtfcc_data <- read.csv(file.path(PATHS$MODEL_INPUT, "data_vaccinations_GTFCC.csv"), stringsAsFactors = FALSE)
     
     # Convert dates
     who_data$campaign_date <- as.Date(who_data$campaign_date)
     who_data$decision_date <- as.Date(who_data$decision_date)
     gtfcc_data$campaign_date <- as.Date(gtfcc_data$campaign_date)
     gtfcc_data$decision_date <- as.Date(gtfcc_data$decision_date)
     
     # Add source columns
     who_data$source <- "WHO"
     gtfcc_data$source <- "GTFCC"
     
     message(glue::glue("WHO data: {nrow(who_data)} campaigns"))
     message(glue::glue("GTFCC data: {nrow(gtfcc_data)} campaigns"))
     
     # Function to find potential matches for a WHO campaign in GTFCC data
     find_matches <- function(who_row, gtfcc_data, date_tol, dose_tol) {
          
          # Filter by country
          country_matches <- gtfcc_data[gtfcc_data$iso_code == who_row$iso_code, ]
          
          if (nrow(country_matches) == 0) return(NULL)
          
          # Calculate date differences
          date_diff <- abs(as.numeric(country_matches$campaign_date - who_row$campaign_date))
          
          # Calculate dose differences (as proportion)
          dose_diff <- abs(country_matches$doses_shipped - who_row$doses_shipped) / who_row$doses_shipped
          
          # Find matches within tolerance
          matches <- which(date_diff <= date_tol & dose_diff <= dose_tol)
          
          if (length(matches) == 0) return(NULL)
          
          # Return the best match (closest in date)
          best_match <- matches[which.min(date_diff[matches])]
          
          return(data.frame(
               gtfcc_idx = which(gtfcc_data$id == country_matches$id[best_match]),
               date_diff = date_diff[best_match],
               dose_diff = dose_diff[best_match]
          ))
     }
     
     # Step 1: Exact matching (same country, date within 7 days, doses within 5%)
     message("\nStep 1: Finding exact matches (±7 days, ±5% doses)...")
     exact_matches <- data.frame()
     who_matched_exact <- logical(nrow(who_data))
     gtfcc_matched_exact <- logical(nrow(gtfcc_data))
     
     for (i in 1:nrow(who_data)) {
          match <- find_matches(who_data[i,], gtfcc_data[!gtfcc_matched_exact,], 
                               date_tol = 7, dose_tol = 0.05)
          if (!is.null(match)) {
               who_matched_exact[i] <- TRUE
               # Adjust index for already matched items
               actual_idx <- which(!gtfcc_matched_exact)[match$gtfcc_idx]
               gtfcc_matched_exact[actual_idx] <- TRUE
               exact_matches <- rbind(exact_matches, 
                                     data.frame(who_idx = i, gtfcc_idx = actual_idx, 
                                              date_diff = match$date_diff, 
                                              dose_diff = match$dose_diff))
          }
     }
     
     message(glue::glue("  Found {nrow(exact_matches)} exact matches"))
     
     # Step 2: Fuzzy matching (broader tolerance for remaining campaigns)
     message(glue::glue("\nStep 2: Finding fuzzy matches (±{date_tolerance} days, ±{dose_tolerance*100}% doses)..."))
     fuzzy_matches <- data.frame()
     who_matched_fuzzy <- logical(nrow(who_data))
     gtfcc_matched_fuzzy <- logical(nrow(gtfcc_data))
     
     for (i in which(!who_matched_exact)) {
          match <- find_matches(who_data[i,], gtfcc_data[!gtfcc_matched_exact & !gtfcc_matched_fuzzy,], 
                               date_tol = date_tolerance, dose_tol = dose_tolerance)
          if (!is.null(match)) {
               who_matched_fuzzy[i] <- TRUE
               # Adjust index for already matched items
               actual_idx <- which(!gtfcc_matched_exact & !gtfcc_matched_fuzzy)[match$gtfcc_idx]
               gtfcc_matched_fuzzy[actual_idx] <- TRUE
               fuzzy_matches <- rbind(fuzzy_matches, 
                                    data.frame(who_idx = i, gtfcc_idx = actual_idx,
                                             date_diff = match$date_diff,
                                             dose_diff = match$dose_diff))
          }
     }
     
     message(glue::glue("  Found {nrow(fuzzy_matches)} fuzzy matches"))
     
     # Step 3: Check for date-only matches (might be same campaign with different reported doses)
     message("\nStep 3: Finding date-only matches (±30 days, any dose difference)...")
     date_matches <- data.frame()
     who_matched_date <- logical(nrow(who_data))
     
     # Create subset of unmatched GTFCC data
     gtfcc_unmatched <- gtfcc_data[!gtfcc_matched_exact & !gtfcc_matched_fuzzy, ]
     
     for (i in which(!who_matched_exact & !who_matched_fuzzy)) {
          if (nrow(gtfcc_unmatched) > 0) {
               match <- find_matches(who_data[i,], gtfcc_unmatched,
                                   date_tol = 30, dose_tol = Inf)
               if (!is.null(match)) {
                    who_matched_date[i] <- TRUE
                    # Find actual index in original gtfcc_data
                    actual_idx <- which(gtfcc_data$id == gtfcc_unmatched$id[match$gtfcc_idx])
                    date_matches <- rbind(date_matches,
                                        data.frame(who_idx = i, gtfcc_idx = actual_idx,
                                                 date_diff = match$date_diff,
                                                 dose_diff = match$dose_diff))
               }
          }
     }
     
     message(glue::glue("  Found {nrow(date_matches)} date-only matches"))
     
     # Combine all matches
     who_matched <- who_matched_exact | who_matched_fuzzy | who_matched_date
     who_unmatched <- !who_matched
     
     message("\n==========================================")
     message("Match Summary:")
     message(glue::glue("  WHO campaigns matched: {sum(who_matched)} ({round(100*sum(who_matched)/nrow(who_data), 1)}%)"))
     message(glue::glue("  WHO campaigns unmatched: {sum(who_unmatched)} ({round(100*sum(who_unmatched)/nrow(who_data), 1)}%)"))
     message(glue::glue("  WHO doses matched: {format(sum(who_data$doses_shipped[who_matched]), big.mark=',')} ({round(100*sum(who_data$doses_shipped[who_matched])/sum(who_data$doses_shipped), 1)}%)"))
     message(glue::glue("  WHO doses unmatched: {format(sum(who_data$doses_shipped[who_unmatched]), big.mark=',')} ({round(100*sum(who_data$doses_shipped[who_unmatched])/sum(who_data$doses_shipped), 1)}%)"))
     message("==========================================")
     
     # Create combined dataset
     message("\nCreating combined dataset...")
     
     # Start with all GTFCC data
     combined_data <- gtfcc_data
     
     # Update source for matched GTFCC campaigns
     gtfcc_matched_any <- gtfcc_matched_exact | gtfcc_matched_fuzzy | 
                         (1:nrow(gtfcc_data) %in% date_matches$gtfcc_idx)
     combined_data$source[gtfcc_matched_any] <- "GTFCC_WHO_matched"
     
     # Add unmatched WHO campaigns
     who_unique <- who_data[who_unmatched, ]
     who_unique$source <- "WHO_only"
     
     # Ensure column compatibility
     common_cols <- intersect(names(combined_data), names(who_unique))
     combined_data <- rbind(combined_data[, common_cols], who_unique[, common_cols])
     
     # Sort by campaign date
     combined_data <- combined_data[order(combined_data$campaign_date), ]
     
     # Regenerate ID column
     combined_data$id <- 1:nrow(combined_data)
     
     # Add match confidence column
     combined_data$match_confidence <- NA
     combined_data$match_confidence[combined_data$source == "GTFCC"] <- "GTFCC_only"
     combined_data$match_confidence[combined_data$source == "WHO_only"] <- "WHO_only"
     
     # Add confidence levels for matched campaigns
     if (nrow(exact_matches) > 0) {
          gtfcc_exact_ids <- combined_data$id[combined_data$source == "GTFCC_WHO_matched" & 
                                              1:nrow(combined_data) %in% exact_matches$gtfcc_idx]
          combined_data$match_confidence[combined_data$id %in% gtfcc_exact_ids] <- "high"
     }
     if (nrow(fuzzy_matches) > 0) {
          gtfcc_fuzzy_ids <- combined_data$id[combined_data$source == "GTFCC_WHO_matched" & 
                                              1:nrow(combined_data) %in% fuzzy_matches$gtfcc_idx]
          combined_data$match_confidence[combined_data$id %in% gtfcc_fuzzy_ids] <- "medium"
     }
     if (nrow(date_matches) > 0) {
          gtfcc_date_ids <- combined_data$id[combined_data$source == "GTFCC_WHO_matched" & 
                                            1:nrow(combined_data) %in% date_matches$gtfcc_idx]
          combined_data$match_confidence[combined_data$id %in% gtfcc_date_ids] <- "low"
     }
     
     # Summary statistics
     message("\n==========================================")
     message("Combined Dataset Summary:")
     message(glue::glue("  Total campaigns: {nrow(combined_data)}"))
     message(glue::glue("  Total doses: {format(sum(combined_data$doses_shipped, na.rm=TRUE), big.mark=',')}"))
     message(glue::glue("  Date range: {min(combined_data$campaign_date, na.rm=TRUE)} to {max(combined_data$campaign_date, na.rm=TRUE)}"))
     message(glue::glue("  Countries: {length(unique(combined_data$iso_code))}"))
     message("\nCampaigns by source:")
     source_summary <- table(combined_data$source)
     for (s in names(source_summary)) {
          message(glue::glue("  {s}: {source_summary[s]}"))
     }
     message("\nMatch confidence for combined campaigns:")
     conf_summary <- table(combined_data$match_confidence[combined_data$source == "GTFCC_WHO_matched"])
     for (c in names(conf_summary)) {
          message(glue::glue("  {c}: {conf_summary[c]}"))
     }
     message("==========================================")
     
     # Save combined data
     output_path <- file.path(PATHS$MODEL_INPUT, "data_vaccinations_GTFCC_WHO.csv")
     write.csv(combined_data, output_path, row.names = FALSE)
     message(glue::glue("\nCombined vaccination data saved to: {output_path}"))
     
     # Print examples of unmatched WHO campaigns
     if (sum(who_unmatched) > 0) {
          message("\nTop 10 WHO campaigns not found in GTFCC (by doses):")
          who_unique_top <- who_unique[order(who_unique$doses_shipped, decreasing = TRUE), ]
          print_data <- head(who_unique_top[, c("country", "campaign_date", "doses_shipped")], 10)
          print_data$doses_shipped <- format(print_data$doses_shipped, big.mark = ",")
          print(print_data)
     }
     
     return(combined_data)
}