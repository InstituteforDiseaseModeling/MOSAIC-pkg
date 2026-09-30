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
#'     - Adds unique WHO campaigns (marked as source = "WHO_only")
#'     - Includes campaigns present in both (marked as source = "GTFCC_WHO_matched")
#'     - \code{match_confidence} is "high" (Step 1: +/-7 days, +/-5% doses), "medium"
#'       (Step 2: within the tolerances), "low" (Step 3: +/-30 days, any dose, or
#'       Step 4 only), or "GTFCC_only" / "WHO_only"
#'     - In Steps 1-3 each GTFCC campaign absorbs at most one WHO campaign, so a
#'       genuinely separate second campaign within 30 days is kept
#'     - When GTFCC records a campaign under the WHO row's ICG request number,
#'       only that request's campaign(s) are candidates in every step
#'     - Step 4 (same ICG request): GTFCC rows are per-request totals (all
#'       deliveries summed) while WHO rows are per shipment, so a still-unmatched
#'       WHO shipment is absorbed by the GTFCC campaign with the same ICG request
#'       number (WHO \code{20174} = GTFCC \code{201704}) as long as the WHO doses
#'       assigned to that campaign stay within \code{(1 + dose_tolerance)} of its
#'       GTFCC doses (e.g. MOZ 2017-I04: 709.1K GTFCC vs 329.6K + 354.6K WHO);
#'       a WHO row whose request number GTFCC does not use is matched on the ICG
#'       decision date (+/-7 days) within the dose tolerance instead
#'     - Step 5 (repeated WHO-only rows): WHO-only rows sharing country, ICG
#'       request and decision date are kept in campaign-date order only while
#'       their summed doses stay within \code{(1 + dose_tolerance)} of the
#'       request's approved total; further rows are dropped as duplicate listings
#'       (MWI 20182: two 500,600-dose rows against 500,600 approved)
#'   \item **Quality Assurance**:
#'     - Validates data structure matches downstream requirements
#'     - Ensures all required columns are present
#'     - Maintains date and ID formatting standards
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage (requires set_root_directory() + the MOSAIC-data repo)
#' PATHS <- get_paths()
#' combined_data <- combine_vaccination_data(PATHS)
#'
#' # With custom tolerance settings
#' combined_data <- combine_vaccination_data(PATHS, date_tolerance = 30, dose_tolerance = 0.1)
#' }
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
     
     # ICG request keys (WHO 20174 and GTFCC 201704 are both request 4 of 2017).
     # A request number is the strongest identity the two sources share, so when
     # GTFCC has a campaign with the WHO row's request number, only that request's
     # campaign(s) are candidates: date/dose proximity to a different request must
     # not override it (the 2nd round of COD request 2019-05, shipped 2019-10-30,
     # otherwise matches COD request 2019-14 delivered 15 days later).
     who_data$request_key <- .vacc_request_key(who_data$request_number)
     gtfcc_data$request_key <- .vacc_request_key(gtfcc_data$request_number)
     gtfcc_iso_key <- paste(gtfcc_data$iso_code, gtfcc_data$request_key)[!is.na(gtfcc_data$request_key)]

     # Function to find potential matches for a WHO campaign in GTFCC data
     find_matches <- function(who_row, gtfcc_data, date_tol, dose_tol) {
          
          # Filter by country
          country_matches <- gtfcc_data[gtfcc_data$iso_code == who_row$iso_code, ]

          # Restrict to the same ICG request when GTFCC records that request at all
          if (!is.na(who_row$request_key) &&
              paste(who_row$iso_code, who_row$request_key) %in% gtfcc_iso_key) {
               country_matches <- country_matches[!is.na(country_matches$request_key) &
                                                       country_matches$request_key == who_row$request_key, ]
          }
          
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
     message("\nStep 1: Finding exact matches (\u00B17 days, \u00B15% doses)...")
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
     message(glue::glue("\nStep 2: Finding fuzzy matches (\u00B1{date_tolerance} days, \u00B1{dose_tolerance*100}% doses)..."))
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
     message("\nStep 3: Finding date-only matches (\u00B130 days, any dose difference)...")
     date_matches <- data.frame()
     who_matched_date <- logical(nrow(who_data))
     gtfcc_matched_date <- logical(nrow(gtfcc_data))
     
     for (i in which(!who_matched_exact & !who_matched_fuzzy)) {
          # A GTFCC campaign can absorb at most one WHO campaign: recompute the
          # unmatched pool each time so a consumed campaign is not matched again
          # (otherwise a second WHO round would be dropped as a duplicate).
          open_idx <- which(!gtfcc_matched_exact & !gtfcc_matched_fuzzy & !gtfcc_matched_date)
          if (length(open_idx) > 0) {
               match <- find_matches(who_data[i,], gtfcc_data[open_idx, ],
                                   date_tol = 30, dose_tol = Inf)
               if (!is.null(match)) {
                    who_matched_date[i] <- TRUE
                    actual_idx <- open_idx[match$gtfcc_idx]
                    gtfcc_matched_date[actual_idx] <- TRUE
                    date_matches <- rbind(date_matches,
                                        data.frame(who_idx = i, gtfcc_idx = actual_idx,
                                                 date_diff = match$date_diff,
                                                 dose_diff = match$dose_diff))
               }
          }
     }
     
     message(glue::glue("  Found {nrow(date_matches)} date-only matches"))

     # Step 4: remaining shipments of an ICG request GTFCC records as one campaign.
     # process_GTFCC_vaccination_data() sums every delivery of a request into one
     # row, whereas the WHO ICG table lists each shipment separately (the raw
     # WHO file repeats the request number, with NA dates on continuation rows).
     # Steps 1-3 let a GTFCC campaign absorb one WHO row, so the second shipment
     # of a two-shipment request would otherwise be added again as WHO_only
     # (MOZ 2017-I04 and CMR 2019-I03-D01 in the ees-cholera-mapping GTFCC
     # requests file: 354.6K and 616.6K doses double-counted). A shipment is
     # absorbed only while the WHO doses assigned to that GTFCC campaign stay
     # within (1 + dose_tolerance) of its GTFCC total, so a genuinely extra
     # campaign under the same request is still kept. Rows GTFCC numbers
     # differently fall back to the decision date (see below).
     message("\nStep 4: Absorbing further shipments of the same ICG request...")
     who_matched_request <- logical(nrow(who_data))
     gtfcc_matched_request <- logical(nrow(gtfcc_data))
     who_key <- who_data$request_key
     gtfcc_key <- gtfcc_data$request_key
     assigned_gtfcc <- rep(NA_integer_, nrow(who_data))
     for (tbl in list(exact_matches, fuzzy_matches, date_matches)) {
          if (nrow(tbl) > 0) assigned_gtfcc[tbl$who_idx] <- tbl$gtfcc_idx
     }
     n_request <- 0L
     for (i in which(!who_matched_exact & !who_matched_fuzzy & !who_matched_date)) {
          same_iso <- gtfcc_data$iso_code == who_data$iso_code[i]
          cand <- if (is.na(who_key[i])) integer(0) else
               which(same_iso & !is.na(gtfcc_key) & gtfcc_key == who_key[i])
          if (length(cand) == 0) {
               # The sources occasionally number one request differently (MOZ:
               # WHO 20203 vs GTFCC 2020-I02, both decided 2020-03-12 with 733.5K
               # doses shipped). Fall back to the ICG decision date (+/-7 days)
               # plus the dose tolerance.
               dec_diff <- abs(as.numeric(gtfcc_data$decision_date - who_data$decision_date[i]))
               dose_rel <- abs(gtfcc_data$doses_shipped - who_data$doses_shipped[i]) /
                    who_data$doses_shipped[i]
               cand <- which(same_iso & !is.na(dec_diff) & dec_diff <= 7 &
                                  !is.na(dose_rel) & dose_rel <= dose_tolerance)
          }
          if (length(cand) == 0) next
          for (j in cand) {
               assigned_doses <- sum(who_data$doses_shipped[which(assigned_gtfcc == j)], na.rm = TRUE)
               if (assigned_doses + who_data$doses_shipped[i] <=
                   (1 + dose_tolerance) * gtfcc_data$doses_shipped[j]) {
                    who_matched_request[i] <- TRUE
                    gtfcc_matched_request[j] <- TRUE
                    assigned_gtfcc[i] <- j
                    n_request <- n_request + 1L
                    break
               }
          }
     }
     message(glue::glue("  Found {n_request} same-request shipment matches"))

     # Combine all matches
     who_matched <- who_matched_exact | who_matched_fuzzy | who_matched_date | who_matched_request
     who_unmatched <- !who_matched

     # Step 5: repeated WHO-only rows of one request. When GTFCC has no row for
     # a request, its WHO shipments cannot be absorbed by Step 4, and the WHO
     # table sometimes lists the same shipment twice (MWI 20182: two 500,600-dose
     # rows, decided 2018-03-02, approved 500,600, dated 2 days apart). Within an
     # (iso, request, decision date) group, rows are kept in campaign-date order
     # only while their summed doses stay within (1 + dose_tolerance) of the
     # approved total; the rest are dropped as duplicates.
     who_dup <- logical(nrow(who_data))
     cand_rows <- which(who_unmatched & !is.na(who_key) & !is.na(who_data$decision_date))
     if (length(cand_rows) > 1L) {
          grp <- paste(who_data$iso_code[cand_rows], who_key[cand_rows],
                       who_data$decision_date[cand_rows])
          for (g in unique(grp[duplicated(grp)])) {
               rows <- cand_rows[grp == g]
               rows <- rows[order(who_data$campaign_date[rows])]
               approved <- suppressWarnings(max(as.numeric(who_data$doses_approved[rows]), na.rm = TRUE))
               if (!is.finite(approved) || approved <= 0) next
               cum <- cumsum(ifelse(is.na(who_data$doses_shipped[rows]), 0,
                                    who_data$doses_shipped[rows]))
               drop <- rows[cum > (1 + dose_tolerance) * approved]
               who_dup[drop] <- TRUE
          }
     }
     if (any(who_dup)) {
          message(glue::glue("  Step 5: dropped {sum(who_dup)} repeated WHO-only shipment(s) ",
                             "exceeding their request's approved total ",
                             "({format(sum(who_data$doses_shipped[who_dup], na.rm = TRUE), big.mark = ',')} doses)"))
     }
     who_unmatched <- who_unmatched & !who_dup
     
     message("\n==========================================")
     message("Match Summary:")
     message(glue::glue("  WHO campaigns matched: {sum(who_matched)} ({round(100*sum(who_matched)/nrow(who_data), 1)}%)"))
     message(glue::glue("  WHO campaigns unmatched: {sum(who_unmatched)} ({round(100*sum(who_unmatched)/nrow(who_data), 1)}%)"))
     message(glue::glue("  WHO doses matched: {format(sum(who_data$doses_shipped[who_matched]), big.mark=',')} ({round(100*sum(who_data$doses_shipped[who_matched])/sum(who_data$doses_shipped), 1)}%)"))
     message(glue::glue("  WHO doses unmatched: {format(sum(who_data$doses_shipped[who_unmatched]), big.mark=',')} ({round(100*sum(who_data$doses_shipped[who_unmatched])/sum(who_data$doses_shipped), 1)}%)"))
     message("==========================================")
     
     # Create combined dataset
     message("\nCreating combined dataset...")
     
     # Start with all GTFCC data. Match confidence is recorded on the GTFCC rows
     # here, by their position in gtfcc_data (the index space the match tables
     # use), BEFORE the rbind/re-sort below changes row positions.
     combined_data <- gtfcc_data
     combined_data$match_confidence <- "GTFCC_only"
     combined_data$match_confidence[gtfcc_matched_request] <- "low"
     combined_data$match_confidence[gtfcc_matched_date]  <- "low"
     combined_data$match_confidence[gtfcc_matched_fuzzy] <- "medium"
     combined_data$match_confidence[gtfcc_matched_exact] <- "high"
     
     # Update source for matched GTFCC campaigns
     gtfcc_matched_any <- gtfcc_matched_exact | gtfcc_matched_fuzzy | gtfcc_matched_date |
                          gtfcc_matched_request
     combined_data$source[gtfcc_matched_any] <- "GTFCC_WHO_matched"
     
     # Add unmatched WHO campaigns
     who_unique <- who_data[who_unmatched, ]
     # rep() so that zero unmatched WHO campaigns yields an empty frame, not an error
     who_unique$source <- rep("WHO_only", nrow(who_unique))
     who_unique$match_confidence <- rep("WHO_only", nrow(who_unique))
     
     # Ensure column compatibility
     common_cols <- intersect(names(combined_data), names(who_unique))
     combined_data <- rbind(combined_data[, common_cols], who_unique[, common_cols])
     combined_data$request_key <- NULL
     
     # Sort by campaign date
     combined_data <- combined_data[order(combined_data$campaign_date), ]
     
     # Regenerate ID column
     combined_data$id <- 1:nrow(combined_data)
     
     # Move match_confidence to the last column (its position before this fix)
     combined_data <- combined_data[, c(setdiff(names(combined_data), "match_confidence"),
                                        "match_confidence")]
     
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

#' Normalise an ICG request number to a year:sequence key
#'
#' WHO writes request 4 of 2017 as \code{20174} and GTFCC-derived rows as
#' \code{201704}; both map to \code{"2017:4"}. Values that are not a
#' four-digit year followed by a sequence number return \code{NA}.
#' @param x Request numbers (numeric or character).
#' @return Character vector of keys, \code{NA} where \code{x} is not parseable.
#' @noRd
.vacc_request_key <- function(x) {
     x <- trimws(as.character(x))
     ok <- !is.na(x) & grepl("^(19|20)[0-9]{2}[0-9]+$", x)
     key <- rep(NA_character_, length(x))
     key[ok] <- paste0(substr(x[ok], 1, 4), ":",
                       as.integer(substr(x[ok], 5, nchar(x[ok]))))
     key
}
