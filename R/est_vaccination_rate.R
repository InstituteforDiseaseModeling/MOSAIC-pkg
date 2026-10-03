#' Estimate OCV Vaccination Rates
#'
#' This function processes vaccination data from WHO or GTFCC, redistributes doses based on a maximum daily rate, splits them into first and second doses, and calculates vaccination parameters for use in the MOSAIC cholera model. The processed data includes redistributed daily doses, cumulative doses, and the proportion of the population vaccinated. The results are saved as CSV files for downstream modeling.
#'
#' @param PATHS A list containing file paths, including:
#' \describe{
#'   \item{DATA_SCRAPE_WHO_VACCINATION}{The path to the folder containing WHO vaccination data.}
#'   \item{DATA_DEMOGRAPHICS}{The path to the folder containing demographic data.}
#'   \item{MODEL_INPUT}{The path to the folder where processed data will be saved.}
#' }
#' @param date_start The start date for the vaccination data range (in "YYYY-MM-DD" format). Defaults to the earliest date in the data.
#' @param date_stop The stop date for the vaccination data range (in "YYYY-MM-DD" format). Defaults to the latest date in the data.
#' @param max_rate_per_day The maximum vaccination rate per day used to redistribute doses (no default; the data pipeline uses 20,000 doses/day).
#' @param data_source The source of the vaccination data. Must be one of \code{"WHO"}, \code{"GTFCC"}, or \code{"BOTH"}. When \code{"BOTH"} is specified, the function uses combined data from both sources with GTFCC prioritized and unique WHO campaigns added.
#'
#' @return This function does not return an R object but saves the following files to the directory specified in \code{PATHS$MODEL_INPUT}:
#' \itemize{
#'   \item A redistributed vaccination data file named \code{"data_vaccinations_<suffix>_redistributed.csv"} where suffix is WHO, GTFCC, or GTFCC_WHO; its \code{doses_distributed_dose1} and \code{doses_distributed_dose2} columns split \code{doses_distributed} into first and second doses.
#'   \item A parameter data frame for the vaccination rate (nu, all doses) named \code{"param_nu_vaccination_rate_<suffix>.csv"}.
#'   \item Parameter data frames for the first-dose (\code{nu_1}) and second-dose (\code{nu_2}) rates named \code{"param_nu_1_vaccination_rate_<suffix>.csv"} and \code{"param_nu_2_vaccination_rate_<suffix>.csv"}; on every location-day \code{nu_1 + nu_2 = nu}.
#' }
#'
#' @details
#' \strong{What nu is.} The output \code{nu} is the number of doses
#' \emph{shipped} per request, administered at up to \code{max_rate_per_day}
#' a day. A GTFCC request releases each delivery on its own date (its
#' \code{delivery_schedule}): deliveries join a stock, the stock is
#' administered at \code{max_rate_per_day} a day while it lasts, and a
#' request whose stock runs out resumes at its next delivery. A request with
#' one delivery, or whose next delivery arrives before its stock runs out, is
#' one unbroken run from its first delivery, as are WHO rows, which have no
#' schedule and start at their campaign date. Requests that overlap in a
#' location add up. Shipped-but-unused doses are counted as delivered. (Up to
#' MOSAIC v0.102.0 every delivery of a request was released from its first
#' delivery date, which moved later deliveries of multi-delivery requests --
#' typically second rounds and later campaigns of GTFCC preventive programmes
#' -- up to two and a half years early.)
#'
#' \strong{First and second doses.} Each day's doses of a request are split
#' into first doses (\code{nu_1}) and second doses (\code{nu_2}) by the
#' request's \code{round_sequence} (written by
#' \code{\link{process_GTFCC_vaccination_data}} from the GTFCC Round events and
#' carried through \code{\link{combine_vaccination_data}}): the request's
#' shipped doses are divided across its round blocks in proportion to the doses
#' administered in each round, and the blocks take consecutive stretches of the
#' request's daily series in administration order, so a two-round campaign
#' delivers its second doses after its first. Only the labels change -- the
#' daily totals are those of \code{nu} -- and the block boundaries are whole
#' doses, so with whole-dose shipments both series are whole numbers. A
#' request with no round information (no GTFCC Round events, or a WHO-only
#' shipment) counts entirely as first doses. In the engine, second doses move
#' \code{phi_2} of their recipients from V1 to V2 and are capped at the V1
#' stock, first doses move \code{phi_1} of theirs from the eligible
#' compartments to V1 (\code{sim_phase_vaccinated()}). Second rounds belong
#' mostly to campaigns up to 2022: the ICG suspended the two-dose regimen for
#' outbreak response in October 2022 (global OCV shortage; WHO news release,
#' 19 October 2022). Pre-t0 campaigns enter the initial conditions through
#' \code{\link{est_initial_V1_V2}}, which pairs rounds from the same Round
#' events.
#'
#' The function performs the following steps:
#' \enumerate{
#'   \item **Load Vaccination Data**:
#'     - Reads processed vaccination data from WHO or GTFCC and filters for relevant columns (\code{iso_code}, \code{campaign_date}, \code{doses_shipped}, \code{delivery_schedule}, \code{round_sequence}).
#'   \item **Redistribute Doses**:
#'     - Releases each delivery on its date and administers the stock day by day at up to a maximum daily rate (\code{max_rate_per_day}), then splits each day into first and second doses.
#'     - Sums requests that overlap on the same \code{distribution_date} within \code{iso_code}.
#'   \item **Validate Redistribution**:
#'     - Checks that the redistributed doses sum to the total shipped doses and that first plus second doses equal the doses distributed.
#'   \item **Ensure Full Coverage**:
#'     - Ensures all ISO codes have data across the full date range (\code{date_start} to \code{date_stop}), filling missing dates with zero doses.
#'   \item **Calculate Population Metrics**:
#'     - Merges population data for 2023, calculates cumulative doses, and computes the proportion of the population vaccinated (doses over population; this does not feed the \code{nu} parameters).
#'   \item **Save Outputs**:
#'     - Saves the redistributed vaccination data and the \code{nu}, \code{nu_1} and \code{nu_2} parameter data frames.
#' }
#'
#' @examples
#' \dontrun{
#' PATHS <- list(
#'   DATA_SCRAPE_WHO_VACCINATION = "path/to/who_vaccination_data",
#'   DATA_DEMOGRAPHICS = "path/to/demographics",
#'   MODEL_INPUT = "path/to/save/processed/data"
#' )
#' est_vaccination_rate(PATHS, max_rate_per_day = 20000, data_source = "WHO")
#' }
#'
#' @importFrom glue glue
#' @importFrom utils read.csv write.csv
#' @importFrom base mean unique
#' @export


est_vaccination_rate <- function(PATHS,
                                 date_start=NULL,
                                 date_stop=NULL,
                                 max_rate_per_day,
                                 data_source) {


     if (data_source == "WHO") {

          message("Loading processed WHO ICG vaccination data")
          data_path <- file.path(PATHS$MODEL_INPUT, "data_vaccinations_WHO.csv")
          vaccination_data <- read.csv(data_path, stringsAsFactors = FALSE)

     } else if (data_source == "GTFCC") {

          message("Loading processed GTFCC vaccination data")
          data_path <- file.path(PATHS$MODEL_INPUT, "data_vaccinations_GTFCC.csv")
          vaccination_data <- read.csv(data_path, stringsAsFactors = FALSE)

     } else if (data_source == "BOTH") {

          message("Loading combined GTFCC+WHO vaccination data")
          # Check if combined file exists, if not create it
          combined_path <- file.path(PATHS$MODEL_INPUT, "data_vaccinations_GTFCC_WHO.csv")
          if (!file.exists(combined_path)) {
               message("Combined data not found, creating it now...")
               combine_vaccination_data(PATHS)
          }
          data_path <- combined_path
          vaccination_data <- read.csv(data_path, stringsAsFactors = FALSE)

     } else {

          stop("data_source must be one of: 'WHO', 'GTFCC', or 'BOTH'")

     }

     # Delivery schedule and round attribution from process_GTFCC_vaccination_data();
     # WHO rows carry neither
     for (nm in c('delivery_schedule', 'round_sequence')) {
          if (!nm %in% names(vaccination_data)) vaccination_data[[nm]] <- NA_character_
          vaccination_data[[nm]] <- as.character(vaccination_data[[nm]])
     }
     vaccination_data <- vaccination_data[, c('iso_code', 'campaign_date', 'doses_shipped',
                                              'delivery_schedule', 'round_sequence')]
     vaccination_data$campaign_date <- as.Date(vaccination_data$campaign_date)

     bad <- is.na(vaccination_data$doses_shipped) | vaccination_data$doses_shipped < 0 |
          is.na(vaccination_data$campaign_date)
     if (any(bad)) {
          stop(glue::glue("{sum(bad)} campaign(s) in {basename(data_path)} have a missing or negative doses_shipped or no campaign_date"))
     }


     message(glue::glue('Redistributing vaccine doses using the maximum daily vaccination rate of {max_rate_per_day} per day'))
     no_schedule <- is.na(vaccination_data$delivery_schedule) | !nzchar(trimws(vaccination_data$delivery_schedule))
     message(glue::glue("Releasing each delivery on its own date for the {sum(!no_schedule)} campaigns with a delivery schedule; ",
                        "the other {sum(no_schedule)} start at their campaign date"))

     no_rounds <- is.na(vaccination_data$round_sequence) | !nzchar(trimws(vaccination_data$round_sequence))
     message(glue::glue("Splitting into first and second doses: {sum(!no_rounds)} of {nrow(vaccination_data)} campaigns carry round information; ",
                        "the other {sum(no_rounds)} ({format(sum(vaccination_data$doses_shipped[no_rounds]), big.mark = ',')} doses) count as first doses"))

     dose_cols <- c('doses_distributed', 'doses_distributed_dose1', 'doses_distributed_dose2')
     redistributed_data <- .vacc_redistribute(vaccination_data, max_rate_per_day)

     message("Summing campaigns that overlap in a location on the same day")
     redistributed_data <- stats::aggregate(
          redistributed_data[, dose_cols],
          by = list(iso_code = redistributed_data$iso_code,
                    distribution_date = redistributed_data$distribution_date),
          FUN = sum
     )


     tot_doses_data <- sum(vaccination_data$doses_shipped)
     tot_doses_redist <- sum(redistributed_data$doses_distributed)
     if (abs(tot_doses_redist - tot_doses_data) > 1e-6 * max(1, tot_doses_data)) {
          stop(glue("doses_distributed ({tot_doses_redist}) do not sum to doses_shipped ({tot_doses_data})"))
     }
     dose_gap <- max(abs(redistributed_data$doses_distributed_dose1 +
                              redistributed_data$doses_distributed_dose2 -
                              redistributed_data$doses_distributed))
     if (dose_gap > 1e-6) {
          stop(glue("first and second doses do not sum to doses_distributed (max gap {dose_gap})"))
     }
     message(glue::glue("Redistribution successful: {format(sum(redistributed_data$doses_distributed_dose1), big.mark = ',')} first doses, ",
                        "{format(sum(redistributed_data$doses_distributed_dose2), big.mark = ',')} second doses"))



     # Ensure all dates from date_start to date_stop are included for all ISO codes
     iso_with_data <- unique(redistributed_data$iso_code)
     message("MOSAIC locations WITH vaccination data:")
     message(paste(iso_with_data, collapse = ", "))
     message("MOSAIC locations WITHOUT vaccination data:")
     message(paste(MOSAIC::iso_codes_mosaic[!MOSAIC::iso_codes_mosaic %in% iso_with_data], collapse = ", "))

     extras <- iso_with_data[!iso_with_data %in% MOSAIC::iso_codes_mosaic]
     if (length(extras) > 0) {
          message("Locations WITH vaccination data NOT in MOSAIC locations:")
          message(paste(extras, collapse = ", "))
     }

     # Set date range for vaccination parameter
     date_min <- as.Date(min(c(vaccination_data$campaign_date, redistributed_data$distribution_date), na.rm = TRUE))
     date_max <- as.Date(max(c(vaccination_data$campaign_date, redistributed_data$distribution_date), na.rm = TRUE))

     if (is.null(date_start)) date_start <- date_min
     if (is.null(date_stop)) date_stop <- date_max

     if (date_start > date_min) warning(glue::glue("date_start ({date_start}) is later than global minimum distribution date in vaccination data ({date_min})"))
     if (date_stop < date_max) warning(glue::glue("date_stop ({date_stop}) is earlier than global maximum distribution date in vaccination data ({date_max})"))

     full_date_range <- as.Date(seq(as.Date(date_start), as.Date(date_stop), by = "day"))

     message("Building redistributed vaccination data that is square across all locations and full date range")
     in_mosaic <- redistributed_data$iso_code %in% MOSAIC::iso_codes_mosaic
     outside <- in_mosaic & !(redistributed_data$distribution_date %in% full_date_range)
     if (any(outside)) {
          stop(glue::glue("{sum(outside)} location-days of distributed doses fall outside {date_start} to {date_stop} ",
                          "({paste(unique(redistributed_data$iso_code[outside]), collapse = ', ')})"))
     }

     redistributed_data_square <- data.frame(
          iso_code = rep(MOSAIC::iso_codes_mosaic, each = length(full_date_range)),
          distribution_date = rep(full_date_range, times = length(MOSAIC::iso_codes_mosaic)),
          stringsAsFactors = FALSE
     )
     idx <- match(paste(redistributed_data_square$iso_code, redistributed_data_square$distribution_date),
                  paste(redistributed_data$iso_code, redistributed_data$distribution_date))
     for (v in dose_cols) {
          x <- numeric(nrow(redistributed_data_square))
          x[!is.na(idx)] <- redistributed_data[[v]][idx[!is.na(idx)]]
          redistributed_data_square[[v]] <- x
     }

     by_iso_square <- tapply(redistributed_data_square$doses_distributed, redistributed_data_square$iso_code, sum)
     by_iso_data <- tapply(redistributed_data$doses_distributed[in_mosaic], redistributed_data$iso_code[in_mosaic], sum)
     if (any(abs(by_iso_square[names(by_iso_data)] - by_iso_data) > 1e-6 * pmax(1, by_iso_data))) {
          stop("redistributed doses not equal after squaring dates")
     }

     redistributed_data_square <- redistributed_data_square[order(redistributed_data_square$iso_code,
                                                                  redistributed_data_square$distribution_date), ]
     redistributed_data_square$doses_distributed_cumulative <- stats::ave(
          redistributed_data_square$doses_distributed, redistributed_data_square$iso_code, FUN = cumsum)
     row.names(redistributed_data_square) <- NULL




     # Merge population data into redistributed data by iso_code

     # Load population data and keep data for the year 2023
     message("Calculating proportion vaccinated")
     message('Loading vaccination and population data')
     pop_data <- read.csv(file.path(PATHS$DATA_DEMOGRAPHICS, 'demographics_africa_2000_2023.csv'), stringsAsFactors = FALSE)
     pop_data <- pop_data[pop_data$year == 2023, c('iso_code', 'population')]
     message('NOTE: population sizes based on 2023')

     sel <- redistributed_data_square$distribution_date >= min(redistributed_data$distribution_date) &
          redistributed_data_square$distribution_date <= max(redistributed_data$distribution_date)
     redistributed_data <- redistributed_data_square[sel,]

     redistributed_data <- merge(redistributed_data, pop_data, by = "iso_code", all.x = TRUE)

     # Calculate the proportion of the population vaccinated for distributed doses
     redistributed_data$prop_vaccinated <- redistributed_data$doses_distributed_cumulative / redistributed_data$population


     redistributed_data <- redistributed_data[order(redistributed_data$iso_code, redistributed_data$distribution_date),]

     redistributed_data$country <- MOSAIC::convert_iso_to_country(redistributed_data$iso_code)

     redistributed_data$date <- redistributed_data$distribution_date

     # The dose split goes last so the columns that predate it keep their positions
     cols <- c('country', 'iso_code', 'date', 'doses_distributed', 'doses_distributed_cumulative', 'prop_vaccinated',
               'doses_distributed_dose1', 'doses_distributed_dose2')
     redistributed_data <- redistributed_data[,cols]



     # Save redistributed vaccination data to CSV
     # Use appropriate suffix for combined data
     suffix <- ifelse(data_source == "BOTH", "GTFCC_WHO", data_source)
     data_path <- file.path(PATHS$MODEL_INPUT, glue::glue("data_vaccinations_{suffix}_redistributed.csv"))
     write.csv(redistributed_data, data_path, row.names = FALSE)
     message(paste("Redistributed vaccination data saved to:", data_path))


     # Parameter data frames for the vaccination rate: all doses (nu), first
     # doses (nu_1) and second doses (nu_2)
     nu_files <- .vacc_nu_files(PATHS$MODEL_INPUT, suffix)
     nu_params <- list(
          nu   = list(value = 'doses_distributed',
                      description = 'vaccination rate absolute value'),
          nu_1 = list(value = 'doses_distributed_dose1',
                      description = 'first-dose vaccination rate absolute value'),
          nu_2 = list(value = 'doses_distributed_dose2',
                      description = 'second-dose vaccination rate absolute value')
     )
     for (nm in names(nu_params)) {
          param_df <- MOSAIC::make_param_df(
               variable_name = nm,
               variable_description = nu_params[[nm]]$description,
               parameter_distribution = 'point',
               parameter_name = 'mean',
               j = redistributed_data_square$iso_code,
               t = redistributed_data_square$distribution_date,
               parameter_value = redistributed_data_square[[nu_params[[nm]]$value]]
          )
          write.csv(param_df, nu_files[[nm]], row.names = FALSE)
          message(glue::glue("Parameter data frame for vaccination rate ({nm}) saved to: {nu_files[[nm]]}"))
     }



}


#' File names of the vaccination rate parameter files
#'
#' The single definition of where \code{\link{est_vaccination_rate}} writes
#' the all-dose, first-dose and second-dose rates and where
#' \code{data-raw/make_config_default.R} reads them.
#' @param dir Directory (\code{PATHS$MODEL_INPUT}).
#' @param suffix Source suffix: \code{"GTFCC_WHO"}, \code{"WHO"} or \code{"GTFCC"}.
#' @return A named list of paths: \code{nu}, \code{nu_1}, \code{nu_2}.
#' @noRd
.vacc_nu_files <- function(dir, suffix) {
     list(nu   = file.path(dir, sprintf("param_nu_vaccination_rate_%s.csv", suffix)),
          nu_1 = file.path(dir, sprintf("param_nu_1_vaccination_rate_%s.csv", suffix)),
          nu_2 = file.path(dir, sprintf("param_nu_2_vaccination_rate_%s.csv", suffix)))
}


#' Daily doses of one request from its deliveries
#'
#' Each delivery joins a stock on its date; the stock is administered at
#' \code{max_rate_per_day} a day while it lasts, and when it runs out the
#' series resumes at the next delivery. A single delivery gives
#' \code{max_rate_per_day} a day from its date and the remainder on the last
#' day.
#' @param dates Delivery dates (Date).
#' @param doses Doses of each delivery (non-negative).
#' @param max_rate_per_day Maximum doses per day.
#' @return A data frame with \code{date} and \code{doses}, one row per day
#'   with doses (no rows when there are no doses).
#' @noRd
.vacc_release <- function(dates, doses, max_rate_per_day) {
     o <- order(dates)
     day_of <- as.numeric(as.Date(dates[o]))
     doses <- as.numeric(doses[o])
     # every day gives a full max_rate_per_day or empties the stock, which needs
     # a fresh delivery to restart, so this bounds the number of days
     n_max <- sum(ceiling(doses / max_rate_per_day)) + length(doses)
     out_day <- numeric(n_max)
     out_doses <- numeric(n_max)
     m <- 0L
     k <- 1L
     stock <- 0
     day <- if (length(day_of)) day_of[1] else 0
     repeat {
          while (k <= length(day_of) && day_of[k] <= day) {
               stock <- stock + doses[k]
               k <- k + 1L
          }
          if (stock > 0) {
               given <- min(stock, max_rate_per_day)
               m <- m + 1L
               out_day[m] <- day
               out_doses[m] <- given
               stock <- stock - given
               day <- day + 1
          } else if (k <= length(day_of)) {
               day <- day_of[k]
          } else {
               break
          }
     }
     data.frame(date = as.Date(out_day[seq_len(m)], origin = "1970-01-01"),
                doses = out_doses[seq_len(m)])
}


#' Parse a delivery_schedule
#'
#' @param x One \code{delivery_schedule} string (\code{"<date>:<doses>;..."}).
#' @return A data frame with \code{date} (Date) and \code{doses}, or
#'   \code{NULL} when there is no schedule (\code{NA} or empty).
#' @noRd
.vacc_parse_delivery_schedule <- function(x) {
     if (is.na(x) || !nzchar(trimws(x))) return(NULL)
     parts <- strsplit(strsplit(trimws(x), ";", fixed = TRUE)[[1]], ":", fixed = TRUE)
     ok <- lengths(parts) == 2L
     date <- suppressWarnings(as.Date(vapply(parts[ok], function(p) p[1], character(1)), optional = TRUE))
     doses <- suppressWarnings(as.numeric(vapply(parts[ok], function(p) p[2], character(1))))
     if (!all(ok) || anyNA(date) || anyNA(doses) || any(doses < 0)) {
          stop("Malformed delivery_schedule '", x, "': expected '<YYYY-MM-DD>:<doses>' pairs ",
               "separated by ';' with non-negative doses")
     }
     data.frame(date = date, doses = doses)
}


#' Parse a round_sequence into dose blocks
#'
#' @param x One \code{round_sequence} string (\code{"<dose>:<weight>;..."}).
#' @return A data frame with integer \code{dose} (1 or 2) and numeric
#'   \code{weight}, in administration order, or \code{NULL} when the round is
#'   unknown (\code{NA} or empty).
#' @noRd
.vacc_parse_round_sequence <- function(x) {
     if (is.na(x) || !nzchar(trimws(x))) return(NULL)
     parts <- strsplit(strsplit(trimws(x), ";", fixed = TRUE)[[1]], ":", fixed = TRUE)
     ok <- lengths(parts) == 2L
     dose <- suppressWarnings(as.integer(vapply(parts[ok], function(p) p[1], character(1))))
     weight <- suppressWarnings(as.numeric(vapply(parts[ok], function(p) p[2], character(1))))
     if (!all(ok) || anyNA(dose) || anyNA(weight) || !all(dose %in% c(1L, 2L)) ||
         any(weight < 0) || sum(weight) <= 0) {
          stop("Malformed round_sequence '", x, "': expected '<dose>:<weight>' blocks ",
               "separated by ';', dose 1 or 2, non-negative weights with a positive sum")
     }
     data.frame(dose = dose, weight = weight)
}


#' Split one shipment's daily doses into first and second doses
#'
#' The shipment's doses are divided across its round blocks in proportion to
#' the block weights, with the boundaries rounded to whole doses, and the
#' blocks take consecutive stretches of the daily series in order. Each day's
#' first and second doses sum to its doses exactly.
#' @param daily Doses per day (see \code{.vacc_release()}).
#' @param blocks Dose blocks (see \code{.vacc_parse_round_sequence()}).
#' @return A \code{length(daily) x 2} matrix: first doses, second doses.
#' @noRd
.vacc_split_drip <- function(daily, blocks) {
     total <- sum(daily)
     bound <- c(0, pmin(round(total * cumsum(blocks$weight) / sum(blocks$weight)), total))
     bound[length(bound)] <- total
     cum <- c(0, cumsum(daily))
     day_lo <- cum[-length(cum)]
     day_hi <- cum[-1]
     out <- matrix(0, nrow = length(daily), ncol = 2L)
     for (k in seq_len(nrow(blocks))) {
          overlap <- pmax(0, pmin(day_hi, bound[k + 1L]) - pmax(day_lo, bound[k]))
          out[, blocks$dose[k]] <- out[, blocks$dose[k]] + overlap
     }
     out
}


#' Redistribute shipped doses over days, split into first and second doses
#'
#' @param vaccination_data Data frame with \code{iso_code}, \code{campaign_date}
#'   (Date), \code{doses_shipped}, \code{delivery_schedule} and
#'   \code{round_sequence}.
#' @param max_rate_per_day Maximum doses per day.
#' @return One row per campaign-day: \code{iso_code}, \code{distribution_date},
#'   \code{doses_distributed}, \code{doses_distributed_dose1},
#'   \code{doses_distributed_dose2}. Campaigns overlapping on a day are not
#'   summed here.
#' @noRd
.vacc_redistribute <- function(vaccination_data, max_rate_per_day) {
     rows <- lapply(seq_len(nrow(vaccination_data)), function(i) {
          deliveries <- .vacc_parse_delivery_schedule(vaccination_data$delivery_schedule[i])
          if (is.null(deliveries)) {
               deliveries <- data.frame(date = vaccination_data$campaign_date[i],
                                        doses = vaccination_data$doses_shipped[i])
          } else if (abs(sum(deliveries$doses) - vaccination_data$doses_shipped[i]) >
                     1e-6 * max(1, vaccination_data$doses_shipped[i])) {
               stop("delivery_schedule '", vaccination_data$delivery_schedule[i], "' does not sum to doses_shipped (",
                    vaccination_data$doses_shipped[i], ")")
          }
          released <- .vacc_release(deliveries$date, deliveries$doses, max_rate_per_day)
          if (!nrow(released)) return(NULL)
          daily <- released$doses
          blocks <- .vacc_parse_round_sequence(vaccination_data$round_sequence[i])
          split <- if (is.null(blocks)) cbind(daily, 0) else .vacc_split_drip(daily, blocks)
          data.frame(iso_code = vaccination_data$iso_code[i],
                     distribution_date = released$date,
                     doses_distributed = daily,
                     doses_distributed_dose1 = split[, 1],
                     doses_distributed_dose2 = split[, 2],
                     stringsAsFactors = FALSE)
     })
     rows <- rows[!vapply(rows, is.null, logical(1))]
     if (!length(rows)) stop("No vaccination doses to redistribute")
     do.call(rbind, rows)
}


#' First- and second-dose rate matrices for a simulation window
#'
#' Reads the \code{nu}, \code{nu_1} and \code{nu_2} parameter files written by
#' \code{\link{est_vaccination_rate}} and returns them as
#' \code{[location x day]} matrices aligned to \code{location_name} and
#' \code{dates}, after checking that every location-day is present and that
#' \code{nu_1 + nu_2 = nu}. Used by \code{data-raw/make_config_default.R}.
#' @param dir Directory holding the files (\code{PATHS$MODEL_INPUT}).
#' @param suffix Source suffix (see \code{.vacc_nu_files()}).
#' @param location_name ISO codes, in config order.
#' @param dates Daily Date sequence of the window.
#' @param tol Largest tolerated \code{|nu_1 + nu_2 - nu|}.
#' @return A list with \code{nu_1_jt}, \code{nu_2_jt} and \code{nu_jt}.
#' @noRd
.vacc_nu_jt <- function(dir, suffix, location_name, dates, tol = 1e-6) {
     files <- .vacc_nu_files(dir, suffix)
     missing <- !file.exists(unlist(files))
     if (any(missing)) {
          stop("Missing vaccination rate file(s): ", paste(unlist(files)[missing], collapse = ", "),
               ". Re-run est_vaccination_rate() to write the nu, nu_1 and nu_2 files together.",
               call. = FALSE)
     }
     dates <- as.Date(dates)
     read_one <- function(path) {
          d <- utils::read.csv(path, stringsAsFactors = FALSE)
          d$t <- as.Date(d$t)
          d <- d[d$j %in% location_name & d$t >= min(dates) & d$t <= max(dates), ]
          if (anyDuplicated(paste(d$j, d$t))) {
               stop(basename(path), " has duplicated location-days.", call. = FALSE)
          }
          m <- matrix(NA_real_, nrow = length(location_name), ncol = length(dates),
                      dimnames = list(location_name, as.character(dates)))
          m[cbind(match(d$j, location_name), match(d$t, dates))] <- d$parameter_value
          if (anyNA(m)) {
               stop(basename(path), " does not cover every location-day from ", min(dates),
                    " to ", max(dates), ".", call. = FALSE)
          }
          if (any(m < 0)) stop(basename(path), " has negative dose counts.", call. = FALSE)
          m
     }
     nu_jt <- read_one(files$nu)
     nu_1_jt <- read_one(files$nu_1)
     nu_2_jt <- read_one(files$nu_2)
     gap <- max(abs(nu_1_jt + nu_2_jt - nu_jt))
     if (gap > tol) {
          stop(sprintf(paste0("First- and second-dose rates do not sum to the vaccination rate ",
                              "(max |nu_1 + nu_2 - nu| = %g): the param_nu files are out of step; ",
                              "re-run est_vaccination_rate()."), gap), call. = FALSE)
     }
     list(nu_1_jt = nu_1_jt, nu_2_jt = nu_2_jt, nu_jt = nu_jt)
}
