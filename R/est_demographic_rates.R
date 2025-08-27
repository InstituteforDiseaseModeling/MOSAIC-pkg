#' Estimate Daily Demographic Rates from UN World Population Prospects Data
#'
#' This function estimates daily population, birth rates, and death rates for countries in the MOSAIC framework based on
#' the UN World Population Prospects data. The function interpolates annual demographic data to daily
#' values and applies an optional smoothing method to generate continuous time-series estimates. The function can produce
#' smoothed times series of any length from 1967 to 2100 (the extent of processed UN data and model projections).
#'
#' @param PATHS A list containing paths where raw and processed data are stored.
#' PATHS is typically the output of the `get_paths()` function and should include the following components:
#' \itemize{
#'   \item \strong{DATA_RAW}: Path to the directory containing the raw UN World Population Prospects data.
#'   \item \strong{DATA_PROCESSED}: Path to the directory where processed demographic data should be saved.
#'   \item \strong{DATA_WHO_ANNUAL}: Path to the directory containing annual WHO cholera mortality data.
#'   \item \strong{MODEL_INPUT}: Path to the directory where model input parameter files should be saved.
#' }
#'
#' @param date_start The starting date for demographic parameters. Accepts `Date` class objects or valid character strings.
#' @param date_stop The ending date for data demographic parameters. Must be in a valid date format (same as `date_start`).
#' @param smooth_method The method used to smooth annual demographic rates when converting them to a daily time scale.
#' Must be one of the following:
#' \itemize{
#'   \item \strong{"none"}: No smoothing applied (annual values transposed to daily time scale).
#'   \item \strong{"linear"}: Applies simple moving average smoothing.
#'   \item \strong{"spline"}: Uses cubic spline interpolation for smoothing.
#' }
#'
#' @return The function does not return a value. It estimates demographic rates and saves the results as
#' CSV files containing population size, birth rates, and death rates over time.
#'
#' @details The function performs the following steps:
#' \enumerate{
#'   \item Filters the demographic data for the specified date range.
#'   \item Converts annual birth and death rates to daily estimates.
#'   \item Removes reported cholera deaths from all-cause mortality estimates.
#'   \item Applies the chosen smoothing method to daily demographic rates.
#'   \item Saves the processed data and model parameters to the specified directory.
#' }
#' The processed data file is saved in `PATHS$DATA_PROCESSED/demographics/`, and the model input parameters
#' are stored in `PATHS$MODEL_INPUT/`.
#'
#' Data Source: [United Nations World Population Prospects](https://population.un.org/wpp/)
#'
#' @importFrom countrycode countrycode
#' @importFrom glue glue
#' @importFrom utils read.csv write.csv
#' @importFrom stats filter approx smooth.spline
#'
#' @examples
#' \dontrun{
#' # Define paths for raw and processed data
#' PATHS <- get_paths()
#'
#' # Estimate demographic rates from 2000-01-01 to 2023-12-31 using spline smoothing
#' est_demographic_rates(PATHS, date_start = "2000-01-01", date_stop = "2023-12-31", smooth_method = "spline")
#' }
#'
#' @export

est_demographic_rates <- function(PATHS, date_start, date_stop, smooth_method) {

     convert_to_date <- function(date_input) {
          if (!inherits(date_input, "Date")) {
               return(as.Date(date_input, tryFormats = c("%Y-%m-%d", "%d-%m-%Y", "%m/%d/%Y")))
          }
          return(date_input)
     }

     # Convert date_start and date_stop if necessary
     date_start <- convert_to_date(date_start)
     date_stop <- convert_to_date(date_stop)

     if (!inherits(date_start, "Date") || !inherits(date_stop, "Date")) stop("Error: date_start and date_stop must be valid Date objects.")
     if (date_start > date_stop) stop("Error: date_start cannot be later than date_stop.")

     year_start <- as.numeric(format(date_start, "%Y"))
     year_stop <- as.numeric(format(date_stop, "%Y"))

     if (!is.numeric(year_start) || !is.numeric(year_stop)) stop("Error: year_start and year_stop must be numeric values.")
     if (year_start-3 < 1967 || year_start+3 > 2100) stop("Error: year_start must be between 1970 and 2097")
     if (year_stop-3 < 1967 || year_stop+3 > 2100) stop("Error: year_stop must be between 1970 and 2097")
     if (year_start > year_stop) stop("Error: year_start cannot be greater than year_stop.")

     years <- c((year_start-3):(year_stop+3))

     # Load the necessary packages and data
     message("Loading processed UN World Prospects demographic data")
     path <- file.path(PATHS$DATA_PROCESSED, "demographics/UN_world_population_prospects_annual.csv")
     d <- read.csv(path, stringsAsFactors = FALSE)

     d <- d[d$year %in% years,]
     d <- d[d$iso_code %in% MOSAIC::iso_codes_mosaic,]

     out <- data.frame(
          country = NA,
          iso_code = d$iso_code,
          year = d$year,
          population = d$total_population,
          births_per_day = (d$births_per_1000 * d$total_population / 1000) / 365,  # Calculate births per day
          deaths_per_day = (d$deaths_per_1000 * d$total_population / 1000) / 365  # Calculate deaths per day
     )


     message("Removing reported cholera deaths from all-cause mortality")
     who_afro_annual <- read.csv(file.path(PATHS$DATA_WHO_ANNUAL, "who_afro_annual_1949_2024.csv"), stringsAsFactors = FALSE)

     for (iso_code in unique(out$iso_code)) {

          for (year in years) {

               # Get avg daily cholera deaths for country and year
               sel <- which(who_afro_annual$iso_code == iso_code & who_afro_annual$year == year)

               if (length(sel) == 0) {

                    sel <- which(who_afro_annual$iso_code == iso_code)
                    tmp_who_afro_annual <- who_afro_annual[sel, c('iso_code', 'year', 'deaths_total')]
                    tmp_who_afro_annual$deaths_total <- mean(tmp_who_afro_annual$deaths_total, na.rm=TRUE)
                    tmp_who_afro_annual$year <- year
                    tmp_who_afro_annual <- tmp_who_afro_annual[1,]

               } else if (length(sel) > 1) {

                    stop("Found duplicated rows for total deaths when adjusting all cause mortality rate")

               } else if (length(sel) == 1) {

                    tmp_who_afro_annual <- who_afro_annual[sel, c('iso_code', 'year', 'deaths_total')]

               } else {

                    stop("Cannot find reported cholera deaths")
               }

               # Subtract avg daily cholera mortality from expected all cause mortality
               avg_daily_cholera_deaths <- tmp_who_afro_annual$deaths_total/365
               sel <- which(out$iso_code == iso_code & out$year == year)
               out[sel, 'deaths_per_day'] <- out[sel, 'deaths_per_day'] - avg_daily_cholera_deaths

          }
     }

     # Calculate birth and death rates per day as a proportion of population
     out$birth_rate_per_day <- out$births_per_day / out$population
     out$death_rate_per_day <- out$deaths_per_day / out$population

     # Convert ISO3 country codes to country names
     out$country <- MOSAIC::convert_iso_to_country(out$iso_code)

     if (F) {
          plot(out$year, out$population, col=factor(out$iso_code))
          plot(out$year, out$birth_rate_per_day, col=factor(out$iso_code))
          plot(out$year, out$death_rate_per_day, col=factor(out$iso_code))
     }


     message("Converting annual demographic rates to daily time scale...")

     # Create a daily time series covering all years
     daily_dates <- seq(from = as.Date(paste0(min(years), "-01-01")), to = as.Date(paste0(max(years), "-12-31")), by = "day")

     # Create a daily dataframe with year and day_of_year
     out_daily <- data.frame(date = daily_dates,
                             year = as.numeric(format(daily_dates, "%Y")))

     # Merge annual data with daily data using base R merge()
     out_daily <- merge(out_daily, out, by = "year", all.x = TRUE)
     out_daily <- out_daily[order(out_daily$country, out_daily$date),]


     # Initialize an empty dataframe to store results
     out_daily_smooth <- data.frame()

     smooth_series <- function(x, method) {

          if (method == 'none') {

               smoothed_series <- x

          } else if (method == 'linear') {

               smoothed_series <- stats::filter(x, rep(1 / 365, 365), sides = 2, circular = TRUE)

          } else if (method == 'spline') {

               n <- length(x)
               x_index <- 1:n
               spline_fit <- smooth.spline(x_index, x, spar = 0.55)
               smoothed_series <- spline_fit$y

          } else {
               stop("Method must be 'none', 'linear', or 'spline'")
          }

          return(smoothed_series)

     }

     # Initialize
     num_countries <- length(unique(out_daily$iso_code))
     pb <- txtProgressBar(min = 0, max = num_countries, style = 3)
     iso_codes <- unique(out_daily$iso_code)
     out_daily_smooth <- data.frame()

     for (i in seq_along(iso_codes)) {

          iso <- iso_codes[i]  # Get current iso_code
          tmp <- out_daily[out_daily$iso_code == iso, ]

          # Interpolate any missing values
          tmp$population_interp <- approx(as.numeric(tmp$date), tmp$population, xout = as.numeric(tmp$date), method = "linear", rule = 2)$y
          tmp$birth_rate_per_day_interp <- approx(as.numeric(tmp$date), tmp$birth_rate_per_day, xout = as.numeric(tmp$date), method = "linear", rule = 2)$y
          tmp$death_rate_per_day_interp <- approx(as.numeric(tmp$date), tmp$death_rate_per_day, xout = as.numeric(tmp$date), method = "linear", rule = 2)$y

          # Smooth demographic rates over daily time series
          tmp$population_smooth <- smooth_series(tmp$population_interp, method = smooth_method)
          tmp$birth_rate_per_day_smooth <- smooth_series(tmp$birth_rate_per_day_interp, method = smooth_method)
          tmp$death_rate_per_day_smooth <- smooth_series(tmp$death_rate_per_day_interp, method = smooth_method)

          # Remove buffer years
          tmp <- tmp[tmp$date >= date_start & tmp$date <= date_stop,]

          if (F) {
               par(mfrow=c(3,1))
               plot(tmp$date, tmp$population_interp, main = paste0('Total population (', iso, ')'), ylab='Total population', xlab='Date')
               lines(tmp$date, tmp$population_smooth, col='dodgerblue', lwd=2)
               plot(tmp$date, tmp$birth_rate_per_day_interp, main = paste0('Birth rate (', iso, ')'), ylab='Birth rate', xlab='Date')
               lines(tmp$date, tmp$birth_rate_per_day_smooth, col='green4', lwd=2)
               plot(tmp$date, tmp$death_rate_per_day_interp, main = paste0('Death rate (', iso, ')'), ylab='Death rate', xlab='Date')
               lines(tmp$date, tmp$death_rate_per_day_smooth, col='red3', lwd=2)
          }

          out_daily_smooth <- rbind(out_daily_smooth, tmp)

          # Update progress bar
          setTxtProgressBar(pb, i)
     }

     # Close progress bar
     close(pb)

     param_population_size <-
          make_param_df(variable_name = 'N',
                        variable_description = 'Total population for location j at time t',
                        parameter_distribution = 'point',
                        j = out_daily_smooth$iso_code,
                        t = out_daily_smooth$date,
                        parameter_name = 'mean',
                        parameter_value = out_daily_smooth$population_smooth)

     param_birth_rate <-
          make_param_df(variable_name = 'b',
                        variable_description = 'Birth rate for location j at time t',
                        parameter_distribution = 'point',
                        j = out_daily_smooth$iso_code,
                        t = out_daily_smooth$date,
                        parameter_name = 'mean',
                        parameter_value = out_daily_smooth$birth_rate_per_day_smooth)

     param_death_rate <-
          make_param_df(variable_name = 'd',
                        variable_description = 'Death rate for location j at time t',
                        parameter_distribution = 'point',
                        j = out_daily_smooth$iso_code,
                        t = out_daily_smooth$date,
                        parameter_name = 'mean',
                        parameter_value = out_daily_smooth$death_rate_per_day_smooth)


     path <- file.path(PATHS$MODEL_INPUT, "param_N_population_size.csv")
     write.csv(param_population_size, path, row.names = FALSE)
     message(glue("Total population from {date_start} to {date_stop} for all locations saved to: {path}"))

     path <- file.path(PATHS$MODEL_INPUT, "param_b_birth_rate.csv")
     write.csv(param_birth_rate, path, row.names = FALSE)
     message(glue("Birth rate from {date_start} to {date_stop} for all locations saved to: {path}"))

     path <- file.path(PATHS$MODEL_INPUT, "param_d_death_rate.csv")
     write.csv(param_death_rate, path, row.names = FALSE)
     message(glue("Death rate from {date_start} to {date_stop} for all locations saved to: {path}"))

}
