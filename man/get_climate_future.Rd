% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_climate_future.R
\name{get_climate_future}
\alias{get_climate_future}
\title{Download Future Climate Data for Multiple Locations (One Model at a Time)}
\usage{
get_climate_future(
  lat,
  lon,
  date_start,
  date_stop,
  climate_variables,
  climate_model,
  api_key = NULL
)
}
\arguments{
\item{lat}{A numeric vector representing the latitudes of the locations.}

\item{lon}{A numeric vector representing the longitudes of the locations.}

\item{date_start}{A character string representing the start date for the data in "YYYY-MM-DD" format.}

\item{date_stop}{A character string representing the end date for the data in "YYYY-MM-DD" format.}

\item{climate_variables}{A character vector of climate variables to retrieve. Valid options include:
\itemize{
\item \strong{temperature_2m_mean}: Mean 2m air temperature.
\item \strong{temperature_2m_max}: Maximum 2m air temperature.
\item \strong{temperature_2m_min}: Minimum 2m air temperature.
\item \strong{wind_speed_10m_mean}: Mean 10m wind speed.
\item \strong{wind_speed_10m_max}: Maximum 10m wind speed.
\item \strong{cloud_cover_mean}: Mean cloud cover.
\item \strong{shortwave_radiation_sum}: Sum of shortwave radiation.
\item \strong{relative_humidity_2m_mean}: Mean 2m relative humidity.
\item \strong{relative_humidity_2m_max}: Maximum 2m relative humidity.
\item \strong{relative_humidity_2m_min}: Minimum 2m relative humidity.
\item \strong{dew_point_2m_mean}: Mean 2m dew point temperature.
\item \strong{dew_point_2m_min}: Minimum 2m dew point temperature.
\item \strong{dew_point_2m_max}: Maximum 2m dew point temperature.
\item \strong{precipitation_sum}: Total precipitation.
\item \strong{rain_sum}: Total rainfall.
\item \strong{snowfall_sum}: Total snowfall.
\item \strong{pressure_msl_mean}: Mean sea level pressure.
\item \strong{soil_moisture_0_to_10cm_mean}: Mean soil moisture (0-10 cm depth).
\item \strong{et0_fao_evapotranspiration_sum}: Sum of evapotranspiration (FAO standard).
}}

\item{climate_model}{A single character string representing the climate model to use. Available models include:
\itemize{
\item \strong{CMCC_CM2_VHR4}
\item \strong{FGOALS_f3_H}
\item \strong{HiRAM_SIT_HR}
\item \strong{MRI_AGCM3_2_S}
\item \strong{EC_Earth3P_HR}
\item \strong{MPI_ESM1_2_XR}
\item \strong{NICAM16_8S}
}}

\item{api_key}{A character string representing the API key for the climate data API. If not provided, the function assumes the API key is not required.}
}
\value{
A data frame with columns:
\itemize{
\item \strong{date}: The date of the climate data.
\item \strong{latitude}: The latitude of the location.
\item \strong{longitude}: The longitude of the location.
\item \strong{climate_model}: The climate model used for the data.
\item \strong{variable_name}: The climate variable retrieved (e.g., temperature_2m_mean, precipitation_sum).
\item \strong{value}: The value of the climate variable for each date.
}
}
\description{
This function retrieves daily future climate data for multiple specified locations and climate variables over a specified date range using a specified climate model.
}
\details{
The function retrieves daily future climate data for multiple specified locations using the Open-Meteo Climate API. It downloads the specified climate variables for each latitude and longitude provided, using a single climate model. The data is retrieved for the date range specified by \code{date_start} and \code{date_stop}. A progress bar is displayed to indicate the download progress.
}
\examples{
\dontrun{
# Define latitudes and longitudes for the locations
lat <- c(40.7128, 34.0522)
lon <- c(-74.0060, -118.2437)

# Define the climate variables and model
climate_vars <- c("temperature_2m_mean", "precipitation_sum")
climate_model <- "MRI_AGCM3_2_S"

# Set the date range and API key
date_start <- "2023-01-01"
date_stop <- "2030-12-31"
api_key <- "your_api_key_here"

# Download the climate data
climate_data <- get_climate_future(lat, lon, date_start, date_stop,
                                   climate_vars, climate_model, api_key)

# Display the climate data
head(climate_data)
}

}
