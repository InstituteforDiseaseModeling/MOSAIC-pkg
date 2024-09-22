if (!requireNamespace("MOSAIC", quietly = TRUE)) {
     message("Installing package 'MOSAIC' from GitHub...")
     if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
     remotes::install_github("InstituteforDiseaseModeling/MOSAIC-pkg")
}

library(MOSAIC)

packages <- c(
     "cowplot",
     "dplyr",
     "ggplot2",
     "ggrepel",
     "ggraph",
     "grid",
     "gridExtra",
     "igraph",
     "mobility",    # GitHub: https://github.com/COVID-19-Mobility-Data-Network/mobility
     "propvacc",    # GitHub: https://github.com/gilesjohnr/propvacc
     "rnaturalearth",
     "reshape2",
     "RColorBrewer",
     "shiny",
     "shinyWidgets",
     "viridis"
)

MOSAIC::load_or_install_packages(packages)


set_root_directory("~/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Projects/MOSAIC_IDM")
PATHS <- MOSAIC::get_paths()

set_openmeteo_api_key("aWshPbO8h8az9ico")


################################################################################
# DATA PREPARATION: downloading and processing all data required for MOSAIC
################################################################################

#-------------------------------------------------------------------------------
# Get shapefiles for all countries

download_africa_shapefile(PATHS)
download_all_country_shapefiles(PATHS)


#-------------------------------------------------------------------------------
# Process demographics data from UN World Prospects Study

process_demographics_data(PATHS)


#-------------------------------------------------------------------------------
# Download climate data for all countries from OpenMeteo APPI (aggregated by sampling point and
# by week)

download_climate_data(PATHS,
                      iso_codes = MOSAIC::iso_codes_africa,
                      date_start = "1970-01-01",
                      date_stop = "2030-12-31",
                      n_points = 30,
                      api_key = getOption('openmeteo_api_key'))


#-------------------------------------------------------------------------------
# Download median elevation of n points within each country

get_elevation(PATHS, n_points=30, api_key = getOption('openmeteo_api_key'))


#-------------------------------------------------------------------------------
# Download El Nino Southern Oscillation (ENSO) and the Dipole Mode Index (DMI)
# historical data from NOAA, forecasts from BOM

for (i in c("daily", "weekly", "monthly")) {

     compiled_enso_data <- compile_ENSO_data(1970, frequency = i, method = "spline")
     path <- file.path(PATHS$DATA_ENSO, paste0("compiled_ENSO_1970_2025_", i, ".csv"))
     write.csv(compiled_enso_data, file = path, row.names = FALSE)
     message("Data for ", i, " frequency saved to: ", path)

}


#-------------------------------------------------------------------------------
# Compile WASH data

get_WASH_data(PATHS)



#-------------------------------------------------------------------------------
# Download and compile WHO data for cases and deaths

process_WHO_annual_data(PATHS)

run_WHO_annual_data_app(PATHS)


process_WHO_weekly_data(PATHS)

#-------------------------------------------------------------------------------
# Get weekly case data from WHO AWD GIS dashboard




#-------------------------------------------------------------------------------
# Get vaccination data from GTFCC dashboard




#-------------------------------------------------------------------------------
# Process OAG flight traffic data

process_OAG_data(PATHS)



#-------------------------------------------------------------------------------
# Data from literature search on the proportion of cases that are symptomatic

get_symptomatic_prop_data(PATHS)




################################################################################
# MODEL INPUTS: all preliminary models of MOSAIC parameters and estimates of
# prior distributions
################################################################################


#-------------------------------------------------------------------------------
# Estimate the proportion of cases symptomatic

est_symptomatic_prop(PATHS)


#-------------------------------------------------------------------------------
# Estimate WASH coverage (theta_j)

est_WASH_coverage(PATHS)


#-------------------------------------------------------------------------------
# Estimate departure (tau_i) diffusion (pi_ij) model using flight data

fit_mobility_model(PATHS)


#-------------------------------------------------------------------------------
# Estimate environmental suitability (psi_jt)

est_environmental_suitability(PATHS)













################################################################################
# RUNNING MOSAIC: Transmission model simulation, posterior parameter estimation
################################################################################












################################################################################
# Scenarios and forecasts
################################################################################
