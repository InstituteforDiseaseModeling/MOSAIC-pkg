library(MOSAIC)
library(mobility)
library(propvacc)
library(ggplot2)
library(rnaturalearth)
library(reshape2)
library(ggraph)
library(igraph)
library(dplyr)
library(cowplot)
library(viridis)
library(gridExtra)
library(grid)
library(ggrepel)
library(shiny)
library(shinyWidgets)
library(RColorBrewer)

set_root_directory("<root_directory_blocked>")
PATHS <- MOSAIC::get_paths()

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
# Process climate data from open-meteo-pipeline repo (ERA5 + MRI projections)

process_open_meteo_data(PATHS)


#-------------------------------------------------------------------------------
# Download median elevation of n points within each country

get_elevation(PATHS, n_points=30)


#-------------------------------------------------------------------------------
# Process ENSO/IOD data from enso-data repo (NOAA + BOM historical/forecast)

process_enso_data(PATHS)


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









################################################################################
# MODEL INPUTS: all preliminary models of MOSAIC parameters and estimates of
# prior distributions
################################################################################



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
