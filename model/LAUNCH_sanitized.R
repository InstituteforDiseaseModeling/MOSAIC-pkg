library(MOSAIC)

set_root_directory("<root_directory_blocked>")
PATHS <- MOSAIC::get_paths()

DATE_START <- as.Date("2023-02-01")
DATE_STOP  <- as.Date("2026-03-31")



################################################################################
#
# GROUP 1: INDEPENDENT DATA SOURCES
#
# These have no dependencies on each other and can be run in any order.
# Each reads from external repos or MOSAIC-data/raw/ and writes to
# MOSAIC-data/processed/.
#
################################################################################


#--- 1A. Geospatial: shapefiles and country similarity matrix -----------------#

download_africa_shapefile(PATHS)
download_all_country_shapefiles(PATHS)
process_country_similarity_data(PATHS)


#--- 1B. Climate: ERA5 historical + MRI projections ---------------------------#
#    Reads from: open-meteo-pipeline repo (must be cloned and up to date)
#    Writes to:  MOSAIC-data/processed/climate/daily/ and climate/weekly/

process_open_meteo_data(PATHS)


#--- 1C. Elevation: median elevation per country ------------------------------#

download_country_DEM(PATHS, iso_codes = MOSAIC::iso_codes_africa)
get_elevation(PATHS, iso_codes = MOSAIC::iso_codes_africa, "mean")
get_elevation(PATHS, iso_codes = MOSAIC::iso_codes_africa, "median")


#--- 1D. ENSO/IOD: historical + BOM forecast ----------------------------------#
#    Reads from: enso-data repo (must be cloned and up to date)
#    Writes to:  MOSAIC-data/processed/enso/

process_enso_data(PATHS)


#--- 1E. Demographics: UN World Population Prospects --------------------------#
#    Writes to: MOSAIC-data/processed/demographics/

process_UN_demographics_data(PATHS)


#--- 1F. World Bank country covariates ----------------------------------------#
#    Writes to: MOSAIC-data/processed/world_bank/

process_WB_GDP_data(PATHS)
process_WB_poverty_ratio_data(PATHS)
process_WB_population_density_data(PATHS)
process_WB_urban_population_data(PATHS)


#--- 1G. UNICEF malnutrition --------------------------------------------------#

process_UNICEF_malnutrition_data(PATHS)


#--- 1H. WASH coverage data from literature -----------------------------------#

get_WASH_data(PATHS)


#--- 1I. OAG flight traffic data ----------------------------------------------#

process_OAG_data(PATHS)


#--- 1J. Literature-derived parameters ----------------------------------------#
#    Symptomatic proportion, immune decay, vaccine effectiveness, care seeking

get_symptomatic_prop_data(PATHS)
get_immune_decay_data(PATHS)
get_vaccine_effectiveness_data(PATHS)
get_rho_care_seeking_params(PATHS)



################################################################################
#
# GROUP 2: SURVEILLANCE AND DERIVED ESTIMATES
#
# These depend on Group 1 outputs. Surveillance processing depends on
# shapefiles for ISO matching. CFR and demographic rates depend on WHO
# annual data and demographics respectively.
#
################################################################################


#--- 2A. Cholera surveillance: WHO + JHU + supplemental -----------------------#
#    Reads from: ees-cholera-mapping repo (WHO weekly), MOSAIC-data/raw/ (WHO annual)
#    Depends on: 1A (shapefiles for ISO matching)

process_WHO_annual_data(PATHS)
process_CFR_data(PATHS, min_obs = 3/0.02)
process_WHO_weekly_data(PATHS)
process_JHU_weekly_data(PATHS)
process_SUPP_weekly_data(PATHS)
downscale_weekly_cholera_data(PATHS)
process_cholera_surveillance_data(PATHS, keep_source = "WHO")


#--- 2B. Case fatality rate: hierarchical Bayesian model ----------------------#
#    Depends on: 2A (WHO annual data)
#    Writes to:  model/input/param_mu_disease_mortality.csv

est_CFR_hierarchical(PATHS,
                     min_cases = 3,
                     k_year = 15,
                     include_country_trends = TRUE,
                     population_weighted = FALSE,
                     save_diagnostics = FALSE,
                     verbose = TRUE)


#--- 2C. Demographic rates: birth and death rates -----------------------------#
#    Depends on: 1E (demographics), 2A (WHO annual for country list)
#    Writes to:  model/input/param_N_population_size.csv, param_b_birth_rate.csv,
#                param_d_death_rate.csv

est_demographic_rates(PATHS,
                      date_start = "2000-01-01",
                      date_stop = "2030-12-31",
                      smooth_method = "none")


#--- 2D. Vaccination data: WHO + GTFCC combined -------------------------------#
#    Reads from: ees-cholera-mapping repo (GTFCC), WHO dashboard
#    Depends on: 1E (demographics for rate calculation)

get_WHO_vaccination_data(PATHS)
process_WHO_vaccination_data(PATHS)
process_GTFCC_vaccination_data(PATHS)
combine_vaccination_data(PATHS)

est_vaccination_rate(PATHS,
                     max_rate_per_day = 20000,
                     date_start = "2000-01-01",
                     date_stop = DATE_STOP,
                     data_source = "BOTH")


#--- 2E. Vaccine effectiveness model ------------------------------------------#
#    Depends on: 1J (literature data)

est_vaccine_effectiveness(PATHS)



################################################################################
#
# GROUP 3: DERIVED MODEL PARAMETERS
#
# These depend on both Group 1 (processed data) and Group 2 (surveillance).
# Each combines multiple data sources to estimate model parameters written
# to model/input/.
#
################################################################################


#--- 3A. Seasonal dynamics: precipitation-cholera relationship ----------------#
#    Depends on: 1B (climate daily), 2A (cholera surveillance weekly)
#    Writes to:  model/input/param_seasonal_dynamics.csv,
#                pred_seasonal_dynamics_day.csv

est_seasonal_dynamics(PATHS,
                      date_start = '2010-09-01',
                      date_stop = '2025-09-01',
                      min_obs = 10,
                      clustering_method = "ward.D2",
                      k = 4,
                      data_sources = c('WHO', 'JHU', 'SUPP'))


#--- 3B. Symptomatic proportion and immune decay ------------------------------#
#    Depends on: 1J (literature data)

est_symptomatic_prop(PATHS)
est_immune_decay_vaccine(PATHS)


#--- 3C. Suspected cases and generation time ----------------------------------#

get_suspected_cases(PATHS)
get_generation_time_distribution(PATHS, mean_generation_time = 5)


#--- 3D. WASH coverage index (theta_j) ---------------------------------------#
#    Depends on: 1H (WASH data), 1A (similarity matrix), 2A (surveillance)

est_WASH_coverage(PATHS)


#--- 3E. Mobility: departure (tau_i) and diffusion (pi_ij) -------------------#
#    Depends on: 1I (OAG data), 1A (shapefiles), 1E (demographics)

est_mobility(PATHS)


#--- 3F. Epidemic peaks -------------------------------------------------------#
#    Depends on: 2A (cholera surveillance)

est_epidemic_peaks(PATHS)



################################################################################
#
# GROUP 4: ENVIRONMENTAL SUITABILITY (depends on all above)
#
# The LSTM suitability model requires climate, ENSO, surveillance, peaks,
# seasonal dynamics, vaccination, demographics, WASH, and mobility data.
# This must run last.
#
################################################################################


#--- 4A. Compile suitability training data ------------------------------------#
#    Depends on: 1B (climate), 1D (ENSO), 1E (demographics), 1F (world bank),
#                1H (WASH), 2A (surveillance), 2D (vaccination), 3E (mobility),
#                3F (epidemic peaks)

compile_suitability_data(PATHS,
                         cutoff = NULL,
                         use_epidemic_peaks = TRUE,
                         date_start = NULL,
                         date_stop = NULL,
                         forecast_mode = TRUE,
                         forecast_horizon = 9,
                         include_lags = TRUE)


#--- 4B. Fit LSTM suitability model -------------------------------------------#
#    Depends on: 4A (compiled suitability data)

est_suitability(PATHS,
                fit_date_start = '2015-01-01',
                fit_date_stop = '2025-07-24',
                pred_date_start = '2015-01-01',
                pred_date_stop = DATE_STOP,
                n_splits = 0,
                seed_base = 99,
                fine_tune_epochs = 15,
                fine_tune_lr = 0.001,
                split_method = "random",
                train_prop = 0.6)



################################################################################
#
# PLOTS AND DIAGNOSTICS
#
# These are visualization-only — they read from processed data and model/input/
# but do not produce any data artifacts. Safe to skip or run selectively.
#
################################################################################

# Surveillance
plot_cholera_surveillance_data(PATHS, iso = 'UGA')
for (i in MOSAIC::iso_codes_mosaic) {
     tryCatch(
          plot_cholera_surveillance_data(PATHS, iso = i),
          error = function(e) message("Error plotting ", i, ": ", e$message)
     )
}

# CFR
plot_CFR_hierarchical(PATHS,
                      countries_to_plot = NULL,
                      pdf_width = 8.5,
                      pdf_height = 11,
                      ncol_facet = 4,
                      show_ci = TRUE,
                      verbose = TRUE)
plot_CFR_by_country(PATHS)

# Vaccination
plot_vaccination_data(PATHS, data_source = "BOTH")
plot_vaccination_maps(PATHS, data_source = "BOTH")

# Vaccine effectiveness and care seeking
plot_vaccine_effectiveness(PATHS)
plot_rho_care_seeking_params(PATHS)

# Seasonal dynamics
plot_seasonal_transmission(PATHS)
plot_seasonal_transmission_example(PATHS, country_iso_code = "MOZ", n_points = 30)
plot_seasonal_clustering(PATHS,
                         use_cases = FALSE,
                         set_inferred_to_na = TRUE,
                         clustering_method = "ward.D2",
                         k = 4)

# Mobility
plot_mobility(PATHS)

# Climate and ENSO
plot_climate_data(PATHS, country_iso_code = 'MOZ')
plot_ENSO_data(PATHS, frequency = 'weekly')

# Epidemic peaks
plot_epidemic_peaks(PATHS)

# Suitability
plot_cases_binary(PATHS)
plot_suitability_by_country(PATHS)

model_data_file <- file.path(PATHS$MODEL_INPUT, "data_psi_suitability.csv")
if (file.exists(model_data_file)) {
     tmp <- read.csv(model_data_file, stringsAsFactors = FALSE)
     iso_with_data <- na.omit(unique(tmp$iso_code[tmp$cases_binary == 1 & !is.na(tmp$cases_binary)]))
     rm(tmp)
} else {
     warning("Model output file not found. Run est_suitability(PATHS) first.")
     iso_with_data <- character(0)
}
for (iso in iso_with_data) plot_suitability_and_cases(PATHS, plot_iso_code = iso)

# General
plot_africa_map(PATHS)
plot_vibrio_decay_rate(PATHS)
plot_recovery_duration(PATHS, symp_range = c(3, 7), asymp_range = c(7, 14))
plot_suspected_cases(PATHS)
plot_generation_time(PATHS)



################################################################################
#
# RUNNING MOSAIC: Transmission model simulation, posterior parameter estimation
#
################################################################################




################################################################################
#
# SCENARIOS AND FORECASTS
#
################################################################################
