utils::globalVariables(c("destination", "origin", "value", "variable", "month_name", "year", "lon", "lat", "population"))
utils::globalVariables(c(
     "Origin_Longitude", "Origin_Latitude", "Destination_Longitude", "Destination_Latitude",
     "iso3", "country", "value", "variable", "month", "Jan", "Dec", "ci_lo", "ci_hi",
     "month_name", "year", "population", "lon", "lat", "destination", "origin"
))
utils::globalVariables(c("get_distance_matrix", "mobility", "element_text", "geom_sf", "geom_text", "guide_colorbar", "guides", "margin", "xlab", "ylab"))
utils::globalVariables(c("Param1", "Param2", "Correlation"))
utils::globalVariables(c(
     "date", "date_start", "cases", "deaths", "iso_code",
     "doses_distributed", "doses_distributed_cumulative", "doses_shipped",
     "prop_vaccinated", "central_days", "min_days", "max_days", "type",
     "day", "days", "predicted", "predicted_lo", "predicted_hi",
     "effectiveness", "effectiveness_lo", "effectiveness_hi", "source",
     "psi", "Days", "Model", "density"
))

# NSE column-name variables used in ggplot2::aes() and dplyr verbs across the
# package. Declared here to silence R CMD check "no visible binding for global
# variable" NOTEs (the column names are resolved at runtime from the data).
utils::globalVariables(c(
     ".", "..density..", "cases_binary", "cases_binary_clean", "cases_scaled",
     "cases_total", "category", "cfr", "cfr_2024_pred", "cfr_estimate",
     "cfr_hi", "cfr_lo", "cfr_lower", "cfr_observed", "cfr_trend",
     "cfr_trend_lower", "cfr_trend_upper", "cfr_upper", "channel", "cloud_cover_mean",
     "cluster", "corr", "correlation_label", "Country", "date_stop",
     "day_week_mid", "dbscan", "delta_max", "delta_min", "density_log10",
     "description", "destination_lat", "destination_lon", "distribution", "distribution_date",
     "diurnal_temp_range", "doy", "dry_week", "emdat_flood_active", "emdat_flood_prob",
     "emdat_flood_prob_anom", "ENSO3", "ENSO3_anom", "ENSO3_clim_mean", "ENSO3_clim_sd",
     "ENSO34", "ENSO34_anom", "ENSO34_clim_mean", "ENSO34_clim_sd", "ENSO4",
     "ENSO4_anom", "ENSO4_clim_mean", "ENSO4_clim_sd", "epoch", "estimated_parameters",
     "et0_fao_evapotranspiration_sum", "fitted_values_fourier_cases", "fitted_values_fourier_precip", "GDP", "group",
     "has_data", "has_historical_data", "hazard", "heat_index", "ic_moment_match",
     "id50", "id50_hi", "id50_lo", "Incidence_per_1000", "inferred_from_neighbor",
     "IOD", "IOD_anom", "IOD_clim_mean", "IOD_clim_sd", "is_subsaharan",
     "iso_a3", "iso_codes_mosaic", "kl", "label", "likelihood",
     "location", "log_cases", "loss", "mae", "method",
     "metric", "mosaic_status", "n_nonzero", "n_peaks", "note2",
     "observed", "origin_lat", "origin_lon", "output_file", "param_base",
     "param_type", "parameter", "parameter_clean", "parameter_name", "peak_value",
     "pop_density", "population_density", "poverty_ratio", "precip_anom", "precip_clim_mean",
     "precip_clim_sd", "precip_p90_threshold", "precip_scaled", "precipitation_sum", "pred",
     "pred_smooth", "predicted_median", "Probability", "psi_star", "q_lower",
     "q_median", "q_upper", "q0.25", "q0.75", "Q2.5",
     "q2.75", "q25", "q75", "q97.5", "Q97.5",
     "r", "random_effect", "ratio", "ratio_hi", "ratio_lo",
     "rel_weight", "relative_humidity_2m_mean", "reported_cases", "sample_initial_conditions", "shape1",
     "shape2", "sim_index", "smoothed", "soil_moisture_0_to_10cm_mean", "soil_moisture_anom",
     "soilm_clim_mean", "soilm_clim_sd", "spei_approx", "study_type", "temp_anom",
     "temp_clim_mean", "temp_clim_sd", "temp_p95_threshold", "temperature_2m_max", "temperature_2m_mean",
     "total_cases", "urban_pop_prop", "urban_population_pct", "val_loss", "val_mae",
     "Value", "variable_name", "vpd", "vpd_anom", "vpd_clim_mean",
     "vpd_clim_sd", "w", "WASH_Variable", "week", "Week",
     "weight", "weight_label", "Weighted_Mean_WASH", "wind_speed_10m_mean", "window_label",
     "x", "y", "ymax", "ymin", "zeta_1",
     "zeta_1_hi", "zeta_1_lo", "zeta_2", "zeta_2_hi", "zeta_2_lo",
     "zeta_ratio"
))

