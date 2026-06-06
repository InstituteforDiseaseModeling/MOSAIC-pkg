#' Named LSTM covariate feature sets for est_suitability()
#'
#' Curated covariate sets that \code{\link{est_suitability}} can select via its
#' \code{feature_set} argument. \code{"v7.3"} is the screening-informed 38-feature
#' set (cross-country sign-consistency; see provenance below); \code{"default"} is
#' the full production candidate set (no restriction).
#'
#' Provenance: \code{MOSAIC-Mozambique/claude/minfeat_v7_iterations.md} and the
#' multi-discipline covariate-screening review. The v7.3 set was selected by the
#' est_suitability ablation sweep (9 variations x 6 cutoffs x 22 countries).
#'
#' @format Character vector of covariate column names.
#' @seealso \code{\link{get_feature_set}}, \code{\link{est_suitability}}
#' @export
MINFEAT_V7_3_FEATURE_SET <- c(
     # Flood anomaly (6)
     "emdat_flood_prob_anom",
     "emdat_flood_prob_anom_lag4",
     "emdat_flood_prob_anom_lag8",
     "emdat_flood_prob_anom_lag12",
     "emdat_flood_prob_anom_lag16",
     "emdat_flood_prob_anom_lag20",
     # Flood displacement (2)
     "emdat_flood_prob", "emdat_flood_prob_12w_max",
     # ENSO4 short cluster (3)
     "ENSO4", "ENSO4_lag4", "ENSO4_lag8",
     # ENSO4 middle band (2)
     "ENSO4_lag12", "ENSO4_lag16",
     # ENSO4 long lag (1)
     "ENSO4_lag28",
     # ENSO34 (6)
     "ENSO34", "ENSO34_lag8", "ENSO34_lag12", "ENSO34_lag16",
     "ENSO34_lag24", "ENSO34_lag36",
     # Core climate (4)
     "temperature_2m_mean", "precipitation_sum",
     "relative_humidity_2m_mean", "soil_moisture_0_to_10cm_mean",
     # Precipitation lags (3)
     "precipitation_sum_lag8",
     "precipitation_sum_lag12",
     "precipitation_sum_lag16",
     # Soil moisture memory (2)
     "soil_moisture_0_to_10cm_mean_lag16",
     "soil_moisture_anom_lag16",
     # 12w integrators (2)
     "precip_sum_12w", "temp_mean_12w",
     # Anomalies / drought-flush channel (5)
     "precip_anom", "precip_anom_lag8", "precip_anom_lag12",
     "temp_anom", "spei_approx_lag12",
     # Seasonality (2) — cosines only
     "cos_annual", "cos_biannual"
)
# 6+2+3+2+1+6+4+3+2+2+5+2 = 38 features

#' Resolve a named feature set to a covariate vector
#'
#' @param name Feature-set name: \code{"v7.3"} (38 screening-informed features) or
#'   \code{"default"} (full production candidate set; returns \code{NULL} so the
#'   caller uses all available covariates).
#' @return Character vector of covariate names for \code{"v7.3"}; \code{NULL} for
#'   \code{"default"}. Errors on an unknown name.
#' @seealso \code{\link{est_suitability}}
#' @export
get_feature_set <- function(name = c("v7.3", "default")) {
     name <- match.arg(name)
     switch(name,
            "v7.3"    = MINFEAT_V7_3_FEATURE_SET,
            "default" = NULL)
}
