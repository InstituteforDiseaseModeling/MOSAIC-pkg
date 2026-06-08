# =============================================================================
# region_maps_suitability.R — Region-map resolution for the lstm_v2 hierarchical
# FiLM. arch_control$region_map selects one of five maps:
#
#   "csv"         -> NULL: use the suitability CSV's `region` column (4 WHO admin
#                    regions). The B4 reproduction baseline.
#   "snf_k5"      -> static map inst/extdata/region_map_snf_k5.csv (production DEFAULT)
#   "snf_k4"      -> static map inst/extdata/region_map_snf_k4.csv
#   "seasonal_v1" -> static map inst/extdata/region_map_seasonal_v1.csv
#   "hydro_v1"    -> .psi_region_map_hydro_v1 (the sandbox 5-zone hand map)
#
# All return a named character vector (iso_code -> region) or NULL ("csv").
# =============================================================================

# Hydro-ecological 5-zone hand map (toggle "hydro_v1"). Transcribed from the
# MOSAIC-Mozambique sandbox MOSAIC_REGION_MAP_V1 (io.R). Includes a few
# countries outside the MOSAIC-40 (COM/DJI/MDG/LSO/CPV) — harmless, since only
# pool countries are looked up; all 40 MOSAIC countries are covered.
#' @keywords internal
#' @noRd
.psi_region_map_hydro_v1 <- c(
     # East-coastal-tropical
     MOZ = "east_coastal", TZA = "east_coastal", KEN = "east_coastal",
     SOM = "east_coastal", COM = "east_coastal", MDG = "east_coastal",
     DJI = "east_coastal",
     # East-highland
     RWA = "east_highland", BDI = "east_highland", UGA = "east_highland",
     ETH = "east_highland", ERI = "east_highland",
     # Southern-Africa
     MWI = "southern", ZMB = "southern", ZWE = "southern", BWA = "southern",
     NAM = "southern", ZAF = "southern", LSO = "southern", SWZ = "southern",
     AGO = "southern",
     # Central-equatorial (Congo basin + west-central)
     COD = "central_equatorial", COG = "central_equatorial",
     CAF = "central_equatorial", CMR = "central_equatorial",
     GAB = "central_equatorial", GNQ = "central_equatorial",
     SSD = "central_equatorial",
     # Sahel-and-West (Sahel + West African coast)
     NER = "sahel_west", TCD = "sahel_west", MLI = "sahel_west",
     BFA = "sahel_west", MRT = "sahel_west", GMB = "sahel_west",
     GNB = "sahel_west", SEN = "sahel_west", GIN = "sahel_west",
     SLE = "sahel_west", LBR = "sahel_west", CIV = "sahel_west",
     GHA = "sahel_west", BEN = "sahel_west", TGO = "sahel_west",
     NGA = "sahel_west", CPV = "sahel_west"
)

#' Resolve a region_map name to a named iso -> region vector (or NULL for "csv").
#' @keywords internal
#' @noRd
.psi_resolve_region_map <- function(name = c("snf_k5", "csv", "seasonal_v1",
                                             "hydro_v1", "snf_k4")) {
     name <- match.arg(name)
     if (identical(name, "csv"))      return(NULL)
     if (identical(name, "hydro_v1")) return(.psi_region_map_hydro_v1)
     f <- system.file("extdata", sprintf("region_map_%s.csv", name),
                      package = "MOSAIC")
     if (!nzchar(f) || !file.exists(f))
          stop(sprintf(".psi_resolve_region_map: static region map for '%s' not found (expected inst/extdata/region_map_%s.csv)",
                       name, name), call. = FALSE)
     m <- utils::read.csv(f, stringsAsFactors = FALSE)
     if (!all(c("iso_code", "region") %in% names(m)))
          stop(sprintf(".psi_resolve_region_map: '%s' CSV must have columns iso_code, region", name))
     stats::setNames(as.character(m$region), as.character(m$iso_code))
}
