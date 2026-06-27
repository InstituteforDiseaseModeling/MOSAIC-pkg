#' Get Location-Specific Configuration
#'
#' Extracts configuration for specific location(s) from a config object.
#' Systematically subsets all location-specific parameters to only include
#' the requested location(s).
#'
#' @param iso Character vector of ISO3 country codes to extract (e.g., "ETH" or c("ETH", "KEN")).
#' @param config A configuration list object in the format of MOSAIC::config_default.
#'   If NULL, will use MOSAIC::config_default.
#'
#' @return A config list object with the same structure as the input, but with
#'   all location-specific parameters subset to only the requested locations.
#'
#' @details
#' This function systematically identifies and subsets all location-specific
#' parameters in the config object. Location-specific parameters are identified
#' as vectors with length matching the number of locations in the original config.
#'
#' For single location extractions, vector parameters are converted to scalars
#' to maintain consistency with single-location configurations.
#'
#' Node-level parameters (N, S, V1, V2, E, I, R) are also properly subset based
#' on the cumulative node indices for each location.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract config for Ethiopia
#' eth_config <- get_location_config(iso = "ETH")
#'
#' # Extract config for multiple countries
#' east_africa_config <- get_location_config(
#'   config = MOSAIC::config_default,
#'   iso = c("ETH", "KEN", "UGA", "TZA")
#' )
#'
#' # Use with sampled parameters
#' config_sampled <- sample_parameters(seed = 123)
#' config_eth <- get_location_config(config_sampled, iso = "ETH")
#' }
get_location_config <- function(iso, config = NULL) {

     # ============================================================================
     # Input validation
     # ============================================================================

     if (missing(iso) || is.null(iso)) {
          stop("iso argument is required")
     }

     if (!is.character(iso)) {
          stop("iso must be a character vector")
     }

     if (length(iso) == 0) {
          stop("iso must contain at least one location")
     }

     # Load default config if not provided
     if (is.null(config)) {
          if (!requireNamespace("MOSAIC", quietly = TRUE)) {
               stop("MOSAIC package not found. Please provide config object explicitly.")
          }
          config <- MOSAIC::config_default
     }

     if (!is.list(config)) {
          stop("config must be a list object")
     }

     if (is.null(config$location_name)) {
          stop("config must contain 'location_name' field")
     }

     # ============================================================================
     # Identify location indices
     # ============================================================================

     # Get original locations
     original_locations <- config$location_name
     n_original_locations <- length(original_locations)

     # Ensure ISO codes are uppercase
     iso <- toupper(iso)

     # Check that all requested locations exist
     missing_locations <- setdiff(iso, original_locations)
     if (length(missing_locations) > 0) {
          stop(paste0(
               "The following location(s) not found in config: ",
               paste(missing_locations, collapse = ", "),
               "\nAvailable locations: ",
               paste(sort(original_locations), collapse = ", ")
          ))
     }

     sel <- which(config$location_name %in% iso)

     # ============================================================================
     # Start with full config and systematically update it
     # ============================================================================

     out <- config
     out$location_name <- out$location_name[sel]

     location_params <- c(
          names(out)[grep( '_initial', names(out))],
          "longitude", "latitude",
          "tau_i", "theta_j",
          "beta_j0_hum", "beta_j0_env",
          "beta_j0_tot", "p_beta",
          "a_1_j", "a_2_j", "b_1_j", "b_2_j",
          "CFR_target",
          "mu_j_baseline", "mu_j_slope", "mu_j_epidemic_factor",
          "epidemic_threshold",
          "psi_star_a", "psi_star_b", "psi_star_z", "psi_star_k"
     )

     location_params <- intersect(location_params, names(out))

     for (l in location_params) {

          out[[l]] <- out[[l]][sel]

     }

     # alpha_1 is DUAL-MODE (config_default v4.7 / priors_default v15.16): a
     # length-nL per-location vector OR a global scalar. Subset it by position
     # ONLY when it is a full-length per-location vector (length == the FULL
     # config location count); leave a scalar (or any non-full-length value,
     # e.g. legacy configs) untouched so the engine broadcasts it. Subsetting a
     # scalar with [sel] for nL>1 would produce NAs. (alpha_2 stays a global
     # scalar and is never subset.)
     if (!is.null(out$alpha_1) &&
         length(out$alpha_1) == length(config$location_name)) {
          out$alpha_1 <- out$alpha_1[sel]
     }


     # Match only true location x time matrices ending in "_jt" — anchored
     # to exclude e.g. nu_jt_sources, a 1D character vector of compartment
     # names that happens to contain the "_jt" substring.
     # Row-subset all location x time matrices: the "_jt" matrices plus the
     # reported fit matrices and their per-observation confidence-weight matrices.
     # The weight matrices must stay row-aligned with reported_cases/deaths or a
     # subset/single-location fit would mismatch dimensions. intersect() keeps this
     # robust to older configs that lack the weight matrices.
     location_params <- c(
          names(out)[grep('_jt$', names(out))],
          intersect(c("reported_cases", "reported_deaths",
                      "reported_cases_weight", "reported_deaths_weight"),
                    names(out))
     )

     for (l in location_params) {
          # Use drop=FALSE to maintain matrix structure even for single location
          out[[l]] <- out[[l]][sel, , drop = FALSE]
     }

     # Filter epidemic_peaks to the selected ISO codes. laser-cholera v0.13+
     # asserts every iso_code in epidemic_peaks appears in location_name.
     if (!is.null(out$epidemic_peaks) && nrow(out$epidemic_peaks) > 0L) {
          out$epidemic_peaks <- .filter_epidemic_peaks(
               peaks          = out$epidemic_peaks,
               date_start     = out$date_start,
               date_stop      = out$date_stop,
               location_names = out$location_name
          )
     }

     # Drop an empty (0-row) epidemic_peaks. A 0-row frame -- e.g. a no-peak
     # location, or a filter that matched nothing -- JSON-round-trips to a Dask
     # worker WITHOUT its iso_code column, crashing laser params.py:303
     # (`.iso_code` on a column-less DataFrame, "object has no attribute
     # 'iso_code'"). Nulling it makes the engine skip the epidemic_peaks block.
     if (!is.null(out$epidemic_peaks) && NROW(out$epidemic_peaks) == 0L) {
          out$epidemic_peaks <- NULL
     }

     return(out)
}
