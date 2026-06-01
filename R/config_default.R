#' Default LASER Configuration
#'
#' The **canonical** LASER parameter object shipped with MOSAIC.
#' It contains default values for *all* model parameters, initial state
#' vectors, and file-path references required to run the full cholera
#' metapopulation transmission model across any number of locations.
#'
#' @format A named **list** built by the `data-raw/make_config_default.R`
#'   script and persisted as `data/config_default.rda`. Elements include:
#'   * **Metadata** – `metadata$version`, `metadata$date`, `metadata$description`
#'     for provenance tracking;
#'   * **Scalars** – biological constants (`phi_1`, `gamma_1`, `epsilon`, …);
#'   * **Vectors** – initial populations (`S_j_initial`, `I_j_initial`, …),
#'     initial proportions (optional: `prop_S_initial`, `prop_I_initial`, …),
#'     coordinates (`longitude`, `latitude`), mobility coefficients, etc.;
#'   * **Matrices** – day-by-day series such as human-to-human transmission
#'     (`b_jt`), environmental suitability (`psi_jt`), birth/death rates;
#'   * **Character paths** – locations of required input data (see below).
#'
#' @details
#' Unlike the lightweight *simulation* configs exported in
#' `make_simulation_epidemic_LASER_config_files.R` and
#' `make_simulation_endemic_LASER_config_files.R`, the **default**
#' configuration assumes the presence of external data files arranged in
#' canonical MOSAIC directories:
#'
#' \describe{
#'   \item{MODEL_INPUT}{Directory containing baseline CSV inputs (population,
#'     rainfall, vaccination, WASH, etc.).}
#'   \item{DATA_WHO_DAILY}{Directory with processed WHO daily cholera line-list
#'     or aggregated case/death counts.}
#'   \item{ENV_RASTERS}{(Optional) Path to geotiff rasters used to build
#'     spatial priors for environmental suitability.}
#' }
#'
#' The object is intended for full-scale analyses and is therefore **large**
#' (tens of megabytes) and **not self-contained** – it will error if the file
#' paths it references do not exist on the user's machine.  For tutorials or
#' automated tests, consider using
#' [`config_simulation_epidemic`] or [`config_simulation_endemic`], which run
#' without any external dependencies.
#'
#' **Note on Initial Condition Formats**: This configuration includes both count
#' (`*_j_initial`) and proportion (`prop_*_initial`) representations of initial
#' conditions. The count fields are required by the LASER model for simulation,
#' while the proportion fields are optional and provided for statistical analysis
#' convenience. Both formats are automatically maintained in sync during
#' parameter sampling operations.
#'
#' @usage
#' config_default
#'
#' @seealso
#' * `data-raw/make_config_default.R` – the script that builds this object.
#' * [make_LASER_config()] – the validator/factory function used internally
#'   by the build script to validate every parameter.
#' * [config_simulation_epidemic] – one-year outbreak toy data set.
#' * [config_simulation_endemic] – 5-year endemic toy data set.
#'
#' @keywords datasets
"config_default"
