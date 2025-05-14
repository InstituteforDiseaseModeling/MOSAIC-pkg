#' Default LASER Configuration (comprehensive)
#'
#' The **canonical** LASER parameter object shipped with MOSAIC.
#' It contains default values for *all* model parameters, initial state
#' vectors, and file-path references required to run the full cholera
#' metapopulation transmission model across any number of locations.
#'
#' @format A named **list** created by `make_default_LASER_config()`, whose
#'   elements include:
#'   * **Scalars** – biological constants (`phi_1`, `gamma_1`, `epsilon`, …);
#'   * **Vectors** – initial populations (`S_j_initial`, `I_j_initial`, …),
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
#' paths it references do not exist on the user’s machine.  For tutorials or
#' automated tests, consider using
#' [`config_simulation_epidemic`] or [`config_simulation_endemic`], which run
#' without any external dependencies.
#'
#' @usage
#' config_default
#'
#' @seealso
#' * [make_default_LASER_config()] – function that creates this object.
#' * [config_simulation_epidemic] – one-year outbreak toy data set.
#' * [config_simulation_endemic] – 5-year endemic toy data set.
#'
#' @keywords datasets
"config_default"
