#' Default prior distributions for MOSAIC model parameters
#'
#' A comprehensive list containing default informative prior distributions for all MOSAIC model parameters,
#' organized into global parameters and location-specific parameters for 40 African countries (301 parameters total).
#'
#' @format A list with 3 main components:
#' \describe{
#'   \item{metadata}{List containing version, date, and description of the priors}
#'   \item{parameters_global}{List of 21 global model parameters with their prior distributions:
#'     \itemize{
#'       \item \code{alpha_1}: Transmission parameter for mixing (beta distribution, range 0-1)
#'       \item \code{alpha_2}: Transmission parameter for density dependence (beta distribution, range 0-1)
#'       \item \code{decay_days_long}, \code{decay_days_short}: Natural immunity decay duration parameters (uniform distribution)
#'       \item \code{decay_shape_1}, \code{decay_shape_2}: Natural immunity decay shape parameters (uniform distribution)
#'       \item \code{epsilon}: Importation rate of infected individuals (lognormal distribution)
#'       \item \code{gamma_1}: Recovery rate for symptomatic infections (uniform distribution, 1/7 to 1/3 day⁻¹)
#'       \item \code{gamma_2}: Recovery rate for asymptomatic infections (uniform distribution, 1/14 to 1/7 day⁻¹)
#'       \item \code{iota}: Incubation rate (lognormal distribution)
#'       \item \code{kappa}: Environmental carrying capacity for V. cholerae (uniform distribution)
#'       \item \code{mobility_gamma}, \code{mobility_omega}: Human mobility model parameters (gamma distribution)
#'       \item \code{omega_1}: Vaccine-derived immunity waning rate for one dose OCV (gamma distribution)
#'       \item \code{omega_2}: Vaccine-derived immunity waning rate for two doses OCV (gamma distribution)
#'       \item \code{phi_1}: Vaccine effectiveness of one dose OCV (beta distribution)
#'       \item \code{phi_2}: Vaccine effectiveness of two doses OCV (beta distribution)
#'       \item \code{rho}: Reporting fraction/detection rate of symptomatic cases (beta distribution)
#'       \item \code{sigma}: Proportion of infections that are symptomatic (beta distribution)
#'       \item \code{zeta_1}: Bacterial shedding rate for symptomatic individuals (uniform distribution)
#'       \item \code{zeta_2}: Bacterial shedding rate for asymptomatic individuals (uniform distribution)
#'     }
#'   }
#'   \item{parameters_location}{List of 7 location-specific parameter types for 40 African countries (ISO3 codes):
#'     \itemize{
#'       \item \code{beta_j0_env}: Environmental base transmission rate (gamma distribution by country)
#'       \item \code{beta_j0_hum}: Human-to-human base transmission rate (gamma distribution by country)
#'       \item \code{tau_i}: Country-level travel probabilities (beta distribution with uncertainty adjustment)
#'       \item \code{a1}, \code{a2}: Seasonality Fourier coefficients for cases (normal distribution)
#'       \item \code{b1}, \code{b2}: Seasonality Fourier coefficients for deaths (normal distribution)
#'     }
#'   }
#' }
#'
#' @details
#' Each parameter entry contains:
#' \itemize{
#'   \item \code{parameter_name}: Name of the parameter
#'   \item \code{distribution}: Type of prior distribution (beta, gamma, normal, uniform, lognormal)
#'   \item \code{parameters}: Distribution-specific parameters (e.g., shape1/shape2 for beta, mean/sd for normal)
#' }
#'
#' Location-specific parameters include data for the following 40 African countries:
#' AGO, BDI, BEN, BFA, BWA, CAF, CIV, CMR, COD, COG, ERI, ETH, GAB, GHA, GIN,
#' GMB, GNB, GNQ, KEN, LBR, MLI, MOZ, MRT, MWI, NAM, NER, NGA, RWA, SEN, SLE,
#' SOM, SSD, SWZ, TCD, TGO, TZA, UGA, ZAF, ZMB, ZWE
#'
#' @usage
#' priors_default
#'
#' @seealso
#' * [config_default] – Default MOSAIC configuration that can use these priors
#' * [sample_parameters()] – Function that uses these priors for parameter sampling
#' * [config_simulation_epidemic] – One-year outbreak simulation configuration
#' * [config_simulation_endemic] – 5-year endemic simulation configuration
#'
#' @keywords datasets
"priors_default"
