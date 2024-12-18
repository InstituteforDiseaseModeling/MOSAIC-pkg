% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_param_yaml.R
\name{make_param_yaml}
\alias{make_param_yaml}
\title{Create a YAML File for MOSAIC Model Parameters}
\usage{
make_param_yaml(
  output_file_path = NULL,
  date_start = NULL,
  date_stop = NULL,
  location_id = NULL,
  location_name = NULL,
  N_j_initial = NULL,
  S_j_initial = NULL,
  I_j_initial = NULL,
  R_j_initial = NULL,
  b_j = NULL,
  d_j = NULL,
  nu_jt = NULL,
  phi = NULL,
  omega = NULL,
  epsilon = NULL,
  gamma = NULL,
  mu = NULL,
  rho = NULL,
  sigma = NULL,
  beta_j0_hum = NULL,
  beta_j_seasonality = NULL,
  tau_i = NULL,
  pi_ij = NULL,
  alpha = NULL,
  beta_j0_env = NULL,
  theta_j = NULL,
  psi_jt = NULL,
  zeta = NULL,
  kappa = NULL,
  delta_min = NULL,
  delta_max = NULL
)
}
\arguments{
\item{output_file_path}{A character string representing the full file path of the output YAML file (e.g., 'path/to/parameters.yaml'). If \code{NULL}, no file is written.
\subsection{Initialization}{
}}

\item{date_start}{Start date for the simulation period in "YYYY-MM-DD" format.}

\item{date_stop}{End date for the simulation period in "YYYY-MM-DD" format.}

\item{location_id}{A vector of integers giving the numerical index for metapopulations.}

\item{location_name}{A character vector giving the names of each metapopulation location, matching the length of \code{location_id}.}

\item{N_j_initial}{An integer vector of length \code{location_id} that gives the total initial population size of each j location.}

\item{S_j_initial}{An integer vector of length \code{location_id} that gives the starting values for the number of susceptible individuals in each j location.}

\item{I_j_initial}{An integer vector of length \code{location_id} that gives the starting values for the number of infected individuals in each j location.}

\item{R_j_initial}{An integer vector of length \code{location_id} that gives the starting values for the number of recovered individuals in each j location.
\subsection{Demographics}{
}}

\item{b_j}{Birth rate of population j. Must be numeric, non-negative, and the same length as \code{location_id}.}

\item{d_j}{Mortality rate of population j. Must be numeric, non-negative, and the same length as \code{location_id}.
\subsection{Immune Dynamics}{
}}

\item{nu_jt}{A matrix of vaccination rates for each location (\code{j}) and time step (\code{t}). Must have rows equal to \code{length(location_id)} and columns equal to the daily sequence from \code{date_start} to \code{date_stop}.}

\item{phi}{Effectiveness of Oral Cholera Vaccine (OCV). Must be numeric and within the range \link{0, 1}.}

\item{omega}{Waning immunity rate of vaccinated individuals. Must be a numeric scalar greater than or equal to zero.}

\item{epsilon}{Waning immunity rate of recovered individuals. Must be a numeric scalar greater than or equal to zero.}

\item{gamma}{Recovery rate of infected individuals. Must be a numeric scalar greater than or equal to zero.
\subsection{Observation Processes}{
}}

\item{mu}{Mortality rate due to V. cholerae infection. Must be a numeric scalar greater than or equal to zero.}

\item{rho}{Proportion of suspected cholera cases that are true infections. Must be a numeric scalar between 0 and 1.}

\item{sigma}{Proportion of symptomatic V. cholerae infections. Must be a numeric scalar between 0 and 1.
\subsection{Force of Infection (human-to-human)}{
}}

\item{beta_j0_hum}{Baseline human-to-human transmission rate for each location. Must be a numeric vector of length equal to \code{location_id} and values greater than or equal to zero.}

\item{beta_j_seasonality}{The seasonal derivation from mean transmission (beta_j0_hum) for each j location. The seasonality term is given in the Z-score indicating the number of standard deviations
away from the mean rate of transmission. Length should be 365 for each day of the year. Values are estimated in the \code{est_seasonal_dynamics()} function.}

\item{tau_i}{Departure probability for each origin location. Must be a numeric vector of length equal to \code{location_id} and values between 0 and 1.}

\item{pi_ij}{A matrix of travel probabilities between origin and destination locations. Must have dimensions equal to \code{length(location_id)} x \code{length(location_id)} and values between 0 and 1.}

\item{alpha}{Population mixing parameter. Must be a numeric scalar between 0 and 1.
\subsection{Force of Infection (environment-to-human)}{
}}

\item{beta_j0_env}{Baseline environment-to-human transmission rate for each location. Must be a numeric vector of length equal to \code{location_id} and values greater than or equal to zero.}

\item{theta_j}{Proportion of the population with adequate Water, Sanitation, and Hygiene (WASH). Must be a numeric vector of length equal to \code{location_id} and values between 0 and 1.}

\item{psi_jt}{A 2D matrix of environmental suitability values for V. cholerae. Must have rows equal to \code{length(location_id)} and columns equal to the daily sequence from \code{date_start} to \code{date_stop}, with values between 0 and 1.}

\item{zeta}{Shedding rate of V. cholerae into the environment. Must be a numeric scalar greater than zero.}

\item{kappa}{Concentration of V. cholerae required for 50\% infection probability. Must be a numeric scalar greater than zero.}

\item{delta_min}{Minimum environmental decay rate of V. cholerae. Must be a numeric scalar greater than zero and less than \code{delta_max} (indicating a faster decay rate).}

\item{delta_max}{Maximum environmental decay rate of V. cholerae.}
}
\value{
Returns the validated list of parameters. If \code{output_file_path} is provided, the parameters are also written to a YAML file.
}
\description{
This function generates a YAML file for MOSAIC model simulation parameters or returns the parameters as a list if no file output is specified.
It validates all input parameters to ensure they meet the required constraints for the MOSAIC simulation framework. The YAML file output is designed
for use with the MOSAIC R package and ensures compatibility with downstream Python-based tools such as the Light-agent Spatial Model for ERadication (LASER) platform.
}
\examples{
make_param_yaml(
     output_file_path = "parameters.yaml",
     date_start = "2024-12-01",
     date_stop = "2024-12-31",
     location_id = 1:2,
     location_name = c("Location A", "Location B"),
     N_j_initial = as.integer(c(1000, 1000)),
     S_j_initial = as.integer(c(999, 999)),
     I_j_initial = as.integer(c(1, 1)),
     R_j_initial = as.integer(c(0, 0)),
     b_j = c(0.01, 0.02),
     d_j = c(0.01, 0.02),
     nu_jt = matrix(data = 0, nrow = 2, ncol = 31),
     phi = 0.8,
     omega = 0.1,
     epsilon = 0.05,
     gamma = 0.2,
     mu = 0.01,
     rho = 0.9,
     sigma = 0.5,
     beta_j0_hum = c(0.05, 0.03),
     beta_j_seasonality = rep(0, 365),
     tau_i = c(0.1, 0.2),
     pi_ij = matrix(c(0.8, 0.2, 0.2, 0.8), nrow = 2),
     alpha = 0.95,
     beta_j0_env = c(0.02, 0.04),
     theta_j = c(0.6, 0.7),
     psi_jt = matrix(data = 0, nrow = 2, ncol = 31),
     zeta = 0.5,
     kappa = 10^5,
     delta_min = 0.001,
     delta_max = 0.01
)

}
