#' Calculate deaths from symptomatic infections with threshold-dependent IFR
#'
#' @description
#' Maps symptomatic infections to deaths using an infection fatality ratio (IFR)
#' that increases during epidemic periods when incidence exceeds a threshold.
#' The IFR can also include a temporal trend component. Deaths can be delayed
#' relative to infections to account for the time between symptom onset and death.
#'
#' @param infections Numeric vector of new symptomatic infections over time for one location
#' @param N Scalar or vector of population size (can be time-varying)
#' @param mu_baseline Scalar baseline IFR for this location (between 0 and 1)
#' @param mu_slope Scalar temporal slope factor (default 0 for no trend)
#' @param mu_epidemic_factor Scalar proportional increase during epidemic (e.g., 0.5 = 50% increase)
#' @param epidemic_threshold Scalar location-specific threshold (infections/N) for epidemic definition
#' @param delta_t Non-negative integer number of days for the infection-to-death delay (default 0)
#'
#' @return Numeric vector of deaths at each time step
#'
#' @details
#' The time-varying IFR is calculated as:
#' \deqn{\mu_t = \mu_{baseline} \times (1 + \mu_{slope} \times t) \times (1 + \mu_{epidemic} \times \mathbb{1}_{epidemic})}
#'
#' where \eqn{\mathbb{1}_{epidemic}} is 1 when incidence rate exceeds the threshold, 0 otherwise.
#'
#' When \code{delta_t > 0}, deaths at time \code{t} result from infections at time \code{t - delta_t}.
#' The first \code{delta_t} elements of the output will be \code{NA} since no infection data
#' is available before the start of the time series.
#'
#' @examples
#' # Simulate an outbreak
#' infections <- c(10, 20, 50, 100, 80, 40, 20, 10)
#' deaths <- calc_deaths_from_infections(
#'   infections = infections,
#'   N = 100000,
#'   mu_baseline = 0.01,
#'   mu_slope = 0,
#'   mu_epidemic_factor = 0.5,
#'   epidemic_threshold = 30/100000,
#'   delta_t = 0
#' )
#'
#' # With temporal trend
#' deaths_trend <- calc_deaths_from_infections(
#'   infections = infections,
#'   N = 100000,
#'   mu_baseline = 0.01,
#'   mu_slope = 0.1,  # 10% increase over time period
#'   mu_epidemic_factor = 0.5,
#'   epidemic_threshold = 30/100000,
#'   delta_t = 0
#' )
#'
#' # With 3-day infection-to-death delay
#' deaths_delayed <- calc_deaths_from_infections(
#'   infections = infections,
#'   N = 100000,
#'   mu_baseline = 0.01,
#'   mu_slope = 0,
#'   mu_epidemic_factor = 0.5,
#'   epidemic_threshold = 30/100000,
#'   delta_t = 3
#' )
#'
#' @export
calc_deaths_from_infections <- function(infections,
                                       N,
                                       mu_baseline,
                                       mu_slope = 0,
                                       mu_epidemic_factor,
                                       epidemic_threshold,
                                       delta_t = 0) {

  # Input validation
  if (!is.numeric(infections) || !is.vector(infections)) {
    stop("`infections` must be a numeric vector.", call. = FALSE)
  }

  n_time <- length(infections)

  if (n_time == 0) {
    return(numeric(0))
  }

  if (!is.numeric(N) || length(N) < 1) {
    stop("`N` must be a numeric scalar or vector.", call. = FALSE)
  }

  # Expand N to vector if scalar
  if (length(N) == 1) {
    N_vec <- rep(N, n_time)
  } else if (length(N) == n_time) {
    N_vec <- N
  } else {
    stop("`N` must be a scalar or have same length as `infections`.", call. = FALSE)
  }

  # Check for invalid N values
  if (any(N_vec <= 0, na.rm = TRUE)) {
    stop("`N` must be positive.", call. = FALSE)
  }

  # Validate other parameters
  if (!is.numeric(mu_baseline) || length(mu_baseline) != 1) {
    stop("`mu_baseline` must be a numeric scalar.", call. = FALSE)
  }

  if (is.na(mu_baseline) || mu_baseline < 0 || mu_baseline > 1) {
    stop("`mu_baseline` must be between 0 and 1.", call. = FALSE)
  }

  if (!is.numeric(mu_slope) || length(mu_slope) != 1) {
    stop("`mu_slope` must be a numeric scalar.", call. = FALSE)
  }

  if (is.na(mu_slope)) {
    stop("`mu_slope` cannot be NA.", call. = FALSE)
  }

  if (!is.numeric(mu_epidemic_factor) || length(mu_epidemic_factor) != 1) {
    stop("`mu_epidemic_factor` must be a numeric scalar.", call. = FALSE)
  }

  if (is.na(mu_epidemic_factor) || mu_epidemic_factor < 0) {
    stop("`mu_epidemic_factor` must be non-negative.", call. = FALSE)
  }

  if (!is.numeric(epidemic_threshold) || length(epidemic_threshold) != 1) {
    stop("`epidemic_threshold` must be a numeric scalar.", call. = FALSE)
  }

  if (is.na(epidemic_threshold) || epidemic_threshold < 0) {
    stop("`epidemic_threshold` must be non-negative.", call. = FALSE)
  }

  # Validate delta_t
  if (!is.numeric(delta_t) || length(delta_t) != 1 || is.na(delta_t) ||
      delta_t < 0 || delta_t != floor(delta_t)) {
    stop("`delta_t` must be a non-negative integer.", call. = FALSE)
  }

  # Initialize output
  deaths <- rep(NA_real_, n_time)

  # Map infections at (t - delta_t) to deaths at t
  k <- as.integer(delta_t)

  if (k >= n_time) {
    # If delay is >= time series length, all deaths are NA
    return(deaths)
  }

  if (k == 0) {
    # No delay - original implementation
    # Handle NA infections
    valid_idx <- !is.na(infections)

    if (!any(valid_idx)) {
      return(deaths)  # All NA
    }

    # Calculate incidence rate (infections per capita) for valid indices
    incidence_rate <- rep(0, n_time)
    incidence_rate[valid_idx] <- infections[valid_idx] / N_vec[valid_idx]

    # Determine epidemic periods (binary flag)
    epidemic_flag <- as.numeric(incidence_rate > epidemic_threshold)

    # Calculate temporal trend factor
    if (mu_slope != 0) {
      time_factor <- (seq_len(n_time) - 1) / max(1, n_time - 1)
    } else {
      time_factor <- rep(0, n_time)
    }

    # Calculate time-varying IFR
    mu_t <- mu_baseline * (1 + mu_slope * time_factor) * (1 + mu_epidemic_factor * epidemic_flag)

    # Ensure IFR stays within [0, 1]
    mu_t <- pmin(pmax(mu_t, 0), 1)

    # Calculate deaths only for valid indices
    deaths[valid_idx] <- mu_t[valid_idx] * infections[valid_idx]

  } else {
    # With delay: deaths at t result from infections at t - delta_t
    idx_death <- seq.int(from = k + 1L, to = n_time)
    idx_infect <- seq.int(from = 1L, to = n_time - k)

    # Only process non-NA infections
    valid_idx <- !is.na(infections[idx_infect])

    if (any(valid_idx)) {
      # Calculate incidence rate for infection days
      incidence_rate <- infections[idx_infect] / N_vec[idx_infect]

      # Determine epidemic periods based on infection day
      epidemic_flag <- as.numeric(incidence_rate > epidemic_threshold)

      # Calculate temporal trend factor for death days
      if (mu_slope != 0) {
        time_factor <- (idx_death - 1) / max(1, n_time - 1)
      } else {
        time_factor <- rep(0, length(idx_death))
      }

      # Calculate time-varying IFR
      mu_t <- mu_baseline * (1 + mu_slope * time_factor) * (1 + mu_epidemic_factor * epidemic_flag)

      # Ensure IFR stays within [0, 1]
      mu_t <- pmin(pmax(mu_t, 0), 1)

      # Calculate deaths
      deaths[idx_death] <- mu_t * infections[idx_infect]

      # Set NA for invalid infection indices
      deaths[idx_death[!valid_idx]] <- NA_real_
    }
  }

  return(deaths)
}
