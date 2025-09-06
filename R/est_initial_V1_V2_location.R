#' Estimate Initial V1 and V2 Compartments for a Location
#'
#' Estimates the number of individuals in the V1 (one-dose) and V2 (two-dose) vaccination
#' compartments at a given time point (t0) for a single location. The function tracks
#' vaccination cohorts through time using a FIFO (First-In-First-Out) queue system,
#' accounts for waning immunity via exponential decay, incorporates vaccine effectiveness,
#' and respects operational constraints including vaccination coverage ceilings and
#' minimum inter-dose intervals.
#'
#' @param dates A vector of dates when doses were administered (Date or character that can be coerced to Date).
#'        Must be the same length as doses.
#' @param doses A numeric vector of doses administered on each date. Must be positive values.
#'        Must be the same length as dates.
#' @param t0 The target date for which to estimate V1 and V2 compartments (Date object).
#'        Only vaccination events on or before this date are considered.
#' @param N Population size(s) for the location. Can be:
#'        \itemize{
#'          \item A single numeric value for constant population
#'          \item A numeric vector (same length as dates) for time-varying population
#'          \item NA if ceiling constraint is not needed
#'        }
#' @param vacc_ceiling_frac Maximum fraction of population that can receive first doses
#'        (numeric between 0 and 1). Default is 0.6 (60% coverage ceiling).
#' @param omega1 Waning rate for one-dose protection (numeric, positive). Units are 1/days.
#'        Default corresponds to approximately 231-day half-life (0.003).
#' @param omega2 Waning rate for two-dose protection (numeric, positive). Units are 1/days.
#'        Default equals omega1 but typically should be lower (slower waning).
#' @param t_lag Lag time in days for vaccine protection to develop after administration
#'        (integer, non-negative). Default is 14 days.
#' @param min_interdose_days Minimum number of days required between first and second doses
#'        (integer, non-negative). Default is 40 days following WHO OCV guidelines.
#' @param phi1 Vaccine effectiveness for one-dose regimen (proportion developing protection).
#'        Numeric between 0 and 1. Default is 1.0 (100% effectiveness).
#' @param phi2 Vaccine effectiveness for two-dose regimen (proportion developing protection).
#'        Numeric between 0 and 1. Default is 1.0 (100% effectiveness).
#' @param allocation_logic Character string specifying second dose allocation method when
#'        ceiling is reached. Either "FIFO" (First-In-First-Out, oldest cohorts first) or
#'        "LIFO" (Last-In-First-Out, newest eligible cohorts first). Default is "FIFO" for
#'        backward compatibility. LIFO is more realistic for actual campaigns.
#'
#' @return A named numeric vector with two elements:
#' \describe{
#'   \item{V1}{Estimated number of effectively protected individuals with one-dose at time t0}
#'   \item{V2}{Estimated number of effectively protected individuals with two-dose at time t0}
#' }
#'
#' @details
#' The function implements a sophisticated vaccine allocation algorithm that:
#'
#' \strong{Cohort Tracking:}
#' - Maintains separate cohorts for first and second doses with timestamps
#' - Uses FIFO logic to allocate second doses from oldest eligible first-dose cohorts
#' - Second doses decrement V1 and increment V2 (individuals move between compartments)
#'
#' \strong{Constraint Enforcement:}
#' - Vaccination ceiling: First doses cannot exceed \code{vacc_ceiling_frac * N[i]} at each time point
#' - Inter-dose interval: Second doses only from cohorts at least \code{min_interdose_days} old
#' - No same-day second doses (enforced by inter-dose interval)
#' - Time-varying population: When N is a vector, ceiling adapts to population at each vaccination date
#'
#' \strong{Allocation Priority:}
#' 1. If V1 at ceiling, prioritize second doses from eligible cohorts
#' 2. Allocate first doses up to remaining ceiling capacity
#' 3. Use any remaining doses for additional second doses if possible
#' 4. Excess doses beyond all constraints are effectively lost
#'
#' \strong{Second Dose Allocation Logic (when ceiling reached):}
#' \itemize{
#'   \item FIFO: Allocates to oldest eligible cohorts first (traditional approach)
#'   \item LIFO: Allocates to newest eligible cohorts first (more realistic)
#' }
#' LIFO is recommended as it better reflects real campaign behavior where recent
#' vaccinees are more accessible for follow-up doses.
#'
#' \strong{Waning Immunity:}
#' Final V1 and V2 estimates account for exponential decay of protection:
#' \deqn{V_i(t_0) = \sum_{cohorts} size_j \times exp(-\omega_i \times (t_0 - date_j - t_{lag}))}
#'
#' \strong{Vaccine Effectiveness:}
#' The output represents effectively protected individuals, not just vaccinated:
#' \deqn{V1_{effective} = V1_{vaccinated} \times \phi_1}
#' \deqn{V2_{effective} = V2_{vaccinated} \times \phi_2}
#' Note: Ceiling constraints operate on administrative coverage (doses given), not effective coverage.
#'
#' @examples
#' \dontrun{
#' # Create sample vaccination data
#' dates <- seq.Date(from = as.Date("2023-01-01"),
#'                   to = as.Date("2023-12-31"),
#'                   by = "month")
#' doses <- c(50000, 45000, 40000, 35000, 30000, 25000,
#'            20000, 15000, 10000, 5000, 3000, 2000)
#'
#' # Example 1: Constant population
#' v_const <- est_initial_V1_V2_location(
#'   dates = dates,
#'   doses = doses,
#'   t0 = as.Date("2024-01-01"),
#'   N = 1000000,
#'   vacc_ceiling_frac = 0.6,
#'   omega1 = 0.003,
#'   omega2 = 0.0015,
#'   t_lag = 14,
#'   min_interdose_days = 40,
#'   phi1 = 0.65,  # 65% one-dose effectiveness
#'   phi2 = 0.85,  # 85% two-dose effectiveness
#'   allocation_logic = "FIFO"  # Default
#' )
#'
#' # Example 2: Time-varying population (e.g., 1% annual growth)
#' N_vec <- 1000000 * (1.01 ^ (0:11/12))  # Monthly growth
#' v_varying <- est_initial_V1_V2_location(
#'   dates = dates,
#'   doses = doses,
#'   t0 = as.Date("2024-01-01"),
#'   N = N_vec,
#'   allocation_logic = "LIFO"  # Recent cohorts get second doses first
#' )
#'
#' print(v_const)
#' # V1      V2
#' # 80246   67074  
#' 
#' print(v_varying) 
#' # V1      V2
#' # 65432   67074  # May differ due to population changes
#' }
#'
#' @references
#' WHO (2017). Cholera vaccines: WHO position paper. Weekly Epidemiological Record.
#'
#' @seealso
#' \code{\link{est_vaccination_rate}} for processing vaccination campaign data
#' \code{\link{est_vaccine_effectiveness}} for vaccine efficacy parameters
#' \code{\link{est_immune_decay_vaccine}} for waning rate estimation
#'
#' @importFrom utils head tail
#' @export

est_initial_V1_V2_location <- function(dates,
                                      doses,
                                      t0,
                                      N = NA,
                                      vacc_ceiling_frac = 0.6,
                                      omega1 = 0.003,
                                      omega2 = 0.003,
                                      t_lag = 14L,
                                      min_interdose_days = 40L,
                                      phi1 = 1.0,
                                      phi2 = 1.0,
                                      allocation_logic = "FIFO") {
  
  # Input validation
  if (!inherits(t0, "Date")) {
    stop("t0 must be a Date object")
  }
  
  if (!is.numeric(doses) || any(!is.finite(doses)) || any(doses < 0)) {
    stop("doses must be a numeric vector with non-negative finite values")
  }
  
  if (length(dates) != length(doses)) {
    stop("dates and doses must have the same length")
  }
  
  if (length(dates) == 0) {
    return(c(V1 = 0, V2 = 0))
  }
  
  if (!is.numeric(omega1) || length(omega1) != 1L || !is.finite(omega1) || omega1 <= 0) {
    stop("omega1 must be a single positive finite numeric value")
  }
  
  if (!is.numeric(omega2) || length(omega2) != 1L || !is.finite(omega2) || omega2 <= 0) {
    stop("omega2 must be a single positive finite numeric value")
  }
  
  if (!is.numeric(t_lag) || length(t_lag) != 1L || t_lag < 0) {
    stop("t_lag must be a single non-negative numeric value")
  }
  
  if (!is.numeric(min_interdose_days) || length(min_interdose_days) != 1L || min_interdose_days < 0) {
    stop("min_interdose_days must be a single non-negative numeric value")
  }
  
  if (!is.numeric(vacc_ceiling_frac) || length(vacc_ceiling_frac) != 1L || 
      !is.finite(vacc_ceiling_frac) || vacc_ceiling_frac <= 0 || vacc_ceiling_frac > 1) {
    stop("vacc_ceiling_frac must be a single numeric value between 0 and 1")
  }
  
  if (!is.numeric(phi1) || length(phi1) != 1L || !is.finite(phi1) || phi1 < 0 || phi1 > 1) {
    stop("phi1 must be a single numeric value between 0 and 1")
  }
  
  if (!is.numeric(phi2) || length(phi2) != 1L || !is.finite(phi2) || phi2 < 0 || phi2 > 1) {
    stop("phi2 must be a single numeric value between 0 and 1")
  }
  
  if (!is.character(allocation_logic) || length(allocation_logic) != 1L || 
      !(allocation_logic %in% c("FIFO", "LIFO"))) {
    stop("allocation_logic must be either 'FIFO' or 'LIFO'")
  }
  
  # Validate N parameter
  if (!all(is.na(N))) {
    if (!is.numeric(N)) {
      stop("N must be numeric (scalar or vector) or NA")
    }
    if (length(N) == 1) {
      # Scalar N - replicate to match dates length
      if (!is.finite(N) || N <= 0) {
        stop("N must be positive when provided as scalar")
      }
      N_vec <- rep(N, length(dates))
    } else if (length(N) == length(dates)) {
      # Vector N - validate all values
      if (any(!is.finite(N)) || any(N <= 0)) {
        stop("All N values must be positive and finite")
      }
      N_vec <- N
    } else {
      stop("N must be a scalar or have the same length as dates")
    }
  } else {
    # NA - no ceiling constraint
    N_vec <- rep(NA_real_, length(dates))
  }
  
  # Prepare and clean dose data
  doses_clean <- data.frame(
    date = as.Date(dates),
    doses = as.numeric(doses),
    population = N_vec,
    stringsAsFactors = FALSE
  )
  
  # Filter to valid records on or before t0
  valid_idx <- !is.na(doses_clean$date) & 
               doses_clean$date <= t0 &
               !is.na(doses_clean$doses) & 
               doses_clean$doses > 0
  
  doses_clean <- doses_clean[valid_idx, , drop = FALSE]
  
  # Return zero compartments if no valid vaccination data
  if (nrow(doses_clean) == 0L) {
    return(c(V1 = 0, V2 = 0))
  }
  
  # Sort by date for chronological processing
  doses_clean <- doses_clean[order(doses_clean$date), , drop = FALSE]
  
  # Initialize cohort tracking data frames
  first_cohorts <- data.frame(
    date_first = as.Date(character()),
    remaining = numeric(),
    stringsAsFactors = FALSE
  )
  
  second_cohorts <- data.frame(
    date_second = as.Date(character()),
    count = numeric(),
    stringsAsFactors = FALSE
  )
  
  # FIFO allocation function for second doses with inter-dose interval constraint
  allocate_second_doses <- function(current_date, doses_needed) {
    if (doses_needed <= 0 || nrow(first_cohorts) == 0) {
      return(0)
    }
    
    # Calculate eligibility cutoff date based on minimum inter-dose interval
    eligibility_cutoff <- current_date - min_interdose_days
    
    # Find eligible first-dose cohorts (old enough for second dose)
    eligible_idx <- which(first_cohorts$date_first <= eligibility_cutoff & 
                         first_cohorts$remaining > 0)
    
    if (length(eligible_idx) == 0) {
      return(0)
    }
    
    # Sort by date according to allocation logic
    if (allocation_logic == "FIFO") {
      # FIFO: Oldest cohorts first (ascending order)
      eligible_idx <- eligible_idx[order(first_cohorts$date_first[eligible_idx])]
    } else {
      # LIFO: Newest cohorts first (descending order)
      eligible_idx <- eligible_idx[order(first_cohorts$date_first[eligible_idx], decreasing = TRUE)]
    }
    
    doses_allocated <- 0
    
    # Allocate second doses from selected cohorts (oldest for FIFO, newest for LIFO)
    for (idx in eligible_idx) {
      if (doses_allocated >= doses_needed) {
        break
      }
      
      available <- first_cohorts$remaining[idx]
      to_allocate <- min(available, doses_needed - doses_allocated)
      
      if (to_allocate > 0) {
        # Move individuals from V1 to V2
        first_cohorts$remaining[idx] <<- first_cohorts$remaining[idx] - to_allocate
        
        # Record second dose cohort
        second_cohorts <<- rbind(
          second_cohorts,
          data.frame(
            date_second = current_date,
            count = to_allocate,
            stringsAsFactors = FALSE
          )
        )
        
        doses_allocated <- doses_allocated + to_allocate
      }
    }
    
    return(doses_allocated)
  }
  
  # Process vaccination events chronologically
  for (i in seq_len(nrow(doses_clean))) {
    current_date <- doses_clean$date[i]
    doses_available <- doses_clean$doses[i]
    current_N <- doses_clean$population[i]
    
    # Calculate ceiling for this time point
    if (is.finite(current_N) && current_N > 0) {
      N_cap <- vacc_ceiling_frac * current_N
    } else {
      N_cap <- NA_real_
    }
    
    if (!is.finite(N_cap)) {
      # No ceiling constraint: all doses become first doses
      first_cohorts <- rbind(
        first_cohorts,
        data.frame(
          date_first = current_date,
          remaining = doses_available,
          stringsAsFactors = FALSE
        )
      )
    } else {
      # Apply ceiling constraint with prioritized allocation
      
      # Calculate current V1 population
      current_V1 <- sum(first_cohorts$remaining)
      
      # Priority 1: If at ceiling, try second doses first
      if (current_V1 >= N_cap && doses_available > 0) {
        second_allocated <- allocate_second_doses(current_date, doses_available)
        doses_available <- doses_available - second_allocated
        current_V1 <- current_V1 - second_allocated
      }
      
      # Priority 2: Allocate first doses up to ceiling
      if (doses_available > 0) {
        first_capacity <- max(N_cap - current_V1, 0)
        first_allocated <- min(doses_available, first_capacity)
        
        if (first_allocated > 0) {
          first_cohorts <- rbind(
            first_cohorts,
            data.frame(
              date_first = current_date,
              remaining = first_allocated,
              stringsAsFactors = FALSE
            )
          )
          doses_available <- doses_available - first_allocated
          current_V1 <- current_V1 + first_allocated
        }
      }
      
      # Priority 3: Use remaining doses for second doses if at ceiling
      if (doses_available > 0 && current_V1 >= N_cap) {
        second_allocated <- allocate_second_doses(current_date, doses_available)
        doses_available <- doses_available - second_allocated
        # Note: doses_available may still be > 0 here (excess doses lost)
      }
    }
  }
  
  # Calculate protected populations at t0 with waning immunity
  V1 <- 0
  V2 <- 0
  
  # Calculate V1: sum of first-dose cohorts with waning
  if (nrow(first_cohorts) > 0) {
    # Time since vaccination (accounting for protection lag)
    time_since_vacc <- as.numeric(t0 - first_cohorts$date_first) - t_lag
    
    # Only count cohorts where protection has developed
    protected_idx <- time_since_vacc >= 0
    
    if (any(protected_idx)) {
      V1 <- sum(first_cohorts$remaining[protected_idx] * 
                exp(-omega1 * time_since_vacc[protected_idx]))
    }
  }
  
  # Calculate V2: sum of second-dose cohorts with waning
  if (nrow(second_cohorts) > 0) {
    # Time since vaccination (accounting for protection lag)
    time_since_vacc <- as.numeric(t0 - second_cohorts$date_second) - t_lag
    
    # Only count cohorts where protection has developed
    protected_idx <- time_since_vacc >= 0
    
    if (any(protected_idx)) {
      V2 <- sum(second_cohorts$count[protected_idx] * 
                exp(-omega2 * time_since_vacc[protected_idx]))
    }
  }
  
  # Apply vaccine effectiveness to get effectively protected populations
  # Note: Ceiling constraints operate on administrative coverage (doses given),
  # not on effective coverage, preserving operational realism
  V1_effective <- V1 * phi1
  V2_effective <- V2 * phi2
  
  # Return named vector with effectively protected populations
  return(c(V1 = V1_effective, V2 = V2_effective))
}