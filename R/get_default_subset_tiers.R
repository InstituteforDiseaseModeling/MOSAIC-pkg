#' Get Default Subset Tiers for Post-Hoc Optimization
#'
#' Returns a structured list of tiered criteria for identifying optimal subsets
#' of calibration results. Uses a hybrid degradation strategy with 30 hardcoded tiers.
#'
#' @param target_ESS_best Numeric target ESS for the best subset (default: 500)
#' @param target_A Numeric target agreement index (default: 0.95)
#' @param target_CVw Numeric target coefficient of variation (default: 0.7)
#'
#' @return A named list where each element contains:
#'   - name: Tier identifier
#'   - A: Target agreement index (0-1)
#'   - CVw: Maximum coefficient of variation for weights
#'   - ESS_B: Target effective sample size
#'
#' @details
#' The function generates 30 hardcoded tiers with a hybrid degradation strategy:
#'
#' **Tiers 1-20 (ESS-constant):**
#' - ESS_B remains constant at target_ESS_best
#' - A degrades by 5% per tier (multiplied by 0.95)
#' - CVw increases by 5% per tier (multiplied by 1.05)
#' - Prioritizes statistical power while relaxing quality criteria
#'
#' **Tiers 21-30 (Fallback):**
#' - All three criteria degrade by 5% per tier
#' - ESS_B, A, CVw all relax simultaneously
#' - Graceful degradation when ESS target cannot be achieved
#'
#' **Example with defaults (target_ESS_best=500, target_A=0.95, target_CVw=0.7):**
#' ```
#' Tier  1: ESS=500, A=0.950, CVw=0.700  (Stringent)
#' Tier  5: ESS=500, A=0.773, CVw=0.817  (High quality)
#' Tier 10: ESS=500, A=0.599, CVw=0.931  (Moderate quality)
#' Tier 20: ESS=500, A=0.358, CVw=1.327  (Last ESS-constant)
#' Tier 21: ESS=475, A=0.340, CVw=1.393  (Fallback begins)
#' Tier 30: ESS=315, A=0.226, CVw=1.986  (Final fallback)
#' ```
#'
#' @examples
#' \dontrun{
#' # Get default 30 tiers
#' tiers <- get_default_subset_tiers(
#'   target_ESS_best = 500,
#'   target_A = 0.95,
#'   target_CVw = 0.7
#' )
#'
#' # Custom starting criteria
#' tiers_custom <- get_default_subset_tiers(
#'   target_ESS_best = 600,
#'   target_A = 0.90,
#'   target_CVw = 0.75
#' )
#'
#' # Loop through tiers in calibration with grid search
#' for (tier_name in names(tiers)) {
#'   tier <- tiers[[tier_name]]
#'   result <- grid_search_best_subset(
#'     results = results,
#'     target_ESS = tier$ESS_B,
#'     target_A = tier$A,
#'     target_CVw = tier$CVw,
#'     min_size = 30,
#'     max_size = 1000
#'   )
#'   if (result$converged) break
#' }
#' }
#'
#' @export
get_default_subset_tiers <- function(
  target_ESS_best = 500,
  target_A = 0.95,
  target_CVw = 0.7
) {

  # Hardcoded tier configuration
  n_tiers_ess_constant <- 20  # Tiers with constant ESS (relax A/CVw only)
  n_tiers_fallback <- 10       # Fallback tiers (degrade all metrics)
  total_tiers <- n_tiers_ess_constant + n_tiers_fallback  # 30 total

  # Degradation factors (5% per tier, more gradual than previous 10%)
  a_decay <- 0.95      # A degrades by 5% each tier
  cvw_growth <- 1.05   # CVw increases by 5% each tier
  ess_decay <- 0.95    # ESS degrades by 5% each tier (fallback only)

  tiers <- list()

  # Phase 1: ESS-constant tiers (1-20)
  # Prioritize statistical power by maintaining ESS while relaxing quality
  for (i in 1:n_tiers_ess_constant) {
    tier_name <- paste0("tier_", i)

    # ESS remains constant
    current_ESS <- target_ESS_best

    # A and CVw degrade
    current_A <- target_A * (a_decay ^ (i - 1))
    current_CVw <- target_CVw * (cvw_growth ^ (i - 1))

    tiers[[tier_name]] <- list(
      name = tier_name,
      A = current_A,
      CVw = current_CVw,
      ESS_B = current_ESS
    )
  }

  # Phase 2: Fallback tiers (21-30)
  # All three criteria degrade when ESS target cannot be achieved
  for (i in 1:n_tiers_fallback) {
    tier_idx <- n_tiers_ess_constant + i
    tier_name <- paste0("tier_", tier_idx)

    # Continue degradation from where phase 1 left off
    # A and CVw continue their degradation trajectory
    current_A <- target_A * (a_decay ^ (n_tiers_ess_constant + i - 1))
    current_CVw <- target_CVw * (cvw_growth ^ (n_tiers_ess_constant + i - 1))

    # ESS starts degrading from target_ESS_best
    current_ESS <- target_ESS_best * (ess_decay ^ i)

    tiers[[tier_name]] <- list(
      name = tier_name,
      A = current_A,
      CVw = current_CVw,
      ESS_B = current_ESS
    )
  }

  return(tiers)
}
