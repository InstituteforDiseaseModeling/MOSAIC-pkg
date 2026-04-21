#' Estimate V1/V2 Initial-Condition Beta Priors from OCV Campaign History
#'
#' Builds country-specific Beta priors for \code{prop_V1_initial} and
#' \code{prop_V2_initial} by reading the raw GTFCC OCV request log,
#' classifying doses by regimen (single-dose Euvichol-S vs. two-dose
#' Shanchol/Euvichol/Euvichol+), pairing rounds within each campaign,
#' applying \code{omega_1} / \code{omega_2} waning from administration
#' date to simulation start, and moment-matching the resulting country-level
#' proportions to Beta distributions.
#'
#' @param PATHS A list of paths (as returned by \code{\link{get_paths}}).
#'   Used to locate \code{ees-cholera-mapping/data/cholera/epicentre/gtfcc/cholera_vacc_requests.csv}
#'   via \code{PATHS$ROOT}.
#' @param config A MOSAIC config list (e.g. \code{config_default}).
#'   Used for \code{location_name}, \code{N_j_initial}, and \code{date_start}.
#' @param date_start Simulation start date (Date or character \code{"YYYY-MM-DD"}).
#'   Defaults to \code{config$date_start}. Only campaigns delivered strictly
#'   before this date contribute to V1/V2 initial conditions.
#' @param cv Coefficient of variation for the Beta prior (natural scale).
#'   Default \code{0.40}. Tighter values produce narrower priors; looser values
#'   permit more data-driven posterior movement.
#' @param omega_1 One-dose waning rate (per day). Defaults to the natural-scale
#'   mean of \code{priors_default$parameters_global$omega_1}.
#' @param omega_2 Two-dose waning rate (per day). Defaults to the natural-scale
#'   mean of \code{priors_default$parameters_global$omega_2}.
#' @param t_lag Protection onset lag (days). Waning starts at
#'   \code{event_date + t_lag}. Default 14.
#' @param vacc_ceiling_frac Hard ceiling on combined V1+V2 coverage (fraction
#'   of population). Default 0.70 (v0.28.7; was 0.60 in v0.28.6). If the waned
#'   dose sum exceeds this, V1 and V2 are scaled proportionally. Rationale for
#'   0.70: published OCV campaigns routinely reach 65-80\% coverage
#'   (Abubakar et al. 2018 DRC 65\%; Qadri et al. 2015 BGD 75\%; Luquero et al.
#'   2014 Haiti 80\%+). A lower ceiling under-captures real emergency responses.
#' @param fallback_shape1_V1,fallback_shape2_V1,fallback_shape1_V2,fallback_shape2_V2
#'   Beta shape parameters used as a fallback when a country has no OCV history
#'   in the CSV. Defaults match the pre-v0.22.11 uninformative priors
#'   (\code{Beta(0.5, 49.5)} for V1, \code{Beta(0.5, 99.5)} for V2).
#' @param verbose Logical; emit per-country diagnostics. Default TRUE.
#'
#' @return A list with the same nested structure as
#'   \code{priors_default$parameters_location}:
#'   \code{$prop_V1_initial$location[[iso]]} and
#'   \code{$prop_V2_initial$location[[iso]]} each carry
#'   \code{list(distribution = "beta", parameters = list(shape1, shape2))}.
#'
#' @details
#' Biological notes:
#' \itemize{
#'   \item V1/V2 in the MOSAIC/LASER model are \emph{administrative} compartments
#'     (dose received). LASER's \code{vaccinated.py} independently splits the
#'     initial counts into immune (V\emph{k}imm) and susceptible (V\emph{k}sus)
#'     substates via \code{phi_1}/\code{phi_2}. This function therefore does NOT
#'     multiply by \code{phi_1}/\code{phi_2} — doing so would double-count
#'     effectiveness, a bug present in the pre-v0.22.11 implementation.
#'   \item For two-dose regimens, the R01 attendees who return for R02
#'     transition V1\eqn{\to}V2 at the R02 date. Non-returners (R01_doses
#'     \eqn{-} R02_doses, when positive) remain in V1 with \code{omega_1} waning
#'     from R01 onward.
#'   \item Single-dose Euvichol-S campaigns contribute only to V1.
#'   \item Blank/unknown vaccine products are treated conservatively as
#'     two-dose (most pre-2022 campaigns were two-dose Shanchol).
#'   \item \strong{Waning model:} single exponential. Real OCV waning is
#'     biphasic (fast decay in months 0-12, slower thereafter; see Xu et al.
#'     2024, Bi et al. 2017). Single exponential biases V1/V2 counts upward
#'     by roughly 10-20\% for campaigns 2-3 years pre-\code{date_start}
#'     relative to a biphasic model. This is considered acceptable for
#'     initial-condition purposes; implement biphasic separately if needed.
#'   \item \strong{Coverage ceiling:} \code{vacc_ceiling_frac = 0.70}
#'     reflects observed peak OCV coverage in published campaigns: DRC Katanga
#'     65\% (Abubakar et al. 2018), Bangladesh 75\% (Qadri et al. 2015),
#'     Haiti 80\%+ (Luquero et al. 2014). A lower ceiling under-captures
#'     real high-coverage emergency responses.
#' }
#'
#' @seealso \code{\link{process_GTFCC_vaccination_data}},
#'   \code{\link{est_vaccination_rate}}, \code{\link{convert_country_to_iso}}.
#'
#' @export
est_initial_V1_V2 <- function(PATHS,
                              config,
                              date_start = NULL,
                              cv = 0.40,
                              omega_1 = NULL,
                              omega_2 = NULL,
                              t_lag = 14,
                              vacc_ceiling_frac = 0.70,
                              fallback_shape1_V1 = 0.5,
                              fallback_shape2_V1 = 49.5,
                              fallback_shape1_V2 = 0.5,
                              fallback_shape2_V2 = 99.5,
                              verbose = TRUE) {

  if (is.null(date_start)) date_start <- config$date_start
  date_start <- as.Date(date_start)

  # Default waning rates: natural-scale means of the global priors
  if (is.null(omega_1) || is.null(omega_2)) {
    pd <- MOSAIC::priors_default
    get_gamma_mean <- function(entry) entry$parameters$shape / entry$parameters$rate
    if (is.null(omega_1)) omega_1 <- get_gamma_mean(pd$parameters_global$omega_1)
    if (is.null(omega_2)) omega_2 <- get_gamma_mean(pd$parameters_global$omega_2)
  }

  # Locate the raw GTFCC CSV
  csv_path <- file.path(PATHS$ROOT, "ees-cholera-mapping", "data", "cholera",
                        "epicentre", "gtfcc", "cholera_vacc_requests.csv")
  if (!file.exists(csv_path)) {
    stop("GTFCC raw data not found at: ", csv_path)
  }
  if (verbose) message("Reading OCV campaign history from: ", csv_path)

  raw <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  raw$event_date <- as.Date(raw$event_date, format = "%Y-%m-%d")

  # Map country names to ISO3 (reuse existing utility)
  raw$iso_code <- MOSAIC::convert_country_to_iso(raw$country, iso3 = TRUE)

  # Keep only Delivery + Round events with doses, and only pre-t0
  keep <- raw$event_type %in% c("Delivery", "Round") &
          !is.na(raw$doses) & raw$doses > 0 &
          !is.na(raw$event_date) & raw$event_date < date_start &
          !is.na(raw$iso_code)
  events <- raw[keep, c("iso_code", "req_id", "round_id", "event_type",
                         "event_date", "doses", "vaccine"), drop = FALSE]

  # Initialize output skeleton
  out <- list(parameters_location = list(
    prop_V1_initial = list(description = "Initial one-dose proportion (data-driven Beta from OCV history)",
                           location = list()),
    prop_V2_initial = list(description = "Initial two-dose proportion (data-driven Beta from OCV history)",
                           location = list())
  ))

  locations <- config$location_name
  N_j <- config$N_j_initial

  # Helper: moment-match (mean, cv) -> (shape1, shape2) Beta
  fit_beta <- function(mean_val, cv_val,
                       fb_s1, fb_s2) {
    if (!is.finite(mean_val) || mean_val <= 0) {
      return(list(shape1 = fb_s1, shape2 = fb_s2))
    }
    mu <- max(1e-6, min(0.95, mean_val))
    sigma <- mu * cv_val
    # Clamp variance to be a valid Beta variance: var < mu * (1 - mu)
    max_var <- mu * (1 - mu) * 0.999
    var_val <- min(sigma^2, max_var)
    precision <- mu * (1 - mu) / var_val - 1
    if (!is.finite(precision) || precision <= 0) {
      return(list(shape1 = fb_s1, shape2 = fb_s2))
    }
    shape1 <- max(1.001, mu * precision)
    shape2 <- max(1.001, (1 - mu) * precision)
    list(shape1 = shape1, shape2 = shape2)
  }

  # Helper: classify regimen from vaccine product string
  is_single_dose <- function(vac) {
    v <- toupper(trimws(as.character(vac)))
    v %in% c("EUVICHOLS", "EUVICHOL-S", "EUVICHOL_S")
  }

  # Helper: extract campaign id (C##) and round num (R##) from round_id like "C01-R02"
  parse_round_id <- function(rid) {
    rid <- as.character(rid)
    m <- regmatches(rid, regexec("^C([0-9]+)-R([0-9]+)$", rid))
    out_list <- lapply(m, function(x) {
      if (length(x) == 3L) list(campaign = as.integer(x[2]), round = as.integer(x[3]))
      else list(campaign = NA_integer_, round = NA_integer_)
    })
    data.frame(
      campaign = vapply(out_list, function(x) x$campaign, integer(1)),
      round    = vapply(out_list, function(x) x$round,    integer(1))
    )
  }

  # Per-country accumulation
  n_with_data <- 0L
  n_fallback  <- 0L

  for (i in seq_along(locations)) {
    iso <- locations[i]
    N   <- N_j[i]
    ce  <- events[events$iso_code == iso, , drop = FALSE]

    if (nrow(ce) == 0) {
      # Fallback: no OCV history for this country
      out$parameters_location$prop_V1_initial$location[[iso]] <- list(
        distribution = "beta",
        parameters = list(shape1 = fallback_shape1_V1, shape2 = fallback_shape2_V1)
      )
      out$parameters_location$prop_V2_initial$location[[iso]] <- list(
        distribution = "beta",
        parameters = list(shape1 = fallback_shape1_V2, shape2 = fallback_shape2_V2)
      )
      n_fallback <- n_fallback + 1L
      next
    }

    # Determine vaccine product per req_id (propagate from Delivery rows to Round rows)
    req_vaccine <- tapply(ce$vaccine, ce$req_id, function(v) {
      v <- v[!is.na(v) & nchar(trimws(v)) > 0]
      if (length(v) == 0) "" else v[1]
    })
    ce$req_vaccine <- req_vaccine[ce$req_id]

    V1_count <- 0
    V2_count <- 0

    # Process campaign-by-campaign (req_id × campaign number inside round_id)
    for (req in unique(ce$req_id)) {
      rc <- ce[ce$req_id == req & ce$event_type == "Round", , drop = FALSE]

      if (nrow(rc) == 0) {
        # No Round rows. Fall back to Delivery rows as a single implicit R01.
        drows <- ce[ce$req_id == req & ce$event_type == "Delivery", , drop = FALSE]
        if (nrow(drows) == 0) next
        vac <- ce$req_vaccine[ce$req_id == req][1]
        single <- is_single_dose(vac)
        for (k in seq_len(nrow(drows))) {
          age_days <- as.numeric(date_start - drows$event_date[k]) - t_lag
          if (age_days < 0) age_days <- 0
          dose <- drows$doses[k]
          # Without round info we conservatively treat Delivery doses as R1 (V1 only).
          # Two-dose campaigns without Round rows under-count V2, but this is rare.
          V1_count <- V1_count + dose * exp(-omega_1 * age_days)
        }
        next
      }

      # Parse round ids (campaign within req, round within campaign)
      parsed <- parse_round_id(rc$round_id)
      rc$campaign <- parsed$campaign
      rc$round    <- parsed$round

      # Rows with unparseable round_id: treat as a single R1 campaign entry
      bad <- is.na(rc$campaign) | is.na(rc$round)
      if (any(bad)) {
        for (k in which(bad)) {
          age_days <- as.numeric(date_start - rc$event_date[k]) - t_lag
          if (age_days < 0) age_days <- 0
          V1_count <- V1_count + rc$doses[k] * exp(-omega_1 * age_days)
        }
        rc <- rc[!bad, , drop = FALSE]
        if (nrow(rc) == 0) next
      }

      for (camp in unique(rc$campaign)) {
        camp_rows <- rc[rc$campaign == camp, , drop = FALSE]
        camp_rows <- camp_rows[order(camp_rows$round), , drop = FALSE]

        vac <- camp_rows$req_vaccine[1]
        single <- is_single_dose(vac)

        r1_idx <- which(camp_rows$round == 1L)
        r2_idx <- which(camp_rows$round == 2L)

        if (single) {
          # Single-dose regimen: every round contributes to V1 only
          for (k in seq_len(nrow(camp_rows))) {
            age_days <- as.numeric(date_start - camp_rows$event_date[k]) - t_lag
            if (age_days < 0) age_days <- 0
            V1_count <- V1_count + camp_rows$doses[k] * exp(-omega_1 * age_days)
          }
          next
        }

        # Two-dose regimen
        if (length(r1_idx) == 1L && length(r2_idx) == 1L) {
          # Paired R1/R2: R2 attendees transition V1->V2
          r1 <- camp_rows[r1_idx, ]
          r2 <- camp_rows[r2_idx, ]
          age1 <- max(0, as.numeric(date_start - r1$event_date) - t_lag)
          age2 <- max(0, as.numeric(date_start - r2$event_date) - t_lag)
          r2_doses <- min(r2$doses, r1$doses)  # safety: R2 can't exceed R1 attendance
          non_returners <- r1$doses - r2_doses
          V1_count <- V1_count + non_returners * exp(-omega_1 * age1)
          V2_count <- V2_count + r2_doses      * exp(-omega_2 * age2)
          # Later rounds (R3+) are rare; treat as V2 booster contribution
          extra <- camp_rows[setdiff(seq_len(nrow(camp_rows)), c(r1_idx, r2_idx)), ]
          for (k in seq_len(nrow(extra))) {
            age_k <- max(0, as.numeric(date_start - extra$event_date[k]) - t_lag)
            V2_count <- V2_count + extra$doses[k] * exp(-omega_2 * age_k)
          }
        } else if (length(r1_idx) >= 1L && length(r2_idx) == 0L) {
          # R1 only — campaign incomplete or ongoing. All to V1.
          for (k in r1_idx) {
            age_days <- max(0, as.numeric(date_start - camp_rows$event_date[k]) - t_lag)
            V1_count <- V1_count + camp_rows$doses[k] * exp(-omega_1 * age_days)
          }
        } else {
          # Odd configurations (R2 without R1, etc.) — treat each round at face value
          for (k in seq_len(nrow(camp_rows))) {
            age_k <- max(0, as.numeric(date_start - camp_rows$event_date[k]) - t_lag)
            if (camp_rows$round[k] == 1L) {
              V1_count <- V1_count + camp_rows$doses[k] * exp(-omega_1 * age_k)
            } else {
              V2_count <- V2_count + camp_rows$doses[k] * exp(-omega_2 * age_k)
            }
          }
        }
      }
    }

    # Apply coverage ceiling: cap V1+V2 at vacc_ceiling_frac * N
    total <- V1_count + V2_count
    ceiling_count <- vacc_ceiling_frac * N
    if (total > ceiling_count && total > 0) {
      scale <- ceiling_count / total
      V1_count <- V1_count * scale
      V2_count <- V2_count * scale
    }

    prop_V1 <- V1_count / N
    prop_V2 <- V2_count / N

    beta_V1 <- fit_beta(prop_V1, cv, fallback_shape1_V1, fallback_shape2_V1)
    beta_V2 <- fit_beta(prop_V2, cv, fallback_shape1_V2, fallback_shape2_V2)

    out$parameters_location$prop_V1_initial$location[[iso]] <- list(
      distribution = "beta",
      parameters = list(shape1 = beta_V1$shape1, shape2 = beta_V1$shape2)
    )
    out$parameters_location$prop_V2_initial$location[[iso]] <- list(
      distribution = "beta",
      parameters = list(shape1 = beta_V2$shape1, shape2 = beta_V2$shape2)
    )
    n_with_data <- n_with_data + 1L

    if (verbose) {
      message(sprintf("  %s: prop_V1=%.4f  prop_V2=%.4f  (Beta V1 shapes %.2f, %.2f; V2 shapes %.2f, %.2f)",
                      iso, prop_V1, prop_V2,
                      beta_V1$shape1, beta_V1$shape2,
                      beta_V2$shape1, beta_V2$shape2))
    }
  }

  if (verbose) {
    message(sprintf("est_initial_V1_V2: %d countries data-driven, %d fallback (omega_1=%.4g, omega_2=%.4g, date_start=%s)",
                    n_with_data, n_fallback, omega_1, omega_2, as.character(date_start)))
  }

  out
}
