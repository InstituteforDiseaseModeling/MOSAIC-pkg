#' Estimate Initial E and I Compartments from Surveillance Data
#'
#' This function estimates the initial number of individuals in the Exposed (E) and
#' Infected (I) compartments at model start time using recent surveillance data
#' through a Monte Carlo simulation approach.
#'
#' The method back-calculates symptom onsets from reported cases through the
#' engine's reporting chain and maps them to E/I stocks at t0 (see
#' \code{\link{est_initial_E_I_location}}). Each Monte Carlo draw samples
#' \code{sigma}, \code{iota}, \code{gamma_1}, \code{gamma_2}, \code{rho},
#' \code{chi_endemic} and \code{delta_reporting_cases} from
#' \code{priors$parameters_global} (a missing prior is replaced by a fixed
#' value with a warning); the parallel and sequential branches run the same
#' draw function. Draws with E or I = 0 are kept in the mean. Locations with no
#' usable surveillance in the window (no rows, or every case count NA) get the
#' near-zero Beta(0.01, 99999.99) template, the same prior as a window that
#' reports zero cases throughout: absent surveillance is not evidence of active
#' infection at t0, so it never seeds more E/I than confirmed zeros. Locations
#' with surveillance but too few usable draws, or an estimation error, get the
#' fallback Beta priors (Beta(1, 9999) for E, Beta(0.5, 9999.5) for I).
#'
#' @param PATHS List of paths from `get_paths()`.
#' @param priors Prior distributions for parameters (e.g., `priors_default`).
#' @param config Configuration object with location codes and `date_start`.
#'   Must include `config$location_name` and `config$date_start`.
#' @param n_samples Number of Monte Carlo samples (default 1000).
#' @param t0 Target date for estimation (default from `config$date_start`).
#' @param lookback_days Days of surveillance data before t0 to use (default 21).
#' @param lookahead_days Days of surveillance data from t0 onward that also enter the onset-rate estimate (default 0). With weekly reports downscaled to days, a window that ends at t0 can miss an outbreak already under way at t0; a window straddling t0 estimates the onset rate at t0 itself. A location gets the near-zero template only when the whole window `[t0 - lookback_days, t0 + lookahead_days)` reports no cases.
#' @param quiet_start What a "quiet-start" location gets. A location is a quiet start when it reports at least one case after the surveillance window, up to `config$date_stop` (the end of the data when `config$date_stop` is NULL), and EITHER (a) its window around t0 reports no cases (or is all NA), OR (b) the E/I priors the window gives imply fewer than one expected initial infection, `N * (E[prop_E] + E[prop_I]) < 1`, with `N` the population at t0 used in the fit and `E[.]` the Beta means. `"template"` (default) leaves the window's priors in place: the near-zero Beta(0.01, 99999.99) for (a), the data-based Beta for (b). `"seed"` gives E and I each the weak seeding prior Beta(`quiet_seed_shape1`, `quiet_seed_shape2`) instead: it stands in for undetected circulation or importation that the model has no mechanism for, so a single-location fit can still reproduce the later outbreak. Locations with no cases anywhere up to `config$date_stop`, and locations whose window-based priors imply at least one expected initial infection, are never changed.
#' @param quiet_seed_shape1,quiet_seed_shape2 Beta shapes of the quiet-start seeding prior (default 1 and 1e5: mean 1e-5 of the population per compartment, mode at zero).
#' @param verbose Print progress messages (default TRUE).
#' @param parallel Enable parallel processing for Monte Carlo sampling when
#'   `n_samples >= 100` (default FALSE). Uses `parallel::mclapply()` with all
#'   available cores. Note: Not supported on Windows.
#' @param seed Optional integer seed. When given, each location's Monte Carlo draws use seeds derived from `seed` and the ISO code, so results are reproducible and identical with or without `parallel`; the caller's RNG state is restored. NULL (default) draws from the session RNG.
#' @param variance_inflation Multiplicative CI factor for the Beta refit (default 2): the Beta keeps the Monte Carlo mean and its spread is fit to the target 95% CI mean / VI to mean * VI. A scalar or a named per-ISO vector. Should be > 1.1 for meaningful variance.
#'
#' @return A list with two main components:
#' \describe{
#'   \item{metadata}{List containing estimation details: description, version, date, t0,
#'     lookback_days, lookahead_days, n_samples, method, quiet_start and
#'     quiet_start_seeded (the locations given the seeding prior).}
#'   \item{parameters_location}{List with `prop_E_initial` and `prop_I_initial`, each containing:
#'     \itemize{
#'       \item parameter_name: Parameter identifier
#'       \item distribution: `"beta"`
#'       \item parameters$location: Named list by ISO code with `shape1` and `shape2`
#'     }
#'   }
#' }
#'
#' @examples
#' \dontrun{
#' PATHS  <- get_paths()
#' priors <- priors_default
#' config <- config_default
#' results <- est_initial_E_I(
#'   PATHS, priors, config,
#'   n_samples = 1000,
#'   variance_inflation = 2           # Factor for expanding CI bounds around sample mean
#' )
#' }
#'
#' @export
est_initial_E_I <- function(PATHS, priors, config, n_samples = 1000,
                            t0 = NULL, lookback_days = 21, lookahead_days = 0,
                            quiet_start = c("template", "seed"),
                            quiet_seed_shape1 = 1, quiet_seed_shape2 = 1e5,
                            verbose = TRUE, parallel = FALSE,
                            variance_inflation = 2, seed = NULL) {

     # ---- Parameter validation ----
     if (n_samples <= 0) stop("n_samples must be positive")
     if (lookback_days <= 0) stop("lookback_days must be positive")
     if (!is.numeric(lookahead_days) || length(lookahead_days) != 1L || lookahead_days < 0)
          stop("lookahead_days must be a single non-negative number")
     quiet_start <- match.arg(quiet_start)
     if (!is.numeric(quiet_seed_shape1) || length(quiet_seed_shape1) != 1L || !(quiet_seed_shape1 > 0) ||
         !is.numeric(quiet_seed_shape2) || length(quiet_seed_shape2) != 1L || !(quiet_seed_shape2 > 0))
          stop("quiet_seed_shape1 and quiet_seed_shape2 must be single positive numbers")
     if (!is.list(PATHS)) stop("PATHS must be a list")
     if (!is.list(priors)) stop("priors must be a list")
     if (!is.list(config)) stop("config must be a list")
     .mosaic_check_seed(seed)
     # Check variance_inflation validity (only for single values)
     if (length(variance_inflation) == 1 && variance_inflation < 1.1) {
          warning("variance_inflation too low - should be > 1.1 for meaningful variance")
     }

     # ---- t0 setup ----
     if (is.null(t0)) {
          t0 <- as.Date(config$date_start)
     } else {
          t0 <- as.Date(t0)
     }

     # ---- Locations ----
     location_codes <- config$location_name
     if (is.null(location_codes)) {
          stop("No location_name found in config")
     }

     # ---- PATHS validation ----
     required_paths <- c("DATA_CHOLERA_DAILY", "DATA_DEMOGRAPHICS")
     missing_paths <- required_paths[!required_paths %in% names(PATHS)]
     if (length(missing_paths) > 0) {
          stop("Missing required paths: ", paste(missing_paths, collapse = ", "))
     }

     # ---- Load surveillance ----
     surveillance_file <- file.path(PATHS$DATA_CHOLERA_DAILY,
                                    "cholera_surveillance_daily_combined.csv")
     if (!file.exists(surveillance_file)) {
          stop("Surveillance data file not found: ", surveillance_file)
     }
     surveillance <- read.csv(surveillance_file, stringsAsFactors = FALSE)
     required_cols <- c("date", "iso_code", "cases")
     missing_cols <- required_cols[!required_cols %in% colnames(surveillance)]
     if (length(missing_cols) > 0) {
          stop("Missing required columns in surveillance data: ", paste(missing_cols, collapse = ", "))
     }
     surveillance$date <- as.Date(surveillance$date)

     # ---- Load population ----
     pop_file <- file.path(PATHS$DATA_DEMOGRAPHICS,
                           "UN_world_population_prospects_daily.csv")
     if (!file.exists(pop_file)) {
          stop("Population data file not found: ", pop_file)
     }
     population_data <- read.csv(pop_file, stringsAsFactors = FALSE)
     required_pop_cols <- c("date", "iso_code", "total_population")
     missing_pop_cols <- required_pop_cols[!required_pop_cols %in% colnames(population_data)]
     if (length(missing_pop_cols) > 0) {
          stop("Missing required columns in population data: ", paste(missing_pop_cols, collapse = ", "))
     }
     population_data$date <- as.Date(population_data$date)

     # ---- Results scaffold ----
     results <- list(
          metadata = list(
               description = "Initial E and I compartment estimates from surveillance data",
               version = "1.2.0",
               date = Sys.Date(),
               t0 = t0,
               lookback_days = lookback_days,
               lookahead_days = lookahead_days,
               n_samples = n_samples,
               quiet_start = quiet_start,
               quiet_start_seeded = character(0),
               method = "monte_carlo_backcalculation"
          ),
          parameters_location = list(
               prop_E_initial = list(
                    parameter_name = "prop_E_initial",
                    distribution = "beta",
                    parameters = list(location = list())
               ),
               prop_I_initial = list(
                    parameter_name = "prop_I_initial",
                    distribution = "beta",
                    parameters = list(location = list())
               )
          )
     )

     # ---- Window & availability ----
     end_date <- t0 + lookahead_days - 1
     start_date <- t0 - lookback_days
     surveillance_window <- surveillance[surveillance$date >= start_date &
                                              surveillance$date <= end_date, ]
     countries_with_data <- unique(surveillance_window$iso_code[
          !is.na(surveillance_window$cases) & surveillance_window$cases >= 0
     ])

     # Locations that report cases after the window (up to date_stop): the
     # candidates for the quiet-start seeding prior.
     later_stop <- if (!is.null(config$date_stop)) as.Date(config$date_stop) else max(surveillance$date)
     later <- surveillance[surveillance$date > end_date & surveillance$date <= later_stop &
                                !is.na(surveillance$cases) & surveillance$cases > 0, ]
     countries_with_later_cases <- unique(later$iso_code)

     if (verbose) {
          cat("\n=== Estimating Initial E and I Compartments ===\n")
          cat(sprintf("Method: Monte Carlo simulation\n"))
          cat(sprintf("Target date (t0): %s\n", t0))
          cat(sprintf("Lookback window: %s to %s (%d days)\n",
                      start_date, end_date, lookback_days + lookahead_days))
          cat(sprintf("Number of Monte Carlo samples: %d\n", n_samples))
          cat(sprintf("CI bounds: variance_inflation=%.1f\n",
                      variance_inflation))
          cat(sprintf("Found surveillance data for %d/%d countries\n",
                      length(countries_with_data), length(location_codes)))
          cat("\n")
     }

     # Handle variance_inflation parameter - convert to location-specific lookup
     if (is.numeric(variance_inflation) && length(variance_inflation) > 1) {
          # Named vector provided
          if (is.null(names(variance_inflation))) {
               stop("Multi-value variance_inflation must be a named vector with ISO codes")
          }
          variance_inflation_lookup <- variance_inflation
          default_variance_inflation <- variance_inflation[1]  # Use first as default
          if (verbose) {
               cat(sprintf("Using location-specific variance inflation (range: %.1f-%.1f)\n",
                          min(variance_inflation), max(variance_inflation)))
          }
     } else {
          # Single value provided - use for all locations
          variance_inflation_lookup <- NULL
          default_variance_inflation <- variance_inflation
     }

     # ---- Reporting-chain priors (shared by both MC branches) ----
     # rho (reporting), chi_endemic (PPV of the suspected-case definition) and
     # delta_reporting_cases (onset-to-report lag) are the engine's own
     # observation-process priors. [[ ]] avoids `$rho` partial-matching
     # `rho_deaths` when `rho` is absent.
     chain_priors <- .est_initial_E_I_chain_priors(priors)

     # ---- Monte Carlo method: Per-location processing ----
     for (loc in location_codes) {
          if (verbose) cat(sprintf("Processing %s... ", loc))

          # Location-specific variance inflation, resolved once per location
          loc_variance_inflation <- if (!is.null(variance_inflation_lookup) &&
                                        loc %in% names(variance_inflation_lookup)) {
               unname(variance_inflation_lookup[loc])
          } else {
               default_variance_inflation
          }

          loc_res <- tryCatch(
               .est_initial_E_I_one(
                    loc = loc,
                    surveillance_window = surveillance_window,
                    countries_with_data = countries_with_data,
                    population_data = population_data,
                    t0 = t0,
                    lookback_days = lookback_days,
                    lookahead_days = lookahead_days,
                    n_samples = n_samples,
                    priors = priors,
                    chain_priors = chain_priors,
                    loc_variance_inflation = loc_variance_inflation,
                    parallel = parallel,
                    verbose = verbose,
                    seed = if (is.null(seed)) NULL else .mosaic_derive_seed(seed, loc)
               ),
               error = function(e) {
                    warning(sprintf("Error processing %s: %s", loc, e$message))
                    if (verbose) cat(sprintf("error: %s\n", e$message))
                    list(E = .est_initial_E_I_default("E", n_samples, "error_fallback",
                                                      paste("Estimation error:", e$message)),
                         I = .est_initial_E_I_default("I", n_samples, "error_fallback",
                                                      paste("Estimation error:", e$message)))
               }
          )

          if (is.null(loc_res)) next   # no usable population row (warned)
          if (quiet_start == "seed" && loc %in% countries_with_later_cases &&
              .est_initial_E_I_is_quiet(loc_res)) {
               loc_res <- list(E = .est_initial_E_I_quiet_seed(quiet_seed_shape1, quiet_seed_shape2,
                                                               n_samples, loc_res$E$method),
                               I = .est_initial_E_I_quiet_seed(quiet_seed_shape1, quiet_seed_shape2,
                                                               n_samples, loc_res$I$method))
               results$metadata$quiet_start_seeded <- c(results$metadata$quiet_start_seeded, loc)
          }
          results$parameters_location$prop_E_initial$parameters$location[[loc]] <- loc_res$E
          results$parameters_location$prop_I_initial$parameters$location[[loc]] <- loc_res$I
     }

     if (verbose) {
          cat("\n=== Monte Carlo Estimation Complete ===\n")
          cat(sprintf("Successfully processed %d locations\n",
                      length(results$parameters_location$prop_E_initial$parameters$location)))

          # Comprehensive results table
          cat("\n=== Final E/I Prior Distribution Summary ===\n")
          cat(sprintf("%-4s %-20s %-15s %-12s %-20s %-15s %-12s\n",
                      "LOC", "E_Beta(shape1,shape2)", "E_Mean_Count", "E_95%_CI",
                      "I_Beta(shape1,shape2)", "I_Mean_Count", "I_95%_CI"))
          cat(paste(rep("-", 110), collapse=""), "\n")

          for (loc in names(results$parameters_location$prop_E_initial$parameters$location)) {
               E_result <- results$parameters_location$prop_E_initial$parameters$location[[loc]]
               I_result <- results$parameters_location$prop_I_initial$parameters$location[[loc]]

               # Calculate 95% CI for E compartment counts
               if (!is.null(E_result$metadata$mean_count) && E_result$metadata$mean_count > 0) {
                    E_mean <- E_result$metadata$mean_count
                    E_sd <- E_result$metadata$sd_count
                    E_ci_low <- max(0, E_mean - 1.96 * E_sd)
                    E_ci_high <- E_mean + 1.96 * E_sd
                    E_ci_str <- sprintf("(%.0f-%.0f)", E_ci_low, E_ci_high)
                    E_mean_str <- sprintf("%.0f", E_mean)
               } else {
                    E_ci_str <- "(-)"
                    E_mean_str <- "0"
               }

               # Calculate 95% CI for I compartment counts
               if (!is.null(I_result$metadata$mean_count) && I_result$metadata$mean_count > 0) {
                    I_mean <- I_result$metadata$mean_count
                    I_sd <- I_result$metadata$sd_count
                    I_ci_low <- max(0, I_mean - 1.96 * I_sd)
                    I_ci_high <- I_mean + 1.96 * I_sd
                    I_ci_str <- sprintf("(%.0f-%.0f)", I_ci_low, I_ci_high)
                    I_mean_str <- sprintf("%.0f", I_mean)
               } else {
                    I_ci_str <- "(-)"
                    I_mean_str <- "0"
               }

               E_beta_str <- sprintf("(%.2f,%.2f)", E_result$shape1, E_result$shape2)
               I_beta_str <- sprintf("(%.2f,%.2f)", I_result$shape1, I_result$shape2)

               cat(sprintf("%-4s %-20s %-15s %-20s %-20s %-15s %-20s\n",
                           loc, E_beta_str, E_mean_str, E_ci_str,
                           I_beta_str, I_mean_str, I_ci_str))
          }
          cat("\nNote: CIs based on Monte Carlo sample statistics (mean \u00B1 1.96\u00D7SD)\n")
     }

     return(results)
}

#' Estimate E and I Compartments for a Single Location
#'
#' This function performs the actual E/I estimation for a single location using
#' surveillance data and epidemiological parameters, following the engine's
#' reporting chain: a report on day d is a symptomatic onset on day
#' \code{d - tau_r}, and all onsets (symptomatic and asymptomatic) are
#' \code{reported * chi / (rho * sigma)}.
#'
#' \itemize{
#'   \item \strong{I}: observed onsets that have not yet recovered at t0
#'     (per-day survival \code{exp(-gamma_1)} for the symptomatic share
#'     \code{sigma}, \code{exp(-gamma_2)} for the rest), plus the onsets the
#'     window cannot see, filled in at the window's mean onset rate: those in
#'     the last \code{tau_r} days before t0 (reported on or after t0) and those
#'     older than the window.
#'   \item \strong{E}: people infected before t0 whose onset comes after t0.
#'     A reported case is already past E, so E is the stock in balance with the
#'     window's mean onset rate \eqn{\lambda}:
#'     \eqn{E = \lambda / (1 - e^{-\iota})}, the engine's daily E-to-I
#'     probability (Azman et al. 2013 for the incubation period behind
#'     \code{iota}).
#' }
#'
#' @param cases Vector of daily suspected cholera cases (must be same length as dates)
#' @param dates Vector of dates corresponding to cases (Date class)
#' @param population Total population of the location (must be positive)
#' @param t0 Target date for estimation (Date class)
#' @param lookback_days Days of reports before t0 to use (default 60, must be positive)
#' @param lookahead_days Days of reports from t0 onward that also enter the onset rate (default 0, non-negative). The onset rate is averaged over the whole window of \code{lookback_days + lookahead_days} days; only reports before t0 enter I directly (later ones are onsets that have not happened yet or that the rate fill-in already covers).
#' @param sigma Symptomatic proportion (must be in (0,1])
#' @param rho Reporting rate - proportion of symptomatic cases reported (must be in (0,1])
#' @param chi Diagnostic positivity - proportion of suspected cases that are true cholera (must be in (0,1])
#' @param tau_r Reporting delay in days from symptom onset to report (must be non-negative; rounded to whole days as in the engine)
#' @param iota Incubation rate (1/incubation period, must be positive)
#' @param gamma_1 Symptomatic recovery rate (must be positive)
#' @param gamma_2 Asymptomatic recovery rate (must be positive)
#' @param verbose Print detailed progress (default FALSE)
#'
#' @return A list with two components:
#' \describe{
#' \item{E}{Number of individuals in Exposed compartment (non-negative numeric)}
#' \item{I}{Number of individuals in Infected compartment (non-negative numeric)}
#' }
#'
#' Returns E=0, I=0 if no cases in the window. Includes numerical stability
#' protections and parameter validation. Warns if E or I exceed 2% of population.
#'
#' @examples
#' \dontrun{
#' # Example with synthetic data
#' cases <- rpois(60, lambda = 5)
#' dates <- seq(as.Date("2023-01-01"), by = "day", length.out = 60)
#' result <- est_initial_E_I_location(
#'   cases = cases,
#'   dates = dates,
#'   population = 1000000,
#'   t0 = as.Date("2023-03-01"),
#'   sigma = 0.125,
#'   rho = 0.1,
#'   chi = 0.5,
#'   tau_r = 4,
#'   iota = 0.714,
#'   gamma_1 = 0.2,
#'   gamma_2 = 0.67
#' )
#' }
#'
#' @export
est_initial_E_I_location <- function(cases, dates, population, t0, lookback_days = 60,
                                     lookahead_days = 0, sigma, rho, chi, tau_r, iota, gamma_1, gamma_2,
                                     verbose = FALSE) {

  # ---- Parameter validation ----
  if (length(cases) != length(dates)) stop("cases and dates must have same length")
  if (length(cases) == 0) stop("cases and dates cannot be empty")
  if (population <= 0) stop("population must be positive")
  if (lookback_days <= 0) stop("lookback_days must be positive")
  if (lookahead_days < 0) stop("lookahead_days must be non-negative")
  if (sigma <= 0 || sigma > 1) stop("sigma must be in (0,1]")
  if (rho <= 0 || rho > 1) stop("rho must be in (0,1]")
  if (chi <= 0 || chi > 1) stop("chi must be in (0,1]")
  if (tau_r < 0) stop("tau_r must be non-negative")
  if (iota <= 0) stop("iota must be positive")
  if (gamma_1 <= 0) stop("gamma_1 must be positive")
  if (gamma_2 <= 0) stop("gamma_2 must be positive")

  # Convert dates to Date class if needed
  if (!inherits(dates, "Date")) {
    tryCatch({
      dates <- as.Date(dates)
    }, error = function(e) {
      stop("dates must be convertible to Date class")
    })
  }

  if (!inherits(t0, "Date")) {
    tryCatch({
      t0 <- as.Date(t0)
    }, error = function(e) {
      stop("t0 must be convertible to Date class")
    })
  }

  # ---- Filter surveillance data to lookback window ----
  lookback_start <- t0 - lookback_days
  lookback_end <- t0 + lookahead_days - 1
  window_days <- lookback_days + lookahead_days

  # Filter cases within lookback window
  in_window <- dates >= lookback_start & dates <= lookback_end
  cases_filtered <- cases[in_window]
  dates_filtered <- dates[in_window]

  if (verbose) {
    cat(sprintf("  Lookback window: %s to %s (%d days)\n",
                lookback_start, lookback_end, window_days))
    cat(sprintf("  Cases in window: %d (total: %.0f)\n",
                length(cases_filtered), sum(cases_filtered, na.rm = TRUE)))
  }

  # ---- Handle no-data case ----
  if (length(cases_filtered) == 0 || sum(cases_filtered, na.rm = TRUE) == 0) {
    if (verbose) cat("  No cases in lookback window, returning E=0, I=0\n")
    return(list(E = 0, I = 0))
  }

  # ---- Back-calculation through the engine's reporting chain ----
  # The engine reports a symptomatic onset on day s as a case on day s + tau_r
  # (tau_r = delta_reporting_cases, whole days) with probability rho, and the
  # reported suspected count is inflated by 1/chi (PPV). So a report on day d
  # is a symptomatic onset on day d - tau_r, and
  #   new symptomatic onsets = reported * chi / rho,
  #   all new onsets (sym + asym) = reported * chi / (rho * sigma).
  # (Same inversion as .moment_match_E_I() in sample_parameters.R.)
  #
  # A reported case has already left E, so E(t0) is NOT built from reports.
  # Everyone in E at t0 has their onset after t0; under locally constant
  # incidence the engine's E stock is in balance with the onset rate lambda:
  # onsets/day = iota_prob * E, i.e. E = lambda / (1 - exp(-iota))
  # (iota_prob as in sim_params()).
  #
  # I(t0) sums the observed onsets that have not yet recovered, with the
  # engine's per-day survival exp(-gamma) for symptomatic (gamma_1) and
  # asymptomatic (gamma_2) infections. Onsets outside the observed window are
  # filled in at the same rate lambda: those in the last tau_r days before t0
  # (reported on/after t0) and those older than the window (reported before
  # it), which matter when the window is short relative to 1/gamma_1.
  tau <- round(tau_r)
  total_cases <- sum(cases_filtered, na.rm = TRUE)
  onset_mult <- chi / (rho * sigma)
  lambda <- total_cases * onset_mult / window_days   # all onsets per day

  if (verbose) {
    cat(sprintf("  Onset multiplier: chi/(rho\u00D7sigma) = %.2f\n", onset_mult))
    cat(sprintf("  Mean onsets per day: %.1f\n", lambda))
  }

  survival <- function(age) {
    sigma * exp(-gamma_1 * age) + (1 - sigma) * exp(-gamma_2 * age)
  }

  # Observed onsets: report day t0 - j (j >= 1) -> onset age j + tau at t0.
  # Reports on or after t0 inform lambda only (onsets after t0, or within the
  # last tau days before it, which I_recent fills at lambda).
  onset_age <- as.numeric(t0 - dates_filtered) + tau
  onsets <- ifelse(is.na(cases_filtered) | dates_filtered >= t0, 0, cases_filtered) * onset_mult
  I_observed <- sum(onsets * survival(onset_age))

  # Unobserved recent onsets: ages 1..tau
  I_recent <- if (tau >= 1) lambda * sum(survival(seq_len(tau))) else 0
  # Onsets older than the window: ages > lookback_days + tau (geometric tail)
  k_max <- lookback_days + tau
  tail_sum <- function(g) exp(-g * (k_max + 1)) / (-expm1(-g))
  I_older <- lambda * (sigma * tail_sum(gamma_1) + (1 - sigma) * tail_sum(gamma_2))

  E_total <- lambda / (-expm1(-iota))
  I_total <- I_observed + I_recent + I_older

  # ---- Numerical stability and bounds checking ----
  E_total <- max(0, round(E_total))
  I_total <- max(0, round(I_total))

  # Check for unrealistic estimates
  if (E_total > 0.02 * population) {
    warning(sprintf("E estimate (%.0f) exceeds 2%% of population (%.0f)", E_total, population))
  }
  if (I_total > 0.02 * population) {
    warning(sprintf("I estimate (%.0f) exceeds 2%% of population (%.0f)", I_total, population))
  }

  if (verbose) {
    cat(sprintf("  Final estimates: E=%.0f, I=%.0f\n", E_total, I_total))
    cat(sprintf("  As proportion of population: E=%.4f%%, I=%.4f%%\n",
                100*E_total/population, 100*I_total/population))
  }

  return(list(E = E_total, I = I_total))
}

# ---- internal helpers for est_initial_E_I() ---------------------------------

# Observation-process priors used by the E/I back-calculation, read from the
# global priors: rho, chi_endemic (falls back to a single `chi`) and
# delta_reporting_cases. A missing prior is replaced by a fixed value with a
# warning (never silently).
.est_initial_E_I_chain_priors <- function(priors) {
     pg <- priors$parameters_global
     get_prior <- function(names_try, default_value, label) {
          for (nm in names_try) {
               if (!is.null(pg[[nm]])) return(pg[[nm]])
          }
          warning(sprintf("est_initial_E_I: priors$parameters_global$%s not found; using %s = %g",
                          names_try[1], label, default_value), call. = FALSE)
          list(distribution = "fixed", value = default_value)
     }
     list(
          rho   = get_prior("rho", 0.43, "rho"),
          chi   = get_prior(c("chi_endemic", "chi"), 0.52, "chi"),
          tau_r = get_prior("delta_reporting_cases", 1, "delta_reporting_cases")
     )
}

# One draw from a prior entry, honouring the fixed-value placeholder above.
.est_initial_E_I_sample <- function(prior) {
     if (identical(prior$distribution, "fixed")) return(prior$value)
     sample_from_prior(n = 1, prior = prior, verbose = FALSE)
}

# Fallback Beta priors (mean ~1e-4 for E and ~5e-5 for I) for a location WITH
# surveillance whose Monte Carlo estimate failed or had too few usable draws.
.est_initial_E_I_default <- function(compartment, n_samples, method, message) {
     shapes <- if (compartment == "E") c(1, 9999) else c(0.5, 9999.5)
     list(shape1 = shapes[1], shape2 = shapes[2], method = method,
          metadata = list(data_available = FALSE, total_cases = 0,
                          mean_count = 0, sd_count = 0, n_samples = n_samples,
                          message = message))
}

# Prior for a location with no usable surveillance in the window (no rows, or
# every count NA): the near-zero template Beta(0.01, 99999.99) (mean ~1e-7,
# MOSAIC CLAUDE.md IC defaults), as for an observed-zero window. The fallback
# above (mean 1e-4 / 5e-5) would seed ~1,000 infections in a country of 20M
# with no evidence of transmission, more than confirmed zeros get.
.est_initial_E_I_no_surveillance <- function(n_samples, message) {
     list(shape1 = 0.01, shape2 = 99999.99, method = "no_data_default",
          metadata = list(data_available = FALSE, total_cases = NA_real_,
                          mean_count = 0, sd_count = 0, n_samples = n_samples,
                          message = message))
}

# Is a location with cases later in the config window a quiet start? (a) its
# window reported no cases (or only NA), or (b) the window's E/I priors imply
# fewer than one expected initial infection, N * (E[prop_E] + E[prop_I]) < 1,
# with N the population at t0 the fit used (loc_res$population).
.est_initial_E_I_is_quiet <- function(loc_res) {
     if (loc_res$E$method %in% c("observed_zero", "no_data_default")) return(TRUE)
     N <- loc_res$population
     if (is.null(N) || length(N) != 1L || !is.finite(N) || N <= 0) return(FALSE)
     beta_mean <- function(x) x$shape1 / (x$shape1 + x$shape2)
     N * (beta_mean(loc_res$E) + beta_mean(loc_res$I)) < 1
}

# Weak seeding prior for a quiet-start location: stands in for undetected
# circulation or importation. `was` records the method the window alone gave.
.est_initial_E_I_quiet_seed <- function(shape1, shape2, n_samples, was) {
     no_cases <- was %in% c("observed_zero", "no_data_default")
     list(shape1 = shape1, shape2 = shape2, method = "quiet_start_seed",
          metadata = list(data_available = was != "no_data_default",
                          total_cases = if (no_cases) 0 else NA_real_,
                          mean_count = 0, sd_count = 0, n_samples = n_samples,
                          message = if (no_cases) {
                               paste0("No cases in the window around t0 (", was,
                                      ") but cases later in the config window")
                          } else {
                               paste0("Window-based prior (", was, ") implied fewer than one ",
                                      "expected initial infection; cases later in the config window")
                          }))
}

# One Monte Carlo draw of (E, I) counts for a location. Shared by the parallel
# and sequential branches so both use identical priors.
.est_initial_E_I_draw <- function(priors, chain_priors, loc_surv, population_t0,
                                  t0, lookback_days, lookahead_days = 0) {
     pg <- priors$parameters_global
     sigma_i   <- sample_from_prior(n = 1, prior = pg[["sigma"]],   verbose = FALSE)
     iota_i    <- sample_from_prior(n = 1, prior = pg[["iota"]],    verbose = FALSE)
     gamma_1_i <- sample_from_prior(n = 1, prior = pg[["gamma_1"]], verbose = FALSE)
     gamma_2_i <- sample_from_prior(n = 1, prior = pg[["gamma_2"]], verbose = FALSE)
     rho_i     <- .est_initial_E_I_sample(chain_priors$rho)
     chi_i     <- .est_initial_E_I_sample(chain_priors$chi)
     # The engine applies the case lag in whole days (make_simulation_config
     # rounds delta_reporting_cases), so the back-calculation does too.
     tau_r_i   <- round(.est_initial_E_I_sample(chain_priors$tau_r))

     # Bounds & fallbacks for failed draws (NA or out of support)
     if (is.na(sigma_i)   || sigma_i <= 0 || sigma_i > 1) sigma_i   <- 0.35
     if (is.na(rho_i)     || rho_i <= 0   || rho_i > 1)   rho_i     <- 0.43
     if (is.na(chi_i)     || chi_i <= 0   || chi_i > 1)   chi_i     <- 0.52
     if (is.na(iota_i)    || iota_i <= 0)                 iota_i    <- 0.714
     if (is.na(gamma_1_i) || gamma_1_i <= 0)              gamma_1_i <- 0.1
     if (is.na(gamma_2_i) || gamma_2_i <= 0)              gamma_2_i <- 0.67
     if (is.na(tau_r_i)   || tau_r_i < 0)                 tau_r_i   <- 1

     ei <- est_initial_E_I_location(
          cases = loc_surv$cases, dates = loc_surv$date,
          population = population_t0, t0 = t0, lookback_days = lookback_days,
          lookahead_days = lookahead_days, sigma = sigma_i, rho = rho_i, chi = chi_i, tau_r = tau_r_i,
          iota = iota_i, gamma_1 = gamma_1_i, gamma_2 = gamma_2_i,
          verbose = FALSE
     )
     c(E = ei$E, I = ei$I)
}

# Beta prior for one compartment from its Monte Carlo counts. Zero draws are
# real outcomes and stay in the mean. The Beta keeps the Monte Carlo mean and
# its spread is fit to the target 95% CI mean / VI to mean * VI.
.est_initial_E_I_fit <- function(counts, population_t0, compartment, loc,
                                 loc_variance_inflation, n_samples, total_cases,
                                 verbose) {
     counts <- counts[is.finite(counts) & counts >= 0]
     prop <- counts / population_t0
     prop <- prop[prop < 1]
     if (length(prop) >= 2 && total_cases == 0) {
          # Surveillance reported zero cases throughout the window: every draw is
          # E = I = 0. Use the near-zero template prior (Beta(0.01, 99999.99),
          # mean ~1e-7; MOSAIC CLAUDE.md IC defaults), not the no-data guess.
          return(list(shape1 = 0.01, shape2 = 99999.99, method = "observed_zero",
                      metadata = list(data_available = TRUE, total_cases = 0,
                                      mean_count = 0, sd_count = 0,
                                      n_samples = length(counts),
                                      message = "Zero reported cases in the surveillance window")))
     }
     if (length(prop) < 2 || mean(prop) <= 0) {
          if (verbose) cat(sprintf("  Insufficient %s data for %s - using no-data default\n",
                                   compartment, loc))
          return(.est_initial_E_I_default(compartment, n_samples, "insufficient_data",
                                          "Fewer than 2 usable Monte Carlo draws"))
     }
     m <- mean(prop)
     ci_lower <- max(1e-10, min(m / loc_variance_inflation, 0.999))
     ci_upper <- max(ci_lower + 1e-10, min(m * loc_variance_inflation, 0.999))
     # m is the Monte Carlo MEAN, so it anchors the Beta mean (not its mode):
     # a mode-exact fit with both shapes > 1 put the prior mean ~6x above m for
     # VI = 65-160 and cannot span the target's decades below m. The mean-anchored
     # fit lets shape1 < 1; for wide VI it reproduces the lower target and falls
     # short on the upper one (a Beta with mean m cannot put 2.5% of its mass
     # above m * VI for large VI).
     shapes <- .fit_beta_mean_ci(m, ci_lower, ci_upper)
     list(shape1 = shapes[1], shape2 = shapes[2], method = "variance_inflation",
          metadata = list(data_available = TRUE, total_cases = total_cases,
                          mean_count = mean(counts), sd_count = stats::sd(counts),
                          n_samples = length(counts)))
}

# E/I priors for one location: list(E = <entry>, I = <entry>), or NULL when the
# location has no usable population row (a warning is raised).
.est_initial_E_I_one <- function(loc, surveillance_window, countries_with_data,
                                 population_data, t0, lookback_days, lookahead_days, n_samples,
                                 priors, chain_priors, loc_variance_inflation,
                                 parallel, verbose, seed = NULL) {

     loc_surv <- surveillance_window[surveillance_window$iso_code == loc, ]
     has_data <- loc %in% countries_with_data && nrow(loc_surv) > 0
     if (!has_data) {
          if (verbose) cat("no data, using default priors\n")
          msg <- "No surveillance data in the surveillance window"
          return(list(E = .est_initial_E_I_no_surveillance(n_samples, msg),
                      I = .est_initial_E_I_no_surveillance(n_samples, msg)))
     }

     # Population at ~t0
     pop_loc <- population_data[population_data$iso_code == loc, ]
     if (nrow(pop_loc) == 0) {
          warning(sprintf("No population data for %s", loc))
          if (verbose) cat("no population data\n")
          return(NULL)
     }
     time_diffs    <- abs(as.numeric(difftime(pop_loc$date, t0, units = "days")))
     population_t0 <- pop_loc$total_population[which.min(time_diffs)]
     if (is.na(population_t0) || population_t0 <= 0) {
          warning(sprintf("Invalid population for %s", loc))
          if (verbose) cat("invalid population\n")
          return(NULL)
     }

     draw_seeds <- if (is.null(seed)) NULL else .mosaic_draw_seeds(seed, n_samples)
     draw <- function(i) {
          .mosaic_maybe_local_seed(draw_seeds[i],
               .est_initial_E_I_draw(priors, chain_priors, loc_surv, population_t0,
                                     t0, lookback_days, lookahead_days))
     }
     if (parallel && n_samples >= 100) {
          if (verbose) cat(sprintf("  Using parallel processing with %d cores\n",
                                   parallel::detectCores()))
          # Pin threads (forks inherit the parent env) and leave one core free
          # so the forks don't oversubscribe the host.
          .mosaic_set_all_thread_env(1L)
          mc_results <- parallel::mclapply(seq_len(n_samples), draw,
                                           mc.cores = max(1L, parallel::detectCores() - 1L))
     } else {
          mc_results <- lapply(seq_len(n_samples), draw)
     }
     E_samples <- vapply(mc_results, function(x) unname(x["E"]), numeric(1))
     I_samples <- vapply(mc_results, function(x) unname(x["I"]), numeric(1))

     total_cases <- sum(loc_surv$cases, na.rm = TRUE)
     E <- .est_initial_E_I_fit(E_samples, population_t0, "E", loc, loc_variance_inflation,
                               n_samples, total_cases, verbose)
     I <- .est_initial_E_I_fit(I_samples, population_t0, "I", loc, loc_variance_inflation,
                               n_samples, total_cases, verbose)
     if (verbose) cat("done\n")
     list(E = E, I = I, population = population_t0)
}
