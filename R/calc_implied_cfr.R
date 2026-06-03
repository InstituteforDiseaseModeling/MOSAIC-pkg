#' Derived implied case fatality ratio (CFR) per posterior sample
#'
#' Adds four derived columns per location to a sampled-parameters table:
#' \itemize{
#'   \item \code{cfr_baseline_<iso>}  — surveillance CFR under endemic regime
#'   \item \code{cfr_epidemic_<iso>}  — surveillance CFR under epidemic regime
#'   \item \code{cfr_clinical_baseline_<iso>}  — per-symptomatic-episode lethality (endemic)
#'   \item \code{cfr_clinical_epidemic_<iso>}  — per-symptomatic-episode lethality (epidemic)
#' }
#' These are policy-relevant transformations of the sampled microparameters
#' via the v0.13+ engine identity in MOSAIC.
#'
#' \strong{Surveillance CFR (reported-deaths / reported-cases)} — analytic
#' steady-state limit of the deaths-cases ratio in the surveillance signal:
#'
#' \deqn{\text{CFR}_{\text{baseline}}^{j}\ =\ \mu_{j,0} \cdot \rho_{\text{deaths}}
#'        \cdot \chi^{\text{end}} / \rho}
#' \deqn{\text{CFR}_{\text{epidemic}}^{j}\ =\ \mu_{j,0} \cdot (1 + \varepsilon_j)
#'        \cdot \rho_{\text{deaths}} \cdot \chi^{\text{epi}} / \rho}
#'
#' \strong{Important — these are evaluated at simulation tick 0} (i.e. with
#' the temporal slope \eqn{\mu_{j,\text{slope}} \cdot t_{\text{factor}} = 0}).
#' Under the default prior \eqn{\mu_{j,\text{slope}} \sim \mathcal{N}(0, 0.05)}
#' the slope contribution over a typical 3-year calibration is < 5% at any
#' point in the window, so the intercept-CFR is a good proxy for the average
#' endemic / epidemic CFR. For applications sensitive to slow CFR drift,
#' compute the time-resolved CFR per tick from the predictions arrays
#' instead.
#'
#' \strong{Clinical CFR (per-episode lethality)} — probability a symptomatic
#' individual dies of cholera over their expected symptomatic period
#' (\eqn{1/\gamma_1}). Approximation \eqn{1 - e^{-\mu_{j,t}/\gamma_1}}:
#'
#' \deqn{\text{CFR}_{\text{clinical, baseline}}^{j}\ =\ 1 - \exp(-\mu_{j,0} / \gamma_1)}
#' \deqn{\text{CFR}_{\text{clinical, epidemic}}^{j}\ =\ 1 - \exp(-\mu_{j,0} (1 + \varepsilon_j) / \gamma_1)}
#'
#' This is the metric clinicians compare against treatment-center death rates
#' and the WHO outbreak-response 1% threshold; it does NOT pass through the
#' observation pipeline.
#'
#' \strong{Approximation caveat for the clinical-CFR epidemic variant.}
#' The expression \eqn{1 - \exp(-\mu_{j,0}(1+\varepsilon_j)/\gamma_1)}
#' applies the outbreak mortality elevation across the entire symptomatic
#' duration. Real patients enter / exit the symptomatic stock during
#' outbreak onset and decline, so a fraction of their symptomatic days
#' may be scored under endemic \eqn{\mu_{j,0}}. The approximation is
#' tight for multi-month outbreaks (where most of \eqn{1/\gamma_1} \eqn{\sim} 5
#' days falls inside a stable epidemic flag) and loose for sub-week flag
#' flips; in the latter regime the clinical-epidemic CFR is upward-biased
#' by roughly the fraction of the symptomatic period spent outside
#' epidemic mode. For policy reporting on protracted outbreaks the
#' approximation is fine; for narrow flare-ups treat it as an upper bound.
#'
#' \strong{Numerical guards.} All four columns are clamped to \eqn{[0, 1]}.
#' Non-finite values (e.g. division by a near-zero \eqn{\rho} sample) are
#' replaced with \code{NA_real_} so that downstream beta posterior fits via
#' method-of-moments do not silently degenerate.
#'
#' @param results data.frame with one row per posterior sample, containing
#'   global columns \code{rho, rho_deaths, chi_endemic, chi_epidemic} and
#'   per-iso columns \code{mu_j_baseline_<iso>, mu_j_epidemic_factor_<iso>}.
#'   Optional global column \code{gamma_1} enables the clinical CFR variants;
#'   when absent, only the surveillance CFRs are added (with a warning).
#' @param iso_codes Character vector of location ISO codes (required). Pass
#'   \code{config$location_name}. The function previously auto-derived this
#'   from \code{mu_j_baseline_<iso>} regex; that was unsafe for non-3-letter
#'   codes and aggregate (\code{_AFRO}) suffixes and has been removed.
#' @param verbose Logical. Print a summary line per added column.
#'
#' @return The input data.frame with up to four added numeric columns per
#'   iso: \code{cfr_baseline_<iso>}, \code{cfr_epidemic_<iso>},
#'   \code{cfr_clinical_baseline_<iso>}, \code{cfr_clinical_epidemic_<iso>}.
#'   If a required constituent column is missing for a given iso, that iso's
#'   CFR columns are silently skipped with a one-line warning.
#'
#' @keywords internal
#' @noRd
.mosaic_add_implied_cfr_columns <- function(results, iso_codes, verbose = TRUE) {

     if (!is.data.frame(results) && !inherits(results, "tbl_df")) {
          return(results)
     }
     if (missing(iso_codes) || is.null(iso_codes) || !length(iso_codes)) {
          if (verbose) message("  Skipping implied CFR: iso_codes is empty")
          return(results)
     }
     stopifnot(is.character(iso_codes))

     required_globals <- c("rho", "rho_deaths", "chi_endemic", "chi_epidemic")
     missing_globals  <- setdiff(required_globals, names(results))
     if (length(missing_globals)) {
          if (verbose) {
               message(sprintf("  Skipping implied CFR: missing global columns: %s",
                               paste(missing_globals, collapse = ", ")))
          }
          return(results)
     }

     has_gamma1 <- "gamma_1" %in% names(results)
     if (!has_gamma1 && verbose) {
          message("  Note: gamma_1 not in samples — clinical (per-episode) CFR will be omitted")
     }

     # Clamp a numeric vector to [0, 1] and replace non-finite (Inf/NaN) with NA.
     # Used on derived CFR columns where rho draws near 0 can produce Inf and
     # epidemic-multiplier draws can push values above 1.
     .clamp01 <- function(x) {
          x[!is.finite(x)] <- NA_real_
          pmin(pmax(x, 0), 1)
     }

     n_added  <- 0L
     n_locs_done <- 0L
     for (iso in iso_codes) {
          mu_col  <- paste0("mu_j_baseline_",         iso)
          eps_col <- paste0("mu_j_epidemic_factor_",  iso)
          if (!(mu_col %in% names(results)) || !(eps_col %in% names(results))) {
               if (verbose) {
                    message(sprintf("  Skipping implied CFR for %s: missing %s or %s",
                                    iso, mu_col, eps_col))
               }
               next
          }

          mu_eff_end <- results[[mu_col]]
          mu_eff_epi <- results[[mu_col]] * (1 + results[[eps_col]])

          # Sign guard: a pathological posterior on mu_j_epidemic_factor (or a
          # numerical accident) could drive (1 + eps) <= 0, flipping the sign
          # of mu_eff_epi. The downstream clamp would silently mask this to 0.
          # Log a count instead so reviewers can audit. The prior is Gamma(1,2)
          # which is strictly positive, so this should be vanishingly rare;
          # any non-zero count indicates a posterior-fitting issue worth
          # investigating.
          n_neg_eps <- sum((1 + results[[eps_col]]) <= 0, na.rm = TRUE)
          if (n_neg_eps > 0L && verbose) {
               message(sprintf("  Warning (%s): %d sample(s) have (1 + mu_j_epidemic_factor) <= 0 — pathological epsilon posterior",
                               iso, n_neg_eps))
          }

          # Surveillance CFR (deaths-among-reported-cases). Domain [0,1].
          results[[paste0("cfr_baseline_", iso)]] <- .clamp01(
               mu_eff_end *
                    results$rho_deaths *
                    results$chi_endemic /
                    results$rho
          )
          results[[paste0("cfr_epidemic_", iso)]] <- .clamp01(
               mu_eff_epi *
                    results$rho_deaths *
                    results$chi_epidemic /
                    results$rho
          )
          n_added <- n_added + 2L

          # Clinical (per-episode) CFR. 1 - exp(-mu_eff / gamma_1); always [0,1].
          if (has_gamma1) {
               .per_ep <- function(mu_eff) {
                    val <- 1 - exp(-mu_eff / results$gamma_1)
                    .clamp01(val)
               }
               results[[paste0("cfr_clinical_baseline_", iso)]] <- .per_ep(mu_eff_end)
               results[[paste0("cfr_clinical_epidemic_", iso)]] <- .per_ep(mu_eff_epi)
               n_added <- n_added + 2L
          }
          n_locs_done <- n_locs_done + 1L

          # Sanity check: warn if cfr_epidemic > 50% (cholera surveillance
          # reported-CFR rarely exceeds 30% even in untreated outbreaks).
          # Common cause: pathological epidemic_factor draws or near-zero rho.
          ce <- results[[paste0("cfr_epidemic_", iso)]]
          n_extreme <- sum(ce > 0.5, na.rm = TRUE)
          if (n_extreme > 0L && verbose) {
               message(sprintf("  Warning (%s): %d/%d cfr_epidemic samples > 0.5 — review epidemic_factor / rho posteriors",
                               iso, n_extreme, sum(is.finite(ce))))
          }
          # Parallel sanity check for clinical (per-episode) epidemic CFR.
          # A per-episode lethality > 50% indicates mu_eff/gamma_1 > 0.69
          # (death hazard exceeds recovery hazard over the symptomatic
          # period) — biologically possible in untreated severe outbreaks
          # but warrants a review of the gamma_1 / mu_j_baseline posterior.
          if (has_gamma1) {
               cce <- results[[paste0("cfr_clinical_epidemic_", iso)]]
               n_clin_extreme <- sum(cce > 0.5, na.rm = TRUE)
               if (n_clin_extreme > 0L && verbose) {
                    message(sprintf("  Warning (%s): %d/%d cfr_clinical_epidemic samples > 0.5 — biologically extreme, audit gamma_1 / mu posteriors",
                                    iso, n_clin_extreme, sum(is.finite(cce))))
               }
          }
     }

     if (verbose && n_added > 0L) {
          message(sprintf("  Added %d implied-CFR columns (%s) for %d locations",
                          n_added,
                          if (has_gamma1) "cfr_baseline, cfr_epidemic, cfr_clinical_baseline, cfr_clinical_epidemic"
                                          else "cfr_baseline, cfr_epidemic",
                          n_locs_done))
     }

     results
}
