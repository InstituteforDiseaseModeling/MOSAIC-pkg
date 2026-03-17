#' Estimate Beta Prior for Rho (Care-Seeking Rate) and Save Parameter Data Frame
#'
#' This function encodes empirical estimates of care-seeking rates for acute
#' watery diarrhea / cholera in sub-Saharan Africa, pools them using inverse-variance
#' weighting, and fits a Beta prior distribution via
#' \code{\link[propvacc]{get_beta_params}}.
#'
#' @description
#' Two data sources are combined:
#'
#' \strong{GEMS (Nasrin et al. 2013, PMC3748499):}
#' The Global Enteric Multicenter Study measured the proportion of moderate-to-severe
#' diarrhea (MSD) cases reaching sentinel health centres across four African sites
#' (The Gambia, Mali, Mozambique, Kenya) by age stratum (0-11 mo, 12-23 mo, 24-59 mo).
#' These are the closest available SSA-specific empirical estimates of facility-based
#' care-seeking for severe enteric disease.
#'
#' \strong{Wiens et al. 2025 (PMC12013865):}
#' A systematic review of 188 studies providing meta-analytic estimates of the
#' proportion of diarrhea episodes reaching formal healthcare in LMICs.
#' The severe diarrhea / cholera-specific estimate (58.6%, 95% CI 39.9-75.2%)
#' is used here because it most closely matches the cholera clinical spectrum.
#'
#' A weighted geometric mean of all stratum-level estimates is computed using
#' inverse-variance weights derived from the 95% confidence intervals.  The
#' resulting 5th, 50th, and 95th percentile summary statistics are passed to
#' \code{propvacc::get_beta_params()} to fit the Beta(shape1, shape2) prior.
#'
#' @param PATHS A list containing paths for saving outputs.  Typically generated
#'   by \code{get_paths()} and must include:
#' \itemize{
#'   \item \strong{MODEL_INPUT}: Directory where the parameter CSV will be saved.
#' }
#' @return A data frame with one row per parameter (shape1, shape2, mean) for the
#'   rho Beta prior, invisibly.  Also writes
#'   \code{param_rho_care_seeking.csv} to \code{PATHS$MODEL_INPUT}.
#' @export

get_rho_care_seeking_params <- function(PATHS) {

     # -----------------------------------------------------------------------
     # 1. GEMS data (Nasrin et al. 2013, Table 6, PMC3748499)
     #    r = proportion of MSD episodes reaching a sentinel health centre
     #    Columns: site, age_stratum, r (point estimate), ci_lower, ci_upper
     # -----------------------------------------------------------------------
     gems <- data.frame(
          site = c(
               "The Gambia", "The Gambia", "The Gambia",
               "Mali",       "Mali",       "Mali",
               "Mozambique", "Mozambique", "Mozambique",
               "Kenya",      "Kenya",      "Kenya"
          ),
          age_stratum = rep(c("0-11mo", "12-23mo", "24-59mo"), 4),
          r      = c(0.35, 0.26, 0.22,
                     0.22, 0.17, 0.09,
                     0.56, 0.64, 0.33,
                     0.20, 0.19, 0.16),
          ci_lo  = c(0.28, 0.21, 0.16,
                     0.16, 0.10, 0.03,
                     0.39, 0.45, 0.11,
                     0.18, 0.17, 0.14),
          ci_hi  = c(0.42, 0.32, 0.30,
                     0.30, 0.28, 0.28,
                     0.76, 0.81, 0.75,
                     0.23, 0.21, 0.18),
          source = "GEMS (Nasrin et al. 2013)",
          stringsAsFactors = FALSE
     )

     # -----------------------------------------------------------------------
     # 2. Wiens et al. 2025 systematic review (PMC12013865)
     #    Severe diarrhea / cholera meta-analytic estimate
     # -----------------------------------------------------------------------
     wiens <- data.frame(
          site        = "Meta-analysis (22 observations)",
          age_stratum = "all ages",
          r     = 0.586,
          ci_lo = 0.399,
          ci_hi = 0.752,
          source = "Wiens et al. 2025",
          stringsAsFactors = FALSE
     )

     all_data <- rbind(gems, wiens)

     # -----------------------------------------------------------------------
     # 3. Random-effects meta-analysis on the log-odds scale
     #    (DerSimonian-Laird estimator)
     #
     #    Fixed-effects pooling would give Kenya's tight stratum-level CIs
     #    disproportionate weight, producing an implausibly precise prior.
     #    Random effects adds between-stratum heterogeneity variance (tau^2),
     #    giving each observation roughly equal influence when heterogeneity
     #    is large — appropriate for a prior that should span SSA settings.
     # -----------------------------------------------------------------------
     logit  <- function(p) log(p / (1 - p))
     ilogit <- function(x) 1 / (1 + exp(-x))

     all_data$logit_r  <- logit(all_data$r)
     all_data$logit_lo <- logit(all_data$ci_lo)
     all_data$logit_hi <- logit(all_data$ci_hi)
     all_data$se_logit <- (all_data$logit_hi - all_data$logit_lo) / (2 * 1.96)
     all_data$vi       <- all_data$se_logit^2   # within-study variance

     # Fixed-effects pooled estimate (step 1 of DL estimator)
     wi_fe            <- 1 / all_data$vi
     theta_fe         <- sum(wi_fe * all_data$logit_r) / sum(wi_fe)
     k                <- nrow(all_data)

     # Cochran's Q and DerSimonian-Laird tau^2
     Q       <- sum(wi_fe * (all_data$logit_r - theta_fe)^2)
     c_denom <- sum(wi_fe) - sum(wi_fe^2) / sum(wi_fe)
     tau2    <- max(0, (Q - (k - 1)) / c_denom)

     # Random-effects weights
     all_data$weight     <- 1 / (all_data$vi + tau2)
     total_weight        <- sum(all_data$weight)
     all_data$rel_weight <- all_data$weight / total_weight

     pooled_logit  <- sum(all_data$weight * all_data$logit_r) / total_weight
     pooled_se     <- sqrt(1 / total_weight)
     pooled_mean   <- ilogit(pooled_logit)

     # Use prediction interval (not confidence interval of the mean) as prior bounds.
     # The prediction interval represents where rho would fall for a new SSA setting,
     # which is the appropriate quantity to encode as a Bayesian prior.
     #   PI = pooled_logit +/- t_{k-1, 0.95} * sqrt(tau^2 + pooled_se^2)
     t_crit   <- qt(0.95, df = k - 1)
     pred_se  <- sqrt(tau2 + pooled_se^2)
     pooled_q05 <- ilogit(pooled_logit - t_crit * pred_se)
     pooled_q50 <- ilogit(pooled_logit)
     pooled_q95 <- ilogit(pooled_logit + t_crit * pred_se)

     message(sprintf("Heterogeneity: Q=%.2f (df=%d), tau^2=%.4f, I^2=%.1f%%",
                     Q, k - 1, tau2, 100 * (Q - (k - 1)) / Q))

     message(sprintf(
          "Pooled rho estimate: mean=%.3f, 5th=%.3f, 95th=%.3f",
          pooled_mean, pooled_q05, pooled_q95
     ))

     # -----------------------------------------------------------------------
     # 4. Fit Beta prior via propvacc::get_beta_params()
     # -----------------------------------------------------------------------
     prm <- propvacc::get_beta_params(
          quantiles = c(0.05, 0.50, 0.95),
          probs     = c(pooled_q05, pooled_q50, pooled_q95)
     )

     message(sprintf(
          "Fitted Beta prior for rho: shape1=%.4f, shape2=%.4f",
          prm$shape1, prm$shape2
     ))

     # -----------------------------------------------------------------------
     # 5. Build and save parameter data frame
     # -----------------------------------------------------------------------
     param_df <- make_param_df(
          variable_name = "rho",
          variable_description = c(
               "Care-seeking rate (rho): pooled point estimate (GEMS + Wiens 2025)",
               "Care-seeking rate (rho): pooled point estimate (GEMS + Wiens 2025)",
               "Care-seeking rate (rho): pooled point estimate (GEMS + Wiens 2025)"
          ),
          parameter_distribution = "beta",
          parameter_name  = c("shape1", "shape2", "mean"),
          parameter_value = c(prm$shape1, prm$shape2, pooled_mean)
     )

     param_path <- file.path(PATHS$MODEL_INPUT, "param_rho_care_seeking.csv")
     utils::write.csv(param_df, param_path, row.names = FALSE)
     message(paste("Parameter data frame for rho saved to:", param_path))

     # Return pooled data for downstream use (e.g. plotting)
     invisible(list(
          data       = all_data,
          pooled     = list(mean = pooled_mean, q05 = pooled_q05, q50 = pooled_q50, q95 = pooled_q95),
          beta_shape = list(shape1 = prm$shape1, shape2 = prm$shape2),
          param_df   = param_df
     ))
}
