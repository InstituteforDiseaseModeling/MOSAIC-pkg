#' Estimate Beta Prior for Rho (Care-Seeking Rate) and Save Parameter Data Frame
#'
#' This function encodes the prior on rho (the care-seeking rate: probability
#' that a true symptomatic cholera infection presents to the surveillance
#' pipeline as a suspected case) by random-effects pooling of the TWO
#' case-definition strata reported by Wiens et al. 2025 (PMC12013865):
#' general diarrhea and severe-diarrhea + cholera.
#'
#' @description
#' \strong{Why pool the two Wiens strata?}
#'
#' Symptomatic cholera in MOSAIC's setting spans the full severity spectrum
#' (mild watery diarrhea to severe rapid dehydration). The two Wiens 2025
#' meta-analytic strata bracket that spectrum:
#'
#' \itemize{
#'   \item \strong{General diarrhea} (3+ loose/liquid stools): 29.9\%
#'         (95\% CI [25.3, 35.1]) from 122 observations. Represents
#'         care-seeking for the broader population of diarrheal episodes,
#'         which is appropriate for the mild-to-moderate tail of
#'         symptomatic cholera (the majority of cases by count).
#'   \item \strong{Severe diarrhea + cholera}: 58.6\% (95\% CI [39.9, 75.2])
#'         from 22 observations. Represents care-seeking when symptoms are
#'         severe enough to be flagged as "severe" or specifically cholera,
#'         which is appropriate for the severe tail of symptomatic cholera.
#' }
#'
#' Anchoring on the severe+cholera stratum ALONE biases rho upward (it
#' represents only the severe tail and is dominated by outbreak-response
#' settings with enhanced care-seeking, e.g. the Haiti 2010-19 outbreak
#' which contributes 50\% of underlying rows). Anchoring on general diarrhea
#' ALONE biases rho downward (the broader category includes many mild,
#' self-resolving episodes that don't reflect cholera-specific severity).
#' Random-effects pooling of the two strata produces an estimate that
#' honestly reflects the severity spectrum and its substantial heterogeneity.
#'
#' \strong{Why not include GEMS Nasrin 2013 as a separate co-anchor?}
#'
#' Earlier versions (through v0.32.5) pooled GEMS pediatric moderate-to-severe
#' diarrhea (4 SSA sites, 12 stratum-level estimates) WITH the Wiens
#' severe+cholera pooled estimate. That pooling had three problems:
#' (1) GEMS measures pediatric MSD care-seeking, a different population than
#' MOSAIC's all-ages cholera; (2) 12 GEMS strata vs. 1 Wiens meta-analytic
#' summary was upside-down dimensionally; (3) Wiens 2025 already includes
#' GEMS-derived data (6 of its Study IDs are tagged GEMS/HUAS), so the
#' direct GEMS entries partially double-counted the same source populations.
#' Dropping GEMS and using Wiens's two strata directly resolves all three.
#'
#' \strong{Methodology.}
#' Random-effects meta-analysis on the logit scale (DerSimonian-Laird tau^2)
#' with the two Wiens strata as the unit of pooling. The 95\% CI of the
#' pooled mean is then fit to a Beta distribution via
#' \code{\link[propvacc]{get_beta_params}}.
#'
#' \strong{Geographic provenance (severe+cholera stratum).} Of 23 underlying
#' studies: 16 SSA, 3 Latin America/Caribbean (mostly Haiti 2010-19), 2
#' Bangladesh, 1 MENA, 1 South Asia (non-BGD). Predominantly SSA + outbreak
#' settings - the regime MOSAIC calibrates.
#'
#' @param PATHS A list containing paths for saving outputs. Typically generated
#'   by \code{get_paths()} and must include:
#' \itemize{
#'   \item \strong{MODEL_INPUT}: Directory where the parameter CSV will be saved.
#' }
#' @return A data frame with one row per parameter (shape1, shape2, mean) for the
#'   rho Beta prior, invisibly. Also writes
#'   \code{param_rho_care_seeking.csv} to \code{PATHS$MODEL_INPUT}.
#' @export

get_rho_care_seeking_params <- function(PATHS) {

     # -----------------------------------------------------------------------
     # 1. Wiens 2025 stratum-level pooled estimates
     #    Source: Wiens et al. 2025 (PMC12013865) - "Care-seeking for diarrhea
     #    in low- and middle-income countries: a systematic review and
     #    meta-analysis". The paper reports separate pooled estimates by
     #    case-definition stratum; we use general diarrhea AND
     #    severe-diarrhea + cholera to bracket the cholera severity spectrum.
     # -----------------------------------------------------------------------
     strata <- data.frame(
          name   = c("general diarrhea", "severe diarrhea + cholera"),
          r      = c(0.299, 0.586),
          ci_lo  = c(0.253, 0.399),
          ci_hi  = c(0.351, 0.752),
          n_obs  = c(122L, 22L),
          source = "Wiens et al. 2025 (PMC12013865)",
          stringsAsFactors = FALSE
     )

     # -----------------------------------------------------------------------
     # 2. Random-effects pool on the logit scale (DerSimonian-Laird)
     # -----------------------------------------------------------------------
     logit  <- function(p) log(p / (1 - p))
     ilogit <- function(x) 1 / (1 + exp(-x))

     strata$logit_r  <- logit(strata$r)
     strata$se_logit <- (logit(strata$ci_hi) - logit(strata$ci_lo)) / (2 * 1.96)
     strata$vi       <- strata$se_logit^2

     wi_fe    <- 1 / strata$vi
     theta_fe <- sum(wi_fe * strata$logit_r) / sum(wi_fe)
     Q        <- sum(wi_fe * (strata$logit_r - theta_fe)^2)
     k        <- nrow(strata)
     c_denom  <- sum(wi_fe) - sum(wi_fe^2) / sum(wi_fe)
     tau2     <- max(0, (Q - (k - 1)) / c_denom)

     w_re         <- 1 / (strata$vi + tau2)
     pooled_logit <- sum(w_re * strata$logit_r) / sum(w_re)
     pooled_se    <- sqrt(1 / sum(w_re))
     pooled_mean  <- ilogit(pooled_logit)
     pooled_ci_lo <- ilogit(pooled_logit - 1.96 * pooled_se)
     pooled_ci_hi <- ilogit(pooled_logit + 1.96 * pooled_se)

     message(sprintf(
          "Wiens 2-stratum RE pool: Q=%.2f (df=%d), tau^2=%.3f, I^2=%.0f%%",
          Q, k - 1, tau2, 100 * max(0, Q - (k - 1)) / max(Q, 1e-9)
     ))
     message(sprintf(
          "Pooled rho: mean=%.3f, 95%% CI of pooled mean=[%.3f, %.3f]",
          pooled_mean, pooled_ci_lo, pooled_ci_hi
     ))

     # -----------------------------------------------------------------------
     # 3. Fit Beta prior to the 95% CI of the pooled mean
     # -----------------------------------------------------------------------
     prm <- propvacc::get_beta_params(
          quantiles = c(0.025, 0.50, 0.975),
          probs     = c(pooled_ci_lo, pooled_mean, pooled_ci_hi)
     )
     fitted_mean <- prm$shape1 / (prm$shape1 + prm$shape2)
     fitted_ess  <- prm$shape1 + prm$shape2

     message(sprintf(
          "Fitted Beta prior for rho: shape1=%.4f, shape2=%.4f (mean=%.3f, ESS=%.1f)",
          prm$shape1, prm$shape2, fitted_mean, fitted_ess
     ))

     # -----------------------------------------------------------------------
     # 4. Build and save parameter data frame
     # -----------------------------------------------------------------------
     desc <- "Care-seeking rate (rho): Wiens et al. 2025 random-effects pool of general diarrhea + severe-diarrhea/cholera strata"

     param_df <- make_param_df(
          variable_name = "rho",
          variable_description = c(desc, desc, desc),
          parameter_distribution = "beta",
          parameter_name  = c("shape1", "shape2", "mean"),
          parameter_value = c(prm$shape1, prm$shape2, pooled_mean)
     )

     param_path <- file.path(PATHS$MODEL_INPUT, "param_rho_care_seeking.csv")
     utils::write.csv(param_df, param_path, row.names = FALSE)
     message(paste("Parameter data frame for rho saved to:", param_path))

     # Return data for downstream use (e.g. plotting)
     invisible(list(
          strata     = strata,
          pooled     = list(mean = pooled_mean, ci_lo = pooled_ci_lo, ci_hi = pooled_ci_hi,
                            tau2 = tau2, Q = Q),
          beta_shape = list(shape1 = prm$shape1, shape2 = prm$shape2),
          beta_mean  = fitted_mean,
          beta_ess   = fitted_ess,
          param_df   = param_df
     ))
}
