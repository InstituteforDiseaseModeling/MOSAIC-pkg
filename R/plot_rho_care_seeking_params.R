#' Plot Rho (Care-Seeking Rate) Prior Estimation
#'
#' Generates a two-panel figure summarising the empirical anchors (Wiens et al.
#' 2025 general diarrhea + severe-diarrhea/cholera strata) and the fitted Beta
#' prior for the rho parameter (care-seeking rate).
#'
#' Panel A - dot-range plot of the two Wiens 2025 pooled estimates with their
#' 95% CIs. Random-effects pooling combines them as described in
#' \code{\link{get_rho_care_seeking_params}}.
#'
#' Panel B - fitted Beta density curve with the 2.5th, 50th, and 97.5th
#' percentiles annotated.
#'
#' @param PATHS A list of paths from \code{get_paths()}. Must include:
#' \itemize{
#'   \item \strong{MODEL_INPUT}: directory containing \code{param_rho_care_seeking.csv}.
#'   \item \strong{DOCS_FIGURES}: directory where the PNG will be saved.
#' }
#' @export

plot_rho_care_seeking_params <- function(PATHS) {

     # -----------------------------------------------------------------------
     # 1. Wiens 2025 anchors (mirrors get_rho_care_seeking_params)
     # -----------------------------------------------------------------------
     logit  <- function(p) log(p / (1 - p))
     ilogit <- function(x) 1 / (1 + exp(-x))

     strata <- data.frame(
          label  = c("Wiens 2025\ngeneral diarrhea\n(n=122 obs)",
                     "Wiens 2025\nsevere + cholera\n(n=22 obs)"),
          r      = c(0.299, 0.586),
          ci_lo  = c(0.253, 0.399),
          ci_hi  = c(0.351, 0.752),
          source = c("Wiens 2025 (general)", "Wiens 2025 (severe+cholera)"),
          stringsAsFactors = FALSE
     )

     # Compute pooled estimate for the reference line
     strata$logit_r  <- logit(strata$r)
     strata$se_logit <- (logit(strata$ci_hi) - logit(strata$ci_lo)) / (2 * 1.96)
     strata$vi       <- strata$se_logit^2
     wi_fe    <- 1 / strata$vi
     theta_fe <- sum(wi_fe * strata$logit_r) / sum(wi_fe)
     Q        <- sum(wi_fe * (strata$logit_r - theta_fe)^2)
     k        <- nrow(strata)
     c_denom  <- sum(wi_fe) - sum(wi_fe^2) / sum(wi_fe)
     tau2     <- max(0, (Q - (k - 1)) / c_denom)
     w_re     <- 1 / (strata$vi + tau2)
     pooled_mean <- ilogit(sum(w_re * strata$logit_r) / sum(w_re))
     strata$rel_weight <- w_re / sum(w_re)

     # -----------------------------------------------------------------------
     # 2. Load fitted Beta parameters from saved CSV
     # -----------------------------------------------------------------------
     param_path <- file.path(PATHS$MODEL_INPUT, "param_rho_care_seeking.csv")
     param_df   <- utils::read.csv(param_path, stringsAsFactors = FALSE)

     shape1 <- param_df$parameter_value[param_df$parameter_name == "shape1"]
     shape2 <- param_df$parameter_value[param_df$parameter_name == "shape2"]

     beta_mean <- shape1 / (shape1 + shape2)
     beta_q025 <- stats::qbeta(0.025, shape1, shape2)
     beta_q975 <- stats::qbeta(0.975, shape1, shape2)

     # -----------------------------------------------------------------------
     # 3. Panel A - dot-range plot of empirical anchors
     # -----------------------------------------------------------------------
     src_colours <- c(
          "Wiens 2025 (general)"        = "#3498db",
          "Wiens 2025 (severe+cholera)" = "#e67e22"
     )

     p_a <- ggplot2::ggplot(strata,
               ggplot2::aes(x = r, y = label,
                            colour = source, size = rel_weight)) +
          ggplot2::geom_vline(xintercept = pooled_mean, linetype = "dashed",
                              colour = "grey40", linewidth = 0.5) +
          ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lo, xmax = ci_hi),
                                  height = 0.2, linewidth = 0.7) +
          ggplot2::geom_point() +
          ggplot2::scale_colour_manual(values = src_colours, name = "Stratum") +
          ggplot2::scale_size_continuous(range = c(3, 6),
                                         name = "Relative weight (RE)",
                                         labels = scales::percent_format(accuracy = 1)) +
          ggplot2::scale_x_continuous(
               limits = c(0, 1),
               breaks = seq(0, 1, 0.2),
               expand = ggplot2::expansion(mult = c(0.01, 0.05))
          ) +
          ggplot2::labs(
               title = "A",
               x     = expression("Care-seeking rate (" * rho * ")"),
               y     = NULL
          ) +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(
               legend.position    = "right",
               panel.grid.major.y = ggplot2::element_blank(),
               panel.grid.minor   = ggplot2::element_blank(),
               panel.grid.major.x = ggplot2::element_line(colour = "grey85", linewidth = 0.25),
               axis.text.y        = ggplot2::element_text(size = 9),
               plot.margin        = ggplot2::unit(c(0.25, 0.25, 0, 0), "inches")
          )

     # -----------------------------------------------------------------------
     # 4. Panel B - fitted Beta density
     # -----------------------------------------------------------------------
     x_seq   <- seq(0, 1, length.out = 2000)
     df_beta <- data.frame(x = x_seq, y = stats::dbeta(x_seq, shape1, shape2))

     ci_txt <- sprintf(
          "Beta(%.2f, %.2f)\nmean = %.2f\n95%% CI: [%.2f, %.2f]\nESS = %.1f",
          shape1, shape2, beta_mean, beta_q025, beta_q975, shape1 + shape2
     )

     p_b <- ggplot2::ggplot(df_beta, ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_ribbon(
               data = subset(df_beta, x >= beta_q025 & x <= beta_q975),
               ggplot2::aes(ymin = 0, ymax = y),
               fill = "#3498db", alpha = 0.25
          ) +
          ggplot2::geom_line(linewidth = 1, colour = "#2c3e50") +
          ggplot2::geom_vline(xintercept = c(beta_q025, beta_q975),
                              linetype = "dashed", colour = "grey40", linewidth = 0.4) +
          ggplot2::geom_vline(xintercept = beta_mean,
                              linetype = "solid", colour = "grey30", linewidth = 0.6) +
          ggplot2::annotate("text",
                            x = min(beta_q975 + 0.05, 0.95), y = max(df_beta$y) * 0.85,
                            label = ci_txt, hjust = 0, size = 3.2, colour = "#2c3e50") +
          ggplot2::scale_x_continuous(
               limits = c(0, 1),
               breaks = seq(0, 1, 0.2),
               expand = ggplot2::expansion(mult = c(0.01, 0.05))
          ) +
          ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.005, 0.05))) +
          ggplot2::labs(
               title = "B",
               x     = expression("Care-seeking rate (" * rho * ")"),
               y     = "Density"
          ) +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(
               panel.grid.major.y = ggplot2::element_blank(),
               panel.grid.minor   = ggplot2::element_blank(),
               panel.grid.major.x = ggplot2::element_line(colour = "grey85", linewidth = 0.25),
               plot.margin        = ggplot2::unit(c(0, 0.25, 0.25, 0), "inches")
          )

     # -----------------------------------------------------------------------
     # 5. Combine panels and save
     # -----------------------------------------------------------------------
     combo <- cowplot::plot_grid(p_a, p_b, ncol = 1, rel_heights = c(0.9, 1), align = "v")
     print(combo)

     plot_path <- file.path(PATHS$DOCS_FIGURES, "rho_care_seeking_prior.png")
     ggplot2::ggsave(plot_path, plot = combo, width = 9, height = 9, dpi = 300)
     message(paste("Plot saved to:", plot_path))

     invisible(combo)
}
