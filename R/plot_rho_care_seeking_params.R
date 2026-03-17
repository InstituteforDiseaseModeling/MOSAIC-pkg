#' Plot Rho (Care-Seeking Rate) Prior Estimation
#'
#' Generates a two-panel figure summarising the empirical data and fitted Beta
#' prior for the rho parameter (care-seeking rate).
#'
#' Panel A — dot-range plot of all per-stratum GEMS estimates and the Wiens et al.
#' 2025 meta-analytic severe-diarrhea estimate, with points sized by relative
#' inverse-variance weight.
#'
#' Panel B — fitted Beta density curve with the 5th, 50th, and 95th percentiles
#' annotated.
#'
#' @param PATHS A list of paths from \code{get_paths()}. Must include:
#' \itemize{
#'   \item \strong{MODEL_INPUT}: directory containing \code{param_rho_care_seeking.csv}.
#'   \item \strong{DOCS_FIGURES}: directory where the PNG will be saved.
#' }
#' @export

plot_rho_care_seeking_params <- function(PATHS) {

     # -----------------------------------------------------------------------
     # 1. Re-derive pooled estimate from raw data (mirrors get_rho_care_seeking_params)
     # -----------------------------------------------------------------------
     logit  <- function(p) log(p / (1 - p))
     ilogit <- function(x) 1 / (1 + exp(-x))

     gems <- data.frame(
          site = c(
               "The Gambia", "The Gambia", "The Gambia",
               "Mali",       "Mali",       "Mali",
               "Mozambique", "Mozambique", "Mozambique",
               "Kenya",      "Kenya",      "Kenya"
          ),
          age_stratum = rep(c("0-11 mo", "12-23 mo", "24-59 mo"), 4),
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

     wiens <- data.frame(
          site        = "Systematic review",
          age_stratum = "all ages",
          r     = 0.586,
          ci_lo = 0.399,
          ci_hi = 0.752,
          source = "Wiens et al. 2025",
          stringsAsFactors = FALSE
     )

     all_data <- rbind(gems, wiens)

     all_data$logit_r  <- logit(all_data$r)
     all_data$logit_lo <- logit(all_data$ci_lo)
     all_data$logit_hi <- logit(all_data$ci_hi)
     all_data$se_logit <- (all_data$logit_hi - all_data$logit_lo) / (2 * 1.96)
     all_data$vi       <- all_data$se_logit^2

     # DerSimonian-Laird tau^2
     wi_fe    <- 1 / all_data$vi
     theta_fe <- sum(wi_fe * all_data$logit_r) / sum(wi_fe)
     k        <- nrow(all_data)
     Q        <- sum(wi_fe * (all_data$logit_r - theta_fe)^2)
     c_denom  <- sum(wi_fe) - sum(wi_fe^2) / sum(wi_fe)
     tau2     <- max(0, (Q - (k - 1)) / c_denom)

     all_data$weight     <- 1 / (all_data$vi + tau2)
     total_weight        <- sum(all_data$weight)
     all_data$rel_weight <- all_data$weight / total_weight

     pooled_logit <- sum(all_data$weight * all_data$logit_r) / total_weight
     pooled_se    <- sqrt(1 / total_weight)
     t_crit       <- qt(0.95, df = k - 1)
     pred_se      <- sqrt(tau2 + pooled_se^2)
     pooled_q05   <- ilogit(pooled_logit - t_crit * pred_se)
     pooled_q50   <- ilogit(pooled_logit)
     pooled_q95   <- ilogit(pooled_logit + t_crit * pred_se)

     # -----------------------------------------------------------------------
     # 2. Load fitted Beta parameters from saved CSV
     # -----------------------------------------------------------------------
     param_path <- file.path(PATHS$MODEL_INPUT, "param_rho_care_seeking.csv")
     param_df   <- utils::read.csv(param_path, stringsAsFactors = FALSE)

     shape1 <- param_df$parameter_value[param_df$parameter_name == "shape1"]
     shape2 <- param_df$parameter_value[param_df$parameter_name == "shape2"]

     # -----------------------------------------------------------------------
     # 3. Panel A — dot-range plot of input data
     # -----------------------------------------------------------------------

     # Create label: site + age stratum
     all_data$label <- ifelse(
          all_data$site == "Systematic review",
          paste0(all_data$source, "\n(severe diarrhea/cholera, N=22)"),
          paste0(all_data$site, "\n", all_data$age_stratum)
     )

     # Order by point estimate for visual clarity
     all_data$label <- factor(all_data$label, levels = all_data$label[order(all_data$r)])

     # Source colour palette
     src_colours <- c(
          "GEMS (Nasrin et al. 2013)" = "#3498db",
          "Wiens et al. 2025"         = "#e67e22"
     )

     p_a <- ggplot2::ggplot(all_data,
               ggplot2::aes(x = r, y = label,
                            colour = source, size = rel_weight)) +
          ggplot2::geom_vline(xintercept = pooled_q50, linetype = "dashed",
                              colour = "grey40", linewidth = 0.5) +
          ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lo, xmax = ci_hi),
                                  height = 0.25, linewidth = 0.5) +
          ggplot2::geom_point() +
          ggplot2::scale_colour_manual(values = src_colours, name = "Source") +
          ggplot2::scale_size_continuous(range = c(2, 6),
                                         name = "Relative weight",
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
               legend.position   = "right",
               panel.grid.major.y = ggplot2::element_blank(),
               panel.grid.minor   = ggplot2::element_blank(),
               panel.grid.major.x = ggplot2::element_line(colour = "grey85", linewidth = 0.25),
               axis.text.y        = ggplot2::element_text(size = 8),
               plot.margin        = ggplot2::unit(c(0.25, 0.25, 0, 0), "inches")
          )

     # -----------------------------------------------------------------------
     # 4. Panel B — fitted Beta density
     # -----------------------------------------------------------------------
     x_seq   <- seq(0, 1, length.out = 2000)
     df_beta <- data.frame(x = x_seq, y = dbeta(x_seq, shape1, shape2))

     ci_txt <- sprintf(
          "Beta(%.2f, %.2f)\nmean = %.2f\n5th-95th: [%.2f, %.2f]",
          shape1, shape2, pooled_q50, pooled_q05, pooled_q95
     )

     p_b <- ggplot2::ggplot(df_beta, ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_ribbon(
               data = subset(df_beta, x >= pooled_q05 & x <= pooled_q95),
               ggplot2::aes(ymin = 0, ymax = y),
               fill = "#3498db", alpha = 0.25
          ) +
          ggplot2::geom_line(linewidth = 1, colour = "#2c3e50") +
          ggplot2::geom_vline(xintercept = c(pooled_q05, pooled_q95),
                              linetype = "dashed", colour = "grey40", linewidth = 0.4) +
          ggplot2::geom_vline(xintercept = pooled_q50,
                              linetype = "solid", colour = "grey30", linewidth = 0.6) +
          ggplot2::annotate("text",
                            x = min(pooled_q95 + 0.05, 0.95), y = max(df_beta$y) * 0.85,
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
     combo <- cowplot::plot_grid(p_a, p_b, ncol = 1, rel_heights = c(1.6, 1), align = "v")
     print(combo)

     plot_path <- file.path(PATHS$DOCS_FIGURES, "rho_care_seeking_prior.png")
     ggplot2::ggsave(plot_path, plot = combo, width = 9, height = 10, dpi = 300)
     message(paste("Plot saved to:", plot_path))

     invisible(combo)
}
