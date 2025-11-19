#' Plot Vibrio Decay Rate as a Function of Environmental Suitability
#'
#' This function generates a plot that visualizes the suitability-dependent decay rate of
#' *V. cholerae* survival based on climate-driven environmental suitability (\eqn{\psi_{jt}}).
#' The figure compares five transformation types for survival time using the cumulative
#' Beta distribution:
#' \itemize{
#'   \item \strong{Linear}: \eqn{f(\psi_{jt}) = \text{pbeta}(\psi_{jt} \mid s_1 = 1, s_2 = 1)}
#'   \item \strong{Concave}: \eqn{f(\psi_{jt}) = \text{pbeta}(\psi_{jt} \mid s_1 = 1, s_2 = 5)}
#'   \item \strong{Convex}: \eqn{f(\psi_{jt}) = \text{pbeta}(\psi_{jt} \mid s_1 = 5, s_2 = 1)}
#'   \item \strong{Sigmoidal}: \eqn{f(\psi_{jt}) = \text{pbeta}(\psi_{jt} \mid s_1 = 5, s_2 = 5)}
#'   \item \strong{Arcsine}: \eqn{f(\psi_{jt}) = \text{pbeta}(\psi_{jt} \mid s_1 = 0.5, s_2 = 0.5)}
#' }
#'
#' The primary y-axis shows survival time in days. The secondary y-axis shows the decay
#' rate \eqn{\delta_{jt} = 1 / \text{days}(\psi_{jt})}. Horizontal dashed lines indicate
#' the minimum and maximum bounds on survival time. The output plot is saved as a PNG
#' file in the directory specified by \code{PATHS$DOCS_FIGURES}.
#'
#' @param PATHS A list containing path locations for saving output. Must include:
#'   \itemize{
#'     \item \code{DOCS_FIGURES}: File path to the directory for saving the plot
#'   }
#' @param decay_days_short Numeric. Minimum survival time (in days). Default is 3.
#' @param decay_days_long Numeric. Maximum survival time (in days). Default is 90.
#'
#' @return Saves a PNG file to \code{PATHS$DOCS_FIGURES}. Invisibly returns the ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' PATHS <- get_paths()
#' plot_vibrio_decay_rate(PATHS)
#' plot_vibrio_decay_rate(PATHS, decay_days_short = 2, decay_days_long = 100)
#' }
plot_vibrio_decay_rate <- function(PATHS, decay_days_short = 3, decay_days_long = 90) {

     requireNamespace("ggplot2")

     output_file <- file.path(PATHS$DOCS_FIGURES, "vibrio_decay_rate.png")

     f <- function(psi, days_short, days_long, shape1, shape2) {
          days_short + pbeta(psi, shape1, shape2) * (days_long - days_short)
     }

     psi_vals <- seq(0, 1, length.out = 100)

     model_info <- data.frame(
          shape1 = c(1, 1, 5, 5, 0.5),
          shape2 = c(1, 5, 1, 5, 0.5),
          Model = c("Linear", "Concave", "Convex", "Sigmoidal", "Arcsine"),
          stringsAsFactors = FALSE
     )

     # Ensure Model is a factor with the desired order for the legend.
     model_info$Model <- factor(model_info$Model, levels = c("Linear", "Concave", "Convex", "Sigmoidal", "Arcsine"))

     df <- do.call(rbind, lapply(1:nrow(model_info), function(i) {
          data.frame(
               psi = psi_vals,
               Days = f(psi_vals, decay_days_short, decay_days_long,
                        model_info$shape1[i], model_info$shape2[i]),
               Model = model_info$Model[i],
               s1 = model_info$shape1[i],
               s2 = model_info$shape2[i]
          )
     }))

     legend_labels <- list(
          expression("Linear (s" [1] * " = 1, " * s[2] * " = 1)"),
          expression("Concave (s" [1] * " = 1, " * s[2] * " = 5)"),
          expression("Convex (s" [1] * " = 5, " * s[2] * " = 1)"),
          expression("Sigmoidal (s" [1] * " = 5, " * s[2] * " = 5)"),
          expression("Arcsine (s" [1] * " = 0.5, " * s[2] * " = 0.5)")
     )

     primary_breaks <- c(0, 20, 40, 60, 80)
     secondary_breaks <- round(1 / primary_breaks, 4)
     secondary_breaks <- secondary_breaks[is.finite(secondary_breaks)]

     png(filename = output_file, width = 9.5, height = 5, units = "in", res = 600)

     p <- ggplot(df, aes(x = psi, y = Days, color = Model)) +
          geom_line(linewidth = 2) +
          scale_color_manual(
               values = c("Linear" = "#1f77b4",    # Dark blue
                          "Concave" = "#2ca02c",   # Dark green
                          "Convex"  = "#d62728",   # Dark red
                          "Sigmoidal" = "#9467bd",  # Dark purple
                          "Arcsine" = "#17becf"),   # Bold gray
               labels = legend_labels,
               name = "Transformation"
          ) +
          scale_y_continuous(
               name = "Survival Days",
               limits = c(0, decay_days_long),
               breaks = primary_breaks,
               sec.axis = sec_axis(~ 1 / .,
                                   name = expression("Suitability-dependent decay rate (" * delta[jt] * ")"),
                                   breaks = round(1 / primary_breaks[primary_breaks != 0], 4))
          ) +
          labs(
               x = expression("Estimated environmental suitability (" * psi[jt] * ")")
          ) +
          theme_minimal(base_size = 16) +
          theme(
               panel.grid.minor = element_blank(),
               axis.title.x = element_text(margin = margin(t = 10)),
               axis.title.y = element_text(margin = margin(r = 10)),
               axis.title.y.right = element_text(margin = margin(l = 10))
          ) +
          geom_hline(yintercept = decay_days_short, linetype = "dashed", color = "black") +
          geom_hline(yintercept = decay_days_long, linetype = "dashed", color = "black") +
          annotate("text", x = 0.7, y = decay_days_short,
                   label = bquote(delta[max] == 1/.(round(1 / decay_days_short, 2)) ~ "=" ~ .(decay_days_short) ~ "days"),
                   color = "black", hjust = 0, vjust = -0.5, size = 4) +
          annotate("text", x = 0.3, y = decay_days_long,
                   label = bquote(delta[min] == 1/.(round(1 / decay_days_long, 2)) ~ "=" ~ .(decay_days_long) ~ "days"),
                   color = "black", hjust = 1, vjust = 1.5, size = 4) +
          coord_cartesian(clip = "off")

     print(p)
     dev.off()
     message("Shedding rate plot saved to: ", output_file)
     invisible(p)
}
