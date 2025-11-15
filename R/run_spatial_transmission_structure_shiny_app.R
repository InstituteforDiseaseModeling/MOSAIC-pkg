#' Launch an interactive viewer for site-specific \eqn{\beta_{\text{hum}}}
#'
#' Display a Shiny dashboard that lets you explore the **global** human-to-human
#' transmission coefficient (\eqn{\beta_{\text{hum}}}) together with
#' location-specific modifiers, incidence, population size and WASH covariates.
#' The app is intended as a quick visual sanity-check of the output produced by
#' [`est_transmission_spatial_structure()`] and is **not** meant for automated
#' pipelines.
#'
#' @details
#' The slider *Global Beta* sets a baseline value
#' \eqn{\beta_{\text{hum}}^{\ast}}, while the slider *Variance Expansion
#' Factor* applies an exponent \eqn{\kappa} to each relative multiplier
#' \eqn{m_j}, producing the site-specific coefficient
#' \deqn{\beta_{\text{hum},j} = \beta_{\text{hum}}^{\ast} \, m_j^{\kappa}.}
#'
#' The main panel is organised as a 4 × 2 layout created with **patchwork**:
#' \describe{
#'   \item{Top row}{A histogram of the current \eqn{\beta_{\text{hum},j}}
#'     values; the dashed red line marks \eqn{\beta_{\text{hum}}^{\ast}}.}
#'   \item{Bottom row}{Four aligned strip plots, all ordered by
#'     \eqn{\beta_{\text{hum},j}}:
#'     \enumerate{
#'       \item Relative multipliers \eqn{m_j};
#'       \item Site-specific \eqn{\beta_{\text{hum},j}};
#'       \item Incidence per population (\eqn{I_j / N_j});
#'       \item WASH covariate value.}}
#' }
#'
#' @param mod A list produced by [`est_transmission_spatial_structure()`],
#'   containing at least
#'   \code{relative_multiplier}, \code{data}, and the columns
#'   \code{iso_code}, \code{incidence_total}, \code{population_size},
#'   \code{WASH} inside \code{mod$data}.
#'
#' @return (Invisibly) the `shiny.appobj` returned by
#'   [\link[shiny]{shinyApp}]—the function is called for its side effect of
#'   launching a Shiny app in the default browser or RStudio viewer.
#'
#' @section Side Effects:
#' Opens a Shiny window and blocks the R session while the app is running.
#'
#' @note This function requires the optional \code{shiny} package.
#'   Install with: \code{install.packages("shiny")}
#'
#' @import ggplot2
#' @import patchwork
#'
#' @examples
#' \dontrun{
#'   paths  <- list(MODEL_INPUT = tempdir())
#'   config <- MOSAIC::config_default[1:3]          # toy example
#'   fit    <- est_transmission_spatial_structure(paths, config)
#'
#'   ## Launch the interactive viewer (stop the app to regain the prompt):
#'   run_spatial_transmission_shiny_app(fit)
#' }
#' @export
#'

run_spatial_transmission_shiny_app <- function(mod) {
     # Check for optional shiny package
     if (!requireNamespace("shiny", quietly = TRUE)) {
          stop(
               "The 'shiny' package is required but not installed.\n",
               "Install with: install.packages('shiny')",
               call. = FALSE
          )
     }

     library(shiny)
     library(ggplot2)
     library(patchwork)

     rel_mult <- mod$relative_multiplier
     df_all   <- mod$data
     locs     <- df_all$iso_code
     inc_vec  <- df_all$incidence_total
     pop_vec  <- df_all$population_size
     wash_vec <- df_all$WASH

     ui <- fluidPage(
          titlePanel("Interactive β_hum Visualization"),
          sidebarLayout(
               sidebarPanel(
                    sliderInput("global_beta", "Global Beta:",
                                min = 0, max = 2, value = 0.5, step = 0.01),
                    sliderInput("variance_expansion", "Variance Expansion Factor:",
                                min = 0, max = 10, value = 1, step = 0.1)
               ),
               mainPanel(
                    plotOutput("combined_plot", height = "700px")
               )
          )
     )

     server <- function(input, output, session) {
          df <- reactive({
               data.frame(
                    location_name       = locs,
                    incidence_total     = inc_vec,
                    population_size     = pop_vec,
                    relative_multiplier = rel_mult,
                    beta_j_hum          = input$global_beta * (rel_mult^input$variance_expansion),
                    WASH                = wash_vec,
                    stringsAsFactors    = FALSE
               )
          })

          fixed <- reactive({
               max_mult <- max(rel_mult, na.rm = TRUE)
               lims     <- c(0, max(1, max_mult^10) * 2.01)
               list(lims = lims, breaks = pretty(lims, n = 6))
          })

          output$combined_plot <- renderPlot({
               d  <- df()
               fl <- fixed()

               # Top‐center: beta_hum histogram
               p1 <- ggplot(d, aes(x = beta_j_hum)) +
                    geom_histogram(bins = 25, fill = "dodgerblue", color = "white") +
                    geom_vline(xintercept = input$global_beta,
                               linetype = "dashed", color = "red2") +
                    scale_x_continuous(limits = fl$lims, breaks = fl$breaks) +
                    labs(title = "Histogram of β_hum",
                         x = expression(beta[hum]),
                         y = "Count") +
                    theme_minimal()

               # Bottom row, col1: spatial multipliers
               p4 <- ggplot(d, aes(x = relative_multiplier,
                                   y = reorder(location_name, beta_j_hum))) +
                    geom_point(size = 2.5, shape = 21, fill = 'aquamarine3') +
                    geom_vline(xintercept = 1, linetype = "solid", color = "black") +
                    labs(title = "Relative Multiplier",
                         x = "Multiplier",
                         y = "Location") +
                    theme_minimal() +
                    theme(axis.text.y = element_text(size=8))

               # Bottom row, col2: site‐specific β_hum
               p2 <- ggplot(d, aes(x = beta_j_hum,
                                   y = reorder(location_name, beta_j_hum))) +
                    geom_point(size = 3, shape = 22, fill = 'dodgerblue') +
                    geom_vline(xintercept = input$global_beta,
                               linetype = "dashed", color = "red2") +
                    scale_x_continuous(limits = fl$lims, breaks = fl$breaks) +
                    labs(title = "Site-specific β_hum",
                         x = expression(beta[hum]),
                         y = NULL) +
                    theme_minimal() +
                    theme(axis.text.y = element_blank())

               # Bottom row, col3: incidence per population
               p3 <- ggplot(d, aes(
                    x = incidence_total / population_size,
                    y = reorder(location_name, beta_j_hum)
               )) +
                    geom_bar(stat = "identity", fill = "bisque4", width = 0.7) +
                    labs(title = "Incidence per Population",
                         x = "Incidence/Population",
                         y = NULL) +
                    theme_minimal() +
                    theme(axis.text.y = element_blank())

               # Bottom row, col4: WASH level
               p5 <- ggplot(d, aes(
                    x = WASH,
                    y = reorder(location_name, beta_j_hum)
               )) +
                    geom_bar(stat = "identity", fill = "seagreen", width = 0.7) +
                    labs(title = "WASH Level",
                         x = "WASH",
                         y = NULL) +
                    theme_minimal() +
                    theme(axis.text.y = element_blank())

               # define top (empty + histogram + empty + empty)
               top_row    <- plot_spacer() + p1 + plot_spacer() + plot_spacer()
               # define bottom (multiplier + β_hum + incidence + WASH)
               bottom_row <- p4        + p2  + p3       + p5

               # now split rows, then add layout
               bottom_row +
                    plot_layout(widths = c(1, 2, 1, 1))

          })
     }

     shinyApp(ui, server)
}
