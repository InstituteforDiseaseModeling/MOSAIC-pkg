#' Plot Model Fit: Timeseries of Predicted vs Observed
#'
#' Creates timeseries plots comparing model predictions with observed data
#' from a laser-cholera model run.
#'
#' @param model A laser-cholera Model object (class 'laser_cholera.metapop.model.Model') 
#'   output from lc$run_model(paramfile = config). Must contain:
#'   \itemize{
#'     \item results$expected_cases - predicted cases
#'     \item results$disease_deaths - predicted deaths  
#'     \item params$reported_cases - observed cases
#'     \item params$reported_deaths - observed deaths
#'     \item params$location_name - location identifiers
#'     \item params$date_start and params$date_stop - time range for the plot
#'     \item params$seed - seed used for the model run (optional, for display)
#'   }
#' @param output_dir Character string specifying the directory where the plot 
#'   should be saved. Directory will be created if it doesn't exist.
#' @param verbose Logical indicating whether to print messages. Default is TRUE.
#' @param precalculated_likelihood Numeric value of precalculated likelihood to use 
#'   in plot titles instead of recalculating. If NULL, likelihood will be calculated 
#'   using calc_model_likelihood. Default is NULL.
#'
#' @return Invisibly returns a list of ggplot objects:
#'   \itemize{
#'     \item \code{individual}: Named list of plots for each location
#'     \item \code{cases_faceted}: Faceted plot of cases by location (if n_locations > 1)
#'     \item \code{deaths_faceted}: Faceted plot of deaths by location (if n_locations > 1)
#'   }
#'
#' @details
#' This function creates three types of publication-quality timeseries plots:
#' \enumerate{
#'   \item Individual location plots: For each location, a plot with cases and deaths as rows
#'   \item Cases faceted plot: All locations' cases in one faceted plot (skipped if only 1 location)
#'   \item Deaths faceted plot: All locations' deaths in one faceted plot (skipped if only 1 location)
#' }
#' 
#' Each plot shows:
#' \itemize{
#'   \item Observed data as black points
#'   \item Predicted data as colored lines (blue for cases, dark red for deaths)
#'   \item Automatic scaling and layout based on the number of locations
#'   \item Model performance statistics in the subtitle and caption:
#'     \itemize{
#'       \item Seed value (from model$params$seed if available)
#'       \item Log-likelihood (calculated using calc_model_likelihood)
#'       \item Total observed and predicted counts
#'       \item Correlation between observed and predicted values
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' # Run a model
#' config <- sample_parameters(priors = priors, config = base_config, seed = 123)
#' model <- lc$run_model(paramfile = config)
#' 
#' # Create timeseries plots
#' plots <- plot_model_fit(
#'     model = model,
#'     output_dir = "output/plots"
#' )
#' 
#' # Access individual plots
#' plots$individual[["ETH"]]  # Individual location plot for Ethiopia
#' plots$cases_faceted         # Faceted cases plot (if n_locations > 1)
#' plots$deaths_faceted        # Faceted deaths plot (if n_locations > 1)
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_line facet_grid facet_wrap scale_color_manual scale_y_continuous scale_x_date theme_minimal theme element_text element_rect element_blank element_line labs ggsave labeller
#' @importFrom dplyr filter mutate
#' @importFrom tidyr pivot_longer
#' @importFrom scales comma
plot_model_fit <- function(model,
                          output_dir,
                          verbose = TRUE,
                          precalculated_likelihood = NULL) {
    
    # ============================================================================
    # Input validation
    # ============================================================================
    
    if (missing(model) || is.null(model)) {
        stop("model is required")
    }
    
    if (missing(output_dir) || is.null(output_dir)) {
        stop("output_dir is required")
    }
    
    # Check model structure
    if (!inherits(model, "laser_cholera.metapop.model.Model") && !is.list(model)) {
        stop("model must be a laser-cholera Model object (class 'laser_cholera.metapop.model.Model') or a list")
    }
    
    # ============================================================================
    # Extract all data from model object
    # ============================================================================
    
    # Try different possible locations for the data in the model object
    # The exact structure depends on the laser-cholera implementation
    
    # Extract predicted data (these should definitely exist)
    if (!is.null(model$results)) {
        pred_cases <- model$results$expected_cases
        pred_deaths <- model$results$disease_deaths
        
        # Observed data comes from params
        obs_cases <- NULL
        obs_deaths <- NULL
        
        # Location names come from params
        location_names <- NULL
        
        # Dates come from params
        dates <- NULL
        date_start <- NULL
        date_stop <- NULL
        
    } else if (!is.null(model$output)) {
        # Alternative structure
        pred_cases <- model$output$expected_cases
        pred_deaths <- model$output$disease_deaths
        obs_cases <- NULL
        obs_deaths <- NULL
        location_names <- NULL
        dates <- NULL
        date_start <- NULL
        date_stop <- NULL
    } else {
        # Direct access
        pred_cases <- model$expected_cases
        pred_deaths <- model$disease_deaths
        obs_cases <- NULL
        obs_deaths <- NULL
        location_names <- NULL
        dates <- NULL
        date_start <- NULL
        date_stop <- NULL
    }
    
    # Get observed data, location names, and dates from params (this is where they're stored)
    if (!is.null(model$params)) {
        obs_cases <- model$params$reported_cases
        obs_deaths <- model$params$reported_deaths
        location_names <- model$params$location_name
        date_start <- model$params$date_start
        date_stop <- model$params$date_stop
    }
    
    # Fallback to other locations if params doesn't have it
    if (is.null(obs_cases) && !is.null(model$input)) {
        obs_cases <- model$input$reported_cases
        obs_deaths <- model$input$reported_deaths
        if (is.null(location_names)) location_names <- model$input$location_name
        if (is.null(date_start)) date_start <- model$input$date_start
        if (is.null(date_stop)) date_stop <- model$input$date_stop
    }
    
    if (is.null(obs_cases) && !is.null(model$config)) {
        obs_cases <- model$config$reported_cases
        obs_deaths <- model$config$reported_deaths
        if (is.null(location_names)) location_names <- model$config$location_name
        if (is.null(date_start)) date_start <- model$config$date_start
        if (is.null(date_stop)) date_stop <- model$config$date_stop
    }
    
    # Final fallback for location names from results if still not found
    if (is.null(location_names) && !is.null(model$results)) {
        location_names <- model$results$location_names
        if (is.null(location_names)) {
            location_names <- model$results$locations
        }
    }
    
    # ============================================================================
    # Validate we have all required data
    # ============================================================================
    
    if (is.null(pred_cases) || is.null(pred_deaths)) {
        stop("model must contain predicted cases and deaths (expected_cases, disease_deaths)")
    }
    
    if (is.null(obs_cases) || is.null(obs_deaths)) {
        stop("model must contain observed data in model$params (reported_cases, reported_deaths). ",
             "The model object does not appear to include the input parameters used to generate it.")
    }
    
    # Handle location names
    if (is.null(location_names)) {
        # Try to infer from data dimensions
        n_locations <- if(is.matrix(obs_cases)) nrow(obs_cases) else 1
        location_names <- if(n_locations == 1) "Location" else paste0("Location_", 1:n_locations)
        if (verbose) {
            message("Warning: No location names found in model object. Using defaults: ", 
                   paste(location_names, collapse = ", "))
        }
    }
    
    # Get number of locations
    n_locations <- length(location_names)
    
    # Get seed from model
    seed <- if (!is.null(model$params$seed)) model$params$seed else NULL
    
    # ============================================================================
    # Handle model likelihood
    # ============================================================================
    
    # Use precalculated likelihood if provided, otherwise calculate it
    if (!is.null(precalculated_likelihood)) {
        likelihood <- precalculated_likelihood
        if (verbose) message("Using precalculated likelihood: ", round(likelihood, 3))
    } else {
        # Calculate likelihood using calc_model_likelihood
        likelihood <- tryCatch({
            calc_model_likelihood(
                obs_cases,
                pred_cases,
                obs_deaths,
                pred_deaths,
                verbose = FALSE
            )
        }, error = function(e) {
            if (verbose) message("Warning: Could not calculate likelihood: ", e$message)
            NA
        })
    }
    
    # ============================================================================
    # Handle dates
    # ============================================================================
    
    # Get number of time points
    n_time_points <- if(is.matrix(obs_cases)) ncol(obs_cases) else length(obs_cases)
    
    # Try to create date sequence
    if (!is.null(date_start) && !is.null(date_stop)) {
        # Create sequence from date_start to date_stop
        dates <- seq(as.Date(date_start), as.Date(date_stop), length.out = n_time_points)
    } else if (!is.null(dates) && length(dates) == n_time_points) {
        # Use provided dates directly if available
        dates <- as.Date(dates)
    } else if (!is.null(date_start)) {
        # Create weekly sequence from start date only
        dates <- seq(as.Date(date_start), length.out = n_time_points, by = "week")
    } else {
        # Default to numeric time points
        dates <- 1:n_time_points
        if (verbose) {
            message("Warning: No date information found in model object. Using numeric time points.")
        }
    }
    
    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
        if (verbose) message("Created output directory: ", output_dir)
    }
    
    if (verbose) {
        message("Number of locations: ", n_locations)
        message("Number of time points: ", n_time_points)
        message("Obs cases dimensions: ", 
                if(is.matrix(obs_cases)) paste(dim(obs_cases), collapse = " x ") else length(obs_cases))
        message("Pred cases dimensions: ", 
                if(is.matrix(pred_cases)) paste(dim(pred_cases), collapse = " x ") else length(pred_cases))
    }
    
    # ============================================================================
    # Prepare data for plotting
    # ============================================================================
    
    # Helper function to extract data for single location
    extract_location_data <- function(data, i) {
        if(is.matrix(data)) data[i,] else data
    }
    
    # Build plot data
    plot_data <- data.frame()
    
    for (i in 1:n_locations) {
        # Extract data for this location
        obs_cases_i <- extract_location_data(obs_cases, i)
        obs_deaths_i <- extract_location_data(obs_deaths, i)
        pred_cases_i <- extract_location_data(pred_cases, i)
        pred_deaths_i <- extract_location_data(pred_deaths, i)
        
        # Create dataframe for this location
        loc_data <- data.frame(
            location = location_names[i],
            date = rep(dates, 2),
            metric = c(rep("Cases", n_time_points), rep("Deaths", n_time_points)),
            observed = c(obs_cases_i, obs_deaths_i),
            predicted = c(pred_cases_i, pred_deaths_i)
        )
        plot_data <- rbind(plot_data, loc_data)
    }
    
    # Convert to long format for plotting
    plot_data_long <- plot_data %>%
        tidyr::pivot_longer(cols = c(observed, predicted),
                           names_to = "type",
                           values_to = "count") %>%
        dplyr::mutate(
            type = factor(type, levels = c("observed", "predicted")),
            metric = factor(metric, levels = c("Cases", "Deaths"))
        )
    
    # ============================================================================
    # Create plots
    # ============================================================================
    
    # Check if we have actual dates or numeric
    use_date_axis <- inherits(dates, "Date")
    
    # List to store all plots
    plot_list <- list()
    plot_list$individual <- list()
    
    # ============================================================================
    # 1. Individual location plots (one plot per location with cases and deaths as rows)
    # ============================================================================
    
    for (i in 1:n_locations) {
        loc_name <- location_names[i]
        
        # Filter data for this location
        loc_data <- plot_data_long %>%
            dplyr::filter(location == loc_name)
        
        # Extract data for this location for statistics
        obs_cases_i <- extract_location_data(obs_cases, i)
        obs_deaths_i <- extract_location_data(obs_deaths, i)
        pred_cases_i <- extract_location_data(pred_cases, i)
        pred_deaths_i <- extract_location_data(pred_deaths, i)
        
        # Calculate correlations (handling NAs)
        cor_cases <- tryCatch({
            round(cor(obs_cases_i, pred_cases_i, use = "complete.obs"), 3)
        }, error = function(e) NA)
        
        cor_deaths <- tryCatch({
            round(cor(obs_deaths_i, pred_deaths_i, use = "complete.obs"), 3)
        }, error = function(e) NA)
        
        # Calculate sums
        sum_obs_cases <- sum(obs_cases_i, na.rm = TRUE)
        sum_pred_cases <- round(sum(pred_cases_i, na.rm = TRUE))
        sum_obs_deaths <- sum(obs_deaths_i, na.rm = TRUE)
        sum_pred_deaths <- round(sum(pred_deaths_i, na.rm = TRUE))
        
        # Create individual location plot
        p_individual <- ggplot2::ggplot(loc_data, 
                                       ggplot2::aes(x = date, y = count)) +
            # Observed data as points (black)
            ggplot2::geom_point(data = dplyr::filter(loc_data, type == "observed"),
                              color = "black",
                              size = 1.5,
                              alpha = 0.6) +
            # Predicted data as lines
            ggplot2::geom_line(data = dplyr::filter(loc_data, type == "predicted"),
                             ggplot2::aes(color = metric),
                             linewidth = 0.8,
                             alpha = 0.8) +
            # Facet by metric (cases and deaths as rows)
            ggplot2::facet_grid(metric ~ .,
                              scales = "free_y",
                              switch = "y") +
            ggplot2::scale_color_manual(values = c("Cases" = "steelblue", 
                                                  "Deaths" = "darkred"),
                                       guide = "none") +
            ggplot2::scale_y_continuous(labels = scales::comma) +
            ggplot2::theme_minimal(base_size = 10) +
            ggplot2::theme(
                strip.text = ggplot2::element_text(size = 9, face = "bold"),
                strip.background = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                panel.grid.major = ggplot2::element_line(linewidth = 0.25, color = "gray85"),
                axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
                axis.text.y = ggplot2::element_text(size = 8),
                axis.title = ggplot2::element_text(size = 10),
                plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5),
                plot.caption = ggplot2::element_text(size = 8, hjust = 1, face = "italic"),
                strip.placement = "outside"
            ) +
            ggplot2::labs(
                x = if(use_date_axis) "Date" else "Time",
                y = NULL,
                title = paste0("Model Fit: ", loc_name),
                subtitle = paste0(
                    "Observed (points) vs Predicted (lines)",
                    if (!is.null(seed)) paste0(" | Seed: ", seed) else "",
                    if (!is.na(likelihood)) paste0(" | Log-likelihood: ", round(likelihood, 1)) else ""
                ),
                caption = paste0(
                    "Cases: Obs = ", format(sum_obs_cases, big.mark = ","),
                    ", Pred = ", format(sum_pred_cases, big.mark = ","),
                    ", Cor = ", ifelse(is.na(cor_cases), "NA", cor_cases),
                    " | Deaths: Obs = ", format(sum_obs_deaths, big.mark = ","),
                    ", Pred = ", format(sum_pred_deaths, big.mark = ","),
                    ", Cor = ", ifelse(is.na(cor_deaths), "NA", cor_deaths),
                    "\nGenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                )
            )
        
        # Add appropriate x-axis scale
        if (use_date_axis) {
            p_individual <- p_individual +
                ggplot2::scale_x_date(date_breaks = "3 months",
                                    date_labels = "%b %Y")
        }
        
        # Store plot
        plot_list$individual[[loc_name]] <- p_individual
        
        # Display plot
        if (verbose) {
            print(p_individual)
        }
        
        # Save individual location plot
        output_file <- file.path(output_dir, paste0("model_timeseries_", loc_name, ".pdf"))
        ggplot2::ggsave(output_file,
                       plot = p_individual,
                       width = 10,
                       height = 6,
                       dpi = 300)
        
        if (verbose) {
            message("Individual location plot saved as '", output_file, "'")
        }
    }
    
    # ============================================================================
    # 2. Cases faceted plot (all locations, cases only) - skip if only 1 location
    # ============================================================================
    
    if (n_locations > 1) {
        # Filter for cases only
        cases_data <- plot_data_long %>%
            dplyr::filter(metric == "Cases")
        
        # Calculate overall correlation and sums for cases
        cor_cases_overall <- tryCatch({
            if (is.matrix(obs_cases)) {
                round(cor(as.vector(obs_cases), as.vector(pred_cases), use = "complete.obs"), 3)
            } else {
                round(cor(obs_cases, pred_cases, use = "complete.obs"), 3)
            }
        }, error = function(e) NA)
        
        sum_obs_cases_all <- sum(obs_cases, na.rm = TRUE)
        sum_pred_cases_all <- round(sum(pred_cases, na.rm = TRUE))
        
        p_cases <- ggplot2::ggplot(cases_data, 
                                   ggplot2::aes(x = date, y = count)) +
            # Observed data as points (black)
            ggplot2::geom_point(data = dplyr::filter(cases_data, type == "observed"),
                              color = "black",
                              size = 1.5,
                              alpha = 0.6) +
            # Predicted data as lines
            ggplot2::geom_line(data = dplyr::filter(cases_data, type == "predicted"),
                             color = "steelblue",
                             linewidth = 0.8,
                             alpha = 0.8) +
            # Facet by location
            ggplot2::facet_wrap(~ location,
                              scales = "free_y",
                              ncol = min(3, n_locations)) +
            ggplot2::scale_y_continuous(labels = scales::comma) +
            ggplot2::theme_minimal(base_size = 10) +
            ggplot2::theme(
                strip.text = ggplot2::element_text(size = 9, face = "bold"),
                strip.background = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                panel.grid.major = ggplot2::element_line(linewidth = 0.25, color = "gray85"),
                axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
                axis.text.y = ggplot2::element_text(size = 8),
                axis.title = ggplot2::element_text(size = 10),
                plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5),
                plot.caption = ggplot2::element_text(size = 8, hjust = 1, face = "italic")
            ) +
            ggplot2::labs(
                x = if(use_date_axis) "Date" else "Time",
                y = "Cases",
                title = "Model Fit: Cases by Location",
                subtitle = paste0(
                    "Observed (points) vs Predicted (lines)",
                    if (!is.null(seed)) paste0(" | Seed: ", seed) else "",
                    if (!is.na(likelihood)) paste0(" | Log-likelihood: ", round(likelihood, 1)) else ""
                ),
                caption = paste0(
                    "Total Cases: Obs = ", format(sum_obs_cases_all, big.mark = ","),
                    ", Pred = ", format(sum_pred_cases_all, big.mark = ","),
                    ", Cor = ", ifelse(is.na(cor_cases_overall), "NA", cor_cases_overall),
                    "\nGenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                )
            )
        
        # Add appropriate x-axis scale
        if (use_date_axis) {
            p_cases <- p_cases +
                ggplot2::scale_x_date(date_breaks = "3 months",
                                    date_labels = "%b %Y")
        }
        
        plot_list$cases_faceted <- p_cases
        
        # Display plot
        if (verbose) {
            print(p_cases)
        }
        
        # Determine dimensions based on number of locations
        if (n_locations <= 3) {
            plot_width <- 12
            plot_height <- 5
        } else if (n_locations <= 6) {
            plot_width <- 14
            plot_height <- 8
        } else {
            plot_width <- 16
            plot_height <- max(10, ceiling(n_locations / 3) * 3)
        }
        
        # Save cases plot
        output_file <- file.path(output_dir, "model_timeseries_cases_all.pdf")
        ggplot2::ggsave(output_file,
                       plot = p_cases,
                       width = plot_width,
                       height = plot_height,
                       dpi = 300,
                       limitsize = FALSE)
        
        if (verbose) {
            message("Cases faceted plot saved as '", output_file, "'")
        }
    }
    
    # ============================================================================
    # 3. Deaths faceted plot (all locations, deaths only) - skip if only 1 location
    # ============================================================================
    
    if (n_locations > 1) {
        # Filter for deaths only
        deaths_data <- plot_data_long %>%
            dplyr::filter(metric == "Deaths")
        
        # Calculate overall correlation and sums for deaths
        cor_deaths_overall <- tryCatch({
            if (is.matrix(obs_deaths)) {
                round(cor(as.vector(obs_deaths), as.vector(pred_deaths), use = "complete.obs"), 3)
            } else {
                round(cor(obs_deaths, pred_deaths, use = "complete.obs"), 3)
            }
        }, error = function(e) NA)
        
        sum_obs_deaths_all <- sum(obs_deaths, na.rm = TRUE)
        sum_pred_deaths_all <- round(sum(pred_deaths, na.rm = TRUE))
        
        p_deaths <- ggplot2::ggplot(deaths_data, 
                                    ggplot2::aes(x = date, y = count)) +
            # Observed data as points (black)
            ggplot2::geom_point(data = dplyr::filter(deaths_data, type == "observed"),
                              color = "black",
                              size = 1.5,
                              alpha = 0.6) +
            # Predicted data as lines
            ggplot2::geom_line(data = dplyr::filter(deaths_data, type == "predicted"),
                             color = "darkred",
                             linewidth = 0.8,
                             alpha = 0.8) +
            # Facet by location
            ggplot2::facet_wrap(~ location,
                              scales = "free_y",
                              ncol = min(3, n_locations)) +
            ggplot2::scale_y_continuous(labels = scales::comma) +
            ggplot2::theme_minimal(base_size = 10) +
            ggplot2::theme(
                strip.text = ggplot2::element_text(size = 9, face = "bold"),
                strip.background = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                panel.grid.major = ggplot2::element_line(linewidth = 0.25, color = "gray85"),
                axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
                axis.text.y = ggplot2::element_text(size = 8),
                axis.title = ggplot2::element_text(size = 10),
                plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5),
                plot.caption = ggplot2::element_text(size = 8, hjust = 1, face = "italic")
            ) +
            ggplot2::labs(
                x = if(use_date_axis) "Date" else "Time",
                y = "Deaths",
                title = "Model Fit: Deaths by Location",
                subtitle = paste0(
                    "Observed (points) vs Predicted (lines)",
                    if (!is.null(seed)) paste0(" | Seed: ", seed) else "",
                    if (!is.na(likelihood)) paste0(" | Log-likelihood: ", round(likelihood, 1)) else ""
                ),
                caption = paste0(
                    "Total Deaths: Obs = ", format(sum_obs_deaths_all, big.mark = ","),
                    ", Pred = ", format(sum_pred_deaths_all, big.mark = ","),
                    ", Cor = ", ifelse(is.na(cor_deaths_overall), "NA", cor_deaths_overall),
                    "\nGenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                )
            )
        
        # Add appropriate x-axis scale
        if (use_date_axis) {
            p_deaths <- p_deaths +
                ggplot2::scale_x_date(date_breaks = "3 months",
                                    date_labels = "%b %Y")
        }
        
        plot_list$deaths_faceted <- p_deaths
        
        # Display plot
        if (verbose) {
            print(p_deaths)
        }
        
        # Determine dimensions based on number of locations
        if (n_locations <= 3) {
            plot_width <- 12
            plot_height <- 5
        } else if (n_locations <= 6) {
            plot_width <- 14
            plot_height <- 8
        } else {
            plot_width <- 16
            plot_height <- max(10, ceiling(n_locations / 3) * 3)
        }
        
        # Save deaths plot
        output_file <- file.path(output_dir, "model_timeseries_deaths_all.pdf")
        ggplot2::ggsave(output_file,
                       plot = p_deaths,
                       width = plot_width,
                       height = plot_height,
                       dpi = 300,
                       limitsize = FALSE)
        
        if (verbose) {
            message("Deaths faceted plot saved as '", output_file, "'")
        }
    }
    
    # Return plot objects invisibly
    invisible(plot_list)
}