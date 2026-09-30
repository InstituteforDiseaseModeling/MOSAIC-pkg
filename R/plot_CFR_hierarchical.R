#' Plot Hierarchical CFR Model Results
#'
#' @description
#' Creates professional visualizations of the hierarchical GAM CFR model results including
#' observed data points and modeled time series predictions. Generates an 8.5x11 inch PDF
#' with faceted plots showing country-specific CFR trends over time.
#'
#' @param PATHS List containing paths to data directories, typically from get_paths()
#' @param countries_to_plot Character vector of ISO codes to plot. If NULL, plots top 20 
#'   countries by data availability
#' @param pdf_width Numeric, width of PDF in inches (default 8.5)
#' @param pdf_height Numeric, height of PDF in inches (default 11)
#' @param ncol_facet Integer, number of columns for facet wrap (default 4)
#' @param show_ci Logical, whether to show confidence intervals (default TRUE)
#' @param verbose Logical, whether to print progress messages (default TRUE)
#'
#' @details
#' The function creates a multi-page PDF containing:
#' \itemize{
#'   \item Page 1: Overview plot with temporal trend and data summary
#'   \item Following pages: Faceted plots of country-specific CFR trends
#'   \item Each panel shows observed CFR (points) and model predictions (lines with 95% predictive intervals for one year's CFR)
#'   \item Years after a country's last fitted year (\code{is_forecast}) are drawn dashed on
#'     a shaded background: the model holds each trend at its last fitted year there
#'   \item A page of each country's deviation from the population trend (the model's
#'     country intercept is weakly identified, so the deviation is taken from the
#'     fitted country curve)
#' }
#'
#' @return Invisibly returns a list of ggplot objects. Saves PDF to DOCS_FIGURES directory.
#'
#' @examples
#' \dontrun{
#' PATHS <- get_paths()
#' # First run the model
#' est_CFR_hierarchical(PATHS)
#' # Then create plots
#' plot_CFR_hierarchical(PATHS)
#' 
#' # Plot specific countries
#' plot_CFR_hierarchical(PATHS, 
#'   countries_to_plot = c("COD", "NGA", "KEN", "MOZ", "ETH", "ZWE"))
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_point facet_wrap
#' @importFrom utils read.csv
#' @importFrom grDevices pdf dev.off
#' @importFrom stats aggregate
#'
#' @export
plot_CFR_hierarchical <- function(
    PATHS,
    countries_to_plot = NULL,
    pdf_width = 8.5,
    pdf_height = 11,
    ncol_facet = 4,
    show_ci = TRUE,
    verbose = TRUE
) {
    
    # Check for required packages
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' is required. Please install it using: install.packages('ggplot2')")
    }
    if (!requireNamespace("scales", quietly = TRUE)) {
        stop("Package 'scales' is required. Please install it using: install.packages('scales')")
    }

    # All required packages loaded via NAMESPACE

    # Check that model outputs exist
    estimates_file <- file.path(PATHS$MODEL_INPUT, "cfr_hierarchical_estimates.csv")
    trend_file <- file.path(PATHS$MODEL_INPUT, "cfr_temporal_trend.csv")
    who_data_file <- file.path(PATHS$DATA_WHO_ANNUAL, "who_afro_annual.csv")
    
    if (!file.exists(estimates_file)) {
        stop("CFR model estimates not found. Please run est_CFR_hierarchical(PATHS) first.")
    }
    
    if (verbose) message("Loading CFR model results and data...")
    
    # Load data
    estimates <- utils::read.csv(estimates_file, stringsAsFactors = FALSE)
    temporal_trend <- utils::read.csv(trend_file, stringsAsFactors = FALSE)
    who_data <- utils::read.csv(who_data_file, stringsAsFactors = FALSE)
    
    # Prepare observed data - filter for MOSAIC countries only
    obs_data <- who_data[
        who_data$country != "AFRO Region" &
        who_data$iso_code %in% MOSAIC::iso_codes_mosaic &  # Filter for MOSAIC countries
        !is.na(who_data$cases_total) &
        !is.na(who_data$deaths_total) &
        who_data$cases_total > 0,
    ]
    # A year still in progress when its snapshot was taken is excluded, as in
    # est_CFR_hierarchical() (its deaths lag its cases).
    if ("source" %in% names(obs_data)) obs_data <- obs_data[!.cfr_in_progress(obs_data), ]
    obs_data$cfr_observed <- obs_data$deaths_total / obs_data$cases_total
    obs_data$log_cases <- log10(obs_data$cases_total + 1)

    # Locations fitted from their own data. The estimates' `pooled` flag marks a
    # location absent from the fit (it gets the population curve); files written
    # before that column existed fall back to having any observed year.
    own_fit_iso <- if ("pooled" %in% names(estimates)) {
        unique(estimates$iso_code[!as.logical(estimates$pooled)])
    } else {
        unique(obs_data$iso_code)
    }

    # Years come from the model outputs: the trend runs past the data, holding
    # each curve at its last fitted year (is_forecast).
    temporal_trend$is_forecast <- as.logical(temporal_trend$is_forecast %||% FALSE)
    estimates$is_forecast <- as.logical(estimates$is_forecast %||% FALSE)
    yr_min <- min(temporal_trend$year)
    yr_max <- max(c(temporal_trend$year, estimates$year))
    yr_fit <- max(temporal_trend$year[!temporal_trend$is_forecast])
    brk <- function(by) seq(yr_min - yr_min %% by + by * (yr_min %% by > 0), yr_max, by = by)
    fc_band <- if (yr_max > yr_fit) ggplot2::annotate("rect", xmin = yr_fit + 0.5, xmax = yr_max + 0.5,
                                                      ymin = -Inf, ymax = Inf, fill = "gray92") else NULL
    
    # Select countries to plot
    if (is.null(countries_to_plot)) {
        # Select top MOSAIC countries by data availability
        country_counts <- aggregate(
            cases_total ~ iso_code, 
            data = obs_data[obs_data$cases_total >= 20,], 
            FUN = length
        )
        names(country_counts)[2] <- "n_years"
        country_counts <- country_counts[order(country_counts$n_years, decreasing = TRUE),]
        # Ensure we only select from MOSAIC countries
        country_counts <- country_counts[country_counts$iso_code %in% MOSAIC::iso_codes_mosaic,]
        countries_to_plot <- head(country_counts$iso_code, 20)
        
        if (verbose) {
            message(sprintf("Plotting top %d MOSAIC countries by data availability", 
                          length(countries_to_plot)))
        }
    } else {
        # Validate that requested countries are in MOSAIC framework
        invalid_countries <- setdiff(countries_to_plot, MOSAIC::iso_codes_mosaic)
        if (length(invalid_countries) > 0) {
            warning(paste("The following countries are not in MOSAIC framework and will be excluded:",
                         paste(invalid_countries, collapse = ", ")))
            countries_to_plot <- intersect(countries_to_plot, MOSAIC::iso_codes_mosaic)
        }
    }
    
    # Filter data for selected countries
    estimates_subset <- estimates[estimates$iso_code %in% countries_to_plot,]
    obs_subset <- obs_data[obs_data$iso_code %in% countries_to_plot,]
    
    # Create country name lookup for better labels
    country_names <- unique(estimates_subset[, c("iso_code", "country")])
    
    # Set up PDF output
    pdf_file <- file.path(PATHS$DOCS_FIGURES, "cfr_hierarchical_model_results.pdf")
    if (verbose) message(sprintf("Creating PDF: %s", pdf_file))
    
    grDevices::pdf(pdf_file, width = pdf_width, height = pdf_height)
    
    # Color palette
    model_color <- "#2E86AB"  # Blue for model predictions
    ci_color <- "#A0C4E2"     # Light blue for confidence intervals
    obs_color <- "#A23B72"     # Purple for observations
    trend_color <- "#F18F01"   # Orange for overall trend
    
    # Page 1: Overview plot with temporal trend and summary statistics
    p1 <- ggplot2::ggplot(temporal_trend, ggplot2::aes(x = year)) +
        fc_band +
        # Confidence interval
        ggplot2::geom_ribbon(
            ggplot2::aes(ymin = cfr_trend_lower * 100, 
                        ymax = cfr_trend_upper * 100),
            alpha = 0.3, fill = trend_color
        ) +
        # Trend line
        ggplot2::geom_line(
            ggplot2::aes(y = cfr_trend * 100), 
            color = trend_color, linewidth = 1.5
        ) +
        # Add scatter of all observations
        ggplot2::geom_point(
            data = obs_data,
            ggplot2::aes(x = year, y = cfr_observed * 100, size = log_cases),
            alpha = 0.2, color = obs_color
        ) +
        ggplot2::scale_size_continuous(
            name = "Cases (log10)",
            range = c(0.5, 4),
            breaks = c(2, 3, 4, 5),
            labels = c("100", "1K", "10K", "100K")
        ) +
        ggplot2::scale_x_continuous(
            breaks = brk(5),
            minor_breaks = seq(yr_min, yr_max, by = 1)
        ) +
        ggplot2::scale_y_continuous(
            labels = scales::percent_format(scale = 1),
            limits = c(0, NA)
        ) +
        ggplot2::labs(
            title = sprintf("Cholera Case Fatality Ratio in Africa: Temporal Trend (%d-%d)", yr_min, yr_fit),
            subtitle = if (yr_max > yr_fit) sprintf(
                "Population average trend with 95%% confidence interval; %d-%d (shaded) held at %d",
                yr_fit + 1L, yr_max, yr_fit) else "Population average trend with 95% confidence interval",
            caption = paste("Model: Hierarchical GAM with spline smoothing | Data: WHO AFRO Region",
                          "\nPoints show country-level observations sized by case count"),
            x = "Year",
            y = "Case Fatality Ratio (%)"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", size = 14),
            plot.subtitle = ggplot2::element_text(color = "gray40", size = 11),
            plot.caption = ggplot2::element_text(size = 8, hjust = 0, color = "gray50"),
            panel.grid.minor = ggplot2::element_line(color = "gray95"),
            legend.position = "bottom",
            plot.margin = ggplot2::margin(0.5, 0.5, 0.5, 0.5, "cm")
        )
    
    print(p1)
    
    # Calculate number of pages needed for country plots
    n_countries <- length(countries_to_plot)
    plots_per_page <- ncol_facet * 5  # Assuming 5 rows per page
    n_pages <- ceiling(n_countries / plots_per_page)
    
    # Create faceted plots for countries (multiple pages if needed)
    for (page in 1:n_pages) {
        start_idx <- (page - 1) * plots_per_page + 1
        end_idx <- min(page * plots_per_page, n_countries)
        countries_page <- countries_to_plot[start_idx:end_idx]
        
        # Filter data for this page
        est_page <- estimates_subset[estimates_subset$iso_code %in% countries_page,]
        obs_page <- obs_subset[obs_subset$iso_code %in% countries_page,]
        
        # Add country names for facet labels
        est_page$facet_label <- paste0(est_page$country, "\n(", est_page$iso_code, ")")
        obs_page <- merge(obs_page, 
                         unique(est_page[, c("iso_code", "facet_label")]), 
                         by = "iso_code", all.x = TRUE)
        
        # Create faceted plot
        p_facet <- ggplot2::ggplot(est_page, ggplot2::aes(x = year)) +
            fc_band +
            # Model predictions with CI
            {if(show_ci) ggplot2::geom_ribbon(
                ggplot2::aes(ymin = cfr_lower * 100, 
                            ymax = cfr_upper * 100),
                alpha = 0.2, fill = model_color
            )} +
            # Model prediction line: solid over fitted years, dashed where the
            # country's trend is held (its forecast years)
            ggplot2::geom_line(
                data = est_page[!est_page$is_forecast, ],
                ggplot2::aes(y = cfr_estimate * 100),
                color = model_color, linewidth = 1
            ) +
            ggplot2::geom_line(
                data = est_page[est_page$is_forecast |
                                (est_page$year == ave(est_page$year * !est_page$is_forecast,
                                                      est_page$iso_code, FUN = max)), ],
                ggplot2::aes(y = cfr_estimate * 100),
                color = model_color, linewidth = 0.7, linetype = "22"
            ) +
            # Observed data points
            ggplot2::geom_point(
                data = obs_page,
                ggplot2::aes(x = year, y = cfr_observed * 100, 
                            size = cases_total),
                alpha = 0.6, color = obs_color
            ) +
            # Facet by country
            ggplot2::facet_wrap(
                ~ facet_label, 
                ncol = ncol_facet,
                scales = "free_y"
            ) +
            # Scales
            ggplot2::scale_x_continuous(
                breaks = brk(20),
                minor_breaks = brk(10)
            ) +
            ggplot2::scale_y_continuous(
                labels = scales::percent_format(scale = 1),
                limits = c(0, NA)
            ) +
            ggplot2::scale_size_continuous(
                name = "Cases",
                range = c(0.5, 3),
                trans = "log10",
                labels = scales::comma,
                guide = "none"
            ) +
            # Labels
            ggplot2::labs(
                title = sprintf("Country-Specific CFR Trends (Page %d of %d)", page, n_pages),
                subtitle = "Model predictions (blue line with 95% predictive interval; dashed and shaded where held) vs. observed data (purple points)",
                x = "Year",
                y = "Case Fatality Ratio (%)",
                caption = if(page == n_pages) "Point size indicates number of reported cases" else ""
            ) +
            # Theme
            ggplot2::theme_minimal(base_size = 9) +
            ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", size = 12),
                plot.subtitle = ggplot2::element_text(color = "gray40", size = 10),
                strip.text = ggplot2::element_text(face = "bold", size = 8),
                strip.background = ggplot2::element_rect(fill = "gray95", color = NA),
                panel.grid.minor = ggplot2::element_blank(),
                panel.spacing = ggplot2::unit(0.5, "lines"),
                plot.margin = ggplot2::margin(0.3, 0.3, 0.3, 0.3, "cm"),
                axis.text = ggplot2::element_text(size = 7),
                axis.title = ggplot2::element_text(size = 9)
            )
        
        print(p_facet)
    }
    
    # Page: each country's CFR relative to the population trend. The model's
    # country intercept (s(iso, re)) is weakly identified -- the per-country trend
    # smooth carries the level -- so the deviation comes from the fitted country
    # curve: its mean logit difference from the population trend over the
    # country's fitted (non-forecast) years.
    tr_logit <- stats::setNames(stats::qlogis(temporal_trend$cfr_trend), temporal_trend$year)
    fit_rows <- estimates_subset[!estimates_subset$is_forecast, , drop = FALSE]
    if (nrow(fit_rows) > 0) {
        dev <- tapply(stats::qlogis(fit_rows$cfr_estimate) - tr_logit[as.character(fit_rows$year)],
                      fit_rows$iso_code, mean, na.rm = TRUE)
        effects_subset <- data.frame(iso_code = names(dev), deviation = as.numeric(dev),
                                     stringsAsFactors = FALSE)
        effects_subset$country <- country_names$country[match(effects_subset$iso_code, country_names$iso_code)]
        effects_subset <- effects_subset[order(effects_subset$deviation), ]
        effects_subset$country <- factor(effects_subset$country, levels = unique(effects_subset$country))

        p_effects <- ggplot2::ggplot(effects_subset, ggplot2::aes(x = deviation, y = country)) +
            ggplot2::geom_segment(
                ggplot2::aes(x = 0, xend = deviation, y = country, yend = country),
                color = "gray60", linewidth = 0.5
            ) +
            ggplot2::geom_point(ggplot2::aes(color = deviation > 0), size = 3) +
            ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", alpha = 0.5) +
            ggplot2::scale_color_manual(
                values = c("TRUE" = "#D32F2F", "FALSE" = "#1976D2"),
                labels = c("TRUE" = "Above the population trend", "FALSE" = "Below the population trend"),
                name = NULL
            ) +
            ggplot2::labs(
                title = "Country CFR Relative to the Population Trend",
                subtitle = "Mean logit difference of each fitted country curve from the population trend, fitted years",
                x = "Deviation (logit scale)",
                y = "Country",
                caption = "exp(deviation) is roughly the country's odds ratio against the population average"
            ) +
            ggplot2::theme_minimal(base_size = 10) +
            ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", size = 14),
                plot.subtitle = ggplot2::element_text(color = "gray40", size = 11),
                plot.caption = ggplot2::element_text(size = 8, color = "gray50"),
                legend.position = "bottom",
                axis.text.y = ggplot2::element_text(size = 8),
                plot.margin = ggplot2::margin(0.5, 0.5, 0.5, 0.5, "cm")
            )

        print(p_effects)
    }

    # Summary statistics page - FOR ALL MOSAIC COUNTRIES
    # Calculate summary stats for ALL MOSAIC countries with sufficient data
    all_mosaic_obs_sufficient <- obs_data[
        obs_data$iso_code %in% MOSAIC::iso_codes_mosaic & 
        obs_data$cases_total >= 20,
    ]
    
    # Only calculate stats for countries that have any data
    countries_with_stats <- unique(all_mosaic_obs_sufficient$iso_code)
    
    if (length(countries_with_stats) > 0) {
        # Keyed on iso_code: the WHO file spells some countries more than one way.
        summary_stats <- aggregate(
            cfr_observed ~ iso_code,
            data = all_mosaic_obs_sufficient,
            FUN = function(x) c(
                mean = mean(x, na.rm = TRUE),
                median = median(x, na.rm = TRUE),
                sd = sd(x, na.rm = TRUE),
                n = length(x)
            )
        )
        summary_df <- data.frame(
            iso_code = summary_stats$iso_code,
            summary_stats$cfr_observed
        )
    } else {
        # Create empty data frame if no countries have sufficient data
        summary_df <- data.frame(
            iso_code = character(),
            country = character(),
            mean = numeric(),
            median = numeric(),
            sd = numeric(),
            n = numeric()
        )
    }
    
    # Model predictions for ALL MOSAIC countries in the last fitted year
    all_mosaic_pred <- estimates[
        estimates$iso_code %in% MOSAIC::iso_codes_mosaic &
        estimates$year == yr_fit,
    ]
    
    # Create complete summary including countries without historical data
    all_countries_df <- unique(all_mosaic_pred[, c("iso_code", "country", "cfr_estimate")])
    names(all_countries_df)[names(all_countries_df) == "cfr_estimate"] <- "cfr_pred"
    
    # Merge with historical stats (will have NA for countries without data)
    if (nrow(summary_df) > 0) {
        summary_df <- merge(all_countries_df, 
                           summary_df[, c("iso_code", "mean", "median", "sd", "n")],
                           by = "iso_code", all = TRUE)
    } else {
        summary_df <- all_countries_df
        summary_df$mean <- NA
        summary_df$median <- NA
        summary_df$sd <- NA
        summary_df$n <- 0
    }
    
    # Historical mean +/- SD bars need at least one year with >= 20 cases.
    summary_df$has_historical_data <- !is.na(summary_df$mean)
    # Symbol/legend key: whether the location got its own fitted curve or the
    # population curve. Read from the estimates' `pooled` flag (a location with
    # only small-count years is still fitted from its own data).
    summary_df$own_fit <- summary_df$iso_code %in% own_fit_iso
    
    # Sort by the prediction (check if cfr_pred exists)
    if ("cfr_pred" %in% names(summary_df) && nrow(summary_df) > 0) {
        summary_df <- summary_df[order(summary_df$cfr_pred, decreasing = TRUE),]
    }
    summary_df$country <- factor(summary_df$country, levels = unique(summary_df$country))
    
    # Split into two plots if too many countries
    n_mosaic_countries <- nrow(summary_df)
    max_countries_per_plot <- 25
    
    # Initialize p_summary to avoid undefined variable error
    p_summary <- NULL
    
    # Only create summary plot if we have data
    if (n_mosaic_countries == 0) {
        if (verbose) message("No data available for summary plot")
    } else if (n_mosaic_countries <= max_countries_per_plot) {
        # Single plot for all countries
        p_summary <- ggplot2::ggplot(summary_df, ggplot2::aes(x = country)) +
            # Historical mean with error bars (only for countries with data)
            ggplot2::geom_pointrange(
                data = summary_df[summary_df$has_historical_data,],
                ggplot2::aes(y = mean * 100,
                            ymin = pmax((mean - sd) * 100, 0),
                            ymax = (mean + sd) * 100),
                color = "gray50", size = 0.3, fatten = 1
            ) +
            # last-fitted-year prediction - different symbols for with/without historical data
            ggplot2::geom_point(
                ggplot2::aes(y = cfr_pred * 100,
                            shape = .data$own_fit,
                            color = .data$own_fit),
                size = 2.5
            ) +
            ggplot2::scale_shape_manual(
                values = c("TRUE" = 16, "FALSE" = 1),
                labels = c("TRUE" = "Country-specific fit", "FALSE" = "Population average only"),
                name = "Estimate"
            ) +
            ggplot2::scale_color_manual(
                values = c("TRUE" = model_color, "FALSE" = "#E57373"),
                labels = c("TRUE" = "Country-specific fit", "FALSE" = "Population average only"),
                name = "Estimate"
            ) +
            ggplot2::coord_flip() +
            ggplot2::scale_y_continuous(
                labels = scales::percent_format(scale = 1),
                limits = c(0, NA)
            ) +
            ggplot2::labs(
                title = sprintf("All MOSAIC Countries: Historical CFR vs. %d Model Predictions", yr_fit),
                subtitle = sprintf("Gray bars: historical mean \u00B1 SD | Points: %d model predictions", yr_fit),
                x = "",
                y = "Case Fatality Ratio (%)",
                caption = sprintf("Total: %d countries | Country-specific fit: %d | Population average: %d",
                                n_mosaic_countries,
                                sum(summary_df$own_fit),
                                sum(!summary_df$own_fit))
            ) +
            ggplot2::theme_minimal(base_size = 9) +
            ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", size = 13),
                plot.subtitle = ggplot2::element_text(color = "gray40", size = 10),
                plot.caption = ggplot2::element_text(size = 7, color = "gray50"),
                axis.text.y = ggplot2::element_text(size = 7),
                legend.position = "bottom",
                legend.title = ggplot2::element_text(size = 8),
                legend.text = ggplot2::element_text(size = 7),
                plot.margin = ggplot2::margin(0.5, 0.5, 0.5, 0.5, "cm")
            )
        
        print(p_summary)
        
    } else {
        # Create two plots for better readability
        p_summary_parts <- list()
        for (plot_num in 1:2) {
            if (plot_num == 1) {
                start_idx <- 1
                end_idx <- ceiling(n_mosaic_countries / 2)
                plot_subtitle <- "Part 1: Higher CFR countries"
            } else {
                start_idx <- ceiling(n_mosaic_countries / 2) + 1
                end_idx <- n_mosaic_countries
                plot_subtitle <- "Part 2: Lower CFR countries"
            }
            
            summary_subset <- summary_df[start_idx:end_idx,]
            
            p_summary_part <- ggplot2::ggplot(summary_subset, ggplot2::aes(x = country)) +
                # Historical mean with error bars (only for countries with data)
                ggplot2::geom_pointrange(
                    data = summary_subset[summary_subset$has_historical_data,],
                    ggplot2::aes(y = mean * 100,
                                ymin = pmax((mean - sd) * 100, 0),
                                ymax = (mean + sd) * 100),
                    color = "gray50", size = 0.3, fatten = 1
                ) +
                # last-fitted-year prediction - different symbols for with/without historical data
                ggplot2::geom_point(
                    ggplot2::aes(y = cfr_pred * 100,
                                shape = .data$own_fit,
                                color = .data$own_fit),
                    size = 2.5
                ) +
                ggplot2::scale_shape_manual(
                    values = c("TRUE" = 16, "FALSE" = 1),
                    labels = c("TRUE" = "Country-specific fit", "FALSE" = "Population average only"),
                    name = "Estimate"
                ) +
                ggplot2::scale_color_manual(
                    values = c("TRUE" = model_color, "FALSE" = "#E57373"),
                    labels = c("TRUE" = "Country-specific fit", "FALSE" = "Population average only"),
                    name = "Estimate"
                ) +
                ggplot2::coord_flip() +
                ggplot2::scale_y_continuous(
                    labels = scales::percent_format(scale = 1),
                    limits = c(0, NA)
                ) +
                ggplot2::labs(
                    title = sprintf("All MOSAIC Countries: Historical CFR vs. %d Model Predictions", yr_fit),
                    subtitle = plot_subtitle,
                    x = "",
                    y = "Case Fatality Ratio (%)",
                    caption = if (plot_num == 2) {
                        sprintf("Total: %d countries | Country-specific fit: %d | Population average: %d",
                              n_mosaic_countries,
                              sum(summary_df$own_fit),
                              sum(!summary_df$own_fit))
                    } else ""
                ) +
                ggplot2::theme_minimal(base_size = 9) +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", size = 13),
                    plot.subtitle = ggplot2::element_text(color = "gray40", size = 10),
                    plot.caption = ggplot2::element_text(size = 7, color = "gray50"),
                    axis.text.y = ggplot2::element_text(size = 7),
                    legend.position = if(plot_num == 2) "bottom" else "none",
                    legend.title = ggplot2::element_text(size = 8),
                    legend.text = ggplot2::element_text(size = 7),
                    plot.margin = ggplot2::margin(0.5, 0.5, 0.5, 0.5, "cm")
                )
            
            print(p_summary_part)
            p_summary_parts[[plot_num]] <- p_summary_part
        }
        # Store the first part as p_summary for the return value
        p_summary <- p_summary_parts[[1]]
    }
    
    # NEW PLOT: All MOSAIC countries overview with data indicators
    if (verbose) message("Creating comprehensive all-countries plot...")
    
    # Get all MOSAIC countries from estimates
    all_mosaic_estimates <- estimates[estimates$iso_code %in% MOSAIC::iso_codes_mosaic,]
    all_mosaic_obs <- obs_data[obs_data$iso_code %in% MOSAIC::iso_codes_mosaic,]
    
    # Line style and facet tag follow the fit: solid for a location fitted from
    # its own data, dashed + tag for one that got the population curve. The
    # >= 20-case threshold only sizes the observed points below.
    countries_with_data <- own_fit_iso
    all_mosaic_estimates$has_data <- all_mosaic_estimates$iso_code %in% countries_with_data

    # Create facet labels with the fit indicator
    all_mosaic_estimates$facet_label <- paste0(
        all_mosaic_estimates$country,
        "\n(", all_mosaic_estimates$iso_code, ")",
        ifelse(all_mosaic_estimates$has_data, "", "\n[Population average]")
    )
    
    # Merge facet labels with observations
    all_mosaic_obs <- merge(all_mosaic_obs,
                            unique(all_mosaic_estimates[, c("iso_code", "facet_label")]),
                            by = "iso_code", all.x = TRUE)
    
    # Order countries by whether they have data and then alphabetically
    country_order <- unique(all_mosaic_estimates[order(!all_mosaic_estimates$has_data, 
                                                       all_mosaic_estimates$country), 
                                                 c("iso_code", "facet_label")])
    all_mosaic_estimates$facet_label <- factor(all_mosaic_estimates$facet_label,
                                               levels = unique(country_order$facet_label))
    all_mosaic_obs$facet_label <- factor(all_mosaic_obs$facet_label,
                                         levels = unique(country_order$facet_label))
    
    # Calculate number of pages needed for all countries plot
    n_all_countries <- length(unique(all_mosaic_estimates$iso_code))
    all_plots_per_page <- 20  # More countries per page for overview
    n_all_pages <- ceiling(n_all_countries / all_plots_per_page)
    
    # Create comprehensive plots for ALL MOSAIC countries
    for (page in 1:n_all_pages) {
        start_idx <- (page - 1) * all_plots_per_page + 1
        end_idx <- min(page * all_plots_per_page, n_all_countries)
        countries_page <- unique(all_mosaic_estimates$iso_code)[start_idx:end_idx]
        
        # Filter data for this page
        est_page <- all_mosaic_estimates[all_mosaic_estimates$iso_code %in% countries_page,]
        obs_page <- all_mosaic_obs[all_mosaic_obs$iso_code %in% countries_page,]
        
        # Create plot
        p_all <- ggplot2::ggplot(est_page, ggplot2::aes(x = year)) +
            fc_band +
            # Model predictions with CI - different transparency for countries with/without data
            ggplot2::geom_ribbon(
                ggplot2::aes(ymin = cfr_lower * 100, 
                            ymax = cfr_upper * 100,
                            alpha = has_data),
                fill = model_color
            ) +
            # Model prediction line - different style for countries with/without data
            ggplot2::geom_line(
                ggplot2::aes(y = cfr_estimate * 100,
                            linetype = has_data),
                color = model_color, linewidth = 0.8
            ) +
            # Observed data points (only for countries with data)
            ggplot2::geom_point(
                data = obs_page[obs_page$cases_total >= 20,],
                ggplot2::aes(x = year, y = cfr_observed * 100, 
                            size = cases_total),
                alpha = 0.7, color = obs_color
            ) +
            # Small points for countries with insufficient data
            ggplot2::geom_point(
                data = obs_page[obs_page$cases_total < 20,],
                ggplot2::aes(x = year, y = cfr_observed * 100),
                alpha = 0.3, color = obs_color, size = 0.5
            ) +
            # Facet by country
            ggplot2::facet_wrap(
                ~ facet_label, 
                ncol = 5,
                scales = "free_y"
            ) +
            # Custom scales
            ggplot2::scale_alpha_manual(
                values = c("TRUE" = 0.3, "FALSE" = 0.1),
                guide = "none"
            ) +
            ggplot2::scale_linetype_manual(
                values = c("TRUE" = "solid", "FALSE" = "dashed"),
                guide = "none"
            ) +
            ggplot2::scale_x_continuous(
                breaks = brk(20),
                limits = c(yr_min, yr_max)
            ) +
            ggplot2::scale_y_continuous(
                labels = scales::percent_format(scale = 1),
                limits = c(0, NA)
            ) +
            ggplot2::scale_size_continuous(
                name = "Cases",
                range = c(0.3, 2.5),
                trans = "log10",
                labels = scales::comma,
                guide = "none"
            ) +
            # Labels
            ggplot2::labs(
                title = sprintf("All MOSAIC Countries CFR Trends (Page %d of %d)", page, n_all_pages),
                subtitle = paste("Solid lines: country-specific fit | ",
                               "Dashed lines: population average (location absent from the fit)\n",
                               "Large points: years with \u226520 cases | Shaded: trends held at the last fitted year"),
                x = "Year",
                y = "Case Fatality Ratio (%)",
                caption = if(page == n_all_pages) paste(
                    sprintf("Total countries: %d | Country-specific fit: %d | Population average: %d",
                           n_all_countries,
                           sum(unique(all_mosaic_estimates$iso_code) %in% countries_with_data),
                           sum(!unique(all_mosaic_estimates$iso_code) %in% countries_with_data)),
                    "\nPoint size indicates number of reported cases"
                ) else ""
            ) +
            # Theme
            ggplot2::theme_minimal(base_size = 8) +
            ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", size = 12),
                plot.subtitle = ggplot2::element_text(color = "gray40", size = 9),
                plot.caption = ggplot2::element_text(size = 7, hjust = 0, color = "gray50"),
                strip.text = ggplot2::element_text(face = "bold", size = 6),
                strip.background = ggplot2::element_rect(fill = "gray95", color = NA),
                panel.grid.minor = ggplot2::element_blank(),
                panel.spacing = ggplot2::unit(0.3, "lines"),
                plot.margin = ggplot2::margin(0.3, 0.3, 0.3, 0.3, "cm"),
                axis.text = ggplot2::element_text(size = 6),
                axis.title = ggplot2::element_text(size = 8),
                panel.border = ggplot2::element_rect(color = "gray90", fill = NA, linewidth = 0.5)
            )
        
        print(p_all)
    }
    
    # Close PDF device
    grDevices::dev.off()
    
    if (verbose) {
        message(sprintf("PDF saved to: %s", pdf_file))
        message(sprintf("  Total pages: %d", n_pages + 3 + n_all_pages))
        message(sprintf("  Countries plotted: %d (selected) + %d (all MOSAIC)", 
                       length(countries_to_plot), n_all_countries))
    }
    
    # Return plots invisibly
    invisible(list(
        overview = p1,
        summary = p_summary,
        pdf_path = pdf_file
    ))
}
