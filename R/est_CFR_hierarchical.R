#' Estimate Case Fatality Rate using Hierarchical GAM with Time Series Splines
#'
#' @description
#' Fits a hierarchical generalized additive model (GAM) to estimate case fatality rates (CFR)
#' across African countries and time periods. The model uses smooth splines for temporal trends
#' with country-specific random effects, handling missingness by borrowing strength across
#' countries and years. Results are saved to MODEL_INPUT directory for use in MOSAIC simulations.
#'
#' @param PATHS List containing paths to data directories, typically from get_paths()
#' @param min_cases Integer, minimum number of cases required to include observation (default 20)
#' @param k_year Integer, number of basis functions for year spline (default 12)
#' @param include_country_trends Logical, whether to include country-specific temporal trends (default TRUE)
#' @param population_weighted Logical, whether to weight observations by population size from UN data (default FALSE)
#' @param save_diagnostics Logical, whether to save diagnostic plots (default TRUE)
#' @param verbose Logical, whether to print progress messages (default TRUE)
#'
#' @details
#' The function fits a hierarchical GAM model with the following structure:
#' \itemize{
#'   \item Global temporal trend using thin-plate splines
#'   \item Country-specific random intercepts
#'   \item Optional country-specific smooth deviations from global trend
#'   \item Binomial likelihood with logit link for proper handling of rates
#'   \item Predictions extended to current year based on Sys.Date()
#' }
#'
#' Model outputs include:
#' \itemize{
#'   \item Point estimates and 95% confidence intervals for all country-years
#'   \item Country-specific random effects quantifying systematic differences
#'   \item Smooth temporal trends showing CFR evolution over time
#'   \item Model diagnostics and validation metrics
#' }
#'
#' @return List containing:
#' \itemize{
#'   \item model: Fitted GAM model object
#'   \item predictions: Data frame with CFR estimates for all countries and years
#'   \item country_effects: Estimated country-specific deviations
#'   \item temporal_trend: Population-average temporal trend
#'   \item validation: Cross-validation results for recent years
#'   \item summary: Summary statistics and model fit metrics
#' }
#'
#' @importFrom mgcv gam s predict.gam
#' @importFrom stats binomial predict AIC
#' @importFrom utils read.csv write.csv
#' @importFrom grDevices pdf dev.off
#'
#' @examples
#' \dontrun{
#' # Standard usage
#' PATHS <- get_paths()
#' cfr_model <- est_CFR_hierarchical(PATHS)
#'
#' # Custom settings for smoother trends
#' cfr_model <- est_CFR_hierarchical(
#'   PATHS,
#'   min_cases = 50,
#'   k_year = 8,
#'   include_country_trends = FALSE
#' )
#' }
#'
#' @export
est_CFR_hierarchical <- function(
    PATHS,
    min_cases = 20,
    k_year = 12,
    include_country_trends = TRUE,
    population_weighted = FALSE,
    save_diagnostics = TRUE,
    verbose = TRUE
) {

    # Check for required package
    if (!requireNamespace("mgcv", quietly = TRUE)) {
        stop("Package 'mgcv' is required. Please install it using: install.packages('mgcv')")
    }

    # Load WHO annual data
    who_data_path <- file.path(PATHS$DATA_WHO_ANNUAL, "who_afro_annual_1949_2024.csv")
    if (!file.exists(who_data_path)) {
        stop("WHO annual data not found. Please run process_WHO_annual_data(PATHS) first.")
    }

    if (verbose) message("Loading WHO annual cholera data...")
    who_data <- utils::read.csv(who_data_path, stringsAsFactors = FALSE)

    # Data preparation
    if (verbose) message("Preparing data for hierarchical GAM...")

    # Filter data - only use MOSAIC framework countries
    model_data <- who_data[
        who_data$country != "AFRO Region" &
        who_data$iso_code %in% MOSAIC::iso_codes_mosaic &  # Filter for MOSAIC countries only
        !is.na(who_data$cases_total) &
        !is.na(who_data$deaths_total) &
        who_data$cases_total >= min_cases,
    ]

    # Create country factor
    model_data$country_factor <- as.factor(model_data$country)

    # Add log offset for cases
    model_data$log_cases <- log(model_data$cases_total)
    
    # Add population weighting if requested
    if (population_weighted) {
        if (verbose) message("  Loading UN population data for weighting...")
        
        # Load UN population data
        un_pop_path <- file.path(PATHS$DATA_PROCESSED, "demographics/UN_world_population_prospects_1967_2100.csv")
        if (!file.exists(un_pop_path)) {
            warning("UN population data not found. Proceeding without population weighting.")
            population_weighted <- FALSE
        } else {
            un_pop <- utils::read.csv(un_pop_path, stringsAsFactors = FALSE)
            
            # Merge population data with model data
            model_data <- merge(model_data, 
                              un_pop[, c("iso_code", "year", "total_population")],
                              by.x = c("iso_code", "year"),
                              by.y = c("iso_code", "year"),
                              all.x = TRUE)
            
            # Check for missing population data
            missing_pop <- is.na(model_data$total_population)
            if (any(missing_pop)) {
                warning(sprintf("Population data missing for %d observations. Using median population for these.", 
                              sum(missing_pop)))
                model_data$total_population[missing_pop] <- median(model_data$total_population, na.rm = TRUE)
            }
            
            # Create population weights
            # Weight = sqrt(population) to moderate the influence of population size
            # Normalize to have mean = number of cases for proper binomial weighting
            model_data$pop_weight <- sqrt(model_data$total_population / median(model_data$total_population, na.rm = TRUE))
            
            # Combine with case weights, but scale to prevent overflow
            # First normalize population weights to [0.5, 2] range to avoid extreme weights
            model_data$pop_weight <- pmin(pmax(model_data$pop_weight, 0.5), 2)
            
            # Apply population adjustment to case weights
            model_data$weights <- model_data$cases_total * model_data$pop_weight
            
            # Ensure weights are reasonable (not too large to cause overflow)
            max_weight <- 1e6  # Maximum reasonable weight
            if (any(model_data$weights > max_weight)) {
                model_data$weights <- model_data$weights * (max_weight / max(model_data$weights))
                if (verbose) message("  Weights rescaled to prevent overflow")
            }
            
            if (verbose) {
                message(sprintf("  Population weighting applied (range: %.2f to %.2f)",
                              min(model_data$pop_weight), max(model_data$pop_weight)))
            }
        }
    } else {
        # For standard binomial weighting, we don't actually need to set weights
        # The binomial family with cbind(deaths, survivors) handles this automatically
        # Setting weights = NULL or not using weights argument at all
        model_data$weights <- NULL
    }

    n_obs <- nrow(model_data)
    n_countries <- length(unique(model_data$country))
    year_range <- range(model_data$year)

    if (verbose) {
        message(sprintf("  Using %d observations from %d countries", n_obs, n_countries))
        message(sprintf("  Year range: %d to %d", year_range[1], year_range[2]))
    }

    # Model fitting
    if (verbose) message("\nFitting hierarchical GAM model...")

    # Build model formula based on settings
    if (include_country_trends) {
        # Full model with country-specific trends
        gam_formula <- deaths_total ~
            s(year, k = k_year, bs = "tp") +           # Global temporal trend
            s(country_factor, bs = "re") +             # Country random intercepts
            s(year, country_factor, bs = "fs", k = 4)  # Country-specific trends

        model_type <- "Full hierarchical model with country-specific trends"
    } else {
        # Simpler model without country-specific trends
        gam_formula <- deaths_total ~
            s(year, k = k_year, bs = "tp") +  # Global temporal trend
            s(country_factor, bs = "re")       # Country random intercepts only

        model_type <- "Hierarchical model with random intercepts only"
    }

    if (verbose) message(paste("  Model type:", model_type))

    # Fit the GAM using cbind for binomial response
    # Create response matrix: successes (deaths) and failures (survivors)
    model_data$survivors <- model_data$cases_total - model_data$deaths_total

    # Update formula to use cbind response
    if (include_country_trends) {
        gam_formula <- cbind(deaths_total, survivors) ~
            s(year, k = k_year, bs = "tp") +           # Global temporal trend
            s(country_factor, bs = "re") +             # Country random intercepts
            s(year, country_factor, bs = "fs", k = 4)  # Country-specific trends
    } else {
        gam_formula <- cbind(deaths_total, survivors) ~
            s(year, k = k_year, bs = "tp") +  # Global temporal trend
            s(country_factor, bs = "re")       # Country random intercepts only
    }

    # Fit GAM with or without population weights
    if (!is.null(model_data$weights)) {
        gam_model <- mgcv::gam(
            formula = gam_formula,
            family = binomial(link = "logit"),
            data = model_data,
            weights = model_data$weights,
            method = "REML",
            control = list(trace = verbose)
        )
    } else {
        # Standard binomial without additional weights
        gam_model <- mgcv::gam(
            formula = gam_formula,
            family = binomial(link = "logit"),
            data = model_data,
            method = "REML",
            control = list(trace = verbose)
        )
    }

    if (verbose) {
        message("\nModel fitting complete!")
        message(sprintf("  Deviance explained: %.1f%%", summary(gam_model)$dev.expl * 100))
        message(sprintf("  REML score: %.2f", gam_model$gcv.ubre))
        message(sprintf("  AIC: %.1f", AIC(gam_model)))
    }

    # Generate predictions for all country-years
    if (verbose) message("\nGenerating predictions...")

    # Create prediction grid for ALL MOSAIC countries
    # Extend predictions to current year
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    min_year <- min(who_data$year)
    max_year <- max(max(who_data$year), current_year)
    all_years <- min_year:max_year
    
    if (verbose) {
        message(sprintf("  Generating predictions for years %d to %d (including current year %d)",
                       min(all_years), max(all_years), current_year))
    }
    
    # Get all MOSAIC countries with their names
    # First get countries that were in the model
    countries_in_model <- unique(model_data[, c("country", "iso_code")])
    
    # Get all MOSAIC countries from WHO data (even those without sufficient data)
    all_mosaic_in_who <- unique(who_data[who_data$iso_code %in% MOSAIC::iso_codes_mosaic, 
                                         c("country", "iso_code")])
    
    # Combine to ensure we have all MOSAIC countries
    all_mosaic_countries <- unique(rbind(countries_in_model, all_mosaic_in_who))
    
    # Check if any MOSAIC countries are completely missing
    missing_iso <- setdiff(MOSAIC::iso_codes_mosaic, all_mosaic_countries$iso_code)
    if (length(missing_iso) > 0) {
        warning(paste("Some MOSAIC countries not found in WHO data:", 
                     paste(missing_iso, collapse = ", "),
                     "\nUsing ISO code as country name for these."))
        # Add missing countries with ISO as name
        missing_df <- data.frame(
            country = missing_iso,
            iso_code = missing_iso,
            stringsAsFactors = FALSE
        )
        all_mosaic_countries <- rbind(all_mosaic_countries, missing_df)
    }
    
    # Create full prediction grid
    pred_grid <- expand.grid(
        year = all_years,
        iso_code = MOSAIC::iso_codes_mosaic,  # Use ISO codes for consistency
        stringsAsFactors = FALSE
    )
    
    # Add country names
    pred_grid <- merge(pred_grid, all_mosaic_countries, by = "iso_code", all.x = TRUE)
    
    # For countries not in the model, we'll use the population average
    # Create a factor that includes a level for "population_average"
    model_countries <- unique(model_data$country)
    pred_grid$country_factor <- factor(
        ifelse(pred_grid$country %in% model_countries, pred_grid$country, model_countries[1]),
        levels = levels(model_data$country_factor)
    )

    # Get predictions
    # For countries not in model, exclude country-specific effects
    countries_without_data <- unique(pred_grid$iso_code[!pred_grid$country %in% model_countries])
    
    if (length(countries_without_data) > 0 && verbose) {
        message(sprintf("  Using population-level estimates for %d countries without sufficient data: %s",
                       length(countries_without_data), 
                       paste(countries_without_data, collapse = ", ")))
    }
    
    # Predict for countries in the model normally
    in_model_idx <- pred_grid$country %in% model_countries
    pred_link_in_model <- predict(gam_model,
                                  newdata = pred_grid[in_model_idx,],
                                  type = "link",
                                  se.fit = TRUE)
    
    # For countries not in model, use population average (exclude country effects)
    pred_link_out_model <- predict(gam_model,
                                   newdata = pred_grid[!in_model_idx,],
                                   type = "link",
                                   exclude = "s(country_factor)",
                                   se.fit = TRUE)
    
    # Combine predictions
    pred_link <- list(
        fit = numeric(nrow(pred_grid)),
        se.fit = numeric(nrow(pred_grid))
    )
    pred_link$fit[in_model_idx] <- pred_link_in_model$fit
    pred_link$fit[!in_model_idx] <- pred_link_out_model$fit
    pred_link$se.fit[in_model_idx] <- pred_link_in_model$se.fit
    pred_link$se.fit[!in_model_idx] <- pred_link_out_model$se.fit

    # Transform to probability scale
    pred_grid$cfr_estimate <- plogis(pred_link$fit)
    pred_grid$cfr_lower <- plogis(pred_link$fit - 1.96 * pred_link$se.fit)
    pred_grid$cfr_upper <- plogis(pred_link$fit + 1.96 * pred_link$se.fit)
    pred_grid$cfr_se <- pred_link$se.fit

    # Reorder columns (ISO code already in pred_grid)
    pred_grid <- pred_grid[, c("country", "iso_code", "year", "cfr_estimate",
                               "cfr_lower", "cfr_upper", "cfr_se")]

    # Extract country effects
    if (verbose) message("Extracting country-specific effects...")

    country_coefs <- coef(gam_model)
    country_idx <- grep("country_factor", names(country_coefs))

    if (length(country_idx) > 0) {
        # Only include countries that were actually in the model
        countries_in_model_names <- sort(unique(model_data$country))
        country_effects <- data.frame(
            country = countries_in_model_names,
            random_effect = country_coefs[country_idx]
        )
        # Sort by effect size
        country_effects <- country_effects[order(country_effects$random_effect, decreasing = TRUE), ]
        rownames(country_effects) <- NULL
    } else {
        country_effects <- NULL
    }

    # Extract temporal trend (population average)
    if (verbose) message("Extracting temporal trend...")

    # Use first country from model for temporal trend extraction
    first_model_country <- unique(model_data$country)[1]
    temporal_grid <- data.frame(
        year = all_years,
        country_factor = factor(first_model_country, levels = levels(model_data$country_factor))
    )

    # Predict without country effects
    temporal_pred <- predict(gam_model,
                           newdata = temporal_grid,
                           type = "link",
                           exclude = "s(country_factor)",
                           se.fit = TRUE)

    temporal_trend <- data.frame(
        year = all_years,
        cfr_trend = plogis(temporal_pred$fit),
        cfr_trend_lower = plogis(temporal_pred$fit - 1.96 * temporal_pred$se.fit),
        cfr_trend_upper = plogis(temporal_pred$fit + 1.96 * temporal_pred$se.fit)
    )

    # Model validation using leave-one-year-out for recent years
    if (verbose) message("\nPerforming model validation...")

    validation_results <- list()
    validation_years <- 2020:2023

    for (val_year in validation_years) {
        # Fit model without validation year
        train_data <- model_data[model_data$year != val_year, ]
        test_data <- model_data[model_data$year == val_year, ]

        if (nrow(test_data) > 0) {
            # Create response for training data
            train_data$survivors <- train_data$cases_total - train_data$deaths_total
            test_data$survivors <- test_data$cases_total - test_data$deaths_total

            # Fit model on training data (with weights if applicable)
            if ("weights" %in% names(train_data)) {
                val_model <- mgcv::gam(
                    formula = gam_formula,
                    family = binomial(link = "logit"),
                    data = train_data,
                    weights = train_data$weights,
                    method = "REML",
                    control = list(trace = FALSE)
                )
            } else {
                val_model <- mgcv::gam(
                    formula = gam_formula,
                    family = binomial(link = "logit"),
                    data = train_data,
                    method = "REML",
                    control = list(trace = FALSE)
                )
            }

            # Predict on test data
            test_pred <- predict(val_model, newdata = test_data, type = "link", se.fit = TRUE)
            test_data$cfr_pred <- plogis(test_pred$fit)
            test_data$cfr_lower <- plogis(test_pred$fit - 1.96 * test_pred$se.fit)
            test_data$cfr_upper <- plogis(test_pred$fit + 1.96 * test_pred$se.fit)

            # Calculate metrics
            observed_cfr <- test_data$deaths_total / test_data$cases_total
            mae <- mean(abs(observed_cfr - test_data$cfr_pred))
            coverage <- mean(observed_cfr >= test_data$cfr_lower &
                           observed_cfr <= test_data$cfr_upper)

            validation_results[[as.character(val_year)]] <- list(
                year = val_year,
                n_obs = nrow(test_data),
                mae = mae,
                coverage = coverage
            )
        }
    }

    validation_df <- do.call(rbind, lapply(validation_results, as.data.frame))

    if (verbose && !is.null(validation_df)) {
        message(sprintf("  Mean absolute error: %.4f", mean(validation_df$mae)))
        message(sprintf("  95%% CI coverage: %.1f%%", mean(validation_df$coverage) * 100))
    }

    # Save outputs to MODEL_INPUT directory
    if (verbose) message("\nSaving outputs...")

    # Ensure MODEL_INPUT directory exists
    if (!dir.exists(PATHS$MODEL_INPUT)) {
        dir.create(PATHS$MODEL_INPUT, recursive = TRUE)
    }

    # Convert predictions to MOSAIC parameter format for mu_jt
    if (verbose) message("  Converting to MOSAIC parameter format for mu_jt...")

    # Create parameter data frame using MOSAIC::make_param_df function
    param_mu_list <- list()

    for (i in 1:nrow(pred_grid)) {
        row <- pred_grid[i,]

        # Create entries for each parameter of the beta distribution
        # Using method of moments to convert from mean and CI to beta parameters
        mean_cfr <- row$cfr_estimate
        var_cfr <- ((row$cfr_upper - row$cfr_lower) / (2 * 1.96))^2

        # Method of moments for beta distribution
        # Ensure variance is less than mean*(1-mean) for valid beta
        max_var <- mean_cfr * (1 - mean_cfr)
        if (var_cfr >= max_var) {
            var_cfr <- max_var * 0.99  # Slightly reduce to ensure valid parameters
        }

        if (mean_cfr > 0 && mean_cfr < 1 && var_cfr > 0) {
            alpha <- mean_cfr * ((mean_cfr * (1 - mean_cfr) / var_cfr) - 1)
            beta_param <- (1 - mean_cfr) * ((mean_cfr * (1 - mean_cfr) / var_cfr) - 1)

            # Ensure parameters are positive
            alpha <- max(alpha, 0.01)
            beta_param <- max(beta_param, 0.01)
        } else {
            # Fallback to reasonable defaults
            alpha <- 2
            beta_param <- 98
        }

        # Create parameter entries using MOSAIC::make_param_df
        # Add point estimate (mean)
        param_mu_list[[length(param_mu_list) + 1]] <- MOSAIC::make_param_df(
            variable_name = "mu",
            variable_description = "disease mortality rate (case fatality ratio)",
            parameter_distribution = "point",
            i = NA,
            j = row$iso_code,
            t = row$year,
            parameter_name = "mean",
            parameter_value = mean_cfr
        )
        
        # Add beta distribution parameters
        param_mu_list[[length(param_mu_list) + 1]] <- MOSAIC::make_param_df(
            variable_name = "mu",
            variable_description = "disease mortality rate (case fatality ratio)",
            parameter_distribution = "beta",
            i = NA,
            j = row$iso_code,
            t = row$year,
            parameter_name = "shape1",
            parameter_value = alpha
        )

        param_mu_list[[length(param_mu_list) + 1]] <- MOSAIC::make_param_df(
            variable_name = "mu",
            variable_description = "disease mortality rate (case fatality ratio)",
            parameter_distribution = "beta",
            i = NA,
            j = row$iso_code,
            t = row$year,
            parameter_name = "shape2",
            parameter_value = beta_param
        )
    }

    # Combine all parameter entries
    param_mu <- do.call(rbind, param_mu_list)

    # Save in MOSAIC parameter format
    param_file <- file.path(PATHS$MODEL_INPUT, "param_mu_disease_mortality.csv")
    utils::write.csv(param_mu, param_file, row.names = FALSE)
    if (verbose) message(paste("  Disease mortality rate parameters (mu_jt) saved to:", param_file))

    # Also save the direct estimates for reference
    pred_file <- file.path(PATHS$MODEL_INPUT, "cfr_hierarchical_estimates.csv")
    utils::write.csv(pred_grid, pred_file, row.names = FALSE)
    if (verbose) message(paste("  CFR estimates saved to:", pred_file))

    # Save temporal trend
    trend_file <- file.path(PATHS$MODEL_INPUT, "cfr_temporal_trend.csv")
    utils::write.csv(temporal_trend, trend_file, row.names = FALSE)
    if (verbose) message(paste("  Temporal trend saved to:", trend_file))

    # Save country effects if available
    if (!is.null(country_effects)) {
        effects_file <- file.path(PATHS$MODEL_INPUT, "cfr_country_effects.csv")
        utils::write.csv(country_effects, effects_file, row.names = FALSE)
        if (verbose) message(paste("  Country effects saved to:", effects_file))
    }

    # Save model diagnostics
    if (save_diagnostics) {
        diag_file <- file.path(PATHS$MODEL_INPUT, "cfr_model_diagnostics.pdf")
        grDevices::pdf(diag_file, width = 10, height = 10)

        # GAM check plots
        mgcv::gam.check(gam_model)

        # Additional diagnostic plots
        plot(gam_model, pages = 1, all.terms = TRUE)

        grDevices::dev.off()
        if (verbose) message(paste("  Diagnostic plots saved to:", diag_file))
    }

    # Save model summary
    summary_list <- list(
        model_type = model_type,
        n_observations = n_obs,
        n_countries = n_countries,
        year_range = year_range,
        min_cases_threshold = min_cases,
        deviance_explained = summary(gam_model)$dev.expl,
        aic = AIC(gam_model),
        reml_score = gam_model$gcv.ubre,
        validation_mae = if (!is.null(validation_df)) mean(validation_df$mae) else NA,
        validation_coverage = if (!is.null(validation_df)) mean(validation_df$coverage) else NA,
        timestamp = Sys.time()
    )

    summary_file <- file.path(PATHS$MODEL_INPUT, "cfr_model_summary.rds")
    saveRDS(summary_list, summary_file)
    if (verbose) message(paste("  Model summary saved to:", summary_file))

    # Compile results
    results <- list(
        model = gam_model,
        predictions = pred_grid,
        country_effects = country_effects,
        temporal_trend = temporal_trend,
        validation = validation_df,
        summary = summary_list,
        settings = list(
            min_cases = min_cases,
            k_year = k_year,
            include_country_trends = include_country_trends
        )
    )

    class(results) <- c("cfr_hierarchical_model", "list")

    if (verbose) {
        message("\n=== CFR Hierarchical Model Complete ===")
        message(sprintf("Files saved to: %s", PATHS$MODEL_INPUT))
    }

    return(invisible(results))
}
