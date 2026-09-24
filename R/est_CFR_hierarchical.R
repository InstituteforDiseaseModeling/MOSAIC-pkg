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
#' The country factor is keyed on \code{iso_code}, not on the country-name
#' string, so an ISO that appears under more than one spelling in the WHO annual
#' file still gets exactly one factor level. Countries absent from the training
#' filter receive the population-average prediction, with every country-indexed
#' smooth (random intercept and factor smooth) excluded.
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
#'   \item country_effects: Country random intercepts (one row per country in
#'     the model, columns \code{iso_code}, \code{country}, \code{random_effect})
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

    # Load WHO annual data (current canonical file written by process_WHO_annual_data)
    who_data_path <- file.path(PATHS$DATA_WHO_ANNUAL, "who_afro_annual.csv")
    if (!file.exists(who_data_path)) {
        stop("WHO annual data not found. Please run process_WHO_annual_data(PATHS) first.")
    }

    if (verbose) message("Loading WHO annual cholera data...")
    who_data <- utils::read.csv(who_data_path, stringsAsFactors = FALSE)

    # Back-compat: if older snapshots lack the coverage columns, fill them as
    # full-year so downstream weighting/filters become no-ops.
    if (!"coverage_days" %in% colnames(who_data)) who_data$coverage_days <- 365L
    if (!"year_fraction" %in% colnames(who_data)) who_data$year_fraction <- 1.0

    # Canonical iso_code -> country-name lookup.
    # A single ISO can appear under several name spellings in the WHO annual
    # file (CIV is present as both "Cote d'Ivoire" and "Cote D'ivoire"). The
    # model keys on iso_code; this map exists only to attach one human-readable
    # label per ISO to the outputs. Rule: the most frequent spelling wins, ties
    # broken alphabetically. Deterministic for a given input file.
    who_named <- who_data[who_data$country != "AFRO Region" &
                          !is.na(who_data$iso_code) & nzchar(who_data$iso_code), ]
    iso_name_map <- data.frame(
        iso_code = sort(unique(who_named$iso_code)),
        stringsAsFactors = FALSE
    )
    iso_name_map$country <- vapply(
        iso_name_map$iso_code,
        function(z) {
            nm <- who_named$country[who_named$iso_code == z]
            nm <- nm[!is.na(nm) & nzchar(nm)]
            if (!length(nm)) return(z)
            tab <- table(nm)
            sort(names(tab)[tab == max(tab)])[1]
        },
        character(1),
        USE.NAMES = FALSE
    )

    # Data preparation
    if (verbose) message("Preparing data for hierarchical GAM...")

    # Filter data - only use MOSAIC framework countries.
    # Partial-year observations (year_fraction < 1) are KEPT — the binomial
    # likelihood already weights by cases_total, so partial-year rows with
    # smaller case counts naturally receive less weight. The min_cases filter
    # protects against extremely small-N high-variance years.
    model_data <- who_data[
        who_data$country != "AFRO Region" &
        who_data$iso_code %in% MOSAIC::iso_codes_mosaic &
        !is.na(who_data$cases_total) &
        !is.na(who_data$deaths_total) &
        who_data$cases_total >= min_cases,
    ]

    # Create country factor.
    # Keyed on iso_code, NOT on the country name string: the WHO annual file
    # carries two spellings for CIV, which as a name-keyed factor produced 41
    # levels for 40 ISOs, split CIV's data across two half-sized factor smooths,
    # and duplicated every CIV row of the prediction grid.
    model_data$country_factor <- as.factor(model_data$iso_code)

    # Attach the canonical name so the `country` column of all outputs is
    # one-to-one with iso_code.
    model_data$country <- iso_name_map$country[match(model_data$iso_code, iso_name_map$iso_code)]

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
    n_countries <- length(unique(model_data$iso_code))
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
    
    # One row per MOSAIC ISO, carrying the single canonical country name.
    all_mosaic_countries <- iso_name_map[
        iso_name_map$iso_code %in% MOSAIC::iso_codes_mosaic,
        c("iso_code", "country"), drop = FALSE]

    # Check if any MOSAIC countries are completely missing
    missing_iso <- setdiff(MOSAIC::iso_codes_mosaic, all_mosaic_countries$iso_code)
    if (length(missing_iso) > 0) {
        warning(paste("Some MOSAIC countries not found in WHO data:",
                     paste(missing_iso, collapse = ", "),
                     "\nUsing ISO code as country name for these."))
        # Add missing countries with ISO as name
        missing_df <- data.frame(
            iso_code = missing_iso,
            country = missing_iso,
            stringsAsFactors = FALSE
        )
        all_mosaic_countries <- rbind(all_mosaic_countries, missing_df)
    }
    rownames(all_mosaic_countries) <- NULL

    # Create full prediction grid
    pred_grid <- expand.grid(
        year = all_years,
        iso_code = MOSAIC::iso_codes_mosaic,  # Use ISO codes for consistency
        stringsAsFactors = FALSE
    )

    # Add country names (one-to-one with iso_code, so row count is preserved)
    stopifnot(!anyDuplicated(all_mosaic_countries$iso_code))
    n_grid_before_merge <- nrow(pred_grid)
    pred_grid <- merge(pred_grid, all_mosaic_countries, by = "iso_code", all.x = TRUE)
    stopifnot(nrow(pred_grid) == n_grid_before_merge)

    # Countries not in the model are assigned an arbitrary (valid) factor level
    # purely to satisfy predict.gam's factor-level check; every country-indexed
    # term is excluded for those rows below, so the level never contributes.
    model_isos <- levels(model_data$country_factor)
    pred_grid$country_factor <- factor(
        ifelse(pred_grid$iso_code %in% model_isos, pred_grid$iso_code, model_isos[1]),
        levels = model_isos
    )

    # Identify the country-indexed smooths that must be dropped when predicting
    # for a country that was not in the training set. Derived from the fitted
    # model rather than hardcoded, so it stays correct if the formula changes.
    #
    # Both `s(country_factor)` (random intercept) AND `s(year, country_factor)`
    # (the factor smooth) are country-indexed. Excluding only the former left the
    # factor smooth in place, so every country absent from the training filter
    # silently inherited the FIRST factor level's fitted temporal trend rather
    # than the population average.
    country_smooth_labels <- vapply(gam_model$smooth, function(s) s$label, character(1))[
        vapply(gam_model$smooth, function(s) "country_factor" %in% s$term, logical(1))
    ]
    if (length(country_smooth_labels) == 0L) {
        stop("No country-indexed smooth found in the fitted GAM; cannot compute population-average predictions.")
    }

    # Get predictions
    countries_without_data <- setdiff(MOSAIC::iso_codes_mosaic, model_isos)

    if (length(countries_without_data) > 0 && verbose) {
        message(sprintf("  Using population-level estimates for %d countries without sufficient data: %s",
                       length(countries_without_data),
                       paste(countries_without_data, collapse = ", ")))
        message(sprintf("  Excluding country-indexed terms for those rows: %s",
                       paste(country_smooth_labels, collapse = ", ")))
    }

    # Predict for countries in the model normally
    in_model_idx <- pred_grid$iso_code %in% model_isos
    pred_link_in_model <- predict(gam_model,
                                  newdata = pred_grid[in_model_idx,],
                                  type = "link",
                                  se.fit = TRUE)

    # For countries not in model, use the population average (exclude ALL
    # country-indexed terms, intercept + trend)
    pred_link_out_model <- predict(gam_model,
                                   newdata = pred_grid[!in_model_idx,],
                                   type = "link",
                                   exclude = country_smooth_labels,
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

    # Select the random-intercept coefficients by the smooth object's own
    # coefficient block, NOT by name matching. `grep("country_factor", ...)`
    # matches the `s(country_factor)` random intercepts AND every
    # `s(year,country_factor)` factor-smooth basis coefficient, so it returned
    # (n_levels + 4*n_levels) coefficients against an n_levels-long name vector,
    # which R then recycled — mislabelling every row of the output file.
    re_smooth <- Filter(
        function(s) inherits(s, "random.effect") && identical(s$term, "country_factor"),
        gam_model$smooth
    )

    if (length(re_smooth) == 1L) {
        re_smooth <- re_smooth[[1]]
        re_idx <- re_smooth$first.para:re_smooth$last.para
        re_levels <- levels(model_data$country_factor)

        # Cardinality is necessary but NOT sufficient: assert one coefficient
        # per factor level before pairing the two ordered vectors.
        if (length(re_idx) != length(re_levels)) {
            stop(sprintf(
                "Random-effect coefficient block has %d entries for %d country levels.",
                length(re_idx), length(re_levels)))
        }

        country_effects <- data.frame(
            iso_code = re_levels,
            country = iso_name_map$country[match(re_levels, iso_name_map$iso_code)],
            random_effect = unname(coef(gam_model)[re_idx]),
            stringsAsFactors = FALSE
        )
        country_effects$country[is.na(country_effects$country)] <- country_effects$iso_code[is.na(country_effects$country)]

        # Sort by effect size
        country_effects <- country_effects[order(country_effects$random_effect, decreasing = TRUE), ]
        rownames(country_effects) <- NULL
    } else {
        country_effects <- NULL
    }

    # Extract temporal trend (population average)
    if (verbose) message("Extracting temporal trend...")

    # A placeholder factor level is needed only to satisfy predict.gam; every
    # country-indexed term is excluded, so the choice does not affect the result.
    temporal_grid <- data.frame(
        year = all_years,
        country_factor = factor(model_isos[1], levels = model_isos)
    )

    # Predict without ANY country-indexed term. Excluding only
    # `s(country_factor)` left the factor smooth in, so the "population average"
    # trend was in fact the first factor level's country-specific trend.
    temporal_pred <- predict(gam_model,
                           newdata = temporal_grid,
                           type = "link",
                           exclude = country_smooth_labels,
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
