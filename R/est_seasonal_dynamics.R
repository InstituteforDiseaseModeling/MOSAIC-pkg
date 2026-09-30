#' Estimate Seasonal Dynamics for Cholera and Precipitation Using Daily Fourier Series
#'
#' This function retrieves historical precipitation data, processes cholera case data,
#' and fits seasonal dynamics models using a double Fourier series at the daily scale (p = 365).
#' Weekly data is expanded by assigning each weekly value to every day in that week.
#'
#' @param PATHS A list containing paths where raw and processed data are stored. See `get_paths()`.
#' @param date_start A date in YYYY-MM-DD format indicating the start date of the precipitation data.
#' @param date_stop A date in YYYY-MM-DD format indicating the stop date of the precipitation data.
#' @param min_obs The minimum number of observations required to fit the Fourier series to cholera case data.
#' @param clustering_method The clustering method to use (e.g., "kmeans", "ward.D2", etc.).
#' @param k Number of clusters for grouping countries by seasonality.
#' @param exclude_iso_codes Optional character vector of ISO codes to exclude from clustering and neighbor matching.
#' @param data_sources Character vector of data sources to include. Default is c('WHO', 'JHU', 'SUPP').
#' @param envelope_floor Minimum allowed value of the human-transmission envelope 1 + f(t) over the year (default 0.1); case-fit coefficients whose envelope dips lower are shrunk towards zero.
#'
#' @details
#' \strong{Phase convention.} The coefficients are fit against calendar
#' day-of-year (\code{lubridate::yday()}, t = 1 is 1 January) with
#' \code{p = 365}. The engine (\code{sim_beta_jt_human()}) evaluates the
#' envelope at the day-of-year of each simulated day, so the coefficients keep
#' this meaning whatever the simulation \code{date_start}.
#'
#' \strong{Weekly alignment.} Precipitation is summed over the same ISO weeks
#' (Monday to Sunday) as the weekly surveillance file and merged on the week's
#' start date; weeks with fewer than 7 days of precipitation at the edges of the
#' window are dropped.
#'
#' \strong{Positivity of the envelope.} The engine uses the case-fit
#' coefficients as a multiplicative modulation of the mean human transmission
#' rate, \code{beta_j0_hum * (1 + f(t))}. A Fourier fit to affine-normalised
#' weekly cases has no such constraint and can dip below -1, which the engine
#' clamps to zero transmission for weeks at a time. Multiplicative seasonal
#' forcing requires the envelope to stay positive (amplitude below 1 in
#' \code{beta(t) = beta0 (1 + beta1 cos(wt))}; Keeling & Rohani 2008,
#' \emph{Modeling Infectious Diseases in Humans and Animals}, section 5.2;
#' King et al. 2008, \emph{Nature} 454:877, fit cholera seasonality on the log
#' scale for the same reason). When \code{min_t(1 + f(t)) < envelope_floor}
#' the four case coefficients (and their SE and CI) are multiplied by the single
#' factor \code{(1 - envelope_floor) / -min_t f(t)}, which keeps the fitted
#' phase and the relative shape of the season and lowers only its amplitude.
#' The factor applied is recorded in the \code{envelope_scale} column
#' (1 = unchanged). \code{envelope_floor} is a numerical positivity margin, not
#' an estimated trough: it leaves room for prior draws around the mean
#' (make_priors_default widens the SE) without crossing zero.
#'
#' @return Saves daily fitted values and parameter estimates to CSV.
#' @export
#'

est_seasonal_dynamics <- function(PATHS,
                                  date_start,
                                  date_stop,
                                  min_obs = 30,
                                  clustering_method,
                                  k,
                                  exclude_iso_codes = NULL,
                                  data_sources = c('WHO', 'JHU', 'SUPP'),
                                  envelope_floor = 0.1) {

     if (!is.numeric(envelope_floor) || length(envelope_floor) != 1L ||
         !is.finite(envelope_floor) || envelope_floor < 0 || envelope_floor >= 1) {
          stop("`envelope_floor` must be a single number in [0, 1).")
     }

     requireNamespace("dplyr")
     requireNamespace("minpack.lm")
     requireNamespace("FNN")
     requireNamespace("sf")
     requireNamespace("arrow")

     # Read weekly cholera case data from combined surveillance data.
     iso_codes <- MOSAIC::iso_codes_mosaic
     cholera_data <- utils::read.csv(file.path(PATHS$DATA_CHOLERA_WEEKLY, "cholera_surveillance_weekly_combined.csv"), stringsAsFactors = FALSE)

     # Exclude AI-mined rows: seasonal forcing must be estimated from direct
     # surveillance, not synthetic/reconstructed AI gap-fills (which would inject a
     # manufactured seasonal template). The AI signal is confined to the
     # suitability/LSTM path.
     if ("source" %in% names(cholera_data)) {
          cholera_data <- cholera_data[is.na(cholera_data$source) | cholera_data$source != "AI", ]
     }

     # Filter by data sources if specified
     if (!is.null(data_sources) && length(data_sources) > 0) {
          # Include rows where source is in data_sources OR source is NA (for missing data rows)
          cholera_data <- cholera_data[is.na(cholera_data$source) | cholera_data$source %in% data_sources, ]
     }
     
     # Remove rows with NA cases (these are placeholder rows with no actual data)
     cholera_data <- cholera_data[!is.na(cholera_data$cases), ]

     cholera_data <- cholera_data[cholera_data$date_start >= date_start & cholera_data$date_stop < date_stop, ]

     # Count observations AND check for non-zero cases (bugfix for all-zero countries)
     cholera_summary <- cholera_data %>%
          dplyr::group_by(iso_code) %>%
          dplyr::summarize(
               n_obs = dplyr::n(),
               total_cases = sum(cases, na.rm = TRUE),
               .groups = 'drop'
          )

     # Require BOTH sufficient observations AND non-zero cases
     iso_codes_with_usable_data <- cholera_summary %>%
          dplyr::filter(n_obs >= min_obs & total_cases > 0) %>%
          dplyr::pull(iso_code)

     cholera_data <- cholera_data[cholera_data$iso_code %in% iso_codes_with_usable_data, ]
     iso_codes_no_data <- setdiff(iso_codes, iso_codes_with_usable_data)

     # Log for diagnostics
     message(sprintf("Countries with usable data: %d", length(iso_codes_with_usable_data)))
     message(sprintf("Countries needing inference: %d", length(iso_codes_no_data)))
     if (length(iso_codes_no_data) > 0) {
          message(sprintf("  Will infer from neighbors: %s", paste(iso_codes_no_data, collapse=", ")))
     }

     normalize_if_needed <- function(x, verbose = FALSE) {
          tryCatch({
               MOSAIC::check_affine_normalization(x, verbose = verbose)
               x
          }, error = function(e) {
               MOSAIC::calc_affine_normalization(x)
          })
     }

     all_precip_data <- list()
     all_fitted_values <- list()
     all_param_values <- list()

     for (iso in iso_codes) {

          message(glue::glue("Processing {iso}"))
          country_name <- MOSAIC::convert_iso_to_country(iso)
          precip_file <- file.path(PATHS$DATA_CLIMATE_DAILY, glue::glue("{iso}.parquet"))

          if (!file.exists(precip_file)) {
               warning(glue::glue("Missing climate data for {iso}"))
               next
          }

          # Read daily climate data and filter to precipitation
          precip_data <- arrow::read_parquet(precip_file)
          precip_data <- precip_data[precip_data$variable_name == "precipitation_sum", ]
          precip_data$date <- as.Date(precip_data$date)
          precip_data <- precip_data[precip_data$date >= date_start & precip_data$date < date_stop, ]
          # Sum precipitation over ISO weeks (Monday-Sunday), the week definition
          # of the weekly surveillance file, and key both on the week's start
          # date. Partial weeks at the window edges are dropped so every
          # precipitation value is a full 7-day sum.
          week_key <- as.character(lubridate::floor_date(precip_data$date, unit = "week",
                                                         week_start = 1))
          week_sum <- tapply(precip_data$value, week_key, sum, na.rm = TRUE)
          week_len <- tapply(precip_data$value, week_key, length)
          precip_data <- data.frame(week_start = as.Date(names(week_sum)),
                                    weekly_precipitation_sum = as.numeric(week_sum),
                                    n_days = as.integer(week_len[names(week_sum)]))
          precip_data <- precip_data[precip_data$n_days == 7L, ]
          precip_data$iso_code <- iso
          precip_data$year <- lubridate::isoyear(precip_data$week_start)
          precip_data$week <- lubridate::isoweek(precip_data$week_start)

          cholera_sub <- cholera_data[cholera_data$iso_code == iso, c("date_start", "cases")]
          cholera_sub$week_start <- as.Date(cholera_sub$date_start)
          cholera_sub <- cholera_sub[, c("week_start", "cases")]
          merged <- merge(precip_data, cholera_sub, by = "week_start", all.x = TRUE)

          # Expand the weekly data to daily.
          daily_rows <- lapply(seq_len(nrow(merged)), function(i) {
               week_row <- merged[i, ]
               dates <- seq(week_row$week_start, week_row$week_start + 6, by = "1 day")
               data.frame(
                    day = dates,
                    day_of_year = lubridate::yday(dates),
                    iso_code = iso,
                    year = week_row$year,
                    week = week_row$week,
                    weekly_precipitation_sum = week_row$weekly_precipitation_sum,
                    cases = week_row$cases
               )
          })
          daily <- do.call(rbind, daily_rows)
          daily$precip_scaled <- MOSAIC::calc_affine_normalization(daily$weekly_precipitation_sum)
          daily$cases_scaled <- MOSAIC::calc_affine_normalization(daily$cases)

          n_obs <- sum(!is.na(daily$cases_scaled))

          # Fit the Fourier series for precipitation.
          fit_p <- minpack.lm::nlsLM(
               precip_scaled ~ MOSAIC::fourier_series_double(day_of_year, a_1, b_1, a_2, b_2, p = 365, beta0 = 0),
               data = daily,
               start = list(a_1 = 1, b_1 = 1, a_2 = 0.5, b_2 = 0.5)
          )

          coef_p <- coef(fit_p)
          se_p <- sqrt(diag(vcov(fit_p)))
          param_p <- data.frame(
               country_name = country_name,
               country_iso_code = iso,
               response = "precipitation",
               parameter = names(coef_p),
               mean = coef_p,
               se = se_p,
               ci_lo = coef_p - 1.96 * se_p,
               ci_hi = coef_p + 1.96 * se_p,
               envelope_scale = NA_real_
          )

          fitted_days <- 1:365
          fitted_precip <- MOSAIC::fourier_series_double(fitted_days, coef_p["a_1"], coef_p["b_1"],
                                                         coef_p["a_2"], coef_p["b_2"],
                                                         p = 365, beta0 = 0)
          fitted_precip <- normalize_if_needed(fitted_precip)

          if (iso %in% iso_codes_with_usable_data && n_obs >= min_obs) {

               fit_c <- minpack.lm::nlsLM(
                    cases_scaled ~ MOSAIC::fourier_series_double(day_of_year, a_1, b_1, a_2, b_2, p = 365, beta0 = 0),
                    data = daily,
                    start = as.list(coef_p)
               )

               coef_c <- coef(fit_c)
               se_c <- sqrt(diag(vcov(fit_c)))

               # Keep the multiplicative envelope 1 + f(t) above envelope_floor
               # (see @details): shrink amplitude, keep phase and shape.
               envelope_scale <- .seasonal_envelope_scale(coef_c, floor = envelope_floor)
               if (envelope_scale < 1) {
                    message(sprintf(
                         "  %s: case-fit envelope min(1 + f) = %.3f < %.2f; amplitude scaled by %.3f",
                         iso, 1 + .seasonal_envelope_min(coef_c), envelope_floor, envelope_scale))
               }
               coef_c <- coef_c * envelope_scale
               se_c <- se_c * envelope_scale

               param_c <- data.frame(
                    country_name = country_name,
                    country_iso_code = iso,
                    response = "cases",
                    parameter = names(coef_c),
                    mean = coef_c,
                    se = se_c,
                    ci_lo = coef_c - 1.96 * se_c,
                    ci_hi = coef_c + 1.96 * se_c,
                    envelope_scale = envelope_scale
               )

               fitted_cases <- MOSAIC::fourier_series_double(fitted_days, coef_c["a_1"], coef_c["b_1"],
                                                             coef_c["a_2"], coef_c["b_2"],
                                                             p = 365, beta0 = 0)

               fitted_cases <- normalize_if_needed(fitted_cases)

          } else {

               fitted_cases <- rep(NA, 365)
               param_c <- data.frame(
                    country_name = country_name,
                    country_iso_code = iso,
                    response = "cases",
                    parameter = names(coef_p),
                    mean = NA, se = NA, ci_lo = NA, ci_hi = NA,
                    envelope_scale = NA_real_
               )

          }

          fitted_df <- data.frame(
               day = fitted_days,
               iso_code = iso,
               Country = country_name,
               fitted_values_fourier_precip = fitted_precip,
               fitted_values_fourier_cases = fitted_cases
          )

          all_precip_data[[iso]] <- daily
          all_fitted_values[[iso]] <- fitted_df
          all_param_values[[iso]] <- rbind(param_p, param_c)

     }

     combined_precip_data <- do.call(rbind, all_precip_data)
     # Simplify combined_precip_data: create a proper Date column and keep only relevant measurement columns.
     combined_precip_data$date <- as.Date(combined_precip_data$day)
     combined_precip_data <- combined_precip_data[, c("date", "iso_code",
                                                      "weekly_precipitation_sum", "cases",
                                                      "precip_scaled", "cases_scaled")]

     combined_fitted_values_daily <- do.call(rbind, all_fitted_values)
     combined_param_values <- do.call(rbind, all_param_values)
     combined_param_values <- combined_param_values[!(combined_param_values$parameter %in% c("beta0", "p")), ]
     row.names(combined_param_values) <- NULL

     # ---------------------------------------------------------------------------
     # CLUSTERING AND NEIGHBOR INFERENCE AT THE DAILY SCALE (USING BASE R)
     # ---------------------------------------------------------------------------
     message("Estimating clustering of countries based on daily rainfall patterns and inferring seasonal transmission dynamics...")

     # Prepare daily precipitation fitted values for clustering using base R reshape.
     # Exclude any rows with iso codes matching exclude_iso_codes.
     temp <- combined_fitted_values_daily[ !is.na(combined_fitted_values_daily$fitted_values_fourier_precip) &
                                                !(combined_fitted_values_daily$iso_code %in% exclude_iso_codes),
                                           c("iso_code", "day", "fitted_values_fourier_precip") ]
     precip_fitted_df <- reshape(temp, idvar = "iso_code", timevar = "day", direction = "wide")
     precip_matrix <- precip_fitted_df[, -1, drop = FALSE]
     dist_matrix <- dist(precip_matrix)
     hc <- hclust(dist_matrix, method = clustering_method)
     precip_fitted_df$cluster <- cutree(hc, k = k)

     africa <- sf::st_read(dsn = file.path(PATHS$DATA_SHAPEFILES, "AFRICA_ADM0.shp"))
     africa_with_clusters <- merge(africa,
                                   precip_fitted_df[, c("iso_code", "cluster")],
                                   by.x = "iso_a3", by.y = "iso_code",
                                   all.x = TRUE)
     centroids <- sf::st_centroid(africa)
     coords <- sf::st_coordinates(centroids)
     initial_k <- 10

     # For each country with no cholera data, identify the best neighbor based on precipitation correlation.
     for (country_iso_code_no_data in iso_codes_no_data) {

          if (!is.null(exclude_iso_codes) && country_iso_code_no_data %in% exclude_iso_codes) {
               message(glue::glue("Skipping neighbor matching for excluded country: {country_iso_code_no_data}"))
               next
          }

          message(glue::glue("Processing country with no cholera data: {country_iso_code_no_data}"))
          country_shp_no_data <- sf::st_read(dsn = file.path(PATHS$DATA_SHAPEFILES,
                                                             paste0(country_iso_code_no_data, "_ADM0.shp")))
          country_shp_no_data <- sf::st_transform(country_shp_no_data, sf::st_crs(africa))
          centroid_no_data <- sf::st_centroid(sf::st_union(country_shp_no_data))
          coord_no_data <- sf::st_coordinates(centroid_no_data)

          country_cluster <- precip_fitted_df[precip_fitted_df$iso_code == country_iso_code_no_data, "cluster"]
          best_neighbor_iso_code <- NULL
          highest_corr <- -Inf

          neighbors_within_cluster <- africa_with_clusters[
               africa_with_clusters$cluster == country_cluster &
                    africa_with_clusters$iso_a3 != country_iso_code_no_data &
                    africa_with_clusters$iso_a3 %in% iso_codes_with_usable_data &
                    !(africa_with_clusters$iso_a3 %in% exclude_iso_codes), ]

          if (nrow(neighbors_within_cluster) > 0) {

               message(glue::glue("Found {nrow(neighbors_within_cluster)} neighbors within the same cluster for {country_iso_code_no_data}"))
               precip_no_data <- combined_fitted_values_daily[combined_fitted_values_daily$iso_code == country_iso_code_no_data, ]
               precip_no_data <- precip_no_data[order(precip_no_data$day), ]
               precip_no_data_vec <- precip_no_data$fitted_values_fourier_precip

               for (neighbor_iso_code in neighbors_within_cluster$iso_a3) {

                    precip_neighbor <- combined_fitted_values_daily[combined_fitted_values_daily$iso_code == neighbor_iso_code, ]
                    precip_neighbor <- precip_neighbor[order(precip_neighbor$day), ]
                    precip_neighbor_vec <- precip_neighbor$fitted_values_fourier_precip

                    if (all(is.na(precip_no_data_vec)) || all(is.na(precip_neighbor_vec))) next

                    corr <- cor(precip_no_data_vec, precip_neighbor_vec, use = "complete.obs")

                    message(glue::glue("Correlation between {country_iso_code_no_data} and {neighbor_iso_code}: {corr}"))

                    if (!is.na(corr) && corr > highest_corr) {
                         highest_corr <- corr
                         best_neighbor_iso_code <- neighbor_iso_code
                    }
               }
          }

          if (is.null(best_neighbor_iso_code)) {

               message(glue::glue("No neighbors within the same cluster for {country_iso_code_no_data}. Checking nearest neighbors..."))
               k_nn <- initial_k

               repeat {

                    k_neighbors <- FNN::get.knnx(coords, coord_no_data, k = k_nn)
                    nearest_neighbors <- africa[k_neighbors$nn.index, ]
                    # Exclude any neighbors in the exclusion list
                    neighbors_with_data <- nearest_neighbors[nearest_neighbors$iso_a3 %in% iso_codes_with_usable_data &
                                                                  !(nearest_neighbors$iso_a3 %in% exclude_iso_codes), ]
                    if (nrow(neighbors_with_data) == 0) {

                         message(glue::glue("No neighbors with data found with k = {k_nn}. Increasing k."))
                         k_nn <- k_nn + 1

                         if (k_nn > nrow(africa)) {

                              message(glue::glue("No valid neighbors found for {country_iso_code_no_data} even after increasing k. Skipping."))
                              break

                         }

                         next

                    }

                    precip_no_data <- combined_fitted_values_daily[combined_fitted_values_daily$iso_code == country_iso_code_no_data, ]
                    precip_no_data <- precip_no_data[order(precip_no_data$day), ]
                    precip_no_data_vec <- precip_no_data$fitted_values_fourier_precip

                    for (neighbor_iso_code in neighbors_with_data$iso_a3) {

                         precip_neighbor <- combined_fitted_values_daily[combined_fitted_values_daily$iso_code == neighbor_iso_code, ]
                         precip_neighbor <- precip_neighbor[order(precip_neighbor$day), ]
                         precip_neighbor_vec <- precip_neighbor$fitted_values_fourier_precip

                         if (all(is.na(precip_no_data_vec)) || all(is.na(precip_neighbor_vec))) next

                         corr <- cor(precip_no_data_vec, precip_neighbor_vec, use = "complete.obs")
                         message(glue::glue("Correlation between {country_iso_code_no_data} and {neighbor_iso_code}: {corr}"))

                         if (!is.na(corr) && corr > highest_corr) {
                              highest_corr <- corr
                              best_neighbor_iso_code <- neighbor_iso_code
                         }
                    }

                    if (!is.null(best_neighbor_iso_code)) break
                    k_nn <- k_nn + 1

               }
          }

          if (!is.null(best_neighbor_iso_code)) {

               message(glue::glue("Best neighbor based on correlation: {best_neighbor_iso_code} (correlation: {highest_corr})"))
               combined_fitted_values_daily[combined_fitted_values_daily$iso_code == country_iso_code_no_data, "fitted_values_fourier_cases"] <-
                    combined_fitted_values_daily[combined_fitted_values_daily$iso_code == best_neighbor_iso_code, "fitted_values_fourier_cases"]
               combined_fitted_values_daily[combined_fitted_values_daily$iso_code == country_iso_code_no_data, "inferred_from_neighbor"] <-
                    MOSAIC::convert_iso_to_country(best_neighbor_iso_code)

               combined_param_values[combined_param_values$country_iso_code == country_iso_code_no_data &
                                          combined_param_values$response == "cases",
                                     c("parameter", "mean", "se", "ci_lo", "ci_hi", "envelope_scale")] <-
                    combined_param_values[combined_param_values$country_iso_code == best_neighbor_iso_code &
                                               combined_param_values$response == "cases",
                                          c("parameter", "mean", "se", "ci_lo", "ci_hi", "envelope_scale")]

               combined_param_values[combined_param_values$country_iso_code == country_iso_code_no_data &
                                          combined_param_values$response == "cases", "inferred_from_neighbor"] <-
                    MOSAIC::convert_iso_to_country(best_neighbor_iso_code)

               message(glue::glue("Assigned data from {best_neighbor_iso_code} to {country_iso_code_no_data}"))

          } else {

               message(glue::glue("No valid neighbor with positive correlation found for {country_iso_code_no_data}."))
          }
     }

     row.names(combined_fitted_values_daily) <- NULL
     row.names(combined_precip_data) <- NULL
     row.names(combined_param_values) <- NULL

     # Save the outputs to CSV files.
     utils::write.csv(combined_fitted_values_daily, file.path(PATHS$MODEL_INPUT, "pred_seasonal_dynamics_day.csv"), row.names = FALSE)
     path1 <- file.path(PATHS$MODEL_INPUT, "data_seasonal_precipitation.csv")
     path2 <- file.path(PATHS$DOCS_TABLES, "data_seasonal_precipitation.csv")
     utils::write.csv(combined_precip_data, file = path1, row.names = FALSE)
     utils::write.csv(combined_precip_data, file = path2, row.names = FALSE)
     message("Daily precipitation data saved to:")
     message(path1)
     message(path2)

     path1 <- file.path(PATHS$MODEL_INPUT, "param_seasonal_dynamics.csv")
     path2 <- file.path(PATHS$DOCS_TABLES, "param_seasonal_dynamics.csv")
     utils::write.csv(combined_param_values, file = path1, row.names = FALSE)
     utils::write.csv(combined_param_values, file = path2, row.names = FALSE)
     message("Estimated Fourier series parameters saved to: ")
     message(path1)
     message(path2)

}


# Minimum over the year (t = 1..365, p = 365) of the zero-mean two-harmonic
# Fourier term f(t) = a1 cos + b1 sin + a2 cos2 + b2 sin2 for named coefficients
# a_1, b_1, a_2, b_2.
.seasonal_envelope_min <- function(coef) {
     t <- seq_len(365L)
     f <- coef[["a_1"]] * cos(2 * pi * t / 365) + coef[["b_1"]] * sin(2 * pi * t / 365) +
          coef[["a_2"]] * cos(4 * pi * t / 365) + coef[["b_2"]] * sin(4 * pi * t / 365)
     min(f)
}

# Factor in (0, 1] by which the Fourier coefficients must be multiplied so that
# min_t(1 + f(t)) >= floor. f is linear in the coefficients, so scaling them by s
# scales min f by s; returns 1 when the envelope already clears the floor.
.seasonal_envelope_scale <- function(coef, floor = 0.1) {
     m <- .seasonal_envelope_min(coef)
     if (!is.finite(m) || 1 + m >= floor) return(1)
     (1 - floor) / (-m)
}
