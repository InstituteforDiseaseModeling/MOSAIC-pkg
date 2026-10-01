# Internal helpers for the seasonal-dynamics documentation figures
# (plot_seasonal_transmission(), plot_seasonal_transmission_example() and
# plot_seasonal_clustering()). They only read est_seasonal_dynamics() outputs
# and build display text; nothing here feeds a prior, a config or a simulation.

# Calendar-year span of `dates` as text: "2010-2025", or "2023" when every date
# falls in one year. NA_character_ when there is no non-missing date.
.seasonal_year_span <- function(dates) {
     if (is.null(dates) || !length(dates)) return(NA_character_)
     dates <- as.Date(dates)
     dates <- dates[!is.na(dates)]
     if (!length(dates)) return(NA_character_)
     years <- as.integer(format(range(dates), "%Y"))
     if (years[1] == years[2]) as.character(years[1]) else paste0(years[1], "-", years[2])
}

# "<prefix> (<years>)" for the dates of the values a figure draws, or the bare
# prefix when there are none (for example a country whose case fit was
# inferred from a neighbour and has no case points of its own).
.seasonal_label <- function(prefix, dates) {
     span <- .seasonal_year_span(dates)
     if (is.na(span)) prefix else paste0(prefix, " (", span, ")")
}

# Legend labels for the precipitation and case points of the seasonal
# transmission figures, from the rows of data_seasonal_precipitation.csv
# (written by est_seasonal_dynamics() for its fit window) that carry a scaled
# value, i.e. the points that are drawn.
.seasonal_point_labels <- function(precip_data) {
     dates <- as.Date(precip_data$date)
     c(precip = .seasonal_label("Precipitation", dates[!is.na(precip_data$precip_scaled)]),
       cases  = .seasonal_label("Cholera Cases", dates[!is.na(precip_data$cases_scaled)]))
}

# Seasonal fits for plot_seasonal_clustering().
#
# Prefers the daily fits est_seasonal_dynamics() writes
# (MODEL_INPUT/pred_seasonal_dynamics_day.csv). Falls back to the weekly table
# that versions before the daily refactor wrote to
# DOCS_TABLES/pred_seasonal_dynamics.csv; nothing produces that file any more,
# so it is read only when the daily fits are absent.
#
# Returns a list:
#   fits      data frame the clustering is run on, one row per country and time
#             step in column `time_col`. For the daily source these are the
#             daily fitted values themselves, the matrix est_seasonal_dynamics()
#             clusters for its neighbour inference.
#   time_col  "day" (daily source) or "week" (legacy table).
#   weekly    data frame drawn in the per-cluster panel, one row per
#             country-week (week 1-52). From the daily fits, week w is the mean
#             of days 7(w - 1) + 1 to 7w, so day 365 is not drawn.
#   source    "daily" or "weekly_legacy"; path: the file read.
#   window    list(precip, cases) of the dates with a scaled value in the
#             data_seasonal_precipitation.csv written next to the daily fits
#             (the fit window), or NULL for the legacy table, whose window is
#             not recorded.
.seasonal_clustering_fits <- function(PATHS) {

     fit_cols <- c("fitted_values_fourier_precip", "fitted_values_fourier_cases")
     daily_path <- if (!is.null(PATHS$MODEL_INPUT)) {
          file.path(PATHS$MODEL_INPUT, "pred_seasonal_dynamics_day.csv")
     }
     legacy_path <- if (!is.null(PATHS$DOCS_TABLES)) {
          file.path(PATHS$DOCS_TABLES, "pred_seasonal_dynamics.csv")
     }

     if (length(daily_path) && file.exists(daily_path)) {

          daily <- utils::read.csv(daily_path, stringsAsFactors = FALSE)
          daily$day <- as.integer(daily$day)
          if (!"inferred_from_neighbor" %in% names(daily)) daily$inferred_from_neighbor <- NA_character_

          blocks <- daily[daily$day <= 364L, ]
          blocks$week <- (blocks$day - 1L) %/% 7L + 1L
          weekly <- stats::aggregate(blocks[fit_cols],
                                     by = list(iso_code = blocks$iso_code, week = blocks$week),
                                     FUN = mean)
          meta <- unique(daily[, c("iso_code", "Country", "inferred_from_neighbor")])
          weekly <- merge(weekly, meta, by = "iso_code", sort = FALSE)
          weekly <- weekly[order(weekly$iso_code, weekly$week),
                           c("week", "iso_code", fit_cols, "Country", "inferred_from_neighbor")]
          row.names(weekly) <- NULL

          window <- NULL
          window_path <- file.path(PATHS$MODEL_INPUT, "data_seasonal_precipitation.csv")
          if (file.exists(window_path)) {
               w <- utils::read.csv(window_path, stringsAsFactors = FALSE)
               window <- list(precip = as.Date(w$date[!is.na(w$precip_scaled)]),
                              cases = as.Date(w$date[!is.na(w$cases_scaled)]))
          }

          return(list(fits = daily, time_col = "day", weekly = weekly,
                      source = "daily", path = daily_path, window = window))
     }

     if (length(legacy_path) && file.exists(legacy_path)) {
          weekly <- utils::read.csv(legacy_path, stringsAsFactors = FALSE)
          return(list(fits = weekly, time_col = "week", weekly = weekly,
                      source = "weekly_legacy", path = legacy_path, window = NULL))
     }

     stop("plot_seasonal_clustering: no seasonal fits found. Looked for ",
          if (length(daily_path)) daily_path else "MODEL_INPUT/pred_seasonal_dynamics_day.csv (PATHS$MODEL_INPUT not set)",
          " and ",
          if (length(legacy_path)) legacy_path else "DOCS_TABLES/pred_seasonal_dynamics.csv (PATHS$DOCS_TABLES not set)",
          ". Run est_seasonal_dynamics() first.", call. = FALSE)
}
