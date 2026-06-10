#' Download, Process, and Save Historical Annual WHO Cholera Data for Africa
#'
#' Compiles cholera surveillance data from WHO for African countries (AFRO
#' region) from 1949 onward. Data sources and the precise per-year coverage
#' are tracked through the pipeline so partial-year snapshots (current year
#' from the WHO Global Cholera & AWD dashboard) are usable downstream
#' without being mislabeled as full-year totals.
#'
#' @param PATHS A list containing the paths to raw and processed data
#'   directories. Typically generated from `get_paths()`.
#'
#' @return The function does not return a value but writes processed CSVs
#'   to `PATHS$DATA_WHO_ANNUAL`.
#'
#' @details
#' Data sources:
#' \itemize{
#'   \item \strong{1949-2021}: WHO annual reports compiled by Our World in
#'         Data. See \url{https://ourworldindata.org}.
#'   \item \strong{2022}: Manually transcribed from the WHO 2022 annual
#'         cholera report (PDF).
#'   \item \strong{2023+}: Downloaded from the
#'         \strong{WHO Global Cholera & AWD dashboard} via ArcGIS. Each
#'         download is archived to
#'         \code{raw/WHO/annual/who_global_dashboard/cholera_adm0_public_snapshot_YYYY-MM-DD.csv}
#'         and ALL files matching \code{cholera_adm0_public_*.csv} in the
#'         dashboard directory are ingested. Each row's actual coverage is
#'         derived from \code{first_epiwk} / \code{last_epiwk} (rolling
#'         snapshots cross calendar-year boundaries -- see the year-labeling
#'         note below).
#' }
#'
#' \strong{Year labeling}: The ArcGIS dashboard returns one row per country
#' summing \code{case_total} / \code{death_total} between \code{first_epiwk}
#' and \code{last_epiwk}. Each row is assigned to the calendar year that
#' contains the majority of its days. \code{coverage_days} reports how many
#' calendar days are covered, and \code{year_fraction = coverage_days/365.25}
#' indicates whether the row is a full-year observation (~1.0) or a partial
#' snapshot (<1.0). When multiple files have a row for the same
#' \code{(iso_code, year)} combination, the row with the larger
#' \code{coverage_days} is kept.
#'
#' \strong{To refresh through a given calendar year}: place a year-filtered
#' CSV in \code{raw/WHO/annual/who_global_dashboard/} named
#' \code{cholera_adm0_public_YYYY.csv} (downloaded from the WHO dashboard UI
#' with the year filter applied). Re-run \code{process_WHO_annual_data()}.
#'
#' Outputs (in \code{PATHS$DATA_WHO_ANNUAL}):
#' \itemize{
#'   \item \code{who_afro_annual.csv} -- current full series, columns
#'         \code{country, iso_code, region, year, cases_total,
#'         cases_imported, deaths_total, cfr, cfr_lo, cfr_hi, first_epiwk,
#'         last_epiwk, coverage_days, year_fraction, source}.
#'   \item Intermediate per-source slices for traceability.
#' }
#'
#' @importFrom utils read.csv write.csv download.file
#' @importFrom stats binom.test
#'
#' @examples
#' \dontrun{
#' PATHS <- get_paths()
#' process_WHO_annual_data(PATHS)
#' }
#'
#' @export
process_WHO_annual_data <- function(PATHS) {

     if (!dir.exists(PATHS$DATA_WHO_ANNUAL)) dir.create(PATHS$DATA_WHO_ANNUAL, recursive = TRUE)

     ################################################################################
     # 1. Annual data 1949-2021 (pre-compiled by Our World in Data)
     ################################################################################

     message("Processing WHO cholera data from 1949 to 2021 (OWiD)...")

     cholera_cases <- utils::read.csv(
          file.path(PATHS$DATA_RAW, "WHO/annual/who_global_1949_2021/number-reported-cases-of-cholera.csv"),
          stringsAsFactors = FALSE
     )
     cholera_deaths <- utils::read.csv(
          file.path(PATHS$DATA_RAW, "WHO/annual/who_global_1949_2021/number-of-reported-cholera-deaths.csv"),
          stringsAsFactors = FALSE
     )

     cholera_cases_afr <- cholera_cases[cholera_cases$Code %in% MOSAIC::iso_codes_who_afro, ]
     colnames(cholera_cases_afr)[colnames(cholera_cases_afr) == "Entity"] <- "country"
     colnames(cholera_cases_afr)[colnames(cholera_cases_afr) == "Code"] <- "iso_code"
     colnames(cholera_cases_afr)[colnames(cholera_cases_afr) == "Year"] <- "year"
     colnames(cholera_cases_afr)[colnames(cholera_cases_afr) == "Reported.cholera.cases"] <- "cases_total"
     cholera_cases_afr$region <- "AFRO"
     cholera_cases_afr$cases_imported <- NA

     cholera_deaths_afr <- cholera_deaths[cholera_deaths$Code %in% MOSAIC::iso_codes_who_afro, ]
     colnames(cholera_deaths_afr)[colnames(cholera_deaths_afr) == "Entity"] <- "country"
     colnames(cholera_deaths_afr)[colnames(cholera_deaths_afr) == "Code"] <- "iso_code"
     colnames(cholera_deaths_afr)[colnames(cholera_deaths_afr) == "Year"] <- "year"
     colnames(cholera_deaths_afr)[colnames(cholera_deaths_afr) == "Reported.cholera.deaths"] <- "deaths_total"

     cholera_data_1949_2021 <- merge(
          cholera_cases_afr,
          cholera_deaths_afr[, c("country", "year", "iso_code", "deaths_total")],
          by = c("country", "iso_code", "year"),
          all.x = TRUE
     )

     cholera_data_1949_2021$first_epiwk    <- as.Date(paste0(cholera_data_1949_2021$year, "-01-01"))
     cholera_data_1949_2021$last_epiwk     <- as.Date(paste0(cholera_data_1949_2021$year, "-12-31"))
     cholera_data_1949_2021$coverage_days  <- 365L
     cholera_data_1949_2021$year_fraction  <- 1.0
     cholera_data_1949_2021$source         <- "OWiD"

     ################################################################################
     # 2. 2022 -- manually transcribed from the WHO annual cholera PDF
     ################################################################################

     message("Processing WHO cholera data for 2022 (manual extraction)...")

     cholera_data_2022 <- data.frame(
          region = rep("AFRO", 17),
          country = c("Benin", "Burkina Faso", "Burundi", "Cameroon",
                      "Democratic Republic of Congo", "Ethiopia", "Kenya",
                      "Liberia", "Malawi", "Mozambique", "Nigeria", "Rwanda",
                      "Somalia", "South Africa", "South Sudan", "Zambia",
                      "Zimbabwe"),
          iso_code = c("BEN", "BFA", "BDI", "CMR", "COD", "ETH", "KEN",
                       "LBR", "MWI", "MOZ", "NGA", "RWA", "SOM", "ZAF",
                       "SSD", "ZMB", "ZWE"),
          cases_total = c(433, 4, 25, 14431, 18961, 846, 3525, 367, 17488,
                          4378, 23839, 24, 15653, 1, 424, 34, 4),
          cases_imported = c(0, 0, 0, 15, 0, 0, 0, 0, 186, NA, 0, 0, 0, 0, 0, 0, 1),
          deaths_total = c(2, 0, 0, 279, 298, 27, 64, 0, 576, 22, 597, 0,
                           88, 0, 1, 0, 1),
          year = 2022,
          first_epiwk = as.Date("2022-01-01"),
          last_epiwk  = as.Date("2022-12-31"),
          coverage_days = 365L,
          year_fraction = 1.0,
          source = "WHO_PDF_2022",
          stringsAsFactors = FALSE
     )

     ################################################################################
     # 3. 2023+ -- WHO Global Cholera & AWD dashboard (ArcGIS)
     ################################################################################

     message("Processing WHO cholera data from 2023 onward (dashboard)...")

     # Live dashboard URL (rolling -- content changes over time as the dashboard
     # updates). We archive each download by snapshot date so historical
     # snapshots are preserved.
     dashboard_url <- "https://who.maps.arcgis.com/sharing/rest/content/items/3aa7bfec5da047a7ba7d4f9bcebd0061/data"

     dashboard_dir <- file.path(PATHS$DATA_RAW, "WHO/annual/who_global_dashboard")
     if (!dir.exists(dashboard_dir)) dir.create(dashboard_dir, recursive = TRUE)

     # Back-compat: if older files exist under the legacy directory, surface
     # them so this function keeps ingesting them. Do not overwrite.
     legacy_dir <- file.path(PATHS$DATA_RAW, "WHO/annual/who_global_2023_2024")
     legacy_files <- if (dir.exists(legacy_dir)) {
          list.files(legacy_dir, pattern = "^cholera_adm0_public_.*\\.csv$", full.names = TRUE)
     } else character(0)

     snapshot_path <- file.path(
          dashboard_dir,
          sprintf("cholera_adm0_public_snapshot_%s.csv", format(Sys.Date(), "%Y-%m-%d"))
     )
     tryCatch({
          message("Attempting to fetch latest WHO dashboard snapshot...")
          utils::download.file(dashboard_url, snapshot_path, mode = "wb", quiet = TRUE)
          message(sprintf("  Saved snapshot: %s", basename(snapshot_path)))
     }, error = function(e) {
          message(sprintf("  Download failed: %s", conditionMessage(e)))
          message("  Falling back to previously downloaded snapshots in raw dir.")
     })

     dashboard_files <- list.files(dashboard_dir, pattern = "^cholera_adm0_public_.*\\.csv$", full.names = TRUE)
     all_dashboard_files <- unique(c(dashboard_files, legacy_files))
     if (!length(all_dashboard_files)) {
          stop("No dashboard files found in either ", dashboard_dir, " or ", legacy_dir)
     }

     # Ingest every file. Each row's actual year coverage is derived from
     # first_epiwk / last_epiwk; rows are deduped by (iso_code, year) keeping
     # the row with the largest coverage_days (= the most complete observation).
     ingest_one <- function(path) {
          df <- utils::read.csv(path, stringsAsFactors = FALSE)
          required_cols <- c("adm0_name", "who_region", "iso_3_code",
                             "first_epiwk", "last_epiwk", "case_total", "death_total")
          missing_cols <- setdiff(required_cols, colnames(df))
          if (length(missing_cols)) {
               warning(sprintf("Skipping %s \u2014 missing columns: %s",
                               basename(path), paste(missing_cols, collapse = ", ")))
               return(NULL)
          }
          # Filter to AFRO; the dashboard uses two naming conventions across snapshots
          df <- df[df$who_region %in% c("AFRO", "African Region"), , drop = FALSE]
          if (!nrow(df)) return(NULL)
          df$who_region <- "AFRO"

          df$first_epiwk <- as.Date(df$first_epiwk)
          df$last_epiwk  <- as.Date(df$last_epiwk)

          # Determine year + coverage from the actual date range
          coverage <- .who_annual_year_coverage(df$first_epiwk, df$last_epiwk)
          df$year          <- coverage$year
          df$coverage_days <- coverage$coverage_days
          df$year_fraction <- coverage$year_fraction
          df$source        <- sprintf("dashboard:%s", basename(path))
          df
     }

     dashboard_rows <- do.call(rbind, lapply(all_dashboard_files, ingest_one))
     dashboard_rows <- dashboard_rows[!is.na(dashboard_rows$year), , drop = FALSE]

     # Dedupe by (iso, year): keep row with max coverage_days
     dashboard_rows <- dashboard_rows[order(
          dashboard_rows$iso_3_code, dashboard_rows$year, -dashboard_rows$coverage_days
     ), ]
     dashboard_rows <- dashboard_rows[
          !duplicated(dashboard_rows[, c("iso_3_code", "year")]), ,
          drop = FALSE
     ]

     # Standardize column names + values
     colnames(dashboard_rows)[colnames(dashboard_rows) == "case_total"]   <- "cases_total"
     colnames(dashboard_rows)[colnames(dashboard_rows) == "death_total"]  <- "deaths_total"
     colnames(dashboard_rows)[colnames(dashboard_rows) == "who_region"]   <- "region"
     colnames(dashboard_rows)[colnames(dashboard_rows) == "iso_3_code"]   <- "iso_code"
     colnames(dashboard_rows)[colnames(dashboard_rows) == "adm0_name"]    <- "country"

     dashboard_rows$cases_imported <- NA

     # Country name harmonization with the 1949-2022 series
     country_names <- sort(unique(c(cholera_data_1949_2021$country, cholera_data_2022$country)))
     capitalize_words <- function(name) {
          s <- tolower(name)
          s <- strsplit(s, " ")[[1]]
          paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
     }
     dashboard_rows$country <- vapply(dashboard_rows$country, capitalize_words, character(1))
     dashboard_rows$country[dashboard_rows$country == "Democratic Republic Of Congo"]      <- "Democratic Republic of Congo"
     dashboard_rows$country[dashboard_rows$country == "Democratic Republic Of The Congo"]  <- "Democratic Republic of Congo"
     dashboard_rows$country[dashboard_rows$country == "United Republic Of Tanzania"]       <- "Tanzania"
     dashboard_rows$country[dashboard_rows$country == "Cote D'ivoire"]                     <- "Cote d'Ivoire"
     dashboard_rows$country[dashboard_rows$country == "Eswatini"]                          <- "Eswatini"

     mismatches <- setdiff(unique(dashboard_rows$country), country_names)
     if (length(mismatches)) {
          message("  Country names not in 1949-2022 series (kept as-is):")
          for (m in mismatches) message("    ", m)
     }

     ################################################################################
     # 4. Combine all sources + summarize coverage
     ################################################################################

     # Standardize columns across all blocks
     keep_cols <- c("country", "iso_code", "region", "year",
                    "cases_total", "cases_imported", "deaths_total",
                    "first_epiwk", "last_epiwk", "coverage_days",
                    "year_fraction", "source")

     cholera_data_1949_2021$source <- "OWiD"
     combined <- rbind(
          cholera_data_1949_2021[, intersect(keep_cols, colnames(cholera_data_1949_2021))],
          cholera_data_2022     [, intersect(keep_cols, colnames(cholera_data_2022))],
          dashboard_rows        [, intersect(keep_cols, colnames(dashboard_rows))]
     )
     combined$country[combined$country == "Democratic Republic of the Congo"] <- "Democratic Republic of Congo"

     # CFR + 95% binomial CI per (country, year) row
     combined$cfr    <- combined$deaths_total / combined$cases_total
     combined$cfr[is.nan(combined$cfr)] <- NA
     combined$cfr_lo <- NA_real_
     combined$cfr_hi <- NA_real_
     for (i in seq_len(nrow(combined))) {
          if (!is.na(combined$cases_total[i]) && !is.na(combined$deaths_total[i]) &&
              combined$cases_total[i] > 0) {
               ci <- stats::binom.test(combined$deaths_total[i], combined$cases_total[i])$conf.int
               combined$cfr_lo[i] <- ci[1]
               combined$cfr_hi[i] <- ci[2]
          }
     }

     # Data-quality validation: deaths must not exceed cases
     bad <- which(!is.na(combined$deaths_total) & !is.na(combined$cases_total) &
                  combined$deaths_total > combined$cases_total)
     if (length(bad)) {
          warning(sprintf("%d row(s) with deaths > cases (will be NA-ed):", length(bad)))
          combined$deaths_total[bad] <- NA
          combined$cfr[bad] <- combined$cfr_lo[bad] <- combined$cfr_hi[bad] <- NA
     }

     # AFRO regional totals (full-year only)
     full_year <- combined[!is.na(combined$year_fraction) & combined$year_fraction >= 0.95, ]
     afro_totals <- aggregate(
          cbind(cases_total, deaths_total) ~ year,
          data = full_year, sum, na.rm = TRUE
     )
     afro_totals$region         <- "AFRO"
     afro_totals$country        <- "AFRO Region"
     afro_totals$iso_code       <- "AFRO"
     afro_totals$cases_imported <- NA
     afro_totals$first_epiwk    <- as.Date(paste0(afro_totals$year, "-01-01"))
     afro_totals$last_epiwk     <- as.Date(paste0(afro_totals$year, "-12-31"))
     afro_totals$coverage_days  <- 365L
     afro_totals$year_fraction  <- 1.0
     afro_totals$source         <- "AFRO_aggregate"
     afro_totals$cfr            <- afro_totals$deaths_total / afro_totals$cases_total
     afro_totals$cfr_lo         <- NA_real_
     afro_totals$cfr_hi         <- NA_real_
     for (i in seq_len(nrow(afro_totals))) {
          if (afro_totals$cases_total[i] > 0) {
               ci <- stats::binom.test(afro_totals$deaths_total[i], afro_totals$cases_total[i])$conf.int
               afro_totals$cfr_lo[i] <- ci[1]
               afro_totals$cfr_hi[i] <- ci[2]
          }
     }

     out <- rbind(combined, afro_totals[, colnames(combined)])

     # Sort for human readability
     out <- out[order(out$country, out$year), ]

     # Write outputs
     out_path <- file.path(PATHS$DATA_WHO_ANNUAL, "who_afro_annual.csv")
     utils::write.csv(out, file = out_path, row.names = FALSE)
     message(sprintf("Wrote %s (%d rows, year range %d-%d)",
                     basename(out_path), nrow(out),
                     min(out$year, na.rm = TRUE), max(out$year, na.rm = TRUE)))

     # Coverage summary to stdout (one row per year showing how many countries
     # contributed full vs partial data)
     by_year <- combined[combined$iso_code %in% MOSAIC::iso_codes_who_afro, ]
     cov_summary <- aggregate(
          coverage_days ~ year,
          data = by_year,
          FUN = function(x) c(
               n_countries     = length(x),
               n_full_year     = sum(x >= 350),
               n_partial       = sum(x < 350),
               median_coverage = median(x)
          )
     )
     message("Year coverage summary (last 5 years):")
     tail_yrs <- tail(unique(by_year$year[order(by_year$year)]), 5)
     for (y in tail_yrs) {
          rows <- by_year[by_year$year == y, ]
          message(sprintf("  %d: %d countries, %d full-year (>=350d), %d partial (<350d), median coverage %d days",
                          y, nrow(rows), sum(rows$coverage_days >= 350),
                          sum(rows$coverage_days < 350), median(rows$coverage_days)))
     }

     invisible(out)
}


#' Determine calendar year + coverage from epi-week date ranges
#'
#' Given vectors of `first_epiwk` and `last_epiwk` dates, returns the
#' calendar year containing the majority of days in the range and the
#' total days covered. Used by `process_WHO_annual_data()` so partial-year
#' rolling snapshots from the WHO dashboard are labeled correctly.
#'
#' @param first A Date vector of range starts.
#' @param last  A Date vector of range ends.
#' @return A list with components `year`, `coverage_days`, `year_fraction`.
#' @noRd
.who_annual_year_coverage <- function(first, last) {
     n <- length(first)
     year_out <- integer(n)
     days_out <- integer(n)
     frac_out <- numeric(n)
     for (i in seq_len(n)) {
          if (is.na(first[i]) || is.na(last[i]) || last[i] < first[i]) {
               year_out[i] <- NA_integer_
               days_out[i] <- NA_integer_
               frac_out[i] <- NA_real_
               next
          }
          fy <- as.integer(format(first[i], "%Y"))
          ly <- as.integer(format(last[i],  "%Y"))
          if (fy == ly) {
               year_out[i] <- fy
               days_out[i] <- as.integer(last[i] - first[i]) + 1L
          } else {
               # Cross-year: assign to the year containing more days
               end_of_fy <- as.Date(sprintf("%d-12-31", fy))
               start_of_ly <- as.Date(sprintf("%d-01-01", ly))
               days_first <- as.integer(end_of_fy - first[i]) + 1L
               days_last  <- as.integer(last[i] - start_of_ly) + 1L
               year_out[i] <- if (days_last >= days_first) ly else fy
               days_out[i] <- days_first + days_last
          }
          frac_out[i] <- days_out[i] / 365.25
     }
     list(year = year_out, coverage_days = days_out, year_fraction = frac_out)
}
