#' Download World Bank indicators from the Indicators API
#'
#' Fetches each indicator from the open World Bank Indicators API (v2, no
#' credentials) and writes it into \code{MOSAIC-data/raw/world_bank/<subdir>/}
#' in the \strong{same wide "bulk CSV" layout the web portal produces} -- four
#' metadata lines, then \code{Country Name, Country Code, Indicator Name,
#' Indicator Code} followed by one column per year. The
#' \code{process_WB_*_data()} functions therefore read an API pull and a
#' hand-downloaded portal export identically.
#'
#' @param PATHS List of paths from \code{\link{get_paths}}. Must include
#'   \code{DATA_RAW}.
#' @param indicators Named character vector mapping World Bank indicator codes
#'   to the \code{raw/world_bank/} subdirectory each belongs in. Defaults to
#'   the four MOSAIC consumes; see \code{\link{MOSAIC_WB_INDICATORS}}.
#' @param snapshot_date Date stamp for the output filenames. Defaults to today.
#' @param overwrite If \code{FALSE} (default), an indicator whose file for
#'   \code{snapshot_date} already exists is skipped.
#' @param per_page API page size (default 20000; the API caps near 32767).
#' @param verbose Print progress and a summary.
#'
#' @return Invisibly, a \code{data.frame} with one row per indicator:
#'   \code{indicator}, \code{subdir}, \code{ok}, \code{n_countries},
#'   \code{year_min}, \code{year_max}, \code{file}, \code{note}.
#'
#' @details
#' \strong{No credentials.} \url{https://api.worldbank.org/v2/} is open. This
#' retires four of the six manual sources reported by
#' \code{\link{check_mosaic_manual_inputs}}.
#'
#' \strong{Newest-wins, not overwrite.} Files are stamped
#' \code{API_<INDICATOR>_DS2_en_csv_v2_api_<date>.csv} and the
#' \code{process_WB_*_data()} functions select the newest match for their
#' indicator. Existing hand-downloaded portal exports are left in place and
#' still readable -- an API pull simply outranks them by date. Nothing is
#' deleted.
#'
#' \strong{All countries are fetched}, not just the MOSAIC-40: the processors
#' do their own ISO filtering, and keeping the full panel means the raw file
#' stays reusable. Records the API returns with a blank \code{countryiso3code}
#' -- the income-group aggregates (\emph{High income}, \emph{Low income},
#' \emph{Lower/Upper middle income}, \emph{Not classified}) -- are dropped,
#' since they are not countries and nothing downstream uses them.
#'
#' \strong{Switching to the API is NOT a no-op.} The World Bank revises history,
#' so a live pull differs from an older portal vintage. Measured 2026-09-17
#' against the 2025-04-15 GDP export: all 40 MOSAIC countries present in both,
#' 2025 gained as a new year, and of 2,331 shared MOSAIC-40 country-years
#' \strong{83.7% were identical} while 194 (8.3%) differed by >1%. Those
#' differences are genuine national-accounts rebasing, not a conversion error:
#' 19 of 40 countries have NO year differing by more than 1% (3 are identical
#' to the byte), and the rest differ in contiguous
#' year blocks with smooth country-specific ratios (MLI 1980-2023 ratio
#' 1.13-1.54; AGO 2002-2023 ratio 1.12-1.25). Expect GDP-derived quantities to
#' move for ~21 countries on first use.
#'
#' @source World Bank Indicators API,
#'   \url{https://datahelpdesk.worldbank.org/knowledgebase/articles/889392}.
#'   Licence CC BY 4.0.
#'
#' @seealso \code{\link{process_WB_GDP_data}},
#'   \code{\link{process_WB_poverty_ratio_data}},
#'   \code{\link{process_WB_population_density_data}},
#'   \code{\link{process_WB_urban_population_data}}
#'
#' @importFrom utils write.csv
#' @export
#'
#' @examples
#' \dontrun{
#' PATHS <- get_paths()
#' download_WB_data(PATHS)
#' process_WB_GDP_data(PATHS)
#' }
download_WB_data <- function(PATHS,
                             indicators    = MOSAIC_WB_INDICATORS,
                             snapshot_date = Sys.Date(),
                             overwrite     = FALSE,
                             per_page      = 20000L,
                             verbose       = TRUE) {

     if (is.null(PATHS$DATA_RAW)) {
          stop("PATHS must include DATA_RAW (regenerate via get_paths()).", call. = FALSE)
     }
     if (is.null(names(indicators)) || any(!nzchar(names(indicators)))) {
          stop("`indicators` must be a NAMED vector: names = indicator codes, ",
               "values = raw/world_bank subdirectory.", call. = FALSE)
     }

     out <- lapply(seq_along(indicators), function(i) {
          code   <- names(indicators)[i]
          subdir <- unname(indicators[i])
          blank  <- data.frame(indicator = code, subdir = subdir, ok = FALSE,
                               n_countries = NA_integer_, year_min = NA_integer_,
                               year_max = NA_integer_, file = NA_character_,
                               note = NA_character_, stringsAsFactors = FALSE)

          dir_out <- file.path(PATHS$DATA_RAW, "world_bank", subdir)
          dest <- file.path(dir_out, sprintf("API_%s_DS2_en_csv_v2_api_%s.csv",
                                             code, format(snapshot_date)))
          if (file.exists(dest) && !overwrite) {
               if (verbose) message(glue::glue("  {code}: already have {basename(dest)}"))
               blank$ok <- TRUE; blank$file <- dest; blank$note <- "existing"
               return(blank)
          }

          if (verbose) message(glue::glue("  {code} -> {subdir}/ ..."))
          recs <- tryCatch(.wb_fetch_indicator(code, per_page = per_page),
                           error = function(e) { blank$note <<- conditionMessage(e); NULL })
          if (is.null(recs) || !nrow(recs)) {
               if (verbose) message(glue::glue("     FAILED: {blank$note %||% 'no records'}"))
               return(blank)
          }

          wide <- .wb_long_to_bulk_wide(recs)
          dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
          # Use the API's own `lastupdated`, not today: in a portal bulk CSV
          # that field means "when the World Bank last updated the data".
          # Writing the retrieval date inverts the field's meaning in a file
          # designed to be read as a portal export.
          .wb_write_bulk_csv(wide, dest,
                             attr(recs, "lastupdated") %||% format(snapshot_date))

          yrs <- suppressWarnings(as.integer(sub("^X", "", grep("^X?[0-9]{4}$", names(wide), value = TRUE))))
          res <- data.frame(indicator = code, subdir = subdir, ok = TRUE,
                            n_countries = nrow(wide),
                            year_min = min(yrs, na.rm = TRUE),
                            year_max = max(yrs, na.rm = TRUE),
                            file = dest, note = NA_character_, stringsAsFactors = FALSE)
          if (verbose) {
               message(glue::glue("     {nrow(wide)} countries, {res$year_min}-{res$year_max}",
                                  " -> {basename(dest)}"))
          }
          res
     })

     out <- do.call(rbind, out)
     if (verbose) {
          message(glue::glue("\nWorld Bank: {sum(out$ok)}/{nrow(out)} indicators written."))
          if (any(!out$ok)) message("  failed: ", paste(out$indicator[!out$ok], collapse = ", "))
     }
     invisible(out)
}


#' World Bank indicators MOSAIC consumes
#'
#' Named character vector: names are World Bank indicator codes, values are the
#' \code{MOSAIC-data/raw/world_bank/} subdirectory each is stored in. The
#' indicator code is embedded in every raw filename, which is how
#' \code{process_WB_*_data()} locates its input.
#'
#' @export
MOSAIC_WB_INDICATORS <- c(
     "NY.GDP.MKTP.CD"    = "GDP",
     "SI.POV.DDAY"       = "poverty_ratio",
     "EN.POP.DNST"       = "population_density",
     "SP.URB.TOTL.IN.ZS" = "urban_population"
)


#' Fetch one indicator (all countries, all years) from the World Bank API
#'
#' @keywords internal
#' @noRd
.wb_fetch_indicator <- function(code, per_page = 20000L) {
     page  <- 1L
     acc   <- list()
     pages <- NA_integer_

     repeat {
          url <- sprintf(
               "https://api.worldbank.org/v2/country/all/indicator/%s?format=json&per_page=%d&page=%d",
               utils::URLencode(code, reserved = TRUE), as.integer(per_page), page)
          txt <- suppressWarnings(tryCatch(
               paste(readLines(url, warn = FALSE), collapse = ""),
               error = function(e) NULL))
          if (is.null(txt)) stop("World Bank API unreachable for ", code, call. = FALSE)

          js <- jsonlite::fromJSON(txt, simplifyVector = TRUE)

          # The API reports errors as a single-element list with $message.
          if (!is.null(js$message)) {
               stop("World Bank API error for ", code, ": ",
                    paste(unlist(js$message), collapse = "; "), call. = FALSE)
          }
          if (length(js) < 2L || is.null(js[[2]]) || !NROW(js[[2]])) {
               if (page == 1L) stop("World Bank API returned no data for ", code, call. = FALSE)
               break
          }

          meta <- js[[1]]
          if (is.na(pages)) pages <- as.integer(meta$pages %||% 1L)
          acc[[length(acc) + 1L]] <- js[[2]]

          if (page >= pages) break
          page <- page + 1L
     }

     d <- do.call(rbind, lapply(acc, function(x) {
          data.frame(
               country_name   = as.character(x$country$value),
               iso3           = as.character(x$countryiso3code),
               indicator_name = as.character(x$indicator$value),
               indicator_code = as.character(x$indicator$id),
               year           = suppressWarnings(as.integer(x$date)),
               value          = suppressWarnings(as.numeric(x$value)),
               stringsAsFactors = FALSE)
     }))
     d <- d[!is.na(d$year) & nzchar(d$iso3), , drop = FALSE]
     attr(d, "lastupdated") <- as.character(meta$lastupdated %||% NA)
     d
}


#' Pivot long API records into the portal's wide bulk layout
#'
#' @keywords internal
#' @noRd
.wb_long_to_bulk_wide <- function(d) {
     years <- sort(unique(d$year))
     key   <- unique(d[, c("country_name", "iso3", "indicator_name", "indicator_code")])
     key   <- key[order(key$country_name), , drop = FALSE]

     mat <- matrix(NA_real_, nrow = nrow(key), ncol = length(years),
                   dimnames = list(key$iso3, as.character(years)))
     # Fill via cbind(row, col). Do NOT "simplify" this to a linear index built
     # from paste(iso, year): that enumerates row-major while `mat` is
     # column-major, so the element COUNT still matches, R raises no error, and
     # the country x year assignment is silently transposed.
     ri <- match(d$iso3, key$iso3)
     ci <- match(d$year, years)
     ok <- !is.na(ri) & !is.na(ci)
     mat[cbind(ri[ok], ci[ok])] <- d$value[ok]

     wide <- data.frame(
          "Country Name"   = key$country_name,
          "Country Code"   = key$iso3,
          "Indicator Name" = key$indicator_name,
          "Indicator Code" = key$indicator_code,
          check.names = FALSE, stringsAsFactors = FALSE)
     for (j in seq_along(years)) wide[[as.character(years[j])]] <- mat[, j]
     wide
}


#' Write a data frame in the World Bank bulk-CSV layout
#'
#' Four metadata lines then the table, matching the portal export byte-for-byte
#' in structure so `read.csv(skip = 4)` in the processors keeps working.
#'
#' @keywords internal
#' @noRd
.wb_write_bulk_csv <- function(wide, dest, snapshot_date) {
     con <- file(dest, "w", encoding = "UTF-8")
     on.exit(close(con), add = TRUE)
     writeLines(c('"Data Source","World Development Indicators",',
                  '',
                  sprintf('"Last Updated Date","%s",', as.character(snapshot_date)),
                  ''), con)
     utils::write.csv(wide, con, row.names = FALSE, na = "")
     invisible(dest)
}


#' Locate the newest raw World Bank CSV for an indicator
#'
#' Both layouts live side by side in \code{raw/world_bank/<subdir>/}: portal
#' exports (\code{API_<CODE>_DS2_en_csv_v2_<vintage>.csv}) and API pulls
#' (\code{..._api_<date>.csv}). Ranking is by file mtime, because the portal's
#' trailing number is an opaque vintage id, not a date, and cannot be ordered.
#'
#' @param PATHS Paths list providing \code{DATA_RAW}.
#' @param subdir Subdirectory under \code{raw/world_bank/}.
#' @param indicator World Bank indicator code, as embedded in the filename.
#' @return Absolute path to the newest matching CSV.
#' @keywords internal
#' @noRd
.wb_newest_raw <- function(PATHS, subdir, indicator) {
     dir_in <- file.path(PATHS$DATA_RAW, "world_bank", subdir)
     hits <- list.files(dir_in,
                        pattern = paste0("^API_", gsub("\\.", "\\\\.", indicator), "_.*\\.csv$"),
                        full.names = TRUE, ignore.case = TRUE)
     if (!length(hits)) {
          stop("No raw World Bank CSV for ", indicator, " in ", dir_in,
               "\n  Fetch it with download_WB_data(PATHS) (open API, no credentials),",
               "\n  or download the portal export into that directory.", call. = FALSE)
     }
     .rank_raw_candidates(hits)[1L]
}


#' Rank candidate raw files by embedded ISO date, then mtime
#'
#' mtime alone is unsafe: a plain \code{cp}, \code{rsync -a}, \code{tar -x} or
#' a restore stamps the destination to now, and on an exact tie \code{order()}
#' is stable so \code{list.files()}'s alphabetical order decides -- which
#' deterministically favours the LEGACY filename ("1..." sorts before "api...").
#' Files carrying a \code{YYYY-MM-DD} in the name are therefore ranked on that
#' date first; undated files fall back to mtime and always lose a tie against a
#' dated file.
#'
#' @keywords internal
#' @noRd
.rank_raw_candidates <- function(hits) {
     if (length(hits) <= 1L) return(hits)
     d <- rep(NA_character_, length(hits))
     m <- regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", basename(hits))
     ok <- m > 0L
     d[ok] <- regmatches(basename(hits), m)[seq_len(sum(ok))]
     mt <- file.info(hits)$mtime
     key <- ifelse(is.na(d), format(mt, "%Y-%m-%d"), d)
     key[is.na(key)] <- "0000-00-00"
     # Sort on `ok` FIRST: an embedded date and an mtime-derived date are not
     # the same kind of key. An undated file's mtime is effectively "now" and
     # would beat any historical embedded date, so a dated file must outrank
     # an undated one unconditionally; dates are only compared within a class.
     hits[order(ok, key, decreasing = TRUE)]
}

