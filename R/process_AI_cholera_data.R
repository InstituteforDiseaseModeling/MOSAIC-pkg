#' Process AI-mined Weekly Cholera Surveillance Data into the MOSAIC Per-Source Schema
#'
#' Reads the per-country \code{cholera_weekly_<ISO>.csv} files produced by the
#' \code{ai-cholera-data-mining} repository, applies the "keep-and-weight"
#' ingestion filter, reformats the rows to match the WHO/JHU per-source schema
#' used by \code{\link{process_cholera_surveillance_data}}, and carries the
#' per-observation \code{confidence_weight} and \code{disaggregation_method}
#' through as two extra columns. The combined output is written as a single
#' processed CSV that the surveillance combiner reads as a fourth source.
#'
#' @details
#' \strong{Keep-and-weight filter.} The AI repo tags each row by
#' \code{disaggregation_method}. We keep:
#' \itemize{
#'   \item \code{"observed"} — direct weekly observations extracted from reports.
#'   \item \code{"fourier_*"} — historical annual/quarterly totals disaggregated
#'     to weekly via a seasonal template. These carry a lower
#'     \code{confidence_weight} (typically ~0.5) so downstream consumers can
#'     down-weight them rather than treat them as direct observations.
#'   \item \code{"documented_zero"} — confirmed-absence weeks. They flow through
#'     the merge and only fill genuine gaps because the surveillance combiner's
#'     priority de-duplication keeps higher-priority sources where they overlap.
#' }
#' We drop \code{"assumed_zero"} (default-zero assumptions with no evidentiary
#' basis) and any other / missing method tag.
#'
#' The \code{source} column in the raw AI file tracks the agent's
#' \emph{collection method}, not data origin, so it is intentionally not carried
#' through; \code{process_cholera_surveillance_data()} labels every row from this
#' file as source \code{"AI"} for priority purposes.
#'
#' CRLF line endings in the raw files leave a trailing carriage return on the
#' last column (\code{disaggregation_method}); it is stripped on read.
#'
#' @param PATHS A list of file paths (typically from \code{get_paths()}). Must
#'   include:
#' \itemize{
#'   \item \strong{AI_CHOLERA_REPO}: Path to the \code{ai-cholera-data-mining}
#'     repo (its \code{data/<ISO>/cholera_weekly_<ISO>.csv} files are read).
#'   \item \strong{DATA_AI_WEEKLY}: Output directory for the combined processed
#'     CSV.
#' }
#'
#' @return Invisibly returns the combined processed data frame. Side effect:
#'   writes \code{DATA_AI_WEEKLY/cholera_country_weekly_processed.csv} with
#'   columns \code{iso_code, country, year, week, cases, deaths, date_start,
#'   date_stop, month, confidence_weight, disaggregation_method}.
#'
#' @seealso \code{\link{process_cholera_surveillance_data}}
#'
#' @importFrom utils read.csv write.csv
#' @export
process_AI_cholera_data <- function(PATHS) {

     in_dir <- file.path(PATHS$AI_CHOLERA_REPO, "data")
     if (!dir.exists(in_dir)) {
          stop(sprintf("AI cholera data directory not found: %s", in_dir), call. = FALSE)
     }

     out_dir <- PATHS$DATA_AI_WEEKLY
     if (is.null(out_dir)) stop("PATHS$DATA_AI_WEEKLY is not set.", call. = FALSE)
     if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

     files <- list.files(in_dir, pattern = "^cholera_weekly_.*\\.csv$",
                         recursive = TRUE, full.names = TRUE)
     if (length(files) == 0) {
          stop(sprintf("No cholera_weekly_<ISO>.csv files found under %s", in_dir),
               call. = FALSE)
     }

     # Read + filter one country file (keep-and-weight).
     read_one <- function(f) {
          df <- utils::read.csv(f, stringsAsFactors = FALSE)
          req <- c("iso_code", "year", "iso_week", "week_start", "week_end",
                   "sCh", "deaths", "confidence_weight", "disaggregation_method")
          if (!all(req %in% names(df))) return(NULL)
          # Strip CRLF carriage return left on the last column (and source if present).
          df$disaggregation_method <- sub("\r$", "", df$disaggregation_method)
          dm <- df$disaggregation_method
          keep <- !is.na(dm) &
               (dm %in% c("observed", "documented_zero") | startsWith(dm, "fourier_"))
          df <- df[keep, , drop = FALSE]
          if (nrow(df) == 0) return(NULL)
          df
     }

     parts <- Filter(Negate(is.null), lapply(files, read_one))
     if (length(parts) == 0) stop("No AI rows passed the keep-and-weight filter.", call. = FALSE)

     ai <- do.call(rbind, lapply(parts, function(x)
          x[, c("iso_code", "year", "iso_week", "week_start", "week_end",
                "sCh", "deaths", "confidence_weight", "disaggregation_method")]))

     week_start <- as.Date(ai$week_start)
     out <- data.frame(
          iso_code              = ai$iso_code,
          country               = MOSAIC::convert_iso_to_country(ai$iso_code),
          year                  = as.integer(ai$year),
          week                  = as.integer(ai$iso_week),
          cases                 = as.numeric(ai$sCh),
          deaths                = as.numeric(ai$deaths),
          date_start            = week_start,
          date_stop             = as.Date(ai$week_end),
          month                 = as.integer(format(week_start, "%m")),
          confidence_weight     = as.numeric(ai$confidence_weight),
          disaggregation_method = ai$disaggregation_method,
          stringsAsFactors      = FALSE
     )

     # Defensive: drop rows missing merge keys.
     out <- out[!is.na(out$iso_code) & !is.na(out$year) & !is.na(out$week), ]
     out <- out[order(out$iso_code, out$year, out$week), ]

     out_file <- file.path(out_dir, "cholera_country_weekly_processed.csv")
     utils::write.csv(out, out_file, row.names = FALSE)

     message(sprintf(
          "AI cholera processed: %d rows across %d countries written to %s",
          nrow(out), length(unique(out$iso_code)), out_file))
     message(sprintf(
          "  - disaggregation_method breakdown: %s",
          paste(sprintf("%s=%d", names(table(out$disaggregation_method)),
                        as.integer(table(out$disaggregation_method))),
                collapse = ", ")))

     invisible(out)
}
