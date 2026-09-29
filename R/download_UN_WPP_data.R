#' Download UN World Population Prospects demographic indicators
#'
#' Fetches the UN WPP "Demographic Indicators (Medium variant)" bulk CSV -- open,
#' no credentials -- and writes the three series MOSAIC consumes (total
#' population, crude birth rate, crude death rate) into
#' \code{MOSAIC-data/raw/demographics/} in the column layout
#' \code{\link{process_UN_demographics_data}} reads.
#'
#' @param PATHS List of paths from \code{\link{get_paths}}. Must include
#'   \code{DATA_RAW}.
#' @param iso_codes ISO3 codes to keep. \code{NULL} (the default) resolves to
#'   \code{MOSAIC::iso_codes_africa} -- \strong{54} countries, all 40 MOSAIC
#'   countries included. Note the hand-downloaded exports carry \strong{58}
#'   ISO codes, so this default silently drops MYT, REU, SHN and ESH (all are
#'   present in the WPP source; this is a scope choice, not a source limit).
#'   Pass \code{character(0)} for every country -- see the scope warning under
#'   Output layout before doing so.
#' @param year_start Earliest year to keep. Default \code{1967}, matching the
#'   existing exports.
#' @param revision WPP revision year. Default \code{2024}. Bumping this is the
#'   single change needed when the UN publishes a new revision.
#' @param url Full override for the source URL. Built from \code{revision}
#'   when \code{NULL}.
#' @param snapshot_date Date stamp for the output filenames. Defaults to today.
#' @param overwrite If \code{FALSE} (default), skip when today's files exist.
#' @param verbose Print progress.
#'
#' @return Invisibly, a \code{data.frame}: \code{measure}, \code{ok},
#'   \code{n_rows}, \code{n_countries}, \code{year_min}, \code{year_max},
#'   \code{file}.
#'
#' @section Units (read before changing this):
#' WPP publishes \code{TPopulation1July} in \strong{thousands}; the Data-Portal
#' exports previously kept in \code{raw/demographics/} are in \strong{persons}.
#' This function multiplies population by 1000 so both vintages agree. Verified
#' against the existing export (MOZ 2020: bulk \code{30783.688} thousand ->
#' \code{30783688} persons, matching the Data-Portal file exactly). \code{CBR}
#' and \code{CDR} are per-1,000 in both and are passed through unchanged
#' (MOZ 2020: 38.736 and 8.007 in both). Getting this wrong is a silent
#' 1000x error in every population-scaled quantity in the model.
#'
#' \code{Variant == "Medium"} is selected, matching the \code{"Median"}-labelled
#' rows in the Data-Portal exports (same series, different label).
#'
#' @section Output layout:
#' Files are written as
#' \code{UN_world_population_prospects_<measure>_wpp<revision>_<date>.csv} with
#' columns \code{Iso3}, \code{Time}, \code{Value}, \code{IndicatorName},
#' \code{Variant}. \code{process_UN_demographics_data()} selects the newest
#' file per measure, so pre-existing hand-downloaded exports remain valid and
#' are simply outranked. Nothing is deleted.
#'
#' @source UN DESA Population Division, World Population Prospects,
#'   \url{https://population.un.org/wpp/}. Licence CC BY 3.0 IGO.
#'
#' @seealso \code{\link{process_UN_demographics_data}}
#'
#' @importFrom utils read.csv write.csv download.file
#' @importFrom glue glue
#' @export
#'
#' @examples
#' \dontrun{
#' PATHS <- get_paths()
#' download_UN_WPP_data(PATHS)
#' process_UN_demographics_data(PATHS)
#' }
download_UN_WPP_data <- function(PATHS,
                                 iso_codes     = NULL,
                                 year_start    = 1967L,
                                 revision      = 2024L,
                                 url           = NULL,
                                 snapshot_date = Sys.Date(),
                                 overwrite     = FALSE,
                                 verbose       = TRUE) {

     if (is.null(PATHS$DATA_RAW)) {
          stop("PATHS must include DATA_RAW (regenerate via get_paths()).", call. = FALSE)
     }
     if (is.null(url)) {
          url <- sprintf(paste0("https://population.un.org/wpp/assets/Excel%%20Files/",
                                "1_Indicator%%20(Standard)/CSV_FILES/",
                                "WPP%d_Demographic_Indicators_Medium.csv.gz"), as.integer(revision))
     }

     dir_out <- file.path(PATHS$DATA_RAW, "demographics")
     dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

     spec <- list(
          list(measure = "population_size", col = "TPopulation1July",
               name = "Total population by sex", scale = 1000),
          list(measure = "birth_rate", col = "CBR",
               name = "Crude birth rate", scale = 1),
          list(measure = "death_rate", col = "CDR",
               name = "Crude death rate", scale = 1)
     )

     dests <- vapply(spec, function(s) file.path(
          dir_out, sprintf("UN_world_population_prospects_%s_wpp%d_%s.csv",
                           s$measure, as.integer(revision), format(snapshot_date))), "")

     if (all(file.exists(dests)) && !overwrite) {
          if (verbose) message("WPP snapshot for ", format(snapshot_date),
                               " already present (overwrite = TRUE to refetch)")
          return(invisible(.wpp_summarise(spec, dests)))
     }

     if (verbose) message("Downloading UN WPP", revision, " demographic indicators ...")
     tmp_gz <- tempfile(fileext = ".csv.gz")
     on.exit(unlink(tmp_gz), add = TRUE)
     dl <- .mosaic_download(url, tmp_gz, min_bytes = 1e6, overwrite = TRUE,
                            verbose = verbose)
     if (!dl$ok) stop("WPP download failed: ", dl$note, call. = FALSE)

     d <- utils::read.csv(gzfile(tmp_gz), stringsAsFactors = FALSE)
     need <- c("ISO3_code", "Time", "Variant", vapply(spec, `[[`, "", "col"))
     miss <- setdiff(need, names(d))
     if (length(miss)) {
          stop("WPP file is missing expected column(s): ", paste(miss, collapse = ", "),
               "\n  The WPP schema may have changed; see ?download_UN_WPP_data.",
               call. = FALSE)
     }

     if (is.null(iso_codes)) {
          iso_codes <- get("iso_codes_africa", envir = asNamespace("MOSAIC"))
     }
     d <- d[d$Variant == "Medium" & nzchar(d$ISO3_code), , drop = FALSE]
     if (length(iso_codes)) d <- d[d$ISO3_code %in% iso_codes, , drop = FALSE]
     if (!is.null(year_start)) {
          d <- d[suppressWarnings(as.integer(d$Time)) >= as.integer(year_start), , drop = FALSE]
     }
     if (!nrow(d)) {
          stop("No WPP rows left after Medium-variant / ISO / year filtering.", call. = FALSE)
     }
     if (verbose) {
          message(glue::glue("  {nrow(d)} Medium-variant country-years ",
                             "({length(unique(d$ISO3_code))} countries, from {year_start})"))
     }

     out <- lapply(seq_along(spec), function(i) {
          s <- spec[[i]]
          v <- suppressWarnings(as.numeric(d[[s$col]])) * s$scale
          keep <- !is.na(v)
          frame <- data.frame(Iso3 = d$ISO3_code[keep], Time = d$Time[keep],
                              Value = v[keep], IndicatorName = s$name,
                              Variant = "Medium", stringsAsFactors = FALSE)
          utils::write.csv(frame, dests[i], row.names = FALSE)
          if (verbose) {
               message(glue::glue(
                    "  {s$measure}: {nrow(frame)} rows, {length(unique(frame$Iso3))} countries, ",
                    "{min(frame$Time)}-{max(frame$Time)}",
                    if (s$scale != 1) glue::glue("  [x{s$scale} -> persons]") else ""))
          }
          data.frame(measure = s$measure, ok = TRUE, n_rows = nrow(frame),
                     n_countries = length(unique(frame$Iso3)),
                     year_min = min(frame$Time), year_max = max(frame$Time),
                     file = dests[i], stringsAsFactors = FALSE)
     })

     res <- do.call(rbind, out)
     if (verbose) message("WPP: wrote ", sum(res$ok), "/", nrow(res), " series.")
     invisible(res)
}


#' @keywords internal
#' @noRd
.wpp_summarise <- function(spec, dests) {
     do.call(rbind, lapply(seq_along(spec), function(i) {
          d <- tryCatch(utils::read.csv(dests[i], stringsAsFactors = FALSE),
                        error = function(e) NULL)
          # a 0-row CSV previously returned ok=TRUE with year_min=Inf
          usable <- !is.null(d) && nrow(d) > 0L
          data.frame(measure = spec[[i]]$measure, ok = usable,
                     n_rows = if (!usable) NA_integer_ else nrow(d),
                     n_countries = if (!usable) NA_integer_ else length(unique(d$Iso3)),
                     year_min = if (!usable) NA_integer_ else min(d$Time),
                     year_max = if (!usable) NA_integer_ else max(d$Time),
                     file = dests[i], stringsAsFactors = FALSE)
     }))
}


#' Locate the newest raw WPP file for one measure
#'
#' Hand-downloaded Data-Portal exports
#' (\code{UN_world_population_prospects_1967_2100_<measure>.csv}) and API pulls
#' (\code{..._<measure>_wpp<rev>_<date>.csv}) coexist; the newest by mtime wins.
#'
#' @keywords internal
#' @noRd
.wpp_newest_raw <- function(PATHS, measure) {
     dir_in <- file.path(PATHS$DATA_RAW, "demographics")
     hits <- list.files(dir_in,
                        pattern = paste0("^UN_world_population_prospects_.*",
                                         measure, ".*\\.csv$"),
                        full.names = TRUE, ignore.case = TRUE)
     if (!length(hits)) {
          stop("No raw WPP file for '", measure, "' in ", dir_in,
               "\n  Fetch it with download_UN_WPP_data(PATHS) (open, no credentials).",
               call. = FALSE)
     }
     .rank_raw_candidates(hits)[1L]
}
