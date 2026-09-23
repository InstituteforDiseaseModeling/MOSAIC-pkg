#' Download EM-DAT disaster events from the CRED GraphQL API
#'
#' Queries the EM-DAT \code{public_emdat} GraphQL endpoint, pages through the
#' full result set, renames the API's snake_case fields to the portal-export
#' column names that \code{\link{process_EMDAT_data}} expects, and writes a
#' date-stamped CSV into \code{PATHS$DATA_EMDAT_RAW}. Also appends a row to
#' that directory's \code{PROVENANCE.md} ledger.
#'
#' @param PATHS List of paths from \code{\link{get_paths}}. Must include
#'   \code{DATA_EMDAT_RAW}.
#' @param source Which distribution to pull.
#'   \describe{
#'     \item{\code{"api"}}{(default) The CRED GraphQL API. Current, CC BY 4.0,
#'       \strong{requires a key}.}
#'     \item{\code{"portal"}}{The session-authenticated file the web portal
#'       mints after a custom request (\code{public.emdat.be/graphql/files/}).
#'       Current, CC BY 4.0, needs a browser \strong{session cookie} rather
#'       than an API key.}
#'     \item{\code{"dataverse"}}{The UCLouvain Dataverse archive. Open, no
#'       credentials, MD5-verified -- but a \strong{static, heavily lagged}
#'       release and CC BY-NC-ND. See the section below before using it.}
#'   }
#' @param api_key EM-DAT API key for \code{source = "api"}. If \code{NULL}
#'   (default), read from the \code{EMDAT_API_KEY} environment variable.
#' @param cookie Session cookie string for \code{source = "portal"}, as taken
#'   from a browser signed in to \url{https://public.emdat.be}. If
#'   \code{NULL} (default), read from the \code{EMDAT_SESSION_COOKIE}
#'   environment variable.
#' @param url For \code{source = "portal"}, the exact download URL the portal
#'   produced. Overrides \code{file_date}. Paste it straight from the browser.
#' @param file_date For \code{source = "portal"}, the date stamp in the
#'   portal's filename, used to build
#'   \code{https://public.emdat.be/graphql/files/public_emdat_<date>.xlsx}.
#'   Defaults to today. The portal mints a file per request, so a guessed date
#'   will usually not exist -- prefer \code{url}.
#' @param year_start,year_stop Inclusive year bounds passed to the API's
#'   \code{from} / \code{to} filters. \code{year_stop = NULL} (default) uses
#'   the current year.
#' @param classif Character vector of EM-DAT classification keys to request.
#'   Defaults to the three hazard families MOSAIC models:
#'   \code{"nat-hyd-flo"} (flood), \code{"nat-met-sto"} (storm, which carries
#'   the Tropical cyclone / Storm surge subtypes) and \code{"nat-cli-dro"}
#'   (drought). Pass \code{NULL} to request \strong{all} classifications,
#'   matching the "all natural disasters" scope of the manual portal extracts.
#' @param include_hist Passed to the API's \code{include_hist} filter
#'   (\code{TRUE} by default) so historic records are not silently dropped.
#' @param page_size Rows per request (default 1000).
#' @param snapshot_date Date stamp for the output filename. Defaults to
#'   \code{Sys.Date()}.
#' @param overwrite If \code{FALSE} (default) and the target file already
#'   exists, the download is skipped.
#' @param verbose If \code{TRUE} (default), print progress and a summary.
#'
#' @return Invisibly, the path to the written CSV.
#'
#' @section Getting an API key:
#' The endpoint requires an \code{Authorization} header; an unauthenticated
#' request returns
#' \code{{"errors":[{"message":"Missing API Key, please provide a value for the header: Authorization"}]}}.
#' Keys are issued by CRED / UCLouvain via \url{https://public.emdat.be} (free
#' registration covers non-commercial use; whether that tier includes API
#' access is not stated in the public documentation -- ask CRED). Store it
#' outside every git tree, e.g. in \code{/Users/<you>/MOSAIC/.env} as
#' \code{EMDAT_API_KEY=...}, and export it before calling.
#'
#' @section The two open alternatives, measured:
#' Both were tested on 2026-09-17 against the committed panels:
#' \itemize{
#'   \item \strong{HDX \code{emdat-country-profiles-<iso>}} -- refreshed daily,
#'     but \emph{annual aggregates}: one row per (Year, Country, Disaster Type,
#'     Disaster Subtype) with \code{Total Events} / \code{Total Affected} /
#'     \code{Total Deaths} and \strong{no event start or end dates}. A
#'     country-week panel cannot be built from them at all. (This is the key
#'     difference from IDMC, whose HDX mirrors \emph{are} event-level -- see
#'     \code{\link{download_IDMC_data}}.)
#'   \item \strong{UCLouvain Dataverse} (\code{source = "dataverse"}) -- open,
#'     event-level, and \emph{structurally a drop-in}: same \code{EM-DAT Data}
#'     sheet, same 47 columns, processes with no code changes. \strong{But its
#'     data stops at 2024-12-31} while being released 2026-04-30 -- a 625-day
#'     lag as measured. Against the 2026-07-09 portal extract it is missing 102
#'     events from 2025 and 33 from 2026 (134 DisNo. absent), 36 AFRO floods and
#'     3 cyclone/surge events from 2000 onward, and the named cyclones
#'     \strong{Dikeledi (2025-01), Jude (2025-03) and Gezani (2026-02)}; only
#'     Chido (2024-12) is present. The resulting panel ends 2024-12-16 versus
#'     2026-04-27 -- 71 country-weeks short per country. It is also
#'     \strong{CC BY-NC-ND} ("unadapted form only"), which sits badly with
#'     deriving weekly panels; the portal and API distributions are CC BY 4.0.
#' }
#' \strong{Verdict:} \code{"dataverse"} is a usable offline/reproducibility
#' fallback and a clean way to pin a historical build, but it is NOT a
#' substitute for the API or a portal extract in a pipeline that forecasts the
#' current year.
#'
#' \strong{Ranking safety.} The downloaded file is stamped with its DATA CUTOFF
#' (\code{public_emdat_dataverse_2024-12-31.xlsx}), not its release date,
#' because \code{process_EMDAT_data()} selects the newest extract by the
#' filename-encoded date. Stamping by release date (2026-04-30) would let this
#' stale archive outrank a fresher portal extract and silently truncate the
#' panel. Verified: with both files present the processor still selects the
#' 2026-07-09 portal extract.
#'
#' @section Field mapping (UNVERIFIED -- applies to source = "api" only):
#' The API path has \strong{not} been executed against a live key, so the
#' API-to-portal column mapping in \code{.emdat_api_field_map()} is derived
#' from EM-DAT's published R API guide and the portal export schema, not from
#' an observed response. \code{process_EMDAT_data()} now hard-fails on a
#' missing required column, so a mismatch surfaces immediately rather than
#' producing an empty panel. To inspect the real schema, open the GraphiQL
#' explorer at \url{https://api.emdat.be/} with your key and adjust
#' \code{fields} / the map as needed.
#'
#' @source EM-DAT, CRED / UCLouvain, Brussels. \url{https://www.emdat.be}.
#'   Licence CC BY 4.0 -- attribution required.
#'
#' @seealso \code{\link{process_EMDAT_data}} to build the country-week panels.
#'
#' @importFrom utils write.csv download.file
#' @importFrom glue glue
#' @export
#'
#' @examples
#' \dontrun{
#' PATHS <- get_paths()
#'
#' # Current data (needs a key)
#' Sys.setenv(EMDAT_API_KEY = "...")
#' download_EMDAT_data(PATHS)
#'
#' # Open fallback -- warns loudly; data stops at 2024
#' download_EMDAT_data(PATHS, source = "dataverse")
#'
#' process_EMDAT_data(PATHS)
#' }
download_EMDAT_data <- function(PATHS,
                                source        = c("api", "portal", "dataverse"),
                                api_key       = NULL,
                                cookie        = NULL,
                                url           = NULL,
                                file_date     = NULL,
                                year_start    = 2000L,
                                year_stop     = NULL,
                                classif       = c("nat-hyd-flo", "nat-met-sto", "nat-cli-dro"),
                                include_hist  = TRUE,
                                page_size     = 1000L,
                                snapshot_date = Sys.Date(),
                                overwrite     = FALSE,
                                verbose       = TRUE) {

     source <- match.arg(source)

     if (is.null(PATHS$DATA_EMDAT_RAW)) {
          stop("PATHS must include DATA_EMDAT_RAW (regenerate via get_paths()).",
               call. = FALSE)
     }

     if (source == "portal") {
          return(invisible(.emdat_download_portal(
               raw_dir   = PATHS$DATA_EMDAT_RAW,
               cookie    = cookie,
               url       = url,
               file_date = file_date,
               overwrite = overwrite,
               verbose   = verbose)))
     }

     if (source == "dataverse") {
          return(invisible(.emdat_download_dataverse(
               raw_dir       = PATHS$DATA_EMDAT_RAW,
               overwrite     = overwrite,
               verbose       = verbose)))
     }

     if (is.null(api_key)) api_key <- Sys.getenv("EMDAT_API_KEY", unset = "")
     if (!nzchar(api_key)) {
          stop("No EM-DAT API key.\n",
               "  Set EMDAT_API_KEY (see ?download_EMDAT_data, section 'Getting an API key')\n",
               "  or pass api_key = \"...\" explicitly.", call. = FALSE)
     }
     if (is.null(year_stop)) year_stop <- as.integer(format(Sys.Date(), "%Y"))

     dir.create(PATHS$DATA_EMDAT_RAW, recursive = TRUE, showWarnings = FALSE)
     dest <- file.path(PATHS$DATA_EMDAT_RAW,
                       sprintf("public_emdat_api_%s.csv", format(snapshot_date)))
     if (file.exists(dest) && !overwrite) {
          if (verbose) message("Snapshot already exists: ", basename(dest),
                               " (pass overwrite = TRUE to refetch)")
          return(invisible(dest))
     }

     fields <- names(.emdat_api_field_map())

     offset    <- 0L
     pages     <- list()
     total     <- NA_integer_
     max_pages <- 1000L      # hard stop: see the guard below

     repeat {
          if (length(pages) >= max_pages) {
               stop("EM-DAT paging exceeded ", max_pages, " pages without terminating.\n",
                    "  The API omitted total_available or ignored the cursor; refusing ",
                    "to loop unbounded.", call. = FALSE)
          }
          resp <- .emdat_api_query(api_key      = api_key,
                                   fields       = fields,
                                   from         = year_start,
                                   to           = year_stop,
                                   classif      = classif,
                                   include_hist = include_hist,
                                   limit        = page_size,
                                   offset       = offset)

          if (is.na(total)) {
               total <- as.integer(resp$total_available %||% NA_integer_)
               if (verbose) message(glue::glue(
                    "EM-DAT API: {total} events match ",
                    "({year_start}-{year_stop}, classif = ",
                    "{if (is.null(classif)) 'ALL' else paste(classif, collapse = '/')})"))
          }

          chunk <- resp$data
          if (is.null(chunk) || !NROW(chunk)) break
          pages[[length(pages) + 1L]] <- chunk
          offset <- offset + NROW(chunk)
          if (verbose) message(glue::glue("  fetched {offset}/{total}"))
          if (!is.na(total) && offset >= total) break
     }

     if (!length(pages)) stop("EM-DAT API returned no rows.", call. = FALSE)

     d <- do.call(rbind, lapply(pages, as.data.frame, stringsAsFactors = FALSE))
     d <- .emdat_api_rename(d)

     utils::write.csv(d, dest, row.names = FALSE, na = "")
     .emdat_append_provenance(PATHS$DATA_EMDAT_RAW, dest, d,
                              year_start, year_stop, classif, snapshot_date)

     if (verbose) {
          message(glue::glue(
               "\nWrote {nrow(d)} events x {ncol(d)} cols -> {basename(dest)}"))
          message("Logged to PROVENANCE.md. Next: process_EMDAT_data(PATHS)")
     }
     invisible(dest)
}


#' EM-DAT API field -> portal column-name map
#'
#' Names are the GraphQL field names requested; values are the column names
#' the portal xlsx export uses, which \code{process_EMDAT_data()} reads.
#' Adjust here if the API schema differs -- see the GraphiQL explorer at
#' \url{https://api.emdat.be/}.
#'
#' @keywords internal
#' @noRd
.emdat_api_field_map <- function() {
     c(disno          = "DisNo.",
       iso            = "ISO",
       country        = "Country",
       classif_key    = "Classification Key",
       group          = "Disaster Group",
       subgroup       = "Disaster Subgroup",
       type           = "Disaster Type",
       subtype        = "Disaster Subtype",
       start_year     = "Start Year",
       start_month    = "Start Month",
       start_day      = "Start Day",
       end_year       = "End Year",
       end_month      = "End Month",
       end_day        = "End Day",
       total_deaths   = "Total Deaths",
       total_affected = "Total Affected",
       entry_date     = "Entry Date",
       last_update    = "Last Update")
}


#' Rename an API response frame to portal column names
#'
#' @keywords internal
#' @noRd
.emdat_api_rename <- function(d) {
     map <- .emdat_api_field_map()
     hit <- intersect(names(d), names(map))
     names(d)[match(hit, names(d))] <- unname(map[hit])
     d
}


#' Execute one paged EM-DAT GraphQL request
#'
#' @keywords internal
#' @noRd
.emdat_api_query <- function(api_key, fields, from, to, classif,
                             include_hist, limit, offset) {

     classif_arg <- if (is.null(classif) || !length(classif)) "" else
          paste0("classif: [", paste0('"', classif, '"', collapse = ", "), "], ")

     query <- sprintf(
          'query mosaic {
             public_emdat(
               cursor: {limit: %d, offset: %d}
               filters: {from: %d, to: %d, %sinclude_hist: %s}
             ) {
               total_available
               data { %s }
             }
           }',
          as.integer(limit), as.integer(offset),
          as.integer(from), as.integer(to),
          classif_arg, if (isTRUE(include_hist)) "true" else "false",
          paste(fields, collapse = " "))

     resp <- httr::POST(
          url    = "https://api.emdat.be/v1",
          httr::add_headers(Authorization = api_key),
          body   = list(query = query),
          encode = "json",
          httr::timeout(max(300, getOption("timeout", 60)))
     )

     code <- httr::status_code(resp)
     txt  <- httr::content(resp, as = "text", encoding = "UTF-8")
     if (code >= 400L) {
          stop("EM-DAT API returned HTTP ", code, ": ", substr(txt, 1, 200), call. = FALSE)
     }
     js <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE),
                    error = function(e)
                         stop("EM-DAT API returned a non-JSON body (HTTP ", code, "): ",
                              substr(txt, 1, 200), call. = FALSE))

     if (!is.null(js$errors)) {
          msg <- paste(unique(as.character(js$errors$message)), collapse = "; ")
          stop("EM-DAT API error: ", msg,
               if (grepl("API Key", msg, ignore.case = TRUE))
                    "\n  (see ?download_EMDAT_data, section 'Getting an API key')" else "",
               call. = FALSE)
     }
     out <- js$data$public_emdat
     if (is.null(out)) stop("EM-DAT API returned an unexpected payload shape.", call. = FALSE)
     out
}


#' Append a snapshot row to raw/EMDAT/PROVENANCE.md
#'
#' The directory's ledger convention is one row per extract. Keeping it
#' current automatically means an API pull is as traceable as a manual one.
#'
#' @keywords internal
#' @noRd
.emdat_append_provenance <- function(raw_dir, dest, d, year_start, year_stop,
                                     classif, snapshot_date) {
     scope <- sprintf("API pull (api.emdat.be/v1). classif=%s, years %d-%d.",
                      if (is.null(classif) || !length(classif)) "ALL"
                      else paste(classif, collapse = "/"),
                      as.integer(year_start), as.integer(year_stop))
     .emdat_append_provenance_row(raw_dir, dest, nrow(d), ncol(d), scope, snapshot_date)
}


#' Append one row to raw/EMDAT/PROVENANCE.md
#'
#' @keywords internal
#' @noRd
.emdat_append_provenance_row <- function(raw_dir, dest, n_row, n_col, scope,
                                         snapshot_date = Sys.Date()) {
     f <- file.path(raw_dir, "PROVENANCE.md")
     row <- sprintf("| %s | %s | %d | %d | %s |",
                    format(snapshot_date), basename(dest), n_row, n_col, scope)
     if (!file.exists(f)) {
          writeLines(c("# EM-DAT extract provenance log", "",
                       "| Request date | UUID | Rows | Cols | Scope notes |",
                       "|---|---|---|---|---|", row), f)
     } else {
          cat(row, "\n", sep = "", file = f, append = TRUE)
     }
     invisible(f)
}


#' Download the EM-DAT archive from the UCLouvain Dataverse
#'
#' Open, credential-free route. Resolves the newest \code{.xlsx} in the
#' dataset, verifies the Dataverse-published MD5, and files it under a name
#' stamped with its DATA CUTOFF (not its release date) so the
#' newest-extract-wins rule in \code{process_EMDAT_data()} ranks extracts by
#' data recency.
#'
#' @keywords internal
#' @noRd
.emdat_download_dataverse <- function(raw_dir,
                                      doi       = "doi:10.14428/DVN/I0LTPH",
                                      overwrite = FALSE,
                                      verbose   = TRUE) {

     base <- "https://dataverse.uclouvain.be"
     meta_url <- sprintf("%s/api/datasets/:persistentId/?persistentId=%s", base, doi)

     if (verbose) message("Resolving EM-DAT archive on UCLouvain Dataverse (", doi, ") ...")
     txt <- suppressWarnings(tryCatch(
          paste(readLines(meta_url, warn = FALSE), collapse = ""),
          error = function(e) NULL))
     if (is.null(txt)) stop("Could not reach the Dataverse API at ", base, call. = FALSE)

     js <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
     lv <- js$data$latestVersion
     if (is.null(lv)) stop("Unexpected Dataverse payload for ", doi, call. = FALSE)

     files <- lv$files$dataFile
     xl <- files[grepl("\\.xlsx$", files$filename, ignore.case = TRUE), , drop = FALSE]
     if (!NROW(xl)) stop("No .xlsx file in Dataverse dataset ", doi, call. = FALSE)
     xl <- xl[order(xl$filename, decreasing = TRUE), , drop = FALSE][1L, ]

     release <- substr(lv$releaseTime %||% "", 1, 10)
     if (verbose) {
          message(glue::glue(
               "  {xl$filename} (id {xl$id}, {round(xl$filesize / 1e6, 1)} MB), ",
               "version {lv$versionNumber %||% NA} released {release}"))
     }

     tmp <- tempfile(fileext = ".xlsx")
     dl <- .mosaic_download(sprintf("%s/api/access/datafile/%s", base, xl$id),
                            tmp, min_bytes = 1e5, overwrite = TRUE, verbose = verbose)
     if (!dl$ok) stop("Dataverse download failed: ", dl$note, call. = FALSE)

     # Integrity: the Dataverse record publishes the MD5, so verify it.
     want <- tolower(xl$md5 %||% "")
     if (nzchar(want) && requireNamespace("tools", quietly = TRUE)) {
          got <- tolower(unname(tools::md5sum(tmp)))
          if (!identical(got, want)) {
               unlink(tmp)
               stop("MD5 mismatch for ", xl$filename,
                    "\n  expected ", want, "\n  got      ", got, call. = FALSE)
          }
          if (verbose) message("  MD5 verified: ", want)
     }

     # Stamp the file by its DATA CUTOFF, not its release date. The archive
     # lags its release by many months, and process_EMDAT_data() selects the
     # newest extract by the filename-encoded date -- stamping by release date
     # would let a stale archive outrank a fresher portal extract and silently
     # truncate the panel.
     # suppressWarnings: readxl type-guesses the trailing GADM JSON columns as
     # logical and emits ~50 coercion warnings. Those columns are unused here --
     # this read exists only to derive the data cutoff.
     d <- suppressWarnings(readxl::read_excel(tmp, sheet = "EM-DAT Data"))
     cutoff <- .emdat_max_start_date(d)
     if (verbose) message("  data cutoff (latest event start): ", cutoff)

     dest <- file.path(raw_dir, sprintf("public_emdat_dataverse_%s.xlsx", cutoff))
     if (file.exists(dest) && !overwrite) {
          unlink(tmp)
          if (verbose) message("Already present: ", basename(dest),
                               " (pass overwrite = TRUE to refetch)")
          return(dest)
     }
     dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
     ok <- file.copy(tmp, dest, overwrite = TRUE)
     unlink(tmp)
     if (!ok) stop("Failed to write ", dest, call. = FALSE)

     lag_days <- as.integer(Sys.Date() - as.Date(cutoff))
     if (!is.na(lag_days) && lag_days > 180L) {
          warning(sprintf(
               paste0("EM-DAT Dataverse archive is %d days behind today (latest event %s).\n",
                      "  It is a STATIC, LAGGED release -- not a live mirror. For a model ",
                      "forecasting\n  the current year, use source = \"api\" or a manual ",
                      "portal extract instead."),
               lag_days, cutoff), call. = FALSE)
     }

     .emdat_append_provenance_row(
          raw_dir, dest, nrow(d), ncol(d),
          sprintf("UCLouvain Dataverse %s v%s, released %s, DATA CUTOFF %s (lag %s d). CC BY-NC-ND.",
                  doi, as.character(lv$versionNumber %||% NA), release, cutoff,
                  ifelse(is.na(lag_days), "?", lag_days)),
          Sys.Date())

     if (verbose) {
          message(glue::glue("\nWrote {nrow(d)} events x {ncol(d)} cols -> {basename(dest)}"))
          message("Logged to PROVENANCE.md. Next: process_EMDAT_data(PATHS)")
     }
     dest
}


#' Latest event start date in an EM-DAT frame, as YYYY-MM-DD
#'
#' Month/day are frequently missing in EM-DAT; missing parts default to the
#' start of the period so the stamp never overstates recency.
#'
#' Dates after today are DISCARDED. EM-DAT carries occasional year typos --
#' e.g. in the 2026-09-16 extract, DisNo. 1995-0275-NPL (a 1995 Nepal storm)
#' has Start Year 2026, giving a max start date of 2026-11-10. Such a record
#' would inflate the cutoff stamp and make a stale extract look current, so
#' the maximum is taken over past-or-present dates only.
#'
#' @keywords internal
#' @noRd
.emdat_max_start_date <- function(d) {
     y <- suppressWarnings(as.integer(d[["Start Year"]]))
     m <- suppressWarnings(as.integer(d[["Start Month"]]))
     dd <- suppressWarnings(as.integer(d[["Start Day"]]))
     m[is.na(m)] <- 1L
     dd[is.na(dd)] <- 1L
     keep <- !is.na(y)
     if (!any(keep)) return(format(Sys.Date()))
     dts <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y[keep], m[keep], dd[keep])))
     dts <- dts[!is.na(dts)]
     dts <- dts[dts <= Sys.Date()]          # drop future-dated typos
     if (!length(dts)) return(format(Sys.Date()))
     format(max(dts))
}


#' Download the session-authenticated EM-DAT portal export
#'
#' The web portal mints \code{public_emdat_<date>.xlsx} under
#' \code{public.emdat.be/graphql/files/} after a custom request and serves it
#' only to the browser session that made it.
#'
#' \strong{The failure mode is silent.} An unauthenticated request does NOT
#' 401 or 404 -- nginx returns \code{HTTP 200} with \code{content-length: 0}
#' and no content-type, and does so for ANY path under that directory
#' (verified 2026-09-17 with a deliberately nonexistent filename). A naive
#' downloader therefore writes a 0-byte file and reports success. Every
#' response is validated here before the file is filed.
#'
#' @keywords internal
#' @noRd
.emdat_download_portal <- function(raw_dir,
                                   cookie    = NULL,
                                   url       = NULL,
                                   file_date = NULL,
                                   overwrite = FALSE,
                                   verbose   = TRUE) {

     if (is.null(cookie)) cookie <- Sys.getenv("EMDAT_SESSION_COOKIE", unset = "")
     if (!nzchar(cookie)) {
          stop("No EM-DAT portal session cookie.\n",
               "  Sign in at https://public.emdat.be, copy the session cookie, then either\n",
               "    Sys.setenv(EMDAT_SESSION_COOKIE = \"...\")   # or put it in ~/MOSAIC/.env\n",
               "  or pass cookie = \"...\".\n",
               "  No cookie? Download in the browser and move the file into ", raw_dir,
               " -- the\n  filename already matches what process_EMDAT_data() looks for.",
               call. = FALSE)
     }

     if (is.null(url)) {
          if (is.null(file_date)) file_date <- Sys.Date()
          url <- sprintf("https://public.emdat.be/graphql/files/public_emdat_%s.xlsx",
                         format(as.Date(file_date)))
     }

     stamp <- regmatches(url, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", url))
     if (!length(stamp) || nchar(stamp) != 10L) stamp <- format(Sys.Date())

     dest <- file.path(raw_dir, sprintf("public_emdat_%s.xlsx", stamp))
     if (file.exists(dest) && !overwrite) {
          if (verbose) message("Already present: ", basename(dest),
                               " (pass overwrite = TRUE to refetch)")
          return(dest)
     }

     if (verbose) message("Fetching ", url)
     tmp <- tempfile(fileext = ".xlsx")
     resp <- httr::GET(url,
                       httr::add_headers(Cookie = cookie),
                       httr::write_disk(tmp, overwrite = TRUE))

     code <- httr::status_code(resp)
     size <- if (file.exists(tmp)) file.info(tmp)$size else 0

     # An empty 200 means the session did not carry -- NOT that the file is
     # missing. Say so explicitly; this is the trap this route exists around.
     if (size == 0) {
          unlink(tmp)
          stop("EM-DAT portal returned an EMPTY body (HTTP ", code, ", 0 bytes).\n",
               "  This endpoint answers 200 with content-length 0 for ANY unauthenticated\n",
               "  request, so this almost certainly means the session cookie is missing,\n",
               "  expired, or for a different host -- not that the file is absent.\n",
               "  Re-copy the cookie from a signed-in browser, or download it manually.",
               call. = FALSE)
     }
     if (code >= 400L) {
          unlink(tmp)
          stop("EM-DAT portal returned HTTP ", code, " for ", url, call. = FALSE)
     }

     # Structural validation before the file is allowed into raw/.
     ok <- tryCatch({
          sheets <- readxl::excel_sheets(tmp)
          if (!"EM-DAT Data" %in% sheets) {
               stop("no 'EM-DAT Data' sheet (found: ", paste(sheets, collapse = ", "), ")",
                    call. = FALSE)
          }
          d <- suppressWarnings(readxl::read_excel(tmp, sheet = "EM-DAT Data", n_max = 5))
          need <- c("ISO", "Disaster Type", "Disaster Subtype",
                    "Start Year", "Start Month", "Start Day",
                    "End Year", "End Month", "End Day",
                    "Total Affected", "Total Deaths")
          miss <- setdiff(need, names(d))
          if (length(miss)) {
               stop("missing required column(s): ", paste(miss, collapse = ", "), call. = FALSE)
          }
          TRUE
     }, error = function(e) {
          unlink(tmp)
          stop("Downloaded ", round(size / 1e6, 2), " MB but it is not a usable EM-DAT ",
               "workbook: ", conditionMessage(e),
               "\n  (A login page or error document served with a 200 looks like this.)",
               call. = FALSE)
     })

     dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
     if (!file.copy(tmp, dest, overwrite = TRUE)) {
          unlink(tmp); stop("Failed to write ", dest, call. = FALSE)
     }
     unlink(tmp)

     d_all <- suppressWarnings(readxl::read_excel(dest, sheet = "EM-DAT Data"))
     cutoff <- .emdat_max_start_date(d_all)
     .emdat_append_provenance_row(
          raw_dir, dest, nrow(d_all), ncol(d_all),
          sprintf("Portal session download from %s. Data cutoff %s. CC BY 4.0.", url, cutoff),
          Sys.Date())

     if (verbose) {
          message(glue::glue(
               "  {round(size / 1e6, 2)} MB, {nrow(d_all)} events x {ncol(d_all)} cols, ",
               "cutoff {cutoff}"))
          message("Wrote ", basename(dest), "; logged to PROVENANCE.md.")
          message("Next: process_EMDAT_data(PATHS)")
     }
     dest
}
