#' Fetch a URL to disk atomically, with timeout, retry and validation
#'
#' Shared transport for every MOSAIC \code{download_*} function. Downloads to
#' a temporary file, validates it, and only then moves it into place, so a
#' failed or truncated fetch can never leave a partial artifact where a
#' newest-wins resolver will later select it as canonical.
#'
#' @param url URL to fetch.
#' @param dest Final destination path.
#' @param min_bytes Reject (and discard) a response smaller than this. The
#'   realistic failure for these sources is a truncated body or an HTML error
#'   page served with HTTP 200, not a clean HTTP error.
#' @param validate Optional function taking the temp path and returning
#'   \code{TRUE} if the content is usable. Runs before the file is moved.
#' @param headers Named character vector of extra request headers.
#' @param timeout Seconds. Default \code{max(300, getOption("timeout"))} --
#'   R's own \code{?download.file} mandates raising the 60 s default in
#'   packages, and several MOSAIC sources are 6-23 MB.
#' @param retries Number of additional attempts after a failure. Default 2.
#' @param backoff Base seconds for exponential backoff between attempts.
#' @param overwrite Overwrite \code{dest} if it already exists.
#' @param verbose Emit progress messages.
#'
#' @return Invisibly, a list: \code{ok}, \code{bytes}, \code{path},
#'   \code{note}. Never throws on a network failure -- callers decide whether
#'   a failure is fatal.
#'
#' @keywords internal
#' @noRd
.mosaic_download <- function(url,
                             dest,
                             min_bytes = 1L,
                             validate  = NULL,
                             headers   = NULL,
                             timeout   = NULL,
                             retries   = 2L,
                             backoff   = 2,
                             overwrite = FALSE,
                             verbose   = TRUE) {

     fail <- function(note) list(ok = FALSE, bytes = NA_real_, path = dest, note = note)

     if (file.exists(dest) && !overwrite) {
          return(list(ok = TRUE, bytes = file.info(dest)$size, path = dest,
                      note = "existing"))
     }

     if (is.null(timeout)) timeout <- max(300, getOption("timeout", 60))
     old_to <- options(timeout = timeout)
     on.exit(options(old_to), add = TRUE)
     if (!is.null(headers)) {
          old_ua <- options(HTTPUserAgent = unname(headers[["User-Agent"]] %||%
                                                        getOption("HTTPUserAgent")))
          on.exit(options(old_ua), add = TRUE)
     }

     last <- "unknown"
     for (attempt in seq_len(max(1L, retries + 1L))) {

          tmp <- tempfile(fileext = paste0(".", tools::file_ext(dest)))
          ok <- tryCatch({
               # warning -> FALSE would discard a COMPLETE file: download.file
               # warns benignly on "downloaded length != reported length" under
               # chunked transfer encoding, AFTER writing everything. Muffle
               # and let the size/validate gates decide.
               withCallingHandlers(
                    utils::download.file(url, destfile = tmp, quiet = TRUE, mode = "wb"),
                    warning = function(w) invokeRestart("muffleWarning"))
               TRUE
          }, error = function(e) { last <<- conditionMessage(e); FALSE })

          sz <- if (file.exists(tmp)) file.info(tmp)$size else 0
          if (ok && sz >= min_bytes) {
               good <- TRUE
               if (is.function(validate)) {
                    good <- isTRUE(tryCatch(validate(tmp), error = function(e) {
                         last <<- paste("validation error:", conditionMessage(e)); FALSE }))
                    if (!good && identical(last, "unknown")) last <- "failed validation"
               }
               if (good) {
                    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
                    # atomic-ish: rename within a filesystem, copy across one
                    if (!file.rename(tmp, dest)) {
                         if (!file.copy(tmp, dest, overwrite = TRUE)) {
                              unlink(tmp); return(fail("could not move into place"))
                         }
                         unlink(tmp)
                    }
                    return(list(ok = TRUE, bytes = sz, path = dest, note = NA_character_))
               }
          } else if (ok) {
               last <- sprintf("short response (%s bytes, need >= %s)", sz, min_bytes)
          }

          unlink(tmp)
          if (attempt <= retries) {
               wait <- backoff^attempt
               if (verbose) message(sprintf("     attempt %d failed (%s); retrying in %gs",
                                            attempt, last, wait))
               Sys.sleep(wait)
          }
     }
     fail(last)
}


#' Write a file atomically
#'
#' Calls \code{writer(tmp)} on a temporary path in the destination directory,
#' then renames it onto \code{dest}, so an interrupted write can never leave a
#' truncated file under the final name (where a newest-wins resolver would pick
#' it up as canonical).
#'
#' @param dest Final destination path.
#' @param writer Function of one argument (the temporary path) that writes the file.
#' @return Invisibly, \code{dest}.
#' @keywords internal
#' @noRd
.write_file_atomic <- function(dest, writer) {
     dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
     tmp <- tempfile(pattern = paste0(".", basename(dest), "."), tmpdir = dirname(dest),
                     fileext = paste0(".", tools::file_ext(dest)))
     on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
     writer(tmp)
     if (!file.exists(tmp)) stop("Atomic write produced no file for ", dest, call. = FALSE)
     if (!file.rename(tmp, dest)) {
          stop("Could not move ", basename(tmp), " into place as ", dest, call. = FALSE)
     }
     invisible(dest)
}


#' Append one row to a raw-source PROVENANCE.md ledger
#'
#' Every automated writer into \code{MOSAIC-data/raw/<source>/} logs what it
#' wrote, so a processed artifact can be traced back to the snapshot that fed
#' it. One markdown table row per written file.
#'
#' @param dir Directory holding (or to hold) \code{PROVENANCE.md}.
#' @param file File written: an absolute path is logged by its basename, a relative path (e.g. \code{"GDP/API_...csv"}) as given.
#' @param n_row,n_col Dimensions of what was written (\code{NA} if not tabular).
#' @param scope Free-text note: source URL / endpoint, filters, revision.
#' @param snapshot_date Retrieval date.
#' @param title Heading used when the ledger is created.
#' @return Invisibly, the ledger path.
#' @keywords internal
#' @noRd
.append_raw_provenance <- function(dir, file, n_row = NA_integer_, n_col = NA_integer_,
                                   scope = "", snapshot_date = Sys.Date(),
                                   title = "Automated snapshot provenance log") {
     dir.create(dir, recursive = TRUE, showWarnings = FALSE)
     f <- file.path(dir, "PROVENANCE.md")
     rec <- if (grepl("^(/|~|[A-Za-z]:)", file)) basename(file) else file
     row <- sprintf("| %s | %s | %s | %s | %s |",
                    format(snapshot_date), rec,
                    ifelse(is.na(n_row), "", as.character(n_row)),
                    ifelse(is.na(n_col), "", as.character(n_col)),
                    gsub("|", "/", scope, fixed = TRUE))
     if (!file.exists(f)) {
          writeLines(c(paste("#", title), "",
                       "| Retrieved | File | Rows | Cols | Scope notes |",
                       "|---|---|---|---|---|", row), f)
     } else {
          cat(row, "\n", sep = "", file = f, append = TRUE)
     }
     invisible(f)
}
