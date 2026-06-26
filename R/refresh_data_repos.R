#' Pull latest data from external scraper repos and report coverage
#'
#' Fast-forward \code{git pull}s each of the five external scraper repos that
#' MOSAIC depends on for raw data, then prints a per-repo summary of the latest
#' commit and (for surveillance repos) the date range of the data file the
#' \code{process_*()} functions read. Designed to be called at the top of
#' \code{model/LAUNCH.R} so a refresh run starts from a known-current snapshot.
#'
#' @param root Path to the MOSAIC parent directory (containing all sibling
#'   repos). Defaults to \code{get_paths()$ROOT}, i.e. whatever
#'   \code{set_root_directory()} was last called with.
#' @param repos Optional character vector restricting the refresh to a subset
#'   of repos. Valid names: \code{"ees-cholera-mapping"},
#'   \code{"jhu_cholera_data"}, \code{"ai-cholera-data-mining"},
#'   \code{"enso-data"}, \code{"open-meteo-pipeline"}. Default refreshes all
#'   five.
#' @param stale_days Threshold (days since last commit) for the \code{STALE}
#'   flag in the printed summary. Default 14.
#' @param verbose If \code{TRUE} (default), print a formatted summary to the
#'   console.
#'
#' @return Invisibly, a list of per-repo results with fields \code{repo},
#'   \code{description}, \code{ok} (pull succeeded), \code{last_commit_date},
#'   \code{last_commit_msg}, \code{days_stale}, \code{coverage} (named list of
#'   coverage stats for surveillance repos; \code{NULL} otherwise), and
#'   \code{pull_output}.
#'
#' @details
#' Uses \code{git pull --ff-only} so the call is non-destructive: a divergent
#' local branch will fail loudly rather than auto-merge. Uncommitted local
#' changes will also block the pull and surface in \code{pull_output}.
#'
#' For \code{ees-cholera-mapping} and \code{jhu_cholera_data}, the function
#' additionally peeks at the canonical surveillance file (the one the
#' \code{process_WHO_weekly_data()} / \code{process_JHU_weekly_data()} functions read)
#' and reports record count, date range, and country count. ENSO and
#' open-meteo coverage stats are not extracted because their data is spread
#' across many per-country / per-source files; commit date is the practical
#' freshness signal.
#'
#' @examples
#' \dontrun{
#' set_root_directory("~/MOSAIC")
#' refresh_data_repos()
#'
#' # Only the surveillance scrapers
#' refresh_data_repos(repos = c("ees-cholera-mapping", "jhu_cholera_data"))
#' }
#'
#' @export
refresh_data_repos <- function(root       = NULL,
                               repos      = NULL,
                               stale_days = 14L,
                               verbose    = TRUE) {

     if (is.null(root)) {
          root <- tryCatch(MOSAIC::get_paths()$ROOT,
                           error = function(e) {
                                stop(
                                     "Cannot determine MOSAIC root directory.\n",
                                     "  Either set it for the session:\n",
                                     "    MOSAIC::set_root_directory(\"~/MOSAIC\")\n",
                                     "  or pass it explicitly:\n",
                                     "    refresh_data_repos(root = \"~/MOSAIC\")\n",
                                     "  (root should be the parent directory containing MOSAIC-pkg, ",
                                     "MOSAIC-data, ees-cholera-mapping, jhu_cholera_data, enso-data, ",
                                     "and open-meteo-pipeline as siblings.)",
                                     call. = FALSE
                                )
                           })
     }

     repo_meta <- list(
          "ees-cholera-mapping"    = "WHO surveillance (AWD weekly + GHO annual + GTFCC)",
          "jhu_cholera_data"       = "JHU public cholera surveillance dataset",
          "ai-cholera-data-mining" = "AI-mined gap-filling cholera observations (historic, no cron)",
          "enso-data"              = "ENSO/IOD: NOAA historical + NMME forecast (default) + BOM (legacy)",
          "open-meteo-pipeline"    = "Climate: ERA5 historical + MRI projections"
     )

     if (is.null(repos)) repos <- names(repo_meta)
     unknown <- setdiff(repos, names(repo_meta))
     if (length(unknown)) {
          stop("Unknown repo(s): ", paste(unknown, collapse = ", "),
               ".\nValid: ", paste(names(repo_meta), collapse = ", "), call. = FALSE)
     }

     results <- lapply(repos, function(repo) {
          path <- file.path(root, repo)

          if (!dir.exists(path)) {
               return(list(repo = repo, description = repo_meta[[repo]],
                           ok = FALSE, error = sprintf("not cloned at %s", path)))
          }

          pull <- suppressWarnings(
               system2("git", c("-C", path, "pull", "--ff-only"),
                       stdout = TRUE, stderr = TRUE)
          )
          pull_status <- attr(pull, "status")
          if (is.null(pull_status)) pull_status <- 0L

          fmt <- function(spec) {
               out <- suppressWarnings(
                    system2("git", c("-C", path, "log", "-1", paste0("--format=", spec)),
                            stdout = TRUE, stderr = FALSE)
               )
               if (length(out)) out[1L] else NA_character_
          }
          last_date <- fmt("%cI")
          last_msg  <- fmt("%s")

          days_stale <- if (!is.na(last_date)) {
               as.integer(difftime(Sys.Date(), as.Date(substr(last_date, 1, 10)), units = "days"))
          } else NA_integer_

          coverage <- .refresh_data_repos_coverage(repo, root)

          list(
               repo             = repo,
               description      = repo_meta[[repo]],
               ok               = pull_status == 0L,
               last_commit_date = last_date,
               last_commit_msg  = last_msg,
               days_stale       = days_stale,
               coverage         = coverage,
               pull_output      = pull
          )
     })
     names(results) <- repos

     if (verbose) .refresh_data_repos_print(results, stale_days = stale_days)
     invisible(results)
}


#' @keywords internal
#' @noRd
.refresh_data_repos_coverage <- function(repo, root) {

     if (repo == "ees-cholera-mapping") {
          f <- file.path(root, "ees-cholera-mapping",
                         "data/cholera/who/awd/cholera_country_weekly.csv")
          if (!file.exists(f)) return(NULL)
          df <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE),
                         error = function(e) NULL)
          if (is.null(df) || !nrow(df)) return(NULL)
          date_col <- intersect(c("date_start", "date", "report_date"), names(df))[1L]
          dr <- if (!is.na(date_col)) {
               d <- suppressWarnings(as.Date(df[[date_col]]))
               d <- d[!is.na(d)]
               if (length(d)) range(d) else NA
          } else NA
          ctry_col <- intersect(c("iso_code", "country", "country_name"), names(df))[1L]
          n_ctry <- if (!is.na(ctry_col)) length(unique(df[[ctry_col]])) else NA_integer_
          return(list(file = basename(f), rows = nrow(df),
                      date_range = dr, n_countries = n_ctry))
     }

     if (repo == "jhu_cholera_data") {
          f <- file.path(root, "jhu_cholera_data", "Public_surveillance_dataset.rds")
          if (!file.exists(f)) {
               # Fall back: scan common locations
               cand <- list.files(file.path(root, "jhu_cholera_data"),
                                  pattern = "Public_surveillance_dataset\\.rds$",
                                  recursive = TRUE, full.names = TRUE)
               if (!length(cand)) return(NULL)
               f <- cand[1L]
          }
          df <- tryCatch(readRDS(f), error = function(e) NULL)
          if (is.null(df) || !nrow(df)) return(NULL)
          dr <- if ("epiweek" %in% names(df)) {
               yr <- suppressWarnings(as.integer(substr(df$epiweek, 1, 4)))
               yr <- yr[!is.na(yr)]
               if (length(yr)) range(yr) else NA
          } else NA
          loc_col <- intersect(c("location_name", "iso_code"), names(df))[1L]
          n_loc <- if (!is.na(loc_col)) length(unique(df[[loc_col]])) else NA_integer_
          return(list(file = basename(f), rows = nrow(df),
                      year_range = dr, n_locations = n_loc))
     }

     NULL
}


#' @keywords internal
#' @noRd
.refresh_data_repos_print <- function(results, stale_days) {
     cat("\n=== MOSAIC data repo refresh ===\n")
     cat(sprintf("(%s)\n\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")))

     for (r in results) {
          status <- if (isTRUE(r$ok)) "OK  " else "FAIL"
          stale  <- if (!is.null(r$days_stale) && !is.na(r$days_stale) &&
                        r$days_stale > stale_days) " [STALE]" else ""

          cat(sprintf("[%s] %s%s\n", status, r$repo, stale))
          cat(sprintf("       %s\n", r$description))

          if (!is.null(r$error)) {
               cat(sprintf("       error: %s\n\n", r$error)); next
          }

          if (!is.na(r$last_commit_date)) {
               cat(sprintf("       last commit:  %s  (%d days ago)\n",
                           substr(r$last_commit_date, 1, 10),
                           r$days_stale %||% NA_integer_))
               cat(sprintf("       \"%s\"\n", r$last_commit_msg))
          }

          if (!isTRUE(r$ok)) {
               cat("       pull output:\n")
               cat(paste0("         ", r$pull_output, collapse = "\n"), "\n")
          }

          cov <- r$coverage
          if (!is.null(cov)) {
               if (!is.null(cov$date_range) && length(cov$date_range) == 2L &&
                   !any(is.na(cov$date_range))) {
                    cat(sprintf("       coverage:     %s -> %s, %d rows, %d countries\n",
                                cov$date_range[1L], cov$date_range[2L],
                                cov$rows, cov$n_countries %||% NA_integer_))
               } else if (!is.null(cov$year_range) && length(cov$year_range) == 2L &&
                          !any(is.na(cov$year_range))) {
                    cat(sprintf("       coverage:     %d -> %d, %d rows, %d locations\n",
                                cov$year_range[1L], cov$year_range[2L],
                                cov$rows, cov$n_locations %||% NA_integer_))
               }
          }
          cat("\n")
     }

     n_ok    <- sum(vapply(results, function(r) isTRUE(r$ok), logical(1)))
     n_stale <- sum(vapply(results, function(r)
          !is.null(r$days_stale) && !is.na(r$days_stale) && r$days_stale > stale_days,
          logical(1)))
     cat(sprintf("Summary: %d/%d pulls succeeded, %d stale (>%d days).\n\n",
                 n_ok, length(results), n_stale, stale_days))
}
