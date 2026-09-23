#' Report the freshness of every MOSAIC data input and output
#'
#' Read-only audit across all three layers of the data pipeline: the external
#' scraper repos, the hand-maintained raw inputs, and the processed artifacts
#' the pipeline produces. Answers "what is stale?" in one call, and touches
#' nothing.
#'
#' The third layer is the one nothing else covers. \code{refresh_data_repos()}
#' reports source-repo staleness (and \emph{pulls} as a side effect);
#' \code{\link{check_mosaic_manual_inputs}} covers the manual raw inputs. Neither
#' looks at what the pipeline \emph{produced}, which is where vintage drift
#' actually accumulates -- a source can be current while the artifact derived
#' from it is two years old.
#'
#' @param root MOSAIC parent directory. Defaults to \code{get_paths()$ROOT}.
#' @param stale_days Age in days beyond which a processed artifact is flagged.
#'   Default 30.
#' @param repo_stale_days Age beyond which a source repo is flagged. Default 14,
#'   matching \code{\link{refresh_data_repos}}.
#' @param verbose Print the report.
#'
#' @return Invisibly, a list with elements \code{repos}, \code{manual},
#'   \code{outputs} and \code{summary}, each a \code{data.frame}.
#'
#' @section Read-only:
#' Unlike \code{\link{refresh_data_repos}}, this performs no \code{git pull} --
#' it reads \code{git log} only. Safe to run on a schedule or before deciding
#' whether a refresh is warranted.
#'
#' @section What "vintage spread" means:
#' The report counts how many distinct year-months the files in a directory were
#' written in. A healthy directory was produced by one build and shows 1-2. A
#' large spread means the directory is sediment from many partial runs and no
#' single coherent build produced it -- the files are individually valid but
#' mutually inconsistent in what data they saw.
#'
#' @seealso \code{\link{update_mosaic_data}},
#'   \code{\link{check_mosaic_manual_inputs}}, \code{\link{refresh_data_repos}}
#'
#' @examples
#' \dontrun{ check_mosaic_data_freshness("~/MOSAIC") }
#'
#' @importFrom glue glue
#' @export
check_mosaic_data_freshness <- function(root            = NULL,
                                        stale_days      = 30L,
                                        repo_stale_days = 14L,
                                        verbose         = TRUE) {

     if (is.null(root)) root <- MOSAIC::get_paths()$ROOT
     root <- normalizePath(path.expand(root), mustWork = TRUE)

     repos   <- .freshness_repos(root, repo_stale_days)
     manual  <- suppressWarnings(check_mosaic_manual_inputs(root, verbose = FALSE))
     outputs <- .freshness_outputs(root, stale_days)

     summary <- data.frame(
          layer   = c("source repos", "manual inputs", "processed outputs"),
          n       = c(nrow(repos), nrow(manual), nrow(outputs)),
          stale   = c(sum(repos$stale, na.rm = TRUE),
                      sum(manual$stale, na.rm = TRUE),
                      sum(outputs$stale, na.rm = TRUE)),
          stringsAsFactors = FALSE
     )

     if (verbose) .freshness_print(repos, manual, outputs, summary,
                                   stale_days, repo_stale_days)

     invisible(list(repos = repos, manual = manual,
                    outputs = outputs, summary = summary))
}


#' Source-repo freshness, WITHOUT pulling
#'
#' @keywords internal
#' @noRd
.freshness_repos <- function(root, stale_days) {
     repos <- c("ees-cholera-mapping", "jhu_cholera_data", "ai-cholera-data-mining",
                "enso-data", "open-meteo-pipeline")
     do.call(rbind, lapply(repos, function(r) {
          path <- file.path(root, r)
          if (!dir.exists(path)) {
               return(data.frame(repo = r, found = FALSE, last_commit = NA_character_,
                                 age_days = NA_integer_, stale = TRUE,
                                 dirty = NA, stringsAsFactors = FALSE))
          }
          d <- suppressWarnings(system2("git", c("-C", path, "log", "-1", "--format=%cI"),
                                        stdout = TRUE, stderr = FALSE))
          d <- if (length(d)) substr(d[1L], 1, 10) else NA_character_
          age <- if (!is.na(d)) as.integer(Sys.Date() - as.Date(d)) else NA_integer_
          st  <- suppressWarnings(system2("git", c("-C", path, "status", "--porcelain"),
                                          stdout = TRUE, stderr = FALSE))
          data.frame(repo = r, found = TRUE, last_commit = d, age_days = age,
                     # NA age = not a git repo, or no commits. That is a broken
                     # checkout, not a healthy one -- never report it as ok.
                     stale = is.na(age) || age > stale_days,
                     dirty = length(st) > 0L, stringsAsFactors = FALSE)
     }))
}


#' Processed-artifact freshness, by directory
#'
#' Walks the processed data tree and the model input layer. Reports per
#' directory: file count, newest and oldest write, age of the newest, and the
#' number of distinct year-months represented (the "vintage spread").
#'
#' @keywords internal
#' @noRd
.freshness_outputs <- function(root, stale_days) {

     targets <- c(
          file.path(root, "MOSAIC-data", "processed"),
          file.path(root, "MOSAIC-pkg", "model", "input")
     )

     # Intentionally static: download_country_DEM() skips files that already
     # exist (download_country_DEM.R:50) because terrain does not change.
     # Ageing these produces a permanent false alarm that trains readers to
     # ignore the panel.
     static_dirs <- c("DEM", "elevation")

     rows <- list()
     for (tgt in targets) {
          if (!dir.exists(tgt)) next
          # Label with the last two path segments so "processed" and "input" are
          # not silently dropped (the two targets otherwise print at different
          # levels: "MOSAIC-data" vs "model").
          label_root <- file.path(basename(dirname(tgt)), basename(tgt))

          dirs <- list.dirs(tgt, recursive = TRUE, full.names = TRUE)
          for (d in dirs) {
               f <- list.files(d, full.names = TRUE, recursive = FALSE)
               f <- f[!dir.exists(f)]
               # Drop documentation, keep data. "^README" still removes a data file
          # named README_country_counts.csv, so match only a bare README.*
          # stem. Dotfiles are already excluded by list.files().
          f <- f[!grepl("^README(\\.|$)|\\.md$", basename(f))]
               if (!length(f)) next

               mt <- file.info(f)$mtime
               mt <- mt[!is.na(mt)]
               if (!length(mt)) next
               newest <- max(mt); oldest <- min(mt)
               age <- as.integer(Sys.Date() - as.Date(newest, tz = Sys.timezone()))
               months <- length(unique(format(mt, "%Y-%m")))

               rel <- if (identical(d, tgt)) "" else substring(d, nchar(tgt) + 2L)
               is_static <- any(vapply(static_dirs, function(x)
                    identical(rel, x) || startsWith(rel, paste0(x, "/")), logical(1)))

               rows[[length(rows) + 1L]] <- data.frame(
                    area      = label_root,
                    # substring(), NOT sub(): `tgt` is a filesystem path and
                    # interpolating it into a regex breaks on roots containing
                    # regex metacharacters -- "Dropbox (Personal)" silently
                    # fails to strip, "[x" is a hard error.
                    directory = rel,
                    n_files   = length(f),
                    newest    = format(as.Date(newest, tz = Sys.timezone())),
                    oldest    = format(as.Date(oldest, tz = Sys.timezone())),
                    age_days  = age,
                    vintages  = months,
                    static    = is_static,
                    stale     = !is_static && age > stale_days,
                    stringsAsFactors = FALSE)
          }
     }
     if (!length(rows)) {
          return(data.frame(area = character(0), directory = character(0),
                            n_files = integer(0), newest = character(0),
                            oldest = character(0), age_days = integer(0),
                            vintages = integer(0), static = logical(0),
                            stale = logical(0),
                            stringsAsFactors = FALSE))
     }
     out <- do.call(rbind, rows)
     out[order(-out$age_days), , drop = FALSE]
}


#' @keywords internal
#' @noRd
.freshness_print <- function(repos, manual, outputs, summary, stale_days, repo_stale_days) {

     cat("\n", strrep("=", 78), "\n", sep = "")
     cat(sprintf("MOSAIC data freshness  (%s)\n", format(Sys.time(), "%Y-%m-%d %H:%M")))
     cat(strrep("=", 78), "\n", sep = "")

     cat(sprintf("\n-- Source repos (stale > %d d; read-only, no pull) --\n", repo_stale_days))
     for (i in seq_len(nrow(repos))) {
          r <- repos[i, ]
          flag <- if (!r$found) "MISSING" else if (r$stale) "STALE  " else "ok     "
          cat(sprintf("  [%s] %-26s %s  %s%s\n", flag, r$repo,
                      ifelse(is.na(r$last_commit), "-", r$last_commit),
                      ifelse(is.na(r$age_days), "", sprintf("(%d d)", r$age_days)),
                      ifelse(isTRUE(r$dirty), "  [uncommitted changes]", "")))
     }

     cat("\n-- Manual inputs --\n")
     for (i in seq_len(nrow(manual))) {
          m <- manual[i, ]
          flag <- if (!m$found) "MISSING" else if (m$stale) "STALE  " else "ok     "
          cat(sprintf("  [%s] %-32s %s%s\n", flag, m$source,
                      ifelse(is.na(m$modified), "-", m$modified),
                      ifelse(is.na(m$age_days), "", sprintf(" (%d d)", m$age_days))))
     }

     cat(sprintf("\n-- Processed outputs (stale > %d d) --\n", stale_days))
     st <- outputs[outputs$stale, , drop = FALSE]
     if (!nrow(st)) {
          cat("  all current\n")
     } else {
          cat(sprintf("  %-46s %5s %11s %5s\n", "DIRECTORY", "FILES", "NEWEST", "AGE"))
          for (i in seq_len(min(nrow(st), 20L))) {
               s <- st[i, ]
               cat(sprintf("  %-46s %5d %11s %4dd\n",
                           substr(file.path(s$area, s$directory), 1, 46),
                           s$n_files, s$newest, s$age_days))
          }
          if (nrow(st) > 20L) cat(sprintf("  ... and %d more\n", nrow(st) - 20L))
     }

     # Vintage spread: the signal that a directory is sediment, not a build.
     mix <- outputs[outputs$vintages >= 3L, , drop = FALSE]
     if (nrow(mix)) {
          cat("\n-- Mixed-vintage directories (files from >=3 distinct months) --\n")
          cat("   These were NOT produced by one coherent build.\n")
          mix <- mix[order(-mix$vintages), , drop = FALSE]
          for (i in seq_len(min(nrow(mix), 10L))) {
               s <- mix[i, ]
               cat(sprintf("  %-46s %2d vintages  %s -> %s\n",
                           substr(file.path(s$area, s$directory), 1, 46),
                           s$vintages, s$oldest, s$newest))
          }
          if (nrow(mix) > 10L) cat(sprintf("  ... and %d more\n", nrow(mix) - 10L))
     }

     cat("\n", strrep("-", 78), "\n", sep = "")
     for (i in seq_len(nrow(summary))) {
          cat(sprintf("  %-20s %3d checked, %3d stale\n",
                      summary$layer[i], summary$n[i], summary$stale[i]))
     }
     cat(strrep("=", 78), "\n\n", sep = "")
}
