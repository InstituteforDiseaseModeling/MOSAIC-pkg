#' Calibration diagnostics report (PDF)
#'
#' Renders the MOSAIC calibration-diagnostics report from
#' \code{3_results/calibration_diagnostics.json} (summary.json schema 2
#' companion). The report answers the three calibration questions, each on
#' its own page, and reads nothing but that file and the metric glossary
#' (\code{inst/extdata/metric_glossary.yml}): every number and display string
#' comes from the file, every word, formula and target template from the
#' glossary. No samples, predictions or configs are opened, so it can be
#' re-rendered from a finished run.
#'
#' @description
#' Pages:
#' \enumerate{
#'   \item \strong{Enough simulations?} The overall convergence status
#'     (green / yellow / red) over a table of its four checks (metric and
#'     formula, what it tells you, value, green / yellow target, status), then,
#'     below a "not part of the status" divider, the ESS growth, the budget and
#'     the ensemble size; the fit to data (cases and deaths, every scored day
#'     and, when they fit, tier 1-2 days); the reasons a check is not green, the
#'     flags (the fixed-budget flag is not shown: the run line says so) and the
#'     sampling details. On the right, the posterior ESS against the draws
#'     (log-log) over the best log-likelihood found so far.
#'   \item \strong{Parameters.} Monte Carlo error of each posterior mean
#'     (left) and prior-to-posterior KL divergence (right). Above 60 parameters
#'     the location-specific parameters are grouped by base name.
#'   \item \strong{Fit.} The fit table (and the worst-fitting locations of a
#'     multi-location run) over the weekly series: every complete week, the
#'     ensemble central line and its 50 and 95 percent predictive intervals
#'     from weekly-summed member trajectories, observed weekly totals on the
#'     scored weeks; unscored weeks shaded light grey, the burn-in darker.
#'   \item \strong{Predicted vs observed.} Weekly log-log scatter over the
#'     square-root-scale distributions of weekly counts, cases and deaths.
#'   \item \strong{Appendix.} What each metric shown means, generated from the
#'     glossary, and the metrics retired in 1.4.0.
#' }
#'
#' A file written before MOSAIC 1.5.1 lacks the display fields; they are
#' completed in memory from its numbers and the glossary
#' (\code{render_MOSAIC_figures()} also writes the completed file, with the
#' full-week series rebuilt from the run's ensemble). A panel whose inputs are
#' missing is replaced by a note rather than failing the report.
#'
#' @param diagnostics Path to \code{calibration_diagnostics.json}, or the list
#'   it parses to. Must carry \code{schema_version} \code{"2"}.
#' @param dir_output Directory the PDF is written to (created if needed).
#'   Default \code{NULL}: the directory holding \code{diagnostics} when it is a
#'   path.
#' @param file File name of the PDF. Default \code{"calibration_report.pdf"}.
#' @param verbose Logical; report the written path. Default \code{TRUE}.
#'
#' @return The path of the PDF, invisibly.
#'
#' @details The PDF is vector (base \code{pdf()} device, 11 x 8.5 in, WinAnsi
#'   encoding for the dashes; formulas and Greek letters in plotmath) and is
#'   written to a temporary file then renamed, so an interrupted render never
#'   leaves a truncated report. The notation writes the likelihood of draw
#'   \eqn{i} as \eqn{L_i} (no script l: the PDF fonts have no glyph for it).
#'   A run made before MOSAIC 1.4.0 has no
#'   \code{calibration_diagnostics.json}; its figures carry the notice from
#'   \code{.mosaic_calibration_report_legacy_notice()} in place of a status.
#'
#' @examples
#' \dontrun{
#' plot_model_calibration_report(
#'   file.path(dir_output, "3_results", "calibration_diagnostics.json"),
#'   dir_output = file.path(dir_output, "3_results", "figures"))
#' }
#'
#' @seealso \code{\link{calc_convergence_status}}
#' @export
plot_model_calibration_report <- function(diagnostics, dir_output = NULL,
                                          file = "calibration_report.pdf", verbose = TRUE) {

     gl <- .mosaic_glossary()
     d <- .mosaic_calrep_read(diagnostics, gl)
     if (is.null(dir_output)) {
          if (!is.character(diagnostics))
               stop("plot_model_calibration_report(): dir_output is required when diagnostics is a list.",
                    call. = FALSE)
          dir_output <- dirname(diagnostics)
     }
     if (length(file) != 1L || !is.character(file) || !nzchar(file))
          stop("plot_model_calibration_report(): file must be a single file name.", call. = FALSE)
     dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)
     out <- file.path(dir_output, file)

     # Reasons that do not fit page 1 continue on a page of their own. Page 1 is
     # laid out once on a throw-away device of the report's size and encoding
     # (drawing on the report device would start a page), so the page count is
     # known up front.
     dev_keep <- grDevices::dev.cur()
     grDevices::pdf(NULL, width = 11, height = 8.5, encoding = .mosaic_calrep_encoding())
     overflow <- tryCatch({
          grid::grid.newpage()
          st <- new.env()
          grid::grid.draw(.mosaic_calrep_page_sampling(d, gl, "", st))
          st$overflow %||% character(0)
     }, error = function(e) character(0))
     grDevices::dev.off()
     if (dev_keep > 1L && dev_keep %in% grDevices::dev.list()) grDevices::dev.set(dev_keep)
     tmp <- tempfile(pattern = "calibration_report_", tmpdir = dir_output, fileext = ".pdf")
     dev_before <- grDevices::dev.cur()
     grDevices::pdf(tmp, width = 11, height = 8.5, onefile = TRUE, useDingbats = FALSE,
                    encoding = .mosaic_calrep_encoding())
     dev_report <- grDevices::dev.cur()
     ok <- FALSE
     on.exit({
          if (dev_report %in% grDevices::dev.list()) grDevices::dev.off(dev_report)
          if (dev_before > 1L && dev_before %in% grDevices::dev.list()) grDevices::dev.set(dev_before)
          if (!ok && file.exists(tmp)) unlink(tmp)
     }, add = TRUE)
     pages <- c(list(function(foot) .mosaic_calrep_page_sampling(d, gl, foot)),
                if (length(overflow)) list(function(foot) .mosaic_calrep_page_reasons(d, gl, overflow, foot)),
                list(function(foot) .mosaic_calrep_page_parameters(d, gl, foot),
                     function(foot) .mosaic_calrep_page_fit(d, gl, foot),
                     function(foot) .mosaic_calrep_page_scatter(d, gl, foot)),
                .mosaic_calrep_appendix(gl, d))
     n_pages <- length(pages)
     foot_of <- function(i) .mosaic_fill(.mosaic_gl_text(gl, "footer"), title = .mosaic_calrep_title(d, gl),
                                         version = d$run$mosaic_version %|NA|% "?", date = format(Sys.Date()),
                                         page = i, pages = n_pages)
     for (i in seq_len(n_pages)) {
          # Open the page before building: measuring text while building the
          # grob would otherwise start a page on a fresh device.
          grid::grid.newpage()
          g <- tryCatch(suppressWarnings(suppressMessages(pages[[i]](foot_of(i)))),
                        error = function(e) .mosaic_calrep_failed_page(gl, i, conditionMessage(e), foot_of(i)))
          grid::grid.draw(g)
     }
     grDevices::dev.off(dev_report)
     if (!file.rename(tmp, out)) {
          if (!file.copy(tmp, out, overwrite = TRUE))
               stop("plot_model_calibration_report(): could not write ", out, call. = FALSE)
          unlink(tmp)
     }
     ok <- TRUE
     if (isTRUE(verbose)) message("Calibration report (", n_pages, " pages): ", out)
     invisible(out)
}

# The PDF encoding: WinAnsi carries the en and em dashes (the default
# ISOLatin1 prints them as hyphens); everything outside it is plotmath.
.mosaic_calrep_encoding <- function() "WinAnsi.enc"


# ---------------------------------------------------------------------------
# Legacy runs
# ---------------------------------------------------------------------------

#' Notice for a run assessed before MOSAIC 1.4.0
#'
#' A pre-1.4.0 run (summary.json without \code{schema_version} "2" or a
#' \code{convergence_status}, which includes 1.3.0 runs) has no convergence status on the 1.4.0 scale and no
#' \code{calibration_diagnostics.json}. Its figures show this notice in place
#' of a status (DEV_PLAN WS3.6; the run is relabelled, never backfilled).
#' @param dir_output Run directory (holding \code{3_results/summary.json}),
#'   its \code{3_results} directory, or the path of \code{summary.json}.
#' @return The notice text for a pre-1.4.0 run; \code{NULL} for a run that
#'   carries \code{convergence_status}.
#' @noRd
.mosaic_calibration_report_legacy_notice <- function(dir_output) {
     if (length(dir_output) != 1L || !is.character(dir_output))
          stop("legacy notice: dir_output must be a single path.", call. = FALSE)
     cand <- if (grepl("\\.json$", dir_output)) dir_output
             else c(file.path(dir_output, "3_results", "summary.json"), file.path(dir_output, "summary.json"))
     f <- cand[file.exists(cand)][1]
     if (is.na(f)) stop("legacy notice: no summary.json found under ", dir_output, call. = FALSE)
     s <- jsonlite::read_json(f)
     if (.mosaic_calrep_is_schema2(s)) return(NULL)
     .mosaic_calrep_legacy_text()
}

#' Whether a summary.json (parsed) is a MOSAIC 1.4.0 schema-2 summary
#'
#' Decided from \code{schema_version} (R8), with a 1.4.0 summary that lacks it
#' but carries a convergence status also counted. A 1.3.0 summary (it has
#' \code{posterior_weighting} but neither) is legacy (SCHEMA_v2 section 5.9).
#' @param s Parsed summary.json.
#' @return Logical scalar.
#' @noRd
.mosaic_calrep_is_schema2 <- function(s) {
     sv <- unlist(s$schema_version)
     if (length(sv) == 1L && !is.na(sv) && identical(as.character(sv), "2")) return(TRUE)
     st <- unlist(s$convergence_status)
     length(st) == 1L && is.character(st) && st %in% c("green", "yellow", "red")
}

#' The single legacy wording (report, render notice, dashboard, docs)
#'
#' From the glossary's \code{report$legacy} (shared word for word with the
#' dashboard resolver's LEGACY_SAMPLING_NOTE). \code{what = "pre140"}: a run
#' made before MOSAIC 1.4.0; \code{"missing"}: a 1.4.0 run whose
#' \code{calibration_diagnostics.json} is missing or unreadable. The
#' \code{*_detail} variants are the explanatory lines under it.
#' @noRd
.mosaic_calrep_legacy_text <- function(what = c("pre140", "missing", "pre140_detail", "missing_detail", "title"),
                                       gl = .mosaic_glossary()) {
     .mosaic_gl_text(gl, "legacy", match.arg(what))
}


# ---------------------------------------------------------------------------
# Reading and normalising calibration_diagnostics.json
# ---------------------------------------------------------------------------

.mosaic_calrep_read <- function(diagnostics, gl = .mosaic_glossary()) {
     src <- "diagnostics"
     if (is.character(diagnostics)) {
          if (length(diagnostics) != 1L || !file.exists(diagnostics))
               stop("plot_model_calibration_report(): file not found: ", paste(diagnostics, collapse = ", "),
                    call. = FALSE)
          src <- diagnostics
          diagnostics <- jsonlite::read_json(diagnostics, simplifyVector = FALSE)
     }
     if (!is.list(diagnostics))
          stop("plot_model_calibration_report(): diagnostics must be a path to calibration_diagnostics.json ",
               "or the parsed list.", call. = FALSE)
     sv <- diagnostics$schema_version
     sv <- if (length(sv) == 1L && !is.na(sv)) as.character(sv) else NA_character_
     if (!identical(sv, "2"))
          stop(sprintf(paste0("plot_model_calibration_report(): %s is not a schema 2 calibration_diagnostics.json ",
                              "(schema_version = %s). The report needs a MOSAIC >= 1.4.0 run; a pre-1.4.0 run is ",
                              "reported as: \"%s\"."),
                       src, if (is.na(sv)) "missing" else sprintf("\"%s\"", sv), .mosaic_calrep_legacy_text(gl = gl)),
               call. = FALSE)
     # A file from before 1.5.1 gets its display fields in memory.
     if (!identical(.mosaic_calrep_chr(diagnostics$schema_revision), .mosaic_cd_revision()))
          diagnostics <- .mosaic_calrep_complete(diagnostics, gl)
     .mosaic_calrep_normalise(diagnostics)
}

# Scalars: NULL / empty / non-finite -> NA.
.mosaic_calrep_num <- function(x) {
     x <- suppressWarnings(as.numeric(unlist(x)))
     if (length(x) != 1L || !is.finite(x)) NA_real_ else x
}
.mosaic_calrep_chr <- function(x) {
     x <- unlist(x)
     if (length(x) != 1L || is.na(x)) NA_character_ else as.character(x)
}
.mosaic_calrep_lgl <- function(x) {
     x <- suppressWarnings(as.logical(unlist(x)))
     if (length(x) != 1L) NA else x
}
.mosaic_calrep_chrv <- function(x) {
     x <- as.character(unlist(x))
     x[!is.na(x) & nzchar(x)]
}

# Array of records (list of lists, or a data frame) -> data frame with `cols`;
# missing columns and JSON nulls become NA. `lgl` columns are logical.
.mosaic_calrep_df <- function(x, cols, chr = character(0), lgl = character(0)) {
     typed <- function(cl, v) if (cl %in% chr) as.character(v) else if (cl %in% lgl) as.logical(v)
                              else suppressWarnings(as.numeric(v))
     empty <- as.data.frame(stats::setNames(lapply(cols, function(cl) typed(cl, character(0))), cols),
                            stringsAsFactors = FALSE)
     if (is.null(x) || !length(x)) return(empty)
     if (!is.data.frame(x)) {
          if (!is.list(x) || !all(vapply(x, is.list, logical(1)))) return(empty)
          x <- lapply(cols, function(cl) lapply(x, function(r) {
               v <- r[[cl]]
               if (is.null(v) || length(v) != 1L) NA else v
          }))
          names(x) <- cols
          x <- lapply(x, unlist)
          x <- as.data.frame(x, stringsAsFactors = FALSE)
     }
     for (cl in cols) {
          if (is.null(x[[cl]])) x[[cl]] <- NA
          x[[cl]] <- typed(cl, x[[cl]])
     }
     x[, cols, drop = FALSE]
}

.mosaic_calrep_normalise <- function(j) {
     n <- .mosaic_calrep_num; ch <- .mosaic_calrep_chr; lg <- .mosaic_calrep_lgl
     r <- j$run %||% list(); s <- j$sampling %||% list(); st <- j$status %||% list()
     f <- j$fit %||% list(); ms <- j$mce_summary %||% list(); ks <- j$kl_summary %||% list()
     locs <- .mosaic_calrep_chrv(r$locations %||% r$location)
     run <- list(mosaic_version = ch(r$mosaic_version), locations = locs,
                 n_locations = n(r$n_locations) %|NA|% length(locs),
                 mode = ch(r$mode), posterior_weighting = ch(r$posterior_weighting),
                 coarsening_alpha = n(r$coarsening_alpha), eta = n(r$eta), n_obs_scored = n(r$n_obs_scored),
                 burn_in_days = n(r$burn_in_days), n_iterations = n(r$n_iterations),
                 n_iter_ensemble = n(r$n_iter_ensemble), n_simulations = n(r$n_simulations),
                 min_ensemble_members = n(r$min_ensemble_members), max_ensemble_members = n(r$max_ensemble_members),
                 date_start = .mosaic_calrep_date1(r$date_start), date_stop = .mosaic_calrep_date1(r$date_stop))
     checks <- .mosaic_calrep_df(st$checks, c("check", "key", "label", "value", "threshold_green", "threshold_yellow",
                                              "colour", "reason", "operator", "operator_yellow", "value_display",
                                              "threshold_green_display", "threshold_yellow_display",
                                              "target_display", "target_note", "status_display"),
                                 chr = c("check", "key", "label", "colour", "reason", "operator", "operator_yellow",
                                         "value_display", "threshold_green_display", "threshold_yellow_display",
                                         "target_display", "target_note", "status_display"))
     status <- list(status = ch(st$convergence_status), label = ch(st$convergence_label),
                    headline = ch(st$headline_display),
                    reasons = .mosaic_calrep_chrv(st$reasons), flags = .mosaic_calrep_chrv(st$flags),
                    flags_text = .mosaic_calrep_chrv(st$flags_text),
                    flags_detail = .mosaic_calrep_df(st$flags_detail, c("id", "text", "display"),
                                                     chr = c("id", "text"), lgl = "display"),
                    checks = checks, stopping_rule_met = lg(st$stopping_rule_met))
     sampling <- list(
          ess_all = n(s$ess_all), ess_all_target = n(s$ess_all_target), ess_best = n(s$ess_best),
          ess_best_target = n(s$ess_best_target), best_subset_ratio = n(s$best_subset_ratio),
          best_subset_mass = n(s$best_subset_mass), ensemble_members = n(s$ensemble_members),
          ensemble_members_unique = n(s$ensemble_members_unique), ensemble_binding = ch(s$ensemble_binding),
          khat_tempered = n(s$khat_tempered), khat_status = ch(s$khat_status), eta_star = n(s$eta_star),
          eta_star_n_eff = n(s$eta_star_n_eff), flat_likelihood = lg(s$flat_likelihood),
          ess_growth_slope = n(s$ess_growth_slope), sims_used = n(s$sims_used),
          sims_projected_for_target = n(s$sims_projected_for_target), budget_exhausted = lg(s$budget_exhausted),
          sims_projected_for_green = n(s$sims_projected_for_green), projection_reliable = lg(s$projection_reliable),
          ess_untempered_frac = n(s$ess_untempered_frac), n_locations = n(s$n_locations),
          alpha_per_location = n(s$alpha_per_location),
          ess_path = .mosaic_calrep_df(s$ess_path, c("n", "ess_all")),
          loglik_path = .mosaic_calrep_df(s$loglik_path, c("n", "best_gap", "top100_gap")),
          details = .mosaic_calrep_df(s$details_display, c("key", "value"), chr = c("key", "value")))
     info_row <- function(x) {
          x <- x %||% list()
          list(key = ch(x$key), display = ch(x$display), sub_display = ch(x$sub_display),
               operator = ch(x$operator), target_value_display = ch(x$target_value_display),
               target_display = ch(x$target_display), target_note = ch(x$target_note),
               status = ch(x$status), status_display = ch(x$status_display))
     }
     info <- list(ess_growth = info_row(j$info$ess_growth), budget = info_row(j$info$budget),
                  ensemble_size = info_row(j$info$ensemble_size))
     params <- .mosaic_calrep_df(j$parameters, c("parameter", "base", "location", "prior_family", "mce", "kl",
                                                 "kl_band"),
                                 chr = c("parameter", "base", "location", "prior_family", "kl_band"))
     weekly_cols <- c("location", "metric", "week", "scored", "observed", "predicted", "lo50", "hi50", "lo95", "hi95")
     full_weeks <- length(f$weekly) && {
          w1 <- if (is.data.frame(f$weekly)) f$weekly[1, , drop = FALSE] else f$weekly[[1]]
          "scored" %in% names(w1)
     }
     weekly <- .mosaic_calrep_df(f$weekly, weekly_cols, chr = c("location", "metric", "week"), lgl = "scored")
     # A file without the full-week series (before 1.5.1) holds scored weeks only.
     if (!full_weeks) weekly$scored <- rep(TRUE, nrow(weekly))
     weekly$date <- .mosaic_calrep_date(weekly$week)
     tab <- .mosaic_calrep_df(f$table, c("metric", "subset", "r2", "bias", "cov50", "cov95", "wis", "n_scored"),
                              chr = c("metric", "subset"))
     tab$subset[is.na(tab$subset)] <- "scored"   # a file without the column has one row per channel
     tdisp <- .mosaic_calrep_df(f$table_display, c("metric", "subset", "label", "r2", "bias", "cov50", "cov95", "wis",
                                                   "n_scored"),
                                chr = c("metric", "subset", "label", "r2", "bias", "cov50", "cov95", "wis", "n_scored"))
     wm <- f$weekly_meta %||% list()
     fit <- list(table = tab, table_display = tdisp,
                 per_location = .mosaic_calrep_df(f$per_location, c("location", "metric", "r2", "bias", "cov95",
                                                                    "observed_total"),
                                                  chr = c("location", "metric")),
                 weekly = weekly[!is.na(weekly$date), , drop = FALSE],
                 weekly_source = if (!full_weeks) "scored_weeks" else ch(wm$source) %|NA|% "trajectories",
                 all_locations_id = ch(wm$all_locations_id))
     list(run = run, status = status, sampling = sampling, info = info, parameters = params,
          mce_summary = list(median = n(ms$median), p90 = n(ms$p90), max = n(ms$max),
                             worst = .mosaic_calrep_df(ms$worst, c("parameter", "mce"), chr = "parameter")),
          kl_summary = list(largely_prior = n(ks$largely_prior), partly_informed = n(ks$partly_informed),
                            data_informed = n(ks$data_informed), withheld = n(ks$withheld),
                            band_edges = suppressWarnings(as.numeric(unlist(ks$band_edges)))),
          fit = fit)
}

`%|NA|%` <- function(a, b) if (length(a) != 1L || is.na(a)) b else a

.mosaic_calrep_date1 <- function(x) {
     v <- .mosaic_calrep_date(x)
     if (length(v) != 1L) as.Date(NA) else v
}
.mosaic_calrep_date <- function(x) {
     x <- as.character(unlist(x))
     if (!length(x)) return(as.Date(character(0)))
     suppressWarnings(as.Date(substr(x, 1L, 10L), format = "%Y-%m-%d"))
}


# ---------------------------------------------------------------------------
# Constants, formatting and shared building blocks
# ---------------------------------------------------------------------------

#' Glossary keys of the metrics this report shows, in the order of the pages
#'
#' The appendix explains exactly these (and the retired metrics). Page 1: the
#' status, its checks (the keys in \code{status.checks[].key}), the flags when
#' one is displayed, the info rows (\code{info.*.key}) with what decides them,
#' the fit table and the sampling details; the plots; page 2 Monte Carlo error
#' and KL; page 3 the weekly series; page 4 the scatter and distribution.
#' @param d Normalised diagnostics.
#' @return Character vector of glossary keys.
#' @noRd
.mosaic_calrep_metric_keys <- function(d) {
     ck <- d$status$checks$key
     info <- vapply(d$info, function(x) x$key %|NA|% NA_character_, character(1))
     flags <- if (any(d$status$flags_detail$display %in% TRUE)) "convergence_flags"
     keys <- c("convergence_status", ck, flags, info, "ensemble_binding", "sims_projected_for_green",
               "projection_reliable", "r2", "bias", "coverage", "wis", "scored_days", "draws", "coarsening_alpha",
               "ess_all", "best_subset_mass", "n_iter_ensemble", "n_obs_scored", "burn_in_days", "best_loglik_path",
               "mce", "kl_divergence", "fit_time_series", "weekly_scatter", "weekly_distribution")
     unique(keys[!is.na(keys)])
}

# KL band edges (nats). The JSON's kl_summary$band_edges when present;
# otherwise the descriptive bands of the glossary's kl_divergence entry.
.mosaic_calrep_kl_edges <- function(d) {
     e <- d$kl_summary$band_edges
     if (length(e) == 2L && all(is.finite(e))) sort(e) else c(0.05, 0.5)
}

.mosaic_calrep_rag <- function() {
     s <- mosaic_colors("status")
     c(green = unname(s["pass"]), yellow = unname(s["warn"]), red = unname(s["fail"]), grey = "grey60")
}
.mosaic_calrep_col <- function(colour) {
     rag <- .mosaic_calrep_rag()
     out <- rag[match(colour, names(rag))]
     out[is.na(out)] <- rag[["grey"]]
     unname(out)
}

.mosaic_calrep_ink <- function() "grey15"
.mosaic_calrep_muted <- function() "grey50"
.mosaic_calrep_tgt <- function() "grey35"

.mosaic_calrep_fmt <- function(x, digits = 0) {
     ifelse(is.finite(x), formatC(x, format = "f", digits = digits, big.mark = ","), "n/a")
}
.mosaic_calrep_pct <- function(x, digits = 1) {
     ifelse(is.finite(x), paste0(formatC(100 * x, format = "f", digits = digits), "%"), "n/a")
}
#' Format a value so its rounding never lands on the other side of a threshold
#'
#' The shared display rule of the report and the run log (red team round 2,
#' RN2): \code{x * scale} is rounded to \code{digits} decimals; when that
#' rounding sits on the other side of (or on) any of \code{thresholds} from the
#' unrounded value, one more decimal is used, up to four more. So an MCE of
#' 0.1504 against the 15% yellow limit reads "15.04%", never "15.0%" red beside
#' "yellow <= 15%".
#' @param x Value(s).
#' @param thresholds Threshold(s) on the scale of \code{x}.
#' @param digits Decimals by default.
#' @param scale Multiplier applied before rounding (100 for percentages).
#' @param suffix Appended text (e.g. a percent sign).
#' @param big.mark Thousands separator.
#' @return Character vector ("n/a" for non-finite values).
#' @noRd
.mosaic_fmt_thr <- function(x, thresholds = numeric(0), digits = 0, scale = 1, suffix = "", big.mark = ",") {
     thr <- suppressWarnings(as.numeric(thresholds)); thr <- thr[is.finite(thr)] * scale
     vapply(suppressWarnings(as.numeric(x)), function(v) {
          if (!is.finite(v)) return("n/a")
          v <- v * scale
          d <- digits
          crosses <- function(d) {
               r <- round(v, d)
               any(vapply(thr, function(t) sign(r - t) != sign(v - t), logical(1)))
          }
          while (length(thr) && d < digits + 4 && crosses(d)) d <- d + 1
          paste0(formatC(round(v, d), format = "f", digits = d, big.mark = big.mark), suffix)
     }, character(1))
}

# A small percentage to two significant digits ("0.026%", "5.3%").
.mosaic_calrep_sig_pct <- function(x) {
     vapply(x, function(v) if (is.finite(v)) paste0(format(signif(100 * v, 2), scientific = FALSE,
                                                             drop0trailing = TRUE), "%") else "n/a", character(1))
}
.mosaic_calrep_sig <- function(x, digits = 2) {
     vapply(x, function(v) if (is.finite(v)) format(signif(v, digits), trim = TRUE) else "n/a", character(1))
}
# Threshold percentages: "10%" for whole percentages, else one decimal.
.mosaic_calrep_pct_thr <- function(x) {
     vapply(x, function(v) {
          if (!is.finite(v)) return("n/a")
          if (abs(100 * v - round(100 * v)) < 1e-9) sprintf("%d%%", as.integer(round(100 * v))) else sprintf("%.1f%%", 100 * v)
     }, character(1))
}

.mosaic_calrep_title <- function(d, gl) {
     L <- d$run$locations
     if (!length(L)) return(.mosaic_gl_text(gl, "title_none"))
     if (length(L) <= 4L) return(paste(L, collapse = ", "))
     .mosaic_fill(.mosaic_gl_text(gl, "title_many"), first = L[1], last = L[length(L)], n = length(L))
}

.mosaic_calrep_theme <- function(base = 9) {
     ggplot2::theme_minimal(base_size = base) +
          ggplot2::theme(
               panel.grid.minor = ggplot2::element_blank(),
               panel.grid.major = ggplot2::element_line(colour = "grey92", linewidth = 0.3),
               plot.title = ggplot2::element_text(face = "bold", size = 10, colour = .mosaic_calrep_ink()),
               plot.subtitle = ggplot2::element_text(colour = .mosaic_calrep_muted(), size = 7.5, lineheight = 1.1),
               axis.title = ggplot2::element_text(size = 8, colour = "grey30"),
               axis.text = ggplot2::element_text(colour = "grey35"),
               legend.text = ggplot2::element_text(size = 7.5),
               plot.title.position = "plot")
}

# The run line under the title (locations, mode, draws, window, scheme).
.mosaic_calrep_run_line <- function(d, gl) {
     r <- d$run
     rl <- .mosaic_gl_text(gl, "run_line")
     fmt <- .mosaic_calrep_fmt
     mode <- switch(r$mode %|NA|% "", fixed = rl$mode_fixed, auto = rl$mode_auto, rl$mode_unknown)
     n_loc <- r$n_locations
     paste(c(.mosaic_fill(if (isTRUE(n_loc == 1)) rl$locations_one else rl$locations_many, n = fmt(n_loc)),
             mode,
             # "1 valid of 40 simulated" when simulations failed (RN5)
             if (is.finite(r$n_simulations) && is.finite(d$sampling$sims_used) && r$n_simulations > d$sampling$sims_used)
                  .mosaic_fill(rl$draws_valid, valid = fmt(d$sampling$sims_used), attempted = fmt(r$n_simulations))
             else .mosaic_fill(rl$draws, n = fmt(d$sampling$sims_used)),
             if (!is.na(r$date_start) && !is.na(r$date_stop))
                  .mosaic_fill(rl$window, start = format(r$date_start), stop = format(r$date_stop)),
             if (!is.na(r$posterior_weighting)) .mosaic_fill(rl$scheme, scheme = r$posterior_weighting)),
           collapse = rl$separator)
}

.mosaic_calrep_header <- function(d, gl, section, foot) {
     patchwork::plot_annotation(
          title = .mosaic_fill(.mosaic_gl_text(gl, "title"), title = .mosaic_calrep_title(d, gl)),
          subtitle = paste0(.mosaic_gl_text(gl, "sections", section), "\n", .mosaic_calrep_run_line(d, gl)),
          caption = foot,
          theme = ggplot2::theme(
               plot.title = ggplot2::element_text(face = "bold", size = 15, margin = ggplot2::margin(b = 2)),
               plot.subtitle = ggplot2::element_text(colour = "grey35", size = 9, lineheight = 1.15,
                                                     margin = ggplot2::margin(b = 6)),
               plot.caption = ggplot2::element_text(colour = .mosaic_calrep_muted(), size = 7.5, hjust = 1),
               plot.margin = ggplot2::margin(14, 18, 10, 18)))
}

# A panel replaced by a note.
.mosaic_calrep_unavailable <- function(title, note) {
     ggplot2::ggplot() + ggplot2::theme_void() +
          ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste(strwrap(note, 70), collapse = "\n"),
                            size = 3, colour = .mosaic_calrep_muted(), fontface = "italic", lineheight = 1.2) +
          ggplot2::labs(title = title) +
          .mosaic_calrep_theme(9) +
          ggplot2::theme(panel.grid.major = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
                         axis.title = ggplot2::element_blank(),
                         panel.border = ggplot2::element_rect(colour = "grey90", fill = NA, linewidth = 0.4))
}

# Build a panel; on error, a note naming the error.
.mosaic_calrep_safe <- function(gl, title, expr) {
     tryCatch(expr, error = function(e)
          .mosaic_calrep_unavailable(title, .mosaic_fill(.mosaic_gl_text(gl, "unavailable"),
                                                         message = conditionMessage(e))))
}

.mosaic_calrep_failed_page <- function(gl, i, msg, foot) {
     grid::gTree(children = grid::gList(
          grid::textGrob(.mosaic_fill(.mosaic_gl_text(gl, "failed_page"), page = i), x = 0.04, y = 0.95, hjust = 0,
                         gp = grid::gpar(fontface = "bold", fontsize = 15)),
          grid::textGrob(paste(strwrap(msg, 150), collapse = "\n"), x = 0.04, y = 0.88, hjust = 0, vjust = 1,
                         gp = grid::gpar(fontsize = 9, col = "grey30")),
          grid::textGrob(foot, x = 0.965, y = 0.025, hjust = 1,
                         gp = grid::gpar(fontsize = 7.5, col = .mosaic_calrep_muted()))))
}

# A table anchored top-left under a bold title (natural size, filler below),
# as a patchwork element.
.mosaic_calrep_table <- function(df, title) {
     g <- .mosaic_calrep_table_grob(df, title)
     out <- patchwork::wrap_elements(full = g)
     attr(out, "height_pt") <- attr(g, "height_pt")
     out
}

# The same as a grid grob, with attribute "height_pt" (title plus table).
.mosaic_calrep_table_grob <- function(df, title, wrap = NULL) {
     # Long values wrap at a fixed width, so a table never outgrows its column
     # and is never clipped at the page or panel edge (RN1).
     for (cl in intersect(names(wrap), names(df)))
          df[[cl]] <- vapply(as.character(df[[cl]]), function(x)
               paste(strwrap(x, width = wrap[[cl]], exdent = 2), collapse = "\n"), character(1), USE.NAMES = FALSE)
     g <- gridExtra::tableGrob(df, rows = NULL, theme = gridExtra::ttheme_minimal(
          base_size = 7.4, padding = grid::unit(c(3, 2), "mm"),
          core = list(fg_params = list(hjust = 0, x = 0.03, col = .mosaic_calrep_ink())),
          colhead = list(fg_params = list(hjust = 0, x = 0.03, fontface = "bold", col = .mosaic_calrep_ink()))))
     ttl <- grid::textGrob(title, x = 0.02, hjust = 0, gp = grid::gpar(fontface = "bold", fontsize = 10))
     row <- gridExtra::arrangeGrob(g, grid::nullGrob(), ncol = 2,
                                   widths = grid::unit.c(sum(g$widths), grid::unit(1, "null")))
     out <- gridExtra::arrangeGrob(ttl, row, grid::nullGrob(), ncol = 1,
                                   heights = grid::unit.c(grid::unit(20, "pt"), sum(g$heights), grid::unit(1, "null")))
     attr(out, "height_pt") <- grid::convertHeight(sum(g$heights), "pt", valueOnly = TRUE) + 20
     out
}


# ---------------------------------------------------------------------------
# Rich text: glossary tokens -> plotmath
# ---------------------------------------------------------------------------

#' Rich text (glossary tokens in braces) as a plotmath expression
#' @param txt A string, or a language object (returned as is).
#' @param gl Glossary.
#' @return The string when it has no token, else an expression.
#' @noRd
.mosaic_calrep_rich <- function(txt, gl) {
     if (is.language(txt) || is.expression(txt)) return(txt)
     # the JSON's real minus sign is the WinAnsi minus of "-" on this device
     txt <- gsub("\u2212", "-", txt, fixed = TRUE)
     tk <- attr(gl, "tokens")
     if (!length(tk) || !grepl("\\{[A-Za-z0-9_]+\\}", txt)) return(txt)
     pat <- paste0("(", paste(sprintf("\\{%s\\}", names(tk)), collapse = "|"), ")")
     parts <- regmatches(txt, gregexpr(pat, txt), invert = NA)[[1]]
     parts <- parts[nzchar(parts)]
     if (!length(parts)) return(txt)
     pieces <- lapply(parts, function(p) {
          nm <- sub("^\\{(.*)\\}$", "\\1", p)
          if (grepl("^\\{.*\\}$", p) && nm %in% names(tk)) str2lang(tk[[nm]]$plotmath) else p
     })
     if (length(pieces) == 1L && is.character(pieces[[1]])) return(pieces[[1]])
     as.expression(Reduce(function(a, b) call("*", a, b), pieces))
}

# Formula lines of a glossary entry (one expression per line; none when "").
.mosaic_calrep_formula <- function(gl, key) {
     e <- gl[gl$key %in% key, , drop = FALSE]
     if (!nrow(e) || !nzchar(e$formula_plotmath[1])) return(list())
     as.list(str2expression(e$formula_plotmath[1]))
}

# ---------------------------------------------------------------------------
# Page 1: simulations and ensemble ESS
# ---------------------------------------------------------------------------

.mosaic_calrep_check <- function(d, id) {
     ck <- d$status$checks
     ck[ck$check %in% id, , drop = FALSE][1, , drop = FALSE]
}

# A target display "A / B" as its green and yellow halves (the report aligns
# them on the slash).
.mosaic_calrep_split_target <- function(x) {
     if (is.na(x) || !nzchar(x)) return(list(green = "", yellow = NULL))
     p <- strsplit(x, " / ", fixed = TRUE)[[1]]
     list(green = p[1], yellow = if (length(p) > 1L) paste(p[-1], collapse = " / ") else NULL)
}

#' What page 1 shows, as data
#'
#' Every string comes from the diagnostics (display fields) or the glossary;
#' nothing here is worded. The page-1 grob draws exactly this.
#' @param d Normalised diagnostics.
#' @param gl Glossary.
#' @return List: \code{headline}, \code{colour}, \code{status_rows},
#'   \code{info_rows}, \code{fit_rows}, \code{reasons}, \code{flags},
#'   \code{details}.
#' @noRd
.mosaic_calrep_page1_model <- function(d, gl) {
     ge <- function(key, f) { e <- gl[gl$key %in% key, , drop = FALSE]; if (nrow(e)) e[[f]][1] else NA_character_ }
     ck <- d$status$checks
     status_rows <- lapply(seq_len(nrow(ck)), function(i) {
          c1 <- ck[i, , drop = FALSE]
          k <- c1$key
          tg <- .mosaic_calrep_split_target(c1$target_display)
          list(name = ge(k, "short_label") %|NA|% (c1$label %|NA|% c1$check),
               formula = .mosaic_calrep_formula(gl, k),
               desc = .mosaic_fill(ge(k, "what_it_tells_you") %|NA|% "", green = c1$threshold_green_display %|NA|% "",
                                   yellow = c1$threshold_yellow_display %|NA|% ""),
               value = list(main = c1$value_display %|NA|% .mosaic_gl_text(gl, "values", "na")),
               target = list(type = "split", green = tg$green, yellow = tg$yellow,
                             note = if (is.na(c1$target_note)) NULL else c1$target_note),
               colour = c1$colour, status = c1$status_display)
     })
     info_rows <- lapply(d$info, function(x) {
          k <- x$key
          if (is.na(k)) return(NULL)
          target <- if (!is.na(x$operator)) {
               tg <- .mosaic_calrep_split_target(x$target_display)
               list(type = "split", green = tg$green, yellow = tg$yellow,
                    note = if (is.na(x$target_note)) NULL else x$target_note)
          } else list(type = "text", main = x$target_display %|NA|% "",
                      note = if (is.na(x$target_note)) NULL else x$target_note)
          list(name = ge(k, "short_label") %|NA|% k, formula = .mosaic_calrep_formula(gl, k),
               desc = .mosaic_fill(ge(k, "what_it_tells_you") %|NA|% "",
                                   min = .mosaic_calrep_fmt(d$run$min_ensemble_members),
                                   max = .mosaic_calrep_fmt(d$run$max_ensemble_members)),
               value = list(main = x$display %|NA|% "", sub = if (is.na(x$sub_display)) NULL else x$sub_display),
               target = target, flag = x$status_display %|NA|% "")
     })
     info_rows <- Filter(Negate(is.null), info_rows)
     fd <- d$status$flags_detail
     fl <- fd$text[fd$display %in% TRUE & !is.na(fd$text)]
     fl <- if (length(fl)) paste0(toupper(substr(fl, 1, 1)), substr(fl, 2, nchar(fl))) else character(0)
     sd_txt <- .mosaic_gl_text(gl, "sampling_details")
     det <- d$sampling$details
     details <- lapply(seq_len(nrow(det)), function(i)
          list(label = sd_txt[[det$key[i]]] %||% det$key[i], value = det$value[i]))
     list(headline = d$status$headline %|NA|% .mosaic_gl_text(gl, "status_unavailable"),
          colour = d$status$status, status_rows = status_rows, info_rows = info_rows,
          fit_rows = d$fit$table_display, reasons = d$status$reasons, flags = fl, details = details)
}

# Layout constants of the page-1 left column (points).
.mosaic_calrep_p1_style <- function() {
     list(ink = "grey15", muted = "grey45", faint = "grey60", formula = "grey30",
          head_bg = "#2C3E50", head_fg = "white", zebra = "#F3F5F7", rule = "#D5DAE0",
          fs_band = 10, fs_head = 7.2, fs_name = 7.4, fs_formula = 6.8, fs_body = 6.9,
          fs_value = 7.6, fs_small = 6.2, fs_text = 7.4, fs_section = 8, chip_w = 32, chip_h = 12,
          gap = 8, pad_x = 3.5, pad_y = 3.5, name_lh = 10, f_lh = 9, desc_lh = 8.6, sub_lh = 8, band_h = 21,
          head_h = 21, div_h = 16, fit_rh = 12.5, lh_r = 8.9, details_lh = 9.3,
          cw = c(metric = 86, desc = 112, value = 38, target = 54))
}

# The left column as a grob laid out at draw time (it then knows its size).
# `state` (an environment) receives the reason lines that did not fit.
.mosaic_calrep_left_column <- function(model, gl, state = NULL) {
     grid::gTree(model = model, gl = gl, state = state, cl = "mosaic_calrep_left")
}

#' Draw the page-1 left column (grid makeContent method)
#'
#' Lays out page 1's left column of \code{plot_model_calibration_report()}
#' (status table, info rows, fit table, reasons, flags, sampling details) at
#' draw time, when its size is known. Registered in NAMESPACE as the
#' \code{grid::makeContent} method of class \code{mosaic_calrep_left}
#' (\code{S3method(grid::makeContent, mosaic_calrep_left,
#' .mosaic_calrep_left_content)}), so it is not exported.
#' @param x The \code{mosaic_calrep_left} gTree.
#' @return The gTree with its children.
#' @noRd
.mosaic_calrep_left_content <- function(x) {
     m <- x$model; gl <- x$gl
     ST <- .mosaic_calrep_p1_style()
     rich <- function(t) .mosaic_calrep_rich(t, gl)
     tg <- function(label, xx, yy, fs, col = ST$ink, face = 1, hjust = 0)
          grid::textGrob(rich(label), x = xx, y = yy, hjust = hjust, vjust = 0.5,
                         gp = grid::gpar(fontsize = fs, col = col, fontface = face))
     w_pt <- function(label, fs, face = 1)
          grid::convertWidth(grid::grobWidth(grid::textGrob(rich(label), gp = grid::gpar(fontsize = fs, fontface = face))),
                             "pt", TRUE)
     wrap_pt <- function(txt, width, fs, face = 1) {
          words <- strsplit(txt, " ", fixed = TRUE)[[1]]
          out <- character(0); cur <- ""
          for (w in words) {
               cand <- if (nzchar(cur)) paste(cur, w) else w
               if (!nzchar(cur) || w_pt(cand, fs, face) <= width) cur <- cand
               else { out <- c(out, cur); cur <- w }
          }
          c(out, cur)
     }
     # same line count as a greedy wrap, at the narrowest width keeping it
     wrap_balanced <- function(txt, width, fs) {
          n <- length(wrap_pt(txt, width, fs))
          if (n < 2L) return(wrap_pt(txt, width, fs))
          lo <- width * 0.5; hi <- width
          for (k in 1:10) {
               mid <- (lo + hi) / 2
               if (length(wrap_pt(txt, mid, fs)) > n) lo <- mid else hi <- mid
          }
          wrap_pt(txt, hi, fs)
     }
     on_col <- function(colour) if (identical(colour, "yellow")) "grey10" else "white"
     W <- grid::convertWidth(grid::unit(1, "npc"), "pt", TRUE)
     H <- grid::convertHeight(grid::unit(1, "npc"), "pt", TRUE)
     X0 <- 2; W <- W - 16
     kids <- list()
     add <- function(g) kids[[length(kids) + 1L]] <<- g
     at <- function(yy) grid::unit(1, "npc") - grid::unit(yy, "pt")
     px <- function(xx) grid::unit(X0 + xx, "pt")
     ex <- function(v) if (is.language(v)) as.expression(v) else v
     y <- 2

     # overall band
     add(grid::rectGrob(px(0), at(y), width = grid::unit(W, "pt"), height = grid::unit(ST$band_h, "pt"),
                        just = c("left", "top"), gp = grid::gpar(fill = .mosaic_calrep_col(m$colour), col = NA)))
     fs_b <- ST$fs_band
     while (fs_b > 7.5 && w_pt(m$headline, fs_b, 2) > W - 14) fs_b <- fs_b - 0.25
     add(tg(m$headline, px(7), at(y + ST$band_h / 2), fs_b, col = on_col(m$colour), face = 2))
     y <- y + ST$band_h + ST$gap

     # status table
     tt <- .mosaic_gl_text(gl, "status_table")
     cw <- c(ST$cw, status = 0)
     cw[["status"]] <- W - sum(ST$cw)
     cx <- c(0, cumsum(cw))[1:5]; names(cx) <- names(cw)
     pxp <- ST$pad_x
     x_val <- cx[["value"]] + cw[["value"]] - pxp - 2
     x_slash <- cx[["target"]] + cw[["target"]] * 0.5
     x_stat <- cx[["status"]] + cw[["status"]] / 2
     hh <- ST$head_h
     add(grid::rectGrob(px(0), at(y), width = grid::unit(W, "pt"), height = grid::unit(hh, "pt"),
                        just = c("left", "top"), gp = grid::gpar(fill = ST$head_bg, col = NA)))
     hy <- y + hh / 2
     add(tg(tt$metric, px(cx[["metric"]] + pxp), at(hy), ST$fs_head, col = ST$head_fg, face = 2))
     add(tg(tt$what, px(cx[["desc"]] + pxp), at(hy), ST$fs_head, col = ST$head_fg, face = 2))
     add(tg(tt$value, px(x_val), at(hy), ST$fs_head, col = ST$head_fg, face = 2, hjust = 1))
     add(tg(tt$target, px(x_slash), at(hy - 3.8), ST$fs_head, col = ST$head_fg, face = 2, hjust = 0.5))
     add(tg(tt$target_sub, px(x_slash), at(hy + 4.6), ST$fs_small, col = "grey80", hjust = 0.5))
     add(tg(tt$status, px(x_stat), at(hy), ST$fs_head, col = ST$head_fg, face = 2, hjust = 0.5))
     y <- y + hh

     prep <- function(r) {
          r$desc_l <- if (nzchar(r$desc)) wrap_balanced(r$desc, cw[["desc"]] - 2 * pxp, ST$fs_body) else character(0)
          r$h_metric <- ST$name_lh + ST$f_lh * length(r$formula)
          r$h_desc <- ST$desc_lh * length(r$desc_l)
          r$h_val <- ST$name_lh + if (!is.null(r$value$sub)) ST$sub_lh else 0
          r$h_tgt <- ST$name_lh + if (!is.null(r$target$note)) ST$sub_lh else 0
          r$h <- max(r$h_metric, r$h_desc, r$h_val, r$h_tgt, ST$chip_h)
          r
     }
     rows <- lapply(m$status_rows, prep)
     irows <- lapply(m$info_rows, prep)
     rh_s <- if (length(rows)) max(vapply(rows, `[[`, 0, "h")) + 2 * ST$pad_y else 0
     rh_i <- if (length(irows)) max(vapply(irows, `[[`, 0, "h")) + 2 * ST$pad_y else 0

     draw_row <- function(r, i, rh, info = FALSE) {
          if (i %% 2 == 0) add(grid::rectGrob(px(0), at(y), width = grid::unit(W, "pt"), height = grid::unit(rh, "pt"),
                                              just = c("left", "top"), gp = grid::gpar(fill = ST$zebra, col = NA)))
          mid <- y + rh / 2
          yy <- mid - r$h_metric / 2
          add(tg(r$name, px(cx[["metric"]] + pxp), at(yy + ST$name_lh / 2), ST$fs_name, face = 2))
          yy <- yy + ST$name_lh
          for (fl in r$formula) {
               fs_f <- ST$fs_formula
               while (fs_f > 5.6 && w_pt(as.expression(fl), fs_f) > cw[["metric"]] - 2 * pxp) fs_f <- fs_f - 0.2
               add(tg(as.expression(fl), px(cx[["metric"]] + pxp), at(yy + ST$f_lh / 2), fs_f, col = ST$formula))
               yy <- yy + ST$f_lh
          }
          yy <- mid - r$h_desc / 2
          for (l in r$desc_l) {
               add(tg(l, px(cx[["desc"]] + pxp), at(yy + ST$desc_lh / 2), ST$fs_body, col = "grey25"))
               yy <- yy + ST$desc_lh
          }
          yy <- mid - r$h_val / 2
          add(tg(r$value$main, px(x_val), at(yy + ST$name_lh / 2), ST$fs_value, face = 2, hjust = 1))
          if (!is.null(r$value$sub))
               add(tg(r$value$sub, px(x_val), at(yy + ST$name_lh + ST$sub_lh / 2), ST$fs_small, col = ST$muted, hjust = 1))
          yy <- mid - r$h_tgt / 2
          ty <- yy + ST$name_lh / 2
          t <- r$target
          if (identical(t$type, "split")) {
               if (is.null(t$yellow)) {
                    add(tg(t$green, px(x_slash), at(ty), ST$fs_body, hjust = 0.5))
               } else {
                    add(tg(t$green, px(x_slash - 4), at(ty), ST$fs_body, hjust = 1))
                    add(tg("/", px(x_slash), at(ty), ST$fs_body, col = ST$faint, hjust = 0.5))
                    add(tg(t$yellow, px(x_slash + 2), at(ty), ST$fs_body, hjust = 0))
               }
          } else add(tg(t$main, px(x_slash), at(ty), ST$fs_body, hjust = 0.5))
          if (!is.null(t$note))
               add(tg(t$note, px(x_slash), at(yy + ST$name_lh + ST$sub_lh / 2), ST$fs_small, col = ST$muted, hjust = 0.5))
          if (!info) {
               add(grid::roundrectGrob(px(x_stat), at(mid), width = grid::unit(ST$chip_w, "pt"),
                                       height = grid::unit(ST$chip_h, "pt"), r = grid::unit(2, "pt"),
                                       gp = grid::gpar(fill = .mosaic_calrep_col(r$colour), col = NA)))
               add(tg(r$status %|NA|% "", px(x_stat), at(mid), 6.3, col = on_col(r$colour), face = 2, hjust = 0.5))
          } else add(tg(r$flag, px(x_stat), at(mid), ST$fs_body, col = "grey40", hjust = 0.5))
          add(grid::segmentsGrob(px(0), at(y + rh), px(W), at(y + rh), gp = grid::gpar(col = ST$rule, lwd = 0.5)))
          y <<- y + rh
     }
     if (!length(rows)) {
          add(tg(tt$no_checks, px(4), at(y + 9), ST$fs_body, col = ST$muted, face = 3))
          y <- y + 18
     }
     for (i in seq_along(rows)) draw_row(rows[[i]], i, rh_s)
     if (length(irows)) {
          dv <- ST$div_h
          lw <- w_pt(tt$divider, ST$fs_small, 3) + 10
          for (seg in list(c(0, W / 2 - lw / 2), c(W / 2 + lw / 2, W)))
               add(grid::segmentsGrob(px(seg[1]), at(y + dv / 2), px(seg[2]), at(y + dv / 2),
                                      gp = grid::gpar(col = "grey60", lty = "11", lwd = 0.8)))
          add(tg(tt$divider, px(W / 2), at(y + dv / 2), ST$fs_small, col = ST$muted, face = 3, hjust = 0.5))
          add(grid::segmentsGrob(px(0), at(y + dv), px(W), at(y + dv), gp = grid::gpar(col = ST$rule, lwd = 0.5)))
          y <- y + dv
          for (i in seq_along(irows)) draw_row(irows[[i]], i, rh_i, info = TRUE)
     }
     add(grid::segmentsGrob(px(0), at(y), px(W), at(y), gp = grid::gpar(col = "grey40", lwd = 0.8)))
     y <- y + ST$gap

     # reasons, flags and sampling details are measured first, so the fit table
     # takes its tier 1-2 rows only when everything still fits
     bullet_groups <- function(txts, fs) lapply(txts, function(r) {
          w <- wrap_pt(r, W - 14, fs)
          paste0(c("\u2022 ", rep("   ", length(w) - 1L)), w)
     })
     bullets <- function(txts, fs) unlist(bullet_groups(txts, fs))
     reason_groups <- bullet_groups(m$reasons, ST$fs_text - 0.3)
     reasons <- unlist(reason_groups)
     flag_l <- bullets(m$flags, ST$fs_text - 0.5)
     lh_r <- ST$lh_r
     det <- m$details
     det_h <- if (length(det)) 14 + ceiling(length(det) / 2) * ST$details_lh + 2 else 0
     flag_block_h <- if (length(flag_l)) 12 + length(flag_l) * (lh_r - 0.4) + ST$gap else 0
     reasons_h <- if (length(reasons)) 13 + length(reasons) * lh_r + ST$gap else 0

     ft <- m$fit_rows
     fit_h <- function(n) 13 + ST$head_h + n * ST$fit_rh + ST$gap
     if (any(ft$subset == "tier12") &&
         H - y - fit_h(nrow(ft)) - reasons_h - flag_block_h - det_h - 4 < 0)
          ft <- ft[ft$subset != "tier12", , drop = FALSE]
     if (nrow(ft)) {
          fx_txt <- .mosaic_gl_text(gl, "fit_table")
          add(tg(fx_txt$caption, px(0), at(y + 4.5), ST$fs_section, face = 2))
          add(tg(fx_txt$caption_note, px(w_pt(fx_txt$caption, ST$fs_section, 2) + 4), at(y + 4.5),
                 ST$fs_small + 0.6, col = ST$muted))
          y <- y + 13
          fcols <- fx_txt$columns
          fw <- c(W * 0.235, rep((W - W * 0.235) / 6, 6))
          fx <- c(0, cumsum(fw))
          xr <- fx[-1] - pxp - 4
          add(grid::rectGrob(px(0), at(y), width = grid::unit(W, "pt"), height = grid::unit(ST$head_h, "pt"),
                             just = c("left", "top"), gp = grid::gpar(fill = ST$head_bg, col = NA)))
          hy <- y + ST$head_h / 2
          for (ci in seq_along(fcols)) {
               hx <- if (ci == 1) fx[1] + pxp else xr[ci]
               hj <- if (ci == 1) 0 else 1
               if (!nzchar(fcols[[ci]]$sub %||% "")) add(tg(fcols[[ci]]$head, px(hx), at(hy), ST$fs_head,
                                                            col = ST$head_fg, face = 2, hjust = hj))
               else {
                    add(tg(fcols[[ci]]$head, px(hx), at(hy - 3.8), ST$fs_head, col = ST$head_fg, face = 2, hjust = hj))
                    add(tg(fcols[[ci]]$sub, px(hx), at(hy + 4.6), ST$fs_small, col = "grey80", hjust = hj))
               }
          }
          y <- y + ST$head_h
          for (i in seq_len(nrow(ft))) {
               if (i %% 2 == 0) add(grid::rectGrob(px(0), at(y), width = grid::unit(W, "pt"),
                                                   height = grid::unit(ST$fit_rh, "pt"), just = c("left", "top"),
                                                   gp = grid::gpar(fill = ST$zebra, col = NA)))
               my <- y + ST$fit_rh / 2
               vals <- unlist(ft[i, c("label", "r2", "bias", "cov50", "cov95", "wis", "n_scored")])
               for (ci in seq_along(vals))
                    add(tg(vals[[ci]], px(if (ci == 1) fx[1] + pxp else xr[ci]), at(my), ST$fs_body + 0.2,
                           face = if (ci == 1) 2 else 1, hjust = if (ci == 1) 0 else 1))
               last <- i == nrow(ft)
               add(grid::segmentsGrob(px(0), at(y + ST$fit_rh), px(W), at(y + ST$fit_rh),
                                      gp = grid::gpar(col = if (last) "grey40" else ST$rule, lwd = if (last) 0.8 else 0.5)))
               y <- y + ST$fit_rh
          }
          y <- y + ST$gap
     }

     overflow <- character(0)
     if (length(reasons)) {
          rs <- .mosaic_gl_text(gl, "reasons")
          room <- H - y - det_h - flag_block_h - 4
          max_lines <- max(2L, floor((room - 13 - ST$gap) / lh_r))
          cont <- rep(FALSE, length(reasons))
          if (length(reasons) > max_lines) {
               # Whole reasons only: the ones that fit stay, the rest continue on
               # the next page. Only a single reason longer than the room is cut.
               fits <- sum(cumsum(lengths(reason_groups)) <= max_lines - 1L)
               keep_n <- if (fits > 0L) sum(lengths(reason_groups)[seq_len(fits)]) else max_lines - 1L
               overflow <- reasons[-seq_len(keep_n)]
               reasons <- c(reasons[seq_len(keep_n)], paste0("   ", rs$continued))
               cont <- c(rep(FALSE, keep_n), TRUE)
          }
          add(tg(rs$heading, px(0), at(y + 4.5), ST$fs_section, face = 2))
          y <- y + 13
          for (k in seq_along(reasons)) {
               add(tg(reasons[k], px(4), at(y + lh_r / 2), ST$fs_text - 0.3, col = if (cont[k]) ST$muted else ST$ink,
                      face = if (cont[k]) 3 else 1))
               y <- y + lh_r
          }
          y <- y + ST$gap
     }
     if (!is.null(x$state)) assign("overflow", overflow, envir = x$state)
     if (length(flag_l)) {
          add(tg(.mosaic_gl_text(gl, "flags", "heading"), px(0), at(y + 4.5), ST$fs_section, face = 2))
          y <- y + 12
          for (l in flag_l) {
               add(tg(l, px(4), at(y + (lh_r - 0.4) / 2), ST$fs_text - 0.5, col = "grey30"))
               y <- y + lh_r - 0.4
          }
          y <- y + ST$gap
     }
     if (length(det)) {
          add(tg(.mosaic_gl_text(gl, "sampling_details", "heading"), px(0), at(y + 5), ST$fs_section, face = 2))
          add(grid::segmentsGrob(px(0), at(y + 11), px(W), at(y + 11), gp = grid::gpar(col = ST$rule, lwd = 0.5)))
          half <- ceiling(length(det) / 2)
          cols <- list(det[seq_len(half)], det[-seq_len(half)])
          cols <- cols[lengths(cols) > 0]
          lab_w <- vapply(cols, function(cl) max(vapply(cl, function(it) w_pt(it$label, ST$fs_body), 0)), 0)
          val_w <- vapply(cols, function(cl) max(vapply(cl, function(it) w_pt(it$value, ST$fs_body), 0)), 0)
          gap <- 6
          xs <- c(0, if (length(cols) > 1L) max(lab_w[1] + gap + val_w[1] + 14, W - (lab_w[2] + gap + val_w[2])))
          for (ci in seq_along(cols)) for (ri in seq_along(cols[[ci]])) {
               it <- cols[[ci]][[ri]]
               yy <- y + 14 + (ri - 1) * ST$details_lh + ST$details_lh / 2
               add(tg(it$label, px(xs[ci]), at(yy), ST$fs_body, col = ST$muted))
               add(tg(it$value, px(xs[ci] + lab_w[ci] + gap), at(yy), ST$fs_body))
          }
     }
     grid::setChildren(x, do.call(grid::gList, kids))
}

# ESS and log-likelihood plots share their x axis.
.mosaic_calrep_path_xlim <- function(d) {
     n <- c(d$sampling$ess_path$n, d$sampling$loglik_path$n)
     n <- n[is.finite(n) & n > 0]
     if (!length(n)) return(c(1, 10))
     r <- range(n)
     if (r[1] == r[2]) r <- r * c(0.7, 1.4)
     r
}

.mosaic_calrep_ess_plot <- function(d, gl) {
     tx <- .mosaic_gl_text(gl, "ess_plot")
     p <- d$sampling$ess_path
     p <- p[is.finite(p$n) & is.finite(p$ess_all) & p$n > 0 & p$ess_all > 0, , drop = FALSE]
     if (!nrow(p)) return(.mosaic_calrep_unavailable(tx$title, tx$empty))
     xl <- .mosaic_calrep_path_xlim(d)
     tgt <- d$sampling$ess_all_target
     col <- .mosaic_calrep_col(.mosaic_calrep_check(d, "ess")$colour)
     slope <- d$sampling$ess_growth_slope
     yr <- range(c(p$ess_all, if (is.finite(tgt)) tgt))
     g <- ggplot2::ggplot(p, ggplot2::aes(.data$n, .data$ess_all))
     for (f in c(0.001, 0.01, 0.1)) {
          x0 <- max(xl[1], 1 / f)
          if (x0 < xl[2]) g <- g + ggplot2::annotate("segment", x = x0, xend = xl[2], y = f * x0, yend = f * xl[2],
                                                     colour = "grey80", linetype = "dotted")
     }
     if (is.finite(tgt)) {
          g <- g + ggplot2::geom_hline(yintercept = tgt, linetype = "dashed", colour = .mosaic_calrep_tgt(), linewidth = 0.4) +
               ggplot2::annotate("text", x = xl[1], y = tgt, label = .mosaic_fill(tx$target, n = .mosaic_calrep_fmt(tgt)),
                                 hjust = 0, vjust = -0.5, size = 2.7, colour = .mosaic_calrep_tgt())
     }
     if (nrow(p) > 1L) g <- g + ggplot2::geom_line(colour = col, linewidth = 0.8)
     g + ggplot2::geom_point(data = p[nrow(p), , drop = FALSE], colour = col, size = 2) +
          ggplot2::scale_x_log10(labels = scales::label_comma(), limits = xl) +
          ggplot2::scale_y_log10(labels = scales::label_comma(accuracy = 1), breaks = 10^(0:7)) +
          ggplot2::coord_cartesian(ylim = c(min(1, yr[1]), yr[2] * 1.6)) +
          ggplot2::labs(title = tx$title, x = NULL, y = tx$y,
                        subtitle = paste0(if (is.finite(slope)) .mosaic_fill(tx$subtitle_slope, slope = sprintf("%.2f", slope))
                                          else tx$subtitle_no_slope, "\n", tx$guides)) +
          .mosaic_calrep_theme(9) + ggplot2::theme(axis.text.x = ggplot2::element_blank())
}

#' Breaks of the signed-log (pseudo-log) log-likelihood axis
#'
#' Below 100 units, the 1-2-5 steps of every decade (no crowding next to 0);
#' beyond, every decade and its 3x step while there are at most six decades,
#' else every decade, so no long stretch of the axis is unlabelled.
#' @param lo The lowest gap (negative).
#' @return Breaks, 0 first.
#' @noRd
.mosaic_calrep_signed_log_breaks <- function(lo) {
     if (-lo < 100) {
          dec <- 10^(-1:max(0, ceiling(log10(-lo))))
          brk <- -c(0, sort(unique(as.vector(outer(c(1, 2, 5), dec)))))
          brk <- brk[brk >= 1.5 * lo | brk == max(brk[brk < lo], na.rm = TRUE)]
          return(brk[brk == 0 | abs(brk) >= abs(lo) / 10])
     }
     nd <- ceiling(log10(-lo))
     steps <- if (nd <= 6) c(1, 3) else 1
     brk <- -c(0, sort(unique(as.vector(outer(steps, 10^(1:nd))))))
     brk[brk >= lo | brk == max(brk[brk < lo])]
}

.mosaic_calrep_loglik_plot <- function(d, gl) {
     tx <- .mosaic_gl_text(gl, "loglik_plot")
     p <- d$sampling$loglik_path
     if (!nrow(p)) return(.mosaic_calrep_unavailable(tx$title, tx$empty))
     ll <- rbind(data.frame(n = p$n, gap = p$best_gap, what = tx$best),
                 data.frame(n = p$n, gap = p$top100_gap, what = tx$top100))
     ll <- ll[is.finite(ll$n) & ll$n > 0 & is.finite(ll$gap), , drop = FALSE]
     if (!nrow(ll)) return(.mosaic_calrep_unavailable(tx$title, tx$empty))
     ll$what <- factor(ll$what, levels = c(tx$best, tx$top100))
     eta <- d$run$eta
     lo <- min(ll$gap, -1)
     g <- ggplot2::ggplot(ll, ggplot2::aes(.data$n, .data$gap, colour = .data$what)) +
          ggplot2::geom_hline(yintercept = 0, colour = .mosaic_calrep_tgt(), linewidth = 0.3)
     g <- g + (if (nrow(p) > 1L) ggplot2::geom_line(linewidth = 0.7) else ggplot2::geom_point(size = 1.8))
     g + ggplot2::expand_limits(y = c(0, min(-1, lo))) +
          ggplot2::scale_colour_manual(values = stats::setNames(c("grey20", "grey60"), c(tx$best, tx$top100)),
                                       name = NULL, drop = TRUE) +
          ggplot2::scale_x_log10(labels = scales::label_comma(), limits = .mosaic_calrep_path_xlim(d)) +
          ggplot2::scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 1, base = 10),
                                      breaks = .mosaic_calrep_signed_log_breaks(lo), labels = scales::label_comma()) +
          ggplot2::labs(title = tx$title,
                        subtitle = paste0(tx$subtitle,
                                          if (is.finite(eta) && eta > 0)
                                               paste0("\n", .mosaic_fill(tx$eta_note, units = .mosaic_calrep_fmt(1 / eta)))
                                          else ""),
                        x = tx$x, y = tx$y) +
          .mosaic_calrep_theme(9) + ggplot2::theme(legend.position = "bottom")
}

# The reasons that did not fit page 1, in full.
.mosaic_calrep_page_reasons <- function(d, gl, lines, foot) {
     kids <- list(
          grid::textGrob(.mosaic_fill(.mosaic_gl_text(gl, "title"), title = .mosaic_calrep_title(d, gl)),
                         x = 0.04, y = 0.955, hjust = 0, gp = grid::gpar(fontface = "bold", fontsize = 15)),
          grid::textGrob(.mosaic_gl_text(gl, "sections", "reasons"), x = 0.04, y = 0.915,
                         hjust = 0, gp = grid::gpar(fontsize = 9, col = "grey35")),
          grid::textGrob(foot, x = 0.965, y = 0.025, hjust = 1,
                         gp = grid::gpar(fontsize = 7.5, col = .mosaic_calrep_muted())))
     yy <- 0.87
     for (l in lines) {
          kids[[length(kids) + 1L]] <- grid::textGrob(l, x = 0.05, y = yy, hjust = 0,
                                                     gp = grid::gpar(fontsize = 8.5, col = .mosaic_calrep_ink()))
          yy <- yy - 0.022
     }
     grid::gTree(children = do.call(grid::gList, kids))
}

# Page 1: the left column (1.14) beside the two plots (1.18).
.mosaic_calrep_page_sampling <- function(d, gl, foot, state = NULL) {
     left <- patchwork::wrap_elements(full = .mosaic_calrep_left_column(.mosaic_calrep_page1_model(d, gl), gl, state))
     pe <- .mosaic_calrep_safe(gl, .mosaic_gl_text(gl, "ess_plot", "title"), .mosaic_calrep_ess_plot(d, gl))
     pl <- .mosaic_calrep_safe(gl, .mosaic_gl_text(gl, "loglik_plot", "title"), .mosaic_calrep_loglik_plot(d, gl))
     right <- (pe / pl) + patchwork::plot_layout(heights = c(1.25, 1))
     p <- (left | right) + patchwork::plot_layout(widths = c(1.14, 1.18)) +
          .mosaic_calrep_header(d, gl, "sampling", foot)
     patchwork::patchworkGrob(p)
}


# ---------------------------------------------------------------------------
# Page 2: parameters (Monte Carlo error | KL)
# ---------------------------------------------------------------------------

.mosaic_calrep_group_params <- function(pp, many) {
     if (!many) {
          pp$g <- pp$parameter; pp$n <- 1L
          pp$mce_lo <- pp$mce; pp$kl_lo <- pp$kl; pp$kl_hi <- pp$kl
          return(pp)
     }
     base <- ifelse(is.na(pp$base), pp$parameter, pp$base)
     do.call(rbind, lapply(split(pp, base), function(x) {
          fin <- function(v) v[is.finite(v)]
          m <- fin(x$mce); k <- fin(x$kl)
          bands <- x$kl_band[is.finite(x$kl)]
          data.frame(g = if (is.na(x$base[1])) x$parameter[1] else x$base[1], prior_family = x$prior_family[1],
                     mce = if (length(m)) max(m) else NA_real_, mce_lo = if (length(m)) min(m) else NA_real_,
                     kl = if (length(k)) stats::median(k) else NA_real_,
                     kl_lo = if (length(k)) min(k) else NA_real_, kl_hi = if (length(k)) max(k) else NA_real_,
                     kl_band = if (length(k)) bands[which.min(abs(x$kl[is.finite(x$kl)] - stats::median(k)))] else NA_character_,
                     n = nrow(x), stringsAsFactors = FALSE)
     }))
}

.mosaic_calrep_label_size <- function(n) if (n > 55) 6 else if (n > 40) 7 else 8.5

.mosaic_calrep_mce_plot <- function(d, gl, g, many) {
     tx <- .mosaic_gl_text(gl, "parameters_page")
     g <- g[is.finite(g$mce), , drop = FALSE]
     if (!nrow(g)) return(.mosaic_calrep_unavailable(tx$mce_title, tx$mce_empty))
     ck <- .mosaic_calrep_check(d, "mce")
     thr_g <- ck$threshold_green; thr_y <- ck$threshold_yellow
     g$band <- if (is.finite(thr_g) && is.finite(thr_y))
          ifelse(g$mce <= thr_g, "green", ifelse(g$mce <= thr_y, "yellow", "red")) else "grey"
     g$lab <- if (many) vapply(seq_len(nrow(g)), function(i) .mosaic_fill(tx$label_many, name = g$g[i], n = g$n[i]), "")
              else g$g
     g$lab <- factor(g$lab, levels = unique(g$lab[order(g$mce)]))
     vl <- c(thr_g, thr_y); vl <- vl[is.finite(vl)]
     p <- ggplot2::ggplot(g, ggplot2::aes(.data$mce, .data$lab, colour = .data$band))
     if (length(vl)) p <- p + ggplot2::geom_vline(xintercept = vl, linetype = c("dashed", "dotted")[seq_along(vl)],
                                                  colour = .mosaic_calrep_tgt())
     p <- p + (if (many) ggplot2::geom_segment(ggplot2::aes(x = .data$mce_lo, xend = .data$mce, yend = .data$lab),
                                               linewidth = 0.8, alpha = 0.35)
               else ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$mce, yend = .data$lab), linewidth = 0.3, alpha = 0.5))
     sub <- paste0(tx$mce_subtitle,
                   if (length(vl)) .mosaic_fill(tx$mce_subtitle_limits, green = .mosaic_calrep_pct_thr(thr_g),
                                                yellow = .mosaic_calrep_pct_thr(thr_y)) else "",
                   .mosaic_fill(tx$mce_subtitle_p90, p90 = .mosaic_calrep_pct(d$mce_summary$p90)),
                   if (many) tx$mce_subtitle_many else "")
     p + ggplot2::geom_point(size = 1.4) +
          ggplot2::scale_colour_manual(values = .mosaic_calrep_rag(), guide = "none") +
          ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 0.1),
                                      limits = c(0, max(c(g$mce, vl), na.rm = TRUE) * 1.04),
                                      expand = ggplot2::expansion(mult = c(0.01, 0.02))) +
          ggplot2::labs(title = tx$mce_title, subtitle = sub, x = tx$mce_x, y = NULL) +
          .mosaic_calrep_theme(8.5) +
          ggplot2::theme(axis.text.y = ggplot2::element_text(size = .mosaic_calrep_label_size(nrow(g))))
}

.mosaic_calrep_kl_plot <- function(d, gl, g, many) {
     tx <- .mosaic_gl_text(gl, "parameters_page")
     g <- g[is.finite(g$kl), , drop = FALSE]
     if (!nrow(g)) return(.mosaic_calrep_unavailable(tx$kl_title, tx$kl_empty))
     edges <- .mosaic_calrep_kl_edges(d)
     levs <- unlist(tx$kl_bands)
     withheld_lab <- tx$kl_withheld
     # A KL without a band was withheld (ensemble ESS below 100: the null bias
     # would put prior-only parameters in "partly informed"); drawn in grey (RN3).
     g$kl_band <- factor(ifelse(g$kl_band %in% levs, g$kl_band, withheld_lab), levels = c(levs, withheld_lab))
     fam <- ifelse(is.na(g$prior_family), "", g$prior_family)
     g$lab <- vapply(seq_len(nrow(g)), function(i)
          if (many) .mosaic_fill(tx$kl_label_many, name = g$g[i], family = fam[i], n = g$n[i])
          else if (nzchar(fam[i])) .mosaic_fill(tx$kl_label, name = g$g[i], family = fam[i]) else g$g[i], "")
     g$lab <- factor(g$lab, levels = unique(g$lab[order(g$kl)]))
     p <- ggplot2::ggplot(g, ggplot2::aes(.data$kl, .data$lab, colour = .data$kl_band)) +
          ggplot2::geom_vline(xintercept = edges, colour = .mosaic_calrep_tgt(), linewidth = 0.3, linetype = "dashed")
     p <- p + (if (many) ggplot2::geom_segment(ggplot2::aes(x = .data$kl_lo, xend = .data$kl_hi, yend = .data$lab),
                                               linewidth = 0.8, alpha = 0.3)
               else ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$kl, yend = .data$lab), linewidth = 0.3, alpha = 0.5))
     brk <- sort(unique(c(0, 0.01, edges, 2)))
     p + ggplot2::geom_point(size = 1.4, show.legend = TRUE) +
          ggplot2::scale_colour_manual(values = stats::setNames(c("grey65", "#6f9fd8", "#1f5fa8", "grey80"),
                                                                c(levs, withheld_lab)),
                                       name = NULL, drop = TRUE, na.translate = FALSE) +
          ggplot2::scale_x_continuous(trans = scales::pseudo_log_trans(sigma = 0.01, base = 10), breaks = brk,
                                      limits = c(0, max(c(g$kl, g$kl_hi, edges[2]), na.rm = TRUE) * 1.5)) +
          ggplot2::labs(title = tx$kl_title,
                        subtitle = paste0(tx$kl_subtitle, .mosaic_fill(tx$kl_subtitle_edges, lo = edges[1], hi = edges[2]),
                                          if (many) tx$kl_subtitle_many else ""),
                        x = tx$kl_x, y = NULL) +
          .mosaic_calrep_theme(8.5) +
          ggplot2::theme(legend.position = "bottom",
                         axis.text.y = ggplot2::element_text(size = .mosaic_calrep_label_size(nrow(g)))) +
          ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2.2)))
}

.mosaic_calrep_page_parameters <- function(d, gl, foot) {
     tx <- .mosaic_gl_text(gl, "parameters_page")
     pp <- d$parameters
     many <- nrow(pp) > 60L
     g <- if (nrow(pp)) .mosaic_calrep_group_params(pp, many) else NULL
     pm <- if (is.null(g)) .mosaic_calrep_unavailable(tx$mce_title, tx$no_parameters)
           else .mosaic_calrep_safe(gl, tx$mce_title, .mosaic_calrep_mce_plot(d, gl, g, many))
     pk <- if (is.null(g)) .mosaic_calrep_unavailable(tx$kl_title, tx$no_parameters)
           else .mosaic_calrep_safe(gl, tx$kl_title, .mosaic_calrep_kl_plot(d, gl, g, many))
     ms <- d$mce_summary; ks <- d$kl_summary
     worst <- ms$worst[is.finite(ms$worst$mce), , drop = FALSE]
     worst_txt <- if (nrow(worst)) .mosaic_fill(tx$note_largest, list = paste(
          sprintf("%s %s", utils::head(worst$parameter, 3), .mosaic_calrep_pct(utils::head(worst$mce, 3))),
          collapse = ", ")) else ""
     cnt <- function(x) if (is.finite(x)) .mosaic_calrep_fmt(x) else .mosaic_gl_text(gl, "values", "na")
     txt <- paste0(.mosaic_fill(tx$note_left, median = .mosaic_calrep_pct(ms$median), p90 = .mosaic_calrep_pct(ms$p90),
                                max = .mosaic_calrep_pct(ms$max), n = .mosaic_calrep_fmt(sum(is.finite(pp$mce)))),
                   worst_txt,
                   if (isTRUE(ks$withheld > 0) && !isTRUE(ks$largely_prior + ks$partly_informed + ks$data_informed > 0))
                        .mosaic_fill(tx$note_right_withheld, n = cnt(ks$withheld))
                   else .mosaic_fill(tx$note_right, prior = cnt(ks$largely_prior), partly = cnt(ks$partly_informed),
                                     data = cnt(ks$data_informed),
                                     withheld = if (isTRUE(ks$withheld > 0))
                                          .mosaic_fill(tx$note_right_withheld_part, n = cnt(ks$withheld)) else ""),
                   tx$note_end)
     txt <- paste(strwrap(txt, width = 165), collapse = "\n")
     # A free grob, so patchwork does not align it to the plots' panel area
     # (which indented it under the left plot's axis labels).
     note <- patchwork::wrap_elements(full = grid::textGrob(
          txt, x = grid::unit(2, "mm"), y = grid::unit(1, "npc"), hjust = 0, vjust = 1,
          gp = grid::gpar(fontsize = 7.7, col = .mosaic_calrep_ink(), lineheight = 1.2)))
     p <- ((pm | pk) / note) + patchwork::plot_layout(heights = c(6, 0.55)) +
          .mosaic_calrep_header(d, gl, "parameters", foot)
     patchwork::patchworkGrob(p)
}

# ---------------------------------------------------------------------------
# Page 3: fit table and time series
# ---------------------------------------------------------------------------

.mosaic_calrep_metric_colour <- function(m) {
     cols <- c(cases = unname(mosaic_colors("cases")), deaths = unname(mosaic_colors("deaths")))
     unname(cols[m]) %|NA|% "grey30"
}

.mosaic_calrep_metrics_present <- function(d) {
     m <- unique(c(d$fit$table$metric, d$fit$weekly$metric))
     intersect(c("cases", "deaths"), m)
}

# Runs of consecutive TRUE in `flag` over weekly dates, as rectangles.
.mosaic_calrep_week_runs <- function(date, flag) {
     if (!length(flag) || !any(flag)) return(data.frame(xmin = as.Date(character(0)), xmax = as.Date(character(0))))
     r <- rle(flag); end <- cumsum(r$lengths); start <- end - r$lengths + 1L
     data.frame(xmin = date[start[r$values]], xmax = date[end[r$values]] + 7)
}

.mosaic_calrep_ts_plot <- function(d, gl, m) {
     tx <- .mosaic_gl_text(gl, "fit_page")
     title <- .mosaic_fill(tx$series_title, metric = m)
     w <- d$fit$weekly
     w <- w[w$metric == m, , drop = FALSE]
     if (!nrow(w)) return(.mosaic_calrep_unavailable(title, .mosaic_fill(tx$series_empty, metric = m)))
     # A multi-location run draws the all-locations row the writer built from
     # trajectories summed over locations; per-location rows are never summed.
     all_id <- d$fit$all_locations_id
     many <- length(setdiff(unique(w$location), all_id)) > 1L
     if (many) {
          if (is.na(all_id) || !any(w$location == all_id))
               return(.mosaic_calrep_unavailable(title, .mosaic_fill(tx$series_no_all, metric = m)))
          w <- w[w$location == all_id, , drop = FALSE]
     }
     band_cols <- c("lo50", "hi50", "lo95", "hi95")
     has_bands <- all(vapply(band_cols, function(cl) any(is.finite(w[[cl]])), logical(1)))
     x <- w[, c("date", "scored", "observed", "predicted", band_cols)]
     x <- x[order(x$date), , drop = FALSE]
     # a gap in the weekly rows (a file holding scored weeks only) breaks the line
     gap <- c(FALSE, diff(as.numeric(x$date)) > 7)
     if (any(gap)) {
          brk <- x[gap, , drop = FALSE]; brk$date <- brk$date - 1
          brk[, setdiff(names(brk), c("date", "scored"))] <- NA
          x <- rbind(x, brk); x <- x[order(x$date), , drop = FALSE]
     }
     col <- .mosaic_calrep_metric_colour(m)
     r <- d$run
     xl <- c(if (!is.na(r$date_start)) r$date_start else min(x$date), if (!is.na(r$date_stop)) r$date_stop else max(x$date) + 6)
     modelled <- is.finite(x$predicted)
     uns <- .mosaic_calrep_week_runs(x$date[modelled], !(x$scored[modelled] %in% TRUE))
     p <- ggplot2::ggplot(x, ggplot2::aes(.data$date))
     burn <- r$burn_in_days
     if (is.finite(burn) && burn > 0 && !is.na(r$date_start))
          p <- p + ggplot2::annotate("rect", xmin = r$date_start, xmax = r$date_start + burn, ymin = -Inf, ymax = Inf,
                                     fill = "grey88")
     if (nrow(uns))
          p <- p + ggplot2::geom_rect(data = uns, ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax),
                                      ymin = -Inf, ymax = Inf, fill = "grey95", inherit.aes = FALSE)
     if (has_bands)
          p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lo95, ymax = .data$hi95), fill = col, alpha = 0.15,
                                        na.rm = TRUE) +
               ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lo50, ymax = .data$hi50), fill = col, alpha = 0.25,
                                    na.rm = TRUE)
     n_uns <- sum(modelled & !(x$scored %in% TRUE))
     src <- d$fit$weekly_source
     p + ggplot2::geom_line(ggplot2::aes(y = .data$predicted), colour = col, linewidth = 0.5, na.rm = TRUE) +
          ggplot2::geom_point(ggplot2::aes(y = .data$observed), size = 0.6, colour = "grey20", alpha = 0.75, na.rm = TRUE) +
          ggplot2::scale_x_date(limits = xl, expand = ggplot2::expansion(mult = 0.01)) +
          ggplot2::scale_y_continuous(labels = scales::label_comma()) +
          ggplot2::labs(title = title,
                        subtitle = paste(strwrap(paste0(
                             if (many) tx$subtitle_many else "",
                             if (identical(src, "scored_weeks")) tx$subtitle_scored_only else tx$subtitle_main,
                             if (has_bands) tx$subtitle_bands else "",
                             if (n_uns) .mosaic_fill(tx$subtitle_unscored, n = n_uns) else "",
                             if (is.finite(burn) && burn > 0)
                                  .mosaic_fill(tx$subtitle_burn_in, days = .mosaic_calrep_fmt(burn))
                             else tx$subtitle_end,
                             if (identical(src, "daily_sums")) tx$subtitle_daily_sums else ""), width = 190),
                             collapse = "\n"),
                        x = NULL, y = .mosaic_fill(tx$series_y, metric = m)) +
          .mosaic_calrep_theme(8.5)
}

.mosaic_calrep_page_fit <- function(d, gl, foot) {
     tx <- .mosaic_gl_text(gl, "fit_page")
     fxt <- .mosaic_gl_text(gl, "fit_table")
     fmt <- .mosaic_calrep_fmt; pct <- .mosaic_calrep_pct
     tb <- d$fit$table_display
     has_tier12 <- any(tb$subset == "tier12")
     heads <- vapply(fxt$columns, function(cl) trimws(paste(cl$head, cl$sub %||% "")), character(1))
     fit_df <- stats::setNames(tb[, c("label", "r2", "bias", "cov50", "cov95", "wis", "n_scored")], heads)
     fit_note <- paste0(tx$note, if (has_tier12) tx$note_tier else "", tx$note_end)
     pl <- d$fit$per_location
     multi <- isTRUE(d$run$n_locations > 1) || length(unique(pl$location)) > 1L
     worst <- if (multi && nrow(pl))
          pl[pl$metric == "cases" & is.finite(pl$observed_total) & pl$observed_total > 0, , drop = FALSE] else pl[0, ]
     rank_note <- tx$worst_rank_r2
     if (nrow(worst)) {
          # Rank by R²; when no location has one (flat series, e.g. 47 Kenya
          # counties), by |log bias| instead, never alphabetically (WS6).
          if (all(!is.finite(worst$r2))) {
               lb <- abs(log(worst$bias)); lb[!is.finite(lb) & !is.na(worst$bias)] <- Inf
               worst <- worst[order(-ifelse(is.na(lb), -Inf, lb)), , drop = FALSE]
               rank_note <- tx$worst_rank_bias
          } else worst <- worst[order(worst$r2, na.last = FALSE), , drop = FALSE]
          worst <- worst[seq_len(min(8L, nrow(worst))), , drop = FALSE]
          wt <- .mosaic_calrep_table(
               stats::setNames(data.frame(worst$location, fmt(worst$observed_total), fmt(worst$r2, 2),
                                          fmt(worst$bias, 2), pct(worst$cov95, 0), stringsAsFactors = FALSE),
                               unlist(tx$worst_columns)),
               .mosaic_fill(tx$worst_title, n = nrow(worst), total = sum(pl$metric == "cases"), rank = rank_note))
          ft <- if (nrow(tb)) .mosaic_calrep_table(fit_df, tx$table_title_short)
                else .mosaic_calrep_unavailable(tx$table_missing_title, tx$table_missing)
          h_top <- max(attr(wt, "height_pt"), attr(ft, "height_pt") %||% 60)
          top <- ft | wt
     } else if (nrow(tb)) {
          top <- .mosaic_calrep_table(fit_df, tx$table_title)
          h_top <- attr(top, "height_pt")
     } else {
          top <- .mosaic_calrep_unavailable(tx$table_missing_title, tx$table_missing)
          h_top <- 60
     }
     mets <- .mosaic_calrep_metrics_present(d)
     ts <- lapply(mets, function(m) .mosaic_calrep_safe(gl, .mosaic_fill(tx$series_title, metric = m),
                                                         .mosaic_calrep_ts_plot(d, gl, m)))
     if (!length(ts))
          ts <- list(.mosaic_calrep_unavailable(tx$series_title_any, tx$series_empty_any))
     missing <- setdiff(c("cases", "deaths"), mets)
     # heights in points of the ~470 pt panel area: tables and the note at their
     # natural size, the series share the rest
     note_pt <- if (length(missing) && length(mets)) 20 else 0
     fn_pt <- if (nrow(tb)) 22 else 0
     fixed <- h_top + 16 + note_pt + fn_pt
     heights <- c(h_top + 16, if (fn_pt) fn_pt, rep(max(120, 470 - fixed) / length(ts), length(ts)))
     panels <- c(list(top), if (fn_pt) list(.mosaic_calrep_note(paste(strwrap(fit_note, 190), collapse = "\n"))), ts)
     if (note_pt > 0) {
          chan <- vapply(missing, function(m) fxt[[paste0("channel_", m)]], character(1))
          panels <- c(panels, list(.mosaic_calrep_note(.mosaic_fill(tx$missing_channel, channels = paste(chan, collapse = ", "),
                                                                    names = paste(missing, collapse = " or ")))))
          heights <- c(heights, note_pt)
     }
     p <- patchwork::wrap_plots(panels, ncol = 1, heights = heights) +
          .mosaic_calrep_header(d, gl, "fit", foot)
     patchwork::patchworkGrob(p)
}

.mosaic_calrep_title_case <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))

# A free text grob (not aligned to the plots' panel area).
.mosaic_calrep_note <- function(txt) {
     patchwork::wrap_elements(full = grid::textGrob(
          txt, x = grid::unit(2, "mm"), y = grid::unit(0.5, "npc"), hjust = 0, vjust = 0.5,
          gp = grid::gpar(fontsize = 7.6, col = .mosaic_calrep_muted(), fontface = "italic", lineheight = 1.15)))
}


# ---------------------------------------------------------------------------
# Page 4: weekly predicted vs observed, 2 x 2
# ---------------------------------------------------------------------------

.mosaic_calrep_page_scatter <- function(d, gl, foot) {
     tx <- .mosaic_gl_text(gl, "scatter_page")
     chan <- function(m) .mosaic_gl_text(gl, "fit_table", paste0("channel_", m))
     # per-location weekly totals (the all-locations row is a sum of them)
     w <- d$fit$weekly
     if (!is.na(d$fit$all_locations_id)) w <- w[w$location != d$fit$all_locations_id, , drop = FALSE]
     w <- w[is.finite(w$observed) & is.finite(w$predicted), , drop = FALSE]
     many <- length(unique(w$location)) > 1L
     lims <- lapply(c(cases = "cases", deaths = "deaths"), function(m) {
          x <- w[w$metric == m, , drop = FALSE]
          if (!nrow(x)) return(NULL)
          r <- range(c(x$observed, x$predicted) + 1)
          c(r[1] / 1.6, r[2] * 1.6)
     })
     sp <- function(m) {
          title <- .mosaic_fill(tx$scatter_title, metric = m)
          x <- w[w$metric == m, , drop = FALSE]
          if (!nrow(x)) return(.mosaic_calrep_unavailable(title, .mosaic_fill(tx$not_scored, channel = chan(m))))
          ggplot2::ggplot(x, ggplot2::aes(.data$observed + 1, .data$predicted + 1)) +
               ggplot2::geom_abline(slope = 1, intercept = 0, colour = .mosaic_calrep_tgt(), linetype = "dashed") +
               ggplot2::geom_point(colour = .mosaic_calrep_metric_colour(m), alpha = if (many) 0.25 else 0.55, size = 1.1) +
               ggplot2::scale_x_log10(labels = scales::label_comma(), limits = lims[[m]]) +
               ggplot2::scale_y_log10(labels = scales::label_comma(), limits = lims[[m]]) +
               ggplot2::labs(title = title,
                             subtitle = .mosaic_fill(tx$scatter_subtitle, all = if (many) tx$scatter_subtitle_all else ""),
                             x = NULL, y = tx$scatter_y) +
               .mosaic_calrep_theme(8.5)
     }
     dp <- function(m) {
          title <- .mosaic_fill(tx$dist_title, metric = m)
          x <- w[w$metric == m, , drop = FALSE]
          if (!nrow(x)) return(.mosaic_calrep_unavailable(title, .mosaic_fill(tx$not_scored, channel = chan(m))))
          col <- .mosaic_calrep_metric_colour(m)
          dd <- rbind(data.frame(v = x$observed + 1, what = tx$observed), data.frame(v = x$predicted + 1, what = tx$predicted))
          vals <- stats::setNames(c("grey40", col), c(tx$observed, tx$predicted))
          ggplot2::ggplot(dd, ggplot2::aes(.data$v, fill = .data$what, colour = .data$what)) +
               ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), position = "identity",
                                       alpha = 0.35, bins = 30, linewidth = 0.2, na.rm = TRUE) +
               ggplot2::scale_fill_manual(values = vals, name = NULL) +
               ggplot2::scale_colour_manual(values = vals, name = NULL) +
               ggplot2::scale_x_log10(labels = scales::label_comma(), limits = lims[[m]]) +
               ggplot2::scale_y_sqrt() +
               ggplot2::labs(title = title, subtitle = tx$dist_subtitle, x = tx$dist_x, y = tx$dist_y) +
               .mosaic_calrep_theme(8.5) + ggplot2::theme(legend.position = "bottom")
     }
     s <- function(m, expr) .mosaic_calrep_safe(gl, .mosaic_fill(tx$panel, channel = chan(m)), expr)
     p <- ((s("cases", sp("cases")) | s("deaths", sp("deaths"))) /
           (s("cases", dp("cases")) | s("deaths", dp("deaths")))) +
          patchwork::plot_layout(heights = c(1.4, 1)) +
          .mosaic_calrep_header(d, gl, "scatter", foot)
     patchwork::patchworkGrob(p)
}

# ---------------------------------------------------------------------------
# Appendix: the glossary
# ---------------------------------------------------------------------------

.mosaic_calrep_appendix <- function(gl, d = NULL) {
     wrap <- 92L
     ap <- .mosaic_gl_text(gl, "appendix")
     plain <- function(x) .mosaic_rich_plain(x, gl, "ascii")
     # Only the metrics this report shows (R17), in glossary order.
     if (!is.null(d)) {
          keys <- .mosaic_calrep_metric_keys(d)
          if (any(gl$key %in% keys)) gl <- gl[gl$key %in% keys, , drop = FALSE]
     }
     # A target as this run applied it (the check's or info row's target
     # display), with the glossary's text labelled as the default (R11).
     run_thr <- function(key) {
          if (is.null(d)) return(NULL)
          ck <- d$status$checks
          t <- ck$target_display[ck$key %in% key]
          if (!length(t)) t <- unlist(lapply(d$info, function(x) if (identical(x$key, key) && !is.na(x$operator))
               x$target_display))
          t <- t[!is.na(t) & nzchar(t)]
          if (length(t)) paste(ap$this_run, plain(t[1])) else NULL
     }
     lines <- list()
     for (i in seq_len(nrow(gl))) {
          e <- gl[i, ]
          ent <- list(list(txt = e$label, kind = "head"))
          has_thr <- !grepl("^none\\b", tolower(trimws(e$threshold)))
          rt <- run_thr(e$key)
          body <- c(e$definition, paste(ap$formula, e$formula), paste(ap$how_to_read, e$how_to_read),
                    rt,
                    if (has_thr) paste(if (is.null(rt)) ap$default_only else ap$default, e$threshold),
                    paste(ap$not_a_measure_of, e$not_a_measure_of))
          for (b in body) for (l in strwrap(b, width = wrap, exdent = 3))
               ent[[length(ent) + 1L]] <- list(txt = l, kind = "body")
          ent[[length(ent) + 1L]] <- list(txt = "", kind = "gap")
          lines <- c(lines, ent)
     }
     # The metrics retired in 1.4.0, and why.
     rt <- attr(gl, "retired")
     if (length(rt$items)) {
          lines <- c(lines, list(list(txt = rt$heading, kind = "head")))
          for (l in strwrap(rt$note, width = wrap)) lines <- c(lines, list(list(txt = l, kind = "body")))
          for (it in rt$items) {
               lines <- c(lines, list(list(txt = "", kind = "gap"), list(txt = it$label, kind = "sub")))
               for (l in strwrap(it$why, width = wrap, exdent = 3)) lines <- c(lines, list(list(txt = l, kind = "body")))
          }
     }
     # two columns per page; an entry may continue in the next column, but a
     # heading never ends a column. Columns are balanced so the last page is
     # not nearly empty (round 3): at most 49 lines each.
     n_lines <- length(lines)
     per_col <- max(20L, min(49L, ceiling(n_lines / (2 * ceiling(n_lines / (2 * 46)))) + 3L))
     cols <- list(); cur <- list()
     for (k in seq_along(lines)) {
          l <- lines[[k]]
          if (!length(cur) && l$kind == "gap") next
          full <- length(cur) >= per_col ||
               (l$kind %in% c("head", "sub") && length(cur) >= per_col - 3L)
          if (full) { cols[[length(cols) + 1L]] <- cur; cur <- list() }
          cur[[length(cur) + 1L]] <- l
     }
     if (length(cur)) cols[[length(cols) + 1L]] <- cur
     pages <- split(cols, ceiling(seq_along(cols) / 2))
     ver <- attr(gl, "version") %||% "?"
     lapply(seq_along(pages), function(i) {
          pg <- pages[[i]]
          function(foot) {
               kids <- list(
                    grid::textGrob(.mosaic_fill(ap$title, i = i, n = length(pages)),
                                   x = 0.04, y = 0.955, hjust = 0, gp = grid::gpar(fontface = "bold", fontsize = 15)),
                    grid::textGrob(.mosaic_fill(ap$intro, version = ver),
                                   x = 0.04, y = 0.93, hjust = 0, vjust = 1, gp = grid::gpar(fontsize = 8.5, col = "grey35")),
                    grid::textGrob(foot, x = 0.965, y = 0.025, hjust = 1,
                                   gp = grid::gpar(fontsize = 7.5, col = .mosaic_calrep_muted())))
               for (cc in seq_along(pg)) {
                    x <- c(0.04, 0.52)[cc]
                    yy <- 0.875
                    for (l in pg[[cc]]) {
                         if (nzchar(l$txt))
                              kids[[length(kids) + 1L]] <- grid::textGrob(
                                   l$txt, x = x, y = yy, hjust = 0,
                                   gp = grid::gpar(fontsize = switch(l$kind, head = 9, sub = 8.2, 7.8),
                                                   fontface = switch(l$kind, head = "bold", sub = "bold", "plain"),
                                                   col = if (l$kind %in% c("head", "sub")) .mosaic_calrep_ink() else "grey25"))
                         yy <- yy - if (l$kind == "gap") 0.009 else 0.0163
                    }
               }
               grid::gTree(children = do.call(grid::gList, kids))
          }
     })
}
