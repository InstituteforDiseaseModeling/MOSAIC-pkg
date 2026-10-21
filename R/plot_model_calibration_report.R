#' Calibration diagnostics report (PDF)
#'
#' Renders the MOSAIC calibration-diagnostics report from
#' \code{3_results/calibration_diagnostics.json} (summary.json schema 2
#' companion). The report answers the three calibration questions, each on
#' its own page, and reads nothing but that file and the metric glossary
#' (\code{inst/extdata/metric_glossary.yml}): no samples, predictions or
#' configs are opened, so it can be re-rendered from a finished run.
#'
#' @description
#' Pages:
#' \enumerate{
#'   \item \strong{Simulations and ensemble ESS.} The simulation-convergence
#'     status (green / yellow / red, from \code{status}) with its four checks,
#'     reasons and flags; tables for all draws (the posterior) and for the
#'     best-fit ensemble; the posterior ESS against the draws (log-log) stacked
#'     on the best log-likelihood found so far, on a shared x axis.
#'   \item \strong{Parameters.} Monte Carlo error of each posterior mean
#'     (left) and prior-to-posterior KL divergence (right). Above 60 parameters
#'     the location-specific parameters are grouped by base name (point =
#'     worst or median location, bar = range over locations).
#'   \item \strong{Fit.} The fit table (and the worst-fitting locations of a
#'     multi-location run) over weekly time series of observed and predicted
#'     counts on the scored weeks.
#'   \item \strong{Predicted vs observed.} Weekly log-log scatter over the
#'     square-root-scale distributions of weekly counts, cases and deaths.
#'   \item \strong{Appendix.} What each metric means, generated from the
#'     glossary.
#' }
#'
#' Every threshold drawn comes from \code{status$checks[]$threshold_green} /
#' \code{threshold_yellow}; the status, labels, reasons and flags are shown
#' exactly as written by \code{\link{calc_convergence_status}}. A panel whose
#' inputs are missing (deaths not scored, a path with no points, an empty fit
#' block) is replaced by a note rather than failing the report.
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
#' @details The PDF is vector (base \code{pdf()} device, 11 x 8.5 in) and is
#'   written to a temporary file then renamed, so an interrupted render never
#'   leaves a truncated report. A run made before MOSAIC 1.4.0 has no
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

     d <- .mosaic_calrep_read(diagnostics)
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

     gl <- .mosaic_glossary()
     # Reasons that do not fit page 1 continue on a page of their own. Measured
     # on a throw-away device of the report's size (measuring on the report
     # device would start a blank page), so the page count is known up front.
     dev_keep <- grDevices::dev.cur()
     grDevices::pdf(NULL, width = 11, height = 8.5)
     overflow <- tryCatch(.mosaic_calrep_page1_layout(d, gl)$overflow, error = function(e) character(0))
     grDevices::dev.off()
     if (dev_keep > 1L && dev_keep %in% grDevices::dev.list()) grDevices::dev.set(dev_keep)
     tmp <- tempfile(pattern = "calibration_report_", tmpdir = dir_output, fileext = ".pdf")
     dev_before <- grDevices::dev.cur()
     grDevices::pdf(tmp, width = 11, height = 8.5, onefile = TRUE, useDingbats = FALSE)
     dev_report <- grDevices::dev.cur()
     ok <- FALSE
     on.exit({
          if (dev_report %in% grDevices::dev.list()) grDevices::dev.off(dev_report)
          if (dev_before > 1L && dev_before %in% grDevices::dev.list()) grDevices::dev.set(dev_before)
          if (!ok && file.exists(tmp)) unlink(tmp)
     }, add = TRUE)
     pages <- c(list(function(foot) .mosaic_calrep_page_sampling(d, gl, foot)),
                if (length(overflow)) list(function(foot) .mosaic_calrep_page_reasons(d, overflow, foot)),
                list(function(foot) .mosaic_calrep_page_parameters(d, foot),
                     function(foot) .mosaic_calrep_page_fit(d, foot),
                     function(foot) .mosaic_calrep_page_scatter(d, foot)),
                .mosaic_calrep_appendix(gl, d))
     n_pages <- length(pages)
     foot_of <- function(i) sprintf("%s  |  MOSAIC %s  |  generated %s  |  page %d of %d",
                                    .mosaic_calrep_title(d), d$run$mosaic_version %|NA|% "?",
                                    format(Sys.Date()), i, n_pages)
     for (i in seq_len(n_pages)) {
          # Open the page before building: measuring text while building the
          # grob would otherwise start a page on a fresh device.
          grid::grid.newpage()
          g <- tryCatch(suppressWarnings(suppressMessages(pages[[i]](foot_of(i)))),
                        error = function(e) .mosaic_calrep_failed_page(i, conditionMessage(e), foot_of(i)))
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
#' \code{what = "pre140"}: a run made before MOSAIC 1.4.0 (no
#' \code{convergence_status} in summary.json, or a schema older than 2).
#' \code{what = "missing"}: a 1.4.0 run whose
#' \code{calibration_diagnostics.json} is missing or unreadable.
#' @noRd
.mosaic_calrep_legacy_text <- function(what = c("pre140", "missing")) {
     switch(match.arg(what),
            # Shared word for word with the dashboard (resolver LEGACY_SAMPLING_NOTE).
            pre140 = "Calibrated with an earlier selection rule; simulation convergence was not assessed on the 1.4.0 scale",
            missing = paste0("MOSAIC 1.4.0 run without a readable calibration_diagnostics.json: see ",
                             "3_results/summary.json for its convergence status"))
}


# ---------------------------------------------------------------------------
# Reading and normalising calibration_diagnostics.json
# ---------------------------------------------------------------------------

.mosaic_calrep_read <- function(diagnostics) {
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
                       src, if (is.na(sv)) "missing" else sprintf("\"%s\"", sv), .mosaic_calrep_legacy_text()),
               call. = FALSE)
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
# missing columns and JSON nulls become NA.
.mosaic_calrep_df <- function(x, cols, chr = character(0)) {
     empty <- as.data.frame(stats::setNames(lapply(cols, function(cl)
          if (cl %in% chr) character(0) else numeric(0)), cols), stringsAsFactors = FALSE)
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
          x[[cl]] <- if (cl %in% chr) as.character(x[[cl]]) else suppressWarnings(as.numeric(x[[cl]]))
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
                 date_start = .mosaic_calrep_date1(r$date_start), date_stop = .mosaic_calrep_date1(r$date_stop))
     checks <- .mosaic_calrep_df(st$checks, c("check", "label", "value", "threshold_green", "threshold_yellow",
                                              "colour", "reason"),
                                 chr = c("check", "label", "colour", "reason"))
     status <- list(status = ch(st$convergence_status), label = ch(st$convergence_label),
                    reasons = .mosaic_calrep_chrv(st$reasons), flags = .mosaic_calrep_chrv(st$flags),
                    flags_text = .mosaic_calrep_chrv(st$flags_text), checks = checks,
                    stopping_rule_met = lg(st$stopping_rule_met))
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
          loglik_path = .mosaic_calrep_df(s$loglik_path, c("n", "best_gap", "top100_gap")))
     params <- .mosaic_calrep_df(j$parameters, c("parameter", "base", "location", "prior_family", "mce", "kl",
                                                 "kl_band"),
                                 chr = c("parameter", "base", "location", "prior_family", "kl_band"))
     weekly_cols <- c("location", "metric", "week", "observed", "predicted", "lo50", "hi50", "lo95", "hi95")
     weekly <- .mosaic_calrep_df(f$weekly, weekly_cols, chr = c("location", "metric", "week"))
     weekly$date <- .mosaic_calrep_date(weekly$week)
     tab <- .mosaic_calrep_df(f$table, c("metric", "subset", "r2", "bias", "cov50", "cov95", "wis", "n_scored"),
                              chr = c("metric", "subset"))
     tab$subset[is.na(tab$subset)] <- "scored"   # a file without the column has one row per channel
     fit <- list(table = tab,
                 per_location = .mosaic_calrep_df(f$per_location, c("location", "metric", "r2", "bias", "cov95",
                                                                    "observed_total"),
                                                  chr = c("location", "metric")),
                 weekly = weekly[!is.na(weekly$date), , drop = FALSE])
     list(run = run, status = status, sampling = sampling, parameters = params,
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

# Glossary keys of the panels (tables carry theirs in a `key` column).
.mosaic_calrep_panel_keys <- function() {
     c("convergence_status", "convergence_flags", "ess_all", "ess_growth_slope", "best_loglik_path", "mce",
       "kl_divergence", "r2", "bias", "coverage", "wis", "scored_days", "fit_time_series", "weekly_scatter",
       "weekly_distribution")
}

# Glossary key of each status check (labels come from the glossary unless the
# JSON carries one).
.mosaic_calrep_check_keys <- function() {
     c(ess = "ess_best", mce = "mce", khat = "khat_tempered", informativeness = "flat_likelihood")
}

# Every glossary key the report uses for this run.
.mosaic_calrep_metric_keys <- function(d) {
     t <- .mosaic_calrep_sampling_tables(d)
     keys <- c(unlist(strsplit(c(t$all$key, t$ensemble$key), ",")), .mosaic_calrep_panel_keys(),
               unname(.mosaic_calrep_check_keys()))
     sort(unique(trimws(keys)))
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

.mosaic_calrep_title <- function(d) {
     L <- d$run$locations
     if (!length(L)) return("calibration")
     if (length(L) <= 4L) return(paste(L, collapse = ", "))
     sprintf("%s ... %s (%d locations)", L[1], L[length(L)], length(L))
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

.mosaic_calrep_header <- function(d, section, foot) {
     r <- d$run
     mode <- switch(r$mode %|NA|% "", fixed = "fixed budget", auto = "adaptive", r$mode %|NA|% "mode n/a")
     n_loc <- r$n_locations
     info <- paste(c(sprintf("%s location%s", .mosaic_calrep_fmt(n_loc), if (isTRUE(n_loc == 1)) "" else "s"),
                     mode,
                     # "1 valid of 40 simulated" when simulations failed (RN5)
                     if (is.finite(r$n_simulations) && is.finite(d$sampling$sims_used) &&
                         r$n_simulations > d$sampling$sims_used)
                          sprintf("%s valid of %s simulated", .mosaic_calrep_fmt(d$sampling$sims_used),
                                  .mosaic_calrep_fmt(r$n_simulations))
                     else sprintf("%s draws", .mosaic_calrep_fmt(d$sampling$sims_used)),
                     if (!is.na(r$date_start) && !is.na(r$date_stop))
                          sprintf("%s to %s", format(r$date_start), format(r$date_stop)),
                     if (!is.na(r$posterior_weighting)) sprintf("%s ensemble", r$posterior_weighting)),
                   collapse = "  |  ")
     patchwork::plot_annotation(
          title = sprintf("Calibration diagnostics: %s", .mosaic_calrep_title(d)),
          subtitle = paste0(section, "\n", info),
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
.mosaic_calrep_safe <- function(title, expr) {
     tryCatch(expr, error = function(e)
          .mosaic_calrep_unavailable(title, paste("Unavailable:", conditionMessage(e))))
}

.mosaic_calrep_failed_page <- function(i, msg, foot) {
     grid::gTree(children = grid::gList(
          grid::textGrob(sprintf("Page %d could not be drawn", i), x = 0.04, y = 0.95, hjust = 0,
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
# Page 1: simulations and ensemble ESS
# ---------------------------------------------------------------------------

.mosaic_calrep_check <- function(d, id) {
     ck <- d$status$checks
     ck[ck$check %in% id, , drop = FALSE][1, , drop = FALSE]
}

# Threshold text of one check, from its threshold_green / threshold_yellow.
.mosaic_calrep_threshold_text <- function(ck) {
     g <- ck$threshold_green; y <- ck$threshold_yellow
     if (!nrow(ck) || is.na(ck$check)) return("")
     fm <- switch(ck$check,
                  ess = function(v) .mosaic_calrep_fmt(v),
                  mce = function(v) .mosaic_calrep_pct_thr(v),
                  function(v) .mosaic_calrep_sig(v, 3))
     op <- switch(ck$check, ess = c(">=", ">="), mce = c("<=", "<="), khat = c("<", "<"), c("", ""))
     if (identical(ck$check, "informativeness"))
          return(if (is.finite(g) && g < 1) sprintf("yellow >= %s", .mosaic_calrep_pct_thr(g))
                 else "yellow if eta* = 1")
     parts <- c(if (is.finite(g)) sprintf("green %s %s", op[1], fm(g)),
                if (is.finite(y)) sprintf("yellow %s %s", op[2], fm(y)))
     paste(parts, collapse = ", ")
}

.mosaic_calrep_check_value <- function(d, ck) {
     v <- ck$value
     thr <- c(ck$threshold_green, ck$threshold_yellow)
     switch(ck$check,
            ess = .mosaic_fmt_thr(v, thr, 0),
            mce = .mosaic_fmt_thr(v, thr, 1, scale = 100, suffix = "%"),
            khat = if (is.finite(v)) .mosaic_fmt_thr(v, thr, 2, big.mark = "") else "undefined",
            informativeness = {
                 frac <- is.finite(ck$threshold_green) && ck$threshold_green < 1
                 val <- if (!is.finite(v)) "" else if (frac)
                      sprintf(" (untempered ESS %s)",
                              if (abs(v - ck$threshold_green) / ck$threshold_green < 0.05)
                                   .mosaic_fmt_thr(v, ck$threshold_green, 2, scale = 100, suffix = "%")
                              else .mosaic_calrep_sig_pct(v))
                      else sprintf(" (eta* = %s)", .mosaic_calrep_sig(v, 2))
                 if (identical(ck$colour, "green")) paste0("informative", val)
                 else if (isTRUE(d$sampling$flat_likelihood)) paste0("flat", val)
                 else paste0("not assessed", val)
            },
            .mosaic_calrep_sig(v, 3))
}

# The tables of page 1, with the glossary key of every row.
.mosaic_calrep_sampling_tables <- function(d) {
     s <- d$sampling; r <- d$run; st <- d$status
     fmt <- .mosaic_calrep_fmt; pct <- .mosaic_calrep_pct
     tgt <- function(v, t) if (nzchar(t)) paste0(v, "   (", t, ")") else v
     ck_mce <- .mosaic_calrep_check(d, "mce")
     n_iter <- r$n_iterations
     mode <- if (identical(r$mode, "fixed")) "fixed budget"
             else if (identical(r$mode, "auto")) {
                  if (isTRUE(s$budget_exhausted)) "adaptive: stopped at the budget cap"
                  else if (isTRUE(st$stopping_rule_met))
                       if (identical(st$status, "green")) "adaptive: stopped at green"
                       else "adaptive: stopped, sampling checks green"
                  else "adaptive"
             } else r$mode %|NA|% "n/a"
     if (is.finite(n_iter)) mode <- sprintf("%s; %s run%s per draw", mode, fmt(n_iter),
                                            if (n_iter == 1) "" else "s")
     # The all-draws posterior is the coarsened posterior under every scheme;
     # posterior_weighting names how the ensemble members are chosen (R13).
     posterior <- paste0("coarsened",
                         if (is.finite(r$coarsening_alpha)) sprintf(" (alpha = %s", .mosaic_calrep_sig(r$coarsening_alpha, 3)) else " (",
                         if (is.finite(r$eta)) sprintf(", eta = %s)", .mosaic_calrep_sig(r$eta, 2)) else ")",
                         if (isTRUE(r$n_locations > 1) && is.finite(s$alpha_per_location))
                              sprintf("; alpha %s per location", .mosaic_calrep_sig(s$alpha_per_location, 2)) else "")
     slope <- s$ess_growth_slope
     slope_txt <- if (is.finite(slope)) {
          paste0(sprintf("%.2f", slope), if ("ess_growth_stalled" %in% st$flags) ", stalled (flag)" else "")
     } else "n/a (too few points)"
     ck_k <- .mosaic_calrep_check(d, "khat")
     khat_txt <- if (is.finite(s$khat_tempered) && nrow(ck_k) && !is.na(ck_k$check))
          sprintf("%s (%s)", .mosaic_fmt_thr(s$khat_tempered, c(ck_k$threshold_green, ck_k$threshold_yellow), 2,
                                             big.mark = ""),
                  switch(ck_k$colour %|NA|% "", green = "good", yellow = "marginal", red = "unreliable", "n/a"))
          else s$khat_status %|NA|% (if (is.finite(s$khat_tempered)) sprintf("%.2f", s$khat_tempered)
                                     else "undefined (weights nearly flat)")
     ck_inf <- .mosaic_calrep_check(d, "informativeness")
     info <- if (nrow(ck_inf) && !is.na(ck_inf$check)) .mosaic_calrep_check_value(d, ck_inf)
             else if (isTRUE(s$flat_likelihood)) "flat likelihood" else "not assessed"
     # Simulations projected for GREEN (the larger of the ESS and Monte Carlo
     # error projections, in simulations); indicative when the ESS growth
     # slope is below the stall threshold or not yet known (R3, R5, R6).
     proj <- s$sims_projected_for_green
     proj_txt <- if (identical(st$status, "green")) {
          "none: the status is green"
     } else if (isTRUE(st$stopping_rule_met) ||
                (nrow(st$checks) && all(st$checks$colour[st$checks$check %in% c("ess", "mce", "khat")] %in% "green"))) {
          "none: the sampling checks are green (more simulations would not change the rest)"
     } else if (is.finite(proj)) {
          paste0("about ", fmt(signif(proj, 2)), " in total",
                 if (identical(r$mode, "auto")) " (incl. one confirming batch)" else "",
                 if (is.finite(s$sims_used)) sprintf(" (now %s valid draws)", fmt(s$sims_used)) else "",
                 if (isFALSE(s$projection_reliable))
                      if (is.finite(s$ess_growth_slope)) " (indicative: ESS growth stalled)"
                      else " (indicative: ESS growth slope not yet known)" else "")
     } else "n/a: no check that more simulations would fix"
     if (isTRUE(s$budget_exhausted)) proj_txt <- paste0(proj_txt, "; budget cap reached")
     prop <- if (is.finite(s$ess_all) && is.finite(s$sims_used) && s$sims_used > 0) s$ess_all / s$sims_used else NA_real_

     all <- data.frame(
          Measure = c("Draws simulated (valid)", "Run mode", "Posterior (all draws)", "Posterior ESS (all draws)",
                      "ESS as % of draws", "ESS growth (log-log slope)", "Validity guard (Pareto k-hat)",
                      "Data informativeness", "Simulations projected for green"),
          Value = c(if (is.finite(r$n_simulations) && is.finite(s$sims_used) && r$n_simulations > s$sims_used)
                         sprintf("%s of %s simulated", fmt(s$sims_used), fmt(r$n_simulations)) else fmt(s$sims_used),
                    mode, posterior,
                    tgt(.mosaic_fmt_thr(s$ess_all, s$ess_all_target, 0),
                        if (is.finite(s$ess_all_target)) paste("target", fmt(s$ess_all_target)) else ""),
                    pct(prop, 2), slope_txt, khat_txt, info, proj_txt),
          key = c("draws", "mode", "coarsening_alpha,alpha_per_location", "ess_all", "ess_prop", "ess_growth_slope",
                  "khat_tempered", "flat_likelihood,ess_untempered_frac",
                  "sims_projected_for_green,projection_reliable"),
          stringsAsFactors = FALSE)

     binding <- s$ensemble_binding
     members <- paste0(fmt(s$ensemble_members),
                       if (is.finite(s$ensemble_members_unique)) sprintf(" (%s unique)", fmt(s$ensemble_members_unique)) else "",
                       if (!is.na(binding)) sprintf("; size set by %s", binding) else "")
     mass <- paste0(pct(s$best_subset_mass, 1),
                    if (is.finite(s$best_subset_ratio)) sprintf("   (ESS ratio %s)", .mosaic_calrep_sig(s$best_subset_ratio, 2)) else "")
     ms <- d$mce_summary
     mce_thr <- if (nrow(ck_mce) && !is.na(ck_mce$check)) c(ck_mce$threshold_green, ck_mce$threshold_yellow) else numeric(0)
     mce_txt <- tgt(sprintf("%s, %s, %s", pct(ms$median),
                            .mosaic_fmt_thr(ms$p90, mce_thr, 1, scale = 100, suffix = "%"), pct(ms$max)),
                    if (nrow(ck_mce) && !is.na(ck_mce$check) && is.finite(ck_mce$threshold_green))
                         sprintf("90th target <= %s", .mosaic_calrep_pct_thr(ck_mce$threshold_green)) else "")
     scheme <- switch(r$posterior_weighting %|NA|% "",
                      bestfit = "bestfit (best-fitting draws)",
                      coarsened = "coarsened (resample of the posterior)",
                      saturated = , tempered = sprintf("%s (legacy tier search)", r$posterior_weighting),
                      r$posterior_weighting %|NA|% "n/a")
     ens <- data.frame(
          Measure = c("Ensemble (posterior_weighting)", "Ensemble members", "Ensemble ESS", "Posterior mass kept",
                      "Monte Carlo error (median/90th/max)",
                      "Stochastic runs per member", "Scored observation cells", "Burn-in (unscored)"),
          Value = c(scheme, members,
                    tgt(.mosaic_fmt_thr(s$ess_best, c(s$ess_best_target, .mosaic_calrep_check(d, "ess")$threshold_yellow), 0),
                        if (is.finite(s$ess_best_target)) paste("target", fmt(s$ess_best_target)) else ""),
                    mass, mce_txt,
                    fmt(r$n_iter_ensemble), fmt(r$n_obs_scored),
                    if (is.finite(r$burn_in_days)) sprintf("%s days", fmt(r$burn_in_days)) else "n/a"),
          key = c("posterior_weighting,posterior_members", "ensemble_members,ensemble_binding", "ess_best",
                  "best_subset_mass,best_fit_ensemble", "mce", "n_iter_ensemble", "n_obs_scored", "burn_in_days"),
          stringsAsFactors = FALSE)
     list(all = all, ensemble = ens)
}

# Status badge, checks, reasons and flags, as one grid grob. Positions are in
# points from the top so text, bullets and the badge stay aligned at any size.
# The reasons as wrapped bullet lines (badge and its continuation page).
.mosaic_calrep_reason_lines <- function(reasons, wrap = 76L) {
     unlist(lapply(reasons, function(rsn) {
          w <- strwrap(rsn, wrap)
          paste0(c("- ", rep("  ", length(w) - 1L)), w)
     }))
}

.mosaic_calrep_badge <- function(d, gl, max_reason_lines = 16L) {
     st <- d$status
     col <- .mosaic_calrep_col(st$status)
     txt_col <- if (identical(st$status, "yellow")) "grey10" else "white"
     label <- st$label %|NA|% "Convergence status unavailable"
     ink <- .mosaic_calrep_ink()
     wrap <- 80L
     x0 <- grid::unit(4, "mm")
     kids <- list()
     y <- 0                                            # points from the top
     at <- function(yy) grid::unit(1, "npc") - grid::unit(yy, "pt")
     add_text <- function(txt, indent = 0, fontsize = 8, face = "plain", colour = ink, step = 1.4) {
          y <<- y + fontsize * step / 2
          kids[[length(kids) + 1L]] <<- grid::textGrob(
               txt, x = x0 + grid::unit(indent, "mm"), y = at(y), hjust = 0, vjust = 0.5,
               gp = grid::gpar(fontsize = fontsize, fontface = face, col = colour))
          y <<- y + fontsize * step / 2
     }
     # badge
     kids[[length(kids) + 1L]] <- grid::roundrectGrob(
          x = x0, y = at(4), width = grid::unit(1, "strwidth", label) * (13 / 12) * 1.08 + grid::unit(7, "mm"),
          height = grid::unit(25, "pt"), just = c("left", "top"), r = grid::unit(1.2, "mm"),
          gp = grid::gpar(fill = col, col = NA))
     kids[[length(kids) + 1L]] <- grid::textGrob(
          label, x = x0 + grid::unit(3.5, "mm"), y = at(16.5), hjust = 0, vjust = 0.5,
          gp = grid::gpar(fontsize = 13, fontface = "bold", col = txt_col))
     y <- 36
     ck <- st$checks
     keys <- .mosaic_calrep_check_keys()
     if (nrow(ck)) {
          for (i in seq_len(nrow(ck))) {
               c1 <- ck[i, , drop = FALSE]
               lab <- c1$label %|NA|% {
                    k <- keys[c1$check]
                    if (!is.na(k) && k %in% gl$key) gl$label[gl$key == k] else c1$check
               }
               thr <- .mosaic_calrep_threshold_text(c1)
               add_text(sprintf("%s: %s%s", lab, .mosaic_calrep_check_value(d, c1),
                                if (nzchar(thr)) sprintf("   (%s)", thr) else ""), indent = 4.2)
               kids[[length(kids) + 1L]] <- grid::pointsGrob(
                    x = x0 + grid::unit(1.4, "mm"), y = at(y - 8 * 1.4 / 2), pch = 16,
                    size = grid::unit(2.6, "mm"), gp = grid::gpar(col = .mosaic_calrep_col(c1$colour)))
          }
     } else {
          add_text("No check results in the diagnostics file.", colour = .mosaic_calrep_muted())
     }
     y <- y + 4
     if (length(st$reasons)) {
          add_text("Why not green:", face = "bold")
          rl <- .mosaic_calrep_reason_lines(st$reasons, wrap - 4L)
          # Every reason is printed: lines that do not fit here continue on the
          # next page (.mosaic_calrep_page1_layout(), round 3).
          if (length(rl) > max_reason_lines)
               rl <- c(rl[seq_len(max_reason_lines - 1L)], "  ... continued on the next page")
          for (l in rl) add_text(l, indent = 1, fontsize = 7.6, step = 1.3)
     } else {
          if (nrow(ck) || is.na(st$status))
               add_text(if (!is.na(st$status)) "All checks are green." else "No status in the diagnostics file.",
                        colour = .mosaic_calrep_muted())
     }
     y <- y + 2
     # Flags in words (convergence_flags_text), never the raw identifiers (R13).
     if (length(st$flags_text)) {
          add_text("Flags:", face = "bold")
          for (ft in st$flags_text) {
               w <- strwrap(ft, wrap - 4L)
               for (l in paste0(c("- ", rep("  ", length(w) - 1L)), w))
                    add_text(l, indent = 1, fontsize = 7.6, colour = "grey30", step = 1.3)
          }
     } else if (length(st$flags)) {
          add_text(paste("Flags:", gsub("_", " ", paste(st$flags, collapse = "; "))), face = "bold")
     } else {
          add_text("Flags: none", colour = .mosaic_calrep_muted())
     }
     list(grob = grid::gTree(children = do.call(grid::gList, kids)), height_pt = y + 8)
}

.mosaic_calrep_ess_plot <- function(d) {
     title <- "Posterior ESS as draws accumulate"
     p <- d$sampling$ess_path
     p <- p[is.finite(p$n) & is.finite(p$ess_all) & p$n > 0 & p$ess_all > 0, , drop = FALSE]
     if (!nrow(p)) return(.mosaic_calrep_unavailable(title, "No ESS path in the diagnostics file."))
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
               ggplot2::annotate("text", x = xl[1], y = tgt, label = paste0("target ", .mosaic_calrep_fmt(tgt)),
                                 hjust = 0, vjust = -0.5, size = 2.7, colour = .mosaic_calrep_tgt())
     }
     if (nrow(p) > 1L) g <- g + ggplot2::geom_line(colour = col, linewidth = 0.8)
     g + ggplot2::geom_point(data = p[nrow(p), , drop = FALSE], colour = col, size = 2) +
          ggplot2::scale_x_log10(labels = scales::label_comma(), limits = xl) +
          ggplot2::scale_y_log10(labels = scales::label_comma(accuracy = 1), breaks = 10^(0:7)) +
          ggplot2::coord_cartesian(ylim = c(min(1, yr[1]), yr[2] * 1.6)) +
          ggplot2::labs(title = title, x = NULL, y = "Kish ESS, all draws (log)",
                        subtitle = paste0(if (is.finite(slope)) sprintf("log-log slope %.2f (1 = grows in proportion to draws)", slope)
                                          else "log-log slope not available (too few points)",
                                          ";\ndotted guides = 0.1%, 1%, 10% of draws")) +
          .mosaic_calrep_theme(9) + ggplot2::theme(axis.text.x = ggplot2::element_blank())
}

.mosaic_calrep_path_xlim <- function(d) {
     n <- c(d$sampling$ess_path$n, d$sampling$loglik_path$n)
     n <- n[is.finite(n) & n > 0]
     if (!length(n)) return(c(1, 10))
     r <- range(n)
     if (r[1] == r[2]) r <- r * c(0.7, 1.4)
     r
}

.mosaic_calrep_loglik_plot <- function(d) {
     title <- "Best log-likelihood found so far (vs final best)"
     p <- d$sampling$loglik_path
     if (!nrow(p)) return(.mosaic_calrep_unavailable(title, "No log-likelihood path in the diagnostics file."))
     ll <- rbind(data.frame(n = p$n, gap = p$best_gap, what = "best draw so far"),
                 data.frame(n = p$n, gap = p$top100_gap, what = "100th best draw so far"))
     ll <- ll[is.finite(ll$n) & ll$n > 0 & is.finite(ll$gap), , drop = FALSE]
     if (!nrow(ll)) return(.mosaic_calrep_unavailable(title, "No log-likelihood path in the diagnostics file."))
     ll$what <- factor(ll$what, levels = c("best draw so far", "100th best draw so far"))
     eta <- d$run$eta
     lo <- min(ll$gap, -1)
     # every decade down to the lowest gap, so the whole axis is labelled; a
     # shallow range (under two decades) adds the 0.2 / 0.5 / 2 / 5 steps (R14)
     dec <- 10^(-1:max(0, ceiling(log10(-lo))))
     brk <- if (-lo < 100) -c(0, sort(unique(as.vector(outer(c(1, 2, 5), dec))))) else -c(0, 10^(0:max(0, ceiling(log10(-lo)))))
     brk <- brk[brk >= 1.5 * lo | brk == max(brk[brk < lo], na.rm = TRUE)]
     brk <- brk[brk == 0 | abs(brk) >= abs(lo) / 10]   # no crowding next to 0
     g <- ggplot2::ggplot(ll, ggplot2::aes(.data$n, .data$gap, colour = .data$what)) +
          ggplot2::geom_hline(yintercept = 0, colour = .mosaic_calrep_tgt(), linewidth = 0.3)
     g <- g + (if (nrow(p) > 1L) ggplot2::geom_line(linewidth = 0.7) else ggplot2::geom_point(size = 1.8))
     g + ggplot2::expand_limits(y = c(0, min(-1, lo))) +
          ggplot2::scale_colour_manual(values = c(`best draw so far` = "grey20", `100th best draw so far` = "grey60"),
                                      name = NULL, drop = TRUE) +
          ggplot2::scale_x_log10(labels = scales::label_comma(), limits = .mosaic_calrep_path_xlim(d)) +
          ggplot2::scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 1, base = 10),
                                      breaks = brk, labels = scales::label_comma()) +
          ggplot2::labs(title = title,
                        subtitle = paste0("signed log scale; a jump concentrates the weights (a dip in the ESS above).",
                                          if (is.finite(eta) && eta > 0)
                                               sprintf("\nA draw %s units below the best gets weight 1/e.",
                                                       .mosaic_calrep_fmt(1 / eta)) else ""),
                        x = "simulations (draws), log scale", y = "log-likelihood\nbelow final best") +
          .mosaic_calrep_theme(9) + ggplot2::theme(legend.position = "bottom")
}

# Page 1's left column: the badge with as many reason lines as fit beside both
# tables at their natural size; \code{overflow} holds the reason lines that
# continue on a page of their own (none are dropped, round 3).
.mosaic_calrep_page1_layout <- function(d, gl) {
     tabs <- .mosaic_calrep_sampling_tables(d)
     wrap <- c(Measure = 34, Value = 52)
     t1 <- .mosaic_calrep_table_grob(tabs$all[, c("Measure", "Value")], "All draws: the posterior", wrap)
     t2 <- .mosaic_calrep_table_grob(tabs$ensemble[, c("Measure", "Value")], "The ensemble: best-fitting draws", wrap)
     rl <- .mosaic_calrep_reason_lines(d$status$reasons, 76L)
     L_used <- length(rl)
     for (L in c(max(16L, length(rl)), 16L, 13L, 10L, 8L, 6L, 4L)) {
          badge <- .mosaic_calrep_badge(d, gl, max_reason_lines = L)
          L_used <- L
          if (badge$height_pt + 8 + attr(t1, "height_pt") + 18 + attr(t2, "height_pt") + 6 <= 515) break
     }
     overflow <- if (length(rl) > L_used) rl[-seq_len(L_used - 1L)] else character(0)
     list(badge = badge, t1 = t1, t2 = t2, overflow = overflow)
}

# The reasons that did not fit page 1, in full.
.mosaic_calrep_page_reasons <- function(d, lines, foot) {
     kids <- list(
          grid::textGrob(sprintf("Calibration diagnostics: %s", .mosaic_calrep_title(d)), x = 0.04, y = 0.955,
                         hjust = 0, gp = grid::gpar(fontface = "bold", fontsize = 15)),
          grid::textGrob("1. Enough simulations? Why not green (continued from the previous page)", x = 0.04, y = 0.915,
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

.mosaic_calrep_page_sampling <- function(d, gl, foot) {
     lay <- .mosaic_calrep_page1_layout(d, gl)
     badge <- lay$badge; t1 <- lay$t1; t2 <- lay$t2
     # One grob for the column: blocks at their natural heights stacked from the
     # top, the rest blank. If they would not fit the ~520 pt column, relative
     # heights instead, so nothing overflows into the page header.
     h <- c(badge$height_pt + 8, attr(t1, "height_pt") + 18, attr(t2, "height_pt") + 6)
     h <- if (sum(h) <= 515) grid::unit(c(h, 1), c("pt", "pt", "pt", "null")) else grid::unit(h, "null")
     left_grobs <- list(badge$grob, t1, t2, grid::nullGrob())[seq_along(h)]
     left <- patchwork::wrap_elements(full = do.call(gridExtra::arrangeGrob,
                                                     c(left_grobs, list(ncol = 1, heights = h))))
     pe <- .mosaic_calrep_safe("Posterior ESS as draws accumulate", .mosaic_calrep_ess_plot(d))
     pl <- .mosaic_calrep_safe("Best log-likelihood found so far", .mosaic_calrep_loglik_plot(d))
     right <- (pe / pl) + patchwork::plot_layout(heights = c(1.25, 1))
     p <- (left | right) + patchwork::plot_layout(widths = c(1.02, 1.3)) +
          .mosaic_calrep_header(d, "1. Enough simulations? Convergence status and ensemble ESS", foot)
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

.mosaic_calrep_mce_plot <- function(d, g, many) {
     title <- "Sampling precision of each posterior mean"
     g <- g[is.finite(g$mce), , drop = FALSE]
     if (!nrow(g)) return(.mosaic_calrep_unavailable(title, "No per-parameter Monte Carlo error in the diagnostics file."))
     ck <- .mosaic_calrep_check(d, "mce")
     thr_g <- ck$threshold_green; thr_y <- ck$threshold_yellow
     g$band <- if (is.finite(thr_g) && is.finite(thr_y))
          ifelse(g$mce <= thr_g, "green", ifelse(g$mce <= thr_y, "yellow", "red")) else "grey"
     g$lab <- if (many) sprintf("%s (n=%d)", g$g, g$n) else g$g
     g$lab <- factor(g$lab, levels = unique(g$lab[order(g$mce)]))
     vl <- c(thr_g, thr_y); vl <- vl[is.finite(vl)]
     p <- ggplot2::ggplot(g, ggplot2::aes(.data$mce, .data$lab, colour = .data$band))
     if (length(vl)) p <- p + ggplot2::geom_vline(xintercept = vl, linetype = c("dashed", "dotted")[seq_along(vl)],
                                                  colour = .mosaic_calrep_tgt())
     p <- p + (if (many) ggplot2::geom_segment(ggplot2::aes(x = .data$mce_lo, xend = .data$mce, yend = .data$lab),
                                               linewidth = 0.8, alpha = 0.35)
               else ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$mce, yend = .data$lab), linewidth = 0.3, alpha = 0.5))
     p90 <- d$mce_summary$p90
     sub <- paste0("Monte Carlo standard error of each posterior mean, as % of its posterior SD (10 seed batches):\n",
                   "the typical change a rerun with new seeds would make.",
                   if (length(vl)) sprintf("\nDashed / dotted = green / yellow limit of the 90th percentile (%s / %s).",
                                           .mosaic_calrep_pct_thr(thr_g), .mosaic_calrep_pct_thr(thr_y)) else "",
                   sprintf("\n90th percentile over parameters: %s.", .mosaic_calrep_pct(p90)),
                   if (many) " Point = worst location, bar = range." else "")
     p + ggplot2::geom_point(size = 1.4) +
          ggplot2::scale_colour_manual(values = .mosaic_calrep_rag(), guide = "none") +
          ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 0.1),
                                      limits = c(0, max(c(g$mce, vl), na.rm = TRUE) * 1.04),
                                      expand = ggplot2::expansion(mult = c(0.01, 0.02))) +
          ggplot2::labs(title = title, subtitle = sub, x = "Monte Carlo error / posterior SD", y = NULL) +
          .mosaic_calrep_theme(8.5) +
          ggplot2::theme(axis.text.y = ggplot2::element_text(size = .mosaic_calrep_label_size(nrow(g))))
}

.mosaic_calrep_kl_plot <- function(d, g, many) {
     title <- "Movement from the prior (KL divergence)"
     g <- g[is.finite(g$kl), , drop = FALSE]
     if (!nrow(g)) return(.mosaic_calrep_unavailable(title, "No per-parameter KL divergence in the diagnostics file."))
     edges <- .mosaic_calrep_kl_edges(d)
     levs <- c("largely prior", "partly informed", "data-informed")
     withheld_lab <- "band withheld (ensemble ESS < 100)"
     # A KL without a band was withheld (ensemble ESS below 100: the null bias
     # would put prior-only parameters in "partly informed"); drawn in grey (RN3).
     g$kl_band <- factor(ifelse(g$kl_band %in% levs, g$kl_band, withheld_lab), levels = c(levs, withheld_lab))
     fam <- ifelse(is.na(g$prior_family), "", g$prior_family)
     g$lab <- if (many) sprintf("%s (%s, n=%d)", g$g, fam, g$n) else ifelse(nzchar(fam), sprintf("%s (%s)", g$g, fam), g$g)
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
          ggplot2::labs(title = title,
                        subtitle = paste0("Prior to posterior, nats (log scale). Prior family in brackets.",
                                          sprintf("\nDashed = band edges %s and %s nats.", edges[1], edges[2]),
                                          if (many) "\nPoint = median over locations, bar = range." else ""),
                        x = "KL divergence (nats)", y = NULL) +
          .mosaic_calrep_theme(8.5) +
          ggplot2::theme(legend.position = "bottom",
                         axis.text.y = ggplot2::element_text(size = .mosaic_calrep_label_size(nrow(g)))) +
          ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2.2)))
}

.mosaic_calrep_page_parameters <- function(d, foot) {
     pp <- d$parameters
     many <- nrow(pp) > 60L
     g <- if (nrow(pp)) .mosaic_calrep_group_params(pp, many) else NULL
     pm <- if (is.null(g)) .mosaic_calrep_unavailable("Sampling precision of each posterior mean",
                                                      "No parameters in the diagnostics file.")
           else .mosaic_calrep_safe("Sampling precision of each posterior mean", .mosaic_calrep_mce_plot(d, g, many))
     pk <- if (is.null(g)) .mosaic_calrep_unavailable("Movement from the prior (KL divergence)",
                                                      "No parameters in the diagnostics file.")
           else .mosaic_calrep_safe("Movement from the prior (KL divergence)", .mosaic_calrep_kl_plot(d, g, many))
     ms <- d$mce_summary; ks <- d$kl_summary
     worst <- ms$worst[is.finite(ms$worst$mce), , drop = FALSE]
     worst_txt <- if (nrow(worst)) sprintf(" Largest: %s.", paste(sprintf("%s %s", utils::head(worst$parameter, 3),
                                                                          .mosaic_calrep_pct(utils::head(worst$mce, 3))),
                                                                  collapse = ", ")) else ""
     cnt <- function(x) if (is.finite(x)) .mosaic_calrep_fmt(x) else "n/a"
     txt <- paste0(sprintf("Left: sampling precision only. Median %s, 90th percentile %s, maximum %s of posterior SD over %s parameters.",
                           .mosaic_calrep_pct(ms$median), .mosaic_calrep_pct(ms$p90), .mosaic_calrep_pct(ms$max),
                           .mosaic_calrep_fmt(sum(is.finite(pp$mce)))), worst_txt,
                   if (isTRUE(ks$withheld > 0) && !isTRUE(ks$largely_prior + ks$partly_informed + ks$data_informed > 0))
                        sprintf("   Right: descriptive only, feeds no status; bands withheld for all %s parameters (ensemble ESS < 100, where the estimator's bias would misclassify them). ",
                                cnt(ks$withheld))
                   else sprintf("   Right: descriptive only, feeds no status (%s largely prior, %s partly informed, %s data-informed parameters%s). ",
                                cnt(ks$largely_prior), cnt(ks$partly_informed), cnt(ks$data_informed),
                                if (isTRUE(ks$withheld > 0)) sprintf("; %s withheld", cnt(ks$withheld)) else ""),
                   "Some priors are informative by design, so a parameter that stays near its prior is not a failure.")
     txt <- paste(strwrap(txt, width = 165), collapse = "\n")
     # A free grob, so patchwork does not align it to the plots' panel area
     # (which indented it under the left plot's axis labels).
     note <- patchwork::wrap_elements(full = grid::textGrob(
          txt, x = grid::unit(2, "mm"), y = grid::unit(1, "npc"), hjust = 0, vjust = 1,
          gp = grid::gpar(fontsize = 7.7, col = .mosaic_calrep_ink(), lineheight = 1.2)))
     p <- ((pm | pk) / note) + patchwork::plot_layout(heights = c(6, 0.55)) +
          .mosaic_calrep_header(d, "2. How informative? Parameter sampling precision and movement from the priors", foot)
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

.mosaic_calrep_ts_plot <- function(d, m) {
     title <- sprintf("Weekly %s: observed and ensemble", m)
     w <- d$fit$weekly
     w <- w[w$metric == m, , drop = FALSE]
     if (!nrow(w)) return(.mosaic_calrep_unavailable(title, sprintf("No scored weekly %s in the diagnostics file.", m)))
     many <- length(unique(w$location)) > 1L
     band_cols <- c("lo50", "hi50", "lo95", "hi95")
     has_bands <- !many && all(vapply(band_cols, function(cl) any(is.finite(w[[cl]])), logical(1)))
     x <- if (many) {
          a <- stats::aggregate(cbind(observed = w$observed, predicted = w$predicted), list(date = w$date),
                                function(v) if (all(!is.finite(v))) NA_real_ else sum(v, na.rm = TRUE))
          a
     } else w[, c("date", "observed", "predicted", band_cols)]
     x <- x[order(x$date), , drop = FALSE]
     # unscored weeks are absent: break the line across gaps longer than a week
     gap <- c(FALSE, diff(as.numeric(x$date)) > 7)
     if (any(gap)) {
          brk <- x[gap, , drop = FALSE]; brk$date <- brk$date - 1
          brk[, setdiff(names(brk), "date")] <- NA
          x <- rbind(x, brk); x <- x[order(x$date), , drop = FALSE]
     }
     col <- .mosaic_calrep_metric_colour(m)
     r <- d$run
     xl <- c(if (!is.na(r$date_start)) r$date_start else min(x$date), if (!is.na(r$date_stop)) r$date_stop else max(x$date) + 6)
     # Runs of consecutive scored weeks: a ribbon over a single isolated week is
     # a sliver, so such weeks get interval bars instead (WS6).
     obs_rows <- !is.na(x$observed) | !is.na(x$predicted)
     seg <- cumsum(c(TRUE, diff(as.numeric(x$date)) > 7))
     seg_n <- stats::ave(as.numeric(obs_rows), seg, FUN = sum)
     ribbon_x <- x[seg_n >= 2 | !obs_rows, , drop = FALSE]
     lone_x <- x[seg_n < 2 & obs_rows, , drop = FALSE]
     p <- ggplot2::ggplot(x, ggplot2::aes(.data$date))
     burn <- r$burn_in_days
     if (is.finite(burn) && burn > 0 && !is.na(r$date_start))
          p <- p + ggplot2::annotate("rect", xmin = r$date_start, xmax = r$date_start + burn, ymin = -Inf, ymax = Inf,
                                     fill = "grey93")
     if (has_bands) {
          p <- p + ggplot2::geom_ribbon(data = ribbon_x, ggplot2::aes(ymin = .data$lo95, ymax = .data$hi95), fill = col,
                                        alpha = 0.15, na.rm = TRUE) +
               ggplot2::geom_ribbon(data = ribbon_x, ggplot2::aes(ymin = .data$lo50, ymax = .data$hi50), fill = col,
                                    alpha = 0.25, na.rm = TRUE)
          if (nrow(lone_x))
               p <- p + ggplot2::geom_linerange(data = lone_x, ggplot2::aes(ymin = .data$lo95, ymax = .data$hi95),
                                                colour = col, alpha = 0.35, linewidth = 0.6, na.rm = TRUE) +
                    ggplot2::geom_linerange(data = lone_x, ggplot2::aes(ymin = .data$lo50, ymax = .data$hi50),
                                            colour = col, alpha = 0.6, linewidth = 1.2, na.rm = TRUE)
     }
     p <- p + ggplot2::geom_point(ggplot2::aes(y = .data$observed), size = 0.6, colour = "grey30", alpha = 0.7, na.rm = TRUE) +
          ggplot2::geom_line(ggplot2::aes(y = .data$predicted), colour = col, linewidth = 0.5, na.rm = TRUE)
     if (nrow(lone_x))
          p <- p + ggplot2::geom_point(data = lone_x, ggplot2::aes(y = .data$predicted), colour = col, size = 1, na.rm = TRUE)
     p +
          ggplot2::scale_x_date(limits = xl, expand = ggplot2::expansion(mult = 0.01)) +
          ggplot2::scale_y_continuous(labels = scales::label_comma()) +
          ggplot2::labs(title = title,
                        subtitle = paste0(if (many) "Summed over all locations. " else "",
                                          "Weekly totals on scored weeks (partial edge weeks left out). Points = observed; line = ensemble central line",
                                          if (has_bands) "; bands = 50% and 95% predictive intervals" else "",
                                          if (any(gap)) ". Gaps = weeks not scored" else "",
                                          if (has_bands && nrow(lone_x)) "; isolated weeks as interval bars" else "",
                                          if (is.finite(burn) && burn > 0) sprintf(". Shaded = %s-day burn-in, not scored.", .mosaic_calrep_fmt(burn)) else "."),
                        x = NULL, y = paste(m, "per week")) +
          .mosaic_calrep_theme(8.5)
}

.mosaic_calrep_page_fit <- function(d, foot) {
     tb <- d$fit$table
     fmt <- .mosaic_calrep_fmt; pct <- .mosaic_calrep_pct
     # One row per channel on every scored day (the definition of summary.json's
     # r2_*_ensemble / bias_ratio_*), then, when the run has surveillance
     # tiers, the same on tier 1-2 days only (D8).
     tb <- tb[order(match(tb$metric, c("cases", "deaths")), match(tb$subset, c("scored", "tier12"))), , drop = FALSE]
     has_tier12 <- any(tb$subset == "tier12")
     fit_df <- function(short) {
          chan <- .mosaic_calrep_title_case(tb$metric)
          days <- ifelse(tb$subset == "tier12", "tier 1-2 days", "all scored days")
          data.frame(Channel = if (has_tier12) sprintf("%s, %s", chan, days) else chan,
                     R2 = fmt(tb$r2, 2), Bias = fmt(tb$bias, 2),
                     a = pct(tb$cov50, 0), b = pct(tb$cov95, 0), WIS = .mosaic_calrep_sig(tb$wis, 3),
                     `Scored days` = fmt(tb$n_scored), check.names = FALSE) |>
               stats::setNames(c("Channel", "R\u00b2", "Bias (pred./obs.)",
                                 if (short) c("50% cov.", "95% cov.") else c("50% coverage", "95% coverage"),
                                 "WIS (per day)", "Scored days"))
     }
     fit_note <- paste0("Scored days count each channel's location-days with an observation after the burn-in; ",
                        "page 1's scored observation cells count location-days with a cases or a deaths observation. ",
                        if (has_tier12) "Tier 1-2 = observed and reconstructed weeks (imputed weeks left out). " else "",
                        "Bias = predicted / observed total; WIS in reported counts per location-day.")
     pl <- d$fit$per_location
     multi <- isTRUE(d$run$n_locations > 1) || length(unique(pl$location)) > 1L
     worst <- if (multi && nrow(pl))
          pl[pl$metric == "cases" & is.finite(pl$observed_total) & pl$observed_total > 0, , drop = FALSE] else pl[0, ]
     rank_note <- "lowest R\u00b2"
     if (nrow(worst)) {
          # Rank by R²; when no location has one (flat series, e.g. 47 Kenya
          # counties), by |log bias| instead, never alphabetically (WS6).
          if (all(!is.finite(worst$r2))) {
               lb <- abs(log(worst$bias)); lb[!is.finite(lb) & !is.na(worst$bias)] <- Inf
               worst <- worst[order(-ifelse(is.na(lb), -Inf, lb)), , drop = FALSE]
               rank_note <- "largest |log bias|; R\u00b2 unavailable"
          } else worst <- worst[order(worst$r2, na.last = FALSE), , drop = FALSE]
          worst <- worst[seq_len(min(8L, nrow(worst))), , drop = FALSE]
          wt <- .mosaic_calrep_table(
               stats::setNames(data.frame(worst$location, fmt(worst$observed_total), fmt(worst$r2, 2),
                                          fmt(worst$bias, 2), pct(worst$cov95, 0), stringsAsFactors = FALSE),
                               c("Location", "Observed cases", "R\u00b2", "Bias (pred./obs.)", "95% cov.")),
               sprintf("Worst-fitting locations, cases (%d of %d, %s)", nrow(worst), sum(pl$metric == "cases"), rank_note))
          ft <- if (nrow(tb)) .mosaic_calrep_table(fit_df(TRUE), "Fit metrics (central line vs observed)")
                else .mosaic_calrep_unavailable("Fit metrics", "No fit table in the diagnostics file.")
          h_top <- max(attr(wt, "height_pt"), attr(ft, "height_pt") %||% 60)
          top <- ft | wt
     } else if (nrow(tb)) {
          top <- .mosaic_calrep_table(fit_df(FALSE), "Fit metrics (central line vs observed, after the burn-in)")
          h_top <- attr(top, "height_pt")
     } else {
          top <- .mosaic_calrep_unavailable("Fit metrics", "No fit table in the diagnostics file.")
          h_top <- 60
     }
     mets <- .mosaic_calrep_metrics_present(d)
     ts <- lapply(mets, function(m) .mosaic_calrep_safe(sprintf("Weekly %s", m), .mosaic_calrep_ts_plot(d, m)))
     if (!length(ts))
          ts <- list(.mosaic_calrep_unavailable("Weekly observed and ensemble", "No scored weekly series in the diagnostics file."))
     missing <- setdiff(c("cases", "deaths"), mets)
     # heights in points of the ~470 pt panel area: tables and the note at their
     # natural size, the series share the rest
     note_pt <- if (length(missing) && length(mets)) 20 else 0
     fn_pt <- if (nrow(tb)) 22 else 0
     fixed <- h_top + 16 + note_pt + fn_pt
     heights <- c(h_top + 16, if (fn_pt) fn_pt, rep(max(120, 470 - fixed) / length(ts), length(ts)))
     panels <- c(list(top), if (fn_pt) list(.mosaic_calrep_note(paste(strwrap(fit_note, 190), collapse = "\n"))), ts)
     if (note_pt > 0) {
          panels <- c(panels, list(.mosaic_calrep_note(sprintf("%s: not scored in this run (no %s in the fit block).",
                                                               paste(.mosaic_calrep_title_case(missing), collapse = ", "),
                                                               paste(missing, collapse = " or ")))))
          heights <- c(heights, note_pt)
     }
     p <- patchwork::wrap_plots(panels, ncol = 1, heights = heights) +
          .mosaic_calrep_header(d, "3. Does it fit? Fit metrics and weekly series", foot)
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

.mosaic_calrep_page_scatter <- function(d, foot) {
     w <- d$fit$weekly
     w <- w[is.finite(w$observed) & is.finite(w$predicted), , drop = FALSE]
     many <- length(unique(w$location)) > 1L
     lims <- lapply(c(cases = "cases", deaths = "deaths"), function(m) {
          x <- w[w$metric == m, , drop = FALSE]
          if (!nrow(x)) return(NULL)
          r <- range(c(x$observed, x$predicted) + 1)
          c(r[1] / 1.6, r[2] * 1.6)
     })
     sp <- function(m) {
          title <- sprintf("Weekly %s: predicted vs observed", m)
          x <- w[w$metric == m, , drop = FALSE]
          if (!nrow(x)) return(.mosaic_calrep_unavailable(title, sprintf("%s not scored in this run.", .mosaic_calrep_title_case(m))))
          ggplot2::ggplot(x, ggplot2::aes(.data$observed + 1, .data$predicted + 1)) +
               ggplot2::geom_abline(slope = 1, intercept = 0, colour = .mosaic_calrep_tgt(), linetype = "dashed") +
               ggplot2::geom_point(colour = .mosaic_calrep_metric_colour(m), alpha = if (many) 0.25 else 0.55, size = 1.1) +
               ggplot2::scale_x_log10(labels = scales::label_comma(), limits = lims[[m]]) +
               ggplot2::scale_y_log10(labels = scales::label_comma(), limits = lims[[m]]) +
               ggplot2::labs(title = title,
                             subtitle = paste0("Scored weekly totals", if (many) ", all locations" else "",
                                               ", log(1 + count). Dashed = agreement; above = over-predicted."),
                             x = NULL, y = "predicted per week + 1") +
               .mosaic_calrep_theme(8.5)
     }
     dp <- function(m) {
          title <- sprintf("Weekly %s: distribution of counts", m)
          x <- w[w$metric == m, , drop = FALSE]
          if (!nrow(x)) return(.mosaic_calrep_unavailable(title, sprintf("%s not scored in this run.", .mosaic_calrep_title_case(m))))
          col <- .mosaic_calrep_metric_colour(m)
          dd <- rbind(data.frame(v = x$observed + 1, what = "observed"), data.frame(v = x$predicted + 1, what = "predicted"))
          ggplot2::ggplot(dd, ggplot2::aes(.data$v, fill = .data$what, colour = .data$what)) +
               ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), position = "identity",
                                       alpha = 0.35, bins = 30, linewidth = 0.2, na.rm = TRUE) +
               ggplot2::scale_fill_manual(values = c(observed = "grey40", predicted = col), name = NULL) +
               ggplot2::scale_colour_manual(values = c(observed = "grey40", predicted = col), name = NULL) +
               ggplot2::scale_x_log10(labels = scales::label_comma(), limits = lims[[m]]) +
               ggplot2::scale_y_sqrt() +
               ggplot2::labs(title = title,
                             subtitle = "Same x-axis as above. Overlap = the model reproduces how often each level occurs.",
                             x = "count per week + 1 (log scale)", y = "density (square-root scale)") +
               .mosaic_calrep_theme(8.5) + ggplot2::theme(legend.position = "bottom")
     }
     s <- function(title, expr) .mosaic_calrep_safe(title, expr)
     p <- ((s("Weekly cases", sp("cases")) | s("Weekly deaths", sp("deaths"))) /
           (s("Weekly cases", dp("cases")) | s("Weekly deaths", dp("deaths")))) +
          patchwork::plot_layout(heights = c(1.4, 1)) +
          .mosaic_calrep_header(d, "4. Does it fit? Weekly predicted vs observed", foot)
     patchwork::patchworkGrob(p)
}


# ---------------------------------------------------------------------------
# Appendix: the glossary
# ---------------------------------------------------------------------------

.mosaic_calrep_appendix <- function(gl, d = NULL) {
     wrap <- 92L
     # Only the metrics this report shows (R17), in glossary order.
     if (!is.null(d)) {
          keys <- .mosaic_calrep_metric_keys(d)
          if (any(gl$key %in% keys)) gl <- gl[gl$key %in% keys, , drop = FALSE]
     }
     # A check's threshold as this run applied it (status.checks[]), with the
     # glossary's text labelled as the default (R11).
     ck_of_key <- stats::setNames(names(.mosaic_calrep_check_keys()), .mosaic_calrep_check_keys())
     run_thr <- function(key) {
          if (is.null(d) || !key %in% names(ck_of_key)) return(NULL)
          ck <- .mosaic_calrep_check(d, ck_of_key[[key]])
          if (!nrow(ck) || is.na(ck$check)) return(NULL)
          t <- .mosaic_calrep_threshold_text(ck)
          if (nzchar(t)) paste("This run:", t) else NULL
     }
     lines <- list()
     for (i in seq_len(nrow(gl))) {
          e <- gl[i, ]
          ent <- list(list(txt = e$label, kind = "head"))
          has_thr <- !grepl("^none\\b", tolower(trimws(e$threshold)))
          rt <- run_thr(e$key)
          body <- c(e$definition, paste("Formula:", e$formula), paste("How to read:", e$how_to_read),
                    rt,
                    if (has_thr) paste(if (is.null(rt)) "Threshold (default):" else "Default:", e$threshold),
                    paste("Not a measure of:", e$not_a_measure_of))
          for (b in body) for (l in strwrap(b, width = wrap, exdent = 3))
               ent[[length(ent) + 1L]] <- list(txt = l, kind = "body")
          ent[[length(ent) + 1L]] <- list(txt = "", kind = "gap")
          lines <- c(lines, ent)
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
               (l$kind == "head" && length(cur) >= per_col - 3L)
          if (full) { cols[[length(cols) + 1L]] <- cur; cur <- list() }
          cur[[length(cur) + 1L]] <- l
     }
     if (length(cur)) cols[[length(cols) + 1L]] <- cur
     pages <- split(cols, ceiling(seq_along(cols) / 2))
     ver <- attr(gl, "version")
     lapply(seq_along(pages), function(i) {
          pg <- pages[[i]]
          function(foot) {
               kids <- list(
                    grid::textGrob(sprintf("Appendix %d of %d: what each metric means", i, length(pages)),
                                   x = 0.04, y = 0.955, hjust = 0, gp = grid::gpar(fontface = "bold", fontsize = 15)),
                    grid::textGrob(sprintf(paste0("From inst/extdata/metric_glossary.yml%s; the metrics in this report only.\n",
                                                  "Thresholds are conventions, not properties of the model: \"This run\" ",
                                                  "is what this run applied, \"default\" the package default."),
                                           if (!is.null(ver)) sprintf(" (version %s)", ver) else ""),
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
                                   gp = grid::gpar(fontsize = if (l$kind == "head") 9 else 7.8,
                                                   fontface = if (l$kind == "head") "bold" else "plain",
                                                   col = if (l$kind == "head") .mosaic_calrep_ink() else "grey25"))
                         yy <- yy - if (l$kind == "gap") 0.009 else 0.0163
                    }
               }
               grid::gTree(children = do.call(grid::gList, kids))
          }
     })
}
