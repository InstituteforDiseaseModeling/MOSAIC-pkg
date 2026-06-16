#' Backfill Short Reporting Gaps in a Weekly Case Series
#'
#' Linearly interpolates \emph{interior} runs of missing weekly case counts that
#' are (a) no longer than \code{max_interp_weeks} consecutive weeks, (b) bounded
#' by reported (non-\code{NA}) weeks on both sides, and (c) shouldered by reported
#' weeks that both exceed \code{min_anchor}. This targets the common surveillance
#' artifact where reporting lapses over public holidays (most often the Christmas
#' / New-Year weeks) leave a 1--2 week hole in an otherwise-reported series.
#'
#' Without backfilling, the suitability pipeline's NA\eqn{\to}0 sanitiser (the
#' keras target cannot contain \code{NaN}) stamps those weeks as
#' \emph{zero-incidence}, which during an active outbreak injects a false "no
#' transmission" signal into the training target. See
#' \code{\link{compile_suitability_data}} (the intended call site).
#'
#' \strong{What is and is not modified.} Only the qualifying interior gap cells
#' are written; \emph{reported anchor values are returned bit-identical} (the
#' rounding and non-negative clamp apply to filled cells only, never to observed
#' counts -- which may legitimately be fractional, e.g. disaggregated AI/monthly
#' values). A filled value that rounds to \code{0} is left \code{NA} (handed to
#' the downstream zero-fill) rather than written as a misleading "observed" zero.
#'
#' \strong{Interpolation basis.} Interpolation is done on the observation
#' \emph{date} (\code{\link[zoo]{na.approx}} with \code{x = date}), so an
#' unevenly-spaced row -- e.g. a dropped ISO week 53 leaving a 14-day step across
#' the New-Year gap -- is weighted by real elapsed time rather than as an even
#' row step. \code{maxgap} caps the run length (in consecutive missing weeks) and
#' \code{na.approx} never extrapolates, so leading / trailing \code{NA} (incl. the
#' post-cutoff forecast tail) and runs longer than \code{max_interp_weeks} are
#' left untouched.
#'
#' \strong{Guards.} (i) Gaps shouldered by a reported week at or below
#' \code{min_anchor} (default \code{0}, i.e. a reported zero) are \emph{not}
#' filled -- a near-zero shoulder is far more likely a genuine low-transmission /
#' non-outbreak week than a fillable outbreak interior, so it is left for the
#' downstream zero-fill. (ii) A missing row that merely duplicates the
#' \code{(group, date)} of a reported row (an upstream phantom-duplicate week) is
#' left untouched -- the observed row already represents that week.
#'
#' @param d A long data frame with one row per group-week, containing at least
#'   the columns named by \code{group_col}, \code{date_col} and \code{cases_col}.
#' @param max_interp_weeks Integer \eqn{\ge 1} (default \code{2L}). Maximum
#'   length, in consecutive missing weeks, of an interior gap that will be
#'   interpolated. Holiday lapses are typically 1--2 weeks.
#' @param method Interpolation method. Currently only \code{"linear"} (default,
#'   via \code{\link[zoo]{na.approx}}); exposed as an argument so richer methods
#'   (e.g. spline, seasonal) can be added later without changing call sites.
#' @param cases_col,group_col,date_col Column names (defaults \code{"cases"},
#'   \code{"iso_code"}, \code{"date"}).
#' @param flag_col Name of the logical column added to mark interpolated weeks
#'   (default \code{"cases_interpolated"}); set \code{TRUE} only for cells this
#'   function actually filled with a positive value.
#' @param min_anchor Numeric (default \code{0}). A gap is interpolated only when
#'   \emph{both} of its bounding reported weeks are strictly greater than
#'   \code{min_anchor}. The default leaves gaps shouldered by a reported zero
#'   unfilled (see Guards).
#' @param round_counts Logical (default \code{TRUE}). Round \emph{interpolated}
#'   values to whole counts (reported anchors are never rounded). Rounding uses
#'   R's round-half-to-even.
#' @param verbose Logical (default \code{TRUE}). Emit a one-line summary.
#'
#' @return \code{d} with \code{cases_col} backfilled in place for qualifying
#'   gaps, a logical \code{flag_col} marking the filled weeks, and the original
#'   row order preserved.
#'
#' @seealso \code{\link{compile_suitability_data}}
#' @export
backfill_weekly_case_gaps <- function(d,
                                      max_interp_weeks = 2L,
                                      method = c("linear"),
                                      cases_col = "cases",
                                      group_col = "iso_code",
                                      date_col  = "date",
                                      flag_col  = "cases_interpolated",
                                      min_anchor = 0,
                                      round_counts = TRUE,
                                      verbose = TRUE) {
     method <- match.arg(method)
     if (!requireNamespace("zoo", quietly = TRUE))
          stop("backfill_weekly_case_gaps() requires the 'zoo' package.", call. = FALSE)
     for (col in c(cases_col, group_col, date_col))
          if (!col %in% names(d))
               stop(sprintf("backfill_weekly_case_gaps(): column '%s' not found in `d`.", col),
                    call. = FALSE)
     max_interp_weeks <- as.integer(max_interp_weeks)
     if (is.na(max_interp_weeks) || max_interp_weeks < 1L)
          stop("backfill_weekly_case_gaps(): `max_interp_weeks` must be a positive integer.",
               call. = FALSE)
     if (!is.numeric(min_anchor) || length(min_anchor) != 1L || is.na(min_anchor))
          stop("backfill_weekly_case_gaps(): `min_anchor` must be a single non-NA number.",
               call. = FALSE)

     # Coerce cases to numeric, WARNING (not silently) on any value that fails to
     # parse, so a corrupt cell is not silently mistaken for a reporting gap.
     if (!is.numeric(d[[cases_col]])) {
          coerced <- suppressWarnings(as.numeric(d[[cases_col]]))
          n_bad <- sum(is.na(coerced) & !is.na(d[[cases_col]]))
          if (n_bad > 0L)
               warning(sprintf(
                    "backfill_weekly_case_gaps(): %d non-numeric '%s' value(s) coerced to NA (treated as gaps).",
                    n_bad, cases_col), call. = FALSE)
          d[[cases_col]] <- coerced
     }
     d[[date_col]] <- as.Date(d[[date_col]])

     # Sort by group then date so each group's weeks are contiguous; remember the
     # inverse permutation to restore the caller's row order on exit.
     ord     <- order(d[[group_col]], d[[date_col]])
     d       <- d[ord, , drop = FALSE]
     restore <- order(ord)

     cases  <- d[[cases_col]]
     grp    <- d[[group_col]]
     filled <- logical(length(cases))   # cells this function actually backfills

     for (g in unique(grp)) {
          idx <- which(grp == g)
          y0  <- cases[idx]
          if (sum(!is.na(y0)) < 2L) next            # need >= 2 anchors to interpolate
          xx  <- as.numeric(d[[date_col]][idx])

          # Date-weighted linear interpolation of bounded interior gaps <= maxgap.
          # Tied dates (upstream phantom-duplicate (group,date) rows) are collapsed
          # by na.approx; suppress that benign warning -- such fills are dropped
          # below by the duplicate guard.
          yf <- switch(method,
                       linear = suppressWarnings(
                            zoo::na.approx(y0, x = xx, maxgap = max_interp_weeks, na.rm = FALSE)))
          if (length(yf) != length(y0)) next         # defensive: shape must be preserved

          fill <- is.na(y0) & !is.na(yf)
          if (!any(fill)) next

          # (Guard 1) Near-zero / unbounded anchors: keep a filled run only when BOTH
          # bounding reported weeks exceed `min_anchor`. na.approx fills only runs
          # bounded by non-NA, so the rows immediately flanking a run are the anchors.
          runs   <- rle(fill)
          ends   <- cumsum(runs$lengths)
          starts <- ends - runs$lengths + 1L
          for (k in which(runs$values)) {
               s <- starts[k]; e <- ends[k]
               left  <- if (s > 1L)         y0[s - 1L] else NA_real_
               right <- if (e < length(y0)) y0[e + 1L] else NA_real_
               if (is.na(left) || is.na(right) || left <= min_anchor || right <= min_anchor)
                    fill[s:e] <- FALSE
          }

          # (Guard 2) Do not fabricate a value on a row that merely duplicates the
          # (group, date) of a reported row -- the observed row already covers it.
          obs_dates <- xx[!is.na(y0)]
          fill <- fill & !(xx %in% obs_dates)
          if (!any(fill)) next

          # Round / non-negative clamp apply ONLY to the cells we fill -- never to
          # reported anchors (which may be fractional). A fill that rounds to 0 is
          # NOT committed (left NA for the downstream zero-fill) and NOT flagged.
          vf <- yf[fill]
          if (isTRUE(round_counts)) vf <- round(vf)
          vf[!is.na(vf) & vf < 0] <- 0
          commit <- !is.na(vf) & vf > 0

          fill_idx <- idx[fill][commit]
          cases[fill_idx]  <- vf[commit]
          filled[fill_idx] <- TRUE
     }

     d[[cases_col]] <- cases
     d[[flag_col]]  <- filled

     if (isTRUE(verbose))
          message(sprintf(
               "  backfill_weekly_case_gaps: filled %d interior gap-week(s) across %d %s (method='%s', max_interp_weeks=%d, min_anchor=%g); %d week(s) left NA (long gaps / zero-bounded / series ends).",
               sum(filled), length(unique(grp[filled])), group_col,
               method, max_interp_weeks, min_anchor, sum(is.na(cases))))

     d <- d[restore, , drop = FALSE]
     rownames(d) <- NULL
     d
}
