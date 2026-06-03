#' Filter epidemic peaks to a simulation window (and optional locations)
#'
#' Single source of truth for the "drop peaks outside this simulation
#' window" filter that the config builders, the runtime config injector,
#' and the multi-component likelihood all share.
#'
#' Without this filter, `which.min(abs(date_seq - peak_date))` silently
#' snaps out-of-window peaks to t=1 or t=N, biasing the peak-timing and
#' peak-magnitude shape terms in [calc_model_likelihood()]. The package
#' dataset [epidemic_peaks] spans the full surveillance record (2010+)
#' and is **not** pre-trimmed to any particular config window, so every
#' consumer must filter.
#'
#' @param peaks          A data.frame of detected peaks. Must contain an
#'                       `iso_code` column and a `peak_date` column
#'                       coercible by `as.Date()`.
#' @param date_start     Inclusive lower bound of the simulation window.
#' @param date_stop      Inclusive upper bound of the simulation window.
#' @param location_names Optional character vector of ISO codes. When
#'                       supplied, peaks for any other location are also
#'                       dropped.
#'
#' @return A data.frame with the same columns as `peaks`, containing
#'         only the rows that survive the date window filter (and the
#'         optional location filter). Returns the input unchanged if it
#'         is NULL or zero-row.
#'
#' @keywords internal
#' @noRd
.filter_epidemic_peaks <- function(peaks,
                                   date_start,
                                   date_stop,
                                   location_names = NULL) {

     if (is.null(peaks) || !nrow(peaks)) return(peaks)
     if (!"peak_date" %in% names(peaks)) {
          stop("peaks must contain a 'peak_date' column")
     }
     if (!is.null(location_names) && !"iso_code" %in% names(peaks)) {
          stop("peaks must contain an 'iso_code' column when location_names is supplied")
     }

     pd  <- suppressWarnings(as.Date(peaks$peak_date))
     ds  <- as.Date(date_start)
     dp  <- as.Date(date_stop)
     keep <- !is.na(pd) & pd >= ds & pd <= dp

     if (!is.null(location_names)) {
          keep <- keep & peaks$iso_code %in% location_names
     }

     peaks[keep, , drop = FALSE]
}
