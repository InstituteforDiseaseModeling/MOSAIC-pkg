#' Detect outbreak onsets for every location of a MOSAIC config
#'
#' Builds each location's weekly series of reported suspected cases from a
#' MOSAIC config and runs \code{\link{calc_outbreak_onset}} on it. Weeks are the
#' complete Monday-to-Sunday weeks of the config's daily window (partial edge
#' weeks are dropped); a week is reported when all seven of its days are, and
#' its count is their sum.
#'
#' Imputed days (\code{reported_tier} 3, every modelled disaggregation method,
#' e.g. AI \code{fourier_*} reconstructions of annual totals and
#' \code{inferred_zero} absences) are treated as unreported
#' by default, as \code{\link{est_epidemic_peaks}} does: a Fourier
#' reconstruction has a seasonal peak every year by construction. With
#' \code{imputed = "keep"} they count as reported, which reproduces the series
#' the onset handoff was tuned on. WHO multi-week reports spread over their
#' weeks (tier 2) count as reported either way.
#'
#' @param config A MOSAIC config (e.g. \code{config_default}) with \code{date_start}, \code{date_stop},
#'   \code{location_name}, \code{reported_cases} and, optionally, \code{reported_tier}.
#' @param method \code{"growth"} (default), \code{"growth_sensitive"} or \code{"zheng"}; see
#'   \code{\link{calc_outbreak_onset}}.
#' @param mode \code{"retrospective"} (default) or \code{"realtime"}.
#' @param imputed \code{"unreported"} (default) treats tier-3 days as unreported; \code{"keep"} counts them.
#' @param population \code{NULL} (default) reads \code{param_N_population_size.csv} from
#'   \code{get_paths()$MODEL_INPUT}; or a data frame with columns \code{iso_code}, \code{date},
#'   \code{population}. Each week takes the latest value on or before its first day (the earliest
#'   value for weeks before the first date).
#' @param locations ISO codes to run (default: every location of the config).
#' @param ... Rule settings passed to \code{\link{calc_outbreak_onset}}.
#'
#' @return A data frame of the columns of \code{\link{calc_outbreak_onset}} with \code{iso_code} first and
#'   \code{imputed_share} added: the share of the config's reported cases within two weeks of the onset
#'   that come from imputed days. The attribute \code{"series"} stacks the locations' weekly series with
#'   an \code{iso_code} column.
#'
#' @examples
#' \dontrun{
#' set_root_directory("~/MOSAIC")
#' onsets <- est_outbreak_onsets(config_default)
#' onsets_rt <- est_outbreak_onsets(config_default, mode = "realtime", locations = c("MOZ", "ETH"))
#' }
#'
#' @seealso \code{\link{calc_outbreak_onset}}
#' @export
est_outbreak_onsets <- function(config,
                                method = c("growth", "growth_sensitive", "zheng"),
                                mode = c("retrospective", "realtime"),
                                imputed = c("unreported", "keep"),
                                population = NULL,
                                locations = NULL,
                                ...) {

     method <- match.arg(method)
     mode <- match.arg(mode)
     imputed <- match.arg(imputed)
     need <- c("date_start", "date_stop", "location_name", "reported_cases")
     miss <- need[!vapply(need, function(k) !is.null(config[[k]]), logical(1))]
     if (length(miss)) stop("config is missing: ", paste(miss, collapse = ", "), call. = FALSE)

     dates <- seq(as.Date(config$date_start), as.Date(config$date_stop), by = "day")
     cases <- as.matrix(config$reported_cases)
     iso_all <- as.character(config$location_name)
     if (nrow(cases) != length(iso_all) || ncol(cases) != length(dates)) {
          stop("config$reported_cases must be locations x days of [date_start, date_stop].", call. = FALSE)
     }
     tier <- if (is.null(config$reported_tier)) NULL else as.matrix(config$reported_tier)
     if (!is.null(tier) && !identical(dim(tier), dim(cases))) {
          stop("config$reported_tier must have the dimensions of config$reported_cases.", call. = FALSE)
     }
     if (is.null(locations)) locations <- iso_all
     locations <- unique(as.character(locations))
     if (!length(locations)) stop("locations is empty.", call. = FALSE)
     bad <- setdiff(locations, iso_all)
     if (length(bad)) stop("locations not in the config: ", paste(bad, collapse = ", "), call. = FALSE)

     blocks <- .mosaic_week_blocks(dates, offset = 0L, partial = "drop")
     week_start <- blocks$week_start
     if (!length(week_start)) stop("The config window holds no complete week.", call. = FALSE)
     pop <- .onset_population(population, locations, week_start)

     week_sum <- function(x) vapply(seq_along(blocks$start), function(b) sum(x[blocks$start[b]:blocks$end[b]]),
                                    numeric(1))
     results <- vector("list", length(locations))
     series <- vector("list", length(locations))
     for (k in seq_along(locations)) {
          j <- match(locations[k], iso_all)
          y <- cases[j, ]
          imp <- if (is.null(tier)) logical(length(y)) else !is.na(tier[j, ]) & tier[j, ] == 3L
          y_use <- if (imputed == "unreported") replace(y, imp, NA) else y
          n_rep <- week_sum(!is.na(y_use))
          weekly <- week_sum(ifelse(is.na(y_use), 0, y_use))
          observed <- n_rep == 7
          raw_all <- week_sum(ifelse(is.na(y), 0, y))
          raw_imp <- week_sum(ifelse(is.na(y) | !imp, 0, y))

          res <- calc_outbreak_onset(ifelse(observed, weekly, NA_real_), week_start, pop[[locations[k]]],
                                     observed = observed, method = method, mode = mode, ...)
          w <- match(res$onset_week, week_start)
          share <- vapply(w, function(i) {
               win <- max(1L, i - 2L):min(length(week_start), i + 2L)
               tot <- sum(raw_all[win])
               if (tot > 0) sum(raw_imp[win]) / tot else NA_real_
          }, numeric(1))
          s <- attr(res, "series")
          attr(res, "series") <- NULL
          results[[k]] <- cbind(data.frame(iso_code = rep(locations[k], nrow(res)), stringsAsFactors = FALSE),
                                res, imputed_share = share)
          series[[k]] <- cbind(data.frame(iso_code = rep(locations[k], nrow(s)), stringsAsFactors = FALSE), s)
     }
     out <- do.call(rbind, results)
     rownames(out) <- NULL
     ser <- do.call(rbind, series)
     rownames(ser) <- NULL
     attr(out, "series") <- ser
     out
}


# Population per location and week: a named list of numeric vectors over
# `week_start`, from the model input file (population = NULL) or a data frame
# (iso_code, date, population). Each week takes the latest value on or before
# its first day; weeks before the first date take the earliest value.
.onset_population <- function(population, locations, week_start) {
     if (is.null(population)) {
          f <- file.path(get_paths()$MODEL_INPUT, "param_N_population_size.csv")
          if (!file.exists(f)) stop("Population file not found: ", f, call. = FALSE)
          p <- data.table::fread(f, select = c("j", "t", "parameter_value"))
          population <- data.frame(iso_code = p$j, date = as.Date(p$t), population = p$parameter_value)
     }
     if (!is.data.frame(population) || !all(c("iso_code", "date", "population") %in% names(population))) {
          stop("population must be NULL or a data frame with iso_code, date and population.", call. = FALSE)
     }
     out <- lapply(locations, function(iso) {
          p <- population[population$iso_code == iso, , drop = FALSE]
          if (!nrow(p)) stop("No population for ", iso, ".", call. = FALSE)
          p <- p[order(as.Date(p$date)), , drop = FALSE]
          i <- findInterval(as.numeric(week_start), as.numeric(as.Date(p$date)))
          as.numeric(p$population[pmax(i, 1L)])
     })
     names(out) <- locations
     out
}
