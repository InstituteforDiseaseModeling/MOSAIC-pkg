# =============================================================================
# calc_model_ensemble_obs.R
#
# Observation-level posterior predictive for calc_model_ensemble() (v0.101.0).
#
# A member's simulated reported_cases / reported_deaths are ENGINE draws: they
# carry the transmission model's process noise, but not the observation noise
# the calibration likelihood assumes when it scores those trajectories against
# surveillance. Intervals built from engine draws alone are therefore too
# narrow for the observations (on the v0.100.1 national rehearsal, 95% coverage
# of observed weekly cases had median 0.87, 50% coverage 0.37). The functions
# here draw, for every member trajectory, an observation from the weekly
# observation model the dispersion was estimated for. It is the likelihood's own
# under cases_scoring = "weekly"; the default daily rule scores per-day cells at
# the same k and keeps this weekly predictive, which is wider than that
# likelihood implies (see .mosaic_resolve_observation_model()):
#
#   cases   weekly totals ~ NB(mu = the member's weekly total, size = k_j), with
#           k_j the per-location weekly dispersion the calibration scored with
#           (control$likelihood$.nb_k_cases_resolved, estimated on weekly
#           totals by est_nb_dispersion(); Inf = Poisson);
#   deaths  weekly totals with mean E_w (the member's expected reported deaths
#           given its onsets and its CFR draw) and variance phi_j * E_w, the
#           quasi-Poisson dispersion of the integrated deaths likelihood
#           (deaths_integration$dispersion).
#
# Weeks are the reporting weeks k is estimated on and the weekly cases rule scores
# (.mosaic_week_blocks() with the location's reporting-week offset), with the
# partial weeks at the edges of the window kept (.mosaic_observation_blocks()).
# The two likelihood floors -- the cases eps floor and the deaths background --
# are NOT part of the predictive: they price a zero-prediction cell in the
# score, they are not a data-generating process, and adding them would shift
# the predictive mean off the engine mean by a tuning constant.
# =============================================================================

#' Resolve the observation model a calibration run scores with
#'
#' Reads the per-location weekly cases dispersion that \code{run_MOSAIC()}
#' resolved once per calibration (\code{control$likelihood$.nb_k_cases_resolved},
#' the vector passed to every \code{calc_model_likelihood()} call) and the
#' reporting-week offset each location's weekly blocks start on (the
#' \code{week_offset} of the cases rows of
#' \code{control$likelihood$.nb_dispersion_table}). Inside \code{run_MOSAIC()}
#' that table always carries the offsets, a user-supplied \code{nb_k_cases}
#' included (\code{.mosaic_resolve_nb_dispersion()} detects them); for a table
#' that lacks them, or lacks a location, the offset is detected from the
#' observed cases over the scored window exactly as \code{est_nb_dispersion()}
#' detects it.
#'
#' The observation model is the same under both cases scoring rules: one
#' negative binomial at size \eqn{k} per reporting-week total. With
#' \code{cases_scoring = "weekly"} it is the likelihood's own. The default
#' \code{"daily"} rule scores each day as its own negative binomial cell at the
#' same \eqn{k}, which implies a weekly variance of about \eqn{C + C^2/(7k)}
#' for a weekly total \eqn{C}; the predictive keeps \eqn{C + C^2/k}, so a
#' default run's intervals are wider than its likelihood implies (and are not
#' the engine-level intervals runs made before v0.101.0 reported).
#'
#' @param config The calibration config (\code{location_name},
#'   \code{reported_cases}, \code{date_start}).
#' @param control The resolved control list.
#' @return \code{list(k_cases, week_offset)}, one value per location in config
#'   order, or \code{NULL} when no dispersion has been resolved.
#' @noRd
.mosaic_resolve_observation_model <- function(config, control) {
  lik <- control$likelihood
  k <- lik$.nb_k_cases_resolved
  if (is.null(k) || !length(k)) return(NULL)
  k <- as.numeric(k)
  n_loc <- length(k)
  locs <- as.character(config$location_name)
  off <- rep(NA_integer_, n_loc)
  tab <- lik$.nb_dispersion_table
  if (is.data.frame(tab) && all(c("channel", "location", "week_offset") %in% names(tab)) &&
      length(locs) == n_loc) {
    tc <- tab[tab$channel == "cases", , drop = FALSE]
    off <- suppressWarnings(as.integer(tc$week_offset[match(locs, tc$location)]))
  }
  if (anyNA(off)) {
    obs <- config$reported_cases
    if (!is.null(obs) && !is.matrix(obs)) obs <- matrix(obs, nrow = 1L)
    d0 <- tryCatch(as.Date(config$date_start), error = function(e) NA)
    if (is.null(obs) || nrow(obs) != n_loc || is.na(d0)) {
      off[is.na(off)] <- 0L
    } else {
      idx <- suppressWarnings(as.integer(lik$.score_window_resolved$idx_cases %||% 1L))
      if (length(idx) != 1L || is.na(idx) || idx < 1L || idx > ncol(obs)) idx <- 1L
      keep <- idx:ncol(obs)
      dates <- d0 + keep - 1L
      for (j in which(is.na(off))) {
        cad <- .nb_disp_cadence(obs[j, keep], dates)
        off[j] <- as.integer(cad$offset)
      }
    }
  }
  list(k_cases = k, week_offset = off)
}

#' Validate and normalise an observation-model specification
#'
#' Accepts the list form (\code{k_cases}, optional \code{week_offset}) or the
#' dispersion table written to \code{2_calibration/diagnostics/nb_dispersion.csv}
#' (columns \code{k} and optionally \code{channel}, \code{location},
#' \code{week_offset}; only the cases rows are used, matched by location name).
#'
#' @param observation_model \code{NULL}, a list or a data frame (see above).
#' @param location_names Location names of the ensemble, in row order.
#' @return \code{NULL}, or \code{list(k_cases, week_offset)} with one value per
#'   location.
#' @noRd
.mosaic_normalize_observation_model <- function(observation_model, location_names) {
  if (is.null(observation_model)) return(NULL)
  n_loc <- length(location_names)
  if (is.data.frame(observation_model)) {
    tab <- observation_model
    if (!"k" %in% names(tab))
      stop("observation_model: a data frame must have a `k` column (the format of ",
           "2_calibration/diagnostics/nb_dispersion.csv).", call. = FALSE)
    if ("channel" %in% names(tab)) tab <- tab[tab$channel == "cases", , drop = FALSE]
    if ("location" %in% names(tab)) {
      i <- match(location_names, as.character(tab$location))
      if (anyNA(i))
        stop("observation_model: no cases dispersion for ",
             paste(location_names[is.na(i)], collapse = ", "), ".", call. = FALSE)
      tab <- tab[i, , drop = FALSE]
    }
    observation_model <- list(k_cases = tab$k,
                              week_offset = if ("week_offset" %in% names(tab)) tab$week_offset else 0L)
  }
  if (!is.list(observation_model) || is.null(observation_model$k_cases))
    stop("observation_model must be NULL, a list with `k_cases` (and optionally ",
         "`week_offset`), or the nb_dispersion.csv table.", call. = FALSE)
  rep_loc <- function(x, nm) {
    if (length(x) == 1L) x <- rep(x, n_loc)
    if (length(x) != n_loc)
      stop(sprintf("observation_model$%s must have length 1 or %d (one per location).",
                   nm, n_loc), call. = FALSE)
    x
  }
  k <- as.numeric(rep_loc(observation_model$k_cases, "k_cases"))
  if (any(!(is.infinite(k) & k > 0) & !(is.finite(k) & k > 0)))
    stop("observation_model$k_cases must be positive and finite, or Inf (Poisson).",
         call. = FALSE)
  off <- suppressWarnings(as.integer(rep_loc(observation_model$week_offset %||% 0L,
                                             "week_offset")))
  off[is.na(off)] <- 0L
  if (any(off < 0L | off > 6L))
    stop("observation_model$week_offset must be in 0-6 (days after Monday).", call. = FALSE)
  list(k_cases = k, week_offset = off)
}

#' Weekly blocks of the ensemble's daily grid, per location
#'
#' The reporting weeks of \code{.mosaic_week_blocks()} -- the weeks the weekly
#' cases likelihood scores, Monday-anchored and shifted by the location's
#' reporting-week offset -- so the noise is applied to the same weekly totals
#' the dispersion was estimated on. Unlike the likelihood, which drops a week
#' cut by the start or end of the window (\code{partial = "drop"}), the
#' predictive keeps it as a block of the days it has (\code{partial = "keep"}):
#' every day gets an observation-level draw, including the burn-in days that are
#' never scored.
#'
#' @param n_time Number of daily columns.
#' @param date_start Date of the first column (\code{NULL}: weeks counted from
#'   column 1, as if it were a Monday).
#' @param week_offset Integer offset per location.
#' @return A list, one element per location, with \code{block} (block index of
#'   each day, 1-based and contiguous), \code{start} and \code{end} (first and
#'   last day of each block).
#' @noRd
.mosaic_observation_blocks <- function(n_time, date_start, week_offset) {
  d0 <- if (is.null(date_start)) NA else tryCatch(as.Date(date_start), error = function(e) NA)
  # Undated: the block anchor is a Monday, so a grid starting on it counts weeks
  # from column 1.
  if (is.na(d0)) d0 <- .NB_DISP_ANCHOR
  dates <- d0 + seq_len(n_time) - 1L
  one <- function(off) {
    b <- .mosaic_week_blocks(dates, off, partial = "keep")
    list(block = b$index, start = b$start, end = b$end)
  }
  by_off <- lapply(sort(unique(week_offset)), one)
  names(by_off) <- as.character(sort(unique(week_offset)))
  lapply(week_offset, function(o) by_off[[as.character(o)]])
}

#' Observation-level draw of one location's daily cases
#'
#' Each block total \eqn{Y_w \sim \mathrm{NB}(\mu = C_w, \mathrm{size} = k)}
#' around the member's block total \eqn{C_w} (Poisson when \code{k = Inf}), then
#' apportioned to the block's days in proportion to the member's daily counts by
#' systematic sampling: with \eqn{U_w \sim U(0,1)} and \eqn{S_t} the within-block
#' cumulative share, day \eqn{t} receives
#' \eqn{\lfloor U_w + Y_w S_t \rfloor - \lfloor U_w + Y_w S_{t-1} \rfloor}.
#' Every day gets the floor or ceiling of its exact share \eqn{Y_w c_t / C_w},
#' with expectation exactly that share; block totals equal \eqn{Y_w} exactly and
#' a day the member puts no cases on gets none. So
#' \eqn{E[y_t \mid \text{member}] = c_t} and the observation noise is
#' mean-preserving.
#'
#' @param x Member's daily reported cases (finite, non-negative).
#' @param blk One element of \code{.mosaic_observation_blocks()}.
#' @param k Weekly NB size.
#' @return Numeric vector, the observation-level daily cases.
#' @noRd
.mosaic_obs_cases_row <- function(x, blk, k) {
  if (!length(x) || any(!is.finite(x))) return(x)
  cs <- cumsum(x)
  end <- blk$end
  before <- c(0, cs[end[-length(end)]])          # cumulative total before each block
  C <- cs[end] - before
  nb <- length(C)
  Y <- numeric(nb)
  pos <- C > 0
  if (any(pos))
    Y[pos] <- if (is.infinite(k)) stats::rpois(sum(pos), C[pos])
              else stats::rnbinom(sum(pos), size = k, mu = C[pos])
  U <- stats::runif(nb)
  b <- blk$block
  q <- numeric(length(x))
  pd <- pos[b]
  q[pd] <- Y[b][pd] * (cs[pd] - before[b][pd]) / C[b][pd]
  q[end] <- Y                                    # block totals exact, no rounding drift
  fl <- floor(U[b] + q)
  prev <- c(0, fl[-length(fl)])
  prev[blk$start] <- 0                           # floor(U + 0) = 0 at each block start
  fl - prev
}

#' Observation-level draw of one location's daily deaths
#'
#' Couples the observation to the member's engine deaths. With \eqn{E_w} the
#' block's expected reported deaths and \eqn{\phi} the deaths dispersion, draw
#' \eqn{G_w \sim \mathrm{Gamma}(E_w/(\phi-1), E_w/(\phi-1))} (mean 1); a day's
#' engine deaths \eqn{r_t} are thinned, \eqn{d_t \sim \mathrm{Binom}(r_t, G_w)},
#' when \eqn{G_w < 1}, and topped up, \eqn{d_t = r_t +
#' \mathrm{Pois}(e_t (G_w - 1))}, when \eqn{G_w > 1}. The engine deaths are
#' binomial thinnings of the onsets with mean \eqn{e_t} (Poisson to within the
#' small reported-CFR factor), so \eqn{d_t \mid G_w \sim
#' \mathrm{Pois}(e_t G_w)} and the block total is
#' \eqn{\mathrm{NB}(\mu = E_w, \mathrm{size} = E_w/(\phi - 1))}: mean
#' \eqn{E_w} and variance \eqn{\phi E_w}, the quasi-Poisson variance the deaths
#' likelihood scores with. At \eqn{\phi = 1} the engine deaths are returned
#' unchanged, since they already carry the Poisson part.
#'
#' @param r Member's daily reported deaths (the engine or post-hoc redraw).
#' @param e Member's daily expected reported deaths.
#' @param blk One element of \code{.mosaic_observation_blocks()}.
#' @param phi Quasi-Poisson dispersion (>= 1).
#' @return Numeric vector, the observation-level daily deaths.
#' @noRd
.mosaic_obs_deaths_row <- function(r, e, blk, phi) {
  if (!is.finite(phi) || phi <= 1 || !length(r) || length(e) != length(r) ||
      any(!is.finite(r)) || any(!is.finite(e))) return(r)
  ce <- cumsum(e)
  end <- blk$end
  E <- ce[end] - c(0, ce[end[-length(end)]])
  G <- rep(1, length(E))
  pos <- E > 0
  if (any(pos)) {
    a <- E[pos] / (phi - 1)
    G[pos] <- stats::rgamma(sum(pos), shape = a, rate = a)
  }
  g <- G[blk$block]
  out <- r
  dn <- g < 1 & r > 0
  if (any(dn)) out[dn] <- stats::rbinom(sum(dn), size = r[dn], prob = g[dn])
  up <- g > 1 & e > 0
  if (any(up)) out[up] <- r[up] + stats::rpois(sum(up), e[up] * (g[up] - 1))
  out
}

#' Draw one ensemble member's observation-level cases and deaths
#'
#' Seeded locally with the engine's generator (\code{.sim_rng_begin()}), the
#' caller's stream restored, so a member's draw depends only on its seed: the
#' same in a sequential and a parallel run, and whatever order the members are
#' gathered in.
#'
#' @param cases,deaths Member's engine matrices \code{[n_loc x n_time]}.
#' @param expected_deaths Member's expected reported deaths, same shape, or
#'   \code{NULL} (deaths then returned unchanged).
#' @param blocks From \code{.mosaic_observation_blocks()}.
#' @param k_cases Weekly cases NB size per location.
#' @param phi_deaths Deaths dispersion per location, or \code{NULL}.
#' @param seed Integer seed for this member.
#' @return \code{list(cases, deaths)}, observation-level matrices.
#' @noRd
.mosaic_observation_draw <- function(cases, deaths, expected_deaths, blocks,
                                     k_cases, phi_deaths, seed) {
  state <- .sim_rng_begin(seed)
  on.exit(.sim_rng_end(state), add = TRUE)
  for (j in seq_len(nrow(cases)))
    cases[j, ] <- .mosaic_obs_cases_row(cases[j, ], blocks[[j]], k_cases[j])
  if (!is.null(phi_deaths) && !is.null(expected_deaths)) {
    for (j in seq_len(nrow(deaths)))
      deaths[j, ] <- .mosaic_obs_deaths_row(deaths[j, ], expected_deaths[j, ],
                                            blocks[[j]], phi_deaths[j])
  }
  list(cases = cases, deaths = deaths)
}

#' Engine-level prediction array of a mosaic_ensemble
#'
#' The member trajectories BEFORE observation noise: what the medoid, R_eff,
#' trajectory, implied-CFR and subset-selection consumers need. Since v0.101.0
#' \code{cases_array}/\code{deaths_array} hold observation-level draws and the
#' engine draws are \code{cases_engine_array}/\code{deaths_engine_array}. An
#' ensemble saved before v0.101.0 carries only \code{cases_array}, which was
#' engine-level, so it is returned as the fallback -- but only for a channel
#' that received no observation noise (\code{ens$observation_model}): an
#' observation-model ensemble whose engine arrays were removed has no engine
#' array to offer, and handing back its observation draws would let the medoid
#' and subset selection score noise without an error.
#'
#' @param ens A \code{mosaic_ensemble}.
#' @param chan \code{"cases"} or \code{"deaths"}.
#' @return The 4-D engine array, or \code{NULL} when the arrays were stripped.
#' @noRd
.mosaic_engine_array <- function(ens, chan = c("cases", "deaths")) {
  chan <- match.arg(chan)
  eng <- ens[[paste0(chan, "_engine_array")]]
  if (!is.null(eng) || isTRUE(ens$observation_model[[chan]])) return(eng)
  ens[[paste0(chan, "_array")]]
}
