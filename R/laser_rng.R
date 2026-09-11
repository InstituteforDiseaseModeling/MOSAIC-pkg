#' Stochastic draw sites for the R transmission engine
#'
#' The engine reaches randomness only through \code{.laser_binom()} and
#' \code{.laser_pois()}. Routing every draw through two functions is what makes
#' the Tier B replay strategy possible: in \code{"replay"} mode they return
#' recorded results from the Python oracle instead of drawing, while asserting
#' that the R engine asked for exactly the draw the oracle made, at the same
#' tick, in the same phase, at the same site.
#'
#' @section RNG contract:
#' A simulation's output is determined solely by its \code{seed} and
#' \code{config} -- never by worker identity, batch position, or how much
#' randomness was consumed earlier in the session. \code{.laser_rng_state()}
#' establishes an isolated stream and returns everything needed to restore the
#' caller's \code{.Random.seed} afterwards, so calling the engine never
#' perturbs the caller's stream.
#'
#' @name laser_rng
#' @keywords internal
NULL

# The draw-site registry. Every call site in the engine passes a `site` label
# from this vector, so a replay mismatch names the site rather than a call
# index. Labels mirror the Python source locations they were ported from.
.LASER_DRAW_SITES <- c(
     "susceptible/non_disease_deaths",   # susceptible.py:135
     "susceptible/births",               # susceptible.py:143
     "exposed/non_disease_deaths",       # exposed.py:94
     "recovered/non_disease_deaths",     # recovered.py:103
     "recovered/waning",                 # recovered.py:110
     "infectious/sym_non_disease_deaths",# infectious.py:181
     "infectious/asym_non_disease_deaths", # infectious.py:203
     "infectious/disease_deaths",        # infectious.py:210
     "infectious/sym_recovery",          # infectious.py:217
     "infectious/asym_recovery",         # infectious.py:231
     "infectious/progression",           # infectious.py:239
     "infectious/sigma_split",           # infectious.py:249
     "infectious/reported_cases",        # infectious.py:267
     "vaccinated/v1_non_disease_deaths", # vaccinated.py:157
     "vaccinated/v2_non_disease_deaths", # vaccinated.py:163
     "vaccinated/v1_waning",             # vaccinated.py:170
     "vaccinated/v2_waning",             # vaccinated.py:175
     "humantohuman/infection",           # humantohuman.py:165
     "envtohuman/infection",             # envtohuman.py:133
     "environmental/decay",              # environmental.py:135
     "environmental/shedding_sym",       # environmental.py:139
     "environmental/shedding_asym"       # environmental.py:143
)

#' Create a draw controller for one simulation
#'
#' @param mode Either \code{"rng"} (draw from R's generator) or \code{"replay"}
#'   (pop recorded draws from \code{record} and assert they match).
#' @param seed Integer seed. Used in \code{"rng"} mode.
#' @param record Replay record from \code{laser_read_fixture()}. Required when
#'   \code{mode = "replay"}.
#' @param tol_rel,tol_abs Combined tolerance for comparing draw parameters
#'   against the record: \code{abs(a - b) <= tol_abs + tol_rel * abs(b)}. A
#'   purely relative tolerance is wrong here because rates legitimately reach
#'   exactly zero in low-transmission patches.
#' @return A draw controller (an environment).
#' @keywords internal
laser_draws <- function(mode = c("rng", "replay"),
                        seed = NULL,
                        record = NULL,
                        tol_rel = 1e-6,
                        tol_abs = 1e-9) {

     mode <- match.arg(mode)

     ctl <- new.env(parent = emptyenv())
     ctl$mode      <- mode
     ctl$seed      <- seed
     ctl$tol_rel   <- tol_rel
     ctl$tol_abs   <- tol_abs
     ctl$cursor    <- 0L
     ctl$tick      <- NA_integer_
     ctl$phase     <- NA_character_
     # Per-site call counts, so a run can report which of the 22 draw sites it
     # actually exercised. A site with zero calls is untested code wearing a
     # passing test, which a short run hides.
     ctl$coverage  <- stats::setNames(integer(length(.LASER_DRAW_SITES)),
                                      .LASER_DRAW_SITES)

     if (mode == "replay") {
          if (is.null(record)) {
               stop("laser_draws(mode = \"replay\") requires a `record`.", call. = FALSE)
          }
          ctl$record <- record
          ctl$n_calls <- length(record$calls$tick)
     }

     ctl
}

#' Stamp the current tick and phase onto a draw controller
#'
#' Called by the engine loop before each phase so that a replay mismatch can
#' report where it happened. Cheap: two assignments per phase per tick.
#' @keywords internal
.laser_at <- function(ctl, tick, phase) {
     ctl$tick  <- tick
     ctl$phase <- phase
     invisible(NULL)
}

#' Draw binomial counts
#'
#' @param ctl Draw controller from \code{laser_draws()}.
#' @param site Draw-site label; must be one of \code{.LASER_DRAW_SITES}.
#' @param n Integer vector of trial counts, length \code{npatches}.
#' @param p Numeric vector (or scalar) of success probabilities.
#' @return Integer vector of length \code{npatches}.
#' @keywords internal
.laser_binom <- function(ctl, site, n, p) {
     npatch <- length(n)
     if (length(p) == 1L) p <- rep(p, npatch)
     .laser_consume(ctl, site, "binomial", n, p,
                    function() as.integer(stats::rbinom(npatch, n, p)))
}

#' Draw Poisson counts
#'
#' @param ctl Draw controller from \code{laser_draws()}.
#' @param site Draw-site label; must be one of \code{.LASER_DRAW_SITES}.
#' @param lambda Numeric vector (or scalar) of rates.
#' @param npatches Patch count, needed when \code{lambda} is scalar.
#' @return Integer vector of length \code{npatches}.
#' @keywords internal
.laser_pois <- function(ctl, site, lambda, npatches = length(lambda)) {
     if (length(lambda) == 1L) lambda <- rep(lambda, npatches)
     .laser_consume(ctl, site, "poisson", NULL, lambda,
                    function() as.integer(stats::rpois(npatches, lambda)))
}

# Shared body of the two draw sites: count coverage, then either draw or
# replay. `draw_fn` is only evaluated in "rng" mode, so replay costs no
# randomness at all and cannot advance the R generator.
.laser_consume <- function(ctl, site, kind, n, param, draw_fn) {

     if (is.na(ctl$coverage[site])) {
          stop(sprintf("Unknown draw site '%s'. Add it to .LASER_DRAW_SITES.", site),
               call. = FALSE)
     }
     ctl$coverage[site] <- ctl$coverage[site] + 1L

     if (ctl$mode == "rng") return(draw_fn())

     ctl$cursor <- ctl$cursor + 1L
     i <- ctl$cursor

     if (i > ctl$n_calls) {
          stop(sprintf(
               paste0("Replay record exhausted: R asked for draw %d (tick %s, ",
                      "phase %s, site %s) but the record holds only %d calls. ",
                      "The R engine is drawing more than the oracle did."),
               i, ctl$tick, ctl$phase, site, ctl$n_calls), call. = FALSE)
     }

     rec <- .laser_record_at(ctl$record, i)
     .laser_assert_match(ctl, i, site, kind, n, param, rec)
     rec$result
}

# Pull call `i` (1-based) out of the record's two-level layout.
.laser_record_at <- function(record, i) {
     off <- record$calls$offset[i]
     len <- record$calls$length[i]
     idx <- seq.int(off + 1L, off + len)
     list(
          tick   = record$calls$tick[i],
          phase  = record$calls$phase[i],
          site   = record$calls$site[i],
          kind   = record$calls$kind[i],
          n      = record$values$n[idx],
          param  = record$values$param[idx],
          result = as.integer(record$values$result[idx])
     )
}

# Assert the R engine asked for the draw the oracle made. Checked in order of
# how diagnostic the failure is: position first (a mis-ordered phase is the
# likeliest and hardest-to-see error), then the arguments.
.laser_assert_match <- function(ctl, i, site, kind, n, param, rec) {

     where <- sprintf("replay call %d (R: tick %s / %s / %s)",
                      i, ctl$tick, ctl$phase, site)

     if (!identical(kind, rec$kind)) {
          stop(sprintf("%s: kind mismatch -- R drew '%s', oracle drew '%s'.",
                       where, kind, rec$kind), call. = FALSE)
     }
     if (!is.na(rec$tick) && !identical(as.integer(ctl$tick), as.integer(rec$tick))) {
          stop(sprintf("%s: tick mismatch -- oracle was at tick %s. Phase order or loop indexing differs.",
                       where, rec$tick), call. = FALSE)
     }
     if (!is.na(rec$site) && !identical(site, rec$site)) {
          stop(sprintf("%s: site mismatch -- oracle was at '%s'. The pipeline is out of order.",
                       where, rec$site), call. = FALSE)
     }
     if (length(param) != length(rec$param)) {
          stop(sprintf("%s: length mismatch -- R asked for %d values, oracle recorded %d.",
                       where, length(param), length(rec$param)), call. = FALSE)
     }

     # `n` is an integer trial count: any difference is a real bug, not float
     # noise, so it is compared exactly.
     if (!is.null(n)) {
          bad <- which(as.numeric(n) != rec$n)
          if (length(bad)) {
               stop(sprintf("%s: binomial n differs at patch(es) %s -- R %s vs oracle %s.",
                            where, .laser_fmt(bad), .laser_fmt(n[bad]),
                            .laser_fmt(rec$n[bad])), call. = FALSE)
          }
     }

     tol <- ctl$tol_abs + ctl$tol_rel * abs(rec$param)
     bad <- which(!(abs(param - rec$param) <= tol))
     if (length(bad)) {
          stop(sprintf("%s: %s differs at patch(es) %s -- R %s vs oracle %s (tol %s).",
                       where, if (kind == "binomial") "probability" else "lambda",
                       .laser_fmt(bad), .laser_fmt(param[bad]),
                       .laser_fmt(rec$param[bad]), .laser_fmt(tol[bad])),
               call. = FALSE)
     }

     invisible(TRUE)
}

.laser_fmt <- function(x, max_n = 6L) {
     shown <- utils::head(x, max_n)
     out <- paste(format(shown, digits = 8), collapse = ", ")
     if (length(x) > max_n) out <- paste0(out, ", ... (", length(x), " total)")
     paste0("[", out, "]")
}

#' Assert a replay run consumed the record exactly
#'
#' Exhaustion must be checked in both directions. A port that skips a draw
#' site passes a naive replay test right up to the point where the offsets
#' happen to realign, so "R never ran past the end" is not enough on its own.
#'
#' @param ctl Draw controller used for the run.
#' @return Invisibly \code{TRUE}; errors on any shortfall.
#' @keywords internal
laser_assert_replay_complete <- function(ctl) {
     if (ctl$mode != "replay") {
          stop("laser_assert_replay_complete() applies to replay runs only.", call. = FALSE)
     }
     if (ctl$cursor != ctl$n_calls) {
          stop(sprintf(
               paste0("Replay record not fully consumed: R made %d draws, the ",
                      "oracle made %d. The R engine is missing %d draw(s)."),
               ctl$cursor, ctl$n_calls, ctl$n_calls - ctl$cursor), call. = FALSE)
     }
     invisible(TRUE)
}

#' Report which draw sites a run exercised
#'
#' @param ctl Draw controller used for the run.
#' @return Data frame of \code{site} and \code{n_calls}, all 22 sites present.
#' @keywords internal
laser_draw_coverage <- function(ctl) {
     data.frame(site = names(ctl$coverage),
                n_calls = as.integer(ctl$coverage),
                row.names = NULL, stringsAsFactors = FALSE)
}

#' Establish an isolated RNG stream and describe how to restore the caller's
#'
#' @param seed Integer seed.
#' @return A list with the caller's prior \code{.Random.seed} (or \code{NULL}
#'   if the caller had none) and their \code{RNGkind()}.
#' @keywords internal
.laser_rng_begin <- function(seed) {
     had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
     prior <- if (had) get(".Random.seed", envir = globalenv()) else NULL
     prior_kind <- RNGkind()

     # Set the generator explicitly rather than inheriting it, so a caller who
     # has switched to a non-default RNG gets the same answer as one who has not.
     suppressWarnings(RNGkind(kind = "Mersenne-Twister",
                              normal.kind = "Inversion",
                              sample.kind = "Rejection"))
     set.seed(as.integer(seed))

     list(prior = prior, prior_kind = prior_kind)
}

#' Restore the caller's RNG stream
#' @param state Value returned by \code{.laser_rng_begin()}.
#' @keywords internal
.laser_rng_end <- function(state) {
     suppressWarnings(do.call(RNGkind, as.list(state$prior_kind)))
     if (is.null(state$prior)) {
          if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
               rm(".Random.seed", envir = globalenv())
          }
     } else {
          assign(".Random.seed", state$prior, envir = globalenv())
     }
     invisible(NULL)
}
