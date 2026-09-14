#' Stochastic draw sites for the R transmission engine
#'
#' The engine reaches randomness only through \code{.sim_binom()} and
#' \code{.sim_pois()}. Routing every draw through two functions is what makes
#' the Tier B replay strategy possible: in \code{"replay"} mode they return
#' recorded results from the Python oracle instead of drawing, while asserting
#' that the R engine asked for exactly the draw the oracle made, at the same
#' tick, in the same phase, at the same site.
#'
#' @section RNG contract:
#' A simulation's output is determined solely by its \code{seed} and
#' \code{config} -- never by worker identity, batch position, or how much
#' randomness was consumed earlier in the session. \code{.sim_rng_state()}
#' establishes an isolated stream and returns everything needed to restore the
#' caller's \code{.Random.seed} afterwards, so calling the engine never
#' perturbs the caller's stream.
#'
#' @name sim_rng
#' @keywords internal
NULL

# The draw-site registry. Every call site in the engine passes a `site` label
# from this vector, so a replay mismatch names the site rather than a call
# index. Labels mirror the Python source locations they were ported from.
.SIM_DRAW_SITES <- c(
     "susceptible/non_disease_deaths",     # susceptible.py:135
     "susceptible/births",                 # susceptible.py:143
     "exposed/non_disease_deaths",         # exposed.py:94
     "recovered/non_disease_deaths",       # recovered.py:103
     "recovered/waning",                   # recovered.py:110
     "infectious/sym_non_disease_deaths",  # infectious.py:181
     "infectious/disease_deaths",          # infectious.py:203
     "infectious/reported_deaths",         # infectious.py:210  (conditional)
     "infectious/sym_recovery",            # infectious.py:217
     "infectious/asym_non_disease_deaths", # infectious.py:231
     "infectious/asym_recovery",           # infectious.py:239
     "infectious/progression",             # infectious.py:249
     "infectious/reported_cases",          # infectious.py:267  (conditional)
     "vaccinated/v1_non_disease_deaths",   # vaccinated.py:157
     "vaccinated/v2_non_disease_deaths",   # vaccinated.py:163
     "vaccinated/v1_waning",               # vaccinated.py:170
     "vaccinated/v2_waning",               # vaccinated.py:175
     "humantohuman/infection",             # humantohuman.py:165
     "envtohuman/infection",               # envtohuman.py:133
     "environmental/decay",                # environmental.py:135
     "environmental/shedding_sym",         # environmental.py:139
     "environmental/shedding_asym"         # environmental.py:143
)

#' Create a draw controller for one simulation
#'
#' @param mode Either \code{"rng"} (draw from R's generator) or \code{"replay"}
#'   (pop recorded draws from \code{record} and assert they match).
#' @param seed Integer seed. Used in \code{"rng"} mode.
#' @param record Replay record from \code{sim_read_fixture()}. Required when
#'   \code{mode = "replay"}.
#' @param tol_rel,tol_abs Combined tolerance for comparing draw parameters
#'   against the record: \code{abs(a - b) <= tol_abs + tol_rel * abs(b)}. A
#'   purely relative tolerance is wrong here because rates legitimately reach
#'   exactly zero in low-transmission patches.
#' @return A draw controller (an environment).
#' @keywords internal
sim_draws <- function(mode = c("rng", "replay"),
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
     #
     # A hashed environment, not a named integer vector. `coverage[site] <- n`
     # on a named vector copies the whole 22-element vector AND its name
     # attribute on every one of ~30,750 draws per run; that allocation churn
     # measured 12% of engine runtime on its own. An environment binding is a
     # hashed store with no copy. `sim_draw_coverage()` materialises the
     # data frame once, at the end of the run.
     ctl$coverage  <- new.env(hash = TRUE, parent = emptyenv())
     for (.s in .SIM_DRAW_SITES) assign(.s, 0L, envir = ctl$coverage)
     # Branch on mode ONCE here rather than ~30,750 times per run. In `"rng"`
     # mode -- every production run -- the replay machinery in
     # `.sim_consume()` is dead weight: the draw wrapper was measured at
     # 3.2 microseconds of overhead per call against a 2.2 microsecond
     # `rbinom()`, i.e. 20% of engine runtime, most of it the per-call closure
     # allocation and the extra frame rather than the bookkeeping itself.
     ctl$fast      <- identical(mode, "rng")

     if (mode == "replay") {
          if (is.null(record)) {
               stop("sim_draws(mode = \"replay\") requires a `record`.", call. = FALSE)
          }
          ctl$record <- record
          ctl$n_calls <- length(record$calls$tick)
     }

     ctl
}

#' Stamp the current tick and phase onto a draw controller
#'
#' Called by the engine loop before each phase so that a replay mismatch can
#' report where it happened. A no-op in \code{"rng"} mode -- see the body.
#' @keywords internal
.sim_at <- function(ctl, tick, phase) {
     # Only replay reads these: they exist so a draw mismatch can name the tick
     # and phase it happened in. Two environment writes x 10 phases x 1,398
     # ticks is 8% of engine runtime, paid in production to populate fields
     # nothing in production reads. Skipped on the fast path, which leaves
     # `ctl$tick`/`ctl$phase` at NA in `"rng"` mode -- do not instrument
     # against them without forcing `mode = "replay"`.
     if (ctl$fast) return(invisible(NULL))
     ctl$tick  <- tick
     ctl$phase <- phase
     invisible(NULL)
}

#' Draw binomial counts
#'
#' @param ctl Draw controller from \code{sim_draws()}.
#' @param site Draw-site label; must be one of \code{.SIM_DRAW_SITES}.
#' @param n Integer vector of trial counts, length \code{npatches}.
#' @param p Numeric vector (or scalar) of success probabilities.
#' @return Integer vector of length \code{npatches}.
#' @keywords internal
.sim_binom <- function(ctl, site, n, p) {
     if (ctl$fast) {
          # Production path. Coverage is still counted -- it is part of the
          # engine's return contract and three tests read it off an ordinary
          # run -- but everything else the replay path needs is skipped: no
          # closure allocation, no `rep()` of a scalar `p` that `rbinom()`
          # recycles for free, no `stats::` namespace resolution per call, and
          # no second frame. Parity is exact: recycling happens inside the
          # sampler, so a scalar `p` yields the same variates in the same
          # order as a materialised length-npatch `p`.
          cnt <- ctl$coverage[[site]]
          if (is.null(cnt)) .sim_unknown_site(site)
          ctl$coverage[[site]] <- cnt + 1L
          return(as.integer(rbinom(length(n), n, p)))
     }
     npatch <- length(n)
     if (length(p) == 1L) p <- rep(p, npatch)
     .sim_consume(ctl, site, "binomial", n, p,
                  function() as.integer(rbinom(npatch, n, p)))
}

#' Draw Poisson counts
#'
#' @param ctl Draw controller from \code{sim_draws()}.
#' @param site Draw-site label; must be one of \code{.SIM_DRAW_SITES}.
#' @param lambda Numeric vector (or scalar) of rates.
#' @param npatches Patch count, needed when \code{lambda} is scalar.
#' @return Integer vector of length \code{npatches}.
#' @keywords internal
# Returns a DOUBLE, not an integer. `rpois()` itself returns a double and the
# environmental shedding sites legitimately exceed int32 (lambda ~ 1e12), where
# `as.integer()` would give NA. Callers that feed an integer compartment coerce
# at the point of use; the value is exactly integral either way below 2^53.
.sim_pois <- function(ctl, site, lambda, npatches = length(lambda)) {
     if (ctl$fast) {
          # Note `npatches`, not `length(lambda)`: this function is called with
          # a scalar `lambda` plus an explicit `npatches`, and using the
          # argument's length would silently draw one variate instead of
          # npatches. `rpois()` recycles a scalar rate, so parity is exact.
          cnt <- ctl$coverage[[site]]
          if (is.null(cnt)) .sim_unknown_site(site)
          ctl$coverage[[site]] <- cnt + 1L
          return(rpois(npatches, lambda))
     }
     if (length(lambda) == 1L) lambda <- rep(lambda, npatches)
     .sim_consume(ctl, site, "poisson", NULL, lambda,
                  function() rpois(npatches, lambda))
}

.sim_unknown_site <- function(site) {
     stop(sprintf("Unknown draw site '%s'. Add it to .SIM_DRAW_SITES.", site),
          call. = FALSE)
}

# Shared body of the two draw sites: count coverage, then either draw or
# replay. `draw_fn` is only evaluated in "rng" mode, so replay costs no
# randomness at all and cannot advance the R generator.
.sim_consume <- function(ctl, site, kind, n, param, draw_fn) {

     cnt <- ctl$coverage[[site]]
     if (is.null(cnt)) .sim_unknown_site(site)
     ctl$coverage[[site]] <- cnt + 1L

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

     rec <- .sim_record_at(ctl$record, i)
     .sim_assert_match(ctl, i, site, kind, n, param, rec)
     rec$result
}

# Pull call `i` (1-based) out of the record's two-level layout.
.sim_record_at <- function(record, i) {
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
          # Kept as double, NOT coerced to integer. Poisson draw results are not
          # bounded by int32: the environmental shedding rate is
          # `zeta_1 * Isym` with `zeta_1` of order 1e8, so lambda reaches ~1e12
          # and `as.integer()` silently returns NA. The engine itself stores
          # those results as float32 (the dtype of `W`), never as an int.
          # Integer-valued sites stay exactly integral in a double up to 2^53.
          result = record$values$result[idx]
     )
}

# Assert the R engine asked for the draw the oracle made. Checked in order of
# how diagnostic the failure is: position first (a mis-ordered phase is the
# likeliest and hardest-to-see error), then the arguments.
# Per-site replay tolerance.
#
# One site needs its own, and the reason is worth stating because it is the
# largest float divergence in the whole port. The environmental reservoir `W` is
# float32 in the Python engine and double here, and unlike every other float32
# difference this one FEEDS BACK: the decay draw's rate is `delta_jt * W`, so
# W's representation error re-enters as a draw parameter and accumulates across
# ticks. Measured over the 1398-tick 40-patch default config
# (claude/oracle/a2_required_tolerance.R) the decay rate needs 4.1e-5 where
# every other site needs 3.6e-7 or less.
#
# This is a limit on what the oracle can certify, not a correctness bound. What
# it costs is one weakened cross-check; what is NOT weakened is the part that
# matters -- all 19 integer channels stay bit-identical over the full run, and
# `W` itself is held to a scale-aware 1e-5 in the channel comparison. Nothing
# integer reads W except through a replayed draw result, which is why the
# divergence cannot reach a compartment.
#
# The R engine keeps W in double deliberately: it is the more accurate of the
# two, exactly as with `pi_ij` (see sim_precompute.R). Emulating float32 here
# would buy an exact replay at the price of reproducing a defect.
.SIM_SITE_TOL <- c("environmental/decay" = 1e-3)

.sim_site_tol <- function(ctl, site) {
     # `[[` on a named vector errors for an absent name, so index with `[` and
     # test for NA -- the override table holds one entry and misses on 21 sites.
     t <- .SIM_SITE_TOL[site]
     if (is.na(t)) ctl$tol_rel else unname(t)
}

.sim_assert_match <- function(ctl, i, site, kind, n, param, rec) {

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
                            where, .sim_fmt(bad), .sim_fmt(n[bad]),
                            .sim_fmt(rec$n[bad])), call. = FALSE)
          }
     }

     # Scale-aware tolerance: `rtol * (|ref| + max|ref|)`.
     #
     # A pure relative tolerance is unusable here because several parameters pass
     # through zero -- the human-to-human rate and the environmental decay rate
     # both go to exactly 0 in low-season and in patches with an empty reservoir,
     # where a physically nil difference reads as a huge relative one. A pure
     # absolute tolerance is equally unusable because the parameters span
     # `Lambda` at ~1e-7 and the decay rate at ~1e9 in the same run. Scaling the
     # floor to the call's own peak handles both, and the remaining number means
     # "error as a fraction of this parameter's scale".
     scale <- max(abs(rec$param))
     tol <- .sim_site_tol(ctl, site) * (abs(rec$param) + scale)
     bad <- which(!(abs(param - rec$param) <= tol))
     if (length(bad)) {
          stop(sprintf("%s: %s differs at patch(es) %s -- R %s vs oracle %s (tol %s).",
                       where, if (kind == "binomial") "probability" else "lambda",
                       .sim_fmt(bad), .sim_fmt(param[bad]),
                       .sim_fmt(rec$param[bad]), .sim_fmt(tol[bad])),
               call. = FALSE)
     }

     invisible(TRUE)
}

.sim_fmt <- function(x, max_n = 6L) {
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
sim_assert_replay_complete <- function(ctl) {
     if (ctl$mode != "replay") {
          stop("sim_assert_replay_complete() applies to replay runs only.", call. = FALSE)
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
sim_draw_coverage <- function(ctl) {
     data.frame(site = .SIM_DRAW_SITES,
                n_calls = vapply(.SIM_DRAW_SITES,
                                 function(s) ctl$coverage[[s]], integer(1L),
                                 USE.NAMES = FALSE),
                row.names = NULL, stringsAsFactors = FALSE)
}

#' Establish an isolated RNG stream and describe how to restore the caller's
#'
#' @param seed Integer seed.
#' @return A list with the caller's prior \code{.Random.seed} (or \code{NULL}
#'   if the caller had none) and their \code{RNGkind()}.
#' @keywords internal
.sim_rng_begin <- function(seed) {
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
#' @param state Value returned by \code{.sim_rng_begin()}.
#' @keywords internal
.sim_rng_end <- function(state) {
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
