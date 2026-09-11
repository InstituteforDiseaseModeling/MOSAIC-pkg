#' Per-tick phase functions for the R transmission engine
#'
#' One function per component of the Python pipeline, in the canonical order
#' fixed by \code{model.py:566-579}:
#'
#' \preformatted{
#' Susceptible -> Exposed -> Recovered -> Infectious -> Vaccinated -> Census
#'   -> HumanToHuman -> EnvToHuman -> Environmental -> DerivedValues
#' }
#'
#' The order is semantically load-bearing and is reproduced rather than
#' rationalised. \code{Infectious} reads \code{E[tick + 1]} *after*
#' \code{Exposed} has written it; \code{HumanToHuman} reads \code{N[tick]} but
#' writes into \code{S[tick + 1]}, which \code{Census} has already summed. That
#' asymmetry is real. **Port the behaviour, not the intent** -- anything that
#' looks wrong gets an issue, not a local fix.
#'
#' Each function takes \code{(state, par, ctl, tick)} where \code{tick} is
#' 0-based as in Python, and writes into row \code{tick + 2} of the state
#' matrices (R's 1-based row for Python's \code{tick + 1}).
#'
#' @name laser_components
#' @keywords internal
NULL

# Row helpers. Python's `x[tick]` is R's `x[tick + 1L, ]`; Python's
# `x[tick + 1]` is R's `x[tick + 2L, ]`. Naming them keeps the +1/+2 out of
# the dynamics, where it is the single easiest thing to get wrong.
.row_at   <- function(tick) tick + 1L
.row_next <- function(tick) tick + 2L

#' @rdname laser_components
#' @keywords internal
laser_phase_susceptible <- function(state, par, ctl, tick) {

     .laser_at(ctl, tick, "Susceptible")
     here <- .row_at(tick)
     nxt  <- .row_next(tick)

     # Carry forward, then kill, then birth (susceptible.py:128-145).
     s_next <- state$S[here, ]

     deaths <- .laser_binom(ctl, "susceptible/non_disease_deaths",
                            s_next, par$non_disease_death_prob_jt[here, ])
     s_next <- s_next - deaths
     state$non_disease_deaths[here, ] <- state$non_disease_deaths[here, ] + deaths

     # Births are Poisson(N[tick] * b_jt[tick]) -- N at `tick`, not `tick + 1`.
     births <- .laser_pois(ctl, "susceptible/births",
                           state$N[here, ] * par$b_jt[here, ], state$.npatches)
     s_next <- s_next + births
     state$births[here, ] <- births

     state$S[nxt, ] <- s_next
     state
}

#' @rdname laser_components
#' @keywords internal
laser_phase_census <- function(state, par, ctl, tick) {

     .laser_at(ctl, tick, "Census")
     nxt <- .row_next(tick)

     # `census.py:76-78` accumulates with `+=` into a zero-initialised slot,
     # which is assignment in practice; kept as accumulation so a future
     # component that pre-seeds N behaves the same way.
     total <- state$N[nxt, ]
     for (nm in par$compartments) {
          total <- total + state[[nm]][nxt, ]
     }
     state$N[nxt, ] <- total
     state
}

# Seed N[0] the way `Census.check()` does: by calling the phase with tick = -1,
# so it sums the compartments that each component's constructor has already
# seeded. Slightly odd in Python and odd here too, but reproducing it avoids
# duplicating the sum logic and keeps N[0] defined in exactly one place.
.laser_seed_census <- function(state, par, ctl) {
     laser_phase_census(state, par, ctl, tick = -1L)
}
