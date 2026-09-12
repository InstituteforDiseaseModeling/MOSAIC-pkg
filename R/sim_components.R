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
#' @name sim_components
#' @keywords internal
NULL

# Row helpers. Python's `x[tick]` is R's `x[tick + 1L, ]`; Python's
# `x[tick + 1]` is R's `x[tick + 2L, ]`. Naming them keeps the +1/+2 out of
# the dynamics, where it is the single easiest thing to get wrong.
.row_at   <- function(tick) tick + 1L
.row_next <- function(tick) tick + 2L

#' @rdname sim_components
#' @keywords internal
sim_phase_susceptible <- function(state, par, ctl, tick) {

     .sim_at(ctl, tick, "Susceptible")
     here <- .row_at(tick)
     nxt  <- .row_next(tick)

     # Carry forward, then kill, then birth (susceptible.py:128-145).
     s_next <- state$S[[here]]

     deaths <- .sim_binom(ctl, "susceptible/non_disease_deaths",
                          s_next, par$non_disease_death_prob_jt[here, ])
     s_next <- s_next - deaths
     state$non_disease_deaths[[here]] <- state$non_disease_deaths[[here]] + deaths

     # Births are Poisson(N[tick] * b_jt[tick]) -- N at `tick`, not `tick + 1`.
     # Births feed an int32 compartment in the engine
     # (`.astype(S_next.dtype)`), so coerce here -- .sim_pois returns a double
     # because the environmental sites overflow int32.
     births <- as.integer(.sim_pois(ctl, "susceptible/births",
                                      state$N[[here]] * par$b_jt[here, ],
                                      state$.npatches))
     s_next <- s_next + births
     state$births[[here]] <- births

     state$S[[nxt]] <- s_next
     state
}

#' @rdname sim_components
#' @keywords internal
sim_phase_census <- function(state, par, ctl, tick) {

     .sim_at(ctl, tick, "Census")
     nxt <- .row_next(tick)

     # `census.py:76-78` accumulates with `+=` into a zero-initialised slot,
     # which is assignment in practice; kept as accumulation so a future
     # component that pre-seeds N behaves the same way.
     total <- state$N[[nxt]]
     for (nm in par$compartments) {
          total <- total + state[[nm]][[nxt]]
     }
     state$N[[nxt]] <- total
     state
}

# Seed N[0] the way `Census.check()` does: by calling the phase with tick = -1,
# so it sums the compartments that each component's constructor has already
# seeded. Slightly odd in Python and odd here too, but reproducing it avoids
# duplicating the sum logic and keeps N[0] defined in exactly one place.
.sim_seed_census <- function(state, par, ctl) {
     sim_phase_census(state, par, ctl, tick = -1L)
}

#' @rdname sim_components
#' @keywords internal
sim_phase_exposed <- function(state, par, ctl, tick) {

     .sim_at(ctl, tick, "Exposed")
     here <- .row_at(tick)
     nxt  <- .row_next(tick)

     # E -> Isym/Iasym progression lives in `Infectious`, not here; this phase
     # is demographic decay only (exposed.py:85-97).
     e <- state$E[[here]]

     deaths <- .sim_binom(ctl, "exposed/non_disease_deaths",
                          e, par$non_disease_death_prob_jt[here, ])

     state$E[[nxt]] <- e - deaths
     state$non_disease_deaths[[here]] <- state$non_disease_deaths[[here]] + deaths
     state
}

#' @rdname sim_components
#' @keywords internal
sim_phase_recovered <- function(state, par, ctl, tick) {

     .sim_at(ctl, tick, "Recovered")
     here <- .row_at(tick)
     nxt  <- .row_next(tick)

     r <- state$R[[here]]

     # `R_next += R` (recovered.py:96). Nothing writes R[tick+1] before this
     # phase, so it is assignment in practice -- but `Infectious` adds
     # recoveries into the same row later in the tick, so accumulating here
     # keeps the two consistent if the pipeline order ever changes.
     r_next <- state$R[[nxt]] + r

     deaths <- .sim_binom(ctl, "recovered/non_disease_deaths",
                          r, par$non_disease_death_prob_jt[here, ])
     r_next <- r_next - deaths
     state$non_disease_deaths[[here]] <- state$non_disease_deaths[[here]] + deaths

     # Waning is drawn on the POST-deaths cohort `R - deaths`, so an individual
     # cannot both die and wane in the same tick (recovered.py:108-110).
     waned <- .sim_binom(ctl, "recovered/waning", r - deaths, par$waning_prob)
     r_next <- r_next - waned

     state$R[[nxt]] <- r_next
     # Waned immunity returns to S, which `Susceptible` has already written.
     state$S[[nxt]] <- state$S[[nxt]] + waned
     state
}

#' @rdname sim_components
#' @keywords internal
sim_phase_infectious <- function(state, par, ctl, tick) {

     .sim_at(ctl, tick, "Infectious")
     here <- .row_at(tick)
     nxt  <- .row_next(tick)
     ndd_prob <- par$non_disease_death_prob_jt[here, ]

     # -- symptomatic: deaths ---------------------------------------------------
     is_next <- state$Isym[[here]]
     ndd <- .sim_binom(ctl, "infectious/sym_non_disease_deaths", is_next, ndd_prob)
     is_next <- is_next - ndd
     ndd_total <- state$non_disease_deaths[[here]] + ndd

     # -- symptomatic: disease deaths ------------------------------------------
     # This N is summed from the compartments at `tick`, NOT read from the
     # Census output `state$N` (infectious.py:191-196). The two differ, and the
     # reported-cases block further down deliberately uses the OTHER one. Both
     # are reproduced as written.
     n_manual <- state$S[[here]] + state$E[[here]] +
                 state$Isym[[here]] + state$Iasym[[here]] + state$R[[here]]
     if ("V1" %in% par$compartments) n_manual <- n_manual + state$V1[[here]]
     if ("V2" %in% par$compartments) n_manual <- n_manual + state$V2[[here]]

     treport <- tick - par$delta_reporting_cases
     epidemic_flag <- if (treport >= 0L) {
          as.integer(state$Isym[[.row_at(treport)]] >
                          par$epidemic_threshold * n_manual)
     } else {
          rep(0L, state$.npatches)
     }

     t_factor <- tick / par$nticks        # 0 <= t_factor <= 1
     mu_jt <- par$mu_j_baseline *
              (1 + par$mu_j_slope * t_factor) *
              (1 + par$mu_j_epidemic_factor * epidemic_flag)

     dd <- .sim_binom(ctl, "infectious/disease_deaths", is_next, -expm1(-mu_jt))
     state$disease_deaths[[here]] <- dd     # assignment, not accumulation
     is_next <- is_next - dd

     # Reported deaths lag the disease deaths they describe, so this draw reads
     # `disease_deaths` at an EARLIER row and fires only once the lag is served.
     idx_death_report <- tick - par$delta_reporting_deaths
     if (idx_death_report >= 0L) {
          rep_d <- .sim_binom(ctl, "infectious/reported_deaths",
                              state$disease_deaths[[.row_at(idx_death_report)]],
                              par$rho_deaths)
          state$reported_deaths[[here]] <- state$reported_deaths[[here]] + rep_d
     }

     # -- symptomatic: recovery -------------------------------------------------
     rec_sym <- .sim_binom(ctl, "infectious/sym_recovery", is_next, par$gamma_1_prob)
     is_next <- is_next - rec_sym
     r_next <- state$R[[nxt]] + rec_sym

     # -- asymptomatic ----------------------------------------------------------
     ia_next <- state$Iasym[[here]]
     ndd_a <- .sim_binom(ctl, "infectious/asym_non_disease_deaths", ia_next, ndd_prob)
     ia_next <- ia_next - ndd_a
     ndd_total <- ndd_total + ndd_a       # same row as the symptomatic deaths

     rec_asym <- .sim_binom(ctl, "infectious/asym_recovery", ia_next, par$gamma_2_prob)
     ia_next <- ia_next - rec_asym
     r_next <- r_next + rec_asym

     state$non_disease_deaths[[here]] <- ndd_total
     state$R[[nxt]] <- r_next

     # -- progression E -> I ----------------------------------------------------
     # Drawn on E[tick + 1], i.e. AFTER `Exposed` removed this tick's deaths --
     # "can't progress deceased individuals" (infectious.py:245-249).
     e_next <- state$E[[nxt]]
     progressing <- .sim_binom(ctl, "infectious/progression", e_next, par$iota_prob)
     state$E[[nxt]] <- e_next - progressing

     # The sigma split is deterministic rounding, NOT a draw. `np.round` and R's
     # `round()` are both round-half-to-even, so they agree; `as.integer()`
     # alone would truncate and bias the split downward.
     new_sym  <- as.integer(round(par$sigma * progressing))
     new_asym <- progressing - new_sym
     is_next <- is_next + new_sym
     ia_next <- ia_next + new_asym

     # Written at tick + 1, not tick -- and read back at a lagged row below.
     state$new_symptomatic[[nxt]] <- new_sym

     state$Isym[[nxt]]  <- is_next
     state$Iasym[[nxt]] <- ia_next

     # -- reported cases --------------------------------------------------------
     idx_probe <- tick - par$delta_reporting_cases
     if (idx_probe >= 0L) {
          probe <- .row_at(idx_probe)
          # Note: the Census `N`, unlike the manual sum used for the epidemic
          # flag above.
          infected_fraction <- state$Isym[[probe]] / state$N[[probe]]
          chi_eff <- ifelse(infected_fraction < par$epidemic_threshold,
                            par$chi_endemic, par$chi_epidemic)
          drawn <- .sim_binom(ctl, "infectious/reported_cases",
                              state$new_symptomatic[[probe]], par$rho)
          state$reported_cases[[here]] <- state$reported_cases[[here]] +
               as.integer(round(drawn / chi_eff))
     }

     state
}

#' @rdname sim_components
#' @keywords internal
sim_phase_vaccinated <- function(state, par, ctl, tick) {

     .sim_at(ctl, tick, "Vaccinated")
     here <- .row_at(tick)
     nxt  <- .row_next(tick)
     ndd_prob <- par$non_disease_death_prob_jt[here, ]

     v1_next <- state$V1[[here]]
     v2_next <- state$V2[[here]]

     # -- natural mortality, both doses ----------------------------------------
     ndd1 <- .sim_binom(ctl, "vaccinated/v1_non_disease_deaths", v1_next, ndd_prob)
     v1_next <- v1_next - ndd1
     ndd_total <- state$non_disease_deaths[[here]] + ndd1

     ndd2 <- .sim_binom(ctl, "vaccinated/v2_non_disease_deaths", v2_next, ndd_prob)
     v2_next <- v2_next - ndd2
     ndd_total <- ndd_total + ndd2
     state$non_disease_deaths[[here]] <- ndd_total

     # -- waning vaccine-derived immunity --------------------------------------
     waned1 <- .sim_binom(ctl, "vaccinated/v1_waning", v1_next, par$omega_1_prob)
     v1_next <- v1_next - waned1
     s_next <- state$S[[nxt]] + waned1

     waned2 <- .sim_binom(ctl, "vaccinated/v2_waning", v2_next, par$omega_2_prob)
     v2_next <- v2_next - waned2
     s_next <- s_next + waned2
     state$S[[nxt]] <- s_next

     # -- second doses, BEFORE first doses -------------------------------------
     # Deliberate: doing first doses first would let an individual move
     # S -> V1 -> V2 within a single tick (vaccinated.py:178-180).
     nu2 <- par$nu_2_jt[here, ]
     if (any(nu2 != 0)) {
          doses2 <- as.integer(round(nu2))
          # The schedule is clamped to the donor pool. The engine logs this at
          # DEBUG and carries on; the clamp itself is the observable behaviour.
          doses2 <- pmin(doses2, v1_next)
          state$dose_two_doses[[here]] <- doses2

          # Only the phi_2-effective fraction transits; ineffective doses leave
          # the recipient in V1.
          immunized2 <- as.integer(round(par$phi_2 * doses2))
          v1_next <- v1_next - immunized2
          v2_next <- v2_next + immunized2
     }

     # -- first doses ----------------------------------------------------------
     nu1 <- par$nu_1_jt[here, ]
     if (any(nu1 != 0)) {
          doses1 <- as.integer(round(nu1))

          # Donor populations are read at tick + 1, as a snapshot: the pro-rata
          # fractions below all divide the SAME snapshot, so decrementing one
          # donor does not change another's share.
          sources <- par$nu_jt_sources
          pop <- vapply(sources, function(nm) state[[nm]][[nxt]],
                        numeric(state$.npatches))
          if (state$.npatches == 1L) pop <- matrix(pop, nrow = 1L)
          available <- rowSums(pop)

          doses1 <- pmin(doses1, as.integer(available))
          state$dose_one_doses[[here]] <- doses1

          # Floor the denominator at 1 to avoid 0/0 where a patch has no donors
          # left; the numerator is 0 there anyway, so the share is 0.
          denom <- pmax(available, 1)

          total_immunized <- rep(0L, state$.npatches)
          for (k in seq_along(sources)) {
               nm <- sources[[k]]
               fraction <- pop[, k] / denom
               # Two independent roundings, in this order, per compartment --
               # the split is not renormalised afterwards, so the per-source
               # doses need not sum to `doses1`. Reproduced, not corrected.
               comp_doses <- as.integer(round(doses1 * fraction))
               effective  <- as.integer(round(par$phi_1 * comp_doses))
               state[[nm]][[nxt]] <- state[[nm]][[nxt]] - effective
               total_immunized <- total_immunized + effective
          }
          v1_next <- v1_next + total_immunized
     }

     state$V1[[nxt]] <- v1_next
     state$V2[[nxt]] <- v2_next
     state
}

#' @rdname sim_components
#' @keywords internal
sim_phase_human_to_human <- function(state, par, ctl, tick) {

     .sim_at(ctl, tick, "HumanToHuman")
     here <- .row_at(tick)
     nxt  <- .row_next(tick)

     total_i <- state$Isym[[here]] + state$Iasym[[here]]
     local_frac <- par$local_frac      # float32-precision `1 - tau_i`

     local_i <- local_frac * total_i

     # Python: `((tau_i * total_i) * pi_ij.T).T.sum(axis=0)`, which is
     # `sum_i v[i] * pi_ij[i, j]` for destination j. `pi_ij` is indexed
     # [source, destination] and its diagonal is zero, so the i != j exclusion
     # is already baked into the matrix.
     v <- par$tau_i * total_i
     immigrating_i <- colSums(v * par$pi_ij)

     effective_i <- local_i + immigrating_i
     numerator   <- par$beta_jt_human[here, ] * effective_i^par$alpha_1
     denominator <- state$N[[here]]^par$alpha_2
     rate <- numerator / denominator

     # The engine clamps a negative rate and logs at DEBUG rather than failing
     # (humantohuman.py:159-161); a negative rate would make `-expm1(-rate)`
     # exceed 1 and the binomial reject it.
     rate <- pmax(rate, 0)
     state$Lambda[[nxt]] <- rate

     # Drawn on S[tick + 1], after mortality and the waning inflows.
     s_next <- state$S[[nxt]]
     local <- as.integer(round(local_frac * s_next))
     new_infections <- .sim_binom(ctl, "humantohuman/infection",
                                  local, -expm1(-rate))

     state$S[[nxt]] <- s_next - new_infections
     state$E[[nxt]] <- state$E[[nxt]] + new_infections
     state$incidence_human[[nxt]] <- state$incidence_human[[nxt]] + new_infections
     state$incidence[[nxt]]       <- state$incidence[[nxt]] + new_infections
     state
}

#' @rdname sim_components
#' @keywords internal
sim_phase_env_to_human <- function(state, par, ctl, tick) {

     .sim_at(ctl, tick, "EnvToHuman")
     here <- .row_at(tick)
     nxt  <- .row_next(tick)

     w <- state$W[[here]]

     # Psi = beta_jt_env * (1 - theta_j) * W / (kappa + W)
     psi <- par$beta_jt_env[here, ] * ((1 - par$theta_j) * w) / (par$kappa + w)
     state$Psi[[nxt]] <- psi

     local_frac <- par$local_frac      # float32-precision `1 - tau_i`
     s_next <- state$S[[nxt]]
     local_s <- as.integer(round(local_frac * s_next))
     new_infections <- .sim_binom(ctl, "envtohuman/infection",
                                  local_s, -expm1(-psi))

     state$S[[nxt]] <- s_next - new_infections
     state$E[[nxt]] <- state$E[[nxt]] + new_infections
     state$incidence_env[[nxt]] <- state$incidence_env[[nxt]] + new_infections
     state$incidence[[nxt]]     <- state$incidence[[nxt]] + new_infections
     state
}

#' @rdname sim_components
#' @keywords internal
sim_phase_environmental <- function(state, par, ctl, tick) {

     .sim_at(ctl, tick, "Environmental")
     here <- .row_at(tick)
     nxt  <- .row_next(tick)

     w <- state$W[[here]]
     w_next <- w

     # Decay is Poisson-drawn and THEN clamped to W, so the reservoir cannot go
     # negative on an extreme draw. The clamp is applied to the draw, not to the
     # rate, so the recorded draw can legitimately exceed W.
     decay <- pmin(.sim_pois(ctl, "environmental/decay",
                               par$delta_jt[here, ] * w, state$.npatches), w)
     w_next <- w_next - decay

     # Shedding enters attenuated by WASH coverage. `W` is float32 in the engine
     # and double here, so the `(1 - theta_j) *` product keeps a fractional part
     # in both -- this is not an integer reservoir.
     shed_sym <- .sim_pois(ctl, "environmental/shedding_sym",
                           par$zeta_1 * state$Isym[[here]], state$.npatches)
     w_next <- w_next + (1 - par$theta_j) * shed_sym

     shed_asym <- .sim_pois(ctl, "environmental/shedding_asym",
                            par$zeta_2 * state$Iasym[[here]], state$.npatches)
     w_next <- w_next + (1 - par$theta_j) * shed_asym

     state$W[[nxt]] <- w_next
     state
}
