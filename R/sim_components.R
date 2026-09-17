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
#' series (R's 1-based row for Python's \code{tick + 1}).
#'
#' State rows are environments (see \code{sim_alloc_state()}), so each phase
#' binds the rows it needs once -- \code{rh} for \code{here}, \code{rn} for
#' \code{nxt} -- and then reads and writes channels by name: \code{rh$S},
#' \code{rn$E <- ...}. The two exceptions are the lagged reads, which address
#' an arbitrary historical row and so spell it out as
#' \code{state$rows[[probe]]$Isym}. Writing through \code{rh}/\code{rn}
#' mutates the state in place; there is no write-back step.
#'
#' @name sim_components
#' @keywords internal
NULL

# Row helpers. Python's `x[tick]` is R's row `tick + 1L`; Python's
# `x[tick + 1]` is R's row `tick + 2L`. Naming them keeps the +1/+2 out of
# the dynamics, where it is the single easiest thing to get wrong.
.row_at   <- function(tick) tick + 1L
.row_next <- function(tick) tick + 2L

#' @rdname sim_components
#' @keywords internal
sim_phase_susceptible <- function(state, par, ctl, tick) {

     .sim_at(ctl, tick, "Susceptible")
     here <- .row_at(tick)
     nxt  <- .row_next(tick)
     rh <- state$rows[[here]]
     rn <- state$rows[[nxt]]

     # Carry forward, then kill, then birth (susceptible.py:128-145).
     s_next <- rh$S

     deaths <- .sim_binom(ctl, "susceptible/non_disease_deaths",
                          s_next, par$non_disease_death_prob_jt[here, ])
     s_next <- s_next - deaths
     rh$non_disease_deaths <- rh$non_disease_deaths + deaths

     # Births are Poisson(N[tick] * b_jt[tick]) -- N at `tick`, not `tick + 1`.
     # Births feed an int32 compartment in the engine
     # (`.astype(S_next.dtype)`), so coerce here -- .sim_pois returns a double
     # because the environmental sites overflow int32.
     births <- as.integer(.sim_pois(ctl, "susceptible/births",
                                      rh$N * par$b_jt[here, ],
                                      state$.npatches))
     s_next <- s_next + births
     rh$births <- births

     rn$S <- s_next
     state
}

#' @rdname sim_components
#' @keywords internal
sim_phase_census <- function(state, par, ctl, tick) {

     .sim_at(ctl, tick, "Census")
     nxt <- .row_next(tick)
     rn <- state$rows[[nxt]]

     # `census.py:76-78` accumulates with `+=` into a zero-initialised slot,
     # which is assignment in practice; kept as accumulation so a future
     # component that pre-seeds N behaves the same way.
     total <- rn$N
     for (nm in par$compartments) {
          total <- total + rn[[nm]]
     }
     rn$N <- total
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
     rh <- state$rows[[here]]
     rn <- state$rows[[nxt]]

     # E -> Isym/Iasym progression lives in `Infectious`, not here; this phase
     # is demographic decay only (exposed.py:85-97).
     e <- rh$E

     deaths <- .sim_binom(ctl, "exposed/non_disease_deaths",
                          e, par$non_disease_death_prob_jt[here, ])

     rn$E <- e - deaths
     rh$non_disease_deaths <- rh$non_disease_deaths + deaths
     state
}

#' @rdname sim_components
#' @keywords internal
sim_phase_recovered <- function(state, par, ctl, tick) {

     .sim_at(ctl, tick, "Recovered")
     here <- .row_at(tick)
     nxt  <- .row_next(tick)
     rh <- state$rows[[here]]
     rn <- state$rows[[nxt]]

     r <- rh$R

     # `R_next += R` (recovered.py:96). Nothing writes R[tick+1] before this
     # phase, so it is assignment in practice -- but `Infectious` adds
     # recoveries into the same row later in the tick, so accumulating here
     # keeps the two consistent if the pipeline order ever changes.
     r_next <- rn$R + r

     deaths <- .sim_binom(ctl, "recovered/non_disease_deaths",
                          r, par$non_disease_death_prob_jt[here, ])
     r_next <- r_next - deaths
     rh$non_disease_deaths <- rh$non_disease_deaths + deaths

     # Waning is drawn on the POST-deaths cohort `R - deaths`, so an individual
     # cannot both die and wane in the same tick (recovered.py:108-110).
     waned <- .sim_binom(ctl, "recovered/waning", r - deaths, par$waning_prob)
     r_next <- r_next - waned

     rn$R <- r_next
     # Waned immunity returns to S, which `Susceptible` has already written.
     rn$S <- rn$S + waned
     state
}

#' @rdname sim_components
#' @keywords internal
sim_phase_infectious <- function(state, par, ctl, tick) {

     .sim_at(ctl, tick, "Infectious")
     here <- .row_at(tick)
     nxt  <- .row_next(tick)
     rh <- state$rows[[here]]
     rn <- state$rows[[nxt]]
     ndd_prob <- par$non_disease_death_prob_jt[here, ]

     # -- symptomatic: deaths ---------------------------------------------------
     is_next <- rh$Isym
     ndd <- .sim_binom(ctl, "infectious/sym_non_disease_deaths", is_next, ndd_prob)
     is_next <- is_next - ndd
     ndd_total <- rh$non_disease_deaths + ndd

     # -- symptomatic: disease deaths ------------------------------------------
     # This N is summed from the compartments at `tick`, NOT read from the
     # Census output `state$N` (infectious.py:191-196). The two differ, and the
     # reported-cases block further down deliberately uses the OTHER one. Both
     # are reproduced as written.
     n_manual <- rh$S + rh$E + rh$Isym + rh$Iasym + rh$R
     if ("V1" %in% par$compartments) n_manual <- n_manual + rh$V1
     if ("V2" %in% par$compartments) n_manual <- n_manual + rh$V2

     treport <- tick - par$delta_reporting_cases
     epidemic_flag <- if (treport >= 0L) {
          as.integer(state$rows[[.row_at(treport)]]$Isym >
                          par$epidemic_threshold * n_manual)
     } else {
          rep(0L, state$.npatches)
     }

     t_factor <- tick / par$nticks        # 0 <= t_factor <= 1
     mu_jt <- par$mu_j_baseline *
              (1 + par$mu_j_slope * t_factor) *
              (1 + par$mu_j_epidemic_factor * epidemic_flag)

     dd <- .sim_binom(ctl, "infectious/disease_deaths", is_next, -expm1(-mu_jt))
     rh$disease_deaths <- dd     # assignment, not accumulation
     is_next <- is_next - dd

     # Reported deaths lag the disease deaths they describe, so this draw reads
     # `disease_deaths` at an EARLIER row and fires only once the lag is served.
     idx_death_report <- tick - par$delta_reporting_deaths
     if (idx_death_report >= 0L) {
          rep_d <- .sim_binom(ctl, "infectious/reported_deaths",
                              state$rows[[.row_at(idx_death_report)]]$disease_deaths,
                              par$rho_deaths)
          rh$reported_deaths <- rh$reported_deaths + rep_d
     }

     # -- symptomatic: recovery -------------------------------------------------
     rec_sym <- .sim_binom(ctl, "infectious/sym_recovery", is_next, par$gamma_1_prob)
     is_next <- is_next - rec_sym
     r_next <- rn$R + rec_sym

     # -- asymptomatic ----------------------------------------------------------
     ia_next <- rh$Iasym
     ndd_a <- .sim_binom(ctl, "infectious/asym_non_disease_deaths", ia_next, ndd_prob)
     ia_next <- ia_next - ndd_a
     ndd_total <- ndd_total + ndd_a       # same row as the symptomatic deaths

     rec_asym <- .sim_binom(ctl, "infectious/asym_recovery", ia_next, par$gamma_2_prob)
     ia_next <- ia_next - rec_asym
     r_next <- r_next + rec_asym

     rh$non_disease_deaths <- ndd_total
     rn$R <- r_next

     # -- progression E -> I ----------------------------------------------------
     # Drawn on E[tick + 1], i.e. AFTER `Exposed` removed this tick's deaths --
     # "can't progress deceased individuals" (infectious.py:245-249).
     e_next <- rn$E
     progressing <- .sim_binom(ctl, "infectious/progression", e_next, par$iota_prob)
     rn$E <- e_next - progressing

     # The symptomatic split.
     #
     # THE SPEC (04-model-description.Rmd, "Table of stochastic transitions")
     # specifies a stochastic split: each progressing individual is
     # independently symptomatic with probability sigma. The ORACLE
     # (laser-cholera 0.16.1, infectious.py) instead does a deterministic
     # `np.round(sigma * progressing)`, and the R port reproduced that
     # faithfully -- so both engines diverged from the spec in the same way.
     #
     # The deterministic form is wrong in the MEAN, not just the variance,
     # because round() is not linear at small counts: round(sigma * n) = 0 for
     # every n <= 2 at sigma = 0.2. Measured on production configs, 15.4% of
     # patch-days with E >= 1 yield zero symptomatic, and GNB/COG/NAM lose the
     # symptomatic arm on 28-41% of days. At low incidence -- exactly where
     # outbreak onset is decided -- the split systematically suppresses the
     # symptomatic arm, and the symptomatic arm is what surveillance observes.
     #
     # MODE-DEPENDENT, DELIBERATELY. In "replay" mode the deterministic form is
     # retained, because replay exists to validate the port draw-for-draw
     # against the oracle and the oracle IS the deterministic form. Drawing here
     # would consume a variate Python never drew, desynchronising every
     # subsequent draw and destroying the parity harness for all 22 other sites.
     # Replay answers "did we port Python correctly?"; for that question,
     # reproducing Python is correct. Production ("rng") answers "does the model
     # implement the spec?", and there the binomial is correct.
     #
     # The cost of this split is that replay no longer covers the rng-mode form
     # -- CLAUDE.md lesson #18(v) exactly. test-sigma-split.R covers it
     # distributionally instead.
     if (isTRUE(ctl$mode == "replay")) {
          new_sym <- as.integer(round(par$sigma * progressing))
     } else {
          new_sym <- .sim_binom(ctl, "infectious/sigma_split", progressing, par$sigma)
     }
     new_asym <- progressing - new_sym
     is_next <- is_next + new_sym
     ia_next <- ia_next + new_asym

     # Written at tick + 1, not tick -- and read back at a lagged row below.
     rn$new_symptomatic <- new_sym

     rn$Isym  <- is_next
     rn$Iasym <- ia_next

     # -- reported cases --------------------------------------------------------
     idx_probe <- tick - par$delta_reporting_cases
     if (idx_probe >= 0L) {
          probe <- .row_at(idx_probe)
          # Note: the Census `N`, unlike the manual sum used for the epidemic
          # flag above.
          infected_fraction <- state$rows[[probe]]$Isym / state$rows[[probe]]$N
          chi_eff <- ifelse(infected_fraction < par$epidemic_threshold,
                            par$chi_endemic, par$chi_epidemic)
          drawn <- .sim_binom(ctl, "infectious/reported_cases",
                              state$rows[[probe]]$new_symptomatic, par$rho)
          rh$reported_cases <- rh$reported_cases +
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
     rh <- state$rows[[here]]
     rn <- state$rows[[nxt]]
     ndd_prob <- par$non_disease_death_prob_jt[here, ]

     v1_next <- rh$V1
     v2_next <- rh$V2

     # -- natural mortality, both doses ----------------------------------------
     ndd1 <- .sim_binom(ctl, "vaccinated/v1_non_disease_deaths", v1_next, ndd_prob)
     v1_next <- v1_next - ndd1
     ndd_total <- rh$non_disease_deaths + ndd1

     ndd2 <- .sim_binom(ctl, "vaccinated/v2_non_disease_deaths", v2_next, ndd_prob)
     v2_next <- v2_next - ndd2
     ndd_total <- ndd_total + ndd2
     rh$non_disease_deaths <- ndd_total

     # -- waning vaccine-derived immunity --------------------------------------
     waned1 <- .sim_binom(ctl, "vaccinated/v1_waning", v1_next, par$omega_1_prob)
     v1_next <- v1_next - waned1
     s_next <- rn$S + waned1

     waned2 <- .sim_binom(ctl, "vaccinated/v2_waning", v2_next, par$omega_2_prob)
     v2_next <- v2_next - waned2
     s_next <- s_next + waned2
     rn$S <- s_next

     # -- second doses, BEFORE first doses -------------------------------------
     # Deliberate: doing first doses first would let an individual move
     # S -> V1 -> V2 within a single tick (vaccinated.py:178-180).
     nu2 <- par$nu_2_jt[here, ]
     if (any(nu2 != 0)) {
          doses2 <- as.integer(round(nu2))
          # The schedule is clamped to the donor pool. The engine logs this at
          # DEBUG and carries on; the clamp itself is the observable behaviour.
          doses2 <- pmin(doses2, v1_next)
          rh$dose_two_doses <- doses2

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
          pop <- vapply(sources, function(nm) rn[[nm]],
                        numeric(state$.npatches))
          if (state$.npatches == 1L) pop <- matrix(pop, nrow = 1L)
          available <- rowSums(pop)

          doses1 <- pmin(doses1, as.integer(available))
          rh$dose_one_doses <- doses1

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
               rn[[nm]] <- rn[[nm]] - effective
               total_immunized <- total_immunized + effective
          }
          v1_next <- v1_next + total_immunized
     }

     rn$V1 <- v1_next
     rn$V2 <- v2_next
     state
}

#' @rdname sim_components
#' @keywords internal
sim_phase_human_to_human <- function(state, par, ctl, tick) {

     .sim_at(ctl, tick, "HumanToHuman")
     here <- .row_at(tick)
     nxt  <- .row_next(tick)
     rh <- state$rows[[here]]
     rn <- state$rows[[nxt]]

     total_i <- rh$Isym + rh$Iasym
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
     denominator <- rh$N^par$alpha_2
     rate <- numerator / denominator

     # The engine clamps a negative rate and logs at DEBUG rather than failing
     # (humantohuman.py:159-161); a negative rate would make `-expm1(-rate)`
     # exceed 1 and the binomial reject it.
     rate <- pmax(rate, 0)
     rn$Lambda <- rate

     # Drawn on S[tick + 1], after mortality and the waning inflows.
     s_next <- rn$S
     local <- as.integer(round(local_frac * s_next))
     new_infections <- .sim_binom(ctl, "humantohuman/infection",
                                  local, -expm1(-rate))

     rn$S <- s_next - new_infections
     rn$E <- rn$E + new_infections
     rn$incidence_human <- rn$incidence_human + new_infections
     rn$incidence       <- rn$incidence + new_infections
     state
}

#' @rdname sim_components
#' @keywords internal
sim_phase_env_to_human <- function(state, par, ctl, tick) {

     .sim_at(ctl, tick, "EnvToHuman")
     here <- .row_at(tick)
     nxt  <- .row_next(tick)
     rh <- state$rows[[here]]
     rn <- state$rows[[nxt]]

     w <- rh$W

     # Psi = beta_jt_env * (1 - theta_j) * D / (kappa + D), where D is the
     # PER-CAPITA reservoir W/N -- not the patch total W.
     #
     # WHY (v0.89.0). The reservoir update accumulates ABSOLUTE cells summed
     # over everyone shedding, so W is extensive: it scales with the number of
     # infectious people and, through them, with patch population. kappa is a
     # CONCENTRATION -- 04-model-description.Rmd defines it as "the V. cholerae
     # concentration at which the per-contact probability of infection is 50%"
     # and its prior is fitted to volunteer dose-response studies reporting
     # CFU concentrations. Comparing an extensive stock against an intensive
     # constant is dimensionally incoherent, and it pins the dose-response at 1.
     #
     # The spec flags this itself, in the shedding section: "the W-vs-kappa
     # scale matching is an open methodological question: at very high
     # simulated W, the dose-response W/(kappa + W) saturates near unity and the
     # environmental force of infection becomes weakly identifying for kappa."
     # Measured, it is worse than "weakly identifying at very high W": with the
     # shipped zeta_1 a SINGLE symptomatic person puts W/(kappa+W) at 0.9994,
     # 95.7% of patch-days exceed 0.99, and across all nine corner combinations
     # of the joint kappa x zeta_1 prior the response lies in [0.986, 1.000].
     # No draw anywhere in the prior escapes saturation, so kappa, zeta_1,
     # zeta_2, zeta_ratio and the four decay parameters are flat directions --
     # their posteriors return their priors.
     #
     # Dividing by N restores density dependence and puts kappa on a per-capita
     # scale where it identifies something: the symptomatic prevalence at which
     # environmental transmission half-saturates. At the shipped kappa = 1e6
     # that is ~0.3% symptomatic prevalence -- linear at low prevalence,
     # saturating during large outbreaks, which is what a dose-response should
     # do. It also brings the realised human share near onset to ~25%, against
     # the p_beta prior's 34.7%, WITHOUT touching p_beta or the mobility priors.
     #
     # HONEST LIMIT: W/N is cells per capita, not cells per mL, so this does not
     # literally make W a concentration. The residual per-capita-to-per-volume
     # conversion stays absorbed into zeta, exactly as the spec's shedding
     # section describes ("the two specifications differ only by whether the
     # daily stool-volume integral is absorbed into zeta_k").
     #
     # REPLAY keeps the oracle's raw-W form: replay validates the port against
     # laser-cholera draw-for-draw, and the oracle uses raw W. See
     # .SIM_RNG_ONLY_CORRECTIONS in sim_rng.R for the full list of deliberate
     # rng-mode divergences and what replay therefore no longer covers.
     dose <- if (isTRUE(ctl$mode == "replay")) w else w / pmax(rh$N, 1)
     psi <- par$beta_jt_env[here, ] * ((1 - par$theta_j) * dose) / (par$kappa + dose)
     rn$Psi <- psi

     local_frac <- par$local_frac      # float32-precision `1 - tau_i`
     s_next <- rn$S
     local_s <- as.integer(round(local_frac * s_next))
     new_infections <- .sim_binom(ctl, "envtohuman/infection",
                                  local_s, -expm1(-psi))

     rn$S <- s_next - new_infections
     rn$E <- rn$E + new_infections
     rn$incidence_env <- rn$incidence_env + new_infections
     rn$incidence     <- rn$incidence + new_infections
     state
}

#' @rdname sim_components
#' @keywords internal
sim_phase_environmental <- function(state, par, ctl, tick) {

     .sim_at(ctl, tick, "Environmental")
     here <- .row_at(tick)
     nxt  <- .row_next(tick)
     rh <- state$rows[[here]]
     rn <- state$rows[[nxt]]

     w <- rh$W
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
                           par$zeta_1 * rh$Isym, state$.npatches)
     w_next <- w_next + (1 - par$theta_j) * shed_sym

     shed_asym <- .sim_pois(ctl, "environmental/shedding_asym",
                            par$zeta_2 * rh$Iasym, state$.npatches)
     w_next <- w_next + (1 - par$theta_j) * shed_asym

     rn$W <- w_next
     state
}
