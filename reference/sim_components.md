# Per-tick phase functions for the R transmission engine

One function per component of the Python pipeline, in the canonical
order fixed by `model.py:566-579`:

## Usage

``` r
sim_phase_susceptible(state, par, ctl, tick)

sim_phase_census(state, par, ctl, tick)

sim_phase_exposed(state, par, ctl, tick)

sim_phase_recovered(state, par, ctl, tick)

sim_phase_infectious(state, par, ctl, tick)

sim_phase_vaccinated(state, par, ctl, tick)

sim_phase_human_to_human(state, par, ctl, tick)

sim_phase_env_to_human(state, par, ctl, tick)

sim_phase_environmental(state, par, ctl, tick)
```

## Details


    Susceptible -> Exposed -> Recovered -> Infectious -> Vaccinated -> Census
      -> HumanToHuman -> EnvToHuman -> Environmental -> DerivedValues

The order is semantically load-bearing and is reproduced rather than
rationalised. `Infectious` reads `E[tick + 1]` *after* `Exposed` has
written it; `HumanToHuman` reads `N[tick]` but writes into
`S[tick + 1]`, which `Census` has already summed. That asymmetry is
real. **Port the behaviour, not the intent** – anything that looks wrong
gets an issue, not a local fix.

Each function takes `(state, par, ctl, tick)` where `tick` is 0-based as
in Python, and writes into row `tick + 2` of the state series (R's
1-based row for Python's `tick + 1`).

State rows are environments (see
[`sim_alloc_state()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sim_alloc_state.md)),
so each phase binds the rows it needs once – `rh` for `here`, `rn` for
`nxt` – and then reads and writes channels by name: `rh$S`,
`rn$E <- ...`. The two exceptions are the lagged reads, which address an
arbitrary historical row and so spell it out as
`state$rows[[probe]]$Isym`. Writing through `rh`/`rn` mutates the state
in place; there is no write-back step.
