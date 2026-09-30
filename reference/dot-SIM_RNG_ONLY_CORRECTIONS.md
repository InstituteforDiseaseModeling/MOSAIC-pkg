# Spec corrections applied in "rng" mode but not in "replay"

The single place that records where the production engine deliberately
departs from laser-cholera 0.16.1, and therefore exactly what the Tier B
replay fixtures no longer certify about production.

## Usage

``` r
.SIM_RNG_ONLY_CORRECTIONS
```

## Details

Replay exists to answer "did we port Python correctly?", so it
reproduces the oracle including its defects. Production answers "does
the engine implement the model?", and where the spec and the oracle
disagree, production follows the spec. Every entry here needs its own
non-replay test, because the parity harness structurally cannot cover it
– CLAUDE.md lesson \#18(v).

- infectious/sigma_split:

  v0.89.0. The oracle splits E-\>I with `np.round(sigma * progressing)`;
  the spec's stochastic-transitions table specifies a binomial. round()
  is not linear, so the deterministic form is wrong in the MEAN at low
  counts (zero symptomatic for every progression \<= 2 at sigma = 0.2).
  Covered by test-sigma-split.R.

- infectious/sigma_split_t0:

  The same correction for the t=0 split of `I_j_initial`, which the
  oracle seeds with `round(sigma * I_j_initial)`: at sigma = 0.25 a
  patch seeded with 1 or 2 infections starts with no symptomatic.
  Covered by test-review-engine-initial-split.R.

- envtohuman/dose_percapita:

  v0.89.0. The oracle's dose-response is `W/(kappa + W)` with W an
  absolute cell count; kappa is a concentration. Production divides
  by N. Covered by test-env-dose-response.R.

- infectious/fatal_onsets:

  v0.96.0. The oracle's mortality is a daily hazard
  `mu_j_baseline * (1 + mu_j_epidemic_factor * flag)` on the symptomatic
  stock, reported `delta_reporting_deaths` days after the death.
  Production draws each onset's outcome at onset from the time-varying
  reported CFR `mu_jt` and reports deaths on the case lag. Covered by
  test-sim-mortality-onset.R.
