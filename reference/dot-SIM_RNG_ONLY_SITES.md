# Draw sites that exist in the R engine but NOT in the oracle

The R engine is a port of laser-cholera 0.16.1 and reproduces its PRNG
calls draw-for-draw, which is what makes the Tier B replay fixtures a
valid parity harness. Any site listed here is a DELIBERATE divergence: a
variate the R engine draws in `"rng"` mode that Python never drew.

## Usage

``` r
.SIM_RNG_ONLY_SITES
```

## Details

Such a site must be drawn ONLY in `"rng"` mode. Drawing it under
`"replay"` would consume a variate the fixture has no record of and
desynchronise every subsequent draw, destroying parity for the 22 oracle
sites.

`infectious/sigma_split` (v0.89.0): the spec specifies a stochastic
symptomatic split and the oracle does a deterministic `np.round`, which
is wrong in the mean at low counts; `infectious/sigma_split_t0` applies
the same correction to the t=0 split of `I_j_initial`.
`infectious/fatal_onsets` (v0.96.0): production draws fatal outcomes at
symptom onset from the time-varying reported CFR `mu_jt`, where the
oracle applies a daily hazard to the symptomatic stock. See
`sim_components.R` for the full rationale.

NOTE for anyone extending this: CLAUDE.md lesson \#15 records that a
PHANTOM `infectious/sigma_split` was once invented in this registry by
mistake, when it was not a PRNG call anywhere. It is a real draw now,
but only on the R side – it must never appear in `.SIM_ORACLE_SITE_MAP`,
and a test asserts exactly that.
