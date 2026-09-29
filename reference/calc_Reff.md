# Route-decomposed Cori effective reproductive number from ensemble trajectories

Computes the per-location, time-varying Cori (2013) instantaneous
**infection** reproductive number split by transmission route,
\\R^{\mathrm{eff}}\_{jt} = R^{\mathrm{hum}}\_{jt} +
R^{\mathrm{env}}\_{jt}\\, by an analytic reduction over the ensemble
trajectories the run already captured (no extra simulation).

## Usage

``` r
calc_Reff(
  ensemble,
  config,
  weights = NULL,
  probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
  infectiousness_floor = 1,
  verbose = TRUE
)
```

## Arguments

- ensemble:

  A `mosaic_trajectories` artifact
  (`2_calibration/trajectories_ensemble.rds`) or a `mosaic_ensemble`
  carrying one in `$trajectories`. Must provide weighted-median
  `incidence_human` and `incidence_env` channels; `E`, `Isym` and
  `Iasym` supply the initial infectious stocks.

- config:

  The medoid `config` list: kernel parameters `iota`, `gamma_1`,
  `gamma_2`, `sigma`, `zeta_1`, `zeta_2`, plus the fields the engine
  needs to rebuild \\\delta\_{jt}\\ (`psi_jt`, `decay_*`).

- weights:

  Optional per-member weights for the posterior reduction, indexed by
  member id. `NULL` (default) uses the weights in `lines`.

- probs:

  Credible-interval quantile probabilities.

- infectiousness_floor:

  Numeric scalar \\\ge 0\\. Minimum route infectiousness (effective past
  infections) required to report that route's R at a step (a route below
  it with no infections of its own contributes 0 to the total). Default
  `1`; `0` is the pure Cori convention.

- verbose:

  Logical; emit progress messages.

## Value

A tidy long `data.frame` (`reproductive_numbers` schema): `location`,
`date`, `t`, `estimand` (`"R_eff"`, `"R_hum"`, `"R_env"`), `central`
(renewal on the weighted-median route incidences) and one column per
quantile. Attributes: `central_matrix` (R_eff, nL x T), `route_central`
(list of R_hum and R_env matrices), `env_share`, `kernel`
(`"route_instantaneous"`), `kernel_params`, `ci_source`, `caveat`.

## Details

**Estimand.** Each route's numerator is its own infection incidence
(`incidence_human`, `incidence_env`); both denominators are driven by
total incidence, because every infection is infectious through both
routes. \\\Lambda^{hum}\\ propagates past infections through the latent
and infectious states (symptomatic and asymptomatic at equal weight, as
in the human force of infection). \\\Lambda^{env}\\ additionally routes
their shedding (weighted by `zeta_1`, `zeta_2`) through the reservoir,
which decays at the \\\psi\\-dependent rate \\\delta\_{jt}\\, so the
environmental generation interval (tens to hundreds of days) and its
seasonal variation are represented. Both kernels are derived from the
engine's own daily transition probabilities and phase order. The
estimate is **instantaneous**: the reservoir is built from the actual
past decay path and one infection's lifetime reservoir contribution is
evaluated at today's \\\delta\_{jt}\\ (secondary infections per
infection if conditions stayed as they are at t). Nothing after t
enters, so truncating the series does not change earlier values. WASH
(`theta_j`), the absolute shedding scale, `kappa` and the transmission
rates cancel from the kernels and live in the R values.

**Initial conditions.** People already latent or infectious at the start
(the `E`, `Isym`, `Iasym` stocks on the first day) are included in both
infectiousness terms, so the series is defined from the start. R in
roughly the first \\1/\delta\\ days still reflects the reservoir filling
from empty and is best read after a burn-in
([`add_reproductive_numbers()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/add_reproductive_numbers.md)
applies one).

**Caveat.** This describes the simulated trajectory (comparable to a
surveillance-derived R_eff computed the same way); it is not an invasion
threshold. The renewal assumes transmission is linear in infectiousness;
the human FOI uses \\I^{\alpha_1}\\ and the environmental dose
saturates, so both route values are trajectory descriptors, not
per-contact constants. Suitability \\\psi\\ enters \\R^{env}\\ twice,
through `beta_jt_env` and through the reservoir lifetime
\\1/\delta\_{jt}\\, and one infection's lifetime is valued at today's
\\\delta\_{jt}\\. Under seasonal \\\psi\\, \\R^{env} \> 1\\ is therefore
not a growth threshold: in a high-\\\psi\\ season it assumes survival
that will not last the infection's lifetime (up to ~200 days), and
between seasons the reverse. R here is also not comparable to literature
cholera R estimated with a ~5-day serial interval: for the same growth
rate a longer generation interval gives a larger R. The renewal is per
location: infectious people arriving through mobility (`tau_i`, `pi_ij`)
drive the destination's human force of infection but are not in its
\\\Lambda^{hum}\\, so in multi-location runs imported spread is credited
to the destination's R_hum.

**Central on this path is a calendar-date descriptor.** The renewal on
weighted-median incidence is phase-smoothed across members and reads
closer to 1 than any coherent trajectory.
[`add_reproductive_numbers`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/add_reproductive_numbers.md)`( recompute_ci = TRUE)`
reports the MEDOID trajectory's R_t and the per-member peak statistic
instead.

**Posterior CI.** Needs daily-consecutive per-member `lines` for both
route channels starting on day 1; production artifacts thin `lines` on a
stride, so the quantile columns are `NA` there
(`ci_source = "unavailable_strided_lines"`). Use the re-simulation path
for a CI. A cell's quantiles are reported only when members holding at
least half the weight are defined there.

## References

Cori A, Ferguson NM, Fraser C, Cauchemez S (2013). A new framework and
software to estimate time-varying reproduction numbers during epidemics.
American Journal of Epidemiology 178(9):1505-1512.

## See also

[`weighted_quantiles`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/weighted_quantiles.md),
[`plot_Reff`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_Reff.md)
