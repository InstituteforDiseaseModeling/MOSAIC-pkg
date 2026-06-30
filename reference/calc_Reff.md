# Cori effective reproductive number (R_eff) from ensemble trajectories

Computes the per-location, time-varying Cori (2013) instantaneous
**infection** effective reproductive number \\R^{\mathrm{cori}}\_{jt}\\
by an analytic reduction over the ensemble trajectories the run already
captured (no extra simulation). The renewal numerator and denominator
both use the realised **infection-incidence** flow (the captured
`incidence` channel = `incidence_human + incidence_env`, recorded at the
S-\>E moment), with the two infectious classes counted at **equal
weight**. Heterogeneity in infectiousness over the course of infection
is carried by the moment-matched generation-interval kernel \\g\\ (see
`.mosaic_generation_time_pmf`), not by any weight on the incidence
input. There is therefore no `numerator` or shedding-weight argument,
and the function never reads `Isym`/`Iasym` or `zeta_*`.

## Usage

``` r
calc_Reff(
  ensemble,
  config,
  max_days = 56L,
  weights = NULL,
  probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
  infectiousness_floor = 1,
  verbose = TRUE
)
```

## Arguments

- ensemble:

  Either a `mosaic_trajectories` artifact (the object
  [`calc_model_ensemble()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md)
  attaches as `$trajectories` and
  `.mosaic_persist_trajectory_artifact()` writes to
  `2_calibration/trajectories_ensemble.rds`) OR a `mosaic_ensemble`
  carrying that artifact in its `$trajectories` field. It must provide
  the weighted-median `incidence` channel (`summary$incidence$median`,
  an \\nL \times T\\ matrix); the per-member `lines` are used for the
  posterior credible interval when they are daily-consecutive.

- config:

  The medoid `config` list. Reads only the kernel timing parameters
  `iota`, `gamma_1`, `gamma_2`, `sigma` (scalars).

- max_days:

  Integer. Generation-interval kernel truncation horizon (days). Default
  `56L`.

- weights:

  Optional numeric vector of per-member weights for the posterior
  reduction. `NULL` (default) uses the member weights carried in the
  trajectory `lines`.

- probs:

  Numeric vector of credible-interval quantile probabilities. Default
  `c(0.025, 0.25, 0.5, 0.75, 0.975)`.

- infectiousness_floor:

  Numeric scalar \\\ge 0\\. Minimum generation-weighted past
  infectiousness (effective past infections) required to report
  \\R\_{t}\\ at a step; below it the cell is `NA`. Guards
  initial-condition seed spikes (which otherwise yield \\R \approx
  10^{3}\\) and deep inter-epidemic troughs (\\R \approx 0.01\\).
  Default `1`. Applied identically to the central (medoid) series and to
  every per-member posterior series. Set `0` for the pure Cori
  convention.

- verbose:

  Logical; emit progress messages. Default `TRUE`.

## Value

A tidy long `data.frame` (`reproductive_numbers` schema) with columns
`location`, `date`, `t`, `estimand` (`"R_eff"`), `central` (**this
direct path**: renewal on the weighted-MEDIAN incidence – see the caveat
below), and one column per requested quantile (`q2.5`, `q25`, `q50`,
`q75`, `q97.5`, ...) from the posterior reduction over members via
[`weighted_quantiles`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/weighted_quantiles.md).
The wide \\nL \times T\\ central matrix is attached as attribute
`"central_matrix"`. Provenance attributes: `kernel`
(`"moment_matched"`), `series` (`"infection_incidence"`),
`kernel_params`, `ci_source`, and `caveat`.

## Details

**Caveat (model descriptor, not an invasion threshold).** Because this
is computed on *simulated* incidence, it describes the model trajectory
and is directly comparable to a surveillance-derived R_eff computed the
same way; it is *not* an independent first-principles basic reproductive
number.

**Estimand limitation (two-clock generation interval).** The kernel
\\g\\ is the **two-clock** (latent + infectious, human-route) generation
interval, \\\mathcal{G} = 1/\iota + 1/\gamma\_{\mathrm{eff}}\\. The
documented **three-clock** extension that adds an environmental-survival
delay \\1/\delta\_{jt}\\ (04-model-description.Rmd, "Generation-time
distribution (latent + infectious + environmental delay)") is
deliberately *excluded* here. Consequently, in waterborne-dominated
locations where the environmental pathway carries much of the
transmission, this estimator uses a shorter mean generation time than
the true infection-to-infection interval and is therefore a
**lower-mean-\\\mathcal{G}\\ approximation**: it will read slightly
closer to 1 (less extreme in either direction) than a three-clock kernel
would. The pooled `incidence` series itself does include both the human
and environmental S-\>E routes; only the kernel timing is two-clock.

**Central definition differs from the production (re-simulation) path.**
This cheap direct path sets `central` = the renewal estimator applied to
`summary$incidence$median` (the full daily weighted-MEDIAN incidence).
The **production** path used by
[`add_reproductive_numbers`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/add_reproductive_numbers.md)`( recompute_ci = TRUE)`
(`.mosaic_reff_resim_ci`) instead sets `central` = the **MEDOID
trajectory's R_t** (a single coherent member's series) because the
per-day cross-member median – and, equivalently, the renewal on the
median incidence – is FLATTENED toward 1 by phase misalignment of member
peaks and does NOT represent the epidemic's peak R_t. On this direct
path the same caveat applies: the weighted-median incidence is a
phase-smoothed series, so its renewal `central` is a calendar-date
descriptor, not the coherent peak R_t. Use the re-simulation path
(`recompute_ci = TRUE`) for the phase-coherent headline and the
`peak_Rt` explosivity statistic.

**Posterior credible interval.** Each retained member's daily R_eff
series is reconstructed from the per-member `incidence` `lines` and the
kernel, then reduced per `(location, t)` cell with
[`weighted_quantiles`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/weighted_quantiles.md)
(weight applied *once*; never `sample(prob = w)`). This requires
daily-consecutive lines; the persisted artifact thins `lines` on a
stride (default every 7 days), on which a daily renewal convolution is
undefined. When `lines` are *not* daily-consecutive the quantile columns
are returned as `NA` with `ci_source = "unavailable_strided_lines"` and
a warning, rather than fabricating a CI from sub-daily data. The kernel
is held fixed at the medoid config across members (the thinned lines do
not carry per-member kernel draws; documented approximation, plan
section G.2).

**The posterior CI is currently unavailable on production-default
artifacts.** The trajectory-capture grid in
[`calc_model_ensemble()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md)
writes `lines` on a stride that does not yield a daily-consecutive run,
so the `unavailable_strided_lines` path is taken in production and only
the `central` (medoid) series is populated. This is **not** fixable by
merely re-capturing with a finer stride from the caller: the builder
grid formula yields an empty-or-still-strided set at every stride
setting, so the daily-consecutive precondition is unreachable until the
capture grid itself is changed. Restoring a usable posterior CI requires
a **Phase-2 fix to the trajectory-capture grid** in
[`calc_model_ensemble()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md);
do not expect a `line_stride = 1` re-capture to enable it.

## References

Cori A, Ferguson NM, Fraser C, Cauchemez S (2013). A new framework and
software to estimate time-varying reproduction numbers during epidemics.
American Journal of Epidemiology 178(9):1505-1512.

## See also

[`weighted_quantiles`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/weighted_quantiles.md)
