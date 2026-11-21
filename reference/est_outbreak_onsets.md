# Detect outbreak onsets for every location of a MOSAIC config

Builds each location's weekly series of reported suspected cases from a
MOSAIC config and runs
[`calc_outbreak_onset`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_outbreak_onset.md)
on it. Weeks are the complete Monday-to-Sunday weeks of the config's
daily window (partial edge weeks are dropped); a week is reported when
all seven of its days are, and its count is their sum.

## Usage

``` r
est_outbreak_onsets(
  config,
  method = c("growth", "growth_sensitive", "zheng"),
  mode = c("retrospective", "realtime"),
  imputed = c("unreported", "keep"),
  population = NULL,
  locations = NULL,
  ...
)
```

## Arguments

- config:

  A MOSAIC config (e.g. `config_default`) with `date_start`,
  `date_stop`, `location_name`, `reported_cases` and, optionally,
  `reported_tier`.

- method:

  `"growth"` (default), `"growth_sensitive"` or `"zheng"`; see
  [`calc_outbreak_onset`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_outbreak_onset.md).

- mode:

  `"retrospective"` (default) or `"realtime"`.

- imputed:

  `"unreported"` (default) treats tier-3 days as unreported; `"keep"`
  counts them.

- population:

  `NULL` (default) reads `param_N_population_size.csv` from
  `get_paths()$MODEL_INPUT`; or a data frame with columns `iso_code`,
  `date`, `population`. Each week takes the latest value on or before
  its first day (the earliest value for weeks before the first date).

- locations:

  ISO codes to run (default: every location of the config).

- ...:

  Rule settings passed to
  [`calc_outbreak_onset`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_outbreak_onset.md).

## Value

A data frame of the columns of
[`calc_outbreak_onset`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_outbreak_onset.md)
with `iso_code` first and `imputed_share` added: the share of the
config's reported cases within two weeks of the onset that come from
imputed days. The attribute `"series"` stacks the locations' weekly
series with an `iso_code` column.

## Details

Imputed days (`reported_tier` 3, every modelled disaggregation method,
e.g. AI `fourier_*` reconstructions of annual totals and `inferred_zero`
absences) are treated as unreported by default, as
[`est_epidemic_peaks`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_epidemic_peaks.md)
does: a Fourier reconstruction has a seasonal peak every year by
construction. With `imputed = "keep"` they count as reported, which
reproduces the series the onset handoff was tuned on. WHO multi-week
reports spread over their weeks (tier 2) count as reported either way.

## See also

[`calc_outbreak_onset`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_outbreak_onset.md)

## Examples

``` r
if (FALSE) { # \dontrun{
set_root_directory("~/MOSAIC")
onsets <- est_outbreak_onsets(config_default)
onsets_rt <- est_outbreak_onsets(config_default, mode = "realtime", locations = c("MOZ", "ETH"))
} # }
```
