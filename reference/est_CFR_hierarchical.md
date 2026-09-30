# Estimate the time-varying reported case fatality ratio (mu_jt) from WHO annual data

Fits a hierarchical binomial GAM to every country-year in the WHO annual
cholera record and returns a per-country, per-year estimate of the
reported case fatality ratio (CFR, reported deaths per reported
suspected case). The estimates are the prior for the engine's
time-varying `mu_jt`: they are expanded to a daily \[location x day\]
matrix by
[`make_mu_jt`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_mu_jt.md)
and carry the widths used by the calibration's integrated deaths
likelihood.

## Usage

``` r
est_CFR_hierarchical(
  PATHS,
  min_cases = 1,
  k_year = 12,
  k_trend = 10,
  include_country_trends = TRUE,
  forecast_years = 3L,
  forecast_method = c("carry_forward", "project"),
  validate = TRUE,
  population_weighted = FALSE,
  save_diagnostics = TRUE,
  verbose = TRUE
)
```

## Arguments

- PATHS:

  List of paths from
  [`get_paths`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md);
  reads `PATHS$DATA_WHO_ANNUAL/who_afro_annual.csv` and writes to
  `PATHS$MODEL_INPUT`.

- min_cases:

  Integer; a country-year enters the fit only with at least this many
  reported cases (default 1).

- k_year:

  Integer; basis dimension of the global temporal smooth (default 12).

- k_trend:

  Integer; basis dimension of each country's trend smooth (default 10).

- include_country_trends:

  Logical; include country-specific trend smooths (default TRUE).

- forecast_years:

  Integer; number of years past the last data year to supply estimates
  for (default 3).

- forecast_method:

  One of `"carry_forward"` (default: every year after the last data year
  repeats that year's estimate) or `"project"` (evaluate the fitted
  smooths beyond the data).

- validate:

  Logical; run the rolling-origin out-of-sample check (default TRUE).

- population_weighted:

  Deprecated and ignored (the former branch counted cases twice).

- save_diagnostics:

  Logical; write `cfr_model_diagnostics.pdf` to `PATHS$MODEL_INPUT`
  (default TRUE).

- verbose:

  Logical; print progress messages (default TRUE).

## Value

A list of class `cfr_hierarchical_model`:

- `model`: the fitted `bam` object;

- `predictions`: one row per MOSAIC location and year with
  `cfr_estimate` (median), predictive `cfr_lower`/`cfr_upper` (95%),
  `cfr_se` (logit-scale estimation error), `logit_mean`, `logit_sd`
  (predictive), `is_forecast`, `pooled` (location absent from the fit),
  `n_country_years`;

- `country_effects`: the country random intercepts;

- `temporal_trend`: the population-average curve;

- `validation`: rolling-origin results, or `NULL`;

- `summary`: fit statistics, `tau`, `sigma` and settings.

Files written to `PATHS$MODEL_INPUT`: `param_mu_disease_mortality.csv`
(MOSAIC parameter format, an export not read by the package; per
location-year `point` median (`parameter_name = "median"`), `beta`
shapes and `logitnormal` mean/sd), `cfr_hierarchical_estimates.csv`,
`cfr_temporal_trend.csv`, `cfr_country_effects.csv` and
`cfr_model_summary.rds`.

## Details

**Model.** For country \\j\\ in year \\y\\, with \\D\_{jy}\\ deaths out
of \\C\_{jy}\\ cases, \$\$D\_{jy} \sim \mathrm{Binomial}(C\_{jy},
p\_{jy}),\quad \mathrm{logit}\\ p\_{jy} = f(y) + u_j + g_j(y) +
e\_{jy},\$\$ where \\f\\ is a global smooth trend, \\u_j \sim N(0,
\tau^2)\\ a country random intercept, \\g_j\\ a country-specific
penalised trend (a factor smooth, shrunk toward the global curve), and
\\e\_{jy} \sim N(0, \sigma^2)\\ a country-year random effect. Fitted by
fREML with [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html).

The country-year term \\e\_{jy}\\ is what makes the hierarchy work.
Annual CFR varies far more between years than a binomial allows (Pearson
dispersion ~29 without it); with no term to absorb that, the country
trends soak up the year-to-year noise, the country intercepts carry no
information, and countries with little data receive an extrapolated
per-country curve. With it, countries with few observations shrink
toward the global curve.

**Data.** Every country-year in the WHO annual file, 1970 onward, for
every country in the file (not only MOSAIC locations): the extra
countries inform the global curve and the between-country spread.
Held-out testing (fit through 2022, predict 2023-25) favoured using all
years with country trends over discarding the pre-2000 record.

**Estimates and widths.** The point estimate for a country-year excludes
\\e\_{jy}\\. Its predictive standard deviation on the logit scale is
\\\sqrt{se^2 + \sigma^2}\\, where \\se\\ is the estimation error of
\\f + u_j + g_j\\; a MOSAIC location absent from the fit receives the
global curve with \\\tau^2\\ added. These are *predictive* widths for a
single year, not the confidence interval of the mean.

\\\tau\\ is weakly identified: the factor smooth \\g_j\\ carries its own
per-country intercept, so the fit can put the between-country spread in
either term (full data 1970-2025: \\\tau \approx 0.003\\; through 2024:
0.31). The per-country estimates are unaffected, but the width given to
a location absent from the fit then understates between-country
variation. Every MOSAIC location is in the WHO annual data, so no MOSAIC
prior relies on it.

**In-progress years and forecast years.** A calendar year still in
progress when its dashboard snapshot was taken is excluded (its deaths
lag its cases). Under `"carry_forward"` the global trend is held after
the last year in the data and each country's own trend after that
country's last year, and `is_forecast` marks years past the country's
own last year. The rolling-origin check compares both forecast rules,
scoring each held-out country-year's observed deaths against their
predictive distribution, and records the result in `summary$validation`.

## See also

[`make_mu_jt`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_mu_jt.md)

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS <- get_paths()
fit <- est_CFR_hierarchical(PATHS)
head(fit$predictions)
} # }
```
