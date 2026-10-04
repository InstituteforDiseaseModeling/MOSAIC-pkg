# Estimate Initial E and I Compartments from Surveillance Data

This function estimates the initial number of individuals in the Exposed
(E) and Infected (I) compartments at model start time using recent
surveillance data through a Monte Carlo simulation approach.

## Usage

``` r
est_initial_E_I(
  PATHS,
  priors,
  config,
  n_samples = 1000,
  t0 = NULL,
  lookback_days = 21,
  lookahead_days = 0,
  quiet_start = c("template", "seed"),
  quiet_seed_shape1 = 1,
  quiet_seed_shape2 = 1e+05,
  verbose = TRUE,
  parallel = FALSE,
  variance_inflation = 2,
  seed = NULL
)
```

## Arguments

- PATHS:

  List of paths from
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).

- priors:

  Prior distributions for parameters (e.g., `priors_default`).

- config:

  Configuration object with location codes and `date_start`. Must
  include `config$location_name` and `config$date_start`.

- n_samples:

  Number of Monte Carlo samples (default 1000).

- t0:

  Target date for estimation (default from `config$date_start`).

- lookback_days:

  Days of surveillance data before t0 to use (default 21).

- lookahead_days:

  Days of surveillance data from t0 onward that also enter the
  onset-rate estimate (default 0). With weekly reports downscaled to
  days, a window that ends at t0 can miss an outbreak already under way
  at t0; a window straddling t0 estimates the onset rate at t0 itself. A
  location gets the near-zero template only when the whole window
  `[t0 - lookback_days, t0 + lookahead_days)` reports no cases.

- quiet_start:

  What a "quiet-start" location gets. A location is a quiet start when
  it reports at least one observed or reconstructed (tier 1-2) case
  after the surveillance window, up to `config$date_stop` (the end of
  the data when `config$date_stop` is NULL), and EITHER (a) its window
  around t0 reports no cases (or is all NA), OR (b) the E/I priors the
  window gives imply fewer than one expected initial infection,
  `N * (E[prop_E] + E[prop_I]) < 1`, with `N` the population at t0 used
  in the fit and `E[.]` the Beta means. `"template"` (default) leaves
  the window's priors in place: the near-zero Beta(0.01, 99999.99) for
  (a), the data-based Beta for (b). `"seed"` gives E and I each the weak
  seeding prior Beta(`quiet_seed_shape1`, `quiet_seed_shape2`) instead:
  it stands in for undetected circulation or importation that the model
  has no mechanism for, so a single-location fit can still reproduce the
  later outbreak. Locations with no cases anywhere up to
  `config$date_stop`, and locations whose window-based priors imply at
  least one expected initial infection, are never changed.

- quiet_seed_shape1, quiet_seed_shape2:

  Beta shapes of the quiet-start seeding prior (default 1 and 1e5: mean
  1e-5 of the population per compartment, mode at zero).

- verbose:

  Print progress messages (default TRUE).

- parallel:

  Enable parallel processing for Monte Carlo sampling when
  `n_samples >= 100` (default FALSE). Uses
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html)
  with all available cores. Note: Not supported on Windows.

- variance_inflation:

  Multiplicative CI factor for the Beta refit (default 2): the Beta
  keeps the Monte Carlo mean and its spread is fit to the target 95% CI
  mean / VI to mean \* VI. A scalar or a named per-ISO vector. Should be
  \> 1.1 for meaningful variance.

- seed:

  Optional integer seed. When given, each location's Monte Carlo draws
  use seeds derived from `seed` and the ISO code, so results are
  reproducible and identical with or without `parallel`; the caller's
  RNG state is restored. NULL (default) draws from the session RNG.

## Value

A list with two main components:

- metadata:

  List containing estimation details: description, version, date, t0,
  lookback_days, lookahead_days, n_samples, method, quiet_start,
  quiet_start_seeded (the locations given the seeding prior) and
  imputed_window_fallback (the locations whose window had no tier 1-2
  count and was read from country-level reconstructions).

- parameters_location:

  List with `prop_E_initial` and `prop_I_initial`, each containing:

  - parameter_name: Parameter identifier

  - distribution: `"beta"`

  - parameters\$location: Named list by ISO code with `shape1` and
    `shape2`

## Details

The method back-calculates symptom onsets from reported cases through
the engine's reporting chain and maps them to E/I stocks at t0 (see
[`est_initial_E_I_location`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_initial_E_I_location.md)).
Each Monte Carlo draw samples `sigma`, `iota`, `gamma_1`, `gamma_2`,
`rho`, `chi_endemic` and `delta_reporting_cases` from
`priors$parameters_global` (a missing prior is replaced by a fixed value
with a warning); the parallel and sequential branches run the same draw
function. Draws with E or I = 0 are kept in the mean. Locations with no
usable surveillance in the window (no rows, or every case count NA) get
the near-zero Beta(0.01, 99999.99) template, the same prior as a window
that reports zero cases throughout: absent surveillance is not evidence
of active infection at t0, so it never seeds more E/I than confirmed
zeros. Locations with surveillance but too few usable draws, or an
estimation error, get the fallback Beta priors (Beta(1, 9999) for E,
Beta(0.5, 9999.5) for I).

Imputed surveillance rows (tier 3 of `.surveillance_tier()`: AI Fourier
reconstructions, which spread a year's reported or residual total over
its unobserved weeks along a seasonal shape) are not dated reports.
Where a location's window holds any observed or reconstructed (tier 1-2)
count, its imputed days are unobserved for the back-calculation. A
window with no tier 1-2 count falls back on its country-level
reconstructions (`fourier_country_*`), the only estimate of that
country's level there (listed in `metadata$imputed_window_fallback`);
regional reconstructions (`fourier_regional_*`) never count. The
quiet-start test counts tier 1-2 later cases only. Without a
`disaggregation_method` column every row is observed.

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS  <- get_paths()
priors <- priors_default
config <- config_default
results <- est_initial_E_I(
  PATHS, priors, config,
  n_samples = 1000,
  variance_inflation = 2           # Factor for expanding CI bounds around sample mean
)
} # }
```
