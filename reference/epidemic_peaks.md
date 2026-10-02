# Cholera Epidemic Peaks Data

A dataset containing identified epidemic peaks from cholera surveillance
data across African countries. Peaks are detected using time series
analysis with smoothing and prominence-based peak detection algorithms.

## Usage

``` r
epidemic_peaks
```

## Format

A data frame with 6 variables:

- iso_code:

  ISO 3166-1 alpha-3 country code

- peak_start:

  Start date of the epidemic period (Date)

- peak_date:

  Date of peak incidence (Date)

- peak_stop:

  End date of the epidemic period (Date)

- reported_cases:

  Number of reported cholera cases at peak (numeric)

- outbreak_interval_days:

  Length of the peak window in days, `peak_stop - peak_start` (numeric)

## Source

Generated from cholera surveillance data using
[`est_epidemic_peaks()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_epidemic_peaks.md)
function on combined daily surveillance data from WHO and other sources.

## Details

**Scope:** The dataset is the full historical detection record from the
surveillance time series (all years of the combined daily series,
observed weeks only – AI Fourier reconstructions are excluded) and is
**not** pre-trimmed to any particular simulation window. Consumers that
score peaks against a specific config window (e.g.
[`calc_model_likelihood()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_likelihood.md),
the Python likelihood port, the simulation config builders) must filter
against `[date_start, date_stop]` first – otherwise
`which.min(abs(date_seq - peak_date))` silently snaps out-of-window
peaks to t=1 or t=N and biases the peak-shape likelihood terms. The
internal helper `MOSAIC:::.filter_epidemic_peaks()` is the canonical
filter and is applied at build time inside `make_config_default.R`, at
runtime inside
[`get_location_config()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_location_config.md),
and defensively inside
[`calc_model_likelihood()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_likelihood.md).

Epidemic peaks are identified using the following methodology:

- Time series smoothing with 28-day running mean window

- Local maxima detection with 10-day comparison windows

- Prominence-based filtering (minimum 8% of maximum smoothed value for
  most countries; country-specific overrides below)

- Minimum peak height threshold of 3 smoothed cases (default)

- Minimum 75-day separation between consecutive peaks

- Peak boundaries defined where incidence drops to 1/3 of peak height

Country-specific adjustments are applied for:

- Niger (NER): Lower prominence threshold (1.5%) for gradual peaks

- Cameroon (CMR): Adjusted threshold (4%) for plateau-shaped peaks

- Ethiopia (ETH): Lower threshold (3%) for multiple outbreaks

Detected peaks sit on observed weeks: imputed (AI Fourier, assumed-zero)
weeks are treated as missing (a WHO multi-week report spread over its
window counts as observed), a peak day must be observed with cases \> 0,
and a detected peak whose window is at least half imputed days is
dropped. The manual corrections below are documented outbreaks and are
exempt from that window filter.

Manual corrections have been applied for known issues including:

- Ethiopia 2024: February peak corrected to March (sustained outbreak)

- DRC 2023: Added January peak (filtered due to proximity)

- Nigeria 2024: Added October peak

- Kenya 2022-2023: Added December 2022, removed June 2023 minor peak

- Mozambique: Added February 2024 and March 2025 peaks

- Somalia: Added major April 2017 peak and 2024-2025 peaks

- Zambia: Added January 2018 peak

- Tanzania: Added January 2017 peak

## See also

[`est_epidemic_peaks`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_epidemic_peaks.md)
for the function that generates this data

[`plot_epidemic_peaks`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_epidemic_peaks.md)
for visualization

## Examples

``` r
data(epidemic_peaks)
head(epidemic_peaks)
#>     iso_code peak_start  peak_date  peak_stop reported_cases
#> 1        AGO 2006-03-27 2006-04-23 2006-05-23            757
#> 118      AGO 2018-01-01 2018-01-04 2018-01-31             24
#> 2        AGO 2025-04-09 2025-04-28 2025-05-25            303
#> 3        AGO 2025-10-02 2025-10-13 2025-10-24            149
#> 4        AGO 2026-04-18 2026-04-27 2026-05-12            106
#> 120      BDI 2016-08-01 2016-08-21 2016-10-31              9
#>     outbreak_interval_days
#> 1                       57
#> 118                     30
#> 2                       46
#> 3                       22
#> 4                       24
#> 120                     91

# Countries with epidemic data
unique(epidemic_peaks$iso_code)
#>  [1] "AGO" "BDI" "BEN" "CAF" "CIV" "CMR" "COD" "COG" "COM" "ETH" "GHA" "GIN"
#> [13] "GNB" "KEN" "LBR" "MOZ" "MWI" "NER" "NGA" "RWA" "SDN" "SEN" "SLE" "SOM"
#> [25] "SSD" "TCD" "TGO" "TZA" "UGA" "ZAF" "ZMB" "ZWE"

# Recent peaks (2024-2025)
recent_peaks <- epidemic_peaks[epidemic_peaks$peak_date >= as.Date("2024-01-01"), ]
table(recent_peaks$iso_code)
#> 
#> AGO BDI CAF CIV COD COG COM ETH GHA KEN MOZ NER NGA RWA SDN SOM SSD TCD TGO TZA 
#>   3   4   1   1   3   1   1   3   1   3   3   1   3   1   4   2   3   2   1   3 
#> UGA ZMB ZWE 
#>   3   2   2 

# Peak duration calculation
epidemic_peaks$duration <- as.numeric(
  epidemic_peaks$peak_stop - epidemic_peaks$peak_start
)
summary(epidemic_peaks$duration)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    2.00   16.00   28.00   32.13   50.00  117.00 
```
